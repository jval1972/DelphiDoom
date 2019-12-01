//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2012 by Jim Valavanis
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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://delphidoom.sitesled.com/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit m_argv;

interface

//
// MISC
//

uses
  d_delphi;

const
  MAXARGS = 1024;

var
  myargc: integer;
  myargv: array[0..MAXARGS] of string;

{ Returns the position of the given parameter }
{ in the arg list (0 if not found). }
function M_CheckParm(const check: string): integer;

function M_CheckParmCDROM: boolean;

procedure M_InitArgv;

procedure M_CmdShowCommandLineParams(const parm: string);

procedure M_CmdShowCmdline(const parm: string);

function M_SaveFileName(const filename: string): string;

const
  CD_WORKDIR = 'c:\doomdata\';

var
  cmdparams: TDStringList;

implementation

uses
  c_cmds,
  i_system;

var
  cdchecked: integer = -1;

function M_CheckParm(const check: string): integer;
var
  i: integer;
begin
  if cmdparams.IndexOf(check) < 0 then
    cmdparams.Add(check);
  for i := 1 to myargc - 1 do
    if strupper(check) = myargv[i] then
    begin
      result := i;
      exit;
    end;
  result := 0;
end;

function M_CheckParmCDROM: boolean;
begin
  if cdchecked = -1 then
  begin
    if I_IsCDRomDrive then
      cdchecked := 1
    else
      cdchecked := M_CheckParm('-cdrom');
    if cdchecked > 0 then
      MakeDir(CD_WORKDIR);
  end;
  result := cdchecked > 0;
end;

procedure M_InitArgv;
var
  i: integer;
  defargv: string;
  cmdln: string;

  procedure LoadFromDefaultCMD(trywrite: boolean);
  var
    i: integer;
    str: TDStringList;
    tmp: string;
    defargs, defargs2: string;
    p: integer;
  begin
    str := TDStringList.Create;
    try
      if fexists(defargv) then
      begin
        str.LoadFromFile(defargv);
        for i := str.Count - 1 downto 0 do
        begin
          tmp := str[i];
          p := Pos('// ', tmp);
          if p = 1 then
            str.Delete(i)
          else if p > 0 then
          begin
            tmp := Copy(tmp, 1, p - 1);
            str[i] := tmp;
          end;
        end;
        defargs := str.Text;
        defargs2 := '';
        for i := 1 to Length(defargs) do
        begin
          if defargs[i] = ' ' then
            defargs2 := defargs2 + #13#10
          else
            defargs2 := defargs2 + toupper(defargs[i]);
        end;
        str.Text := defargs2;
        myargv[myargc] := '-DEFAULTCMDMARKER';
        inc(myargc);
        for i := 0 to str.Count - 1 do
        begin
          if myargc > MAXARGS then
            exit;
          if strtrim(str[i]) <> '' then
          begin
            myargv[myargc] := strtrim(str[i]);
            inc(myargc);
          end;
        end;
      end
      else if trywrite then
      begin
        str.Add('// Add default command line parameters to this file.');
        str.Add('// These command line parameters will be appended everytime you run DelphiDoom.');
        str.Add(strlower(fname(myargv[0])) + ' ');
        str.SaveToFile(defargv);
      end;
    finally
      str.Free;
    end;
  end;

begin
  myargc := ParamCount + 1;
  for i := 0 to myargc - 1 do
  begin
    myargv[i] := strupper(ParamStr(i));
    if i = MAXARGS then
    begin
      myargc := i + 1;
      exit;
    end;
  end;

  defargv := 'Doom32.cmd';
  LoadFromDefaultCMD(not I_IsCDRomDrive);

  if I_IsCDRomDrive then
  begin
    defargv := M_SaveFileName('Doom32.cmd');
    LoadFromDefaultCMD(not I_IsCDRomDrive(defargv[1]));
  end;

  for i := myargc to MAXARGS do
    myargv[i] := '';

  cmdln := fname(myargv[0]);
  for i := 1 to myargc - 1 do
    cmdln := cmdln + ' ' + myargv[i];
  printf('%s'#13#10, [cmdln]);

end;

procedure M_CmdShowCommandLineParams(const parm: string);
var
  i: integer;
  mlist: TDStringList;
  mask: string;
begin
  if parm = '' then
    mask := '*'
  else
    mask := parm;
  mlist := C_GetMachingList(cmdparams, mask);
  printf('Command line parameters: '#13#10);
  for i := 0 to mlist.Count - 1 do
    printf(' %s'#13#10, [mlist[i]]);
  mlist.Free;
end;

procedure M_CmdShowCmdline(const parm: string);
var
  i: integer;
begin
  for i := 1 to myargc - 1 do
    printf('%s ', [myargv[i]]);
  printf(#13#10);
end;

function M_SaveFileName(const filename: string): string;
begin
  if M_CheckParmCDROM then
    result := CD_WORKDIR + filename
  else
    result := filename;
end;

initialization
  cmdparams := TDStringList.Create;

finalization
{$IFDEF DEBUG}
  cmdparams.SaveToFile('cmdparams.txt');
{$ENDIF}
  cmdparams.Free;

end.

