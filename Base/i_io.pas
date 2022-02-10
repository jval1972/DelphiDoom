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

unit i_io;

interface

uses
  d_delphi;

var
  debugfile: TFile;
  stderr: TFile;
  stdout: TFile;
  stdoutbuffer: TDStringList;

//==============================================================================
//
// I_InitializeIO
//
//==============================================================================
procedure I_InitializeIO;

//==============================================================================
//
// I_ShutDownIO
//
//==============================================================================
procedure I_ShutDownIO;

//==============================================================================
//
// I_IOMessageBox
//
//==============================================================================
procedure I_IOMessageBox(const s: string);

//==============================================================================
//
// I_IOErrorMessageBox
//
//==============================================================================
procedure I_IOErrorMessageBox(const s: string);

//==============================================================================
//
// I_IOprintf
//
//==============================================================================
procedure I_IOprintf(const s: string);

//==============================================================================
//
// I_IOSetWindowHandle
//
//==============================================================================
procedure I_IOSetWindowHandle(const handle: integer);

implementation

uses
  Windows,
  d_main,
  g_game,
  i_mainwindow,
{$IFNDEF FPC}
  i_startup,
{$ENDIF}
  m_argv;

var
  msghandle: integer = 0;

//==============================================================================
//
// I_IOMessageBox
//
//==============================================================================
procedure I_IOMessageBox(const s: string);
begin
  MessageBox(msghandle, PChar(s), AppTitle, MB_OK);
end;

//==============================================================================
//
// I_IOErrorMessageBox
//
//==============================================================================
procedure I_IOErrorMessageBox(const s: string);
begin
  MessageBox(msghandle, PChar(s), AppTitle, MB_OK or MB_ICONERROR or MB_APPLMODAL);
end;

var
  io_lastNL: boolean = true;

//==============================================================================
//
// I_IOprintf
//
//==============================================================================
procedure I_IOprintf(const s: string);
var
  i: integer;
  l: TDStringList;
  len: integer;
begin
  len := Length(s);
  if len = 0 then
    exit;

  l := TDStringList.Create;
  l.Text := s;

  if (len = 1) and (s[1] = #13) then
  begin
    stdoutbuffer.Add('');
    io_lastNL := true;
  end
  else
  begin
    if io_lastNL or (stdoutbuffer.Count = 0) then
      stdoutbuffer.AddStrings(l)
    else
    begin
      stdoutbuffer.Strings[stdoutbuffer.Count - 1] :=
        stdoutbuffer.Strings[stdoutbuffer.Count - 1] + l.Strings[0];
      for i := 1 to l.Count - 1 do
        stdoutbuffer.Add(l.Strings[i]);
    end;
    io_lastNL := s[len] = #13;
  end;

  l.Free;

  if IsConsole then
    write(s);
end;

const
{$IFDEF DOOM}
  basename = 'Doom';
{$ENDIF}
{$IFDEF HERETIC}
  basename = 'Heretic';
{$ENDIF}
{$IFDEF HEXEN}
  basename = 'Hexen';
{$ENDIF}
{$IFDEF STRIFE}
  basename = 'Strife';
{$ENDIF}

//==============================================================================
//
// I_InitializeIO
//
//==============================================================================
procedure I_InitializeIO;
var
  dfilename: string;
  efilename: string;
  sfilename: string;
begin
  if M_CheckParm('-debugfile') <> 0 then
{$IFDEF OPENGL}
    sprintf(dfilename, 'DATA\LOGS\GL%s32_debug%d.txt', [basename, consoleplayer])
  else
    sprintf(dfilename, 'DATA\LOGS\GL%s32_debug.txt', [basename]);
  sprintf(efilename, 'DATA\LOGS\GL%s32_stderr.txt', [basename]);
  sprintf(sfilename, 'DATA\LOGS\GL%s32_stdout.txt', [basename]);
{$ELSE}
    sprintf(dfilename, 'DATA\LOGS\%s32_debug%d.txt', [basename, consoleplayer])
  else
    sprintf(dfilename, 'DATA\LOGS\%s32_debug.txt', [basename]);
  sprintf(efilename, 'DATA\LOGS\%s32_stderr.txt', [basename]);
  sprintf(sfilename, 'DATA\LOGS\%s32_stdout.txt', [basename]);
{$ENDIF}

  MkDir(M_SaveFileName('DATA'));
  MkDir(M_SaveFileName('DATA\LOGS'));

  if M_CheckParmCDROM then
  begin
    dfilename := CD_WORKDIR + dfilename;
    efilename := CD_WORKDIR + efilename;
    sfilename := CD_WORKDIR + sfilename;
  end;

  printf(' error output to: %s'#13#10, [efilename]);
  stderr := TFile.Create(efilename, fCreate);
  printf(' debug output to: %s'#13#10, [dfilename]);
  debugfile := TFile.Create(dfilename, fCreate);
  printf(' standard output to: %s'#13#10, [sfilename]);
  stdout := TFile.Create(sfilename, fCreate);
end;

//==============================================================================
//
// I_ShutDownIO
//
//==============================================================================
procedure I_ShutDownIO;
begin
  stderr.Free;
  debugfile.Free;
  stdout.Free;
  {$IFNDEF FPC}
  SUC_Close;
  {$ENDIF}
end;

//==============================================================================
//
// I_IOSetWindowHandle
//
//==============================================================================
procedure I_IOSetWindowHandle(const handle: integer);
begin
  if handle > 0 then
    msghandle := handle
  else
    msghandle := hMainWnd
end;

initialization

  stdoutbuffer := TDStringList.Create;

finalization

  stdoutbuffer.Free;

end.
