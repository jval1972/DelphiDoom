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

unit c_utils;

interface

//==============================================================================
//
// C_RegisterUtilityCommands
//
//==============================================================================
procedure C_RegisterUtilityCommands;

implementation

uses
  d_delphi,
  c_cmds,
  doomdef,
{$IFDEF OPENGL}
  gl_main,
{$ELSE}
  i_video,
{$ENDIF}
  i_displaymodes,
  i_system;

//==============================================================================
//
// C_CmdDir
//
//==============================================================================
procedure C_CmdDir(const parm1, parm2: string);
var
  mask: string;
  mask1, mask2: string;
  files: TDStringList;
  i: integer;
begin
  mask := parm1;
  if mask = '' then
    mask := '*.*';

  files := findfiles(mask);
  try
    for i := 0 to files.Count - 1 do
      printf('%s'#13#10, [files[i]]);
  finally
    files.Free;
  end;

  mask := parm2;
  if mask = '' then
    exit;

  splitstring_ch(mask, mask1, mask2);
  C_CmdDir(mask1, mask2);

end;

//==============================================================================
//
// C_CmdCD
//
//==============================================================================
procedure C_CmdCD;
var
  cd: string;
begin
  GetDir(0, cd);
  printf('%s'#13#10, [cd]);
end;

//==============================================================================
//
// C_CmdCat
//
//==============================================================================
procedure C_CmdCat(const parm1, parm2: string);
var
  files: TDStringList;
  s: TDStringList;
  mask: string;
  i, j: integer;
  mask1, mask2: string;
begin
  mask := parm1;
  if mask = '' then
  begin
    printf('Please specify the files to display.'#13#10);
    exit;
  end;

  files := findfiles(mask);

  s := TDSTringList.Create;
  try
    for i := 0 to files.Count - 1 do
    begin
      s.LoadFromFile(files[i]);
      for j := 0 to s.Count - 1 do
        printf('%s'#13#10, [s[j]]);
      s.Clear;
    end;
  finally
    s.Free;
  end;
  files.Free;

  mask := parm2;
  if mask = '' then
    exit;

  splitstring_ch(mask, mask1, mask2);
  C_CmdCat(mask1, mask2);

end;

//==============================================================================
//
// C_CmdGoToWebPage
//
//==============================================================================
procedure C_CmdGoToWebPage(const parm: string);
begin
  if fullscreen {$IFNDEF OPENGL}= FULLSCREEN_EXCLUSIVE {$ENDIF} then
  {$IFDEF OPENGL}
    GL_ChangeFullScreen(false);
  {$ELSE}
    I_ChangeFullScreen(FULLSCREEN_SHARED);
  {$ENDIF}
  I_GoToWebPage(parm);
end;

//==============================================================================
//
// C_CmdHomepage
//
//==============================================================================
procedure C_CmdHomepage;
begin
  C_CmdGoToWebPage('http://sourceforge.net/projects/delphidoom/');
end;

//==============================================================================
//
// C_CmdHelp
//
//==============================================================================
procedure C_CmdHelp;
begin
  C_CmdGoToWebPage('http://sourceforge.net/projects/delphidoom/');
end;

//==============================================================================
//
// C_CmdForums
//
//==============================================================================
procedure C_CmdForums;
begin
  C_CmdGoToWebPage('http://delphidoom.forumotion.com/index.htm');
end;

//==============================================================================
//
// C_CmdGetLatestVersion
//
//==============================================================================
procedure C_CmdGetLatestVersion;
begin
  C_ExecuteCmd('ver');
  C_CmdGoToWebPage('http://sourceforge.net/projects/delphidoom/files/');
end;

//==============================================================================
//
// C_RegisterUtilityCommands
//
//==============================================================================
procedure C_RegisterUtilityCommands;
begin
  C_AddCmd('dir, ls', @C_CmdDir);
  C_AddCmd('cd, pwd', @C_CmdCD);
  C_AddCmd('type, cat', @C_CmdCat);
  C_AddCmd('gotowebpage', @C_CmdGoToWebPage);
  C_AddCmd('homepage', @C_CmdHomepage);
  C_AddCmd('help, documentation', @C_CmdHelp);
  C_AddCmd('getlatestversion, downloadlatestversion', @C_CmdGetLatestVersion);
  C_AddCmd('forum, forums', @C_CmdForums);
end;

end.
