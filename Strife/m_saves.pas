//------------------------------------------------------------------------------
//
//  DelphiStrife is a source port of the game Strife.
//
//  Based on:
//    - Linux Doom by "id Software"
//    - Chocolate Strife by "Simon Howard"
//    - DelphiDoom by "Jim Valavanis"
//
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2005 Simon Howard
//  Copyright (C) 2010 James Haley, Samuel Villarreal
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
// DESCRIPTION:
// Strife Hub Saving Code
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit m_saves;

interface

//
// File Paths
//
// Strife maintains multiple file paths related to savegames.
//
var
  savepath: string;     // The actual path of the selected saveslot
  savepathtemp: string; // The path of the temporary saveslot (strfsav6.ssg)
  loadpath: string;     // Path used while loading the game

var
  character_name: string; // Name of "character" for saveslot

//==============================================================================
//
// M_SafeFilePath
//
//==============================================================================
function M_SafeFilePath(basepath, newcomponent: string): string;

//==============================================================================
//
// M_MakeStrifeSaveDir
//
//==============================================================================
function M_MakeStrifeSaveDir(const slotnum: integer; const extra: string): string;

//==============================================================================
//
// M_ToCurr
//
//==============================================================================
procedure M_ToCurr;

//==============================================================================
//
// M_FromCurr
//
//==============================================================================
procedure M_FromCurr;

//==============================================================================
//
// M_ClearSlot
//
//==============================================================================
procedure M_ClearSlot;

//==============================================================================
//
// M_ClearTmp
//
//==============================================================================
procedure M_ClearTmp;

//==============================================================================
//
// M_SaveMoveHereToMap
//
//==============================================================================
procedure M_SaveMoveHereToMap;

//==============================================================================
//
// M_ReadMisObj
//
//==============================================================================
procedure M_ReadMisObj;

//==============================================================================
//
// M_ReadWorldVars
//
//==============================================================================
procedure M_ReadWorldVars;

//==============================================================================
//
// M_SaveMoveMapToHere
//
//==============================================================================
procedure M_SaveMoveMapToHere;

//==============================================================================
//
// M_SaveMisObj
//
//==============================================================================
procedure M_SaveMisObj(const path: string);

//==============================================================================
//
// M_SaveWorldVars
//
//==============================================================================
procedure M_SaveWorldVars(const path: string);

//==============================================================================
//
// M_SaveSaveScreenShot
//
//==============================================================================
procedure M_SaveSaveScreenShot(const path: string);

//==============================================================================
//
// M_CreateSaveDirs
//
//==============================================================================
procedure M_CreateSaveDirs(const savedir: string);

//==============================================================================
//
// M_ClearSlotNum
//
//==============================================================================
procedure M_ClearSlotNum(const num: integer);

//==============================================================================
//
// M_CopySlotNum
//
//==============================================================================
procedure M_CopySlotNum(src, dest: integer);

implementation

uses
  d_delphi,
  i_system,
  p_dialog,
  p_saveg,
  m_argv,
  g_game;

//==============================================================================
//
// M_ClearSlotNum
//
//==============================================================================
procedure M_ClearSlotNum(const num: integer);
var
  files: TDSTringList;
  filepath: string;
  i: integer;
  path: string;
begin
  path := M_SafeFilePath(M_SaveFileName(''), M_MakeStrifeSaveDir(num, ''));
  if path = '' then
    I_Error('M_ClearSlotNum(): you fucked up savedir man!');

  files := findfiles(path + '\*');
  if (files = nil) or (files.Count = 0) then
    I_Error('M_ClearTmp(): Couldn''t open dir %s', [path]);

  for i := 0 to files.Count - 1 do
  begin
    if (files.Strings[i] = '.') or (files.Strings[i] = '..') then
      continue;

    // haleyjd: use M_SafeFilePath, not sprintf
    filepath := M_SafeFilePath(path, files.Strings[i]);
    fdelete(filepath);
  end;
  files.Free;
end;

//==============================================================================
//
// M_CopySlotNum
//
//==============================================================================
procedure M_CopySlotNum(src, dest: integer);
var
  files: TDSTringList;
  srcfilename: string;
  dstfilename: string;
  i: integer;
  srcpath, destpath: string;
begin
  srcpath := M_SafeFilePath(M_SaveFileName(''), M_MakeStrifeSaveDir(src, ''));
  destpath := M_SafeFilePath(M_SaveFileName(''), M_MakeStrifeSaveDir(dest, ''));

  files := findfiles(srcpath + '\*');
  if (files = nil) or (files.Count = 0) then
  begin
    if files <> nil then
      files.Free;
    exit;
  end;

  M_ClearSlotNum(dest);
  for i := 0 to files.Count - 1 do
  begin
    if (files.Strings[i] = '.') or (files.Strings[i] = '..') then
      continue;

    // haleyjd: use M_SafeFilePath, not sprintf
    srcfilename := M_SafeFilePath(srcpath, files.Strings[i]);
    dstfilename := M_SafeFilePath(destpath, files.Strings[i]);
    CopyFile2(srcfilename, dstfilename);
  end;
  files.Free;
end;

//==============================================================================
// M_ClearTmp
//
// ClearTmp
//
// Clear the temporary save directory
//
//==============================================================================
procedure M_ClearTmp;
var
  files: TDSTringList;
  filepath: string;
  i: integer;
begin
  if savepathtemp = '' then
    I_Error('M_ClearTmp(): you fucked up savedir man!');

  files := findfiles(savepathtemp + '\*');
  if (files = nil) or (files.Count = 0) then
    I_Error('M_ClearTmp(): Couldn''t open dir %s', [savepathtemp]);

  for i := 0 to files.Count - 1 do
  begin
    if (files.Strings[i] = '.') or (files.Strings[i] = '..') then
      continue;

    // haleyjd: use M_SafeFilePath, not sprintf
    filepath := M_SafeFilePath(savepathtemp, files.Strings[i]);
    fdelete(filepath);
  end;
  files.Free;
end;

//==============================================================================
// M_ClearSlot
//
// ClearSlot
//
// Clear a single save slot folder
//
//==============================================================================
procedure M_ClearSlot;
var
  files: TDSTringList;
  filepath: string;
  i: integer;
begin
  if savepath = '' then
    I_Error('userdir is fucked up man!');

  files := findfiles(savepath + '\*');
  if (files = nil) or (files.Count = 0) then
    I_Error('M_ClearSlot(): Couldn''t open dir %s', [savepath]);

  for i := 0 to files.Count - 1 do
  begin
    if (files.Strings[i] = '.') or (files.Strings[i] = '..') then
      continue;

    // haleyjd: use M_SafeFilePath, not sprintf
    filepath := M_SafeFilePath(savepath, files.Strings[i]);
    fdelete(filepath);
  end;
  files.Free;
end;

//==============================================================================
// M_FromCurr
//
// FromCurr
//
// Copying files from savepathtemp to savepath
//
//==============================================================================
procedure M_FromCurr;
var
  files: TDSTringList;
  srcfilename: string;
  dstfilename: string;
  i: integer;
begin
  files := findfiles(savepathtemp + '\*');
  if (files = nil) or (files.Count = 0) then
    I_Error('ClearSlot(): Couldn''t open dir %s', [savepathtemp]);

  for i := 0 to files.Count - 1 do
  begin
    if (files.Strings[i] = '.') or (files.Strings[i] = '..') then
      continue;

    // haleyjd: use M_SafeFilePath, not sprintf
    srcfilename := M_SafeFilePath(savepathtemp, files.Strings[i]);
    dstfilename := M_SafeFilePath(savepath, files.Strings[i]);
    CopyFile2(srcfilename, dstfilename);
  end;
  files.Free;
end;

//==============================================================================
// M_ToCurr
//
// ToCurr
//
// Copying files from savepath to savepathtemp
//
//==============================================================================
procedure M_ToCurr;
var
  files: TDSTringList;
  srcfilename: string;
  dstfilename: string;
  i: integer;
begin
  M_ClearTmp;
  files := findfiles(savepath + '\*');
  if (files = nil) or (files.Count = 0) then
    I_Error('ToCurr(): Couldn''t open dir %s', [savepath]);

  for i := 0 to files.Count - 1 do
  begin
    if (files.Strings[i] = '.') or (files.Strings[i] = '..') then
      continue;

    // haleyjd: use M_SafeFilePath, not sprintf
    srcfilename := M_SafeFilePath(savepath,     files.Strings[i]);
    dstfilename := M_SafeFilePath(savepathtemp, files.Strings[i]);
    CopyFile2(srcfilename, dstfilename);
  end;
  files.Free;
end;

//==============================================================================
//
// M_SaveMoveMapToHere
//
// Moves a map to the "HERE" save.
//
//==============================================================================
procedure M_SaveMoveMapToHere;
var
 mapsave: string;
 heresave: string;
begin
  // haleyjd: use M_SafeFilePath, not sprintf
  mapsave := M_SafeFilePath(savepath, itoa(gamemap));
  heresave := M_SafeFilePath(savepath, 'here');

  frename(mapsave, heresave);
end;

//==============================================================================
//
// M_SaveMoveHereToMap
//
// Moves the "HERE" save to a map.
//
//==============================================================================
procedure M_SaveMoveHereToMap;
var
 mapsave: string;
 heresave: string;
begin
  // haleyjd: use M_SafeFilePath, not sprintf
  mapsave := M_SafeFilePath(savepathtemp, itoa(gamemap));
  heresave := M_SafeFilePath(savepathtemp, 'here');

  frename(mapsave, heresave);
end;

//==============================================================================
//
// M_SaveMisObj
//
// Writes the mission objective into the MIS_OBJ file.
//
//==============================================================================
procedure M_SaveMisObj(const path: string);
var
 destpath: string;
 l: TDStringList;
begin
  // haleyjd 20110210: use M_SafeFilePath, not sprintf
  destpath := M_SafeFilePath(path, 'mis_obj');
  l := TDStringList.Create;
  l.Text := mission_objective;
  l.SaveToFile(destpath);
  l.Free;
end;

//==============================================================================
//
// M_SaveWorldVars
//
//==============================================================================
procedure M_SaveWorldVars(const path: string);
begin
  P_ArchiveWorldVariables(M_SafeFilePath(path, 'world_vars'));
end;

//==============================================================================
//
// M_SaveSaveScreenShot
//
//==============================================================================
procedure M_SaveSaveScreenShot(const path: string);
begin
  P_ArchiveScreenShot(M_SafeFilePath(path, 'sshot'));
end;

//==============================================================================
//
// M_ReadMisObj
//
// Reads the mission objective from the MIS_OBJ file.
//
//==============================================================================
procedure M_ReadMisObj;
var
  srcpath: string;
  l: TDStringList;
begin
  // haleyjd: use M_SafeFilePath, not sprintf
  srcpath := M_SafeFilePath(savepathtemp, 'mis_obj');
  l := TDStringList.Create;
  l.LoadFromFile(srcpath);
  if l.Count = 0 then
    I_Warning('M_ReadMisObj(): Can not load mission objective'#13#10);
  mission_objective := l.Text;
  l.Free;
end;

//==============================================================================
//
// M_ReadWorldVars
//
//==============================================================================
procedure M_ReadWorldVars;
begin
  P_UnarchiveWorldVariables(M_SafeFilePath(savepathtemp, 'world_vars'));
end;

//==============================================================================
//
// M_NormalizeSlashes
//
// Remove trailing slashes, translate backslashes to slashes
// The string to normalize is passed and returned in str
//
// killough 11/98: rewritten
//
// [STRIFE] - haleyjd 20110210: Borrowed from Eternity and adapted to respect
// the DIR_SEPARATOR define used by Choco Doom. This routine originated in
// BOOM.
//
//==============================================================================
function M_NormalizeSlashes(const str: string): string;
var
  i: integer;
  lastsl: boolean;
  len: integer;
begin
  lastsl := true;
  result := '';
  for i := 1 to length(str) do
  begin
    if not (str[i] in ['/', '\']) then
    begin
      result := result + str[i];
      lastsl := false;
    end
    else
    begin
      if not lastsl then
      begin
        result := result + '\';
        lastsl := true;
      end;
    end;
  end;
  len := length(result);
  if len > 0 then
    if result[len] = '\' then
      setlength(result, len - 1);
end;

//==============================================================================
//
// M_SafeFilePath
//
//==============================================================================
function M_SafeFilePath(basepath, newcomponent: string): string;
begin
  if basepath = '' then
    basepath := '.';

  // Always throw in a slash. M_NormalizeSlashes will remove it in the case
  // that either basepath or newcomponent includes a redundant slash at the
  // end or beginning respectively.
  result := M_NormalizeSlashes(basepath + '\' + newcomponent);
end;

//==============================================================================
//
// M_MakeStrifeSaveDir
//
// haleyjd 20110211: Convenience routine
//
//==============================================================================
function M_MakeStrifeSaveDir(const slotnum: integer; const extra: string): string;
begin
  sprintf(result, 'strfsav%d.ssg%s', [slotnum, extra]);
end;

//==============================================================================
//
// M_CreateSaveDirs
//
// haleyjd 20110210: Vanilla Strife went tits-up if it didn't have the full set
// of save folders which were created externally by the installer. fraggle says
// that's no good for Choco purposes, and I agree, so this routine will create
// the full set of folders under the configured savegamedir.
//
//==============================================================================
procedure M_CreateSaveDirs(const savedir: string);
var
  i: integer;
  compositedir: string;
begin
  for i := 0 to 8 do
  begin
    // compose the full path by concatenating with savedir
    compositedir := M_SafeFilePath(savedir, M_MakeStrifeSaveDir(i, ''));

    if not MkDir(compositedir) then
      I_Warning('M_CreateSaveDirs(): Can not create directory %s'#13#10, [compositedir]);
  end;
end;

end.
