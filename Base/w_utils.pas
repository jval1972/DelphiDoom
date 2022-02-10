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

unit w_utils;

interface

//==============================================================================
//
// W_RegisterUtilityCommands
//
//==============================================================================
procedure W_RegisterUtilityCommands;

implementation

uses
  d_delphi,
  c_cmds,
  i_system,
  m_argv,
  m_misc,
  w_wad,
  z_zone;

//==============================================================================
//
// W_CmdLumpLen
//
//==============================================================================
procedure W_CmdLumpLen(const name: string);
var
  lump: integer;
  len: integer;
begin
  if name = '' then
  begin
    printf('Please specify the lump name'#13#10);
    exit;
  end;

  lump := atoi(name, -1);
  if lump = -1 then
    lump := W_CheckNumForName(name);
  if lump = -1 then
    printf('Lump %s does not exist'#13#10)
  else
  begin
    len := W_LumpLength(lump);
    printf('Lump %s size = %dKB (%d bytes)'#13#10, [name, len div 1024, len]);
  end;
end;

//==============================================================================
//
// W_CmdCheckNumForName
//
//==============================================================================
procedure W_CmdCheckNumForName(const name: string);
var
  lump: integer;
begin
  if name = '' then
  begin
    printf('Please specify the lump name'#13#10);
    exit;
  end;

  lump := W_CheckNumForName(name);
  if lump = -1 then
    printf('Lump %s does not exist'#13#10)
  else
    printf('Lump %s num = %d'#13#10, [lump]);
end;

//==============================================================================
//
// W_CmdNumLumps
//
//==============================================================================
procedure W_CmdNumLumps;
begin
  printf('%d total lumps'#13#10, [W_NumLumps]);
end;

//==============================================================================
//
// W_CmdSaveLumpToDisk
//
//==============================================================================
procedure W_CmdSaveLumpToDisk(const lumpname: string; const filename: string);
var
  fname: string;
  p: pointer;
  lump: integer;
  len: integer;
begin
  if (lumpname = '') or (filename = '') then
  begin
    printf('Usage is:'#13#10);
    printf('  savelumptodisk [lumpname] [filename]'#13#10);
    exit;
  end;

  if CharPos('.', filename) = 0 then
    fname := filename + '.lmp'
  else
    fname := filename;

  fname := M_SaveFileName(fname);

  lump := W_CheckNumForName(lumpname);
  if lump = -1 then
  begin
    printf('Lump %s not found.'#13#10, [lumpname]);
    exit;
  end;

  len := W_LumpLength(lump);
  p := Z_Malloc(len, PU_STATIC, nil);
  W_ReadLump(lump, p);

  if M_WriteFile(fname, p, len) then
    printf('Lump %s saved to file %s (%d bytes).'#13#10, [lumpname, fname, len])
  else
    I_Warning('Can not save lump %s to file %s.'#13#10, [lumpname, fname]);

  Z_Free(p);
end;

//==============================================================================
//
// W_RegisterUtilityCommands
//
//==============================================================================
procedure W_RegisterUtilityCommands;
begin
  C_AddCmd('lumpsize, lumplength, lumplen', @W_CmdLumpLen);
  C_AddCmd('lumpnum, checknumforname', @W_CmdCheckNumForName);
  C_AddCmd('numlumps', @W_CmdNumLumps);
  C_AddCmd('savelumptodisk', @W_CmdSaveLumpToDisk);
end;

end.

