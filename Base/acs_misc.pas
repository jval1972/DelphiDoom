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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit acs_misc;

interface

uses
  acs_common;

const
  MSG_NORMAL = 0;
  MSG_VERBOSE = 1;
  MSG_DEBUG = 2;

//==============================================================================
//
// ACS_LoadFile
//
//==============================================================================
function ACS_LoadFile(const name: string; var buffer: pointer): integer;

//==============================================================================
//
// ACS_LoadLump
//
//==============================================================================
function ACS_LoadLump(const lump: integer; var buffer: pointer): integer;

//==============================================================================
//
// ACS_SaveFile
//
//==============================================================================
function ACS_SaveFile(const name: string; const buffer: pointer; const len: integer): boolean;

//==============================================================================
//
// ACS_StripFileExt
//
//==============================================================================
procedure ACS_StripFileExt(var name: string);

//==============================================================================
//
// ACS_SuggestFileExt
//
//==============================================================================
procedure ACS_SuggestFileExt(var base: string; const extension: string);

//==============================================================================
//
// ACS_Message
//
//==============================================================================
procedure ACS_Message(const typ: integer; const fmt: string; const args: array of const);

//==============================================================================
//
// ACS_Alloc
//
//==============================================================================
function ACS_Alloc(const size: integer; const error: integer): pointer;

//==============================================================================
//
// ACS_Free
//
//==============================================================================
procedure ACS_Free(var ptr: Pointer);

//==============================================================================
//
// ACS_LittleULONG
//
//==============================================================================
function ACS_LittleULONG(const v: U_LONG): U_LONG;

implementation

uses
  d_delphi,
  acs,
  acs_error,
  w_wad,
  w_pak,
  z_zone;

const
  ASCII_SLASH = 47;
  ASCII_BACKSLASH = 92;
  O_BINARY = 0;

//==============================================================================
//
// ACS_Alloc
//
//==============================================================================
function ACS_Alloc(const size: integer; const error: integer): pointer;
begin
  result := Z_Malloc(size, PU_STATIC, nil);
  if result = nil then
    ERR_Exit(error, false, '', []);
  ZeroMemory(result, size);
end;

//==============================================================================
//
// ACS_Free
//
//==============================================================================
procedure ACS_Free(var ptr: Pointer);
begin
  Z_Free(ptr);
end;

//==============================================================================
//
// ACS_LittleUWORD
//
// Converts a host U_WORD (2 bytes) to little endian byte order.
//
//==============================================================================
function ACS_LittleUWORD(const v: U_WORD): U_WORD;
begin
  if not acs_BigEndianHost then
  begin
    result := v;
    exit;
  end;
  result := (v and 255) shl 8 + (v shr 8) and 255;
end;

//==============================================================================
//
// ACS_LittleULONG
//
// Converts a host U_LONG (4 bytes) to little endian byte order.
//
//==============================================================================
function ACS_LittleULONG(const v: U_LONG): U_LONG;
begin
  if not acs_BigEndianHost then
  begin
    result := v;
    exit;
   end;
  result := (v and 255) shl 24 + (((v shr 8) and 255) shl 16) + (((v shr 16) and 255) shl 8) +
    (v shr 24) and 255;
end;

//==============================================================================
//
// ACS_LoadFile
//
//==============================================================================
function ACS_LoadFile(const name: string; var buffer: pointer): integer;
var
  size, cnt: integer;
  strm: TPakStream;
begin
  strm := TPakStream.Create(name, pm_short);
  if strm.IOResult <> 0 then
  begin
    strm.Free;
    ERR_Exit(ERR_CANT_OPEN_FILE, false, 'File: ''%s''.', [name]);
    result := 0;
    exit;
  end;

  size := strm.Size;
  buffer := ACS_Alloc(size, ERR_ALLOC_SCRIPT_BUFFER);
  if buffer = nil then
    ERR_Exit(ERR_NONE, false, 'Couldn''t malloc %d bytes for file ''%s''.', [size, name]);

  strm.Seek(0, sFromBeginning);
  cnt := strm.Read(buffer^, size);
  strm.Free;
  if cnt < size then
    ERR_Exit(ERR_CANT_READ_FILE, false, 'File: ''%s''.', [name]);

  result := size;
end;

//==============================================================================
//
// ACS_LoadLump
//
//==============================================================================
function ACS_LoadLump(const lump: integer; var buffer: pointer): integer;
var
  src: string;
  i: integer;
  size: integer;
  pb: PByteArray;
begin
  src := W_TextLumpNum(lump);
  size := Length(src);
  buffer := ACS_Alloc(size, ERR_ALLOC_SCRIPT_BUFFER);
  if buffer = nil then
    ERR_Exit(ERR_NONE, false, 'Couldn''t malloc %d bytes for lump ''%d''.', [size, lump]);
  pb := buffer;
  for i := 1 to size do
    pb[i - 1] := Ord(src[i]);
  result := size;
end;

//==============================================================================
//
// ACS_SaveFile
//
//==============================================================================
function ACS_SaveFile(const name: string; const buffer: pointer; const len: integer): boolean;
var
  handle: file;
begin
  if not fopen(handle, name, fCreate) then
  begin
    result := false;
    exit;
  end;

  result := fwrite(buffer, len, 1, handle);
  close(handle);
end;

//==============================================================================
//
// ACS_SuggestFileExt
//
//==============================================================================
procedure ACS_SuggestFileExt(var base: string; const extension: string);
var
  i, len: integer;
begin
  len := Length(base);
  for i := len downto 1 do
  begin
    if base[i] in ['\', '/'] then
      break;
    if base[i] = '.' then
    begin
      SetLength(base, i - 1);
      break;
    end;
  end;
  if CharPos('.', extension) = 1 then
    base := base + extension
  else
    base := base + '.' + extension;
end;

//==============================================================================
//
// ACS_StripFileExt
//
//==============================================================================
procedure ACS_StripFileExt(var name: string);
var
  i: integer;
begin
  for i := Length(name) downto 1 do
  begin
    if name[i] = '.' then
      SetLength(name, i - 1)
    else if name[i] in ['\', '/'] then
      break;
  end;
end;

//==============================================================================
//
// ACS_Message
//
//==============================================================================
procedure ACS_Message(const typ: integer; const fmt: string; const args: array of const);
begin
  if typ = MSG_VERBOSE then
    if not acs_VerboseMode then
      exit;

  if typ = MSG_DEBUG then
    if not acs_DebugMode then
      exit;

  printf(fmt, args);
end;

end.
