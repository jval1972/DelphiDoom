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

// Free Pascal Compiler compatibility unit

unit d_fpc;

interface
{$IFDEF FPC}

type
  PKeyboardState = ^TKeyboardState;
  TKeyboardState = array[0..255] of byte;

  TImageFileHeader = packed record
    Machine: Word;
    NumberOfSections: Word;
    TimeDateStamp: DWORD;
    PointerToSymbolTable: DWORD;
    NumberOfSymbols: DWORD;
    SizeOfOptionalHeader: Word;
    Characteristics: Word;
  end;
  PImageFileHeader = ^TImageFileHeader;

  TImageDataDirectory = record
    VirtualAddress: DWORD;
    Size: DWORD;
  end;
  PImageDataDirectory = ^TImageDataDirectory;

const
  IMAGE_NUMBEROF_DIRECTORY_ENTRIES = 16;

type
  TImageOptionalHeader = packed record
    { Standard fields. }
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: DWORD;
    SizeOfInitializedData: DWORD;
    SizeOfUninitializedData: DWORD;
    AddressOfEntryPoint: DWORD;
    BaseOfCode: DWORD;
    BaseOfData: DWORD;
    { NT additional fields. }
    ImageBase: DWORD;
    SectionAlignment: DWORD;
    FileAlignment: DWORD;
    MajorOperatingSystemVersion: Word;
    MinorOperatingSystemVersion: Word;
    MajorImageVersion: Word;
    MinorImageVersion: Word;
    MajorSubsystemVersion: Word;
    MinorSubsystemVersion: Word;
    Win32VersionValue: DWORD;
    SizeOfImage: DWORD;
    SizeOfHeaders: DWORD;
    CheckSum: DWORD;
    Subsystem: Word;
    DllCharacteristics: Word;
    SizeOfStackReserve: DWORD;
    SizeOfStackCommit: DWORD;
    SizeOfHeapReserve: DWORD;
    SizeOfHeapCommit: DWORD;
    LoaderFlags: DWORD;
    NumberOfRvaAndSizes: DWORD;
    DataDirectory: packed array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1] of TImageDataDirectory;
  end;
  PImageOptionalHeader = ^TImageOptionalHeader;

//==============================================================================
//
// GetEnvironmentVariable
//
//==============================================================================
function GetEnvironmentVariable(lpName: PChar; lpBuffer: PChar; nSize: DWORD): DWORD; stdcall;

//==============================================================================
//
// AllocMemSize
//
//==============================================================================
function AllocMemSize: integer;

{$ENDIF}
implementation
{$IFDEF FPC}

//==============================================================================
//
// GetEnvironmentVariable
//
//==============================================================================
function GetEnvironmentVariable(lpName: PChar; lpBuffer: PChar; nSize: DWORD): DWORD; stdcall; external 'kernel32.dll' name 'GetEnvironmentVariableA';

//==============================================================================
//
// AllocMemSize
//
//==============================================================================
function AllocMemSize: integer;
begin
  result := 0;
end;

{$ENDIF}
end.
