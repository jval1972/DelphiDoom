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

unit acs_common;

interface

const
  MAX_IDENTIFIER_LENGTH = 32;
  MAX_QUOTED_LENGTH = 256;
  MAX_FILE_NAME_LENGTH = 512;
  MAX_SCRIPT_COUNT = 64;
  MAX_MAP_VARIABLES = 32;
  MAX_SCRIPT_VARIABLES = 10;
  MAX_WORLD_VARIABLES = 64;
  MAX_STRINGS = 128;
  DEFAULT_OBJECT_SIZE = 65536;
  ASCII_SPACE = 32;
  ASCII_QUOTE = 34;
  ASCII_UNDERSCORE = 95;
  EOF_CHARACTER = 127;
  DIRECTORY_DELIMITER = '\';

type
  S_BYTE = ShortInt;
  U_BYTE = Byte;
  S_WORD = SmallInt;
  U_WORD = Word;
  S_LONG = Integer;
  U_LONG = LongWord;

implementation

end.
