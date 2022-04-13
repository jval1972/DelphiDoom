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

unit v_data;

interface

uses
  d_delphi;

const
//
// VIDEO
//

// drawing stuff
//
// Background and foreground screen numbers
//
  SCN_320x200 = -1;
  SCN_FG = 0;
  SCN_BG = 1;
  SCN_CON = 2;    // Console Screen Buffer
  SCN_TMP = 3;    // Temporary Screen Buffer 320x200
  SCN_TMP426 = 4; // Temporary Screen Buffer 426x200
  SCN_ST426 = 5;  // Status Bar Screen Buffer (426x32)
  SCN_ST2 = 6;    // Status Bar Back Buffer (320x32)
  SCN_ST = 7;     // Status Bar Screen Buffer (320x32)

var
// Screen 0 is the screen updated by I_Update screen.
// Screen 1 is an extra buffer.
// Screen 4 is an extra buffer for finale.
// Screen 5 is used by status line
  screens: array[SCN_FG..SCN_ST] of PByteArray;
  screen32: PLongWordArray;

type
  screendimention_t = record
    width: integer;
    height: integer;
    depth: byte;
  end;

{$IFDEF OPENGL}
var
  GLDRAWWIDTH: integer = 1024;
  GLDRAWHEIGHT: integer = 768;
  GLDRAWTEXWIDTH: integer = 1024;
  GLDRAWTEXHEIGHT: integer = 1024;
{$ENDIF}

const
  FIXED_DIMENTIONS: array[SCN_320x200..SCN_ST] of screendimention_t = (
    (width: 320; height: 200; depth: 1),
    {$IFDEF OPENGL}
    (width: -1; height: -1; depth: 1),
    (width: -1; height: -1; depth: 1),
    (width: -1; height: -1; depth: 1),
    {$ELSE}
    (width:  -1; height:  -1; depth: 1),
    (width:  -1; height:  -1; depth: 1),
    (width:  -1; height:  -1; depth: 1),
    {$ENDIF}
    (width: 320; height: 200; depth: 1),
    (width: 426; height: 200; depth: 1),
    (width: 426; height:  32; depth: 1),
    (width: 320; height:  32; depth: 1),
    (width: 320; height:  32; depth: 1)
  );

var
  screendimentions: array[SCN_FG..SCN_ST] of screendimention_t;

const
  PLAYPAL = 'PLAYPAL';

//==============================================================================
//
// V_ReadPalette
//
//==============================================================================
function V_ReadPalette(tag: integer): PByteArray;

var
  pg_CREDIT: string = 'CREDIT';
  pg_HELP: string = 'HELP';
  pg_HELP1: string = 'HELP1';
  pg_HELP2: string = 'HELP2';
  pg_VICTORY2: string = 'VICTORY2';
  pg_ENDPIC: string = 'ENDPIC';
  pg_TITLE: string = 'TITLEPIC';
  pg_DMENUPIC: string = 'DMENUPIC';

implementation

uses
  w_wad;

const
  playpalnum: integer = -2;

//==============================================================================
// V_ReadPalette
//
// JVAL
// Reads the 'PLAYPAL' lump, optimized, keep lump number
//
//==============================================================================
function V_ReadPalette(tag: integer): PByteArray;
begin
  if playpalnum < 0 then
    playpalnum := W_GetNumForName(PLAYPAL);
  result := W_CacheLumpNum(playpalnum, tag);
end;

end.

