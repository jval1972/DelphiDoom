//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2019 by Jim Valavanis
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
// Random number LUT.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit m_rnd;

{$IFDEF DEBUGRANDOM}
{$DEFINE DEBUG}
{$ENDIF}

interface

// Returns a number from 0 to 255,
// from a lookup table.
function M_Random: integer;

// As M_Random, but used only by the play simulation.
function P_Random: integer;

// JVAL: As P_Random, but used only if no compatibility mode.
function N_Random: integer;

function I_Random: integer;

// JVAL: Using custom seed
function C_Random(var idx: integer): integer;

// Fix randoms for demos.
procedure M_ClearRandom;

procedure P_SaveRandom;

procedure P_RestoreRandom;

// JVAL: Random number for seed
function P_RandomFromSeed(const seed: integer): integer;

var
  rndindex: integer = 0;
  prndindex: integer = 0;
  nrndindex: integer = 0; // JVAL new random index

implementation

uses
  {$IFDEF DEBUG}
  d_delphi,
  g_game,
  {$ENDIF}
  i_system,
  m_stack;

const
  rndtable: array[0..255] of byte = (
      0,   8, 109, 220, 222, 241, 149, 107,  75, 248, 254, 140,  16,  66,
     74,  21, 211,  47,  80, 242, 154,  27, 205, 128, 161,  89,  77,  36,
     95, 110,  85,  48, 212, 140, 211, 249,  22,  79, 200,  50,  28, 188,
     52, 140, 202, 120,  68, 145,  62,  70, 184, 190,  91, 197, 152, 224,
    149, 104,  25, 178, 252, 182, 202, 182, 141, 197,   4,  81, 181, 242,
    145,  42,  39, 227, 156, 198, 225, 193, 219,  93, 122, 175, 249,   0,
    175, 143,  70, 239,  46, 246, 163,  53, 163, 109, 168, 135,   2, 235,
     25,  92,  20, 145, 138,  77,  69, 166,  78, 176, 173, 212, 166, 113,
     94, 161,  41,  50, 239,  49, 111, 164,  70,  60,   2,  37, 171,  75,
    136, 156,  11,  56,  42, 146, 138, 229,  73, 146,  77,  61,  98, 196,
    135, 106,  63, 197, 195,  86,  96, 203, 113, 101, 170, 247, 181, 113,
     80, 250, 108,   7, 255, 237, 129, 226,  79, 107, 112, 166, 103, 241,
     24, 223, 239, 120, 198,  58,  60,  82, 128,   3, 184,  66, 143, 224,
    145, 224,  81, 206, 163,  45,  63,  90, 168, 114,  59,  33, 159,  95,
     28, 139, 123,  98, 125, 196,  15,  70, 194, 253,  54,  14, 109, 226,
     71,  17, 161,  93, 186,  87, 244, 138,  20,  52, 123, 251,  26,  36,
     17,  46,  52, 231, 232,  76,  31, 221,  84,  37, 216, 165, 212, 106,
    197, 242,  98,  43,  39, 175, 254, 145, 190,  84, 118, 222, 187, 136,
    120, 163, 236, 249
  );

// Which one is deterministic?
function M_Random: integer;
begin
  rndindex := (rndindex + 1) and $ff;
  result := rndtable[rndindex];
  {$IFDEF DEBUG}
  printf('M_Random(): %3d, tic=%10d, seed = %3d'#13#10, [Result, gametic, rndindex]);
  {$ENDIF}
end;

function P_Random: integer;
begin
  prndindex := (prndindex + 1) and $ff;
  result := rndtable[prndindex];
  {$IFDEF DEBUG}
  printf('P_Random(): %3d, tic=%10d, seed = %3d'#13#10, [Result, gametic, prndindex]);
  {$ENDIF}
end;

function N_Random: integer;
begin
  nrndindex := (nrndindex + 1) and $ff;
  result := rndtable[nrndindex];
  {$IFDEF DEBUG}
  printf('N_Random(): %3d, tic=%10d, seed = %3d'#13#10, [Result, gametic, nrndindex]);
  {$ENDIF}
end;

function I_Random: integer;
begin
  result := Random(256);
  {$IFDEF DEBUG}
  printf('I_Random(): %3d, tic=%10d'#13#10, [Result, gametic]);
  {$ENDIF}
end;

function C_Random(var idx: integer): integer;
begin
  idx := (idx + 1) and $ff;
  result := rndtable[idx];
  {$IFDEF DEBUG}
  printf('C_Random(): %3d, tic=%10d, seed = %3d'#13#10, [Result, gametic, idx]);
  {$ENDIF}
end;

function P_RandomFromSeed(const seed: integer): integer;
begin
  result := rndtable[seed and $ff];
  {$IFDEF DEBUG}
  printf('P_RandomFromSeed(): %3d, tic=%10d, seed = %3d'#13#10, [Result, gametic, seed]);
  {$ENDIF}
end;

var
  stack: TIntegerStack;

procedure M_ClearRandom;
begin
  rndindex := 0;
  prndindex := 0;
  nrndindex := 0;
  stack.Clear;
end;

procedure P_SaveRandom;
begin
  stack.Push(prndindex);
end;

procedure P_RestoreRandom;
begin
  if not stack.Pop(prndindex) then
    I_DevError('P_RestoreRandom(): Stack is empty!'#13#10);
end;

initialization
  stack := TIntegerStack.Create;
  Randomize;

finalization
  stack.Free;

end.

