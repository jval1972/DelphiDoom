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

{$IFNDEF HEXEN}
function Sys_Random: integer;
{$ENDIF}

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
{$IFNDEF HEXEN}
  sysrndindex: integer = 0;
  sysrndseed: integer = 0;
{$ENDIF}  

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

{$IFNDEF HEXEN}
const
  SYSRNDSIZE = 521;

  sysrndtable: array[0..SYSRNDSIZE - 1] of byte = (
    $C2, $26, $56, $BD, $1B, $BE, $87, $DA, $BA, $71, $B8, $DA, $C5, $1F, $A5,
    $03, $55, $A6, $39, $73, $84, $CF, $8E, $63, $AA, $37, $F6, $71, $BA, $D5,
    $0A, $27, $EE, $FC, $17, $4D, $28, $EB, $70, $A3, $B1, $52, $6F, $24, $53,
    $65, $33, $ED, $E0, $4D, $69, $5A, $B1, $2C, $AE, $0B, $A5, $F3, $46, $CE,
    $B0, $C0, $50, $AE, $0C, $8D, $A8, $9D, $83, $55, $10, $7B, $80, $0A, $72,
    $75, $C7, $F7, $32, $EB, $12, $6A, $F7, $79, $EF, $C2, $5C, $B2, $94, $3F,
    $72, $B6, $8E, $2B, $FC, $BD, $82, $3E, $D9, $25, $5F, $55, $FE, $BC, $BA,
    $DC, $F7, $3C, $30, $7D, $D1, $3F, $0A, $00, $DD, $18, $8A, $6B, $BC, $62,
    $84, $C1, $9A, $2B, $3B, $00, $86, $06, $AA, $35, $4F, $7F, $A2, $C8, $66,
    $9F, $69, $52, $88, $8F, $D1, $A5, $50, $8C, $DF, $DB, $EB, $A5, $82, $86,
    $84, $08, $F8, $65, $34, $D1, $D5, $2F, $77, $A1, $D5, $5F, $85, $6A, $E3,
    $0C, $08, $44, $07, $37, $04, $24, $20, $1F, $04, $DA, $99, $87, $C4, $B0,
    $89, $F5, $FD, $75, $E5, $58, $06, $F3, $AC, $AA, $6F, $06, $45, $6A, $C8,
    $CA, $73, $06, $37, $AE, $86, $9C, $9D, $57, $C8, $A1, $55, $96, $35, $56,
    $E0, $ED, $09, $1D, $04, $5E, $B6, $0C, $2F, $7F, $45, $69, $21, $14, $32,
    $CA, $38, $A3, $51, $1E, $DC, $C4, $49, $D6, $D9, $28, $A7, $E4, $C1, $90,
    $A5, $5E, $8A, $36, $1B, $E8, $0D, $5D, $49, $39, $1D, $06, $58, $66, $53,
    $0E, $C6, $B7, $CC, $22, $33, $40, $72, $52, $29, $7D, $93, $3A, $D1, $A3,
    $D8, $FC, $CD, $77, $E1, $87, $90, $C4, $1C, $EE, $F0, $D9, $96, $BE, $D1,
    $A0, $E0, $9A, $20, $2C, $4C, $95, $84, $DB, $09, $07, $71, $6D, $F9, $D6,
    $8B, $A2, $3F, $72, $86, $40, $DA, $01, $EC, $A4, $CC, $73, $71, $2B, $B1,
    $80, $D2, $1B, $B8, $C1, $54, $E9, $56, $6F, $60, $E4, $45, $76, $E1, $C8,
    $8D, $E7, $50, $A3, $B0, $EE, $9F, $19, $3A, $E8, $66, $FD, $4A, $32, $06,
    $AF, $DC, $CB, $B0, $DE, $DD, $F3, $69, $70, $5B, $46, $9A, $D3, $5D, $17,
    $AE, $F1, $26, $FC, $CF, $71, $F6, $D1, $D7, $29, $9D, $A5, $E9, $C9, $59,
    $53, $5D, $2F, $D6, $C0, $F1, $F2, $65, $01, $5A, $0A, $A1, $D5, $B7, $A4,
    $1F, $43, $80, $91, $ED, $4A, $EB, $D8, $F3, $E0, $E4, $58, $74, $C6, $15,
    $86, $4D, $E3, $99, $B3, $B3, $25, $54, $68, $7C, $21, $16, $C2, $41, $FB,
    $F0, $D6, $E9, $3C, $73, $94, $63, $A8, $B9, $A9, $9C, $41, $6F, $54, $00,
    $FA, $3E, $36, $11, $D5, $4F, $8A, $DD, $63, $C9, $52, $60, $EF, $80, $67,
    $28, $BB, $33, $6E, $AF, $23, $16, $2B, $58, $92, $85, $BA, $F9, $82, $16,
    $8F, $5A, $17, $6C, $60, $24, $FC, $89, $46, $0F, $89, $74, $C8, $65, $67,
    $6F, $8D, $17, $3B, $22, $F4, $FA, $01, $6F, $CB, $F3, $27, $CE, $E3, $27,
    $92, $68, $3E, $6D, $15, $59, $0C, $95, $13, $C6, $BF, $5A, $47, $AE, $E3,
    $F1, $EC, $AE, $8C, $A5, $C5, $12, $73, $E4, $C0, $F5
  );
{$ENDIF}

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

{$IFNDEF HEXEN}
function Sys_Random: integer;
begin
  sysrndindex := sysrndindex + sysrndseed + 1;
  if sysrndindex >= SYSRNDSIZE then
    sysrndindex := sysrndindex - SYSRNDSIZE;
  result := sysrndtable[sysrndindex];
end;
{$ENDIF}

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
{$IFNDEF HEXEN}
  sysrndindex := 0;
{$ENDIF}  
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

