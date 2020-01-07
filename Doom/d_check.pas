//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2020 by Jim Valavanis
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
//  DESCRIPTION (d_main.h):
//   Identify known wads
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit d_check;

interface

procedure D_CheckCustomWad(const filename: string);

implementation

uses
  d_delphi,
  doomdef,
  doomstat,
  m_crc32,
  w_wad;

type
  waddetect_t = record
    crc32: string[8];
    numlumps: integer;
    size: integer;
    version: GameVersion_t;
    customgame: CustomGame_t;
  end;

const
  NUMWADDETECTITEMS = 12;
  wdtbl: array[0..NUMWADDETECTITEMS - 1] of waddetect_t = (
    (crc32: '723e60f9'; numlumps: 2194; size: 11159840; version: exe_doom_1_9; customgame: cg_none),  // registered
    (crc32: 'bf0eaac0'; numlumps: 2306; size: 12408292; version: exe_ultimate; customgame: cg_none),  // Ultimate
    (crc32: 'ff1ba733'; numlumps: 2318; size: 12538385; version: exe_ultimate; customgame: cg_none),  // Ultimate X-box
    (crc32: '5efa677e'; numlumps: 2312; size: 12487824; version: exe_ultimate; customgame: cg_none),  // Ultimate BFG
    (crc32: '162b696a'; numlumps: 1264; size:  4196020; version: exe_ultimate; customgame: cg_none),  // shareware 1.9
    (crc32: 'ec8725db'; numlumps: 2919; size: 14604584; version: exe_doom_1_9; customgame: cg_none),  // Doom2 1.9
    (crc32: '927a778a'; numlumps: 2935; size: 14691821; version: exe_doom_1_9; customgame: cg_bfg2),  // Doom2 BFG
    (crc32: '903dcc27'; numlumps: 3101; size: 18195736; version: exe_final2;   customgame: cg_none),  // TNT
    (crc32: 'd4bb05c0'; numlumps: 3106; size: 18654796; version: exe_final2;   customgame: cg_none),  // TNT
    (crc32: '7f572c1f'; numlumps: 3101; size: 18222568; version: exe_final2;   customgame: cg_none),  // TNT
    (crc32: '48d1453c'; numlumps: 2984; size: 17420824; version: exe_final2;   customgame: cg_none),  // PLUTONIA
    (crc32: '15cd1448'; numlumps: 2988; size: 18240172; version: exe_final2;   customgame: cg_none)   // PLUTONIA
  );


procedure D_CheckCustomWad(const filename: string);
var
  sname: string;
  crc32: string[8];
  wi: wadinfo_t;
  numlmps: integer;
  f: TFile;
  size: integer;
  i: integer;
begin
  if not fexists(filename) then
    exit;
  f := TFile.Create(filename, fOpenReadOnly);
  size := f.Size;
  if f.Read(wi, SizeOf(wadinfo_t)) <> SizeOf(wadinfo_t) then
    numlmps := 0
  else
    numlmps := wi.numlumps;
  f.Free;
  for i := 0 to NUMWADDETECTITEMS - 1 do
    if (wdtbl[i].size = size) and (wdtbl[i].numlumps = numlmps) then
    begin
      crc32 := GetCRC32(filename);
      if wdtbl[i].crc32 = crc32 then
      begin
        gameversion := wdtbl[i].version;
        customgame := wdtbl[i].customgame;
        exit;
      end;
    end;
    
  sname := strupper(fname(filename));
  // JVAL: Chex Support
  if sname = 'CHEX.WAD' then
    if customgame <> cg_chex2 then
    begin
      gameversion := exe_chex;
      customgame := cg_chex;
      Exit;
    end;
  if sname = 'CHEX2.WAD' then
  begin
    gameversion := exe_chex;
    customgame := cg_chex2;
    exit;
  end;
  // JVAL: Hacx Support
  if sname = 'HACX.WAD' then
  begin
    gameversion := exe_hacx;
    customgame := cg_hacx;
    exit;
  end;
  // JVAL: FreeDoom Support
  // Doomworld post -> https://www.doomworld.com/vb/freedoom/66965-source-port-support-for-new-iwad-names/
  if (sname = 'FREEDOOM.WAD') or (sname = 'FREEDOOM1.WAD') or (sname = 'FREEDOOM2.WAD') or (sname = 'FREEDM.WAD') then
  begin
    customgame := cg_freedoom;
    exit;
  end;
  // JVAL: FINAL DOOM
  if (sname = 'TNT.WAD') or (sname = 'PLUTONIA.WAD') then
  begin
    gameversion := exe_final2;
    exit;
  end;
  // JVAL: DOOM2
  if sname = 'DOOM2.WAD' then
  begin
    gameversion := exe_doom_1_9;
    exit;
  end;
  // JVAL: UNTIMATE DOOM
  if (sname = 'DOOM.WAD') or (sname = 'DOOM1.WAD') or (sname = 'DOOMU.WAD') then
  begin
    gameversion := exe_ultimate;
    exit;
  end;
end;

end.
