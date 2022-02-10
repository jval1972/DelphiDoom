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
// DESCRIPTION:
//  32 bit software rendering texture cache
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_cache_main;

interface

//==============================================================================
//
// R_Reset32Cache
//
//==============================================================================
procedure R_Reset32Cache;

//==============================================================================
//
// R_Clear32Cache
//
//==============================================================================
procedure R_Clear32Cache;

//==============================================================================
//
// R_Init32Cache
//
//==============================================================================
procedure R_Init32Cache;

//==============================================================================
//
// R_ShutDown32Cache
//
//==============================================================================
procedure R_ShutDown32Cache;

var
  c_ctot, c_cmiss: int64;
  c_stot, c_smiss: int64;

implementation

uses
  d_delphi,
  c_cmds,
  r_cache_walls,
  r_cache_flats;

//==============================================================================
//
// R_Reset32Cache
//
//==============================================================================
procedure R_Reset32Cache;
begin
  R_ResetDC32Cache;
  R_ResetDS32Cache;
end;

//==============================================================================
//
// R_Clear32Cache
//
//==============================================================================
procedure R_Clear32Cache;
begin
  R_ClearDC32Cache;
  R_ClearDS32Cache;
end;

//==============================================================================
//
// R_ShutDown32Cache
//
//==============================================================================
procedure R_ShutDown32Cache;
begin
  R_ShutDownDC32Cache;
  R_ShutDownDS32Cache;
end;

//==============================================================================
//
// R_CmdCacheHit
//
//==============================================================================
procedure R_CmdCacheHit;
var
  c_tot, c_miss: int64;
begin
  c_tot := c_ctot + c_stot;
  if c_tot <> 0 then
  begin
    if c_ctot <> 0 then
    begin
      printf('Columns:'#13#10);
      printf(' %10d hits'#13#10, [c_ctot - c_cmiss]);
      printf(' %10d misses'#13#10, [c_cmiss]);
      printf(' %10d total'#13#10, [c_ctot]);
      printf(' 32 bit cache hit factor for columns = %2.2f'#13#10, [100 * (c_ctot - c_cmiss) / c_ctot]);
    end;
    if c_stot <> 0 then
    begin
      printf('Flats:'#13#10);
      printf('%10d hits'#13#10, [c_stot - c_smiss]);
      printf('%10d misses'#13#10, [c_smiss]);
      printf('%10d total'#13#10, [c_stot]);
      printf(' 32 bit cache hit factor for flats = %2.2f'#13#10, [100 * (c_stot - c_smiss) / c_stot]);
    end;
    c_miss := c_cmiss + c_smiss;
    printf('Total:'#13#10);
    printf('%10d hits'#13#10, [c_tot - c_miss]);
    printf('%10d misses'#13#10, [c_miss]);
    printf('%10d total'#13#10, [c_tot]);
    printf(' 32 bit cache hit factor = %2.2f'#13#10, [100 * (c_tot - c_miss) / c_tot]);
  end
  else
    printf('32 bit cache not yet initialized'#13#10);
end;

//==============================================================================
//
// R_CmdResetCacheHit
//
//==============================================================================
procedure R_CmdResetCacheHit;
begin
  printf('32 bit cache hit factor reset. Re-initializing stats.'#13#10);
  c_ctot := 0;
  c_cmiss := 0;
  c_stot := 0;
  c_smiss := 0;
end;

//==============================================================================
//
// R_Init32Cache
//
//==============================================================================
procedure R_Init32Cache;
begin
  R_InitDC32Cache;
  R_InitDS32Cache;
  C_AddCmd('cachehit', @R_CmdCacheHit);
  C_AddCmd('resetcachehit', @R_CmdResetCacheHit);
end;

end.

