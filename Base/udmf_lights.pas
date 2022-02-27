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

unit udmf_lights;

interface

uses
  d_delphi,
  d_think,
  r_defs;

type
  lighttype_t = (
    LITE_RAISEBYVALUE,
    LITE_LOWERBYVALUE,
    LITE_CHANGETOVALUE,
    LITE_FADE,
    LITE_GLOW,
    LITE_FLICKER,
    LITE_STROBE,
    // JVAL: 20220227 - Added new light types
    LITE_MAXNEIGHTBOR,
    LITE_MINNEIGHTBOR,
    LITE_STROBEDOOM
  );

//==============================================================================
//
// EVH_SpawnLight
//
//==============================================================================
function EVH_SpawnLight(line: Pline_t; arg: PByteArray; _type: lighttype_t): boolean;

//==============================================================================
//
// P_SpawnPhasedLight
//
//==============================================================================
procedure P_SpawnPhasedLight(sector: Psector_t; base: integer; index: integer);

//==============================================================================
//
// P_SpawnLightSequence
//
//==============================================================================
procedure P_SpawnLightSequence(sector: Psector_t; indexStep: integer);

type
  light_t = record
    thinker: thinker_t;
    sector: Psector_t;
    _type: lighttype_t;
    value1: integer;
    value2: integer;
    tics1: integer;
    tics2: integer;
    count: integer;
  end;
  Plight_t = ^light_t;

//==============================================================================
//
// TH_Light
//
//==============================================================================
procedure TH_Light(light: Plight_t);

type
  phase_t = record
    thinker: thinker_t;
    sector: Psector_t;
    index: integer;
    base: integer;
  end;
  Pphase_t = ^phase_t;

//==============================================================================
//
// TH_Phase
//
//==============================================================================
procedure TH_Phase(phase: Pphase_t);

const
  {$IFDEF DOOM_OR_STRIFE}
  LIGHT_SEQUENCE_START = 22;
  LIGHT_SEQUENCE = 23;
  LIGHT_SEQUENCE_ALT = 24;
  {$ENDIF}
  {$IFDEF HERETIC}
  LIGHT_SEQUENCE_START = 52;
  LIGHT_SEQUENCE = 53;
  LIGHT_SEQUENCE_ALT = 54;
  {$ENDIF}
  {$IFDEF HEXEN}
  LIGHT_SEQUENCE_START = 2;
  LIGHT_SEQUENCE = 3;
  LIGHT_SEQUENCE_ALT = 4;
  {$ENDIF}

implementation

uses
  m_fixed,
  m_rnd,
  p_tick,
  p_spec,
  {$IFNDEF HEXEN}
  udmf_spec,
  {$ENDIF}
  p_setup,
  z_zone;

//==============================================================================
//
// P_FindLowestLightSurrounding
//
//==============================================================================
function P_FindLowestLightSurrounding(sec: Psector_t): integer;
const
  MAXLIGHT = $FFFF;
var
  i: integer;
  check: Pline_t;
  other: Psector_t;
begin
  result := MAXLIGHT;

  for i := 0 to sec.linecount - 1 do
  begin
    check := sec.lines[i];
    other := getNextSector(check, sec);

    if other <> nil then
      if other.lightlevel < result then
        result := other.lightlevel;
  end;

  if result = MAXLIGHT then
    result := sec.lightlevel;
end;

//==============================================================================
//
// P_FindHighestLightSurrounding
//
//==============================================================================
function P_FindHighestLightSurrounding(sec: Psector_t): integer;
const
  MINLIGHT = -1;
var
  i: integer;
  check: Pline_t;
  other: Psector_t;
begin
  result := MINLIGHT;

  for i := 0 to sec.linecount - 1 do
  begin
    check := sec.lines[i];
    other := getNextSector(check, sec);

    if other <> nil then
      if other.lightlevel < result then
        result := other.lightlevel;
  end;

  if result = MINLIGHT then
    result := sec.lightlevel;
end;

//==============================================================================
//
//  TH_Light
//
//==============================================================================
procedure TH_Light(light: Plight_t);
begin
  if light.count > 0 then
  begin
    dec(light.count);
    exit;
  end;

  case light._type of
    LITE_FADE:
      begin
        light.sector.lightlevel := ((light.sector.lightlevel * FRACUNIT) + light.value2) div FRACUNIT;
        if light.tics2 = 1 then
        begin
          if light.sector.lightlevel >= light.value1 then
          begin
            light.sector.lightlevel := light.value1;
            P_RemoveThinker(@light.thinker);
          end;
        end
        else if light.sector.lightlevel <= light.value1 then
        begin
          light.sector.lightlevel := light.value1;
          P_RemoveThinker(@light.thinker);
        end;
      end;
    LITE_GLOW:
      begin
        light.sector.lightlevel := ((light.sector.lightlevel * FRACUNIT) + light.tics1) div FRACUNIT;
        if light.tics2 = 1 then
        begin
          if light.sector.lightlevel >= light.value1 then
          begin
            light.sector.lightlevel := light.value1;
            light.tics1 := -light.tics1;
            light.tics2 := -1; // reverse direction
          end;
        end
        else if light.sector.lightlevel <= light.value2 then
        begin
          light.sector.lightlevel := light.value2;
          light.tics1 := -light.tics1;
          light.tics2 := 1; // reverse direction
        end;
      end;
    LITE_FLICKER:
      begin
        if light.sector.lightlevel = light.value1 then
        begin
          light.sector.lightlevel := light.value2;
          light.count := (P_Random and 7) + 1;
        end
        else
        begin
          light.sector.lightlevel := light.value1;
          light.count := (P_Random and 31) + 1;
        end;
      end;
    LITE_STROBE,
    LITE_STROBEDOOM:
      begin
        if light.sector.lightlevel = light.value1 then
        begin
          light.sector.lightlevel := light.value2;
          light.count := light.tics2;
        end
        else
        begin
          light.sector.lightlevel := light.value1;
          light.count := light.tics1;
        end;
      end;
  end;
end;

//==============================================================================
//
//  EVH_SpawnLight
//
//==============================================================================
function EVH_SpawnLight(line: Pline_t; arg: PByteArray; _type: lighttype_t): boolean;
var
  light: Plight_t;
  sec: Psector_t;
  secNum: integer;
  arg1, arg2, arg3, arg4: integer;
  think: boolean;
begin
  result := false;

  arg1 := arg[1];
  arg2 := arg[2];
  arg3 := arg[3];
  arg4 := arg[4];

  secNum := -1;
  while P_FindSectorFromTag2(arg[0], secNum) >= 0 do
  begin
    think := false;
    sec := @sectors[secNum];

    light := Z_Malloc(sizeof(light_t), PU_LEVSPEC, nil);
    light._type := _type;
    light.sector := sec;
    light.count := 0;
    result := true;
    case _type of
      LITE_RAISEBYVALUE:
        begin
          sec.lightlevel := sec.lightlevel + arg1;
          if sec.lightlevel > 255 then
            sec.lightlevel := 255;
        end;
      LITE_LOWERBYVALUE:
        begin
          sec.lightlevel := sec.lightlevel - arg1;
          if sec.lightlevel < 0 then
            sec.lightlevel := 0;
        end;
      LITE_CHANGETOVALUE:
        begin
          sec.lightlevel := arg1;
          if sec.lightlevel < 0 then
            sec.lightlevel := 0
          else if sec.lightlevel > 255 then
            sec.lightlevel := 255;
        end;
      LITE_FADE:
        begin
          think := true;
          light.value1 := arg1; // destination lightlevel
          light.value2 := FixedDiv((arg1 - sec.lightlevel) * FRACUNIT, arg2 * FRACUNIT);  // delta lightlevel
          if sec.lightlevel <= arg1 then
            light.tics2 := 1 // get brighter
          else
            light.tics2 := -1;
        end;
      LITE_GLOW:
        begin
          think := true;
          light.value1 := arg1; // upper lightlevel
          light.value2 := arg2; // lower lightlevel
          light.tics1 := FixedDiv((arg1 - sec.lightlevel) * FRACUNIT, arg3 * FRACUNIT);  // lightlevel delta
          if sec.lightlevel <= arg1 then
            light.tics2 := 1 // get brighter
          else
            light.tics2 := -1;
        end;
      LITE_FLICKER:
        begin
          think := true;
          light.value1 := arg1; // upper lightlevel
          light.value2 := arg2; // lower lightlevel
          sec.lightlevel := light.value1;
          light.count := (P_Random and 64) + 1;
        end;
      LITE_STROBE:
        begin
          think := true;
          light.value1 := arg1; // upper lightlevel
          light.value2 := arg2; // lower lightlevel
          light.tics1 := arg3;  // upper tics
          light.tics2 := arg4;  // lower tics
          light.count := arg3;
          sec.lightlevel := light.value1;
        end;
      LITE_STROBEDOOM:
        begin
          think := true;
          light.value1 := sec.lightlevel; // upper lightlevel
          light.value2 := P_FindLowestLightSurrounding(sec); // lower lightlevel
          if light.value2 = light.value1 then
            light.value2 := 0;
          light.tics1 := arg1;  // upper tics
          light.tics2 := arg2;  // lower tics
          light.count := arg1;
          sec.lightlevel := light.value1;
        end;
      LITE_MAXNEIGHTBOR:
        begin
          sec.lightlevel := P_FindHighestLightSurrounding(sec);
          if sec.lightlevel < 0 then
            sec.lightlevel := 0
          else if sec.lightlevel > 255 then
            sec.lightlevel := 255;
        end;
      LITE_MINNEIGHTBOR:
        begin
          sec.lightlevel := P_FindLowestLightSurrounding(sec);
          if sec.lightlevel < 0 then
            sec.lightlevel := 0
          else if sec.lightlevel > 255 then
            sec.lightlevel := 255;
        end;
      else
        result := false;
    end;
    if think then
    begin
      P_AddThinker(@light.thinker);
      light.thinker._function.acp1 := @TH_Light;
    end
    else
      Z_Free(light);
  end;
end;

//
//  TH_Phase
//

const
  PhaseTable: array[0..63] of integer = (
    128, 112,  96,  80,  64,  48,  32,  32,
     16,  16,  16,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,   0,   0,   0,
      0,   0,   0,   0,   0,  16,  16,  16,
     32,  32,  48,  64,  80,  96, 112, 128
  );

//==============================================================================
//
// TH_Phase
//
//==============================================================================
procedure TH_Phase(phase: Pphase_t);
begin
  phase.index := (phase.index + 1) and 63;
  phase.sector.lightlevel := phase.base + PhaseTable[phase.index];
end;

const
  {$IFDEF DOOM_OR_STRIFE}
  PHASE_MASK = 31;
  {$ELSE}
  PHASE_MASK = $FFFF;
  {$ENDIF}

//==============================================================================
//
// P_SpawnPhasedLight
//
//==============================================================================
procedure P_SpawnPhasedLight(sector: Psector_t; base: integer; index: integer);
var
  phase: Pphase_t;
begin
  phase := Z_Malloc(SizeOf(phase_t), PU_LEVSPEC, nil);
  P_AddThinker(@phase.thinker);
  phase.sector := sector;
  if index = -1 then
  begin // sector.lightlevel as the index
    phase.index := sector.lightlevel and 63;
  end
  else
  begin
    phase.index := index and 63;
  end;
  phase.base := base and 255;
  sector.lightlevel := phase.base + PhaseTable[phase.index];
  phase.thinker._function.acp1 := @TH_Phase;

  sector.special := sector.special and not PHASE_MASK;
end;

//==============================================================================
//
// P_SpawnLightSequence
//
//==============================================================================
procedure P_SpawnLightSequence(sector: Psector_t; indexStep: integer);
var
  sec: Psector_t;
  nextSec: Psector_t;
  tempSec: Psector_t;
  seqSpecial: integer;
  i: integer;
  count: integer;
  index: fixed_t;
  indexDelta: fixed_t;
  base: integer;
begin
  seqSpecial := LIGHT_SEQUENCE; // look for Light_Sequence, first
  sec := sector;
  count := 1;
  repeat
    nextSec := nil;
    sec.special := LIGHT_SEQUENCE_START or (sec.special and not PHASE_MASK); // make sure that the search doesn't back up.
    for i := 0 to sec.linecount - 1 do
    begin
      tempSec := getNextSector(sec.lines[i], sec);
      if tempSec = nil then
        continue;

      if tempSec.special and PHASE_MASK = seqSpecial then
      begin
        if seqSpecial = LIGHT_SEQUENCE then
        begin
          seqSpecial := LIGHT_SEQUENCE_ALT;
        end
        else
        begin
          seqSpecial := LIGHT_SEQUENCE;
        end;
        nextSec := tempSec;
        inc(count);
      end;
    end;
    sec := nextSec;
  until sec = nil;

  sec := sector;
  count := count * indexStep;
  index := 0;
  indexDelta := FixedDiv(64 * FRACUNIT, count * FRACUNIT);
  base := sector.lightlevel;
  repeat
    nextSec := nil;
    if sec.lightlevel <> 0 then
    begin
      base := sec.lightlevel;
    end;
    P_SpawnPhasedLight(sec, base, index div FRACUNIT);
    index := index + indexDelta;
    for i := 0 to sec.linecount - 1 do
    begin
      tempSec := getNextSector(sec.lines[i], sec);
      if tempSec = nil then
        continue;

      if tempSec.special and PHASE_MASK = LIGHT_SEQUENCE_START then
      begin
        nextSec := tempSec;
      end;
    end;
    sec := nextSec;
  until sec = nil;
end;

end.
