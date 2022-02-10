//------------------------------------------------------------------------------
//
//  DelphiHeretic is a source port of the game Heretic and it is
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
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

unit p_ambient;

interface

//==============================================================================
//
// P_InitAmbientSound
//
//==============================================================================
procedure P_InitAmbientSound;

//==============================================================================
//
// P_AddAmbientSfx
//
//==============================================================================
procedure P_AddAmbientSfx(const sequence: integer);

//==============================================================================
//
// P_AmbientSound
//
//==============================================================================
procedure P_AmbientSound;

implementation

uses
  d_delphi,
  doomdef,
  i_system,
  m_rnd,
  s_sound,
  sounddata;

const
  MAX_AMBIENT_SFX = 8; // Per level

// Types

type
  afxcmd_t = (
    afxcmd_play,    // (sound)
    afxcmd_playabsvol,  // (sound, volume)
    afxcmd_playrelvol,  // (sound, volume)
    afxcmd_delay,    // (ticks)
    afxcmd_delayrand,  // (andbits)
    afxcmd_end      // ()
  );

// Data

var
  LevelAmbientSfx: array[0..MAX_AMBIENT_SFX - 1] of PIntegerArray;
  AmbSfxPtr: PInteger;
  AmbSfxCount: integer;
  AmbSfxTics: integer;
  AmbSfxVolume: integer;

var
  AmbSndSeqInit: array[0..0] of integer = (
  // Startup
    Ord(afxcmd_end)
  );

  AmbSndSeq1: array[0..2] of integer = (
 // Scream
    Ord(afxcmd_play),
    Ord(sfx_amb1),
    Ord(afxcmd_end)
  );

  AmbSndSeq2: array[0..2] of integer = (
  // Squish
    Ord(afxcmd_play),
    Ord(sfx_amb2),
    Ord(afxcmd_end)
  );

  AmbSndSeq3: array[0..36] of integer = (
  // Drops
    Ord(afxcmd_play),
    Ord(sfx_amb3),
    Ord(afxcmd_delay),
    16,
    Ord(afxcmd_delayrand),
    31,
    Ord(afxcmd_play),
    Ord(sfx_amb7),
    Ord(afxcmd_delay),
    16,
    Ord(afxcmd_delayrand),
    31,
    Ord(afxcmd_play),
    Ord(sfx_amb3),
    Ord(afxcmd_delay),
    16,
    Ord(afxcmd_delayrand),
    31,
    Ord(afxcmd_play),
    Ord(sfx_amb7),
    Ord(afxcmd_delay),
    16,
    Ord(afxcmd_delayrand),
    31,
    Ord(afxcmd_play),
    Ord(sfx_amb3),
    Ord(afxcmd_delay),
    16,
    Ord(afxcmd_delayrand),
    31,
    Ord(afxcmd_play),
    Ord(sfx_amb7),
    Ord(afxcmd_delay),
    16,
    Ord(afxcmd_delayrand),
    31,
    Ord(afxcmd_end)
  );

  AmbSndSeq4: array[0..37] of integer = (
  // SlowFootSteps
    Ord(afxcmd_play),
    Ord(sfx_amb4),
    Ord(afxcmd_delay),
    15,
    Ord(afxcmd_playrelvol),
    Ord(sfx_amb11),
    -3,
    Ord(afxcmd_delay),
    15,
    Ord(afxcmd_playrelvol),
    Ord(sfx_amb4),
    -3,
    Ord(afxcmd_delay),
    15,
    Ord(afxcmd_playrelvol),
    Ord(sfx_amb11),
    -3,
    Ord(afxcmd_delay),
    15,
    Ord(afxcmd_playrelvol),
    Ord(sfx_amb4),
    -3,
    Ord(afxcmd_delay),
    15,
    Ord(afxcmd_playrelvol),
    Ord(sfx_amb11),
    -3,
    Ord(afxcmd_delay),
    15,
    Ord(afxcmd_playrelvol),
    Ord(sfx_amb4),
    -3,
    Ord(afxcmd_delay),
    15,
    Ord(afxcmd_playrelvol),
    Ord(sfx_amb11),
    -3,
    Ord(afxcmd_end)
  );

  AmbSndSeq5: array[0..14] of integer = (
  // Heartbeat
    Ord(afxcmd_play),
    Ord(sfx_amb5),
    Ord(afxcmd_delay),
    35,
    Ord(afxcmd_play),
    Ord(sfx_amb5),
    Ord(afxcmd_delay),
    35,
    Ord(afxcmd_play),
    Ord(sfx_amb5),
    Ord(afxcmd_delay),
    35,
    Ord(afxcmd_play),
    Ord(sfx_amb5),
    Ord(afxcmd_end)
  );

  AmbSndSeq6: array[0..17] of integer = (
  // Bells
    Ord(afxcmd_play),
    Ord(sfx_amb6),
    Ord(afxcmd_delay),
    17,
    Ord(afxcmd_playrelvol),
    Ord(sfx_amb6),
    -8,
    Ord(afxcmd_delay),
    17,
    Ord(afxcmd_playrelvol),
    Ord(sfx_amb6),
    -8,
    Ord(afxcmd_delay),
    17,
    Ord(afxcmd_playrelvol),
    Ord(sfx_amb6),
    -8,
    Ord(afxcmd_end)
  );

  AmbSndSeq7: array[0..2] of integer = (
  // Growl
    Ord(afxcmd_play),
    Ord(sfx_bstsit),
    Ord(afxcmd_end)
  );

  AmbSndSeq8: array[0..2] of integer = (
  // Magic
    Ord(afxcmd_play),
    Ord(sfx_amb8),
    Ord(afxcmd_end)
  );

  AmbSndSeq9: array[0..27] of integer = (
  // Laughter
    Ord(afxcmd_play),
    Ord(sfx_amb9),
    Ord(afxcmd_delay),
    16,
    Ord(afxcmd_playrelvol),
    Ord(sfx_amb9),
    -4,
    Ord(afxcmd_delay),
    16,
    Ord(afxcmd_playrelvol),
    Ord(sfx_amb9),
    -4,
    Ord(afxcmd_delay),
    16,
    Ord(afxcmd_playrelvol),
    Ord(sfx_amb10),
    -4,
    Ord(afxcmd_delay),
    16,
    Ord(afxcmd_playrelvol),
    Ord(sfx_amb10),
    -4,
    Ord(afxcmd_delay),
    16,
    Ord(afxcmd_playrelvol),
    Ord(sfx_amb10),
    -4,
    Ord(afxcmd_end)
  );

  AmbSndSeq10: array[0..37] of integer = (
  // FastFootsteps
    Ord(afxcmd_play),
    Ord(sfx_amb4),
    Ord(afxcmd_delay),
    8,
    Ord(afxcmd_playrelvol),
    Ord(sfx_amb11),
    -3,
    Ord(afxcmd_delay),
    8,
    Ord(afxcmd_playrelvol),
    Ord(sfx_amb4),
    -3,
    Ord(afxcmd_delay),
    8,
    Ord(afxcmd_playrelvol),
    Ord(sfx_amb11),
    -3,
    Ord(afxcmd_delay),
    8,
    Ord(afxcmd_playrelvol),
    Ord(sfx_amb4),
    -3,
    Ord(afxcmd_delay),
    8,
    Ord(afxcmd_playrelvol),
    Ord(sfx_amb11),
    -3,
    Ord(afxcmd_delay),
    8,
    Ord(afxcmd_playrelvol),
    Ord(sfx_amb4),
    -3,
    Ord(afxcmd_delay),
    8,
    Ord(afxcmd_playrelvol),
    Ord(sfx_amb11),
    -3,
    Ord(afxcmd_end)
  );

var
  AmbientSfx: array[0..9] of PIntegerArray;

//----------------------------------------------------------------------------
//
// PROC P_InitAmbientSound
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure P_InitAmbientSound;
begin
  AmbSfxCount := 0;
  AmbSfxVolume := 0;
  AmbSfxTics := 10 * TICRATE;
  AmbSfxPtr := @AmbSndSeqInit[0];

  AmbientSfx[0] := @AmbSndSeq1;    // Scream
  AmbientSfx[1] := @AmbSndSeq2;    // Squish
  AmbientSfx[2] := @AmbSndSeq3;    // Drops
  AmbientSfx[3] := @AmbSndSeq4;    // SlowFootsteps
  AmbientSfx[4] := @AmbSndSeq5;    // Heartbeat
  AmbientSfx[5] := @AmbSndSeq6;    // Bells
  AmbientSfx[6] := @AmbSndSeq7;    // Growl
  AmbientSfx[7] := @AmbSndSeq8;    // Magic
  AmbientSfx[8] := @AmbSndSeq9;    // Laughter
  AmbientSfx[9] := @AmbSndSeq10;   // FastFootsteps
end;

//----------------------------------------------------------------------------
//
// PROC P_AddAmbientSfx
//
// Called by (P_mobj):P_SpawnMapThing during (P_setup):P_SetupLevel.
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure P_AddAmbientSfx(const sequence: integer);
begin
  if AmbSfxCount = MAX_AMBIENT_SFX then
  begin
    I_DevError('P_AddAmbientSfx(): Too many ambient sound sequences: %d.'#13#10, [AmbSfxCount]);
    exit;
  end;

  LevelAmbientSfx[AmbSfxCount] := AmbientSfx[sequence];
  inc(AmbSfxCount);
end;

//----------------------------------------------------------------------------
//
// PROC P_AmbientSound
//
// Called every tic by (P_tick):P_Ticker.
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure P_AmbientSound;
var
  cmd: integer;
  sound: integer;
  done: boolean;
begin
  if AmbSfxCount = 0 then // No ambient sound sequences on current level
    exit;

  dec(AmbSfxTics);
  if AmbSfxTics <> 0 then
    exit;

  done := false;

  repeat
    cmd := AmbSfxPtr^;
    inc(AmbSfxPtr);
    case cmd of
      Ord(afxcmd_play):
        begin
          AmbSfxVolume := P_Random div 4;
          S_StartSoundAtVolume(nil, AmbSfxPtr^, AmbSfxVolume);
          inc(AmbSfxPtr);
        end;
      Ord(afxcmd_playabsvol):
        begin
          sound := AmbSfxPtr^;
          inc(AmbSfxPtr);
          AmbSfxVolume := AmbSfxPtr^;
          inc(AmbSfxPtr);
          S_StartSoundAtVolume(nil, sound, AmbSfxVolume);
        end;
      Ord(afxcmd_playrelvol):
        begin
          sound := AmbSfxPtr^;
          inc(AmbSfxPtr);
          AmbSfxVolume := AmbSfxVolume + AmbSfxPtr^;
          inc(AmbSfxPtr);
          if AmbSfxVolume < 0 then
            AmbSfxVolume := 0
          else if AmbSfxVolume > 127 then
            AmbSfxVolume := 127;
          S_StartSoundAtVolume(nil, sound, AmbSfxVolume);
        end;
      Ord(afxcmd_delay):
        begin
          AmbSfxTics := AmbSfxPtr^;
          inc(AmbSfxPtr);
          done := true;
        end;
      Ord(afxcmd_delayrand):
        begin
          AmbSfxTics := P_Random and AmbSfxPtr^;
          inc(AmbSfxPtr);
          done := true;
        end;
      Ord(afxcmd_end):
        begin
          AmbSfxTics := 6 * TICRATE + P_Random;
          AmbSfxPtr := @LevelAmbientSfx[P_Random mod AmbSfxCount][0];
          done := true;
        end;
      else
        I_Error('P_AmbientSound(): Unknown afxcmd %d', [cmd]);
    end;
  until done;
end;

end.
