//------------------------------------------------------------------------------
//
//  DelphiHexen is a source port of the game Hexen and it is
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
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

unit s_sndseq;

interface

uses
  d_delphi,
  p_mobj_h;

//==============================================================================
//
// S_InitSequenceScript
//
//==============================================================================
procedure S_InitSequenceScript;

//==============================================================================
//
// S_StartSequence
//
//==============================================================================
procedure S_StartSequence(mobj: Pmobj_t; sequence: integer);

//==============================================================================
//
// S_StartSequenceName
//
//==============================================================================
procedure S_StartSequenceName(mobj: Pmobj_t; name: string);

//==============================================================================
//
// S_StopSequence
//
//==============================================================================
procedure S_StopSequence(mobj: Pmobj_t);

//==============================================================================
//
// S_StopAllSequences
//
//==============================================================================
procedure S_StopAllSequences;

//==============================================================================
//
// S_UpdateActiveSequences
//
//==============================================================================
procedure S_UpdateActiveSequences;

type
  Pseqnode_t = ^seqnode_t;
  seqnode_t = record
    sequencePtr: PInteger;
    sequence: integer;
    mobj: Pmobj_t;
    currentSoundID: integer;
    delayTics: integer;
    volume: integer;
    stopSound: integer;
    prev, next: Pseqnode_t;
  end;

type
  seqitemtype_t = (
    SEQ_PLATFORM,
    SEQ_PLATFORM_HEAVY,    // same script as a normal platform
    SEQ_PLATFORM_METAL,
    SEQ_PLATFORM_CREAK,    // same script as a normal platform
    SEQ_PLATFORM_SILENCE,
    SEQ_PLATFORM_LAVA,
    SEQ_PLATFORM_WATER,
    SEQ_PLATFORM_ICE,
    SEQ_PLATFORM_EARTH,
    SEQ_PLATFORM_METAL2,
    SEQ_DOOR_STONE,
    SEQ_DOOR_HEAVY,
    SEQ_DOOR_METAL,
    SEQ_DOOR_CREAK,
    SEQ_DOOR_SILENCE,
    SEQ_DOOR_LAVA,
    SEQ_DOOR_WATER,
    SEQ_DOOR_ICE,
    SEQ_DOOR_EARTH,
    SEQ_DOOR_METAL2,
    SEQ_ESOUND_WIND,
    SEQ_NUMSEQ
  );

  seqtype_t = (
    SEQTYPE_STONE,
    SEQTYPE_HEAVY,
    SEQTYPE_METAL,
    SEQTYPE_CREAK,
    SEQTYPE_SILENCE,
    SEQTYPE_LAVA,
    SEQTYPE_WATER,
    SEQTYPE_ICE,
    SEQTYPE_EARTH,
    SEQTYPE_METAL2,
    SEQTYPE_NUMSEQ
  );

var
  ActiveSequences: integer;
  SequenceListHead: Pseqnode_t;

//==============================================================================
//
// S_GetSequenceOffset
//
//==============================================================================
function S_GetSequenceOffset(sequence: integer; sequencePtr: PInteger): integer;

//==============================================================================
//
// S_ChangeNodeData
//
//==============================================================================
procedure S_ChangeNodeData(nodeNum: integer; seqOffset: integer; delayTics: integer;
  volume: integer; currentSoundID: integer);

implementation

uses
  g_game,
  i_system,
  m_rnd,
  sounddata,
  s_sound,
  sc_engine,
  w_wad,
  z_zone;
// HEADER FILES ------------------------------------------------------------

// MACROS ------------------------------------------------------------------

const
  SS_MAX_SCRIPTS = 64;
  SS_TEMPBUFFER_SIZE = 1024;
  SS_SEQUENCE_NAME_LENGTH = 32;

  SS_SCRIPT_NAME = 'SNDSEQ';
  SS_STRING_PLAY = 'play';
  SS_STRING_PLAYUNTILDONE = 'playuntildone';
  SS_STRING_PLAYTIME = 'playtime';
  SS_STRING_PLAYREPEAT = 'playrepeat';
  SS_STRING_DELAY = 'delay';
  SS_STRING_DELAYRAND = 'delayrand';
  SS_STRING_VOLUME = 'volume';
  SS_STRING_END = 'end';
  SS_STRING_STOPSOUND = 'stopsound';

// TYPES -------------------------------------------------------------------

type
  sscmds_t = (
    SS_CMD_NONE,
    SS_CMD_PLAY,
    SS_CMD_WAITUNTILDONE, // used by PLAYUNTILDONE
    SS_CMD_PLAYTIME,
    SS_CMD_PLAYREPEAT,
    SS_CMD_DELAY,
    SS_CMD_DELAYRAND,
    SS_CMD_VOLUME,
    SS_CMD_STOPSOUND,
    SS_CMD_END
  );

// PRIVATE DATA DEFINITIONS ------------------------------------------------

  SequenceTranslateItem_t = record
    name: string[SS_SEQUENCE_NAME_LENGTH];
    scriptNum: integer;
    stopSound: integer;
  end;

var
  SequenceTranslate: array[0..Ord(SEQ_NUMSEQ) - 1] of SequenceTranslateItem_t = (
    (name: 'Platform'; scriptNum: 0; stopSound: 0;),
    (name: 'Platform'; scriptNum: 0; stopSound: 0;),      // a 'heavy' platform is just a platform
    (name: 'PlatformMetal'; scriptNum: 0; stopSound: 0;),
    (name: 'Platform'; scriptNum: 0; stopSound: 0;),      // same with a 'creak' platform
    (name: 'Silence'; scriptNum: 0; stopSound: 0;),
    (name: 'Lava'; scriptNum: 0; stopSound: 0;),
    (name: 'Water'; scriptNum: 0; stopSound: 0;),
    (name: 'Ice'; scriptNum: 0; stopSound: 0;),
    (name: 'Earth'; scriptNum: 0; stopSound: 0;),
    (name: 'PlatformMetal2'; scriptNum: 0; stopSound: 0;),
    (name: 'DoorNormal'; scriptNum: 0; stopSound: 0;),
    (name: 'DoorHeavy'; scriptNum: 0; stopSound: 0;),
    (name: 'DoorMetal'; scriptNum: 0; stopSound: 0;),
    (name: 'DoorCreak'; scriptNum: 0; stopSound: 0;),
    (name: 'Silence'; scriptNum: 0; stopSound: 0;),
    (name: 'Lava'; scriptNum: 0; stopSound: 0;),
    (name: 'Water'; scriptNum: 0; stopSound: 0;),
    (name: 'Ice'; scriptNum: 0; stopSound: 0;),
    (name: 'Earth'; scriptNum: 0; stopSound: 0;),
    (name: 'DoorMetal2'; scriptNum: 0; stopSound: 0;),
    (name: 'Wind'; scriptNum: 0; stopSound: 0;)
  );

var
  SequenceData: array[0..SS_MAX_SCRIPTS - 1] of PIntegerArray;

// CODE --------------------------------------------------------------------

//==============================================================================
//
// VerifySequencePtr
//
//   Verifies the integrity of the temporary ptr, and ensures that the ptr
//     isn't exceeding the size of the temporary buffer
//
//==============================================================================
procedure VerifySequencePtr(base: PIntegerArray; ptr: PInteger);
begin
  if integer(ptr) - integer(base) > SS_TEMPBUFFER_SIZE * SizeOf(integer) then
    I_Error('VerifySequencePtr:  tempPtr >= %d', [SS_TEMPBUFFER_SIZE]);
end;

//==============================================================================
//
// GetSoundOffset
//
//==============================================================================
function GetSoundOffset(const name: string): integer;
var
  check: string;
  i: integer;
begin
  check := strupper(name);
  for i := 0 to numsfx - 1 do
  begin
    if check = strupper(S_sfx[i].tagName) then
    begin
      result := i;
      exit;
    end;
  end;
  I_Warning('GetSoundOffset():  Unknown sound name %s', [name]);
  result := 0;
end;

//==============================================================================
//
// S_InitSequenceScript
//
//==============================================================================
procedure S_InitSequenceScript;
var
  i, j: integer;
  inSequence: integer;
  freeSequence: integer;
  tempDataStart: PIntegerArray;
  tempDataPtr: PInteger;
  sc: TScriptEngine;
  tmp_str: string;
  datasize: integer;
begin
  inSequence := -1;
  ActiveSequences := 0;
  tempDataStart := nil; // Avoid compiler warning
  tempDataPtr := nil;
  freeSequence := 0;

  for i := 0 to SS_MAX_SCRIPTS - 1 do
    SequenceData[i] := nil;

  sc := TScriptEngine.Create(W_TextLumpName(SS_SCRIPT_NAME));
  while sc.GetString do
  begin
    if sc._String[1] = ':' then
    begin
      if inSequence <> -1 then
        I_Warning('S_InitSequenceScript():  Nested Script Error'#13#10);

      tempDataStart := PIntegerArray(Z_Malloc(SS_TEMPBUFFER_SIZE, PU_STATIC, nil));
      memset(tempDataStart, 0, SS_TEMPBUFFER_SIZE);
      tempDataPtr := @tempDataStart[0];
      i := 0;
      while i < SS_MAX_SCRIPTS do
      begin
        if SequenceData[i] = nil then
        begin
          freeSequence := i;
          break;
        end;
        inc(i);
      end;

      if i = SS_MAX_SCRIPTS then
        I_Error('S_InitSequenceScript(): Number of SS Scripts >= SS_MAX_SCRIPTS');

      tmp_str := strupper(Copy(sc._String, 2, Length(sc._String) - 1));
      for j := 0 to Ord(SEQ_NUMSEQ) - 1 do
      begin
        if strupper(SequenceTranslate[j].name) = tmp_str then
        begin
          SequenceTranslate[j].scriptNum := i;
          inSequence := j;
          break;
        end;
      end;
      continue; // parse the next command
    end;

    if inSequence = -1 then
      continue;

    if sc.Compare(SS_STRING_PLAYUNTILDONE) then
    begin
      VerifySequencePtr(tempDataStart, tempDataPtr);
      SC.MustGetString;
      tempDataPtr^ := Ord(SS_CMD_PLAY);
      inc(tempDataPtr);
      tempDataPtr^ := GetSoundOffset(sc._String);
      inc(tempDataPtr);
      tempDataPtr^ := Ord(SS_CMD_WAITUNTILDONE);
      inc(tempDataPtr);
    end
    else if sc.Compare(SS_STRING_PLAY) then
    begin
      VerifySequencePtr(tempDataStart, tempDataPtr);
      sc.MustGetString;
      tempDataPtr^ := Ord(SS_CMD_PLAY);
      inc(tempDataPtr);
      tempDataPtr^ := GetSoundOffset(sc._String);
      inc(tempDataPtr);
    end
    else if sc.Compare(SS_STRING_PLAYTIME) then
    begin
      VerifySequencePtr(tempDataStart, tempDataPtr);
      sc.MustGetString;
      tempDataPtr^ := Ord(SS_CMD_PLAY);
      inc(tempDataPtr);
      tempDataPtr^ := GetSoundOffset(sc._String);
      inc(tempDataPtr);
      sc.MustGetInteger;
      tempDataPtr^ := Ord(SS_CMD_DELAY);
      inc(tempDataPtr);
      tempDataPtr^ := sc._Integer;
      inc(tempDataPtr);
    end
    else if sc.Compare(SS_STRING_PLAYREPEAT) then
    begin
      VerifySequencePtr(tempDataStart, tempDataPtr);
      sc.MustGetString;
      tempDataPtr^ := Ord(SS_CMD_PLAYREPEAT);
      inc(tempDataPtr);
      tempDataPtr^ := GetSoundOffset(sc._String);
      inc(tempDataPtr);
    end
    else if sc.Compare(SS_STRING_DELAY) then
    begin
      VerifySequencePtr(tempDataStart, tempDataPtr);
      tempDataPtr^ := Ord(SS_CMD_DELAY);
      inc(tempDataPtr);
      sc.MustGetInteger;
      tempDataPtr^ := sc._Integer;
      inc(tempDataPtr);
    end
    else if sc.Compare(SS_STRING_DELAYRAND) then
    begin
      VerifySequencePtr(tempDataStart, tempDataPtr);
      tempDataPtr^ := Ord(SS_CMD_DELAYRAND);
      inc(tempDataPtr);
      sc.MustGetInteger;
      tempDataPtr^ := sc._Integer;
      inc(tempDataPtr);
      sc.MustGetInteger;
      tempDataPtr^ := sc._Integer;
      inc(tempDataPtr);
    end
    else if sc.Compare(SS_STRING_VOLUME) then
    begin
      VerifySequencePtr(tempDataStart, tempDataPtr);
      tempDataPtr^ := Ord(SS_CMD_VOLUME);
      inc(tempDataPtr);
      sc.MustGetInteger;
      tempDataPtr^ := sc._Integer;
      inc(tempDataPtr);
    end
    else if sc.Compare(SS_STRING_END) then
    begin
      tempDataPtr^ := Ord(SS_CMD_END);
      inc(tempDataPtr);
      dataSize := integer(tempDataPtr) - integer(tempDataStart);
      SequenceData[freeSequence] := PIntegerArray(Z_Malloc(dataSize, PU_STATIC, nil));
      memcpy(SequenceData[freeSequence], tempDataStart, dataSize);
      Z_Free(tempDataStart);
      inSequence := -1;
    end
    else if sc.Compare(SS_STRING_STOPSOUND) then
    begin
      sc.MustGetString;
      SequenceTranslate[inSequence].stopSound := GetSoundOffset(sc._String);
      tempDataPtr^ := Ord(SS_CMD_STOPSOUND);
      inc(tempDataPtr);
    end
    else
    begin
      I_Warning('S_InitSequenceScript:() Unknown commmand.'#13#10);
    end;
  end;
  sc.Free;
end;

//==============================================================================
//
//  S_StartSequence
//
//==============================================================================
procedure S_StartSequence(mobj: Pmobj_t; sequence: integer);
var
  node: Pseqnode_t;
begin
  S_StopSequence(mobj); // Stop any previous sequence
  node := Pseqnode_t(Z_Malloc(SizeOf(seqnode_t), PU_STATIC, nil));
  node.sequencePtr := @SequenceData[SequenceTranslate[sequence].scriptNum][0];
  node.sequence := sequence;
  node.mobj := mobj;
  node.delayTics := 0;
  node.stopSound := SequenceTranslate[sequence].stopSound;
  node.volume := 127; // Start at max volume

  if SequenceListHead = nil then
  begin
    SequenceListHead := node;
    node.next := nil;
    node.prev := nil;
  end
  else
  begin
    SequenceListHead.prev := node;
    node.next := SequenceListHead;
    node.prev := nil;
    SequenceListHead := node;
  end;
  inc(ActiveSequences);
end;

//==============================================================================
//
//  S_StartSequenceName
//
//==============================================================================
procedure S_StartSequenceName(mobj: Pmobj_t; name: string);
var
  i: integer;
begin
  name := strupper(name);
  for i := 0 to Ord(SEQ_NUMSEQ) - 1 do
  begin
    if name = strupper(SequenceTranslate[i].name) then
    begin
      S_StartSequence(mobj, i);
      exit;
    end;
  end;
end;

//==============================================================================
//
//  S_StopSequence
//
//==============================================================================
procedure S_StopSequence(mobj: Pmobj_t);
var
  node: Pseqnode_t;
begin
  node := SequenceListHead;
  while node <> nil do
  begin
    if node.mobj = mobj then
    begin
      S_StopSound(mobj);
      if node.stopSound <> 0 then
        S_StartSoundAtVolume(mobj, node.stopSound, node.volume);

      if SequenceListHead = node then
        SequenceListHead := node.next;

      if node.prev <> nil then
        node.prev.next := node.next;

      if node.next <> nil then
        node.next.prev := node.prev;

      Z_Free(node);
      dec(ActiveSequences);
    end;
    node := node.next;
  end;
end;

//==============================================================================
//
//  S_UpdateActiveSequences
//
//==============================================================================
procedure S_UpdateActiveSequences;
var
  node: Pseqnode_t;
  sndPlaying: boolean;
  parm1, parm2: integer;
begin
  if (ActiveSequences = 0) or paused then
    exit; // No sequences currently playing/game is paused

  node := SequenceListHead;
  while node <> nil do
  begin
    if node.delayTics <> 0 then
    begin
      dec(node.delayTics);
      node := node.next;
      continue;
    end;

    sndPlaying := S_GetSoundPlayingInfo(node.mobj, node.currentSoundID);
    case node.sequencePtr^ of
      Ord(SS_CMD_PLAY):
        begin
          if not sndPlaying then
          begin
            inc(node.sequencePtr);
            node.currentSoundID := node.sequencePtr^;
            S_StartSoundAtVolume(node.mobj, node.currentSoundID, node.volume);
          end;
          inc(node.sequencePtr, 1);
        end;
      Ord(SS_CMD_WAITUNTILDONE):
        begin
          if not sndPlaying then
          begin
            inc(node.sequencePtr);
            node.currentSoundID := 0;
          end;
        end;
      Ord(SS_CMD_PLAYREPEAT):
        begin
          if not sndPlaying then
          begin
            inc(node.sequencePtr);
            node.currentSoundID := node.sequencePtr^;
            dec(node.sequencePtr);
            S_StartSoundAtVolume(node.mobj, node.currentSoundID, node.volume);
          end;
        end;
      Ord(SS_CMD_DELAY):
        begin
          inc(node.sequencePtr);
          node.delayTics := node.sequencePtr^;
          inc(node.sequencePtr);
          node.currentSoundID := 0;
        end;
      Ord(SS_CMD_DELAYRAND):
        begin
          inc(node.sequencePtr);
          parm1 := node.sequencePtr^;
          inc(node.sequencePtr);
          parm2 := node.sequencePtr^;
          node.delayTics := parm1 + M_Random mod (parm2 - parm1);
          node.currentSoundID := 0;
        end;
      Ord(SS_CMD_VOLUME):
        begin
          inc(node.sequencePtr);
          node.volume := (127 * node.sequencePtr^) div 100;
          inc(node.sequencePtr);
        end;
      Ord(SS_CMD_STOPSOUND):
        begin // Wait until something else stops the sequence
        end;
      Ord(SS_CMD_END):
        begin
          S_StopSequence(node.mobj);
        end;
    end;

    node := node.next;
  end;
end;

//==============================================================================
//
//  S_StopAllSequences
//
//==============================================================================
procedure S_StopAllSequences;
var
  node: Pseqnode_t;
begin
  node := SequenceListHead;
  while node <> nil do
  begin
    node.stopSound := 0; // don't play any stop sounds
    S_StopSequence(node.mobj);
    node := node.next;
  end;
end;

//==============================================================================
//
//  S_GetSequenceOffset
//
//==============================================================================
function S_GetSequenceOffset(sequence: integer; sequencePtr: PInteger): integer;
begin
  result := (integer(sequencePtr) - integer(SequenceData[SequenceTranslate[sequence].scriptNum])) div SizeOf(integer);
end;

//==============================================================================
//
//  S_ChangeNodeData
//
//   nodeNum zero is the first node
//
//==============================================================================
procedure S_ChangeNodeData(nodeNum: integer; seqOffset: integer; delayTics: integer;
  volume: integer; currentSoundID: integer);
var
  i: integer;
  node: Pseqnode_t;
begin
  i := 0;
  node := SequenceListHead;

  while (node <> nil) and (i < nodeNum) do
  begin
    node := node.next;
    inc(i);
  end;

  if node = nil then
    exit; // reach the end of the list before finding the nodeNum-th node

  node.delayTics := delayTics;
  node.volume := volume;
  inc(node.sequencePtr, seqOffset);
  node.currentSoundID := currentSoundID;
end;

end.
