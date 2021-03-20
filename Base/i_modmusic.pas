//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2021 by Jim Valavanis
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
//  Mod music file playback
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit i_modmusic;

{ Protracker Replay In Pascal (C)2017 mumart@gmail.com }

interface

procedure I_PlayMod(const data: pointer; const size: integer);

procedure I_PauseMod;

procedure I_ResumeMod;

procedure I_StopMod;

procedure I_InitMod;

procedure I_ShutDownMod;

procedure I_SetMusicVolumeMod(volume: integer);

procedure I_ProcessMod;

function IsModMusicFile(const buf: pointer; const size: integer): boolean;

const
  MICROMOD_VERSION: string = '20171013';

const
  MICROMOD_ERROR_MODULE_FORMAT_NOT_SUPPORTED: longint = -1;

const
  MICROMOD_ERROR_SAMPLING_RATE_NOT_SUPPORTED: longint = -2;

{ Calculate the length in bytes of the MOD file from the 1084 byte header. }
function MicromodCalculateFileLength(const Header1084Bytes: array of shortint): longint;

{ Initialise the replay to play the specified module. }
function MicromodInit(const Module: array of shortint; SamplingRate: longint;
  Interpolation: boolean): longint;

{ Set the sampling rate of playback.
  Returns True if the value is in range.
  Use with MicromodSetC2Rate() to adjust the playback tempo.
  For example, to play at half-speed multiply both the SamplingRate and C2Rate by 2. }
function MicromodSetSamplingRate(SamplingRate: longint): boolean;

{ Enable or disable the linear interpolation filter. }
procedure MicromodSetInterpolation(Interpolation: boolean);

{ Returns the song name. }
function MicromodGetSongName: string;

{ Returns the specified instrument name. }
function MicromodGetInstrumentName(InstrumentIndex: longint): string;

{ Returns the duration of the song in samples. }
function MicromodCalculateSongDuration: longint;

{ Get a tick of audio.
  Returns the number of stereo sample pairs produced.
  OutputBuffer should be at least SamplingRate * 2 / 5 in length. }
function MicromodGetAudio(var OutputBuffer: array of smallint): longint;

{ Quickly seek to approximately SamplePos.
  Returns the actual sample position reached. }
function MicromodSeek(SamplePos: longint): longint;

{ Get the current row int the pattern being played. }
function MicromodGetRow: longint;

{ Get the current pattern in the sequence. }
function MicromodGetSequencePos: longint;

{ Set the replay to play the specified pattern in the sequence.}
procedure MicromodSetSequencePos(SequencePos: longint);

{ Get the current value of the C2Rate. }
function MicromodGetC2Rate(): longint;

{ Set the value of the C2Rate.
  Returns True if the value is in range.
  This affects the frequency at which notes are played.
  The default is 8287hz for PAL/Amiga modules, or 8363hz for NTSC/PC modules. }
function MicromodSetC2Rate(Rate: longint): boolean;

implementation

uses
  MMSystem,
  Windows,
  d_delphi,
  d_main,
  m_fixed,
  i_system,
  i_threads,
  w_wad,
  z_zone;

const
  MAX_CHANNELS: longint = 32;
  FP_SHIFT: longint = 15;
  FP_ONE: longint = 32768;
  FP_MASK: longint = 32767;

const
  FineTuning: array[0..15] of word = (
    4340, 4308, 4277, 4247, 4216, 4186, 4156, 4126,
    4096, 4067, 4037, 4008, 3979, 3951, 3922, 3894);

  ArpTuning: array[0..15] of word = (
    4096, 4340, 4598, 4871, 5161, 5468, 5793, 6137,
    6502, 6889, 7298, 7732, 8192, 8679, 9195, 9742);

  SineTable: array[0..31] of byte = (
    0, 24, 49, 74, 97, 120, 141, 161, 180, 197, 212, 224, 235, 244, 250, 253,
    255, 253, 250, 244, 235, 224, 212, 197, 180, 161, 141, 120, 97, 74, 49, 24);

type
  TShortIntArray = array of shortint;

type
  TNote = record
    Key: word;
    Instrument, Effect, Param: byte;
  end;

type
  TInstrument = record
    Name: string;
    Volume, FineTune: byte;
    LoopStart, LoopLength: longint;
    SampleData: TShortIntArray;
  end;

type
  TChannel = record
    Note: TNote;
    Period, PortaPeriod: smallint;
    Volume, Panning, FineTune, Ampl: byte;
    SampleOffset, SampleIndex, SampleFrac, Step: longint;
    PortaSpeed, VTPhase, PLRow, FXCount, Instrument, Assigned, ID: byte;
    VibratoSpeed, VibratoDepth, TremoloSpeed, TremoloDepth: byte;
    TremoloAdd, VibratoAdd, ArpeggioAdd: shortint;
  end;

var
  SongName: string;

var
  Channels: array of TChannel;

var
  Instruments: array of TInstrument;

var
  Sequence, Patterns: array of byte;

var
  Interpolate: boolean;

var
  RampBuffer: array of smallint;

var
  SampleRate, C2Rate, Gain: longint;

var
  NumChannels, SequenceLength, RestartPos: longint;

var
  Pattern, BreakPattern, Row, NextRow, Tick: longint;

var
  Speed, Tempo, PLCount, PLChannel: longint;

function CalculateNumChannels(const Header: array of shortint): longint;
var
  NumChan, FormatID: longint;
begin
  NumChan := 0;
  if (Length(Header) >= 1084) then
  begin
    FormatID := (Header[1082] shl 8) or Header[1083];
    case FormatID of
      $4B2E: NumChan := 4; { M.K. }
      $4B21: NumChan := 4; { M!K! }
      $542E: NumChan := 4; { N.T. }
      $5434: NumChan := 4; { FLT4 }
      $484E: NumChan := Header[1080] - 48; { xCHN }
      $4348: NumChan := (Header[1080] - 48) * 10 + (Header[1081] - 48); { xxCH }
    end;
  end;
  if (NumChan < 0) or (NumChan > MAX_CHANNELS) then
    NumChan := 0;
  CalculateNumChannels := NumChan;
end;

function UBEWord(const Data: array of shortint; Index: longint): longint;
begin
  UBEWord := ((Data[Index] and $FF) shl 8) + (Data[Index + 1] and $FF);
end;

function MicromodInit(const Module: array of shortint; SamplingRate: longint;
  Interpolation: boolean): longint;
var
  NumPatterns: longint;
  StrIndex, PatIndex, SeqEntry, PatternDataLength: longint;
  SampleOffset, SampleLength, LoopStart, LoopLength: longint;
  InstIndex, Volume, FineTune: longint;
  Instrument: TInstrument;
begin
  NumChannels := CalculateNumChannels(Module);
  if NumChannels = 0 then
  begin
    MicromodInit := MICROMOD_ERROR_MODULE_FORMAT_NOT_SUPPORTED;
    Exit;
  end;
  SetLength(Channels, NumChannels);
  if not MicromodSetSamplingRate(SamplingRate) then
  begin
    MicromodInit := MICROMOD_ERROR_SAMPLING_RATE_NOT_SUPPORTED;
    Exit;
  end;
  MicromodSetInterpolation(Interpolation);
  SetLength(RampBuffer, 128);
  if (NumChannels > 4) then
  begin
    C2Rate := 8363;
    Gain := 1;
  end
  else
  begin
    C2Rate := 8287;
    Gain := 2;
  end;
  SetLength(SongName, 20);
  for StrIndex := 1 to 20 do
    SongName[StrIndex] := char(Module[StrIndex - 1] and $FF);
  SequenceLength := Module[950] and $7F;
  RestartPos := Module[951] and $7F;
  if RestartPos >= SequenceLength then
    RestartPos := 0;
  SetLength(Sequence, 128);
  NumPatterns := 0;
  for PatIndex := 0 to 127 do
  begin
    SeqEntry := Module[952 + PatIndex] and $7F;
    Sequence[PatIndex] := SeqEntry;
    if (SeqEntry >= NumPatterns) then
      NumPatterns := SeqEntry + 1;
  end;
  PatternDataLength := 4 * NumChannels * 64 * NumPatterns;
  SetLength(Patterns, PatternDataLength);
  Move(Module[1084], Patterns[0], PatternDataLength);
  SetLength(Instruments, 32);
  SampleOffset := 1084 + PatternDataLength;
  for InstIndex := 1 to 31 do
  begin
    SetLength(Instrument.Name, 22);
    for StrIndex := 1 to 22 do
      Instrument.Name[StrIndex] :=
        char(Module[InstIndex * 30 + StrIndex - 11] and $FF);
    FineTune := Module[InstIndex * 30 + 14] and $F;
    Instrument.FineTune := (FineTune and $7) - (FineTune and $8) + 8;
    Volume := Module[InstIndex * 30 + 15] and $7F;
    if Volume > 64 then
      Volume := 64;
    Instrument.Volume := Volume;
    SampleLength := UBEWord(Module, InstIndex * 30 + 12) * 2;
    if (SampleOffset + SampleLength > Length(Module)) then
      SampleLength := Length(Module) - SampleOffset;
    SetLength(Instrument.SampleData, SampleLength + 1);
    Move(Module[SampleOffset], Instrument.SampleData[0], SampleLength);
    Instrument.SampleData[SampleLength] := 0;
    SampleOffset := SampleOffset + SampleLength;
    LoopStart := UBEWord(Module, InstIndex * 30 + 16) * 2;
    LoopLength := UBEWord(Module, InstIndex * 30 + 18) * 2;
    if LoopStart + LoopLength > SampleLength then
    begin
      if LoopStart div 2 + LoopLength <= SampleLength then
      begin
        { Some old modules have loop start in bytes. }
        LoopStart := LoopStart div 2;
      end
      else
      begin
        LoopLength := SampleLength - LoopStart;
      end;
    end;
    if LoopLength < 4 then
    begin
      LoopStart := SampleLength;
      LoopLength := 0;
    end;
    Instrument.LoopStart := LoopStart;
    Instrument.LoopLength := LoopLength;
    Instrument.SampleData[LoopStart + LoopLength] := Instrument.SampleData[LoopStart];
    Instruments[InstIndex] := Instrument;
  end;
  MicromodSetSequencePos(0);
  MicromodInit := 0;
end;

function MicromodSetSamplingRate(SamplingRate: longint): boolean;
begin
  MicromodSetSamplingRate := False;
  if (SamplingRate >= 8000) and (SamplingRate <= 128000) then
  begin
    SampleRate := SamplingRate;
    MicromodSetSamplingRate := True;
  end;
end;

procedure MicromodSetInterpolation(Interpolation: boolean);
begin
  Interpolate := Interpolation;
end;

function MicromodGetSongName: string;
begin
  MicromodGetSongName := '';
  if NumChannels = 0 then
    Exit;
  MicromodGetSongName := SongName;
end;

function MicromodGetInstrumentName(InstrumentIndex: longint): string;
begin
  MicromodGetInstrumentName := '';
  if NumChannels = 0 then
    Exit;
  if (InstrumentIndex > 0) and (InstrumentIndex < 32) then
    MicromodGetInstrumentName := Instruments[InstrumentIndex].Name;
end;

function CalculateTickLength(Tempo, SamplingRate: longint): longint;
begin
  CalculateTickLength := (SamplingRate * 5) div (Tempo * 2);
end;

procedure Resample(const Channel: TChannel; var OutputBuffer: array of smallint;
  Length: longint);
var
  OutputIndex, OutputLength: longint;
  Ins, Ampl, LAmpl, RAmpl: longint;
  SampleIdx, SampleFra, Step, LoopLen, LoopEp1: longint;
  SampleData: TShortIntArray;
  C, M, Y, L, R: longint;
begin
  Ins := Channel.Instrument;
  SampleData := Instruments[Ins].SampleData;
  Ampl := Channel.Ampl;
  if Ampl <= 0 then
    Exit;
  RAmpl := Ampl * Channel.Panning;
  LAmpl := Ampl * (255 - Channel.Panning);
  SampleIdx := Channel.SampleIndex;
  SampleFra := Channel.SampleFrac;
  Step := Channel.Step;
  LoopLen := Instruments[Ins].LoopLength;
  LoopEp1 := Instruments[Ins].LoopStart + LoopLen;
  OutputIndex := 0;
  OutputLength := Length * 2;
  if Interpolate then
  begin
    while OutputIndex < OutputLength do
    begin
      if SampleIdx >= LoopEp1 then
      begin
        if LoopLen <= 1 then
          Break;
        while SampleIdx >= LoopEp1 do
          SampleIdx := SampleIdx - LoopLen;
      end;
      C := SampleData[SampleIdx] * 256;
      M := SampleData[SampleIdx + 1] * 256 - C;
      Y := ((M * SampleFra) div FP_ONE) + C;
      L := Y * LAmpl div 65536;
      R := Y * RAmpl div 65536;
      OutputBuffer[OutputIndex] := OutputBuffer[OutputIndex] + L;
      OutputIndex := OutputIndex + 1;
      OutputBuffer[OutputIndex] := OutputBuffer[OutputIndex] + R;
      OutputIndex := OutputIndex + 1;
      SampleFra := SampleFra + Step;
      SampleIdx := SampleIdx + (SampleFra shr FP_SHIFT);
      SampleFra := SampleFra and FP_MASK;
    end;
  end
  else
  begin
    while OutputIndex < OutputLength do
    begin
      if SampleIdx >= LoopEp1 then
      begin
        if LoopLen <= 1 then
          Break;
        while SampleIdx >= LoopEp1 do
          SampleIdx := SampleIdx - LoopLen;
      end;
      Y := SampleData[SampleIdx];
      L := Y * LAmpl div 256;
      R := Y * RAmpl div 256;
      OutputBuffer[OutputIndex] := OutputBuffer[OutputIndex] + L;
      OutputIndex := OutputIndex + 1;
      OutputBuffer[OutputIndex] := OutputBuffer[OutputIndex] + R;
      OutputIndex := OutputIndex + 1;
      SampleFra := SampleFra + Step;
      SampleIdx := SampleIdx + (SampleFra shr FP_SHIFT);
      SampleFra := SampleFra and FP_MASK;
    end;
  end;
end;

procedure UpdateSampleIndex(var Channel: TChannel; Length: longint);
var
  SampleFrac, SampleIndex, LoopStart, LoopLength, LoopOffset: longint;
begin
  SampleFrac := Channel.SampleFrac + Channel.Step * Length;
  SampleIndex := Channel.SampleIndex + (SampleFrac shr FP_SHIFT);
  LoopStart := Instruments[Channel.Instrument].LoopStart;
  LoopLength := Instruments[Channel.Instrument].LoopLength;
  LoopOffset := SampleIndex - LoopStart;
  if loopOffset > 0 then
  begin
    SampleIndex := LoopStart;
    if LoopLength > 1 then
      SampleIndex := SampleIndex + LoopOffset mod LoopLength;
  end;
  Channel.SampleIndex := SampleIndex;
  Channel.SampleFrac := SampleFrac and FP_MASK;
end;

procedure VolumeRamp(var Buffer: array of smallint; TickLength: longint);
var
  Offset, RampRate, A1, A2: longint;
begin
  Offset := 0;
  RampRate := 256 * 2048 div SampleRate;
  A1 := 0;
  while A1 < 256 do
  begin
    A2 := 256 - A1;
    Buffer[Offset] := (Buffer[Offset] * A1 + RampBuffer[Offset] * A2) div 256;
    Offset := Offset + 1;
    Buffer[Offset] := (Buffer[Offset] * A1 + RampBuffer[Offset] * A2) div 256;
    Offset := Offset + 1;
    A1 := A1 + RampRate;
  end;
  Move(Buffer[TickLength * 2], RampBuffer[0], 128 * SizeOf(smallint));
end;

{ 2:1 downsampling with simple but effective anti-aliasing. Buffer must contain count * 2 + 1 stereo samples. }
procedure Downsample(var Buffer: array of smallint; Count: longint);
var
  InIdx, OutIdx, OutLen: longint;
begin
  InIdx := 0;
  OutIdx := 0;
  OutLen := Count * 2;
  while OutIdx < OutLen do
  begin
    Buffer[OutIdx] := (Buffer[InIdx] div 4) +
      (Buffer[InIdx + 2] div 2) + (Buffer[InIdx + 4] div 4);
    Buffer[OutIdx + 1] := (Buffer[InIdx + 1] div 4) +
      (Buffer[InIdx + 3] div 2) + (Buffer[InIdx + 5] div 4);
    InIdx := InIdx + 4;
    OutIdx := OutIdx + 2;
  end;
end;

procedure Trigger(var Channel: TChannel);
var
  Period, Ins: longint;
begin
  Ins := Channel.Note.Instrument;
  if Ins > 0 then
  begin
    Channel.Assigned := Ins;
    Channel.SampleOffset := 0;
    Channel.FineTune := Instruments[Ins].FineTune;
    Channel.Volume := Instruments[Ins].Volume;
    if (Instruments[Ins].LoopLength > 0) and (Channel.Instrument > 0) then
      Channel.Instrument := Ins;
  end;
  if Channel.Note.Effect = $9 then
  begin
    Channel.SampleOffset := (Channel.Note.Param and $FF) shl 8;
  end
  else if Channel.Note.Effect = $15 then
  begin
    Channel.FineTune := Channel.Note.Param;
  end;
  if Channel.Note.Key > 0 then
  begin
    Period := (Channel.Note.Key * FineTuning[Channel.FineTune and $F]) shr 11;
    Channel.PortaPeriod := (Period shr 1) + (Period and 1);
    if (Channel.Note.Effect <> $3) and (Channel.Note.Effect <> $5) then
    begin
      Channel.Instrument := Channel.Assigned;
      Channel.Period := Channel.PortaPeriod;
      Channel.SampleIndex := Channel.SampleOffset;
      Channel.SampleFrac := 0;
      Channel.VTPhase := 0;
    end;
  end;
end;

procedure Vibrato(var Channel: TChannel);
var
  Phase, Out: longint;
begin
  Phase := Channel.VTPhase * Channel.VibratoSpeed;
  Out := (SineTable[Phase and $1F] * Channel.VibratoDepth) shr 7;
  if (Phase and $20) > 0 then
    Out := -Out;
  Channel.VibratoAdd := Out;
end;

procedure Tremolo(var Channel: TChannel);
var
  Phase, Out: longint;
begin
  Phase := Channel.VTPhase * Channel.TremoloSpeed;
  Out := (SineTable[Phase and $1F] * Channel.TremoloDepth) shr 6;
  if (Phase and $20) > 0 then
    Out := -Out;
  Channel.TremoloAdd := Out;
end;

procedure VolumeSlide(var Channel: TChannel; Param: longint);
var
  Volume: longint;
begin
  Volume := Channel.Volume + (Param shr 4) - (Param and $F);
  if Volume > 64 then
    Volume := 64;
  if Volume < 0 then
    Volume := 0;
  Channel.Volume := Volume;
end;

procedure UpdateFrequency(var Channel: TChannel);
var
  Period, Freq, Volume: longint;
begin
  Period := Channel.Period + Channel.VibratoAdd;
  if Period < 14 then
    Period := 6848;
  Freq := C2Rate * 107 div Period;
  Freq := ((Freq * ArpTuning[Channel.ArpeggioAdd]) shr 12) and $FFFF;
  Channel.Step := (Freq shl FP_SHIFT) div (SampleRate shr 1);
  Volume := Channel.Volume + Channel.TremoloAdd;
  if Volume > 64 then
    Volume := 64;
  if Volume < 0 then
    Volume := 0;
  Channel.Ampl := Volume * Gain;
end;

procedure ChannelRow(var Channel: TChannel);
var
  Effect, Param, Volume, Period: longint;
begin
  Effect := Channel.Note.Effect;
  Param := Channel.Note.Param;
  if not ((Effect = $1D) and (Param > 0)) then
  begin
    { Not Note Delay. }
    Trigger(Channel);
  end;
  Channel.ArpeggioAdd := 0;
  Channel.VibratoAdd := 0;
  Channel.TremoloAdd := 0;
  Channel.FXCount := 0;
  case Effect of
    $3:
    begin { Tone Portamento }
      if Param > 0 then
        Channel.PortaSpeed := Param;
    end;
    $4:
    begin { Vibrato. }
      if (Param shr 4) > 0 then
        Channel.VibratoSpeed := Param shr 4;
      if (Param and $F) > 0 then
        Channel.VibratoDepth := Param and $F;
      Vibrato(Channel);
    end;
    $6:
    begin { Vibrato + Volume Slide. }
      Vibrato(Channel);
    end;
    $7:
    begin { Tremolo }
      if (Param shr 4) > 0 then
        Channel.TremoloSpeed := Param shr 4;
      if (Param and $F) > 0 then
        Channel.TremoloDepth := Param and $F;
      Tremolo(Channel);
    end;
    $8:
    begin { Set Panning. Not for 4-channel ProTracker. }
      if NumChannels <> 4 then
      begin
        if Param < 128 then
          Channel.Panning := (Param shl 1)
        else
          Channel.Panning := 255;
      end;
    end;
    $B:
    begin { Pattern Jump. }
      if PLCount < 0 then
      begin
        BreakPattern := Param;
        NextRow := 0;
      end;
    end;
    $C:
    begin { Set Volume }
      if Param > 64 then
        Channel.Volume := 64
      else
        Channel.Volume := Param;
    end;
    $D:
    begin { Pattern Break. }
      if PLCount < 0 then
      begin
        if BreakPattern < 0 then
          BreakPattern := Pattern + 1;
        NextRow := (Param shr 4) * 10 + (Param and $F);
        if NextRow >= 64 then
          NextRow := 0;
      end;
    end;
    $F:
    begin { Set Speed }
      if Param > 0 then
        if Param < 32 then
        begin
          Speed := Param;
          Tick := Speed;
        end
        else
        begin
          Tempo := Param;
        end;
    end;
    $11:
    begin { Fine Portamento Up }
      Period := Channel.Period - Param;
      if Period < 0 then
        Period := 0;
      Channel.Period := Period;
    end;
    $12:
    begin { Fine Portamento Down }
      Period := Channel.Period + Param;
      if Period > 65535 then
        Period := 65535;
      Channel.Period := Period;
    end;
    $16:
    begin { Pattern Loop }
      if Param = 0 then
        Channel.PLRow := Row;
      if (Channel.PLRow < Row) and (BreakPattern < 0) then
      begin
        if PlCount < 0 then
        begin
          PLCount := Param;
          PLChannel := Channel.ID;
        end;
        if PLChannel = Channel.ID then
        begin
          if PLCount = 0 then
          begin
            Channel.PLRow := Row + 1;
          end
          else
          begin
            NextRow := Channel.PLRow;
          end;
          PLCount := PLCount - 1;
        end;
      end;
    end;
    $1A:
    begin { Fine Volume Up }
      Volume := Channel.Volume + Param;
      if Volume > 64 then
        Volume := 64;
      Channel.Volume := Volume;
    end;
    $1B:
    begin { Fine Volume Down }
      Volume := Channel.Volume - Param;
      if Volume < 0 then
        Volume := 0;
      Channel.Volume := Volume;
    end;
    $1C:
    begin { Note Cut }
      if Param <= 0 then
        Channel.Volume := 0;
    end;
    $1E:
    begin { Pattern Delay }
      Tick := Speed + Speed * Param;
    end;
  end;
  UpdateFrequency(Channel);
end;

function SequenceRow: boolean;
var
  SongEnd: boolean;
  PatternOffset, Chan: longint;
  Effect, Param: byte;
  Note: TNote;
begin
  SongEnd := False;
  if NextRow < 0 then
  begin
    BreakPattern := Pattern + 1;
    NextRow := 0;
  end;
  if BreakPattern >= 0 then
  begin
    if BreakPattern >= SequenceLength then
    begin
      BreakPattern := 0;
      NextRow := 0;
    end;
    if BreakPattern <= Pattern then
      SongEnd := True;
    Pattern := BreakPattern;
    for Chan := 0 to NumChannels - 1 do
      Channels[Chan].PLRow := 0;
    BreakPattern := -1;
  end;
  Row := NextRow;
  NextRow := Row + 1;
  if NextRow >= 64 then
    NextRow := -1;
  PatternOffset := (Sequence[Pattern] * 64 + Row) * NumChannels * 4;
  for Chan := 0 to NumChannels - 1 do
  begin
    Note.Key := (Patterns[PatternOffset] and $F) shl 8;
    Note.Key := Note.Key or Patterns[PatternOffset + 1];
    Note.Instrument := (Patterns[PatternOffset + 2] and $F0) shr 4;
    Note.Instrument := Note.Instrument or (Patterns[PatternOffset] and $10);
    Effect := Patterns[PatternOffset + 2] and $F;
    Param := Patterns[PatternOffset + 3];
    PatternOffset := PatternOffset + 4;
    if Effect = $E then
    begin
      Effect := $10 or (Param shr 4);
      Param := Param and $F;
    end;
    if (Effect = 0) and (Param > 0) then
      Effect := $E;
    Note.Effect := Effect;
    Note.Param := Param;
    Channels[Chan].Note := Note;
    ChannelRow(Channels[Chan]);
  end;
  SequenceRow := SongEnd;
end;

procedure TonePortamento(var Channel: TChannel);
var
  Source, Destin: longint;
begin
  Source := Channel.Period;
  Destin := Channel.PortaPeriod;
  if Source < Destin then
  begin
    Source := Source + Channel.PortaSpeed;
    if (Source > Destin) then
      Source := Destin;
  end;
  if Source > Destin then
  begin
    Source := Source - Channel.PortaSpeed;
    if (Source < Destin) then
      Source := Destin;
  end;
  Channel.Period := Source;
end;

procedure ChannelTick(var Channel: TChannel);
var
  Effect, Param, Period: longint;
begin
  Effect := Channel.Note.Effect;
  Param := Channel.Note.Param;
  Channel.VTPhase := Channel.VTPhase + 1;
  Channel.FXCount := Channel.FXCount + 1;
  case Effect of
    $1:
    begin { Portamento Up.}
      Period := Channel.Period - Param;
      if Period < 0 then
        Period := 0;
      Channel.Period := Period;
    end;
    $2:
    begin { Portamento Down. }
      Period := Channel.Period + Param;
      if Period > 65535 then
        Period := 65535;
      Channel.Period := Period;
    end;
    $3:
    begin { Tone Portamento. }
      TonePortamento(Channel);
    end;
    $4:
    begin { Vibrato. }
      Vibrato(Channel);
    end;
    $5:
    begin { Tone Portamento + Volume Slide. }
      TonePortamento(Channel);
      VolumeSlide(Channel, Param);
    end;
    $6:
    begin { Vibrato + Volume Slide. }
      Vibrato(Channel);
      VolumeSlide(Channel, Param);
    end;
    $7:
    begin { Tremolo. }
      Tremolo(Channel);
    end;
    $A:
    begin { Volume Slide }
      VolumeSlide(Channel, Param);
    end;
    $E:
    begin { Arpeggio }
      if Channel.FXCount > 2 then
        Channel.FXCount := 0;
      if Channel.FXCount = 0 then
        Channel.ArpeggioAdd := 0;
      if Channel.FXCount = 1 then
        Channel.ArpeggioAdd := Param shr 4;
      if Channel.FXCount = 2 then
        Channel.ArpeggioAdd := Param and $F;
    end;
    $19:
    begin { Retrig }
      if Channel.FXCount >= Param then
      begin
        Channel.FXCount := 0;
        Channel.SampleIndex := 0;
        Channel.SampleFrac := 0;
      end;
    end;
    $1C:
    begin { Note Cut }
      if Param = Channel.FXCount then
        Channel.Volume := 0;
    end;
    $1D:
    begin { Note Delay }
      if Param = Channel.FXCount then
        Trigger(Channel);
    end;
  end;
  if Effect > 0 then
    UpdateFrequency(Channel);
end;

function SequenceTick: boolean;
var
  SongEnd: boolean;
  Chan: longint;
begin
  SongEnd := False;
  Tick := Tick - 1;
  if Tick <= 0 then
  begin
    Tick := Speed;
    SongEnd := SequenceRow;
  end
  else
  begin
    for Chan := 0 to NumChannels - 1 do
      ChannelTick(Channels[Chan]);
  end;
  SequenceTick := SongEnd;
end;

function MicromodGetAudio(var OutputBuffer: array of smallint): longint;
var
  TickLength, Chan: longint;
begin
  MicromodGetAudio := 0;
  if NumChannels > 0 then
  begin
    TickLength := CalculateTickLength(Tempo, SampleRate);
    FillChar(OutputBuffer[0], (TickLength + 65) * 4 * SizeOf(smallint), 0);
    for Chan := 0 to NumChannels - 1 do
    begin
      Resample(Channels[Chan], OutputBuffer, (TickLength + 65) * 2);
      UpdateSampleIndex(Channels[Chan], TickLength * 2);
    end;
    Downsample(OutputBuffer, TickLength + 64);
    VolumeRamp(OutputBuffer, TickLength);
    SequenceTick();
    MicromodGetAudio := TickLength;
  end;
end;

function MicromodSeek(SamplePos: longint): longint;
var
  CurrentPos, TickLength, Chan: longint;
begin
  CurrentPos := 0;
  if NumChannels > 0 then
  begin
    MicromodSetSequencePos(0);
    TickLength := CalculateTickLength(Tempo, SampleRate);
    while (SamplePos - CurrentPos) >= TickLength do
    begin
      for Chan := 0 to NumChannels - 1 do
        UpdateSampleIndex(Channels[Chan], TickLength * 2);
      CurrentPos := CurrentPos + TickLength;
      SequenceTick();
      TickLength := CalculateTickLength(Tempo, SampleRate);
    end;
  end;
  MicromodSeek := CurrentPos;
end;

function MicromodCalculateSongDuration: longint;
var
  SongEnd: boolean;
  Duration: longint;
begin
  Duration := 0;
  if NumChannels > 0 then
  begin
    MicromodSetSequencePos(0);
    SongEnd := False;
    while not SongEnd do
    begin
      Duration := Duration + CalculateTickLength(Tempo, SampleRate);
      SongEnd := SequenceTick();
    end;
    MicromodSetSequencePos(0);
  end;
  MicromodCalculateSongDuration := Duration;
end;

function MicromodGetRow: longint;
begin
  MicromodGetRow := Row;
end;

function MicromodGetSequencePos: longint;
begin
  MicromodGetSequencePos := Pattern;
end;

procedure MicromodSetSequencePos(SequencePos: longint);
var
  Chan: longint;
begin
  if NumChannels = 0 then
    Exit;
  if SequencePos >= SequenceLength then
    SequencePos := 0;
  BreakPattern := SequencePos;
  NextRow := 0;
  Tick := 1;
  Speed := 6;
  Tempo := 125;
  PLCount := -1;
  PLChannel := -1;
  for Chan := 0 to NumChannels - 1 do
  begin
    FillChar(Channels[Chan], SizeOf(TChannel), 0);
    Channels[Chan].ID := Chan;
    case Chan and 3 of
      0: Channels[Chan].Panning := 51;
      1: Channels[Chan].Panning := 204;
      2: Channels[Chan].Panning := 204;
      3: Channels[Chan].Panning := 51;
    end;
  end;
  FillChar(RampBuffer[0], 128 * SizeOf(smallint), 0);
  SequenceTick();
end;

function MicromodGetC2Rate(): longint;
begin
  MicromodGetC2Rate := C2Rate;
end;

function MicromodSetC2Rate(Rate: longint): boolean;
begin
  MicromodSetC2Rate := False;
  if (Rate > 0) and (Rate < 65536) then
  begin
    C2Rate := Rate;
    MicromodSetC2Rate := True;
  end;
end;

function MicromodCalculateFileLength(const Header1084Bytes: array of shortint): longint;
var
  NumChan, NumPatterns, PatIndex, SeqEntry, Length, InstIndex: longint;
begin
  Length := MICROMOD_ERROR_MODULE_FORMAT_NOT_SUPPORTED;
  NumChan := CalculateNumChannels(Header1084Bytes);
  if NumChan > 0 then
  begin
    NumPatterns := 0;
    for PatIndex := 0 to 127 do
    begin
      SeqEntry := Header1084Bytes[952 + PatIndex] and $7F;
      if (SeqEntry >= NumPatterns) then
        NumPatterns := SeqEntry + 1;
    end;
    Length := 1084 + 4 * NumChan * 64 * NumPatterns;
    for InstIndex := 1 to 31 do
    begin
      Length := Length + UBEWord(Header1084Bytes, InstIndex * 30 + 12) * 2;
    end;
  end;
  MicromodCalculateFileLength := Length;
end;

const
  SAMPLING_FREQ: longint = 48000; { 48khz. }
  NUM_CHANNELS: longint = 2;     { Stereo. }
  BUFFER_SAMPLES: longint = 16384; { 64k per buffer. }
  NUM_BUFFERS: longint = 8;     { 8 buffers. }
  EXIT_FAILURE: integer = 1;


procedure CheckMMError(ReturnCode: MMRESULT);
var
  ErrorText: array[0..63] of char;
begin
  if ReturnCode <> MMSYSERR_NOERROR then
  begin
    WaveOutGetErrorText(ReturnCode, @ErrorText, Length(ErrorText));
    I_Error('CheckMMError(): ' + string(ErrorText));
  end;
end;


const
  MOD_MSG_NONE = 0;
  MOD_MSG_PLAY = 1;
  MOD_MSG_PAUSE = 2;
  MOD_MSG_RESUME = 3;
  MOD_MSG_STOP = 4;
  MOD_MSG_DESTROY = 5;

  MOD_STATUS_IDLE = 0;
  MOD_STATUS_PLAYING = 1;

var
  mod_msg: integer = 0;
  mod_datapointer: pointer;
  mod_datasize: integer;
  mod_thread: TDThread;
  mod_status: integer = 0;

var
  Semaphore: THandle;
  MixBuffer: array Of SmallInt;
  MixIndex, MixLength: LongInt;


procedure WaveOutProc(hWaveOut: HWAVEOUT; uMsg: UINT;
  dwInstance, dwParam1, dwParam2: DWORD) stdcall;
begin
  if uMsg = WOM_DONE then
    ReleaseSemaphore(Semaphore, 1, nil);
end;

procedure LoadModule;
var
  ModuleData: array of shortint;
  FileLength, ReturnCode: longint;
  ms: TAttachableMemoryStream;
  i: integer;
  si: ShortInt;
begin
  ismultithread := true;
  while mod_datapointer = nil do
    I_Sleep(10);

  if mod_datasize < 1084 then
    I_Error('LoadModule(): Unable to read module header');

  ms :=  TAttachableMemoryStream.Create;
  ms.Attach(mod_datapointer, mod_datasize);
  SetLength(ModuleData, 1084);
  for i := 0 to 1083 do
  begin
    ms.Read(si, SizeOf(ShortInt));
    ModuleData[i] := si;
  end;
  FileLength := MicromodCalculateFileLength(ModuleData);
  if FileLength = MICROMOD_ERROR_MODULE_FORMAT_NOT_SUPPORTED then
    I_Error('TModMusicPlayer.LoadModule(): Module format not supported');

  SetLength(ModuleData, FileLength);
  for i := 1084 to FileLength - 1084 - 1 do
  begin
    ms.Read(si, SizeOf(ShortInt));
    ModuleData[i] := si;
  end;
  ms.Free;
  if mod_datasize < FileLength then
    I_Warning('TModMusicPlayer.LoadModule(): Module file has been truncated! Should be %d.'#13#10, [FileLength]);
  ReturnCode := MicromodInit(ModuleData, SAMPLING_FREQ, False);
  if ReturnCode <> 0 then
    I_Error('TModMusicPlayer.LoadModule(): Unable to initialize replay! Error Code = %d', [ReturnCode]);
  SetLength(ModuleData, 0);
  ismultithread := false;
end;

var
  modvolume: integer = $FFFF;

procedure GetAudio(var OutputBuffer: array of smallint; Length: longint);
var
  OutOffset, OutRemain, Count: longint;
  i: integer;
begin
  OutOffset := 0;
  while OutOffset < Length do
  begin
    OutRemain := Length - OutOffset;
    Count := MixLength - MixIndex;
    if Count > OutRemain then
      Count := OutRemain;
    Move(MixBuffer[MixIndex * 2], OutputBuffer[OutOffset * 2],
      Count * 2 * SizeOf(smallint));
    if modvolume < 65535 then
      for i := 0 to 2 * Count - 1 do
        OutputBuffer[OutOffset * 2 + i] := FixedMul(OutputBuffer[OutOffset * 2 + i], modvolume);

    MixIndex := MixIndex + Count;
    if MixIndex >= MixLength then
    begin
      MixLength := MicromodGetAudio(MixBuffer);
      MixIndex := 0;
    end;
    OutOffset := OutOffset + Count;
  end;
end;

procedure PlayModule;
var
  WaveFormat: TWaveFormatEx;
  WaveOutHandle: HWaveOut;
  WaveHeaders: array of TWaveHdr;
  WaveBuffers: array of array of smallint;
  PWaveHeader: PWaveHdr;
  Idx, Err, CurrentBuffer, SamplesRemaining, InitialSamples, Count: longint;
begin
  ismultithread := true;
  { Initialise Wave Format Structure. }
  WaveFormat.wFormatTag := WAVE_FORMAT_PCM;
  WaveFormat.nChannels := NUM_CHANNELS;
  WaveFormat.nSamplesPerSec := SAMPLING_FREQ;
  WaveFormat.nAvgBytesPerSec := SAMPLING_FREQ * NUM_CHANNELS * 2;
  WaveFormat.nBlockAlign := NUM_CHANNELS * 2;
  WaveFormat.wBitsPerSample := 16;

  { Initialise Waveform Buffers. }
  SetLength(WaveBuffers, NUM_BUFFERS, BUFFER_SAMPLES * NUM_CHANNELS);
  SetLength(WaveHeaders, NUM_BUFFERS);
  for Idx := 0 to NUM_BUFFERS - 1 do
  begin
    FillChar(WaveHeaders[Idx], SizeOf(TWaveHdr), 0);
    WaveHeaders[Idx].lpData := @WaveBuffers[Idx][0];
    WaveHeaders[Idx].dwBufferLength := BUFFER_SAMPLES * NUM_CHANNELS * 2;
  end;

  { Initialise Semaphore. }
  Semaphore := CreateSemaphore(nil, NUM_BUFFERS, NUM_BUFFERS, '');

  { Open Audio Device. }
  Err := WaveOutOpen(@WaveOutHandle, WAVE_MAPPER, @WaveFormat, DWORD(
    @WaveOutProc), 0, CALLBACK_FUNCTION);
  CheckMMError(Err);

  { Calculate Duration. }
  InitialSamples := MicromodCalculateSongDuration;
  SamplesRemaining := InitialSamples;

  { Initialise Mix Buffer. }
  SetLength(MixBuffer, SAMPLING_FREQ * 2 div 5);

  ismultithread := false;
  { Play Through Once. }
  CurrentBuffer := 0;
  while true do
  begin
    { Wait for a buffer to become available. }
    WaitForSingleObject(Semaphore, INFINITE);

    if SamplesRemaining <= 0 then
      SamplesRemaining := InitialSamples;

    while mod_msg = MOD_MSG_PAUSE do
      I_Sleep(10);

    if mod_msg = MOD_MSG_STOP then
      break;

    { Get audio from replay. }
    Count := BUFFER_SAMPLES;
    if Count > SamplesRemaining then
    begin
      { Last buffer, clear as it will be partially filled. }
      FillChar(WaveBuffers[CurrentBuffer][0], BUFFER_SAMPLES * NUM_CHANNELS * 2, 0);
      Count := SamplesRemaining;
    end;
    GetAudio(WaveBuffers[CurrentBuffer], Count);
    SamplesRemaining := SamplesRemaining - Count;

    { Submit buffer to audio system. }
    PWaveHeader := @WaveHeaders[CurrentBuffer];
    CheckMMError(WaveOutUnprepareHeader(WaveOutHandle, PWaveHeader,
      SizeOf(TWaveHdr)));
    CheckMMError(WaveOutPrepareHeader(WaveOutHandle, PWaveHeader, SizeOf(TWaveHdr)));
    CheckMMError(WaveOutWrite(WaveOutHandle, PWaveHeader, SizeOf(TWaveHdr)));

    { Next buffer. }
    CurrentBuffer := CurrentBuffer + 1;
    if CurrentBuffer >= NUM_BUFFERS then
      CurrentBuffer := 0;
    if SamplesRemaining = 0 then
       SamplesRemaining := InitialSamples;

  end;

  { Close audio device when finished. }
  while WaveOutClose(WaveOutHandle) = WAVERR_STILLPLAYING do
    I_Sleep(100);

  mod_msg := MOD_MSG_NONE;
end;

procedure I_PlayMod(const data: pointer; const size: integer);
begin
  if mod_status = MOD_STATUS_PLAYING then
  begin
    mod_msg := MOD_MSG_STOP;
    while mod_msg <> MOD_MSG_NONE do
      I_Sleep(10);
  end;

  mod_thread.Wait;

  mod_datapointer := data;
  mod_datasize := size;

  mod_msg := MOD_MSG_PLAY;
  mod_thread.Activate(nil);
end;

procedure I_PauseMod;
begin
  if mod_status = MOD_STATUS_PLAYING then
    mod_msg := MOD_MSG_PAUSE;
end;

procedure I_ResumeMod;
begin
  if mod_status = MOD_STATUS_PLAYING then
    if mod_msg = MOD_MSG_PAUSE then
      mod_msg := MOD_MSG_RESUME;
end;

procedure I_StopMod;
begin
  mod_msg := MOD_MSG_STOP;
end;

function threadproc(p: pointer): integer; stdcall;
begin
  mod_status := MOD_STATUS_PLAYING;
  LoadModule;
  PlayModule;
  result := 0;
  mod_status := MOD_STATUS_IDLE;
end;

procedure I_InitMod;
begin
  mod_msg := MOD_MSG_NONE;
  mod_status := MOD_STATUS_IDLE;
  mod_thread := TDThread.Create(@threadproc);
end;

procedure I_ShutDownMod;
begin
  if mod_status = MOD_STATUS_PLAYING then
  begin
    mod_msg := MOD_MSG_STOP;
    while mod_msg <> MOD_MSG_NONE do
      I_Sleep(10);
  end;
  mod_thread.Wait;
  mod_thread.Free;
end;

const
  MOD_VOLUME_CONTROL: array[0..15] of word = (
        0,     4369,     8738,    13107,
    17476,    21845,    26214,    30583,
    34952,    39321,    43690,    48059,
    52428,    56797,    61166,    65535
  );

procedure I_SetMusicVolumeMod(volume: integer);
var
  vol: integer;
begin
  vol := GetIntegerInRange(volume, 0, 15);
  modvolume := MOD_VOLUME_CONTROL[vol];
end;

procedure I_ProcessMod;
begin
end;

function IsModMusicFile(const buf: pointer; const size: integer): boolean;
var
  ModuleData: array of shortint;
  ms: TAttachableMemoryStream;
  i: integer;
  si: ShortInt;
begin
  if size < 1084 then
  begin
    result := false;
    exit;
  end;

  ms :=  TAttachableMemoryStream.Create;
  ms.Attach(buf, size);

  SetLength(ModuleData, 1084);
  for i := 0 to 1083 do
  begin
    ms.Read(si, SizeOf(ShortInt));
    ModuleData[i] := si;
  end;
  result := MicromodCalculateFileLength(ModuleData) <> MICROMOD_ERROR_MODULE_FORMAT_NOT_SUPPORTED;
  ms.Free;
end;

end.
