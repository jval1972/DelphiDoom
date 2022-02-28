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
//  Midi file playback
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

{$WARN SYMBOL_DEPRECATED OFF}

unit i_midi_new;

interface

//==============================================================================
//
// I_PlayMidi_New
//
//==============================================================================
procedure I_PlayMidi_New(const aMidiFile: string);

//==============================================================================
//
// I_StopMidi_New
//
//==============================================================================
procedure I_StopMidi_New;

//==============================================================================
//
// I_IsMidiPlaying_New
//
//==============================================================================
function I_IsMidiPlaying_New: boolean;

//==============================================================================
//
// I_PauseMidi_New
//
//==============================================================================
procedure I_PauseMidi_New;

//==============================================================================
//
// I_ResumeMidi_New
//
//==============================================================================
procedure I_ResumeMidi_New;

//==============================================================================
//
// I_InitMidi_New
//
//==============================================================================
procedure I_InitMidi_New;

//==============================================================================
//
// I_ShutDownMidi_New
//
//==============================================================================
procedure I_ShutDownMidi_New;

//==============================================================================
//
// I_SetMusicVolumeMidi_New
//
//==============================================================================
procedure I_SetMusicVolumeMidi_New(volume: integer);

//==============================================================================
//
// I_ProcessMidi_New
//
//==============================================================================
procedure I_ProcessMidi_New;

implementation

uses
  Windows,
  d_delphi,
  d_main,
  i_system,
  i_music,
  i_midi,
  i_io,
  Messages,
  MMSystem;

// This message is sent to the controlling window, if the volume changes in
// another way than explicitly set by the owner of the CMIDI object.
// WPARAM: the pointer to the MIDI object
// LPARAM: lo-word: the number of the channel that changed volume
//         hi-word: the new volume in percent
const
  WM_MIDI_VOLUMECHANGED = WM_USER + 23;
  MIDI_PRGMCHANGE: Byte = $C0; // + new patch
  MIDI_CHANPRESS: Byte = $D0; // + pressure (1 byte)

  MIDI_SYSEX: Byte = $F0; // SysEx begin
  MIDI_SYSEXEND: Byte = $F7; // SysEx end
  MIDI_META: Byte = $FF; // Meta event begin
  MIDI_META_TEMPO: Byte = $51; // Tempo change
  MIDI_META_EOT: Byte = $2F; // End-of-track
  // flags for the ConvertToBuffer() method
  CONVERTF_RESET = $00000001;
  CONVERTF_STATUS_DONE = $00000001;
  CONVERTF_STATUS_STUCK = $00000002;
  CONVERTF_STATUS_GOTEVENT = $00000004;
  // Return values from the ConvertToBuffer() method
  CONVERTERR_NOERROR = 0; // No error occured
  CONVERTERR_CORRUPT = -101; // The input file is corrupt
  // The converter has already encountered a corrupt file and cannot convert any
  // more of this file -- must reset the converter
  CONVERTERR_STUCK = -102;
  CONVERTERR_DONE = -103; // Converter is done
  CONVERTERR_BUFFERFULL = -104; // The buffer is full
  CONVERTERR_METASKIP = -105; // Skipping unknown meta event

  STATUS_KILLCALLBACK = 100; // Signals that the callback should die
  STATUS_CALLBACKDEAD = 200; // Signals callback is done processing
  STATUS_WAITINGFOREND = 300; // Callback's waiting for buffers to play

// Description of a track
type
  _TRACK = record
    fdwTrack: DWORD; // Track's flags
    dwTrackLength: DWORD; // Total bytes in track
    pTrackStart: PBYTE; // -> start of track data buffer
    pTrackCurrent: PBYTE; // -> next byte to read in buffer
    tkNextEventDue: DWORD; // Absolute time of next event in track
    byRunningStatus: BYTE; // Running status from last channel msg
  end;
  TTRACK = _TRACK;
  LPTRACK = ^_TRACK;

const
  ITS_F_ENDOFTRK = $00000001;

// This structure is used to pass information to the ConvertToBuffer()
// system and then internally by that function to send information about the
// target stream buffer and current state of the conversion process to internal
// lower level conversion routines.
type
  _CONVERTINFO = record
    mhBuffer: MIDIHDR; // Standard Windows stream buffer header
    dwStartOffset: DWORD; // Start offset from mhStreamBuffer.lpStart
    dwMaxLength: DWORD; // Max length to convert on this pass
    dwBytesRecorded: DWORD;
    tkStart: DWORD;
    bTimesUp: BOOL;
  end;
  TCONVERTINFO = _CONVERTINFO;
  LPCONVERTINFO = ^_CONVERTINFO;

// Temporary event structure which stores event data until we're ready to
// dump it into a stream buffer
type
  _TEMPEVENT = record
    tkEvent: DWORD; // Absolute time of event
    byShortData: array[0..3] of BYTE; // Event type and parameters if channel msg
    dwEventLength: DWORD; // Length of data which follows if meta or sysex
    pLongData: PBYTE; // -> Event data if applicable
  end;
  TTEMPEVENT = _TEMPEVENT;
  LPTEMPEVENT = ^_TEMPEVENT;

const
  NUM_CHANNELS = 16; // 16 volume channels
  VOLUME_INIT = 100; // 100% volume by default
  NUM_STREAM_BUFFERS = 2;
  OUT_BUFFER_SIZE = 1024; // Max stream buffer size in bytes
  DEBUG_CALLBACK_TIMEOUT = 2000;
  VOLUME_MIN = 0;
  VOLUME_MAX = 127; // == 100%

type
  TrackArray_t = array of TTRACK;
  VolumeArray_t = array of DWORD;
  ConvertArray_t = array of TCONVERTINFO;

type
  TMidi = class(Tobject)
  private
    m_dwSoundSize: DWORD;
    m_pSoundData: Pointer;
    m_dwFormat: DWORD;
    m_dwTrackCount: DWORD;
    m_dwTimeDivision: DWORD;
    m_bPlaying: BOOL;
    m_hStream: HMIDISTRM;
    m_dwProgressBytes: DWORD;
    m_bLooped: BOOL;
    m_tkCurrentTime: DWORD;
    m_dwBufferTickLength: DWORD;
    m_dwCurrentTempo: DWORD;
    m_dwTempoMultiplier: DWORD;
    m_bInsertTempo: BOOL;
    m_bBuffersPrepared: BOOL;
    m_nCurrentBuffer: integer;
    m_uMIDIDeviceID: UINT;
    m_nEmptyBuffers: integer;
    m_bPaused: BOOL;
    m_uCallbackStatus: UINT;
    m_hBufferReturnEvent: THandle;
    m_Tracks: TrackArray_t;
    m_Volumes: VolumeArray_t;
    m_StreamBuffers: ConvertArray_t;
    // data members especially for ConvertToBuffer()
    m_ptsTrack: LPTRACK;
    m_ptsFound: LPTRACK;
    m_dwStatus: DWORD;
    m_tkNext: DWORD;
    m_dwMallocBlocks: DWORD;
    m_teTemp: TTEMPEVENT;
    m_volume: integer;
  protected
    // This function converts MIDI data from the track buffers.
    function ConvertToBuffer(dwFlags: DWORD; lpciInfo: LPCONVERTINFO): integer;
    // Fills in the event struct with the next event from the track
    function GetTrackEvent(ptsTrack: LPTRACK; pteTemp: LPTEMPEVENT): BOOL;
    // Retrieve the next byte from the track buffer, refilling the buffer from
    // disk if necessary.
    function GetTrackByte(ptsTrack: LPTRACK; lpbyByte: PBYTE): BOOL;
    // Attempts to parse a variable length DWORD from the given track.
    function GetTrackVDWord(ptsTrack: LPTRACK; lpdw: PDWORD): BOOL;
    // Put the given event into the given stream buffer at the given location.
    function AddEventToStreamBuffer(pteTemp: LPTEMPEVENT; lpciInfo: LPCONVERTINFO): integer;
    // Opens a MIDI stream. Then it goes about converting the data into a midiStream buffer for playback.
    function StreamBufferSetup(): BOOL;
    procedure FreeBuffers();
    // Error handling
    procedure MidiError(mmResult: MMRESULT); virtual;
    // Failure in converting track into stream.
    // The default implementation displays the offset and the total
    // number of bytes of the failed track and the error message in
    // the debuggers output window.
    procedure TrackError(ptsTrack: LPTRACK; const lpszErr: string); virtual;
    // called when a MIDI output device is opened
    procedure OnMidiOutOpen(); virtual;
    // called when the MIDI output device is closed
    procedure OnMidiOutClose(); virtual;
    // called when the specified system-exclusive or stream buffer
    // has been played and is being returned to the application
    procedure OnMidiOutDone(var rHdr: MIDIHDR); virtual;
    // called when a MEVT_F_CALLBACK event is reached in the MIDI output stream
    procedure OnMidiOutPositionCB(var rHdr: MIDIHDR; var rEvent: MIDIEVENT); virtual;
    // Debug Outpur
    procedure DebugOutput(const fmt: string; const A: array of const);
    // Set globalvolume
    procedure SetGlobalVolume(const v: integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function LoadData(pSoundData: Pointer; dwSize: DWORD): BOOL;

    function Play(bInfinite: BOOL = False): BOOL;
    function Stop(bReOpen: BOOL = True): BOOL;
    function IsPlaying(): BOOL;

    function Pause(): BOOL;
    function Resume(): BOOL;
    function IsPaused(): BOOL;

    // Set playback position back to the start
    function Rewind(): BOOL;

    // Get the number of volume channels
    function GetChannelCount(): DWORD;

    // Set the volume of a channel in percent. Channels are from 0 to (GetChannelCount()-1)
    procedure SetChannelVolume(dwChannel, dwPercent: DWORD);

    // Get the volume of a channel in percent
    function GetChannelVolume(dwChannel: DWORD): DWORD;

    // Set the volume for all channels in percent
    procedure SetVolume(dwpercent: DWORD);

    // Get the average volume for all channels
    function GetVolume(): DWORD;

    // Set the tempo of the playback. Default: 100%
    procedure SetTempo(dwPercent: DWORD);

    // Get the current tempo in percent (usually 100)
    function GetTempo(): DWORD;

    // You can (un)set an infinite loop during playback.
    // Note that "Play()" resets this setting!
    procedure SetInfinitePlay(bSet: BOOL = True);

    property volume: integer read m_volume write SetGlobalVolume;
  end;

const
  BUFFER_TIME_LENGTH = 60; // Amount to fill in milliseconds

  // These structures are stored in MIDI files; they need to be byte aligned.
  //
{$ALIGN 1}

  // Contents of MThd chunk.
type
  _MIDIFILEHDR = record
    wFormat: WORD; // Format (hi-lo)
    wTrackCount: WORD; // # tracks (hi-lo)
    wTimeDivision: WORD; // Time division (hi-lo)
  end;
  TMIDIFILEHDR = _MIDIFILEHDR;
  LPMIDIFILEHDR = ^_MIDIFILEHDR;

{$ALIGN OFF} // End of need for byte-aligned structures

//==============================================================================
// WORDSWAP
//
// Macros for swapping hi/lo-endian data
//
//==============================================================================
function WORDSWAP(w: WORD): WORD;
begin
  Result := (w shr 8) or ((w shl 8) and $FF00);
end;

//==============================================================================
//
// DWORDSWAP
//
//==============================================================================
function DWORDSWAP(dw: DWORD): DWORD;
begin
  Result := (dw shr 24) or ((dw shr 8) and $0000FF00) or ((dw shl 8) and $00FF0000) or ((dw shl 24) and $FF000000);
end;

const
  gteBadRunStat = 'Reference to missing running status.';
  gteRunStatMsgTrunc = 'Running status message truncated';
  gteChanMsgTrunc = 'Channel message truncated';
  gteSysExLenTrunc = 'SysEx event truncated (length)';
  gteSysExTrunc = 'SysEx event truncated';
  gteMetaNoClass = 'Meta event truncated (no class byte)';
  gteMetaLenTrunc = 'Meta event truncated (length)';
  gteMetaTrunc = 'Meta event truncated';
  gteNoMem = 'Out of memory during malloc call';

//==============================================================================
//
// MIDIEVENT_CHANNEL
//
//==============================================================================
function MIDIEVENT_CHANNEL(dw: DWORD): DWORD;
begin
  Result := dw and $0000000F;
end;

//==============================================================================
//
// MIDIEVENT_TYPE
//
//==============================================================================
function MIDIEVENT_TYPE(dw: DWORD): DWORD;
begin
  Result := dw and $000000F0;
end;

//==============================================================================
//
// MIDIEVENT_DATA1
//
//==============================================================================
function MIDIEVENT_DATA1(dw: DWORD): DWORD;
begin
  Result := (dw and $0000FF00) shr 8;
end;

//==============================================================================
//
// MIDIEVENT_VOLUME
//
//==============================================================================
function MIDIEVENT_VOLUME(dw: DWORD): DWORD;
begin
  Result := (dw and $007F0000) shr 16;
end;

//==============================================================================
//
// AssertMidiError
//
//==============================================================================
procedure AssertMidiError(const b: boolean; const Fmt: string; const A: array of const);
begin
  if b then
    Exit;
  I_Error(Fmt, A);
end;

//==============================================================================
//
// MidiProc
//
//==============================================================================
procedure MidiProc(hMidi: HMIDIOUT; uMsg: UINT; dwInstanceData, dwParam1, dwParam2: DWORD); stdcall;
var
  pMidi: TMidi;
  pHdr: PMIDIHDR;
begin
  pMidi := TMidi(dwInstanceData);
  AssertMidiError(pMidi <> nil, 'MidiProc(): Midi object is null', []);
  pHdr := PMIDIHDR(dwParam1);

  case uMsg of
    MOM_OPEN: pMidi.OnMidiOutOpen;
    MOM_CLOSE: pMidi.OnMidiOutClose;
    MOM_DONE:
      begin
        AssertMidiError(pHdr <> nil, 'MidiProc(): Midi header is null', []);
        pMidi.OnMidiOutDone(pHdr^);
      end;
    MOM_POSITIONCB:
      begin
        AssertMidiError(pHdr <> nil, 'MidiProc(): Midi header is null', []);
        pMidi.OnMidiOutPositionCB(pHdr^, PMIDIEVENT(pHdr^.lpData + pHdr^.dwOffset)^);
      end;
  end;
end;

//==============================================================================
//
// TMidi.LoadData
//
//==============================================================================
function TMidi.LoadData(pSoundData: Pointer; dwSize: DWORD): BOOL;
var
  p: PBYTE;
  dwHeaderSize: DWORD;
  hdr: TMIDIFILEHDR;
  i: integer;
begin
  if m_pSoundData <> nil then
  begin
    // already created
    AssertMidiError(False, 'TMidi.LoadData(): Sound data already created', []);
    Result := False;
    Exit;
  end;

  AssertMidiError(pSoundData <> nil, 'TMidi.LoadData(): Sound data is null', []);
  AssertMidiError(dwSize > 0, 'TMidi.LoadData(): Data size is zero', []);

  p := PBYTE(pSoundData);

  // check header of MIDI
  if PDWORD(p)^ <> MThd then
  begin
    AssertMidiError(False, 'TMidi.LoadData(): Invalid header', []);
    Result := False;
    Exit;
  end;
  Inc(p, SizeOf(DWORD));

  // check header size
  dwHeaderSize := DWORDSWAP(PDWORD(p)^);
  if dwHeaderSize <> SizeOf(TMIDIFILEHDR) then
  begin
    AssertMidiError(False, 'TMidi.LoadData(): Invalid header', []);
    Result := False;
    Exit;
  end;
  Inc(p, SizeOf(DWORD));

  // get header
  CopyMemory(@hdr, p, dwHeaderSize);
  m_dwFormat := DWORD(WORDSWAP(hdr.wFormat));
  m_dwTrackCount := DWORD(WORDSWAP(hdr.wTrackCount));
  m_dwTimeDivision := DWORD(WORDSWAP(hdr.wTimeDivision));
  Inc(p, dwHeaderSize);

  // create the array of tracks
  SetLength(m_Tracks, m_dwTrackCount);
  for i := 0 to m_dwTrackCount - 1 do
  begin
    // check header of track
    if PDWORD(p)^ <> MTrk then
    begin
      AssertMidiError(False, 'TMidi.LoadData(): Invalid track header', []);
      Result := False;
      Exit;
    end;
    Inc(p, SizeOf(DWORD));

    m_Tracks[i].dwTrackLength := DWORDSWAP(PDWORD(p)^);
    Inc(p, SizeOf(DWORD));

    m_Tracks[i].pTrackCurrent := p;
    m_Tracks[i].pTrackStart := m_Tracks[i].pTrackCurrent;
    Inc(p, m_Tracks[i].dwTrackLength);

    // Handle bozo MIDI files which contain empty track chunks
    if m_Tracks[i].dwTrackLength = 0 then
    begin
      m_Tracks[i].fdwTrack := m_Tracks[i].fdwTrack or ITS_F_ENDOFTRK;
      continue;
    end;

    // We always preread the time from each track so the mixer code can
    // determine which track has the next event with a minimum of work
    if not GetTrackVDWord(@m_Tracks[i], @m_Tracks[i].tkNextEventDue) then
    begin
      I_Error('TMidi.LoadData(): Error in MIDI data');
      Result := False;
      Exit;
    end;
  end;

  m_pSoundData := pSoundData;
  m_dwSoundSize := dwSize;

  // allocate volume channels and initialise them
  SetLength(m_Volumes, NUM_CHANNELS);
  for i := Low(m_Volumes) to High(m_Volumes) do m_Volumes[i] := VOLUME_INIT;
  //vc:m_Volumes.resize(NUM_CHANNELS, VOLUME_INIT);

  if not StreamBufferSetup() then
  begin
    AssertMidiError(False, 'TMidi.LoadData(): Can not setup stream buffer', []);
    Result := False;
    Exit;
  end;

  Result := True;
end;

//==============================================================================
// TMidi.AddEventToStreamBuffer
//
// AddEventToStreamBuffer
//
// Put the given event into the given stream buffer at the given location
// pteTemp must point to an event filled out in accordance with the
// description given in GetTrackEvent
//
// Handles its own error notification by displaying to the appropriate
// output device (either our debugging window, or the screen).
//
//==============================================================================
function TMidi.AddEventToStreamBuffer(pteTemp: LPTEMPEVENT;
  lpciInfo: LPCONVERTINFO): integer;
var
  pmeEvent: PMIDIEVENT;
  tkDelta: DWORD;
begin
  pmeEvent := PMIDIEVENT(lpciInfo^.mhBuffer.lpData
    + lpciInfo^.dwStartOffset
    + lpciInfo^.dwBytesRecorded);

  // When we see a new, empty buffer, set the start time on it...
  if (lpciInfo^.dwBytesRecorded = 0) then
    lpciInfo^.tkStart := m_tkCurrentTime;

  // Use the above set start time to figure out how much longer we should fill
  // this buffer before officially declaring it as "full"
  if (m_tkCurrentTime - lpciInfo^.tkStart > m_dwBufferTickLength) then
    if (lpciInfo^.bTimesUp) then
    begin
      lpciInfo^.bTimesUp := False;
      Result := CONVERTERR_BUFFERFULL;
      Exit;
    end
    else
      lpciInfo^.bTimesUp := True;

  // Delta time is absolute event time minus absolute time
  // already gone by on this track
  tkDelta := pteTemp^.tkEvent - m_tkCurrentTime;

  // Event time is now current time on this track
  m_tkCurrentTime := pteTemp^.tkEvent;

  if m_bInsertTempo then
  begin
    m_bInsertTempo := False;

    if (lpciInfo^.dwMaxLength - lpciInfo^.dwBytesRecorded < 3 * SizeOf(DWORD)) then
    begin
      // Cleanup from our write operation
      Result := CONVERTERR_BUFFERFULL;
      Exit;
    end;
    if (m_dwCurrentTempo <> 0) then
    begin
      pmeEvent^.dwDeltaTime := 0;
      pmeEvent^.dwStreamID := 0;
      pmeEvent^.dwEvent := (m_dwCurrentTempo * 100) div m_dwTempoMultiplier;
      pmeEvent^.dwEvent := pmeEvent^.dwEvent or ((DWORD(MEVT_TEMPO) shl 24) or MEVT_F_SHORT);

      lpciInfo^.dwBytesRecorded := lpciInfo^.dwBytesRecorded + 3 * SizeOf(DWORD);
      Inc(pmeEvent, 3 * SizeOf(DWORD));
    end;
  end;

  if pteTemp^.byShortData[0] < MIDI_SYSEX then
  begin
    // Channel message. We know how long it is, just copy it.
    // Need 3 DWORD's: delta-t, stream-ID, event
    if lpciInfo^.dwMaxLength - lpciInfo^.dwBytesRecorded < 3 * SizeOf(DWORD) then
    begin
      // Cleanup from our write operation
      Result := CONVERTERR_BUFFERFULL;
      Exit;
    end;

    pmeEvent^.dwDeltaTime := tkDelta;
    pmeEvent^.dwStreamID := 0;
    pmeEvent^.dwEvent := (pteTemp^.byShortData[0])
      or ((DWORD(pteTemp^.byShortData[1])) shl 8)
      or ((DWORD(pteTemp^.byShortData[2])) shl 16)
      or MEVT_F_SHORT;

    if ((pteTemp^.byShortData[0] and $F0) = MIDI_CTRLCHANGE) and (pteTemp^.byShortData[1] = MIDICTRL_VOLUME) then
    begin
      // If this is a volume change, generate a callback so we can grab
      // the new volume for our cache
      pmeEvent^.dwEvent := pmeEvent^.dwEvent or MEVT_F_CALLBACK;
    end;
    lpciInfo^.dwBytesRecorded := lpciInfo^.dwBytesRecorded + 3 * SizeOf(DWORD);
  end
  else
  if (pteTemp^.byShortData[0] = MIDI_SYSEX) or (pteTemp^.byShortData[0] = MIDI_SYSEXEND) then
  begin
    DebugOutput('TMidi.AddEventToStreamBuffer(): AddEventToStreamBuffer: Ignoring SysEx event.'#13#10, []);
    if m_dwMallocBlocks <> 0 then
    begin
      FreeMem(pteTemp^.pLongData);
      pteTemp^.pLongData := nil;
      Dec(m_dwMallocBlocks);
    end;
  end
  else
  begin
    // Better be a meta event.
    //  BYTE  byEvent
    //  BYTE  byEventType
    //  VDWORD  dwEventLength
    //  BYTE  pLongEventData[dwEventLength]
    AssertMidiError(pteTemp^.byShortData[0] = MIDI_META, 'TMidi.AddEventToStreamBuffer(): Invalid event', []);

    // The only meta-event we care about is change tempo
    if pteTemp^.byShortData[1] <> MIDI_META_TEMPO then
    begin
      if (m_dwMallocBlocks <> 0) then
      begin
        FreeMem(pteTemp^.pLongData);
        pteTemp^.pLongData := nil;
        Dec(m_dwMallocBlocks);
      end;
      Result := CONVERTERR_METASKIP;
      Exit;
    end;

    // We should have three bytes of parameter data...
    AssertMidiError(pteTemp^.dwEventLength = 3, 'TMidi.AddEventToStreamBuffer(): Invalid parameter data length', []);

    // Need 3 DWORD's: delta-t, stream-ID, event data
    if lpciInfo^.dwMaxLength - lpciInfo^.dwBytesRecorded < 3 * SizeOf(DWORD) then
    begin
      // Cleanup the temporary event if necessary and return
      if m_dwMallocBlocks <> 0 then
      begin
        FreeMem(pteTemp^.pLongData);
        pteTemp^.pLongData := nil;
        Dec(m_dwMallocBlocks);
      end;
      Result := CONVERTERR_BUFFERFULL;
      Exit;
    end;

    pmeEvent^.dwDeltaTime := tkDelta;
    pmeEvent^.dwStreamID := 0;
    // Note: this is backwards from above because we're converting a single
    //       data value from hi-lo to lo-hi format...

    pmeEvent^.dwEvent := (PByte(DWORD(pteTemp^.pLongData) + 2)^)
      or (DWORD(PBYTE(DWORD(pteTemp^.pLongData) + 1)^) shl 8)
      or (DWORD(PBYTE(DWORD(pteTemp^.pLongData))^) shl 16);

    //MessageBox(0, PChar(IntToStr(pmeEvent^.dwEvent)), '', 0);

    // This next step has absolutely nothing to do with the conversion of a
    // MIDI file to a stream, it's simply put here to add the functionality
    // of the tempo slider. If you don't need this, be sure to remove the
    // next two lines.
    m_dwCurrentTempo := pmeEvent^.dwEvent;
    pmeEvent^.dwEvent := (pmeEvent^.dwEvent * 100) div m_dwTempoMultiplier;

    pmeEvent^.dwEvent := pmeEvent^.dwEvent or (((DWORD(MEVT_TEMPO)) shl 24) or MEVT_F_SHORT);

    m_dwBufferTickLength := (m_dwTimeDivision * 1000 * BUFFER_TIME_LENGTH) div m_dwCurrentTempo;
    DebugOutput('TMidi.AddEventToStreamBuffer(): m_dwBufferTickLength = %d'#13#10, [m_dwBufferTickLength]);

    if m_dwMallocBlocks <> 0 then
    begin
      FreeMem(pteTemp^.pLongData);
      pteTemp^.pLongData := nil;
      Dec(m_dwMallocBlocks);
    end;
    lpciInfo^.dwBytesRecorded := lpciInfo^.dwBytesRecorded + 3 * SizeOf(DWORD);
  end;

  Result := CONVERTERR_NOERROR;
end;

//==============================================================================
//
// TMidi.Resume
//
//==============================================================================
function TMidi.Resume: BOOL;
begin
  if m_bPaused and m_bPlaying and (m_pSoundData <> nil) and (m_hStream <> 0) then
  begin
    midiStreamRestart(m_hStream);
    m_bPaused := False;
  end;
  Result := False;
end;

//==============================================================================
// TMidi.ConvertToBuffer
//
// This function converts MIDI data from the track buffers setup by a
// previous call to ConverterInit().  It will convert data until an error is
// encountered or the output buffer has been filled with as much event data
// as possible, not to exceed dwMaxLength. This function can take a couple
// bit flags, passed through dwFlags. Information about the success/failure
// of this operation and the number of output bytes actually converted will
// be returned in the CONVERTINFO structure pointed at by lpciInfo.
//
//==============================================================================
function TMidi.ConvertToBuffer(dwFlags: DWORD;
  lpciInfo: LPCONVERTINFO): integer;
var
  nChkErr: integer;
  idx: DWORD;
begin
  lpciInfo^.dwBytesRecorded := 0;

  if dwFlags and CONVERTF_RESET <> 0 then
  begin
    m_dwProgressBytes := 0;
    m_dwStatus := 0;
    ZeroMemory(@m_teTemp, SizeOf(TTEMPEVENT));
    m_ptsFound := nil;
    m_ptsTrack := m_ptsFound;
  end;

  // If we were already done, then return with a warning...
  if m_dwStatus and CONVERTF_STATUS_DONE <> 0 then
  begin
    if m_bLooped then
    begin
      Rewind();
      m_dwProgressBytes := 0;
      m_dwStatus := 0;
    end
    else
    begin
      Result := CONVERTERR_DONE;
      Exit;
    end;
  end
  else
  if m_dwStatus and CONVERTF_STATUS_STUCK <> 0 then
  begin
    // The caller is asking us to continue, but we're already hosed because we
    // previously identified something as corrupt, so complain louder this time.
    Result := CONVERTERR_STUCK;
    Exit;
  end
  else if m_dwStatus and CONVERTF_STATUS_GOTEVENT <> 0 then
  begin
    // Turn off this bit flag
    m_dwStatus := m_dwStatus xor CONVERTF_STATUS_GOTEVENT;

    // The following code for this case is duplicated from below, and is
    // designed to handle a "straggler" event, should we have one left over
    // from previous processing the last time this function was called.

    // Don't add end of track event 'til we're done
    if (m_teTemp.byShortData[0] = MIDI_META) and (m_teTemp.byShortData[1] = MIDI_META_EOT) then
    begin
      if m_dwMallocBlocks <> 0 then
      begin
        FreeMem(m_teTemp.pLongData);
        m_teTemp.pLongData := nil;
        dec(m_dwMallocBlocks);
      end;
    end
    else
    begin
      nChkErr := AddEventToStreamBuffer(@m_teTemp, lpciInfo);
      if (nChkErr <> CONVERTERR_NOERROR) then
      begin
        if (nChkErr = CONVERTERR_BUFFERFULL) then
        begin
          // Do some processing and tell caller that this buffer's full
          m_dwStatus := m_dwStatus or CONVERTF_STATUS_GOTEVENT;
          Result := CONVERTERR_NOERROR;
          Exit;
        end
        else if nChkErr = CONVERTERR_METASKIP then
        begin
          // We skip by all meta events that aren't tempo changes...
        end
        else
        begin
          DebugOutput('TMidi.ConvertToBuffer(): Unable to add event to stream buffer.'#13#10, []);
          if m_dwMallocBlocks <> 0 then
          begin
            FreeMem(m_teTemp.pLongData);
            m_teTemp.pLongData := nil;
            Dec(m_dwMallocBlocks);
          end;
          Result := 1;
          Exit;
        end;
      end;
    end;

  end;

  while true do
  begin
    m_ptsFound := nil;
    m_tkNext := $FFFFFFFF;
    // Find nearest event due
    for idx := 0 to Length(m_Tracks) - 1 do
    begin
      m_ptsTrack := @m_Tracks[idx];
      if (m_ptsTrack^.fdwTrack and ITS_F_ENDOFTRK = 0) and (m_ptsTrack^.tkNextEventDue < m_tkNext) then
      begin
        m_tkNext := m_ptsTrack^.tkNextEventDue;
        m_ptsFound := m_ptsTrack;
      end;
    end;

    // None found?  We must be done, so return to the caller with a smile.
    if m_ptsFound = nil then
    begin
      m_dwStatus := m_dwStatus or CONVERTF_STATUS_DONE;
      // Need to set return buffer members properly
      Result := CONVERTERR_NOERROR;
      Exit;
    end;

    // Ok, get the event header from that track
    if not GetTrackEvent(m_ptsFound, @m_teTemp) then
    begin
      // Warn future calls that this converter is stuck at a corrupt spot
      // and can't continue
      m_dwStatus := m_dwStatus or CONVERTF_STATUS_STUCK;
      Result := CONVERTERR_CORRUPT;
      Exit;
    end;

    // Don't add end of track event 'til we're done
    if (m_teTemp.byShortData[0] = MIDI_META) and (m_teTemp.byShortData[1] = MIDI_META_EOT) then
    begin
      if m_dwMallocBlocks <> 0 then
      begin
        FreeMem(m_teTemp.pLongData);
        m_teTemp.pLongData := nil;
        Dec(m_dwMallocBlocks);
      end;
      continue;
    end;

    nChkErr := AddEventToStreamBuffer(@m_teTemp, lpciInfo);
    if nChkErr <> CONVERTERR_NOERROR then
    begin
      if nChkErr = CONVERTERR_BUFFERFULL then
      begin
        // Do some processing and tell somebody this buffer is full...
        m_dwStatus := m_dwStatus or CONVERTF_STATUS_GOTEVENT;
        Result := CONVERTERR_NOERROR;
        Exit;
      end
      else if nChkErr = CONVERTERR_METASKIP then
      begin
        // We skip by all meta events that aren't tempo changes...
      end
      else
      begin
        DebugOutput('TMidi.ConvertToBuffer(): Unable to add event to stream buffer.'#13#10, []);
        if m_dwMallocBlocks <> 0 then
        begin
          FreeMem(m_teTemp.pLongData);
          m_teTemp.pLongData := nil;
          Dec(m_dwMallocBlocks);
        end;
        Result := 1;
        Exit;
      end;
    end;
  end;

  Result := CONVERTERR_NOERROR;
end;

//==============================================================================
//
// TMidi.Create
//
//==============================================================================
constructor TMidi.Create;
begin
  m_dwSoundSize := 0;
  m_pSoundData := nil;
  m_dwFormat := 0;
  m_dwTrackCount := 0;
  m_dwTimeDivision := 0;
  m_bPlaying := False;
  m_hStream := 0;
  m_dwProgressBytes := 0;
  m_bLooped := False;
  m_tkCurrentTime := 0;
  m_dwBufferTickLength := 0;
  m_dwCurrentTempo := 0;
  m_dwTempoMultiplier := 100;
  m_bInsertTempo := False;
  m_bBuffersPrepared := False;
  m_nCurrentBuffer := 0;
  m_uMIDIDeviceID := I_SelectDefaultMidiDevice;
  m_nEmptyBuffers := 0;
  m_bPaused := False;
  m_uCallbackStatus := 0;
  m_hBufferReturnEvent := 0;

  m_ptsTrack := nil;
  m_ptsFound := nil;
  m_dwStatus := 0;
  m_tkNext := 0;
  m_dwMallocBlocks := 0;

  m_volume := VOLUME_INIT;

  m_hBufferReturnEvent := CreateEvent(nil, False, False, 'Wait For Buffer Return');
  AssertMidiError(m_hBufferReturnEvent <> 0, 'TMidi.Create(): Can not create event', []);
  Inherited;
end;

//==============================================================================
//
// TMidi.Destroy
//
//==============================================================================
destructor TMidi.Destroy;
begin
  Stop(False);

  if m_hBufferReturnEvent <> 0 then
    CloseHandle(m_hBufferReturnEvent);
  inherited;
end;

//==============================================================================
// TMidi.FreeBuffers
//
// This function unprepares and frees all our buffers -- something we must
// do to work around a bug in MMYSYSTEM that prevents a device from playing
// back properly unless it is closed and reopened after each stop.
//
//==============================================================================
procedure TMidi.FreeBuffers;
var
  idx: DWORD;
  mmrRetVal: MMRESULT;
begin
  if m_bBuffersPrepared then
  begin
    for idx := 0 to NUM_STREAM_BUFFERS - 1 do
    begin
      mmrRetVal := midiOutUnprepareHeader(HMIDIOUT(m_hStream),
        @m_StreamBuffers[idx].mhBuffer,
        SizeOf(MIDIHDR));
      if (mmrRetVal <> MMSYSERR_NOERROR) then
      begin
        MidiError(mmrRetVal);
      end;
    end;
    m_bBuffersPrepared := False;
  end;
  // Free our stream buffers...
  for idx := 0 to NUM_STREAM_BUFFERS - 1 do
    if m_StreamBuffers[idx].mhBuffer.lpData <> nil then
    begin
      FreeMem(m_StreamBuffers[idx].mhBuffer.lpData);
      m_StreamBuffers[idx].mhBuffer.lpData := nil;
    end;
end;

//==============================================================================
//
// TMidi.GetChannelCount
//
//==============================================================================
function TMidi.GetChannelCount: DWORD;
begin
  Result := Length(m_Volumes);
end;

//==============================================================================
//
// TMidi.GetChannelVolume
//
//==============================================================================
function TMidi.GetChannelVolume(dwChannel: DWORD): DWORD;
begin
  AssertMidiError(dwChannel < GetChannelCount(), 'TMidi.GetChannelVolume(): Invalid channel number (%d)', [dwChannel]);
  Result := m_Volumes[dwChannel];
end;

//==============================================================================
//
// TMidi.GetTempo
//
//==============================================================================
function TMidi.GetTempo: DWORD;
begin
  Result := m_dwTempoMultiplier;
end;

//==============================================================================
//
// TMidi.GetTrackByte
//
//==============================================================================
function TMidi.GetTrackByte(ptsTrack: LPTRACK; lpbyByte: PBYTE): BOOL;
begin
  if DWORD(ptsTrack^.pTrackCurrent) - DWORD(ptsTrack^.pTrackStart) = ptsTrack^.dwTrackLength then
  begin
    Result := False;
    Exit;
  end;
  lpbyByte^ := PByte(ptsTrack^.pTrackCurrent)^;
  Inc(ptsTrack^.pTrackCurrent);
  Result := True;
end;

//==============================================================================
// TMidi.GetTrackEvent
//
// GetTrackEvent
//
// Fills in the event struct with the next event from the track
//
// pteTemp->tkEvent will contain the absolute tick time of the event
// pteTemp->byShortData[0] will contain
//  MIDI_META if the event is a meta event;
//   in this case pteTemp->byShortData[1] will contain the meta class
//  MIDI_SYSEX or MIDI_SYSEXEND if the event is a SysEx event
//  Otherwise, the event is a channel message and pteTemp->byShortData[1]
//   and pteTemp->byShortData[2] will contain the rest of the event.
//
// pteTemp->dwEventLength will contain
//  The total length of the channel message in pteTemp->byShortData if
//   the event is a channel message
//  The total length of the paramter data pointed to by
//   pteTemp->pLongData otherwise
//
// pteTemp->pLongData will point at any additional paramters if the
//  event is a SysEx or meta event with non-zero length; else
//  it will contain NULL
//
// Returns True on success or False on any kind of parse error
// Prints its own error message ONLY in the debug version
//
// Maintains the state of the input track (i.e.
// ptsTrack->pTrackPointers, and ptsTrack->byRunningStatus).
//
//==============================================================================
function TMidi.GetTrackEvent(ptsTrack: LPTRACK;
  pteTemp: LPTEMPEVENT): BOOL;
var
  idx: DWORD;
  dwEventLength: UINT;
  byByte: BYTE;
  tkDelta: DWORD;
begin
  // Clear out the temporary event structure to get rid of old data...
  ZeroMemory(pteTemp, SizeOf(TTEMPEVENT));

  // Already at end of track? There's nothing to read.
  if ptsTrack^.fdwTrack and ITS_F_ENDOFTRK <> 0 then
  begin
    Result := False;
    Exit;
  end;

  // Get the first byte, which determines the type of event.
  if not GetTrackByte(ptsTrack, @byByte) then
  begin
    Result := False;
    Exit;
  end;

  // If the high bit is not set, then this is a channel message
  // which uses the status byte from the last channel message
  // we saw. NOTE: We do not clear running status across SysEx or
  // meta events even though the spec says to because there are
  // actually files out there which contain that sequence of data.
  if byByte and $80 = 0 then
  begin
    // No previous status byte? We're hosed.
    if (ptsTrack^.byRunningStatus = 0) then
    begin
      TrackError(ptsTrack, gteBadRunStat);
      Result := False;
      Exit;
    end;

    pteTemp^.byShortData[0] := ptsTrack^.byRunningStatus;
    pteTemp^.byShortData[1] := byByte;

    byByte := pteTemp^.byShortData[0] and $F0;
    pteTemp^.dwEventLength := 2;

    // Only program change and channel pressure events are 2 bytes long;
    // the rest are 3 and need another byte
    if ((byByte <> MIDI_PRGMCHANGE) and (byByte <> MIDI_CHANPRESS)) then
    begin
      if not GetTrackByte(ptsTrack, @pteTemp^.byShortData[2]) then
      begin
        Result := False;
        Exit;
      end;
      Inc(pteTemp^.dwEventLength);
    end;
  end
  else if byByte and $F0 <> MIDI_SYSEX then
  begin
    // Not running status, not in SysEx range - must be
    // normal channel message (0x80-0xEF)
    pteTemp^.byShortData[0] := byByte;
    ptsTrack^.byRunningStatus := byByte;

    // Strip off channel and just keep message type
    byByte := byByte and $F0;

    if (byByte = MIDI_PRGMCHANGE) or (byByte = MIDI_CHANPRESS) then
      dwEventLength := 1 else dwEventLength := 2;
    pteTemp^.dwEventLength := dwEventLength + 1;

    if not GetTrackByte(ptsTrack, @pteTemp^.byShortData[1]) then
    begin
      Result := False;
      Exit;
    end;
    if dwEventLength = 2 then
      if not GetTrackByte(ptsTrack, @pteTemp^.byShortData[2]) then
      begin
        Result := False;
        Exit;
      end;
  end
  else if (byByte = MIDI_SYSEX) or (byByte = MIDI_SYSEXEND) then
  begin
    // One of the SysEx types. (They are the same as far as we're concerned;
    // there is only a semantic difference in how the data would actually
    // get sent when the file is played. We must take care to put the proper
    // event type back on the output track, however.)
    //
    // Parse the general format of:
    //  BYTE  bEvent (MIDI_SYSEX or MIDI_SYSEXEND)
    //  VDWORD  cbParms
    //  BYTE  abParms[cbParms]
    pteTemp^.byShortData[0] := byByte;
    if not GetTrackVDWord(ptsTrack, @pteTemp^.dwEventLength) then
    begin
      TrackError(ptsTrack, gteSysExLenTrunc);
      Result := False;
      Exit;
    end;

    // Malloc a temporary memory block to hold the parameter data
    GetMem(pteTemp^.pLongData, pteTemp^.dwEventLength);
    if pteTemp^.pLongData = nil then
    begin
      TrackError(ptsTrack, gteNoMem);
      Result := False;
      Exit;
    end;
    // Increment our counter, which tells the program to look around for
    // a malloc block to free, should it need to exit or reset before the
    // block would normally be freed
    Inc(m_dwMallocBlocks);

    // Copy from the input buffer to the parameter data buffer
    for idx := 0 to pteTemp^.dwEventLength - 1 do
      if not GetTrackByte(ptsTrack, PByte(PChar(pteTemp^.pLongData) + idx)) then
      begin
        TrackError(ptsTrack, gteSysExTrunc);
        Result := False;
        Exit;
      end;
  end
  else if byByte = MIDI_META then
  begin
    // It's a meta event. Parse the general form:
    //  BYTE  bEvent  (MIDI_META)
    //  BYTE  bClass
    //  VDWORD  cbParms
    //  BYTE  abParms[cbParms]
    pteTemp^.byShortData[0] := byByte;

    if not GetTrackByte(ptsTrack, @pteTemp^.byShortData[1]) then
    begin
      Result := False;
      Exit;
    end;

    if not GetTrackVDWord(ptsTrack, @pteTemp^.dwEventLength) then
    begin
      TrackError(ptsTrack, gteMetaLenTrunc);
      Result := False;
      Exit;
    end;

    // NOTE: It's perfectly valid to have a meta with no data
    // In this case, dwEventLength == 0 and pLongData == NULL
    if pteTemp^.dwEventLength <> 0 then
    begin
      // Malloc a temporary memory block to hold the parameter data
      GetMem(pteTemp^.pLongData, pteTemp^.dwEventLength);
      if pteTemp^.pLongData = nil then
      begin
        TrackError(ptsTrack, gteNoMem);
        Result := False;
        Exit;
      end;
      // Increment our counter, which tells the program to look around for
      // a malloc block to free, should it need to exit or reset before the
      // block would normally be freed
      Inc(m_dwMallocBlocks);

      // Copy from the input buffer to the parameter data buffer
      for idx := 0 to pteTemp^.dwEventLength - 1 do
        if not GetTrackByte(ptsTrack, PByte(PChar(pteTemp^.pLongData) + idx)) then
        begin
          TrackError(ptsTrack, gteMetaTrunc);
          Result := False;
          Exit;
        end;
    end;

    if (pteTemp^.byShortData[1] = MIDI_META_EOT) then
      ptsTrack^.fdwTrack := ptsTrack^.fdwTrack or ITS_F_ENDOFTRK;
  end
  else
  begin
    // Messages in this range are system messages and aren't supposed to
    // be in a normal MIDI file. If they are, we've either misparsed or the
    // authoring software is stupid.
    Result := False;
    Exit;
  end;

  // Event time was already stored as the current track time
  pteTemp^.tkEvent := ptsTrack^.tkNextEventDue;

  // Now update to the next event time. The code above MUST properly
  // maintain the end of track flag in case the end of track meta is
  // missing.  NOTE: This code is a continuation of the track event
  // time pre-read which is done at the end of track initialization.
  if ptsTrack^.fdwTrack and ITS_F_ENDOFTRK = 0 then
  begin
    if not GetTrackVDWord(ptsTrack, @tkDelta) then
    begin
      Result := False;
      Exit;
    end;

    Inc(ptsTrack^.tkNextEventDue, tkDelta);
  end;

  Result := True;
end;

//==============================================================================
// TMidi.GetTrackVDWord
//
// GetTrackVDWord
//
// Attempts to parse a variable length DWORD from the given track. A VDWord
// in a MIDI file
//  (a) is in lo-hi format
//  (b) has the high bit set on every byte except the last
//
// Returns the DWORD in *lpdw and True on success; else
// False if we hit end of track first.
//
//==============================================================================
function TMidi.GetTrackVDWord(ptsTrack: LPTRACK; lpdw: PDWORD): BOOL;
var
  byByte: Byte;
  dw: DWORD;
begin
  AssertMidiError(ptsTrack <> nil, 'TMidi.GetTrackVDWord(): Track is null', []);
  AssertMidiError(lpdw <> nil, 'TMidi.GetTrackVDWord(): Parameter is null', []);

  if ptsTrack^.fdwTrack and ITS_F_ENDOFTRK <> 0 then
  begin
    Result := False;
    Exit;
  end;

  dw := 0;

  repeat
    begin
      if not GetTrackByte(ptsTrack, @byByte) then
      begin
        Result := False;
        Exit;
      end;

      dw := (dw shl 7) or (byByte and $7F);
    end;
  until byByte and $80 = 0;

  lpdw^ := dw;

  Result := True;
end;

//==============================================================================
//
// TMidi.GetVolume
//
//==============================================================================
function TMidi.GetVolume: DWORD;
var
  dwVolume, i: DWORD;
begin
  dwVolume := 0;
  for i := Low(m_Volumes) to High(m_Volumes) do
    Inc(dwVolume, GetChannelVolume(i));

  Result := dwVolume div GetChannelCount();
end;

//==============================================================================
//
// TMidi.IsPaused
//
//==============================================================================
function TMidi.IsPaused: BOOL;
begin
  Result := m_bPaused;
end;

//==============================================================================
//
// TMidi.IsPlaying
//
//==============================================================================
function TMidi.IsPlaying: BOOL;
begin
  Result := m_bPlaying;
end;

//==============================================================================
//
// TMidi.MidiError
//
//==============================================================================
procedure TMidi.MidiError(mmResult: MMRESULT);
var
  chText: array[0..511] of char;
begin
  midiOutGetErrorText(mmResult, chText, SizeOf(chText));
  DebugOutput('Midi error: %s'#13#10, [chText]);
end;

//==============================================================================
//
// TMidi.OnMidiOutClose
//
//==============================================================================
procedure TMidi.OnMidiOutClose;
begin
  DebugOutput('Midi device closed'#13#10, []);
end;

//==============================================================================
//
// TMidi.OnMidiOutDone
//
//==============================================================================
procedure TMidi.OnMidiOutDone(var rHdr: MIDIHDR);
var
  nChkErr: integer;
  mmrRetVal: MMRESULT;
begin
  if m_uCallbackStatus = STATUS_CALLBACKDEAD then
    Exit;

  Inc(m_nEmptyBuffers);

  if m_uCallbackStatus = STATUS_WAITINGFOREND then
  begin
    if m_nEmptyBuffers < NUM_STREAM_BUFFERS then
      Exit
    else
    begin
      m_uCallbackStatus := STATUS_CALLBACKDEAD;
      Stop();
      SetEvent(m_hBufferReturnEvent);
      Exit;
    end;
  end;

  // This flag is set whenever the callback is waiting for all buffers to
  // come back.
  if m_uCallbackStatus = STATUS_KILLCALLBACK then
  begin
    // Count NUM_STREAM_BUFFERS-1 being returned for the last time
    if m_nEmptyBuffers < NUM_STREAM_BUFFERS then
      Exit
    else
    begin
      // Change the status to callback dead
      m_uCallbackStatus := STATUS_CALLBACKDEAD;
      SetEvent(m_hBufferReturnEvent);
      Exit;
    end;
  end;

  Inc(m_dwProgressBytes, m_StreamBuffers[m_nCurrentBuffer].mhBuffer.dwBytesRecorded);

  ///////////////////////////////////////////////////////////////////////////////
  // Fill an available buffer with audio data again...

  if m_bPlaying and (m_nEmptyBuffers <> 0) then
  begin
    m_StreamBuffers[m_nCurrentBuffer].dwStartOffset := 0;
    m_StreamBuffers[m_nCurrentBuffer].dwMaxLength := OUT_BUFFER_SIZE;
    m_StreamBuffers[m_nCurrentBuffer].tkStart := 0;
    m_StreamBuffers[m_nCurrentBuffer].dwBytesRecorded := 0;
    m_StreamBuffers[m_nCurrentBuffer].bTimesUp := False;

    nChkErr := ConvertToBuffer(0, @m_StreamBuffers[m_nCurrentBuffer]);
    if (nChkErr <> CONVERTERR_NOERROR) then
    begin
      if (nChkErr = CONVERTERR_DONE) then
      begin
        m_uCallbackStatus := STATUS_WAITINGFOREND;
        Exit;
      end
      else
      begin
        DebugOutput('MidiProc() conversion pass failed!'#13#10, []);
        Exit;
      end;
    end;

    m_StreamBuffers[m_nCurrentBuffer].mhBuffer.dwBytesRecorded := m_StreamBuffers[m_nCurrentBuffer].dwBytesRecorded;

    mmrRetVal := midiStreamOut(m_hStream, @m_StreamBuffers[m_nCurrentBuffer].mhBuffer, SizeOf(MIDIHDR));
    if (mmrRetVal <> MMSYSERR_NOERROR) then
    begin
      MidiError(mmrRetVal);
      Exit;
    end;
    m_nCurrentBuffer := (m_nCurrentBuffer + 1) mod NUM_STREAM_BUFFERS;
    Dec(m_nEmptyBuffers);
  end;
end;

//==============================================================================
//
// TMidi.OnMidiOutOpen
//
//==============================================================================
procedure TMidi.OnMidiOutOpen;
begin
  DebugOutput('Midi device opened'#13#10, []);
end;

//==============================================================================
//
// TMidi.OnMidiOutPositionCB
//
//==============================================================================
procedure TMidi.OnMidiOutPositionCB(var rHdr: MIDIHDR; var rEvent: MIDIEVENT);
begin
  if MIDIEVENT_TYPE(rEvent.dwEvent) = MIDI_CTRLCHANGE then
    if MIDIEVENT_DATA1(rEvent.dwEvent) = MIDICTRL_VOLUME then
      // Mask off the channel number and cache the volume data byte
      m_Volumes[MIDIEVENT_CHANNEL(rEvent.dwEvent)] := DWORD(MIDIEVENT_VOLUME(rEvent.dwEvent) * 100 div VOLUME_MAX);
end;

//==============================================================================
//
// TMidi.Pause
//
//==============================================================================
function TMidi.Pause: BOOL;
begin
  if not m_bPaused and m_bPlaying and (m_pSoundData <> nil) and (m_hStream <> 0) then
  begin
    midiStreamPause(m_hStream);
    m_bPaused := True;
  end;
  Result := False;
end;

//==============================================================================
//
// TMidi.Play
//
//==============================================================================
function TMidi.Play(bInfinite: BOOL): BOOL;
var
  _mmResult: MMRESULT;
begin
  if IsPaused() then
  begin
    Resume();
    Result := True;
    Exit;
  end;

  // calling Play() while it is already playing will restart from scratch
  if IsPlaying() then
    Stop();

  // Clear the status of our callback so it will handle
  // MOM_DONE callbacks once more
  m_uCallbackStatus := 0;

  if not m_bLooped then
    m_bInsertTempo := True;
  _mmResult := midiStreamRestart(m_hStream);
  if _mmResult <> MMSYSERR_NOERROR then
  begin
    MidiError(_mmResult);
    Result := False;
    Exit;
  end;

  m_bPlaying := True;
  m_bLooped := bInfinite;

  Result := m_bPlaying;
end;

//==============================================================================
//
// TMidi.Rewind
//
//==============================================================================
function TMidi.Rewind: BOOL;
var
  i: integer;
begin
  if m_pSoundData = nil then
  begin
    Result := False;
    Exit;
  end;

  for i := 0 to m_dwTrackCount - 1 do
  begin
    m_Tracks[i].pTrackCurrent := m_Tracks[i].pTrackStart;
    m_Tracks[i].byRunningStatus := 0;
    m_Tracks[i].tkNextEventDue := 0;
    m_Tracks[i].fdwTrack := 0;

    // Handle bozo MIDI files which contain empty track chunks
    if m_Tracks[i].dwTrackLength = 0 then
    begin
      m_Tracks[i].fdwTrack := m_Tracks[i].fdwTrack or ITS_F_ENDOFTRK;
      continue;
    end;

    // We always preread the time from each track so the mixer code can
    // determine which track has the next event with a minimum of work
    if not GetTrackVDWord(@m_Tracks[i], @m_Tracks[i].tkNextEventDue) then
    begin
      AssertMidiError(False, 'TMidi.Rewind(): Error in MIDI data', []);
      Result := False;
      Exit;
    end;
  end;

  Result := True;
end;

//==============================================================================
//
// TMidi.SetChannelVolume
//
//==============================================================================
procedure TMidi.SetChannelVolume(dwChannel, dwPercent: DWORD);
var
  dwEvent: DWORD;
  mmrRetVal: MMRESULT;
begin
  AssertMidiError(dwChannel < DWORD(Length(m_Volumes)), 'TMidi.SetChannelVolume(): Invalid channel %d', [dwChannel]);

  if not m_bPlaying then
    Exit;

  if dwPercent > VOLUME_INIT then
    m_Volumes[dwChannel] := m_volume
  else
    m_Volumes[dwChannel] := round(dwPercent * m_volume / VOLUME_INIT);
  dwEvent := MIDI_CTRLCHANGE or dwChannel or (DWORD(MIDICTRL_VOLUME) shl 8) or (DWORD(m_Volumes[dwChannel] * VOLUME_MAX div 100) shl 16);
  mmrRetVal := midiOutShortMsg(HMIDIOUT(m_hStream), dwEvent);
  if (mmrRetVal <> MMSYSERR_NOERROR) then
  begin
    MidiError(mmrRetVal);
    Exit;
  end;
end;

//==============================================================================
//
// TMidi.SetInfinitePlay
//
//==============================================================================
procedure TMidi.SetInfinitePlay(bSet: BOOL);
begin
  m_bLooped := bSet;
end;

//==============================================================================
//
// TMidi.SetTempo
//
//==============================================================================
procedure TMidi.SetTempo(dwPercent: DWORD);
begin
  if dwPercent <> 0 then
    m_dwTempoMultiplier := dwPercent
  else
    m_dwTempoMultiplier := 1;
  m_bInsertTempo := True;
end;

//==============================================================================
//
// TMidi.SetVolume
//
//==============================================================================
procedure TMidi.SetVolume(dwpercent: DWORD);
var
  i: DWORD;
begin
  for i := Low(m_Volumes) to High(m_Volumes) do
    SetChannelVolume(i, dwPercent);
end;

//==============================================================================
//
// TMidi.Stop
//
//==============================================================================
function TMidi.Stop(bReOpen: BOOL): BOOL;
var
  mmrRetVal: MMRESULT;
begin
  if IsPlaying() or (m_uCallbackStatus <> STATUS_CALLBACKDEAD) then
  begin
    m_bPaused := False;
    m_bPlaying := m_bPaused;
    if (m_uCallbackStatus <> STATUS_CALLBACKDEAD) and (m_uCallbackStatus <> STATUS_WAITINGFOREND) then
      m_uCallbackStatus := STATUS_KILLCALLBACK;

    mmrRetVal := midiStreamStop(m_hStream);
    if mmrRetVal <> MMSYSERR_NOERROR then
    begin
      MidiError(mmrRetVal);
      Result := False;
      Exit;
    end;

    mmrRetVal := midiOutReset(HMIDIOUT(m_hStream));
    if mmrRetVal <> MMSYSERR_NOERROR then
    begin
      MidiError(mmrRetVal);
      Result := False;
      Exit;
    end;

    // Wait for the callback thread to release this thread, which it will do by
    // calling SetEvent() once all buffers are returned to it
    if WaitForSingleObject(m_hBufferReturnEvent, DEBUG_CALLBACK_TIMEOUT) = WAIT_TIMEOUT then
    begin
      // Note, this is a risky move because the callback may be genuinely busy, but
      // when we're debugging, it's safer and faster than freezing the application,
      // which leaves the MIDI device locked up and forces a system reset...
      DebugOutput('TMidi.Stop(): Timed out waiting for MIDI callback'#13#10, []);
      m_uCallbackStatus := STATUS_CALLBACKDEAD;
    end;
  end;

  if m_uCallbackStatus = STATUS_CALLBACKDEAD then
  begin
    m_uCallbackStatus := 0;
    FreeBuffers();
    if (m_hStream <> 0) then
    begin
      mmrRetVal := midiStreamClose(m_hStream);
      if mmrRetVal <> MMSYSERR_NOERROR then
        MidiError(mmrRetVal);
      m_hStream := 0;
    end;

    if bReOpen then
    begin
      if not StreamBufferSetup() then
      begin
        // Error setting up for MIDI file
        // Notification is already taken care of...
        Result := False;
        Exit;
      end;
      if not m_bLooped then
      begin
        Rewind();
        m_dwProgressBytes := 0;
        m_dwStatus := 0;
      end;
    end;
  end;
  Result := True;
end;

//==============================================================================
// TMidi.StreamBufferSetup
//
// StreamBufferSetup()
//
// Opens a MIDI stream. Then it goes about converting the data into a midiStream buffer for playback.
//
//==============================================================================
function TMidi.StreamBufferSetup: BOOL;
var
  nChkErr: integer;
  bFoundEnd: BOOL;
  mmrRetVal: MMRESULT;
  mptd: MIDIPROPTIMEDIV;
  dwConvertFlag: DWORD;
begin
  bFoundEnd := False;

  if m_hStream = 0 then
  begin
    mmrRetVal := midiStreamOpen(@m_hStream,
      @m_uMIDIDeviceID,
      DWORD(1),
      DWORD(@MidiProc),
      DWORD(Self),
      CALLBACK_FUNCTION);
    if mmrRetVal <> MMSYSERR_NOERROR then
    begin
      MidiError(mmrRetVal);
      Result := False;
      Exit;
    end;
  end;

  // allocate stream buffers and initialise them
  SetLength(m_StreamBuffers, NUM_STREAM_BUFFERS);

  mptd.cbStruct := SizeOf(mptd);
  mptd.dwTimeDiv := m_dwTimeDivision;
  mmrRetVal := midiStreamProperty(m_hStream, @mptd,
    MIDIPROP_SET or MIDIPROP_TIMEDIV);
  if mmrRetVal <> MMSYSERR_NOERROR then
  begin
    MidiError(mmrRetVal);
    Result := False;
    Exit;
  end;

  m_nEmptyBuffers := 0;
  dwConvertFlag := CONVERTF_RESET;

  m_nCurrentBuffer := 0;
  while m_nCurrentBuffer < NUM_STREAM_BUFFERS do
  begin
    m_StreamBuffers[m_nCurrentBuffer].mhBuffer.dwBufferLength := OUT_BUFFER_SIZE;
    GetMem(m_StreamBuffers[m_nCurrentBuffer].mhBuffer.lpData, OUT_BUFFER_SIZE);
    if (m_StreamBuffers[m_nCurrentBuffer].mhBuffer.lpData = nil) then
    begin
      Result := False;
      Exit;
    end;

    // Tell the converter to convert up to one entire buffer's length of output
    // data. Also, set a flag so it knows to reset any saved state variables it
    // may keep from call to call.
    m_StreamBuffers[m_nCurrentBuffer].dwStartOffset := 0;
    m_StreamBuffers[m_nCurrentBuffer].dwMaxLength := OUT_BUFFER_SIZE;
    m_StreamBuffers[m_nCurrentBuffer].tkStart := 0;
    m_StreamBuffers[m_nCurrentBuffer].bTimesUp := False;

    nChkErr := ConvertToBuffer(dwConvertFlag, @m_StreamBuffers[m_nCurrentBuffer]);
    if nChkErr <> CONVERTERR_NOERROR then
    begin
      if nChkErr = CONVERTERR_DONE then
      begin
        bFoundEnd := True;
      end
      else
      begin
        DebugOutput('TMidi.StreamBufferSetup(): Initial conversion pass failed'#13#10, []);
        Result := False;
        Exit;
      end;
    end;
    m_StreamBuffers[m_nCurrentBuffer].mhBuffer.dwBytesRecorded := m_StreamBuffers[m_nCurrentBuffer].dwBytesRecorded;

    if not m_bBuffersPrepared then
    begin
      mmrRetVal := midiOutPrepareHeader(HMIDIOUT(m_hStream),
        @m_StreamBuffers[m_nCurrentBuffer].mhBuffer,
        SizeOf(MIDIHDR));
      if (mmrRetVal <> MMSYSERR_NOERROR) then
      begin
        MidiError(mmrRetVal);
        Result := False;
        Exit;
      end;
    end;

    mmrRetVal := midiStreamOut(m_hStream,
      @m_StreamBuffers[m_nCurrentBuffer].mhBuffer,
      SizeOf(MIDIHDR));
    if mmrRetVal <> MMSYSERR_NOERROR then
    begin
      MidiError(mmrRetVal);
      break;
    end;
    dwConvertFlag := 0;

    if bFoundEnd then
      break;
    Inc(m_nCurrentBuffer);
  end;

  m_bBuffersPrepared := True;
  m_nCurrentBuffer := 0;
  Result := True;
end;

//==============================================================================
//
// TMidi.TrackError
//
//==============================================================================
procedure TMidi.TrackError(ptsTrack: LPTRACK; const lpszErr: string);
begin
  DebugOutput('Track buffer offset %d', [DWORD(ptsTrack^.pTrackCurrent) - DWORD(ptsTrack^.pTrackStart)]);
  DebugOutput('Track total length %d', [ptsTrack^.dwTrackLength]);
  DebugOutput('%s', [lpszErr]);
end;

//==============================================================================
//
// TMidi.DebugOutput
//
//==============================================================================
procedure TMidi.DebugOutput(const fmt: string; const A: array of const);
begin
  if devparm then
    if debugfile <> nil then
      fprintf(debugfile, fmt, A);
end;

//==============================================================================
//
// TMidi.SetGlobalVolume
//
//==============================================================================
procedure TMidi.SetGlobalVolume(const v: integer);
begin
  m_volume := GetIntegerInRange(v, 0, VOLUME_INIT);
  SetVolume(VOLUME_INIT);
end;

var
  midi: TMidi;
  MidiData: PByteArray;
  MidiDataSize: integer;

//==============================================================================
//
// I_PlayMidi
//
//==============================================================================
procedure I_PlayMidi_New(const aMidiFile: string);
var
  f: TFile;
begin
  if midi <> nil then
    FreeAndNil(midi);

  memfree(pointer(MidiData), MidiDataSize);

  midi := TMidi.Create();

  f := TFile.Create(aMidiFile, fOpenReadOnly);
  MidiDataSize := f.Size;
  MidiData := malloc(MidiDataSize);
  f.Read(MidiData^, MidiDataSize);
  f.Free;

  midi.LoadData(MidiData, MidiDataSize);
  midi.Play(True);
end;

//==============================================================================
//
// I_StopMidi
//
//==============================================================================
procedure I_StopMidi_New;
begin
  if midi <> nil then
  begin
    midi.Stop;
    FreeAndNil(midi);
  end;
end;

//==============================================================================
//
// I_IsMidiPlaying_New
//
//==============================================================================
function I_IsMidiPlaying_New: boolean;
begin
  if midi = nil then
    result := false
  else
    result := midi.IsPlaying;
end;

//==============================================================================
//
// I_PauseMidi_New
//
//==============================================================================
procedure I_PauseMidi_New;
begin
  if midi <> nil then
    midi.Pause;
end;

//==============================================================================
//
// I_ResumeMidi
//
//==============================================================================
procedure I_ResumeMidi_New;
begin
  if midi <> nil then
    midi.Resume;
end;

//==============================================================================
//
// I_InitMidi
//
//==============================================================================
procedure I_InitMidi_New;
begin
  midi := nil;
  MidiData := nil;
  MidiDataSize := 0;
end;

//==============================================================================
//
// I_ShutDownMidi_New
//
//==============================================================================
procedure I_ShutDownMidi_New;
begin
  FreeAndNil(midi);
  memfree(pointer(MidiData), MidiDataSize);
end;

var
  ws_volume: integer;

//==============================================================================
//
// I_SetMusicVolumeMidi_New
//
//==============================================================================
procedure I_SetMusicVolumeMidi_New(volume: integer);
begin
  if midi <> nil then
  begin
    ws_volume := (midivolumecontrol[GetIntegerInRange(volume, 0, 15)]);
    midi.volume := ws_volume;
  end;
end;

//==============================================================================
//
// I_ProcessMidi_New
//
//==============================================================================
procedure I_ProcessMidi_New;
begin
  if midi <> nil then
    midi.volume := ws_volume;
end;

end.

