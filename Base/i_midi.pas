//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2013 by Jim Valavanis
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
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

{$WARN SYMBOL_DEPRECATED OFF}

unit i_midi;

interface

procedure I_PlayMidi(index: integer);

procedure I_StopMidi;

procedure AddMidiFileToPlayList(const MidiFile: string);

procedure DeleteMidiFileFromPlayList(const MidiFile: string);

procedure ClearMidiFilePlayList;

function I_IsMidiPlaying: boolean;

procedure I_ResumeMidi;

procedure I_PauseMidi;

procedure I_InitMidi;

procedure I_ShutDownMidi;

function _mciGetErrorString(const code: LongWord): string;

implementation

uses
  Windows,
  d_delphi,
  Messages,
  MMSystem;

var
  wDeviceID: DWORD;
  MidiFileNames: TDStringList;
  MidiID: integer;
  Window: HWnd;
  fIsMidiPlaying: boolean;
  WindowClass: TWndClass;
  currentmidi: string;

const
  rsAppName = 'MIDIPLAYERWNDNOTIFY';
  rsSequencer = 'sequencer';
  rsWndTitle = 'Notify Window';
  rsErrNoMIDIMapper = 'MIDI mapper unavailable';
  rsErrOutOfRange = 'I_PlayMidi(index): index out of playlist range.';

// Plays a specified MIDI file by using MCI_OPEN and MCI_PLAY. Returns
// as soon as playback begins. The window procedure function for the
// specified window will be notified when playback is complete.
// Returns 0L on success; otherwise, it returns an MCI error code.
function playMIDIFile(hWndNotify: HWnd; lpszMIDIFileName: string; doCheckMidiMapper: boolean = false): DWORD;
var
  mciOpenParms: MCI_OPEN_PARMS;
  mciPlayParms: MCI_PLAY_PARMS;
  mciStatusParms: MCI_STATUS_PARMS;
begin
  // Open the device by specifying the device and filename.
  // MCI will attempt to choose the MIDI mapper as the output port.
  ZeroMemory(@mciOpenParms, SizeOf(MCI_OPEN_PARMS));
  mciOpenParms.lpstrDeviceType := PAnsiChar(rssequencer);
  mciOpenParms.lpstrElementName := PChar(lpszMIDIFileName);
  result := mciSendCommand(0, MCI_OPEN, MCI_OPEN_TYPE or MCI_OPEN_ELEMENT, DWORD(@mciOpenParms));
  // Failed to open device. Don't close it; just return error.
  if result <> 0 then exit;

  // The device opened successfully; get the device ID.
  wDeviceID := mciOpenParms.wDeviceID;

  if doCheckMidiMapper then
  begin
    // Check if the output port is the MIDI mapper.
    mciStatusParms.dwItem := MCI_SEQ_STATUS_PORT;
    result := mciSendCommand(wDeviceID, MCI_STATUS, MCI_STATUS_ITEM, DWORD(@mciStatusParms));
    if result <> 0 then
    begin
      mciSendCommand(wDeviceID, MCI_CLOSE, 0, 0);
      exit;
    end
    else if LOWORD(mciStatusParms.dwReturn) <> WORD(MIDI_MAPPER) then
    // The output port is not the MIDI mapper.
    begin
      printf(rsErrNoMIDIMapper);
      exit;
    end;
  end;

  // Begin playback. The window procedure function for the parent
  // window will be notified with an MM_MCINOTIFY message when
  // playback is complete. At this time, the window procedure closes
  // the device.
  ZeroMemory(@mciPlayParms, SizeOf(MCI_PLAY_PARMS));
  mciPlayParms.dwCallback := DWORD(hWndNotify);
  result := mciSendCommand(wDeviceID, MCI_PLAY, MCI_NOTIFY, DWORD(@mciPlayParms));
  if result > 0 then
  begin
    mciSendCommand(wDeviceID, MCI_CLOSE, 0, 0);
    exit;
  end;

  currentmidi := strupper(lpszMIDIFileName);
end;

function restartMIDIFile(hWndNotify: HWnd): DWORD;
var
  mciPlayParms: MCI_PLAY_PARMS;
  mciSeekParms: MCI_SEEK_PARMS;
begin
  ZeroMemory(@mciSeekParms, SizeOf(MCI_SEEK_PARMS));
  mciSeekParms.dwCallback := DWORD(hWndNotify);
  result := mciSendCommand(wDeviceID, MCI_SEEK, MCI_NOTIFY or MCI_SEEK_TO_START, DWORD(@mciSeekParms));
  if result > 0 then
  begin
    mciSendCommand(wDeviceID, MCI_CLOSE, 0, 0);
    exit;
  end;

  ZeroMemory(@mciPlayParms, SizeOf(MCI_PLAY_PARMS));
  mciPlayParms.dwCallback := DWORD(hWndNotify);
  result := mciSendCommand(wDeviceID, MCI_PLAY, MCI_NOTIFY, DWORD(@mciPlayParms));
  if result > 0 then
    mciSendCommand(wDeviceID, MCI_CLOSE, 0, 0);
end;

procedure StopPlayingMidi;
begin
  mciSendCommand(wDeviceID, MCI_STOP, 0, 0);
  mciSendCommand(wDeviceID, MCI_CLOSE, 0, 0);
  currentmidi := '';
end;

function WindowProc(hWnd: HWND; Msg: UINT; wParam: WPARAM;
  lParam: LPARAM): LRESULT; stdcall; export;
var
  newmidi: string;
begin
  case Msg of
    MM_MCINOTIFY:
      if (wParam = MCI_NOTIFY_SUCCESSFUL) then
      begin
        if MidiID >= MidiFileNames.Count - 1 then
          MidiID := 0
        else
          inc(MidiID);
        newmidi := strupper(MidiFileNames[MidiID]);
        if newmidi = currentmidi then
          restartMIDIFile(Window)
        else
        begin
          StopPlayingMidi;
          playMIDIFile(Window, newmidi);
        end;
      end;
    WM_CLOSE:
      begin
        DestroyWindow(hWnd);
        Window := 0;
        result := 0;
        exit;
      end;
  end;
  result := DefWindowProc(hWnd, Msg, WParam, LParam);
end;

procedure AddMidiFileToPlayList(const MidiFile: string);
begin
  if MidiFileNames = nil then
    MidiFileNames := TDStringList.Create;
  MidiFileNames.Add(MidiFile);
end;

procedure DeleteMidiFileFromPlayList(const MidiFile: string);
var
  i: integer;
begin
  if MidiFileNames <> nil then
  begin
    if MidiFileNames.IndexOf(MidiFile) > -1 then
      MidiFileNames.Delete(MidiFileNames.IndexOf(MidiFile))
    else
    begin
      for i := 0 to MidiFileNames.Count - 1 do
        if strupper(MidiFileNames[i]) = strupper(MidiFile) then
        begin
          MidiFileNames.Delete(i);
          exit;
        end;
    end;
  end;
end;

procedure ClearMidiFilePlayList;
begin
  if MidiFileNames <> nil then
    MidiFileNames.Clear;
end;

procedure I_PlayMidi(index: integer);
begin
  I_StopMidi;
  ZeroMemory(@WindowClass, SizeOf(WindowClass));
  if Window = 0 then
  begin
    WindowClass.style := CS_DBLCLKS;
    WindowClass.lpfnWndProc := @WindowProc;
    WindowClass.lpszClassName := PChar(rsAppName);
    if HPrevInst = 0 then
    begin
      WindowClass.hInstance := HInstance;
      WindowClass.hCursor := LoadCursor(0, idc_Arrow);
      RegisterClass(WindowClass);
    end;
    Window := CreateWindowEx(
      0,
      WindowClass.lpszClassName,
      PChar(rsWndTitle),
      ws_OverlappedWindow,
      integer(CW_USEDEFAULT),
      integer(CW_USEDEFAULT),
      integer(CW_USEDEFAULT),
      integer(CW_USEDEFAULT),
      0,
      0,
      HInstance,
      nil);
    ShowWindow(Window, SW_HIDE);
  end;

  if (index < MidiFileNames.Count) and (index > -1) then
  begin
    MidiID := index;
    playMIDIFile(Window, MidiFileNames[MidiID], true);
    fIsMidiPlaying := true;
  end
  else
    printf(rsErrNoMIDIMapper);
end;

procedure I_StopMidi;
begin
  if Window <> 0 then
  begin
    StopPlayingMidi;
    SendMessage(Window, WM_CLOSE, 0, 0);
    Window := 0;
    fIsMidiPlaying := false;
  end;
end;

function I_IsMidiPlaying: boolean;
begin
  result := fIsMidiPlaying;
end;

procedure I_ResumeMidi;
begin
  mciSendCommand(wDeviceID, MCI_RESUME, 0, 0);
end;

procedure I_PauseMidi;
begin
  mciSendCommand(wDeviceID, MCI_PAUSE, 0, 0);
end;

function _mciGetErrorString(const code: LongWord): string;
var
  buf: array[0..127] of char;
  i:  integer;
begin
  result := '';
  ZeroMemory(@buf, SizeOf(buf));
  if mciGetErrorString(code, buf, SizeOf(buf)) then
    for i := 0 to SizeOf(buf) - 1 do
    begin
      if buf[i] = #0 then
        break;
      result := result + buf[i];
    end;
end;

procedure I_InitMidi;
begin
  Window := 0;
  MidiFileNames := TDStringList.Create;
  MidiID := 0;
  fIsMidiPlaying := false;
end;

procedure I_ShutDownMidi;
begin
  StopPlayingMidi;
  MidiFileNames.Free;
end;

end.
