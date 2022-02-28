unit xmi_core;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, Grids, Math, Menus, Buttons,  MMSystem, ValEdit,
  xmi_consts, xmi_iff;

const
  WM_EVENTIDX = WM_USER + 110;
  WM_TRACKIDX = WM_USER + 111;
  WM_SETVU = WM_USER + 112;
  WM_PLAYCTRL = WM_USER + 113;
  WM_EVENTREF = WM_USER + 114;
  WM_EVENTREQ = WM_USER + 120;

  PLAY_DEF = 0;
  PLAY_POS = 1;
  PLAY_STEP = 2;
  PLAY_REC = 3;
  PLAYER_PLAY = 1;
  PLAYER_STOP = 2;
  PLAYER_REC = 3;

type
  FILE_VERSION = record
    Version: record
      case boolean of
        True: (dw: DWORD);
        False: (w: record
            Minor, Major: word;
          end;)
    end;
    Release, Build: word;
    bDebug, bPrerelease, bPrivate, bSpecial: boolean;
  end;

  Command = record
    Ticks: UInt64;
    Status: byte;
    BParm1, BParm2: byte;
    Value: int64;
    Len: UInt64;
    DataArray: array of byte;
    DataString: ansistring;
    RunStatMode: boolean;
  end;

  Chunk = record
    Title: ansistring;
    Data: array of Command;
  end;

  TSearchEvent = record
    dtOp: byte;
    dt: UInt64;
    chan: byte;
    evnt: byte;
    v1Op: byte;
    v1: word;
    v2Op: byte;
    v2: word;
    Text: string;
  end;

  TPlaySet = record
    Mode: byte;
    TrackIdx,
    EventIdx: integer;
  end;
  PPlaySet = ^TPlaySet;

  TStepEvent = record
    Send: boolean;
    E: Command;
    TrackIdx,
    EventIdx: integer;
  end;
  PStepEvent = ^TStepEvent;

type

  { TXMIPlayForm }

  TXMICore = class(TForm)
    panTop: TPanel;
    Log: TMemo;
    panMiddle: TPanel;
    TrkCh: TComboBox;
    Label1: TLabel;
    Events: TStringGrid;
    bPlay: TBitBtn;
    bStop: TBitBtn;
    bPlayPos: TBitBtn;
    bStep: TBitBtn;
    bRecord: TBitBtn;
    panVert1: TPanel;
    panVert2: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure TrkChChange(Sender: TObject);
    procedure EventsClick(Sender: TObject);
    procedure EventsKeyPress(Sender: TObject; var Key: char);
    procedure FormResize(Sender: TObject);
    procedure MOptimizeClick(Sender: TObject);
    procedure MFindNextClick(Sender: TObject);
    procedure Changechannel1Click(Sender: TObject);
    procedure MCalcLenClick(Sender: TObject);
    procedure MFormatMIDClick(Sender: TObject);
    procedure MFormatMDIClick(Sender: TObject);
    procedure MFormatCMFClick(Sender: TObject);
    procedure MFormatROLClick(Sender: TObject);
    procedure MFormatMUSClick(Sender: TObject);
    procedure MFormatIMSClick(Sender: TObject);
    procedure bPlayClick(Sender: TObject);
    procedure bStopClick(Sender: TObject);
    procedure bPlayPosClick(Sender: TObject);
    procedure bStepClick(Sender: TObject);
    procedure bRecordClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MFormatXMIClick(Sender: TObject);
    procedure EventsTopLeftChanged(Sender: TObject);
    procedure MFormatHERADClick(Sender: TObject);
    procedure MDelTracksClick(Sender: TObject);
    procedure MFormatSOPClick(Sender: TObject);
  private
    { Private declarations }
    procedure OnEventChange(var Msg: TMessage); message WM_EVENTIDX;
    procedure OnTrackChange(var Msg: TMessage); message WM_TRACKIDX;
    procedure OnVUChange(var Msg: TMessage); message WM_SETVU;
    procedure OnPlayerChange(var Msg: TMessage); message WM_PLAYCTRL;
    procedure OnPlayerRefresh(var Msg: TMessage); message WM_EVENTREF;
    procedure OnEventRequest(var Msg: TMessage); message WM_EVENTREQ;
  public
    { Public declarations }
    function LoadFile(FileName, Fmt: string): boolean;
    function LoadMemory(const adata: Pointer; const asize: integer; Fmt: string): boolean;
    function SaveFile(FileName: string): boolean;
    function SaveFileToStream(FileName: string; M: TMemoryStream): boolean;
    function SaveFileToPointer(FileName: string; var p: pointer; var sz: integer): boolean;
    function DetectFile(var F: TMemoryStream): string;
    function DetectMIDI(var F: TMemoryStream): boolean;
    function DetectRMI(var F: TMemoryStream): boolean;
    function DetectMIDS(var F: TMemoryStream): boolean;
    function DetectXMI(var F: TMemoryStream): boolean;
    function DetectXMIMemory(const adata: Pointer; const asize: integer): boolean;
    function DetectCMF(var F: TMemoryStream): boolean;
    function DetectCMFMemory(const adata: Pointer; const asize: integer): boolean;
    function DetectROL(var F: TMemoryStream): boolean;
    function DetectROLMemory(const adata: Pointer; const asize: integer): boolean;
    function DetectMUS(var F: TMemoryStream): boolean;
    function DetectMUSMemory(const adata: Pointer; const asize: integer): boolean;
    function DetectSOP(var F: TMemoryStream): boolean;
    function DetectSOPMemory(const adata: Pointer; const asize: integer): boolean;
    function ReadMIDI(var F: TMemoryStream): boolean;
    function ReadRMI(var F: TMemoryStream): boolean;
    function ReadMIDS(var F: TMemoryStream): boolean;
    function ReadXMI(var F: TMemoryStream): boolean;
    function ReadCMF(var F: TMemoryStream): boolean;
    function ReadROL(var F: TMemoryStream; FileName: string): boolean;
    function ReadMUS(var F: TMemoryStream; FileName: string): boolean;
    function ReadSOP(var F: TMemoryStream): boolean;
    function ReadHERAD(var F: TMemoryStream): boolean;
    function ReadRaw(var F: TMemoryStream): boolean;
    function ReadSYX(var F: TMemoryStream): boolean;
    procedure ReadTrackData(var F: TMemoryStream; var Trk: Chunk);
    procedure ReadTrackData_MIDS(var F: TMemoryStream; var Trk: Chunk);
    procedure ReadTrackData_XMI(var F: TMemoryStream; var Trk: Chunk);
    procedure ReadTrackData_MUS(var F: TMemoryStream; var Trk: Chunk);
    procedure ReadTrackData_SOP(var F: TMemoryStream; var Trk: Chunk);
    procedure ReadTrackData_HERAD(var F: TMemoryStream; var Trk: Chunk);
    procedure ReadTrackData_SYX(var F: TMemoryStream; var Trk: Chunk);
    procedure WriteMIDI(var F: TMemoryStream);
    procedure WriteRMI(var F: TMemoryStream);
    procedure WriteXMI(var F: TMemoryStream);
    procedure WriteCMF(var F: TMemoryStream);
    procedure WriteROL(var F: TMemoryStream; FileName: string);
    procedure WriteMUS(var F: TMemoryStream; FileName: string);
    procedure WriteSOP(var F: TMemoryStream);
    procedure WriteHERAD(var F: TMemoryStream);
    procedure WriteRaw(var F: TMemoryStream);
    procedure WriteSYX(var F: TMemoryStream);
    procedure WriteTrackData(var F: TMemoryStream; var Trk: Chunk);
    procedure WriteTrackData_XMI(var F: TMemoryStream; var Trk: Chunk);
    procedure WriteTrackData_MUS(var F: TMemoryStream; var Trk: Chunk);
    procedure WriteTrackData_SOP(var F: TMemoryStream; var Trk: Chunk);
    procedure WriteTrackData_HERAD(var F: TMemoryStream; var Trk: Chunk);
    procedure WriteTrackData_SYX(var F: TMemoryStream; var Trk: Chunk);
    procedure ConvertEvents(DestProfile: ansistring);
    procedure Convert_MID_FixTempo;
    procedure Convert_XMI_MID;
    procedure Convert_MID_XMI;
    procedure Convert_MUS_MID;
    procedure Convert_MUS_MDI;
    procedure Convert_MDI_MID;
    procedure Convert_MID_MUS;
    procedure Convert_MDI_MUS;
    procedure Convert_CMF_MID;
    procedure Convert_CMF_MDI;
    procedure Convert_MDI_CMF;
    procedure Convert_ROL_MID;
    procedure Convert_MUS_ROL;
    procedure Convert_MDI_ROL;
    procedure Convert_IMS_MID;
    procedure Convert_IMS_ROL;
    procedure Convert_SOP_MID;
    procedure Convert_SOP_MUS;
    procedure Convert_HERAD_MID;
    procedure Convert_HERAD_HERAD_V2;
    procedure ConvertTicks(RelToAbs: boolean; var Data: array of Command);
    function MergeTracksUsingTicks(Tracks: array of integer; EOT: boolean): Chunk;
    procedure MergeTracksByTicks(Trk: array of integer);
    procedure MergeTracksByOrder(Trk: array of integer);
    procedure RefTrackList;
    procedure ChkButtons;
    function GetInstType(Chn: byte): string;
    function GetInstName(Prog: byte; Chn: byte): string;
    procedure FillEvents(Idx: integer);
    procedure LogSongInfo;
    procedure AddTrack;
    procedure DelTrack(Idx: integer);
    procedure NewEvent(Trk, Idx: integer; E, Parm: byte);
    procedure AddEvent(E, Parm: byte);
    function DelEvent(Trk, Idx: integer; TimeSave: boolean): boolean;
    function DelRange(Trk, From, Count: integer; TimeSave: boolean): boolean;
    function TimeBetween(Trk, Idx1, Idx2: integer): UInt64;
    procedure CopyEvent(SrcTrk, SrcIdx, DestTrk, DestIdx: integer);
    procedure MMSysError(Err: DWord);
    procedure LogOutput(const s: string);
  end;

var
  XMICore: TXMICore;
  VisRows: integer = 0;
  Opened: boolean = False;
  Container, EventFormat, EventProfile, EventViewProfile: ansistring;
  // Container (smf, rmi, xmi, cmf, mus, ims, raw, syx, mids)
  // EventFormat (mid, xmi, mus)
  // EventProfile (mid, mdi, xmi, cmf, mus, ims)
  // EventViewProfile = EventProfile
  SongData: TValueListEditor;
  TrackData: array of Chunk;
  // Search
  SearchEv: TSearchEvent;
  // Player
  MIDIDev: DWORD = MIDI_MAPPER;
  MIDIOut: HMIDIOUT;
  MIDIThr: THandle;
  MIDIThrId: cardinal;
  PlayerMode: byte;
  LoopEnabled: boolean = True;
  gStepEvent: TStepEvent;
  // Recorder
  MIDIIDev: DWORD = MIDI_MAPPER;
  MIDIIn: HMIDIIN;
  MidInTrk: integer;
  MidInEcho: boolean = True;
  UpdTicks: cardinal = 0;
  MinUpdDelay: cardinal = 100;
  RecDelayCoef: double;
  RecCountStart: int64;
  RecLastTick: UInt64;
  // Visual
  vFS: TFormatSettings;
  vEvntIndex: integer;
  vChangeTrack: boolean = False;
  vTrkIndex: integer;
  VU: array[0..15] of byte;

implementation

{$R *.dfm}

//==============================================================================
//
// SongData_PutInt
//
//==============================================================================
procedure SongData_PutInt(Name: string; Val: integer);
var
  Row: integer;
begin
  if SongData.FindRow(Name, Row) then
  begin
    SongData.Cells[1, Row] := IntToStr(Val);
    Exit;
  end;
  SongData.InsertRow(Name, IntToStr(Val), True);
end;

//==============================================================================
//
// SongData_PutDWord
//
//==============================================================================
procedure SongData_PutDWord(Name: string; Val: cardinal);
var
  Row: integer;
begin
  if SongData.FindRow(Name, Row) then
  begin
    SongData.Cells[1, Row] := IntToStr(Val);
    Exit;
  end;
  SongData.InsertRow(Name, IntToStr(Val), True);
end;

//==============================================================================
//
// SongData_PutFloat
//
//==============================================================================
procedure SongData_PutFloat(Name: string; Val: single);
var
  Row: integer;
begin
  if SongData.FindRow(Name, Row) then
  begin
    SongData.Cells[1, Row] := FloatToStr(Val);
    Exit;
  end;
  SongData.InsertRow(Name, FloatToStr(Val), True);
end;

//==============================================================================
//
// SongData_PutStr
//
//==============================================================================
procedure SongData_PutStr(Name, Val: string);
var
  Row: integer;
begin
  if SongData.FindRow(Name, Row) then
  begin
    SongData.Cells[1, Row] := Val;
    Exit;
  end;
  SongData.InsertRow(Name, Val, True);
end;

//==============================================================================
//
// SongData_PutArray
//
//==============================================================================
procedure SongData_PutArray(Name: string; A: array of byte); overload;
var
  I, Row: integer;
  S: string;
begin
  S := '';
  for I := 0 to Length(A) - 1 do
    S := S + IntToStr(A[I]) + ' ';
  if SongData.FindRow(Name, Row) then
  begin
    SongData.Cells[1, Row] := S;
    Exit;
  end;
  SongData.InsertRow(Name, S, True);
end;

//==============================================================================
//
// SongData_PutArray
//
//==============================================================================
procedure SongData_PutArray(Name: string; A: array of word); overload;
var
  I, Row: integer;
  S: string;
begin
  S := '';
  for I := 0 to Length(A) - 1 do
    S := S + IntToStr(A[I]) + ' ';
  if SongData.FindRow(Name, Row) then
  begin
    SongData.Cells[1, Row] := S;
    Exit;
  end;
  SongData.InsertRow(Name, S, True);
end;

//==============================================================================
//
// SongData_GetByte
//
//==============================================================================
function SongData_GetByte(Name: string; var B: byte): boolean;
var
  Row, Code: integer;
begin
  Result := False;
  if not SongData.FindRow(Name, Row) then
    Exit;
  Val(SongData.Cells[1, Row], B, Code);
  Result := Code = 0;
end;

//==============================================================================
//
// SongData_GetSInt
//
//==============================================================================
function SongData_GetSInt(Name: string; var I: shortint): boolean;
var
  Row, Code: integer;
begin
  Result := False;
  if not SongData.FindRow(Name, Row) then
    Exit;
  Val(SongData.Cells[1, Row], I, Code);
  Result := Code = 0;
end;

//==============================================================================
//
// SongData_GetInt
//
//==============================================================================
function SongData_GetInt(Name: string; var I: integer): boolean;
var
  Row, Code: integer;
begin
  Result := False;
  if not SongData.FindRow(Name, Row) then
    Exit;
  Val(SongData.Cells[1, Row], I, Code);
  Result := Code = 0;
end;

//==============================================================================
//
// SongData_GetWord
//
//==============================================================================
function SongData_GetWord(Name: string; var W: word): boolean;
var
  Row, Code: integer;
begin
  Result := False;
  if not SongData.FindRow(Name, Row) then
    Exit;
  Val(SongData.Cells[1, Row], W, Code);
  Result := Code = 0;
end;

//==============================================================================
//
// SongData_GetDWord
//
//==============================================================================
function SongData_GetDWord(Name: string; var DW: cardinal): boolean;
var
  Row, Code: integer;
begin
  Result := False;
  if not SongData.FindRow(Name, Row) then
    Exit;
  Val(SongData.Cells[1, Row], DW, Code);
  Result := Code = 0;
end;

//==============================================================================
//
// SongData_GetFloat
//
//==============================================================================
function SongData_GetFloat(Name: string; var Fl: single): boolean;
var
  Row: integer;
begin
  Result := False;
  if not SongData.FindRow(Name, Row) then
    Exit;
  try
    Fl := StrToFloat(SongData.Cells[1, Row]);
    Result := True;
  except

  end;
end;

//==============================================================================
//
// SongData_GetStr
//
//==============================================================================
function SongData_GetStr(Name: string; var S: string): boolean;
var
  Row: integer;
begin
  Result := False;
  if not SongData.FindRow(Name, Row) then
    Exit;
  S := SongData.Cells[1, Row];
  Result := True;
end;

//==============================================================================
//
// SongData_GetArray
//
//==============================================================================
function SongData_GetArray(Name: string; var A: array of byte): boolean; overload;
var
  Row, I: integer;
  S: string;
  SL: TStringList;
begin
  Result := False;
  if not SongData.FindRow(Name, Row) then
    Exit;
  S := SongData.Cells[1, Row];
  SL := TStringList.Create;
  SL.Delimiter := ' ';
  SL.DelimitedText := S;
  for I := 0 to SL.Count - 1 do
  begin
    if I >= Length(A) then
      Break;
    try
      A[I] := StrToInt(SL[I]) and $FF;
    except
      A[I] := 0;
    end;
  end;
  SL.Free;
  Result := True;
end;

//==============================================================================
//
// SongData_GetArray
//
//==============================================================================
function SongData_GetArray(Name: string; var A: array of word): boolean; overload;
var
  Row, I: integer;
  S: string;
  SL: TStringList;
begin
  Result := False;
  if not SongData.FindRow(Name, Row) then
    Exit;
  S := SongData.Cells[1, Row];
  SL := TStringList.Create;
  SL.Delimiter := ' ';
  SL.DelimitedText := S;
  for I := 0 to SL.Count - 1 do
  begin
    if I >= Length(A) then
      Break;
    try
      A[I] := StrToInt(SL[I]) and $FFFF;
    except
      A[I] := 0;
    end;
  end;
  SL.Free;
  Result := True;
end;

//==============================================================================
//
// SongData_Delete
//
//==============================================================================
function SongData_Delete(Name: string): boolean;
var
  Row: integer;
begin
  Result := False;
  if not SongData.FindRow(Name, Row) then
    Exit;
  SongData.Strings.Delete(Row - 1);
  Result := True;
end;

//==============================================================================
//
// GetFiles
//
//==============================================================================
function GetFiles(const StartDir, Mask: string; const List: TStrings): boolean;
var
  SRec: TSearchRec;
  Res: integer;
begin
  if not Assigned(List) then
  begin
    Result := False;
    Exit;
  end;
  Res := FindFirst(StartDir + Mask, faAnyfile, SRec);
  if Res = 0 then
    try
      while res = 0 do
      begin
        if (SRec.Attr and faDirectory <> faDirectory) then
          List.Add(StartDir + SRec.Name);
        Res := FindNext(SRec);
      end;
    finally
      FindClose(SRec)
    end;
  Result := (List.Count > 0);
end;

//==============================================================================
//
// ReadVarVal
//
//==============================================================================
function ReadVarVal(var F: TMemoryStream; var Error: byte): UInt64;
var
  B, I: byte;
begin
  Result := 0;
  Error := 0;
  if F.Position >= F.Size then
  begin
    Error := 2;
    Exit;
  end
  else
    F.ReadBuffer(B, 1);
  I := 7;
  Result := B and 127;
  while (B shr 7) = 1 do
  begin
    if F.Position >= F.Size then
    begin
      Error := 2;
      Exit;
    end
    else
      F.ReadBuffer(B, 1);
    if I + 7 <= 64 then
      Inc(I, 7)
    else
    begin
      Error := 1;
      Break;
    end;
    Result := (Result shl 7) or (B and 127);
  end;
end;

//==============================================================================
//
// ReadVarVal_XMI
//
//==============================================================================
function ReadVarVal_XMI(var F: TMemoryStream; var Error: byte): UInt64;
var
  B: byte;
begin
  Result := 0;
  Error := 0;
  if F.Position >= F.Size then
  begin
    Error := 2;
    Exit;
  end
  else
    F.ReadBuffer(B, 1);
  while B = $7F do
  begin
    Result := Result + B;
    if F.Position >= F.Size then
    begin
      Error := 2;
      Exit;
    end
    else
      F.ReadBuffer(B, 1);
  end;
  if B >= $80 then
    F.Seek(-1, soCurrent)
  else
    Result := Result + B;
end;

//==============================================================================
//
// ReadVarVal_MUS
//
//==============================================================================
function ReadVarVal_MUS(var F: TMemoryStream; var Error: byte): UInt64;
var
  B: byte;
begin
  Result := 0;
  Error := 0;
  if F.Position >= F.Size then
  begin
    Error := 2;
    Exit;
  end
  else
    F.ReadBuffer(B, 1);
  while B = $F8 do
  begin
    Result := Result + $F0;
    if F.Position >= F.Size then
    begin
      Error := 2;
      Exit;
    end
    else
      F.ReadBuffer(B, 1);
  end;
  Result := Result + B;
end;

//==============================================================================
//
// WriteVarVal
//
//==============================================================================
procedure WriteVarVal(var F: TMemoryStream; Val: UInt64);
var
  I: integer;
  A: array of byte;
begin
  while True do
  begin
    SetLength(A, Length(A) + 1);
    A[Length(A) - 1] := Val and 127;
    Val := Val shr 7;
    if Val = 0 then
      Break;
  end;
  for I := Length(A) - 1 downto 0 do
  begin
    if I > 0 then
      A[I] := A[I] or 128;
    F.WriteBuffer(A[I], 1);
  end;
end;

//==============================================================================
//
// WriteVarVal_XMI
//
//==============================================================================
procedure WriteVarVal_XMI(var F: TMemoryStream; Val: UInt64);
var
  B: byte;
begin
  B := $7F;
  while Val > B do
  begin
    F.WriteBuffer(B, 1);
    Val := Val - B;
  end;
  if Val > 0 then
    F.WriteBuffer(Val, 1);
end;

//==============================================================================
//
// WriteVarVal_MUS
//
//==============================================================================
procedure WriteVarVal_MUS(var F: TMemoryStream; Val: UInt64);
var
  B: byte;
begin
  B := $F8;
  while Val >= $F0 do
  begin
    Val := Val - $F0;
    F.WriteBuffer(B, 1);
  end;
  B := Val;
  F.WriteBuffer(B, 1);
end;

//==============================================================================
//
// TXMICore.DetectMIDI
//
//==============================================================================
function TXMICore.DetectMIDI(var F: TMemoryStream): boolean;
var
  Head: array[0..3] of AnsiChar;
begin
  Result := False;
  if F.Size < 14 then
    Exit;
  F.Seek(0, soFromBeginning);
  F.ReadBuffer(Head, 4);
  Result := (Head = 'MThd') or (Head = 'MTrk');
end;

//==============================================================================
//
// TXMICore.DetectRMI
//
//==============================================================================
function TXMICore.DetectRMI(var F: TMemoryStream): boolean;
var
  Nodes: IFFNodes;
begin
  IFFGetNodes('RIFF', F.Memory, 0, F.Size, Nodes);
  Result := IFFSearchNode(Nodes, 'RIFF', 'RMID') > 0;
end;

//==============================================================================
//
// TXMICore.DetectMIDS
//
//==============================================================================
function TXMICore.DetectMIDS(var F: TMemoryStream): boolean;
var
  Nodes: IFFNodes;
begin
  IFFGetNodes('RIFF', F.Memory, 0, F.Size, Nodes);
  Result := IFFSearchNode(Nodes, 'RIFF', 'MIDS') > 0;
end;

//==============================================================================
//
// TXMICore.DetectXMI
//
//==============================================================================
function TXMICore.DetectXMI(var F: TMemoryStream): boolean;
var
  Nodes: IFFNodes;
begin
  IFFGetNodes('IFF', F.Memory, 0, F.Size, Nodes);
  Result := (IFFSearchNode(Nodes, 'CAT ', 'XMID') > 0) or
    (IFFSearchNode(Nodes, 'FORM', 'XMID') > 0);
end;

//==============================================================================
//
// TXMICore.DetectXMIMemory
//
//==============================================================================
function TXMICore.DetectXMIMemory(const adata: Pointer; const asize: integer): boolean;
var
  Nodes: IFFNodes;
begin
  IFFGetNodes('IFF', adata, 0, asize, Nodes);
  Result := (IFFSearchNode(Nodes, 'CAT ', 'XMID') > 0) or
    (IFFSearchNode(Nodes, 'FORM', 'XMID') > 0);
end;

//==============================================================================
//
// TXMICore.DetectCMF
//
//==============================================================================
function TXMICore.DetectCMF(var F: TMemoryStream): boolean;
var
  Head: array[0..3] of AnsiChar;
begin
  Result := False;
  if F.Size < 36 then
    Exit;
  F.Seek(0, soFromBeginning);
  F.ReadBuffer(Head, 4);
  Result := Head = 'CTMF';
end;

//==============================================================================
//
// TXMICore.DetectCMFMemory
//
//==============================================================================
function TXMICore.DetectCMFMemory(const adata: Pointer; const asize: integer): boolean;
var
  Head: array[0..3] of AnsiChar;
  pb: PByteArray;
begin
  Result := False;
  if asize < 36 then
    Exit;
  pb := adata;
  Head[0] := Chr(pb[0]);
  Head[1] := Chr(pb[1]);
  Head[2] := Chr(pb[2]);
  Head[3] := Chr(pb[3]);
  Result := Head = 'CTMF';
end;

//==============================================================================
//
// TXMICore.DetectROL
//
//==============================================================================
function TXMICore.DetectROL(var F: TMemoryStream): boolean;
var
  W: word;
  Meta: array[0..39] of byte;
begin
  Result := False;
  if F.Size < $B6 then
    Exit;
  F.Seek(0, soFromBeginning);
  F.ReadBuffer(W, 2);
  if W <> 0 then
    Exit;
  F.ReadBuffer(W, 2);
  if W <> 4 then
    Exit;
  F.ReadBuffer(Meta, SizeOf(Meta));
  if PAnsiChar(@Meta[0]) <> '\roll\default' then
    Exit;
  Result := True;
end;

//==============================================================================
//
// TXMICore.DetectROLMemory
//
//==============================================================================
function TXMICore.DetectROLMemory(const adata: Pointer; const asize: integer): boolean;
var
  W: word;
  Meta: array[0..39] of byte;
  pb: PByteArray;
  i: integer;
begin
  Result := False;
  if asize < $B6 then
    Exit;
  pb := adata;
  W := PWord(@pb[0])^;
  if W <> 0 then
    Exit;
  W := PWord(@pb[2])^;
  if W <> 4 then
    Exit;
  for i := 0 to 39 do
    Meta[i] := pb[i + 4];
  if PAnsiChar(@Meta[0]) <> '\roll\default' then
    Exit;
  Result := True;
end;

//==============================================================================
//
// TXMICore.DetectMUS
//
//==============================================================================
function TXMICore.DetectMUS(var F: TMemoryStream): boolean;
var
  W: word;
begin
  Result := False;
  if F.Size < 70 then
    Exit;
  F.Seek(0, soFromBeginning);
  F.ReadBuffer(W, 2);
  Result := W = 1;
end;

//==============================================================================
//
// TXMICore.DetectMUSMemory
//
//==============================================================================
function TXMICore.DetectMUSMemory(const adata: Pointer; const asize: integer): boolean;
var
  pw: PWord;
begin
  Result := False;
  if asize < 70 then
    Exit;
  pw := adata;
  Result := pw^ = 1;
end;

//==============================================================================
//
// TXMICore.DetectSOP
//
//==============================================================================
function TXMICore.DetectSOP(var F: TMemoryStream): boolean;
var
  sign: array[0..7] of byte;
  B: byte;
begin
  Result := False;
  if F.Size < $4C then
    Exit;
  F.Seek(0, soFromBeginning);
  sign[6] := 0;
  F.ReadBuffer(sign, SizeOf(sign) - 1);
  if PAnsiChar(@sign[0]) <> 'sopepos' then
    Exit;
  F.ReadBuffer(B, 1);
  if B <> 0 then
    Exit;
  F.ReadBuffer(B, 1);
  if B <> 1 then
    Exit;
  F.ReadBuffer(B, 1);
  if B <> 0 then
    Exit;
  Result := True;
end;

//==============================================================================
//
// TXMICore.DetectSOPMemory
//
//==============================================================================
function TXMICore.DetectSOPMemory(const adata: Pointer; const asize: integer): boolean;
var
  pb: PByteArray;
begin
  Result := False;
  if asize < $4C then
    Exit;
  pb := adata;
  Result :=
    (pb[0] = Ord('s')) and
    (pb[1] = Ord('o')) and
    (pb[2] = Ord('p')) and
    (pb[3] = Ord('e')) and
    (pb[4] = Ord('p')) and
    (pb[5] = Ord('o')) and
    (pb[6] = Ord('s')) and
    (pb[7] = 0) and
    (pb[8] = 1) and
    (pb[9] = 0);
end;

//==============================================================================
//
// TXMICore.ReadMIDI
//
//==============================================================================
function TXMICore.ReadMIDI(var F: TMemoryStream): boolean;
type
  TrackInfo = record
    Offset, Size: cardinal;
  end;
label
  Done;
var
  Nodes: IFFNodes;
  B: byte;
  I: integer;
  Ver, Tracks, Division: word;
  SMPTE: shortint;
  TracksInfo: array of TrackInfo;
  TrackStream: TMemoryStream;
begin
  Result := False;
  // Checking file size
  if F.Size < 14 then
  begin
    LogOutput('[-] Error: Wrong file size.');
    Exit;
  end;
  IFFGetNodes('OIFF', F.Memory, 0, F.Size, Nodes);
  if Length(Nodes) = 0 then
  begin
    LogOutput('[-] Error: Wrong file header.');
    Exit;
  end;
  // Checking header
  if (Nodes[0].Name <> 'MThd') and (Nodes[0].Name <> 'MTrk') then
  begin
    LogOutput('[-] Error: Wrong MIDI header.');
    Exit;
  end;
  if Nodes[0].Name = 'MThd' then
  begin
    // Reading header size
    if Nodes[0].Size < 6 then
    begin
      LogOutput('[-] Error: MIDI header too small (' + IntToStr(
        Nodes[0].Size) + ' bytes).');
      Exit;
    end;
    if Nodes[0].Size > 255 then
    begin
      LogOutput('[-] Error: MIDI header too big (' + IntToStr(
        Nodes[0].Size) + ' bytes).');
      Exit;
    end;
    if (Nodes[0].Size > 6) and (Nodes[0].Size <= 255) then
      LogOutput('[*] Warning: Non-standart MIDI header size (' +
        IntToStr(Nodes[0].Size) + ' bytes).');
    // Reading version
    F.Seek(Nodes[0].DataOffs, soFromBeginning);
    F.ReadBuffer(B, 1);
    Ver := B shl 8;
    F.ReadBuffer(B, 1);
    Ver := Ver or B;
    SongData_PutInt('MIDIType', Ver);
    if Ver > 2 then
      LogOutput('[*] Warning: Unknown MIDI Type.');
    // Reading track count
    F.ReadBuffer(B, 1);
    Tracks := B shl 8;
    F.ReadBuffer(B, 1);
    Tracks := Tracks or B;
    if (Ver = 0) and (Tracks > 1) then
      LogOutput('[*] Warning: MIDI Type 0 supports only one track.');
    // Reading speed value
    F.ReadBuffer(SMPTE, 1);
    F.ReadBuffer(B, 1);
    Division := B;
    if SMPTE >= 0 then
      Division := Division or (SMPTE shl 8);
    SongData_PutDWord('InitTempo', MIDIStdTempo);
    SongData_PutInt('SMPTE', SMPTE);
    SongData_PutInt('Division', Division);

    if Tracks = 0 then
      goto Done;

    LogOutput('[*] Checking tracks...');
    SetLength(TrackData, 0);
    if Tracks > Length(Nodes) - 1 then
    begin
      LogOutput('[*] Warning: Missing tracks (' + IntToStr(Tracks -
        Length(Nodes) + 1) + ' out of ' + IntToStr(Tracks) + ').');
      Tracks := Length(Nodes) - 1;
    end;

    for I := 0 to Tracks - 1 do
    begin
      if Nodes[I + 1].Name <> 'MTrk' then
      begin
        LogOutput('[*] Warning: Track #' + IntToStr(I) +
          ' has incorrect chunk name "' + Nodes[I + 1].Name + '", skipping.');
        Continue;
      end;
      SetLength(TracksInfo, Length(TracksInfo) + 1);
      TracksInfo[High(TracksInfo)].Offset := Nodes[I + 1].DataOffs;
      TracksInfo[High(TracksInfo)].Size := Nodes[I + 1].Size;
      LogOutput('[+] Track #' + IntToStr(I) + ' found (' + IntToStr(
        Nodes[I + 1].Size) + ' bytes).');
    end;
  end
  else
  begin
    // Load tracks without header
    LogOutput('[*] Warning: MIDI header not found, using default settings.');
    SongData_PutInt('MIDIType', 1);
    SongData_PutInt('SMPTE', 0);
    SongData_PutInt('Division', 96);
    SongData_PutDWord('InitTempo', MIDIStdTempo);

    LogOutput('[*] Checking tracks...');
    Tracks := 0;
    for I := 0 to Length(Nodes) - 1 do
    begin
      if Nodes[I].Name <> 'MTrk' then
      begin
        LogOutput('[*] Warning: Track #' + IntToStr(I) +
          ' has incorrect chunk name "' + Nodes[I].Name + '", skipping.');
        Continue;
      end;
      SetLength(TracksInfo, Length(TracksInfo) + 1);
      TracksInfo[High(TracksInfo)].Offset := Nodes[I].DataOffs;
      TracksInfo[High(TracksInfo)].Size := Nodes[I].Size;
      Inc(Tracks);
      LogOutput('[+] Track #' + IntToStr(I) + ' found (' + IntToStr(
        Nodes[I].Size) + ' bytes).');
    end;
  end;

  SetLength(TrackData, Length(TracksInfo));
  for I := 0 to Length(TracksInfo) - 1 do
  begin
    // Reading track #I
    LogOutput('[*] Reading track ' + IntToStr(I) + '...');
    if F.Size < TracksInfo[I].Offset + TracksInfo[I].Size then
    begin
      if F.Size - TracksInfo[I].Offset < 0 then
      begin
        LogOutput('[-] Error: Track ' + IntToStr(I) + ' is incomplete.');
        Continue;
      end;
      LogOutput('[*] Warning: Track ' + IntToStr(I) + ' is incomplete.');
      TracksInfo[I].Size := F.Size - TracksInfo[I].Offset;
    end;
    TrackStream := TMemoryStream.Create;
    TrackStream.SetSize(TracksInfo[I].Size);
    F.Seek(TracksInfo[I].Offset, soFromBeginning);
    F.ReadBuffer(TrackStream.Memory^, TrackStream.Size);
    TrackData[I].Title := '';
    SetLength(TrackData[I].Data, 0);
    ReadTrackData(TrackStream, TrackData[I]);
    TrackStream.Free;
    LogOutput('[+] ' + IntToStr(Length(TrackData[I].Data)) + ' events found.');
  end;
  Done:
    if Length(Nodes) > Tracks + 1 then
      LogOutput(
      '[*] Warning: Additional data was found after the end of last track (additional data processing is disabled).');
  Result := True;
  LogOutput('');
  if Nodes[0].Name = 'MThd' then
    LogOutput('[*] Information:')
  else
    LogOutput('[*] Default settings:');
  LogSongInfo;
  LogOutput('');
  RefTrackList;
end;

//==============================================================================
//
// TXMICore.ReadRMI
//
//==============================================================================
function TXMICore.ReadRMI(var F: TMemoryStream): boolean;
var
  Nodes, FNodes, WNodes: IFFNodes;
  M: TMemoryStream;
begin
  Result := False;
  LogOutput('[*] Reading RIFF MIDI file...');
  IFFGetNodes('RIFF', F.Memory, 0, F.Size, Nodes);
  if IFFSearchNode(Nodes, 'RIFF') = 0 then
  begin
    LogOutput('[-] Error: No RIFF chunks found.');
    Exit;
  end;
  IFFSearchNode(Nodes, FNodes, 'RIFF', 'RMID');
  if Length(FNodes) = 0 then
  begin
    LogOutput('[-] Error: RIFF RMID chunk not found.');
    Exit;
  end;
  if F.Size < FNodes[0].DataOffs + FNodes[0].Size then
    LogOutput('[*] Warning: RIFF tree is truncated.');
  if IFFSearchNode(Nodes, WNodes, 'RIFF', 'WAVE') > 0 then
  begin
    IFFGetNodes('RIFF', F.Memory, WNodes[0].DataOffs, WNodes[0].Size, WNodes);
    if IFFSearchNode(WNodes, 'voyl') > 0 then
      LogOutput('[*] Digital Orchestrator Pro samples detected.');
  end;
  IFFGetNodes('RIFF', F.Memory, FNodes[0].DataOffs, FNodes[0].Size, Nodes);
  IFFSearchNode(Nodes, FNodes, 'data');
  if Length(FNodes) = 0 then
  begin
    LogOutput('[-] Error: Data chunk not found.');
    Exit;
  end;
  if F.Size < FNodes[0].DataOffs + FNodes[0].Size then
  begin
    if F.Size - FNodes[0].DataOffs < 0 then
    begin
      LogOutput('[-] Error: Data chunk is truncated.');
      Exit;
    end;
    LogOutput('[*] Warning: Data chunk is truncated.');
    FNodes[0].Size := F.Size - FNodes[0].DataOffs;
  end;

  if IFFSearchNode(Nodes, 'RIFF', 'DLS ') > 0 then
    LogOutput('[*] DLS instrument bank detected.');

  F.Seek(FNodes[0].DataOffs, soFromBeginning);
  M := TMemoryStream.Create;
  M.SetSize(FNodes[0].Size);
  F.ReadBuffer(M.Memory^, M.Size);
  Result := ReadMIDI(M);
  M.Free;
end;

//==============================================================================
//
// TXMICore.ReadMIDS
//
//==============================================================================
function TXMICore.ReadMIDS(var F: TMemoryStream): boolean;
var
  Nodes, FNodes: IFFNodes;
  M: TMemoryStream;
  Division: word;
begin
  Result := False;
  LogOutput('[*] Reading MIDI Stream file...');
  IFFGetNodes('RIFF', F.Memory, 0, F.Size, Nodes);
  if IFFSearchNode(Nodes, 'RIFF') = 0 then
  begin
    LogOutput('[-] Error: No RIFF chunks found.');
    Exit;
  end;
  IFFSearchNode(Nodes, FNodes, 'RIFF', 'MIDS');
  if Length(FNodes) = 0 then
  begin
    LogOutput('[-] Error: RIFF MIDS chunk not found.');
    Exit;
  end;
  if F.Size < FNodes[0].DataOffs + FNodes[0].Size then
    LogOutput('[*] Warning: RIFF tree is truncated.');
  IFFGetNodes('RIFF', F.Memory, FNodes[0].DataOffs, FNodes[0].Size, Nodes);
  IFFSearchNode(Nodes, FNodes, 'fmt ');
  if Length(FNodes) = 0 then
  begin
    LogOutput('[-] Error: Format chunk not found.');
    Exit;
  end;
  if FNodes[0].Size < 12 then
  begin
    LogOutput('[-] Error: Wrong format chunk size.');
    Exit;
  end;
  if F.Size < FNodes[0].DataOffs + FNodes[0].Size then
  begin
    LogOutput('[-] Error: Format chunk is truncated.');
    Exit;
  end;
  F.Seek(FNodes[0].DataOffs, soFromBeginning);
  F.ReadBuffer(Division, 2);
  SongData_PutDWord('InitTempo', MIDIStdTempo);
  SongData_PutInt('SMPTE', 0);
  SongData_PutInt('Division', Division);
  IFFSearchNode(Nodes, FNodes, 'data');
  if Length(FNodes) = 0 then
  begin
    LogOutput('[-] Error: Data chunk not found.');
    Exit;
  end;
  if F.Size < FNodes[0].DataOffs + FNodes[0].Size then
  begin
    if F.Size - FNodes[0].DataOffs < 0 then
    begin
      LogOutput('[-] Error: Data chunk is truncated.');
      Exit;
    end;
    LogOutput('[*] Warning: Data chunk is truncated.');
    FNodes[0].Size := F.Size - FNodes[0].DataOffs;
  end;

  SetLength(TrackData, 1);
  TrackData[0].Title := '';
  SetLength(TrackData[0].Data, 0);

  F.Seek(FNodes[0].DataOffs, soFromBeginning);
  M := TMemoryStream.Create;
  M.SetSize(FNodes[0].Size);
  F.ReadBuffer(M.Memory^, M.Size);
  ReadTrackData_MIDS(M, TrackData[0]);
  M.Free;

  LogOutput('[+] ' + IntToStr(Length(TrackData[0].Data)) + ' events found.');
  SongData_PutInt('MIDIType', 0);
  Result := True;
  LogOutput('');
  LogOutput('[*] Information:');
  LogSongInfo;
  LogOutput('');
  RefTrackList;
end;

//==============================================================================
//
// TXMICore.ReadXMI
//
//==============================================================================
function TXMICore.ReadXMI(var F: TMemoryStream): boolean;
type
  TrackInfo = record
    Offset, Size: cardinal;
  end;
var
  Nodes, FNodes, NodeData: IFFNodes;
  Tracks: word;
  TracksInfo: array of TrackInfo;
  I: integer;
  M: TMemoryStream;
begin
  Result := False;
  LogOutput('[*] Reading Extended MIDI file...');
  IFFGetNodes('IFF', F.Memory, 0, F.Size, Nodes);
  if IFFSearchNode(Nodes, 'CAT ', 'XMID') > 0 then
  begin
    // Standard multi-song XMIDI
    Tracks := 0;
    if (Nodes[0].Name = 'FORM') and (Nodes[0].Sub = 'XDIR') then
    begin
      // with header
      IFFGetNodes('IFF', F.Memory, Nodes[0].DataOffs, Nodes[0].Size, FNodes);
      if IFFSearchNode(FNodes, NodeData, 'INFO') > 0 then
      begin
        if NodeData[0].Size >= 2 then
        begin
          if NodeData[0].DataOffs + NodeData[0].Size <= F.Size then
          begin
            F.Seek(NodeData[0].DataOffs, soFromBeginning);
            F.ReadBuffer(Tracks, 2);
          end
          else
            LogOutput('[*] Warning: XMIDI INFO chunk is truncated.');
        end
        else
          LogOutput('[*] Warning: XMIDI INFO chunk has wrong size.');
      end
      else
        LogOutput('[*] Warning: XMIDI INFO chunk not found.');

      LogOutput('[*] Checking tracks...');
      IFFSearchNode(Nodes, FNodes, 'CAT ', 'XMID');
      IFFGetNodes('IFF', F.Memory, FNodes[0].DataOffs, FNodes[0].Size, Nodes);
      IFFSearchNode(Nodes, FNodes, 'FORM', 'XMID');
      Nodes := FNodes;
      if Tracks > 0 then
      begin
        if Length(Nodes) > Tracks then
          LogOutput('[*] Warning: Additional XMIDI chunks found.');
        if Length(Nodes) < Tracks then
        begin
          LogOutput('[*] Warning: Missing XMIDI tracks (' + IntToStr(Tracks -
            Length(Nodes)) + ' out of ' + IntToStr(Tracks) + ').');
          Tracks := Length(Nodes);
        end;
      end
      else
        Tracks := Length(Nodes);
      for I := 0 to Tracks - 1 do
      begin
        IFFGetNodes('IFF', F.Memory, Nodes[I].DataOffs, Nodes[I].Size, NodeData);
        IFFSearchNode(NodeData, FNodes, 'EVNT');
        SetLength(TracksInfo, Length(TracksInfo) + 1);
        if Length(FNodes) > 0 then
        begin
          TracksInfo[High(TracksInfo)].Offset := FNodes[0].DataOffs;
          TracksInfo[High(TracksInfo)].Size := FNodes[0].Size;
        end
        else
        begin
          TracksInfo[High(TracksInfo)].Offset := Nodes[I].DataOffs;
          TracksInfo[High(TracksInfo)].Size := 0;
        end;
        LogOutput('[+] Track #' + IntToStr(I) + ' found (' + IntToStr(
          TracksInfo[High(TracksInfo)].Size) + ' bytes).');
      end;
    end;
  end
  else
  if IFFSearchNode(Nodes, 'FORM', 'XMID') > 0 then
  begin
    // Detached XMIDI chunks
    LogOutput('[*] Checking tracks...');
    IFFSearchNode(Nodes, FNodes, 'FORM', 'XMID');
    Nodes := FNodes;
    for I := 0 to Length(Nodes) - 1 do
    begin
      IFFGetNodes('IFF', F.Memory, Nodes[I].DataOffs, Nodes[I].Size, NodeData);
      IFFSearchNode(NodeData, FNodes, 'EVNT');
      SetLength(TracksInfo, Length(TracksInfo) + 1);
      if Length(FNodes) > 0 then
      begin
        TracksInfo[High(TracksInfo)].Offset := FNodes[0].DataOffs;
        TracksInfo[High(TracksInfo)].Size := FNodes[0].Size;
      end
      else
      begin
        TracksInfo[High(TracksInfo)].Offset := Nodes[I].DataOffs;
        TracksInfo[High(TracksInfo)].Size := 0;
      end;
      LogOutput('[+] Track #' + IntToStr(I) + ' found (' + IntToStr(
        TracksInfo[High(TracksInfo)].Size) + ' bytes).');
    end;
  end
  else
  begin
    LogOutput('[-] Error: XMIDI headers not found.');
    Exit;
  end;

  SongData_PutDWord('InitTempo', MIDIStdTempo);
  SongData_PutInt('SMPTE', 0);
  SongData_PutInt('Division', 60);

  SetLength(TrackData, 0);
  for I := 0 to Length(TracksInfo) - 1 do
  begin
    // Reading track #I
    LogOutput('[*] Reading track ' + IntToStr(I) + '...');
    if F.Size < TracksInfo[I].Offset + TracksInfo[I].Size then
    begin
      if F.Size - TracksInfo[I].Offset < 0 then
      begin
        LogOutput('[-] Error: Event data is truncated.');
        Continue;
      end;
      LogOutput('[*] Warning: Event data is truncated.');
      TracksInfo[I].Size := F.Size - TracksInfo[I].Offset;
    end;
    SetLength(TrackData, Length(TrackData) + 1);
    TrackData[High(TrackData)].Title := '';
    SetLength(TrackData[High(TrackData)].Data, 0);

    F.Seek(TracksInfo[I].Offset, soFromBeginning);
    M := TMemoryStream.Create;
    M.SetSize(TracksInfo[I].Size);
    F.ReadBuffer(M.Memory^, M.Size);
    ReadTrackData_XMI(M, TrackData[High(TrackData)]);
    M.Free;
    LogOutput('[+] ' + IntToStr(Length(TrackData[High(TrackData)].Data)) +
      ' events found.');
  end;
  SongData_PutInt('MIDIType', 2);
  Result := True;
  LogOutput('');
  LogOutput('[*] Information:');
  LogSongInfo;
  LogOutput('');
  RefTrackList;
end;

//==============================================================================
//
// TXMICore.ReadCMF
//
//==============================================================================
function TXMICore.ReadCMF(var F: TMemoryStream): boolean;
type
  CMFInstrument = array[0..15] of byte;
var
  DW: cardinal;
  W: word;
  iOffsetInstruments, iOffsetMusic, iTicksPerQuarter, iTicksPerSecond,
  iOffsetTitle, iOffsetComposer, iOffsetRemarks, iInstrumentCount, iTempo: word;
  iChannelInUse: array[0..15] of byte;
  dwInstrumentSize, dwDataSize: DWord;
  Instruments: array of CMFInstrument;
  MIDIData: TMemoryStream;
  sTitle, sComposer, sRemarks: ansistring;
  I: integer;
begin
  Result := False;
  LogOutput('[*] Reading Creative Music File...');
  if F.Size < 36 then
  begin
    LogOutput('[-] Error: Wrong file size.');
    Exit;
  end;

  F.Seek(0, soFromBeginning);
  F.ReadBuffer(DW, 4);
  if DW <> $464D5443 then
  begin // 'CTMF'
    LogOutput('[-] Error: Wrong header signature.');
    Exit;
  end;
  F.ReadBuffer(W, 2);
  if (W <> $0100) and (W <> $0101) then
  begin
    LogOutput('[-] Error: Unknown file version.');
    Exit;
  end;
  SongData_PutInt('CMF_Version', W);

  F.ReadBuffer(iOffsetInstruments, 2);
  F.ReadBuffer(iOffsetMusic, 2);
  F.ReadBuffer(iTicksPerQuarter, 2);
  F.ReadBuffer(iTicksPerSecond, 2);
  F.ReadBuffer(iOffsetTitle, 2);
  F.ReadBuffer(iOffsetComposer, 2);
  F.ReadBuffer(iOffsetRemarks, 2);
  F.ReadBuffer(iChannelInUse, 16);
  SongData_PutInt('CMF_TicksPerQuarter', iTicksPerQuarter);
  SongData_PutInt('CMF_TicksPerSecond', iTicksPerSecond);

  SongData_PutDWord('InitTempo', 1000000);
  SongData_PutInt('SMPTE', 0);
  SongData_PutInt('Division', iTicksPerSecond);

  if W = $0100 then
  begin
    iInstrumentCount := 0;
    iTempo := 0;
    F.ReadBuffer(iInstrumentCount, 1);
  end
  else
  begin
    F.ReadBuffer(iInstrumentCount, 2);
    F.ReadBuffer(iTempo, 2);
  end;

  dwInstrumentSize := iInstrumentCount * 16;
  if iOffsetInstruments > 0 then
  begin
    if iOffsetInstruments + dwInstrumentSize > F.Size then
    begin
      LogOutput('[*] Warning: Instrument section is truncated.');
      if iOffsetInstruments >= F.Size then
        dwInstrumentSize := 0
      else
        dwInstrumentSize := F.Size - iOffsetInstruments;
    end;

    SetLength(Instruments, iInstrumentCount);
    FillChar(Instruments[0], iInstrumentCount * 16, 0);
    if dwInstrumentSize > 0 then
    begin
      F.Seek(iOffsetInstruments, soFromBeginning);
      F.ReadBuffer(Instruments[0], dwInstrumentSize);
    end;
  end
  else
    iInstrumentCount := 0;

  SongData_PutInt('CMF_Tempo', iTempo);

  for I := 0 to Length(Instruments) - 1 do
    SongData_PutArray('CMF_Inst#' + IntToStr(I), Instruments[I]);

  if iOffsetTitle >= F.Size then
  begin
    LogOutput('[*] Warning: Title string is out of file.');
    iOffsetTitle := 0;
  end;
  if iOffsetTitle > 0 then
    sTitle := PAnsiChar(cardinal(F.Memory) + iOffsetTitle);

  SongData_PutStr('CMF_Title', sTitle);

  if iOffsetComposer >= F.Size then
  begin
    LogOutput('[*] Warning: Composer string is out of file.');
    iOffsetComposer := 0;
  end;
  if iOffsetComposer > 0 then
    sComposer := PAnsiChar(cardinal(F.Memory) + iOffsetComposer);

  SongData_PutStr('CMF_Composer', sComposer);

  if iOffsetRemarks >= F.Size then
  begin
    LogOutput('[*] Warning: Remarks string is out of file.');
    iOffsetRemarks := 0;
  end;
  if iOffsetRemarks > 0 then
    sRemarks := PAnsiChar(cardinal(F.Memory) + iOffsetRemarks);

  SongData_PutStr('CMF_Remarks', sRemarks);

  if iOffsetMusic >= F.Size then
    iOffsetMusic := 0;
  if iOffsetMusic > 0 then
  begin
    dwDataSize := F.Size - iOffsetMusic;
    if iOffsetTitle >= iOffsetMusic then
      dwDataSize := iOffsetTitle - iOffsetMusic
    else if iOffsetComposer >= iOffsetMusic then
      dwDataSize := iOffsetComposer - iOffsetMusic
    else if iOffsetRemarks >= iOffsetMusic then
      dwDataSize := iOffsetRemarks - iOffsetMusic
    else if iOffsetInstruments >= iOffsetMusic then
      dwDataSize := iOffsetInstruments - iOffsetMusic;

    SetLength(TrackData, 1);
    TrackData[0].Title := sTitle;
    SetLength(TrackData[0].Data, 0);
    MIDIData := TMemoryStream.Create;
    MIDIData.SetSize(dwDataSize);
    F.Seek(iOffsetMusic, soFromBeginning);
    F.ReadBuffer(MIDIData.Memory^, dwDataSize);
    ReadTrackData(MIDIData, TrackData[0]);
    MIDIData.Free;
    LogOutput('[+] ' + IntToStr(Length(TrackData[0].Data)) + ' events found.');
  end
  else
    LogOutput('[*] Warning: Track data is missing.');

  SongData_PutInt('MIDIType', 0);
  Result := True;
  LogOutput('');
  LogOutput('[*] Information:');
  LogSongInfo;
  LogOutput('');
  RefTrackList;
end;

//==============================================================================
//
// ROL_LoadBank
//
//==============================================================================
function ROL_LoadBank(FileName: string): boolean;
var
  M: TMemoryStream;
  W, numUsed, numInstruments: word;
  offsetName, offsetData: DWord;
  Sign: array[0..5] of AnsiChar;
  I: integer;
  B: byte;
  Name: array[0..8] of AnsiChar;
  Inst: array[0..13 + 13 + 2 - 1] of byte;
begin
  Result := False;
  M := TMemoryStream.Create;
  try
    M.LoadFromFile(FileName);
  except
    M.Free;
    Exit;
  end;
  if M.Size < 28 then
  begin
    // Wrong file size
    M.Free;
    Exit;
  end;

  M.Seek(0, soFromBeginning);
  M.ReadBuffer(W, 2);
  if W > 1 then
  begin
    // Wrong file version
    M.Free;
    Exit;
  end;
  M.ReadBuffer(Sign, 6);
  if Sign <> 'ADLIB-' then
  begin
    // Wrong signature
    M.Free;
    Exit;
  end;
  M.ReadBuffer(numUsed, 2);
  M.ReadBuffer(numInstruments, 2);
  M.ReadBuffer(offsetName, 4);
  M.ReadBuffer(offsetData, 4);

  if (offsetName >= M.Size) or (offsetData >= M.Size) then
  begin
    // Wrong offsets
    M.Free;
    Exit;
  end;
  if (M.Size < offsetName + 12 * numInstruments) or
    (M.Size < offsetData + 30 * numInstruments) then
  begin
    // Wrong file size
    M.Free;
    Exit;
  end;

  SongData_PutInt('BNK_Version', W);
  SongData_PutStr('BNK_Signature', string(Sign));
  SongData_PutInt('BNK_Used', numUsed);

  M.Seek(offsetName, soFromBeginning);
  for I := 0 to numInstruments - 1 do
  begin
    M.ReadBuffer(W, 2);
    SongData_PutInt('BNK_Idx#' + IntToStr(I), W);
    M.ReadBuffer(B, 1);
    SongData_PutInt('BNK_Flags#' + IntToStr(I), B);
    M.ReadBuffer(Name[0], 9);
    SongData_PutStr('BNK_Name#' + IntToStr(I), string(PAnsiChar(@Name[0])));
  end;
  M.Seek(offsetData, soFromBeginning);
  for I := 0 to numInstruments - 1 do
  begin
    M.ReadBuffer(B, 1);
    SongData_PutInt('BNK_Perc#' + IntToStr(I), B);
    M.ReadBuffer(B, 1);
    SongData_PutInt('BNK_Chan#' + IntToStr(I), B);
    M.ReadBuffer(Inst, Length(Inst));
    SongData_PutArray('BNK_Data#' + IntToStr(I), Inst);
  end;
  M.Free;
  Result := True;
end;

//==============================================================================
//
// TXMICore.ReadROL
//
//==============================================================================
function TXMICore.ReadROL(var F: TMemoryStream; FileName: string): boolean;
var
  W: word;
  Meta: array[0..40 - 1] of byte;
  B: byte;
  I, J: integer;
  Name: array[0..15 - 1] of byte;
  Inst: array[0..9 + 1 + 2 - 1] of byte;
  Fl: single;
  Tk: word;
  Bank: string;
  BankLoad: boolean;
begin
  Result := False;
  LogOutput('[*] Reading AdLib ROL file...');
  if F.Size < $B6 then
  begin
    LogOutput('[-] Error: Wrong file size.');
    Exit;
  end;

  F.Seek(0, soFromBeginning);
  F.ReadBuffer(I, 4);
  if I <> $40000 then
  begin
    LogOutput('[-] Error: Wrong file version.');
    Exit;
  end;
  SongData_PutInt('ROL_Version', I);
  F.ReadBuffer(Meta, SizeOf(Meta));
  if PAnsiChar(@Meta[0]) <> '\roll\default' then
  begin
    LogOutput('[-] Error: Wrong file header.');
    Exit;
  end;
  SongData_PutStr('ROL_Signature', PAnsiChar(@Meta[0]));
  F.ReadBuffer(W, 2);
  SongData_PutInt('ROL_TicksPerBeat', W);
  SongData_PutInt('InitTempo', 60000000 div W);
  F.ReadBuffer(W, 2);
  SongData_PutInt('ROL_BeatPerMeasure', W);
  F.ReadBuffer(W, 2);
  SongData_PutInt('ROL_ScaleY', W);
  F.ReadBuffer(W, 2);
  SongData_PutInt('ROL_ScaleX', W);
  F.Seek(1, soCurrent); // skip reserved value
  F.ReadBuffer(B, 1);
  SongData_PutInt('ROL_Melodic', B);
  F.Seek(90, soCurrent); // skip temp values
  F.Seek(38, soCurrent); // skip padding
  if F.Position < F.Size then
  begin
    // add tracks
    I := 0;
    F.ReadBuffer(Name, SizeOf(Name));
    SetLength(TrackData, I + 1);
    TrackData[I].Title := PAnsiChar(@Name[0]);
    F.ReadBuffer(Fl, 4);
    SongData_PutFloat('ROL_BasicTempo', Fl);
    SongData_PutInt('Division', Round(Fl));
    F.ReadBuffer(W, 2);
    SetLength(TrackData[I].Data, W);
    for J := 0 to Length(TrackData[I].Data) - 1 do
    begin
      F.ReadBuffer(W, 2);
      F.ReadBuffer(Fl, 4);
      TrackData[I].Data[J].Ticks := W;
      TrackData[I].Data[J].Status := $FF;
      TrackData[I].Data[J].BParm1 := $7F;
      SetLength(TrackData[I].Data[J].DataArray, 1 + 4);
      TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);
      TrackData[I].Data[J].DataArray[0] := 0;
      Move(Fl, TrackData[I].Data[J].DataArray[1], 4);
    end;
    // Convert ticks from absolute to relative
    ConvertTicks(False, TrackData[I].Data);

    while F.Position < F.Size do
    begin
      Inc(I);
      F.ReadBuffer(Name, SizeOf(Name));
      SetLength(TrackData, I + 1);
      TrackData[I].Title := PAnsiChar(@Name[0]);
      if (I - 1) mod 4 = 0 then
      begin // Voix #
        F.ReadBuffer(Tk, 2);
        J := 0;
        while Tk > 0 do
        begin
          SetLength(TrackData[I].Data, J + 1);
          F.ReadBuffer(W, 2);
          TrackData[I].Data[J].Status := $90;
          if W > 0 then
            TrackData[I].Data[J].BParm2 := $7F
          else
            TrackData[I].Data[J].BParm2 := $00;
          TrackData[I].Data[J].Status :=
            TrackData[I].Data[J].Status or (((I - 1) div 4) and $F);
          TrackData[I].Data[J].BParm1 := W;
          F.ReadBuffer(W, 2);
          TrackData[I].Data[J].Len := W;
          Dec(Tk, W);
          Inc(J);
        end;
        if Length(TrackData[I].Data) > 0 then
        begin
          SetLength(TrackData[I].Data, J + 1);
          TrackData[I].Data[J].Status := $90 or (((I - 1) div 4) and $F);
          TrackData[I].Data[J].BParm1 := 0;
          TrackData[I].Data[J].BParm2 := 0;
          for J := 0 to Length(TrackData[I].Data) - 2 do
          begin
            TrackData[I].Data[J + 1].Ticks := TrackData[I].Data[J].Len;
            TrackData[I].Data[J].Len := 0;
          end;
          while TrackData[I].Data[0].BParm1 = 0 do
            DelEvent(I, 0, True);
        end;
      end
      else
      begin
        F.ReadBuffer(W, 2);
        SetLength(TrackData[I].Data, W);
        for J := 0 to Length(TrackData[I].Data) - 1 do
          case (I - 1) mod 4 of
            1: // Timbre #
            begin
              F.ReadBuffer(W, 2);
              TrackData[I].Data[J].Ticks := W;
              TrackData[I].Data[J].Status := $FF;
              TrackData[I].Data[J].BParm1 := $7F;
              F.ReadBuffer(Inst, SizeOf(Inst));
              SetLength(TrackData[I].Data[J].DataArray, 1 + SizeOf(Inst));
              TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);
              TrackData[I].Data[J].DataArray[0] := 1;
              Move(Inst, TrackData[I].Data[J].DataArray[1], SizeOf(Inst));
            end;
            2: // Volume #
            begin
              F.ReadBuffer(W, 2);
              TrackData[I].Data[J].Ticks := W;
              TrackData[I].Data[J].Status := $FF;
              TrackData[I].Data[J].BParm1 := $7F;
              SetLength(TrackData[I].Data[J].DataArray, 1 + 4);
              TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);
              F.ReadBuffer(Fl, 4);
              TrackData[I].Data[J].DataArray[0] := 2;
              Move(Fl, TrackData[I].Data[J].DataArray[1], 4);
            end;
            3: // Pitch #
            begin
              F.ReadBuffer(W, 2);
              TrackData[I].Data[J].Ticks := W;
              TrackData[I].Data[J].Status := $FF;
              TrackData[I].Data[J].BParm1 := $7F;
              SetLength(TrackData[I].Data[J].DataArray, 1 + 4);
              TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);
              F.ReadBuffer(Fl, 4);
              TrackData[I].Data[J].DataArray[0] := 3;
              Move(Fl, TrackData[I].Data[J].DataArray[1], 4);
            end;
          end;
        // Convert ticks from absolute to relative
        ConvertTicks(False, TrackData[I].Data);
      end;
      if I >= 11 * 4 then
        Break;
    end;
  end;

  LogOutput('[*] Looking for instrument bank file...');
  Bank := ChangeFileExt(FileName, '.bnk');
  BankLoad := ROL_LoadBank(Bank);
  if not BankLoad then
  begin
    Bank := ExtractFilePath(FileName) + 'standard.bnk';
    BankLoad := ROL_LoadBank(Bank);
  end;
  if BankLoad then
    LogOutput('[*] Loaded instrument bank: ' + Bank)
  else
    LogOutput('[*] Instrument bank file not found.');
  SongData_PutInt('MIDIType', 1);
  Result := True;
  LogOutput('');
  LogOutput('[*] Information:');
  LogSongInfo;
  LogOutput('');
  RefTrackList;
end;

//==============================================================================
//
// MUS_LoadBank
//
//==============================================================================
function MUS_LoadBank(FileName: string): boolean;
var
  M: TMemoryStream;
  W: word;
  InstCnt, InstOff: word;
  I: integer;
  Name: array[0..8] of AnsiChar;
  Inst: array[0..13 + 13 + 2 - 1] of word;
begin
  Result := False;
  M := TMemoryStream.Create;
  try
    M.LoadFromFile(FileName);
  except
    M.Free;
    Exit;
  end;
  if M.Size < 6 then
  begin
    // Wrong file size
    M.Free;
    Exit;
  end;

  M.Seek(0, soFromBeginning);
  M.ReadBuffer(W, 2);
  if W <> 1 then
  begin
    // Wrong file version
    M.Free;
    Exit;
  end;
  M.ReadBuffer(InstCnt, 2);
  if M.Size < InstCnt * 65 + 6 then
  begin
    // Wrong file size
    M.Free;
    Exit;
  end;
  M.ReadBuffer(InstOff, 2);
  if InstOff <> InstCnt * 9 + 6 then
  begin
    // Wrong instrument offset
    M.Free;
    Exit;
  end;
  SongData_PutInt('SND_Version', W);
  for I := 0 to InstCnt - 1 do
  begin
    M.ReadBuffer(Name[0], 9);
    SongData_PutStr('SND_Name#' + IntToStr(I), PAnsiChar(@Name[0]));
  end;
  for I := 0 to InstCnt - 1 do
  begin
    M.ReadBuffer(Inst, SizeOf(Inst));
    SongData_PutArray('SND_Data#' + IntToStr(I), Inst);
  end;
  M.Free;
  Result := True;
end;

//==============================================================================
//
// TXMICore.ReadMUS
//
//==============================================================================
function TXMICore.ReadMUS(var F: TMemoryStream; FileName: string): boolean;
var
  MIDILength, totalTick, nrCommand, totalTickAct, nrCommandAct: DWORD;
  DW, InitTempo: cardinal;
  Division, W: word;
  tuneName: array[0..29] of AnsiChar;
  Title, Bank: string;
  MIDIData: TMemoryStream;
  BankLoad: boolean;
  I: integer;
  Banks: TStringList;
  B: byte;
  instName: array[0..8] of AnsiChar;
begin
  Result := False;
  if Container = 'ims' then
    LogOutput('[*] Reading AdLib IMS file...')
  else
    LogOutput('[*] Reading AdLib MUS file...');

  if F.Size < 70 then
  begin
    LogOutput('[-] Error: Wrong file size.');
    Exit;
  end;

  F.Seek(0, soFromBeginning);
  F.ReadBuffer(W, 2);
  if W <> 1 then
  begin
    LogOutput('[-] Error: Wrong file version.');
    Exit;
  end;
  SongData_PutInt('MUS_Version', W);

  F.ReadBuffer(DW, 4);
  SongData_PutInt('MUS_ID', DW);
  F.ReadBuffer(tuneName[0], 30);
  Title := PAnsiChar(@tuneName[0]);
  SongData_PutStr('MUS_TuneName', Title);

  F.Seek($24, soFromBeginning);
  F.ReadBuffer(B, 1);
  SongData_PutInt('MUS_TicksPerBeat', B);
  if B = 0 then
    B := 240;
  InitTempo := 60000000 div B;
  SongData_PutDWord('InitTempo', InitTempo);

  F.Seek($25, soFromBeginning);
  F.ReadBuffer(B, 1);
  SongData_PutInt('MUS_BeatPerMeasure', B);

  F.Seek($26, soFromBeginning);
  F.ReadBuffer(totalTick, 4);

  F.Seek($2E, soFromBeginning);
  F.ReadBuffer(nrCommand, 4);

  F.Seek($3A, soFromBeginning);
  F.ReadBuffer(B, 1);
  SongData_PutInt('MUS_Percussive', B);

  F.Seek($3B, soFromBeginning);
  F.ReadBuffer(B, 1);
  SongData_PutInt('MUS_PitchBendRange', B);

  F.Seek($3C, soFromBeginning);
  F.ReadBuffer(Division, 2);
  SongData_PutInt('MUS_BasicTempo', Division);
  SongData_PutInt('SMPTE', 0);
  SongData_PutInt('Division', Division);

  F.Seek($2A, soFromBeginning);
  F.ReadBuffer(MIDILength, 4);
  if F.Size > 70 + MIDILength then
  begin
    if Container = 'mus' then
    begin
      LogOutput('[*] Warning: File has some excess data.');
      F.SetSize(70 + MIDILength);
    end;
  end
  else
  if F.Size < 70 + MIDILength then
  begin
    LogOutput('[*] Warning: File seems to be truncated.');
    MIDILength := F.Size - 70;
  end;

  SetLength(TrackData, 1);
  TrackData[0].Title := Title;
  SetLength(TrackData[0].Data, 0);
  MIDIData := TMemoryStream.Create;
  MIDIData.SetSize(MIDILength);
  F.Seek($46, soFromBeginning);
  F.ReadBuffer(MIDIData.Memory^, MIDILength);
  ReadTrackData_MUS(MIDIData, TrackData[0]);
  MIDIData.Free;
  LogOutput('[+] ' + IntToStr(Length(TrackData[0].Data)) + ' events found.');
  SongData_PutInt('MIDIType', 0);

  totalTickAct := 0;
  for I := 0 to Length(TrackData[0].Data) - 1 do
    totalTickAct := totalTickAct + TrackData[0].Data[I].Ticks;
  if totalTickAct <> totalTick then
    LogOutput(
      '[*] Warning: Defined number of ticks in the header doesn''t correspond actual number of ticks.');

  nrCommandAct := 0;
  for I := 0 to Length(TrackData[0].Data) - 1 do
  begin
    if TrackData[0].Data[I].Ticks >= 240 then
      nrCommandAct := nrCommandAct + TrackData[0].Data[I].Ticks div 240;
    Inc(nrCommandAct);
  end;
  if nrCommandAct <> nrCommand then
    LogOutput(
      '[*] Warning: Defined number of commands in the header doesn''t correspond actual number of commands.');

  if Container = 'mus' then
  begin
    LogOutput('[*] Looking for instrument bank file...');
    Bank := ChangeFileExt(FileName, '.snd');
    BankLoad := MUS_LoadBank(Bank);
    if not BankLoad then
    begin
      Bank := ChangeFileExt(FileName, '.tim');
      BankLoad := MUS_LoadBank(Bank);
    end;
    if not BankLoad then
    begin
      Banks := TStringList.Create;
      GetFiles(ExtractFilePath(FileName), '*.snd', Banks);
      for I := 0 to Banks.Count - 1 do
      begin
        Bank := Banks[I];
        BankLoad := MUS_LoadBank(Bank);
        if BankLoad then
          Break;
      end;
      Banks.Free;
    end;
    if not BankLoad then
    begin
      Banks := TStringList.Create;
      GetFiles(ExtractFilePath(FileName), '*.tim', Banks);
      for I := 0 to Banks.Count - 1 do
      begin
        Bank := Banks[I];
        BankLoad := MUS_LoadBank(Bank);
        if BankLoad then
          Break;
      end;
      Banks.Free;
    end;
    if BankLoad then
      LogOutput('[*] Loaded instrument bank: ' + Bank)
    else
      LogOutput('[*] Instrument bank file not found.');
  end;
  if Container = 'ims' then
    if F.Size < 70 + MIDILength + 2 + 2 then
      LogOutput('[-] Instrument section not found.')
    else
    begin
      F.Seek(70 + MIDILength, soFromBeginning);
      F.ReadBuffer(W, 2);
      if W <> $7777 then
        LogOutput('[-] Instrument section has wrong signature.')
      else
      begin
        F.ReadBuffer(W, 2);
        if F.Size < 70 + MIDILength + 2 + 2 + W * 9 then
          LogOutput('[-] Instrument section is truncated.')
        else
          for I := 0 to W - 1 do
          begin
            F.ReadBuffer(instName[0], 9);
            SongData_PutStr('IMS_Name#' + IntToStr(I), string(PAnsiChar(@instName[0])));
          end;
      end;
    end;
  Result := True;
  LogOutput('');
  LogOutput('[*] Information:');
  LogSongInfo;
  LogOutput('');
  RefTrackList;
end;

//==============================================================================
//
// TXMICore.ReadSOP
//
//==============================================================================
function TXMICore.ReadSOP(var F: TMemoryStream): boolean;
var
  sign: array[0..7] of AnsiChar;
  DW: cardinal;
  fname: array[0..12] of AnsiChar;
  sname: array[0..30] of AnsiChar;
  B, nTracks, nInsts: byte;
  chanMode: array of byte;
  I, J: integer;
  i1name: array[0..8] of AnsiChar;
  i2name: array[0..19] of AnsiChar;
  op2inst: array[0..10] of byte;
  op4inst: array[0..21] of byte;
  evCount: word;
  dwDataSize: DWord;
  MIDIData: TMemoryStream;
begin
  Result := False;
  LogOutput('[*] Reading Sopepos'' Note file...');
  if F.Size < 76 then
  begin
    LogOutput('[-] Error: Wrong file size.');
    Exit;
  end;

  F.Seek(0, soFromBeginning);
  sign[6] := #0;
  F.ReadBuffer(sign, SizeOf(sign) - 1);
  if PAnsiChar(@sign[0]) <> 'sopepos' then
  begin
    LogOutput('[-] Error: Wrong header signature.');
    Exit;
  end;
  DW := 0;
  F.ReadBuffer(DW, 3);
  if DW <> $100 then
  begin
    LogOutput('[-] Error: Unknown file version.');
    Exit;
  end;
  SongData_PutInt('SOP_Version', DW);
  F.ReadBuffer(fname, SizeOf(fname));
  fname[Length(fname) - 1] := #0;
  F.ReadBuffer(sname, SizeOf(sname));
  sname[Length(sname) - 1] := #0;
  SongData_PutStr('SOP_FileName', PAnsiChar(@fname[0]));
  SongData_PutStr('SOP_SongName', PAnsiChar(@sname[0]));
  F.ReadBuffer(B, 1);
  SongData_PutInt('SOP_Percussive', B);
  F.Seek(1, soCurrent);
  F.ReadBuffer(B, 1);
  SongData_PutInt('SOP_TicksPerBeat', B);
  SongData_PutInt('SMPTE', 0);
  SongData_PutInt('Division', B);
  F.Seek(1, soCurrent);
  F.ReadBuffer(B, 1);
  SongData_PutInt('SOP_BeatPerMeasure', B);
  F.ReadBuffer(B, 1);
  SongData_PutInt('SOP_BasicTempo', B);
  if B = 0 then
    SongData_PutDWord('InitTempo', 60000000 div 120)
  else
    SongData_PutDWord('InitTempo', 60000000 div B);
  F.ReadBuffer(fname, SizeOf(fname));
  fname[Length(fname) - 1] := #0;
  SongData_PutStr('SOP_Comment', PAnsiChar(@fname[0]));
  F.ReadBuffer(nTracks, 1);
  F.ReadBuffer(nInsts, 1);
  F.Seek(1, soCurrent);
  // Channel modes
  SetLength(chanMode, nTracks);
  F.ReadBuffer(chanMode[0], nTracks);
  SongData_PutArray('SOP_Channels', chanMode);
  // Instruments
  for I := 0 to nInsts - 1 do
  begin
    F.ReadBuffer(B, 1);
    SongData_PutInt('SOP_Inst#' + IntToStr(I), B);
    i1name[Length(i1name) - 1] := #0;
    F.ReadBuffer(i1name, SizeOf(i1name) - 1);
    SongData_PutStr('SOP_SName#' + IntToStr(I), PAnsiChar(@i1name[0]));
    i2name[Length(i2name) - 1] := #0;
    F.ReadBuffer(i2name, SizeOf(i2name) - 1);
    SongData_PutStr('SOP_LName#' + IntToStr(I), PAnsiChar(@i2name[0]));
    if B = 12 then // comment
      Continue
    else if B = 0 then // 4OP instrument
    begin
      F.ReadBuffer(op4inst[0], SizeOf(op4inst));
      SongData_PutArray('SOP_Data#' + IntToStr(I), op4inst);
    end
    else if B <= 10 then // 2OP instrument
    begin
      F.ReadBuffer(op2inst[0], SizeOf(op2inst));
      SongData_PutArray('SOP_Data#' + IntToStr(I), op2inst);
    end;
  end;

  // Tracks
  SetLength(TrackData, nTracks + 1);
  for I := 0 to nTracks - 1 do
  begin
    LogOutput('[*] Reading track ' + IntToStr(I) + '...');
    F.ReadBuffer(evCount, 2);
    F.ReadBuffer(dwDataSize, 4);
    SetLength(TrackData[I].Data, 0);
    if I = 0 then
    begin
      TrackData[I].Title := PAnsiChar(@sname[0]);
      if TrackData[I].Title = '' then
        TrackData[I].Title := PAnsiChar(@fname[0]);
    end
    else
      TrackData[I].Title := '';
    MIDIData := TMemoryStream.Create;
    MIDIData.SetSize(dwDataSize);
    F.ReadBuffer(MIDIData.Memory^, dwDataSize);
    ReadTrackData_SOP(MIDIData, TrackData[I]);
    for J := 0 to Length(TrackData[I].Data) - 1 do
      if TrackData[I].Data[J].Status < $F0 then
        TrackData[I].Data[J].Status :=
          (TrackData[I].Data[J].Status and $F0) or (I mod 16);
    MIDIData.Free;
    LogOutput('[+] ' + IntToStr(Length(TrackData[I].Data)) + ' events found.');
    if Length(TrackData[I].Data) <> evCount then
      LogOutput('[*] Warning: Actual event count differs from stored.');
  end;
  // Control track
  I := Length(TrackData) - 1;
  LogOutput('[*] Reading track ' + IntToStr(I) + '...');
  F.ReadBuffer(evCount, 2);
  F.ReadBuffer(dwDataSize, 4);
  SetLength(TrackData[I].Data, 0);
  MIDIData := TMemoryStream.Create;
  MIDIData.SetSize(dwDataSize);
  F.ReadBuffer(MIDIData.Memory^, dwDataSize);
  ReadTrackData_SOP(MIDIData, TrackData[I]);
  MIDIData.Free;
  LogOutput('[+] ' + IntToStr(Length(TrackData[I].Data)) + ' events found.');
  if Length(TrackData[I].Data) <> evCount then
    LogOutput('[*] Warning: Actual event count differs from stored.');

  SongData_PutInt('MIDIType', 1);
  Result := True;
  LogOutput('');
  LogOutput('[*] Information:');
  LogSongInfo;
  LogOutput('');
  RefTrackList;
end;

//==============================================================================
//
// TXMICore.ReadHERAD
//
//==============================================================================
function TXMICore.ReadHERAD(var F: TMemoryStream): boolean;
const
  szSDB = 50;
  szAGD = 82;
  InstLen = 40;
var
  Header: array[0..5] of byte;
  Bits: word;
  BitsCnt: byte;
  DecCnt, DecOff: smallint;
  DecPos: integer;

  W, InstOffset, Division: word;
  I, InstCnt: integer;
  InitTempo: cardinal;
  Speed: single;
  wOffsets, wSizes: array[0..20] of word;
  Params: array[0..31] of byte;
  checkOff, isAGD, isM32, isV2: boolean;
  HERADInst: array[0..InstLen - 1] of byte;
  MIDIData: TMemoryStream;

  function GetBit(): byte;
  begin
    if BitsCnt = 0 then
    begin
      F.ReadBuffer(Bits, 2);
      BitsCnt := 16;
    end;
    Result := Bits and 1;
    Bits := Bits shr 1;
    Dec(BitsCnt);
  end;

  function SetMask: word;
  var
    I: integer;
    U, L: word;
  begin
    U := word(not 0);
    L := U;
    for I := 1 to Header[5] do
    begin
      U := U shr 1;
      L := L shl 1;
    end;
    Result := (not U) or (not L);
  end;

  function IsHSQ: boolean;
  var
    Chk: byte;
    I: integer;
  begin
    // Header[0] - word DecompSize
    // Header[1]
    // Header[2] - byte Check = 0
    // Header[3] - word CompSize
    // Header[4]
    // Header[5] - byte Checksum
    Result := False;
    if Header[2] > 0 then
    begin
      // Wrong check byte
      Exit;
    end;
    if PWord(@Header[3])^ <> F.Size then
    begin
      // Wrong compressed size
      Exit;
    end;
    Chk := 0;
    for I := 0 to 6 - 1 do
      Chk := (Chk + Header[I]) and $FF;
    if Chk <> $AB then
    begin
      // Wrong checksum
      Exit;
    end;
    Result := True;
  end;

  function IsSQX: boolean;
  begin
    // Header[0] - word OutbufInit
    // Header[1]
    // Header[2] - byte SQX flag #1
    // Header[3] - byte SQX flag #2
    // Header[4] - byte SQX flag #3
    // Header[5] - byte CntOffPart
    Result := False;
    if Header[2] > 2 then
    begin
      // Wrong SQX flag #1
      Exit;
    end;
    if Header[3] > 2 then
    begin
      // Wrong SQX flag #2
      Exit;
    end;
    if Header[4] > 2 then
    begin
      // Wrong SQX flag #2
      Exit;
    end;
    if (Header[5] = 0) or (Header[5] > 15) then
    begin
      // Wrong bit count
      Exit;
    end;
    Result := True;
  end;

  procedure DecHSQ;
  var
    O: TMemoryStream;
    B: byte;
    I: integer;
  begin
    LogOutput('[*] Decompressing HSQ data...');
    O := TMemoryStream.Create;
    BitsCnt := 0;
    DecCnt := 0;
    while O.Size < PWord(@Header[0])^ do
    begin
      if GetBit() > 0 then
      begin
        F.ReadBuffer(B, 1);
        O.WriteBuffer(B, 1);
      end
      else
      begin
        if GetBit() > 0 then
        begin
          F.ReadBuffer(DecCnt, 2);
          DecOff := (word(DecCnt) shr 3) - 8192;
          DecCnt := word(DecCnt) and 7;
          if DecCnt = 0 then
            F.ReadBuffer(DecCnt, 1);
          if DecCnt = 0 then
            Break;
        end
        else
        begin
          DecCnt := GetBit() * 2 + GetBit();
          F.ReadBuffer(B, 1);
          DecOff := B - 256;
        end;
        DecCnt := DecCnt + 2;
        DecPos := O.Position;
        for I := 0 to DecCnt - 1 do
        begin
          O.Position := DecPos + DecOff + I;
          if O.Position >= 0 then
          begin
            O.ReadBuffer(B, 1);
            O.Position := O.Size;
            O.WriteBuffer(B, 1);
          end;
        end;
      end;
    end;
    SongData_PutStr('HERAD_Compression', 'HSQ');
    F.SetSize(O.Size);
    O.Seek(0, soFromBeginning);
    O.ReadBuffer(F.Memory^, F.Size);
    F.Seek(0, soFromBeginning);
  end;

  procedure DecSQX;
  var
    O: TMemoryStream;
    Mask: word;
    Chk, ChkS, B: byte;

    procedure CopyOutStr(DecOff, DecCnt: smallint);
    var
      DecPos, OutPos, I: integer;
      B: byte;
    begin
      DecCnt := DecCnt + 2;
      DecPos := O.Position;
      OutPos := DecPos;
      for I := 0 to DecCnt - 1 do
      begin
        O.Position := DecPos + DecOff + I;
        if O.Position >= 0 then
        begin
          O.ReadBuffer(B, 1);
          O.Position := OutPos;
          O.WriteBuffer(B, 1);
          OutPos := O.Position;
        end;
      end;
    end;

  begin
    LogOutput('[*] Decompressing SQX data...');
    O := TMemoryStream.Create;
    O.SetSize(65528);
    FillChar(O.Memory^, O.Size, 0);
    O.WriteBuffer(PWord(@Header[0])^, 2);
    O.Seek(0, soFromBeginning);
    Mask := SetMask;
    Bits := 1;
    while True do
    begin
      Chk := Bits and 1;
      Bits := Bits shr 1;
      if Bits = 0 then
      begin
        ChkS := Chk;
        F.ReadBuffer(Bits, 2);
        Chk := Bits and 1;
        Bits := Bits shr 1;
        if ChkS <> 0 then
          Bits := Bits or $8000;
      end;
      if (Chk = 0) then
        case Header[2] of
          0:
          begin
            F.ReadBuffer(B, 1);
            O.WriteBuffer(B, 1);
            Continue;
          end;
          1:
          begin
            DecCnt := 0;
            Chk := Bits and 1;
            Bits := Bits shr 1;
            if Bits = 0 then
            begin
              F.ReadBuffer(Bits, 2);
              ChkS := Chk;
              Chk := Bits and 1;
              Bits := Bits shr 1;
              if ChkS <> 0 then
                Bits := Bits or $8000;
              DecCnt := Chk;
              Chk := Bits and 1;
              Bits := Bits shr 1;
            end
            else
            begin
              DecCnt := Chk;
              Chk := Bits and 1;
              Bits := Bits shr 1;
              if Bits = 0 then
              begin
                F.ReadBuffer(Bits, 2);
                ChkS := Chk;
                Chk := Bits and 1;
                Bits := Bits shr 1;
                if ChkS <> 0 then
                  Bits := Bits or $8000;
              end;
            end;
            DecCnt := (DecCnt shl 1) + Chk;
            F.ReadBuffer(B, 1);
            DecOff := B - 256;
            CopyOutStr(DecOff, DecCnt);
            Continue;
          end;
          2:
          begin
            F.ReadBuffer(DecCnt, 2);
            DecOff := (DecCnt shr Header[5]) or (Mask and $FF00);
            DecCnt := DecCnt and Lo(Mask);
            if DecCnt = 0 then
            begin
              F.ReadBuffer(B, 1);
              DecCnt := B;
              if DecCnt = 0 then
                Break;
            end;
            CopyOutStr(DecOff, DecCnt);
            Continue;
          end;
        end
      else
      begin
        Chk := Bits and 1;
        Bits := Bits shr 1;
        if Bits = 0 then
        begin
          F.ReadBuffer(Bits, 2);
          ChkS := Chk;
          Chk := Bits and 1;
          Bits := Bits shr 1;
          if ChkS <> 0 then
            Bits := Bits or $8000;
        end;
        if (Chk = 0) then
          case Header[3] of
            0:
            begin
              F.ReadBuffer(B, 1);
              O.WriteBuffer(B, 1);
              Continue;
            end;
            1:
            begin
              DecCnt := 0;
              Chk := Bits and 1;
              Bits := Bits shr 1;
              if Bits = 0 then
              begin
                F.ReadBuffer(Bits, 2);
                ChkS := Chk;
                Chk := Bits and 1;
                Bits := Bits shr 1;
                if ChkS <> 0 then
                  Bits := Bits or $8000;
                DecCnt := Chk;
                Chk := Bits and 1;
                Bits := Bits shr 1;
              end
              else
              begin
                DecCnt := Chk;
                Chk := Bits and 1;
                Bits := Bits shr 1;
                if Bits = 0 then
                begin
                  F.ReadBuffer(Bits, 2);
                  ChkS := Chk;
                  Chk := Bits and 1;
                  Bits := Bits shr 1;
                  if ChkS <> 0 then
                    Bits := Bits or $8000;
                end;
              end;
              DecCnt := (DecCnt shl 1) + Chk;
              F.ReadBuffer(B, 1);
              DecOff := B - 256;
              CopyOutStr(DecOff, DecCnt);
              Continue;
            end;
            2:
            begin
              F.ReadBuffer(DecCnt, 2);
              DecOff := (DecCnt shr Header[5]) or (Mask and $FF00);
              DecCnt := DecCnt and Lo(Mask);
              if DecCnt = 0 then
              begin
                F.ReadBuffer(B, 1);
                DecCnt := B;
                if DecCnt = 0 then
                  Break;
              end;
              CopyOutStr(DecOff, DecCnt);
              Continue;
            end;
          end
        else
          case Header[4] of
            0:
            begin
              F.ReadBuffer(B, 1);
              O.WriteBuffer(B, 1);
              Continue;
            end;
            1:
            begin
              DecCnt := 0;
              Chk := Bits and 1;
              Bits := Bits shr 1;
              if Bits = 0 then
              begin
                F.ReadBuffer(Bits, 2);
                ChkS := Chk;
                Chk := Bits and 1;
                Bits := Bits shr 1;
                if ChkS <> 0 then
                  Bits := Bits or $8000;
                DecCnt := Chk;
                Chk := Bits and 1;
                Bits := Bits shr 1;
              end
              else
              begin
                DecCnt := Chk;
                Chk := Bits and 1;
                Bits := Bits shr 1;
                if Bits = 0 then
                begin
                  F.ReadBuffer(Bits, 2);
                  ChkS := Chk;
                  Chk := Bits and 1;
                  Bits := Bits shr 1;
                  if ChkS <> 0 then
                    Bits := Bits or $8000;
                end;
              end;
              DecCnt := (DecCnt shl 1) + Chk;
              F.ReadBuffer(B, 1);
              DecOff := B - 256;
              CopyOutStr(DecOff, DecCnt);
              Continue;
            end;
            2:
            begin
              F.ReadBuffer(DecCnt, 2);
              DecOff := (DecCnt shr Header[5]) or (Mask and $FF00);
              DecCnt := DecCnt and Lo(Mask);
              if DecCnt = 0 then
              begin
                F.ReadBuffer(B, 1);
                DecCnt := B;
                if DecCnt = 0 then
                  Break;
              end;
              CopyOutStr(DecOff, DecCnt);
              Continue;
            end;
          end;
      end;
    end;
    SongData_PutStr('HERAD_Compression', 'SQX');
    O.SetSize(O.Position);
    F.SetSize(O.Size);
    O.Seek(0, soFromBeginning);
    O.ReadBuffer(F.Memory^, F.Size);
    F.Seek(0, soFromBeginning);
  end;

begin
  Result := False;
  LogOutput('[*] Reading Cryo HERAD music file...');

  if F.Size < 2 + szSDB then
  begin
    LogOutput('[-] Error: Wrong file size.');
    Exit;
  end;

  // Try to decompress data
  F.Seek(0, soFromBeginning);
  F.ReadBuffer(Header, 6);
  if IsHSQ then
    DecHSQ
  else
  if IsSQX then
    DecSQX;
  F.Seek(0, soFromBeginning);
  F.ReadBuffer(InstOffset, 2); // Instruments offset
  if (InstOffset = 0) or (F.Size < InstOffset) then
  begin
    LogOutput('[-] Error: Incorrect offset / file size.');
    Exit;
  end;
  InstCnt := (F.Size - InstOffset) div InstLen;
  checkOff := False;
  isAGD := False;
  for I := 0 to Length(wOffsets) - 1 do
  begin // Track offsets
    F.ReadBuffer(wOffsets[I], 2);
    if wOffsets[I] = 0 then
      checkOff := True
    else
    begin
      if checkOff then
      begin
        LogOutput('[-] Error: Unexpected track offset after last track.');
        Exit;
      end;
      if (wOffsets[I] < szSDB) or (isAGD and (wOffsets[I] < szAGD)) or
        (wOffsets[I] >= F.Size) then
      begin
        LogOutput('[-] Error: Wrong track offset.');
        Exit;
      end;
    end;
    if I = 0 then
      case wOffsets[I] of
        szSDB: ; // SDB, M32
        szAGD:   // AGD
          isAGD := True;
        else
        begin
          LogOutput('[-] Error: Wrong first track offset.');
          Exit;
        end;
      end;
  end;
  for I := 0 to Length(wSizes) - 2 do
    if wOffsets[I + 1] > 0 then
      wSizes[I] := wOffsets[I + 1] - wOffsets[I]
    else
    begin
      wSizes[I] := InstOffset - 2 - wOffsets[I];
      Break;
    end;
  if isAGD and (F.Size < 2 + szAGD) then
  begin
    LogOutput('[-] Error: Wrong file size.');
    Exit;
  end;
  F.ReadBuffer(W, 2);
  SongData_PutInt('HERAD_LoopStart', W);
  F.ReadBuffer(W, 2);
  SongData_PutInt('HERAD_LoopEnd', W);
  F.ReadBuffer(W, 2);
  SongData_PutInt('HERAD_LoopCount', W);
  F.ReadBuffer(W, 2); // Speed int / frac
  Speed := (W shr 8) + (W and $FF / 256);
  SongData_PutFloat('HERAD_Speed', Speed);
  Division := 24;
  InitTempo := Round(Speed * MIDIStdTempo * 48 / 203);
  SongData_PutDWord('InitTempo', InitTempo);
  SongData_PutInt('SMPTE', 0);
  SongData_PutInt('Division', Division);
  if isAGD then
  begin
    F.ReadBuffer(Params, SizeOf(Params));
    SongData_PutArray('HERAD_Params', Params);
  end;

  isM32 := InstCnt = 0;
  isV2 := not isM32;
  F.Seek(InstOffset, soFromBeginning);
  for I := 0 to InstCnt - 1 do
  begin
    F.ReadBuffer(HERADInst, SizeOf(HERADInst));
    if HERADInst[0] = 0 then
      isV2 := False;
    SongData_PutArray('HERAD_Inst#' + IntToStr(I), HERADInst);
  end;
  SongData_PutInt('HERAD_V2', byte(isV2));

  isM32 := InstCnt = 0;
  if isM32 then
    SongData_PutInt('MIDIType', 0)
  else
    SongData_PutInt('MIDIType', 1);

  SetLength(TrackData, 0);
  for I := 0 to Length(wOffsets) - 1 do
  begin
    if wOffsets[I] = 0 then
      Break;
    LogOutput('[*] Reading track at offset ' + IntToStr(wOffsets[I]) + '.');
    SetLength(TrackData, I + 1);
    TrackData[I].Title := '';
    MIDIData := TMemoryStream.Create;
    MIDIData.SetSize(wSizes[I]);
    F.Seek(2 + wOffsets[I], soFromBeginning);
    F.ReadBuffer(MIDIData.Memory^, MIDIData.Size);
    SongData_PutInt('HERAD_TmpIdx', I);
    ReadTrackData_HERAD(MIDIData, TrackData[I]);
    MIDIData.Free;
    LogOutput('[+] ' + IntToStr(Length(TrackData[I].Data)) + ' events found.');
  end;
  SongData_Delete('HERAD_TmpIdx');

  if Length(TrackData) > 1 then
    SongData_PutInt('MIDIType', 1);

  Result := True;
  LogOutput('');
  LogOutput('[*] Information:');
  LogSongInfo;
  LogOutput('');
  RefTrackList;
end;

//==============================================================================
//
// TXMICore.ReadRaw
//
//==============================================================================
function TXMICore.ReadRaw(var F: TMemoryStream): boolean;
begin
  LogOutput('[*] Reading raw MIDI data...');
  SetLength(TrackData, 1);
  TrackData[0].Title := '';
  SetLength(TrackData[0].Data, 0);

  F.Seek(0, soFromBeginning);
  if EventFormat = 'mid' then
    ReadTrackData(F, TrackData[0]);

  LogOutput('[+] ' + IntToStr(Length(TrackData[0].Data)) + ' events found.');
  SongData_PutInt('MIDIType', 1);
  SongData_PutInt('SMPTE', 0);
  SongData_PutInt('Division', 96);
  SongData_PutDWord('InitTempo', MIDIStdTempo);
  Result := True;
  LogOutput('');
  LogOutput('[*] Default settings:');
  LogSongInfo;
  LogOutput('');
  RefTrackList;
end;

//==============================================================================
//
// TXMICore.ReadSYX
//
//==============================================================================
function TXMICore.ReadSYX(var F: TMemoryStream): boolean;
begin
  LogOutput('[*] Reading System Exclusive data...');
  SetLength(TrackData, 1);
  TrackData[0].Title := '';
  SetLength(TrackData[0].Data, 0);

  F.Seek(0, soFromBeginning);
  ReadTrackData_SYX(F, TrackData[0]);

  LogOutput('[+] ' + IntToStr(Length(TrackData[0].Data)) + ' events found.');
  SongData_PutInt('MIDIType', 1);
  SongData_PutInt('SMPTE', 0);
  SongData_PutInt('Division', 96);
  SongData_PutDWord('InitTempo', MIDIStdTempo);
  Result := True;
  LogOutput('');
  LogOutput('[*] Default settings:');
  LogSongInfo;
  LogOutput('');
  RefTrackList;
end;

//==============================================================================
//
// TXMICore.ReadTrackData
//
//==============================================================================
procedure TXMICore.ReadTrackData(var F: TMemoryStream; var Trk: Chunk);
var
  J: integer;
  Buf: Pointer;
  B: byte;
  ReadTicks, StatByte: boolean;
  Ticks: UInt64;
  Err: byte;

  procedure ReadBuf(var Buf; Size: cardinal);
  begin
    try
      F.ReadBuffer(Buf, Size);
    except
      LogOutput('[-] Error: Incomplete event (end of track reached).');
    end;
  end;

begin
  StatByte := False;
  Ticks := 0;
  ReadTicks := True;
  while F.Position < F.Size do
  begin
    if ReadTicks then
    begin
      // Reading ticks count
      Ticks := ReadVarVal(F, Err);
      if Err = 1 then
      begin
        LogOutput('[-] Error: Ticks count overflow at offset ' +
          IntToStr(F.Position) + '.');
        Continue;
      end;
      if Err = 2 then
      begin
        LogOutput('[-] Error: Incomplete event (end of track reached).');
        Break;
      end;
    end;
    // Reading status byte
    ReadBuf(B, 1);
    if (B < 128) and not StatByte then
    begin
      LogOutput('[-] Error: Status byte expected at offset ' +
        IntToStr(F.Position - 1) + '.');
      ReadTicks := False;
      Continue;
    end;
    ReadTicks := True;
    // Adding event
    SetLength(Trk.Data, Length(Trk.Data) + 1);
    // Initializing structure
    Trk.Data[Length(Trk.Data) - 1].Status := 0;
    Trk.Data[Length(Trk.Data) - 1].BParm1 := 0;
    Trk.Data[Length(Trk.Data) - 1].BParm2 := 0;
    Trk.Data[Length(Trk.Data) - 1].Value := 0;
    Trk.Data[Length(Trk.Data) - 1].Len := 0;
    Trk.Data[Length(Trk.Data) - 1].RunStatMode := False;
    Trk.Data[Length(Trk.Data) - 1].Ticks := Ticks;
    if B >= 128 then
    begin
      // Status byte found
      Trk.Data[Length(Trk.Data) - 1].Status := B;
      StatByte := True;
    end;
    if StatByte and (B < 128) then
    begin
      // New status byte not found, but already set
      Trk.Data[Length(Trk.Data) - 1].Status :=
        Trk.Data[Length(Trk.Data) - 2].Status;
      Trk.Data[Length(Trk.Data) - 1].RunStatMode := True;
      B := Trk.Data[Length(Trk.Data) - 1].Status;
      F.Seek(F.Position - 1, 0);
    end;
    case B shr 4 of
      8..11:
      begin // NoteOff, NoteOn, PolyAfterTouch, Control (2 params)
        ReadBuf(B, 1);
        if B > 127 then
        begin
          LogOutput('[-] Error: Parameter greater than 127 at offset ' +
            IntToStr(F.Position - 1) + '.');
          B := 127;
        end;
        Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
        ReadBuf(B, 1);
        if B > 127 then
        begin
          LogOutput('[-] Error: Parameter greater than 127 at offset ' +
            IntToStr(F.Position - 1) + '.');
          B := 127;
        end;
        Trk.Data[Length(Trk.Data) - 1].BParm2 := B;
      end;
      12..13:
      begin // ProgramChange, ChanAfterTouch (1 param)
        ReadBuf(B, 1);
        if B > 127 then
        begin
          LogOutput('[-] Error: Parameter greater than 127 at offset ' +
            IntToStr(F.Position - 1) + '.');
          B := 127;
        end;
        Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
      end;
      14:
      begin // Pitch Bend (1 param / 2 bytes)
        ReadBuf(B, 1);
        if B > 127 then
        begin
          LogOutput('[-] Error: Parameter greater than 127 at offset ' +
            IntToStr(F.Position - 1) + '.');
          B := 127;
        end;
        Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
        ReadBuf(B, 1);
        if B > 127 then
        begin
          LogOutput('[-] Error: Parameter greater than 127 at offset ' +
            IntToStr(F.Position - 1) + '.');
          B := 127;
        end;
        Trk.Data[Length(Trk.Data) - 1].BParm2 := B;
        Trk.Data[Length(Trk.Data) - 1].Value :=
          Trk.Data[Length(Trk.Data) - 1].BParm1 +
          Trk.Data[Length(Trk.Data) - 1].BParm2 * 128;
      end;
      15:
      begin // System
        case B and 15 of
          0, 7:
          begin // SysEx and EOX
            Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
            if Err = 2 then
            begin
              LogOutput('[-] Error: Incomplete event (end of track reached).');
              SetLength(Trk.Data, Length(Trk.Data) - 1);
              LogOutput('[+] Fixed.');
              Break;
            end;
            if Err = 1 then
            begin
              LogOutput('[-] Error: Length overflow at offset ' +
                IntToStr(F.Position) + '.');
              Trk.Data[Length(Trk.Data) - 1].Len := 0;
            end
            else
              for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
              begin
                ReadBuf(B, 1);
                if F.Position >= F.Size then
                begin
                  Trk.Data[Length(Trk.Data) - 1].Len :=
                    Length(Trk.Data[Length(Trk.Data) - 1].DataArray);
                  LogOutput(
                    '[-] Error: Incomplete event data (end of track reached).');
                  Break;
                end;
                SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray,
                  Length(Trk.Data[Length(Trk.Data) - 1].DataArray) + 1);
                Trk.Data[Length(Trk.Data) - 1].DataArray[Length(
                  Trk.Data[Length(Trk.Data) - 1].DataArray) - 1] := B;
              end;
          end;
          1:
          begin // QuarterFrame
            ReadBuf(B, 1);
            if B > 127 then
            begin
              LogOutput('[-] Error: Parameter greater than 127 at offset ' +
                IntToStr(F.Position - 1) + '.');
              B := 127;
            end;
            Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
          end;
          2:
          begin // PSongPos
            ReadBuf(B, 1);
            if B > 127 then
            begin
              LogOutput('[-] Error: Parameter greater than 127 at offset ' +
                IntToStr(F.Position - 1) + '.');
              B := 127;
            end;
            Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
            ReadBuf(B, 1);
            if B > 127 then
            begin
              LogOutput('[-] Error: Parameter greater than 127 at offset ' +
                IntToStr(F.Position - 1) + '.');
              B := 127;
            end;
            Trk.Data[Length(Trk.Data) - 1].BParm2 := B;
            Trk.Data[Length(Trk.Data) - 1].Value :=
              Trk.Data[Length(Trk.Data) - 1].BParm1 * 128 +
              Trk.Data[Length(Trk.Data) - 1].BParm2;
          end;
          3:
          begin // SongSelect
            ReadBuf(B, 1);
            if B > 127 then
            begin
              LogOutput('[-] Error: Parameter greater than 127 at offset ' +
                IntToStr(F.Position - 1) + '.');
              B := 127;
            end;
            Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
          end;
          6: ; // TuneRequest
          8: ; // TimingClock
          10: ; // Start
          11: ; // Continue
          12: ; // Stop
          14: ; // ActiveSens
          15:
          begin // MetaEvnt
            ReadBuf(B, 1);
            if B > 127 then
            begin
              LogOutput('[-] Error: Parameter greater than 127 at offset ' +
                IntToStr(F.Position - 1) + '.');
              B := 127;
            end;
            Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
            case B of
              0:
              begin  // Track Number
                Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                if Err = 2 then
                begin
                  LogOutput('[-] Error: Incomplete event (end of track reached).');
                  SetLength(Trk.Data, Length(Trk.Data) - 1);
                  LogOutput('[+] Fixed.');
                  Break;
                end;
                if Err = 1 then
                begin
                  LogOutput('[-] Error: Length overflow at offset ' +
                    IntToStr(F.Position) + '.');
                  Trk.Data[Length(Trk.Data) - 1].Len := 0;
                end
                else
                  for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                  begin
                    ReadBuf(B, 1);
                    if J > 0 then
                      Trk.Data[Length(Trk.Data) - 1].Value :=
                        Trk.Data[Length(Trk.Data) - 1].Value shl 8;
                    Trk.Data[Length(Trk.Data) - 1].Value :=
                      Trk.Data[Length(Trk.Data) - 1].Value or B;
                  end;
              end;
              1..7:
              begin  // Text, Copyright, TrackName, InstrName, Lyrics, Marker, CuePoint
                Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                if Err = 2 then
                begin
                  LogOutput('[-] Error: Incomplete event (end of track reached).');
                  SetLength(Trk.Data, Length(Trk.Data) - 1);
                  LogOutput('[+] Fixed.');
                  Break;
                end;
                if Err = 1 then
                begin
                  LogOutput('[-] Error: Length overflow at offset ' +
                    IntToStr(F.Position) + '.');
                  Trk.Data[Length(Trk.Data) - 1].Len := 0;
                end
                else
                begin
                  Buf := AllocMem(Trk.Data[Length(Trk.Data) - 1].Len + 1);
                  ZeroMemory(Buf, Trk.Data[Length(Trk.Data) - 1].Len + 1);
                  ReadBuf(Buf^, Trk.Data[Length(Trk.Data) - 1].Len);
                  Trk.Data[Length(Trk.Data) - 1].DataString := PAnsiChar(Buf);
                  FreeMem(Buf, Trk.Data[Length(Trk.Data) - 1].Len + 1);
                end;
                if Trk.Data[Length(Trk.Data) - 1].BParm1 = 3 then
                  if (Trk.Title = '') and
                    (Trk.Data[Length(Trk.Data) - 1].DataString <> '') then
                    Trk.Title := Trk.Data[Length(Trk.Data) - 1].DataString;
              end;
              32:
              begin // MIDI Channel
                Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                if Err = 2 then
                begin
                  LogOutput('[-] Error: Incomplete event (end of track reached).');
                  SetLength(Trk.Data, Length(Trk.Data) - 1);
                  LogOutput('[+] Fixed.');
                  Break;
                end;
                if Err = 1 then
                begin
                  LogOutput('[-] Error: Length overflow at offset ' +
                    IntToStr(F.Position) + '.');
                  Trk.Data[Length(Trk.Data) - 1].Len := 0;
                end
                else
                begin
                  for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                  begin
                    ReadBuf(B, 1);
                    if J > 0 then
                      Trk.Data[Length(Trk.Data) - 1].Value :=
                        Trk.Data[Length(Trk.Data) - 1].Value shl 8;
                    Trk.Data[Length(Trk.Data) - 1].Value :=
                      Trk.Data[Length(Trk.Data) - 1].Value or B;
                  end;
                  if not (Trk.Data[Length(Trk.Data) - 1].Value in [0..15]) then
                  begin
                    LogOutput('[-] Error: MIDI Channel is wrong at offset ' +
                      IntToStr(F.Position - Trk.Data[Length(Trk.Data) - 1].Len) + '.');
                    Trk.Data[Length(Trk.Data) - 1].Len := 1;
                    Trk.Data[Length(Trk.Data) - 1].Value := 0;
                    LogOutput('[+] Fixed.');
                  end;
                end;
              end;
              33:
              begin // MIDI Port
                Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                if Err = 2 then
                begin
                  LogOutput('[-] Error: Incomplete event (end of track reached).');
                  SetLength(Trk.Data, Length(Trk.Data) - 1);
                  LogOutput('[+] Fixed.');
                  Break;
                end;
                if Err = 1 then
                begin
                  LogOutput('[-] Error: Length overflow at offset ' +
                    IntToStr(F.Position) + '.');
                  Trk.Data[Length(Trk.Data) - 1].Len := 0;
                end
                else
                  for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                  begin
                    ReadBuf(B, 1);
                    if J > 0 then
                      Trk.Data[Length(Trk.Data) - 1].Value :=
                        Trk.Data[Length(Trk.Data) - 1].Value shl 8;
                    Trk.Data[Length(Trk.Data) - 1].Value :=
                      Trk.Data[Length(Trk.Data) - 1].Value or B;
                  end;
              end;
              81:
              begin  // Tempo
                Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                if Err = 2 then
                begin
                  LogOutput('[-] Error: Incomplete event (end of track reached).');
                  SetLength(Trk.Data, Length(Trk.Data) - 1);
                  LogOutput('[+] Fixed.');
                  Break;
                end;
                if Err = 1 then
                begin
                  LogOutput('[-] Error: Length overflow at offset ' +
                    IntToStr(F.Position) + '.');
                  Trk.Data[Length(Trk.Data) - 1].Len := 0;
                end
                else
                  for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                  begin
                    ReadBuf(B, 1);
                    if J > 0 then
                      Trk.Data[Length(Trk.Data) - 1].Value :=
                        Trk.Data[Length(Trk.Data) - 1].Value shl 8;
                    Trk.Data[Length(Trk.Data) - 1].Value :=
                      Trk.Data[Length(Trk.Data) - 1].Value or B;
                  end;
              end;
              84:
              begin  // SMPTE Offset
                Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                if Err = 2 then
                begin
                  LogOutput('[-] Error: Incomplete event (end of track reached).');
                  SetLength(Trk.Data, Length(Trk.Data) - 1);
                  LogOutput('[+] Fixed.');
                  Break;
                end;
                if Err = 1 then
                begin
                  LogOutput('[-] Error: Length overflow at offset ' +
                    IntToStr(F.Position) + '.');
                  Trk.Data[Length(Trk.Data) - 1].Len := 0;
                end
                else
                begin
                  if Trk.Data[Length(Trk.Data) - 1].Len <> 5 then
                    LogOutput(
                      '[-] Error: SMPTE Offset is wrong formatted at offset ' + IntToStr(
                      F.Position - Trk.Data[Length(Trk.Data) - 1].Len) + '.');
                  for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                  begin
                    SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray,
                      Length(Trk.Data[Length(Trk.Data) - 1].DataArray) + 1);
                    ReadBuf(B, 1);
                    Trk.Data[Length(Trk.Data) - 1].DataArray[Length(
                      Trk.Data[Length(Trk.Data) - 1].DataArray) - 1] := B;
                  end;
                  if Trk.Data[Length(Trk.Data) - 1].Len < 5 then
                  begin
                    SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray, 5);
                    for J := Trk.Data[Length(Trk.Data) - 1].Len to 4 do
                      Trk.Data[Length(Trk.Data) - 1].DataArray[J] := 0;
                    Trk.Data[Length(Trk.Data) - 1].Len := 5;
                    LogOutput('[+] Fixed.');
                  end;
                  if Trk.Data[Length(Trk.Data) - 1].Len > 5 then
                  begin
                    SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray, 5);
                    Trk.Data[Length(Trk.Data) - 1].Len := 5;
                    LogOutput('[+] Fixed.');
                  end;
                end;
              end;
              88:
              begin  // TimeSignature
                Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                if Err = 2 then
                begin
                  LogOutput('[-] Error: Incomplete event (end of track reached).');
                  SetLength(Trk.Data, Length(Trk.Data) - 1);
                  LogOutput('[+] Fixed.');
                  Break;
                end;
                if Err = 1 then
                begin
                  LogOutput('[-] Error: Length overflow at offset ' +
                    IntToStr(F.Position) + '.');
                  Trk.Data[Length(Trk.Data) - 1].Len := 0;
                end
                else
                begin
                  if Trk.Data[Length(Trk.Data) - 1].Len <> 4 then
                    LogOutput(
                      '[-] Error: Time Signature is wrong formatted at offset ' + IntToStr(
                      F.Position - Trk.Data[Length(Trk.Data) - 1].Len) + '.');
                  for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                  begin
                    SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray,
                      Length(Trk.Data[Length(Trk.Data) - 1].DataArray) + 1);
                    ReadBuf(B, 1);
                    Trk.Data[Length(Trk.Data) - 1].DataArray[Length(
                      Trk.Data[Length(Trk.Data) - 1].DataArray) - 1] := B;
                  end;
                  if Trk.Data[Length(Trk.Data) - 1].Len < 4 then
                  begin
                    SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray, 4);
                    for J := Trk.Data[Length(Trk.Data) - 1].Len to 3 do
                      Trk.Data[Length(Trk.Data) - 1].DataArray[J] := 0;
                    Trk.Data[Length(Trk.Data) - 1].Len := 4;
                    LogOutput('[+] Fixed.');
                  end;
                  if Trk.Data[Length(Trk.Data) - 1].Len > 4 then
                  begin
                    SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray, 4);
                    Trk.Data[Length(Trk.Data) - 1].Len := 4;
                    LogOutput('[+] Fixed.');
                  end;
                end;
              end;
              89:
              begin  // KeySignature
                Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                if Err = 2 then
                begin
                  LogOutput('[-] Error: Incomplete event (end of track reached).');
                  SetLength(Trk.Data, Length(Trk.Data) - 1);
                  LogOutput('[+] Fixed.');
                  Break;
                end;
                if Err = 1 then
                begin
                  LogOutput('[-] Error: Length overflow at offset ' +
                    IntToStr(F.Position) + '.');
                  Trk.Data[Length(Trk.Data) - 1].Len := 0;
                end
                else
                begin
                  if Trk.Data[Length(Trk.Data) - 1].Len <> 2 then
                    LogOutput(
                      '[-] Error: Key Signature is wrong formatted at offset ' + IntToStr(
                      F.Position - Trk.Data[Length(Trk.Data) - 1].Len) + '.');
                  for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                  begin
                    SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray,
                      Length(Trk.Data[Length(Trk.Data) - 1].DataArray) + 1);
                    ReadBuf(B, 1);
                    Trk.Data[Length(Trk.Data) - 1].DataArray[Length(
                      Trk.Data[Length(Trk.Data) - 1].DataArray) - 1] := B;
                  end;
                  if Trk.Data[Length(Trk.Data) - 1].Len < 2 then
                  begin
                    SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray, 2);
                    for J := Trk.Data[Length(Trk.Data) - 1].Len to 1 do
                      Trk.Data[Length(Trk.Data) - 1].DataArray[J] := 0;
                    Trk.Data[Length(Trk.Data) - 1].Len := 2;
                    LogOutput('[+] Fixed.');
                  end;
                  if Trk.Data[Length(Trk.Data) - 1].Len > 2 then
                  begin
                    SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray, 2);
                    Trk.Data[Length(Trk.Data) - 1].Len := 2;
                    LogOutput('[+] Fixed.');
                  end;
                end;
              end;
              else
              begin
                // Custom meta event
                Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                if Err = 2 then
                begin
                  LogOutput('[-] Error: Incomplete event (end of track reached).');
                  SetLength(Trk.Data, Length(Trk.Data) - 1);
                  LogOutput('[+] Fixed.');
                  Break;
                end;
                if Err = 1 then
                begin
                  LogOutput('[-] Error: Length overflow at offset ' +
                    IntToStr(F.Position) + '.');
                  Trk.Data[Length(Trk.Data) - 1].Len := 0;
                end
                else
                  for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                  begin
                    ReadBuf(B, 1);
                    if F.Position >= F.Size then
                    begin
                      Trk.Data[Length(Trk.Data) - 1].Len :=
                        Length(Trk.Data[Length(Trk.Data) - 1].DataArray);
                      LogOutput(
                        '[-] Error: Incomplete event data (end of track reached).');
                      Break;
                    end;
                    SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray,
                      Length(Trk.Data[Length(Trk.Data) - 1].DataArray) + 1);
                    Trk.Data[Length(Trk.Data) - 1].DataArray[Length(
                      Trk.Data[Length(Trk.Data) - 1].DataArray) - 1] := B;
                  end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

//==============================================================================
//
// TXMICore.ReadTrackData_SYX
//
//==============================================================================
procedure TXMICore.ReadTrackData_SYX(var F: TMemoryStream; var Trk: Chunk);
var
  B: byte;
  Idx: integer;

  procedure ReadBuf(var Buf; Size: cardinal);
  begin
    try
      F.ReadBuffer(Buf, Size);
    except
      LogOutput('[-] Error: Incomplete data (end of file reached).');
    end;
  end;

begin
  while F.Position < F.Size do
  begin
    ReadBuf(B, 1);
    if B <> $F0 then
      Continue;
    // Adding event
    SetLength(Trk.Data, Length(Trk.Data) + 1);
    // Initializing variables
    Idx := Length(Trk.Data) - 1;
    Trk.Data[Idx].Status := B;
    Trk.Data[Idx].BParm1 := 0;
    Trk.Data[Idx].BParm2 := 0;
    Trk.Data[Idx].Value := 0;
    Trk.Data[Idx].Len := 0;
    Trk.Data[Idx].RunStatMode := False;
    while B <> $F7 do
    begin
      if F.Position >= F.Size then
        B := $F7
      else
        ReadBuf(B, 1);
      Inc(Trk.Data[Idx].Len);
      SetLength(Trk.Data[Idx].DataArray, Trk.Data[Idx].Len);
      Trk.Data[Idx].DataArray[Trk.Data[Idx].Len - 1] := B;
    end;
  end;
end;

//==============================================================================
//
// TXMICore.ReadTrackData_MIDS
//
//==============================================================================
procedure TXMICore.ReadTrackData_MIDS(var F: TMemoryStream; var Trk: Chunk);
var
  Sz, DW: LongWord;
  Tick: boolean;
  I: integer;

  procedure ReadBuf(var Buf; Size: cardinal);
  begin
    try
      F.ReadBuffer(Buf, Size);
    except
      LogOutput('[-] Error: Incomplete event (end of track reached).');
    end;
  end;

begin
  I := 0;
  F.Seek(4, soCurrent);
  while F.Position < F.Size do
  begin
    // Reading block size
    F.Seek(4, soCurrent);
    ReadBuf(Sz, 4);
    // Reading block
    Tick := True;
    while Sz > 0 do
    begin
      ReadBuf(DW, 4);
      if Tick then
      begin
        // Adding event
        I := Length(Trk.Data);
        SetLength(Trk.Data, I + 1);
        // Reading ticks
        Trk.Data[I].Ticks := DW;
        // Initializing variables
        Trk.Data[I].Status := 0;
        Trk.Data[I].BParm1 := 0;
        Trk.Data[I].BParm2 := 0;
        Trk.Data[I].Value := 0;
        Trk.Data[I].Len := 0;
        Trk.Data[I].RunStatMode := False;
      end
      else
      begin
        // Reading event
        case DW shr 24 of
          0:
          begin // MIDI event
            Trk.Data[I].Status := DW and $FF;
            case Trk.Data[I].Status shr 4 of
              8, 9, 10, 11, 14:
              begin
                Trk.Data[I].BParm1 := DW shr 8 and $FF;
                Trk.Data[I].BParm2 := DW shr 16 and $FF;
              end;
              12, 13:
              begin
                Trk.Data[I].BParm1 := DW shr 8 and $FF;
              end;
            end;
          end;
          1:
          begin // Tempo change
            Trk.Data[I].Status := $FF;
            Trk.Data[I].BParm1 := $51;
            Trk.Data[I].Len := 3;
            Trk.Data[I].Value := DW and $FFFFFF;
            SetLength(Trk.Data[I].DataArray, 3);
            Trk.Data[I].DataArray[0] := DW shr 16 and $FF;
            Trk.Data[I].DataArray[1] := DW shr 8 and $FF;
            Trk.Data[I].DataArray[2] := DW and $FF;
          end;
        end;
      end;
      Tick := not Tick;
      Dec(Sz, 4);
    end;
  end;
end;

//==============================================================================
//
// TXMICore.ReadTrackData_XMI
//
//==============================================================================
procedure TXMICore.ReadTrackData_XMI(var F: TMemoryStream; var Trk: Chunk);
var
  J: integer;
  Buf: Pointer;
  B: byte;
  Ticks: UInt64;
  Err: byte;

  procedure ReadBuf(var Buf; Size: cardinal);
  begin
    try
      F.ReadBuffer(Buf, Size);
    except
      LogOutput('[-] Error: Incomplete event (end of track reached).');
    end;
  end;

begin
  while F.Position < F.Size do
  begin
    // Reading ticks count
    Ticks := ReadVarVal_XMI(F, Err);
    if Err = 1 then
    begin
      LogOutput('[-] Error: Ticks count overflow at offset ' +
        IntToStr(F.Position) + '.');
      Continue;
    end;
    if Err = 2 then
    begin
      LogOutput('[-] Error: Incomplete event (end of track reached).');
      Break;
    end;
    if F.Position >= F.Size then
      Break;
    // Reading status byte
    ReadBuf(B, 1);
    // Adding event
    SetLength(Trk.Data, Length(Trk.Data) + 1);
    // Initializing structure
    Trk.Data[Length(Trk.Data) - 1].Status := B;
    Trk.Data[Length(Trk.Data) - 1].BParm1 := 0;
    Trk.Data[Length(Trk.Data) - 1].BParm2 := 0;
    Trk.Data[Length(Trk.Data) - 1].Value := 0;
    Trk.Data[Length(Trk.Data) - 1].Len := 0;
    Trk.Data[Length(Trk.Data) - 1].RunStatMode := False;
    Trk.Data[Length(Trk.Data) - 1].Ticks := Ticks;
    case B shr 4 of
      8:
      begin // NoteOff (unused in XMI)
        Trk.Data[Length(Trk.Data) - 1].BParm1 := 0;
        Trk.Data[Length(Trk.Data) - 1].BParm2 := 0;
      end;
      9:
      begin // XMI Note (3 params)
        ReadBuf(B, 1);
        if B > 127 then
        begin
          LogOutput('[-] Error: Parameter greater than 127 at offset ' +
            IntToStr(F.Position - 1) + '.');
          B := 127;
        end;
        Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
        ReadBuf(B, 1);
        if B > 127 then
        begin
          LogOutput('[-] Error: Parameter greater than 127 at offset ' +
            IntToStr(F.Position - 1) + '.');
          B := 127;
        end;
        Trk.Data[Length(Trk.Data) - 1].BParm2 := B;
        // Read note duration
        Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
        if Err = 2 then
        begin
          LogOutput('[-] Error: Incomplete event (end of track reached).');
          SetLength(Trk.Data, Length(Trk.Data) - 1);
          LogOutput('[+] Fixed.');
          Break;
        end;
        if Err = 1 then
        begin
          LogOutput('[-] Error: Length overflow at offset ' +
            IntToStr(F.Position) + '.');
          Trk.Data[Length(Trk.Data) - 1].Len := 0;
        end;
      end;
      10..11:
      begin // PolyAfterTouch, Control (2 params)
        ReadBuf(B, 1);
        if B > 127 then
        begin
          LogOutput('[-] Error: Parameter greater than 127 at offset ' +
            IntToStr(F.Position - 1) + '.');
          B := 127;
        end;
        Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
        ReadBuf(B, 1);
        if B > 127 then
        begin
          LogOutput('[-] Error: Parameter greater than 127 at offset ' +
            IntToStr(F.Position - 1) + '.');
          B := 127;
        end;
        Trk.Data[Length(Trk.Data) - 1].BParm2 := B;
      end;
      12..13:
      begin // ProgramChange, ChanAfterTouch (1 param)
        ReadBuf(B, 1);
        if B > 127 then
        begin
          LogOutput('[-] Error: Parameter greater than 127 at offset ' +
            IntToStr(F.Position - 1) + '.');
          B := 127;
        end;
        Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
      end;
      14:
      begin // Pitch Bend (1 param / 2 bytes)
        ReadBuf(B, 1);
        if B > 127 then
        begin
          LogOutput('[-] Error: Parameter greater than 127 at offset ' +
            IntToStr(F.Position - 1) + '.');
          B := 127;
        end;
        Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
        ReadBuf(B, 1);
        if B > 127 then
        begin
          LogOutput('[-] Error: Parameter greater than 127 at offset ' +
            IntToStr(F.Position - 1) + '.');
          B := 127;
        end;
        Trk.Data[Length(Trk.Data) - 1].BParm2 := B;
        Trk.Data[Length(Trk.Data) - 1].Value :=
          Trk.Data[Length(Trk.Data) - 1].BParm1 +
          Trk.Data[Length(Trk.Data) - 1].BParm2 * 128;
      end;
      15:
      begin // System
        case B and 15 of
          0:
          begin // SysEx
            Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
            if Err = 2 then
            begin
              LogOutput('[-] Error: Incomplete event (end of track reached).');
              SetLength(Trk.Data, Length(Trk.Data) - 1);
              LogOutput('[+] Fixed.');
              Break;
            end;
            if Err = 1 then
            begin
              LogOutput('[-] Error: Length overflow at offset ' +
                IntToStr(F.Position) + '.');
              Trk.Data[Length(Trk.Data) - 1].Len := 0;
            end
            else
              for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
              begin
                ReadBuf(B, 1);
                if F.Position >= F.Size then
                begin
                  Trk.Data[Length(Trk.Data) - 1].Len :=
                    Length(Trk.Data[Length(Trk.Data) - 1].DataArray);
                  LogOutput(
                    '[-] Error: Incomplete event data (end of track reached).');
                  Break;
                end;
                SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray,
                  Length(Trk.Data[Length(Trk.Data) - 1].DataArray) + 1);
                Trk.Data[Length(Trk.Data) - 1].DataArray[Length(
                  Trk.Data[Length(Trk.Data) - 1].DataArray) - 1] := B;
              end;
          end;
          1:
          begin // QuarterFrame
            ReadBuf(B, 1);
            if B > 127 then
            begin
              LogOutput('[-] Error: Parameter greater than 127 at offset ' +
                IntToStr(F.Position - 1) + '.');
              B := 127;
            end;
            Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
          end;
          2:
          begin // PSongPos
            ReadBuf(B, 1);
            if B > 127 then
            begin
              LogOutput('[-] Error: Parameter greater than 127 at offset ' +
                IntToStr(F.Position - 1) + '.');
              B := 127;
            end;
            Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
            ReadBuf(B, 1);
            if B > 127 then
            begin
              LogOutput('[-] Error: Parameter greater than 127 at offset ' +
                IntToStr(F.Position - 1) + '.');
              B := 127;
            end;
            Trk.Data[Length(Trk.Data) - 1].BParm2 := B;
            Trk.Data[Length(Trk.Data) - 1].Value :=
              Trk.Data[Length(Trk.Data) - 1].BParm1 * 128 +
              Trk.Data[Length(Trk.Data) - 1].BParm2;
          end;
          3:
          begin // SongSelect
            ReadBuf(B, 1);
            if B > 127 then
            begin
              LogOutput('[-] Error: Parameter greater than 127 at offset ' +
                IntToStr(F.Position - 1) + '.');
              B := 127;
            end;
            Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
          end;
          6: ; // TuneRequest
          7: ; // EOX
          8: ; // TimingClock
          10: ; // Start
          11: ; // Continue
          12: ; // Stop
          14: ; // ActiveSens
          15:
          begin // MetaEvnt
            ReadBuf(B, 1);
            if B > 127 then
            begin
              LogOutput('[-] Error: Parameter greater than 127 at offset ' +
                IntToStr(F.Position - 1) + '.');
              B := 127;
            end;
            Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
            case B of
              0:
              begin  // Track Number
                Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                if Err = 2 then
                begin
                  LogOutput('[-] Error: Incomplete event (end of track reached).');
                  SetLength(Trk.Data, Length(Trk.Data) - 1);
                  LogOutput('[+] Fixed.');
                  Break;
                end;
                if Err = 1 then
                begin
                  LogOutput('[-] Error: Length overflow at offset ' +
                    IntToStr(F.Position) + '.');
                  Trk.Data[Length(Trk.Data) - 1].Len := 0;
                end
                else
                  for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                  begin
                    ReadBuf(B, 1);
                    if J > 0 then
                      Trk.Data[Length(Trk.Data) - 1].Value :=
                        Trk.Data[Length(Trk.Data) - 1].Value shl 8;
                    Trk.Data[Length(Trk.Data) - 1].Value :=
                      Trk.Data[Length(Trk.Data) - 1].Value or B;
                  end;
              end;
              1..7:
              begin  // Text, Copyright, TrackName, InstrName, Lyrics, Marker, CuePoint
                Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                if Err = 2 then
                begin
                  LogOutput('[-] Error: Incomplete event (end of track reached).');
                  SetLength(Trk.Data, Length(Trk.Data) - 1);
                  LogOutput('[+] Fixed.');
                  Break;
                end;
                if Err = 1 then
                begin
                  LogOutput('[-] Error: Length overflow at offset ' +
                    IntToStr(F.Position) + '.');
                  Trk.Data[Length(Trk.Data) - 1].Len := 0;
                end
                else
                begin
                  Buf := AllocMem(Trk.Data[Length(Trk.Data) - 1].Len + 1);
                  ZeroMemory(Buf, Trk.Data[Length(Trk.Data) - 1].Len + 1);
                  ReadBuf(Buf^, Trk.Data[Length(Trk.Data) - 1].Len);
                  Trk.Data[Length(Trk.Data) - 1].DataString := PAnsiChar(Buf);
                  FreeMem(Buf, Trk.Data[Length(Trk.Data) - 1].Len + 1);
                end;
                if Trk.Data[Length(Trk.Data) - 1].BParm1 = 3 then
                  if (Trk.Title = '') and
                    (Trk.Data[Length(Trk.Data) - 1].DataString <> '') then
                    Trk.Title := Trk.Data[Length(Trk.Data) - 1].DataString;
              end;
              32:
              begin // MIDI Channel
                Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                if Err = 2 then
                begin
                  LogOutput('[-] Error: Incomplete event (end of track reached).');
                  SetLength(Trk.Data, Length(Trk.Data) - 1);
                  LogOutput('[+] Fixed.');
                  Break;
                end;
                if Err = 1 then
                begin
                  LogOutput('[-] Error: Length overflow at offset ' +
                    IntToStr(F.Position) + '.');
                  Trk.Data[Length(Trk.Data) - 1].Len := 0;
                end
                else
                begin
                  for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                  begin
                    ReadBuf(B, 1);
                    if J > 0 then
                      Trk.Data[Length(Trk.Data) - 1].Value :=
                        Trk.Data[Length(Trk.Data) - 1].Value shl 8;
                    Trk.Data[Length(Trk.Data) - 1].Value :=
                      Trk.Data[Length(Trk.Data) - 1].Value or B;
                  end;
                  if not (Trk.Data[Length(Trk.Data) - 1].Value in [0..15]) then
                  begin
                    LogOutput('[-] Error: MIDI Channel is wrong at offset ' +
                      IntToStr(F.Position - Trk.Data[Length(Trk.Data) - 1].Len) + '.');
                    Trk.Data[Length(Trk.Data) - 1].Len := 1;
                    Trk.Data[Length(Trk.Data) - 1].Value := 0;
                    LogOutput('[+] Fixed.');
                  end;
                end;
              end;
              33:
              begin // MIDI Port
                Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                if Err = 2 then
                begin
                  LogOutput('[-] Error: Incomplete event (end of track reached).');
                  SetLength(Trk.Data, Length(Trk.Data) - 1);
                  LogOutput('[+] Fixed.');
                  Break;
                end;
                if Err = 1 then
                begin
                  LogOutput('[-] Error: Length overflow at offset ' +
                    IntToStr(F.Position) + '.');
                  Trk.Data[Length(Trk.Data) - 1].Len := 0;
                end
                else
                  for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                  begin
                    ReadBuf(B, 1);
                    if J > 0 then
                      Trk.Data[Length(Trk.Data) - 1].Value :=
                        Trk.Data[Length(Trk.Data) - 1].Value shl 8;
                    Trk.Data[Length(Trk.Data) - 1].Value :=
                      Trk.Data[Length(Trk.Data) - 1].Value or B;
                  end;
              end;
              81:
              begin  // Tempo
                Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                if Err = 2 then
                begin
                  LogOutput('[-] Error: Incomplete event (end of track reached).');
                  SetLength(Trk.Data, Length(Trk.Data) - 1);
                  LogOutput('[+] Fixed.');
                  Break;
                end;
                if Err = 1 then
                begin
                  LogOutput('[-] Error: Length overflow at offset ' +
                    IntToStr(F.Position) + '.');
                  Trk.Data[Length(Trk.Data) - 1].Len := 0;
                end
                else
                  for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                  begin
                    ReadBuf(B, 1);
                    if J > 0 then
                      Trk.Data[Length(Trk.Data) - 1].Value :=
                        Trk.Data[Length(Trk.Data) - 1].Value shl 8;
                    Trk.Data[Length(Trk.Data) - 1].Value :=
                      Trk.Data[Length(Trk.Data) - 1].Value or B;
                  end;
              end;
              84:
              begin  // SMPTE Offset
                Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                if Err = 2 then
                begin
                  LogOutput('[-] Error: Incomplete event (end of track reached).');
                  SetLength(Trk.Data, Length(Trk.Data) - 1);
                  LogOutput('[+] Fixed.');
                  Break;
                end;
                if Err = 1 then
                begin
                  LogOutput('[-] Error: Length overflow at offset ' +
                    IntToStr(F.Position) + '.');
                  Trk.Data[Length(Trk.Data) - 1].Len := 0;
                end
                else
                begin
                  if Trk.Data[Length(Trk.Data) - 1].Len <> 5 then
                    LogOutput(
                      '[-] Error: SMPTE Offset is wrong formatted at offset ' + IntToStr(
                      F.Position - Trk.Data[Length(Trk.Data) - 1].Len) + '.');
                  for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                  begin
                    SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray,
                      Length(Trk.Data[Length(Trk.Data) - 1].DataArray) + 1);
                    ReadBuf(B, 1);
                    Trk.Data[Length(Trk.Data) - 1].DataArray[Length(
                      Trk.Data[Length(Trk.Data) - 1].DataArray) - 1] := B;
                  end;
                  if Trk.Data[Length(Trk.Data) - 1].Len < 5 then
                  begin
                    SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray, 5);
                    for J := Trk.Data[Length(Trk.Data) - 1].Len to 4 do
                      Trk.Data[Length(Trk.Data) - 1].DataArray[J] := 0;
                    Trk.Data[Length(Trk.Data) - 1].Len := 5;
                    LogOutput('[+] Fixed.');
                  end;
                  if Trk.Data[Length(Trk.Data) - 1].Len > 5 then
                  begin
                    SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray, 5);
                    Trk.Data[Length(Trk.Data) - 1].Len := 5;
                    LogOutput('[+] Fixed.');
                  end;
                end;
              end;
              88:
              begin  // TimeSignature
                Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                if Err = 2 then
                begin
                  LogOutput('[-] Error: Incomplete event (end of track reached).');
                  SetLength(Trk.Data, Length(Trk.Data) - 1);
                  LogOutput('[+] Fixed.');
                  Break;
                end;
                if Err = 1 then
                begin
                  LogOutput('[-] Error: Length overflow at offset ' +
                    IntToStr(F.Position) + '.');
                  Trk.Data[Length(Trk.Data) - 1].Len := 0;
                end
                else
                begin
                  if Trk.Data[Length(Trk.Data) - 1].Len <> 4 then
                    LogOutput(
                      '[-] Error: Time Signature is wrong formatted at offset ' + IntToStr(
                      F.Position - Trk.Data[Length(Trk.Data) - 1].Len) + '.');
                  for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                  begin
                    SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray,
                      Length(Trk.Data[Length(Trk.Data) - 1].DataArray) + 1);
                    ReadBuf(B, 1);
                    Trk.Data[Length(Trk.Data) - 1].DataArray[Length(
                      Trk.Data[Length(Trk.Data) - 1].DataArray) - 1] := B;
                  end;
                  if Trk.Data[Length(Trk.Data) - 1].Len < 4 then
                  begin
                    SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray, 4);
                    for J := Trk.Data[Length(Trk.Data) - 1].Len to 3 do
                      Trk.Data[Length(Trk.Data) - 1].DataArray[J] := 0;
                    Trk.Data[Length(Trk.Data) - 1].Len := 4;
                    LogOutput('[+] Fixed.');
                  end;
                  if Trk.Data[Length(Trk.Data) - 1].Len > 4 then
                  begin
                    SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray, 4);
                    Trk.Data[Length(Trk.Data) - 1].Len := 4;
                    LogOutput('[+] Fixed.');
                  end;
                end;
              end;
              89:
              begin  // KeySignature
                Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                if Err = 2 then
                begin
                  LogOutput('[-] Error: Incomplete event (end of track reached).');
                  SetLength(Trk.Data, Length(Trk.Data) - 1);
                  LogOutput('[+] Fixed.');
                  Break;
                end;
                if Err = 1 then
                begin
                  LogOutput('[-] Error: Length overflow at offset ' +
                    IntToStr(F.Position) + '.');
                  Trk.Data[Length(Trk.Data) - 1].Len := 0;
                end
                else
                begin
                  if Trk.Data[Length(Trk.Data) - 1].Len <> 2 then
                    LogOutput(
                      '[-] Error: Key Signature is wrong formatted at offset ' + IntToStr(
                      F.Position - Trk.Data[Length(Trk.Data) - 1].Len) + '.');
                  for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                  begin
                    SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray,
                      Length(Trk.Data[Length(Trk.Data) - 1].DataArray) + 1);
                    ReadBuf(B, 1);
                    Trk.Data[Length(Trk.Data) - 1].DataArray[Length(
                      Trk.Data[Length(Trk.Data) - 1].DataArray) - 1] := B;
                  end;
                  if Trk.Data[Length(Trk.Data) - 1].Len < 2 then
                  begin
                    SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray, 2);
                    for J := Trk.Data[Length(Trk.Data) - 1].Len to 1 do
                      Trk.Data[Length(Trk.Data) - 1].DataArray[J] := 0;
                    Trk.Data[Length(Trk.Data) - 1].Len := 2;
                    LogOutput('[+] Fixed.');
                  end;
                  if Trk.Data[Length(Trk.Data) - 1].Len > 2 then
                  begin
                    SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray, 2);
                    Trk.Data[Length(Trk.Data) - 1].Len := 2;
                    LogOutput('[+] Fixed.');
                  end;
                end;
              end;
              else
              begin
                // Custom meta event
                Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                if Err = 2 then
                begin
                  LogOutput('[-] Error: Incomplete event (end of track reached).');
                  SetLength(Trk.Data, Length(Trk.Data) - 1);
                  LogOutput('[+] Fixed.');
                  Break;
                end;
                if Err = 1 then
                begin
                  LogOutput('[-] Error: Length overflow at offset ' +
                    IntToStr(F.Position) + '.');
                  Trk.Data[Length(Trk.Data) - 1].Len := 0;
                end
                else
                  for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                  begin
                    ReadBuf(B, 1);
                    if F.Position >= F.Size then
                    begin
                      Trk.Data[Length(Trk.Data) - 1].Len :=
                        Length(Trk.Data[Length(Trk.Data) - 1].DataArray);
                      LogOutput(
                        '[-] Error: Incomplete event data (end of track reached).');
                      Break;
                    end;
                    SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray,
                      Length(Trk.Data[Length(Trk.Data) - 1].DataArray) + 1);
                    Trk.Data[Length(Trk.Data) - 1].DataArray[Length(
                      Trk.Data[Length(Trk.Data) - 1].DataArray) - 1] := B;
                  end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

//==============================================================================
//
// TXMICore.ReadTrackData_MUS
//
//==============================================================================
procedure TXMICore.ReadTrackData_MUS(var F: TMemoryStream; var Trk: Chunk);
var
  J: integer;
  Buf: Pointer;
  B: byte;
  StatByte: boolean;
  Err: byte;

  procedure ReadBuf(var Buf; Size: cardinal);
  begin
    try
      F.ReadBuffer(Buf, Size);
    except
      LogOutput('[-] Error: Incomplete event (end of track reached).');
    end;
  end;

begin
  StatByte := False;
  while F.Position < F.Size do
  begin
    // Adding event
    SetLength(Trk.Data, Length(Trk.Data) + 1);
    // Initializing variables
    Trk.Data[Length(Trk.Data) - 1].Status := 0;
    Trk.Data[Length(Trk.Data) - 1].BParm1 := 0;
    Trk.Data[Length(Trk.Data) - 1].BParm2 := 0;
    Trk.Data[Length(Trk.Data) - 1].Value := 0;
    Trk.Data[Length(Trk.Data) - 1].Len := 0;
    Trk.Data[Length(Trk.Data) - 1].RunStatMode := False;
    // Reading ticks count
    // modified (MUS format)
    Trk.Data[Length(Trk.Data) - 1].Ticks := ReadVarVal_MUS(F, Err);
    if Err = 2 then
    begin
      LogOutput('[-] Error: Incomplete event (end of track reached).');
      SetLength(Trk.Data, Length(Trk.Data) - 1);
      LogOutput('[+] Fixed.');
      Break;
    end;
    if Err = 1 then
      LogOutput('[-] Error: Ticks count overflow at offset ' +
        IntToStr(F.Position) + '.')
    else
    begin
      // Reading status byte
      ReadBuf(B, 1);
      if (B < 128) and not StatByte then
        LogOutput('[-] Error: Status byte expected at offset ' +
          IntToStr(F.Position - 1) + '.')
      else
      begin
        if B >= 128 then
        begin
          // Status byte found
          Trk.Data[Length(Trk.Data) - 1].Status := B;
          StatByte := True;
        end;
        if StatByte and (B < 128) then
        begin
          // New status byte not found, but already set
          Trk.Data[Length(Trk.Data) - 1].Status :=
            Trk.Data[Length(Trk.Data) - 2].Status;
          Trk.Data[Length(Trk.Data) - 1].RunStatMode := True;
          B := Trk.Data[Length(Trk.Data) - 1].Status;
          F.Seek(F.Position - 1, 0);
        end;
        case B shr 4 of
          8, 9, 11:
          begin // NoteOff, NoteOn, Control (2 params)
            ReadBuf(B, 1);
            if B > 127 then
            begin
              LogOutput('[-] Error: Parameter greater than 127 at offset ' +
                IntToStr(F.Position - 1) + '.');
              B := 127;
            end;
            Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
            ReadBuf(B, 1);
            if B > 127 then
            begin
              LogOutput('[-] Error: Parameter greater than 127 at offset ' +
                IntToStr(F.Position - 1) + '.');
              B := 127;
            end;
            Trk.Data[Length(Trk.Data) - 1].BParm2 := B;
          end;
          // modified (MUS format)
          10:
          begin // PolyAfterTouch (1 param)
            ReadBuf(B, 1);
            if B > 127 then
            begin
              LogOutput('[-] Error: Parameter greater than 127 at offset ' +
                IntToStr(F.Position - 1) + '.');
              B := 127;
            end;
            Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
            Trk.Data[Length(Trk.Data) - 1].BParm2 := 64;
          end;
          12..13:
          begin // ProgramChange, ChanAfterTouch (1 param)
            ReadBuf(B, 1);
            if B > 127 then
            begin
              LogOutput('[-] Error: Parameter greater than 127 at offset ' +
                IntToStr(F.Position - 1) + '.');
              B := 127;
            end;
            Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
          end;
          14:
          begin // Pitch Bend (1 param / 2 bytes)
            ReadBuf(B, 1);
            if B > 127 then
            begin
              LogOutput('[-] Error: Parameter greater than 127 at offset ' +
                IntToStr(F.Position - 1) + '.');
              B := 127;
            end;
            Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
            ReadBuf(B, 1);
            if B > 127 then
            begin
              LogOutput('[-] Error: Parameter greater than 127 at offset ' +
                IntToStr(F.Position - 1) + '.');
              B := 127;
            end;
            Trk.Data[Length(Trk.Data) - 1].BParm2 := B;
            Trk.Data[Length(Trk.Data) - 1].Value :=
              Trk.Data[Length(Trk.Data) - 1].BParm1 +
              Trk.Data[Length(Trk.Data) - 1].BParm2 * 128;
          end;
          15:
          begin // System
            case B and 15 of
              // modified (MUS format)
              0:
              begin // SysEx / Set Speed
                Trk.Data[Length(Trk.Data) - 1].Len := 0;
                repeat
                  ReadBuf(B, 1);
                  if F.Position >= F.Size then
                  begin
                    Trk.Data[Length(Trk.Data) - 1].Len :=
                      Length(Trk.Data[Length(Trk.Data) - 1].DataArray);
                    LogOutput(
                      '[-] Error: Incomplete event data (end of track reached).');
                    Break;
                  end;
                  SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray,
                    Length(Trk.Data[Length(Trk.Data) - 1].DataArray) + 1);
                  Trk.Data[Length(Trk.Data) - 1].DataArray[Length(
                    Trk.Data[Length(Trk.Data) - 1].DataArray) - 1] := B;
                until B = $F7;
                Trk.Data[Length(Trk.Data) - 1].Len :=
                  Length(Trk.Data[Length(Trk.Data) - 1].DataArray);
              end;
              1:
              begin // QuarterFrame
                ReadBuf(B, 1);
                if B > 127 then
                begin
                  LogOutput('[-] Error: Parameter greater than 127 at offset ' +
                    IntToStr(F.Position - 1) + '.');
                  B := 127;
                end;
                Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
              end;
              2:
              begin // PSongPos
                ReadBuf(B, 1);
                if B > 127 then
                begin
                  LogOutput('[-] Error: Parameter greater than 127 at offset ' +
                    IntToStr(F.Position - 1) + '.');
                  B := 127;
                end;
                Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
                ReadBuf(B, 1);
                if B > 127 then
                begin
                  LogOutput('[-] Error: Parameter greater than 127 at offset ' +
                    IntToStr(F.Position - 1) + '.');
                  B := 127;
                end;
                Trk.Data[Length(Trk.Data) - 1].BParm2 := B;
                Trk.Data[Length(Trk.Data) - 1].Value :=
                  Trk.Data[Length(Trk.Data) - 1].BParm1 * 128 +
                  Trk.Data[Length(Trk.Data) - 1].BParm2;
              end;
              3:
              begin // SongSelect
                ReadBuf(B, 1);
                if B > 127 then
                begin
                  LogOutput('[-] Error: Parameter greater than 127 at offset ' +
                    IntToStr(F.Position - 1) + '.');
                  B := 127;
                end;
                Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
              end;
              6: ; // TuneRequest
              // modified (MUS format)
              7: ; // EOX
              8: ; // TimingClock
              10: ; // Start
              11: ; // Continue
              12: ; // Stop
              14: ; // ActiveSens
              15:
              begin // MetaEvnt
                ReadBuf(B, 1);
                if B > 127 then
                begin
                  LogOutput('[-] Error: Parameter greater than 127 at offset ' +
                    IntToStr(F.Position - 1) + '.');
                  B := 127;
                end;
                Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
                case B of
                  0:
                  begin  // Track Number
                    Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                    if Err = 2 then
                    begin
                      LogOutput(
                        '[-] Error: Incomplete event (end of track reached).');
                      SetLength(Trk.Data, Length(Trk.Data) - 1);
                      LogOutput('[+] Fixed.');
                      Break;
                    end;
                    if Err = 1 then
                    begin
                      LogOutput('[-] Error: Length overflow at offset ' +
                        IntToStr(F.Position) + '.');
                      Trk.Data[Length(Trk.Data) - 1].Len := 0;
                    end
                    else
                      for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                      begin
                        ReadBuf(B, 1);
                        if J > 0 then
                          Trk.Data[Length(Trk.Data) - 1].Value :=
                            Trk.Data[Length(Trk.Data) - 1].Value shl 8;
                        Trk.Data[Length(Trk.Data) - 1].Value :=
                          Trk.Data[Length(Trk.Data) - 1].Value or B;
                      end;
                  end;
                  1..7:
                  begin  // Text, Copyright, TrackName, InstrName, Lyrics, Marker, CuePoint
                    Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                    if Err = 2 then
                    begin
                      LogOutput(
                        '[-] Error: Incomplete event (end of track reached).');
                      SetLength(Trk.Data, Length(Trk.Data) - 1);
                      LogOutput('[+] Fixed.');
                      Break;
                    end;
                    if Err = 1 then
                    begin
                      LogOutput('[-] Error: Length overflow at offset ' +
                        IntToStr(F.Position) + '.');
                      Trk.Data[Length(Trk.Data) - 1].Len := 0;
                    end
                    else
                    begin
                      Buf := AllocMem(Trk.Data[Length(Trk.Data) - 1].Len + 1);
                      ZeroMemory(Buf, Trk.Data[Length(Trk.Data) - 1].Len + 1);
                      ReadBuf(Buf^, Trk.Data[Length(Trk.Data) - 1].Len);
                      Trk.Data[Length(Trk.Data) - 1].DataString := PAnsiChar(Buf);
                      FreeMem(Buf, Trk.Data[Length(Trk.Data) - 1].Len + 1);
                    end;
                    if Trk.Data[Length(Trk.Data) - 1].BParm1 = 3 then
                      if (Trk.Title = '') and
                        (Trk.Data[Length(Trk.Data) - 1].DataString <> '') then
                        Trk.Title := Trk.Data[Length(Trk.Data) - 1].DataString;
                  end;
                  32:
                  begin // MIDI Channel
                    Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                    if Err = 2 then
                    begin
                      LogOutput(
                        '[-] Error: Incomplete event (end of track reached).');
                      SetLength(Trk.Data, Length(Trk.Data) - 1);
                      LogOutput('[+] Fixed.');
                      Break;
                    end;
                    if Err = 1 then
                    begin
                      LogOutput('[-] Error: Length overflow at offset ' +
                        IntToStr(F.Position) + '.');
                      Trk.Data[Length(Trk.Data) - 1].Len := 0;
                    end
                    else
                    begin
                      for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                      begin
                        ReadBuf(B, 1);
                        if J > 0 then
                          Trk.Data[Length(Trk.Data) - 1].Value :=
                            Trk.Data[Length(Trk.Data) - 1].Value shl 8;
                        Trk.Data[Length(Trk.Data) - 1].Value :=
                          Trk.Data[Length(Trk.Data) - 1].Value or B;
                      end;
                      if not (Trk.Data[Length(Trk.Data) - 1].Value in [0..15]) then
                      begin
                        LogOutput('[-] Error: MIDI Channel is wrong at offset ' +
                          IntToStr(F.Position - Trk.Data[Length(Trk.Data) - 1].Len) + '.');
                        Trk.Data[Length(Trk.Data) - 1].Len := 1;
                        Trk.Data[Length(Trk.Data) - 1].Value := 0;
                        LogOutput('[+] Fixed.');
                      end;
                    end;
                  end;
                  33:
                  begin // MIDI Port
                    Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                    if Err = 2 then
                    begin
                      LogOutput(
                        '[-] Error: Incomplete event (end of track reached).');
                      SetLength(Trk.Data, Length(Trk.Data) - 1);
                      LogOutput('[+] Fixed.');
                      Break;
                    end;
                    if Err = 1 then
                    begin
                      LogOutput('[-] Error: Length overflow at offset ' +
                        IntToStr(F.Position) + '.');
                      Trk.Data[Length(Trk.Data) - 1].Len := 0;
                    end
                    else
                      for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                      begin
                        ReadBuf(B, 1);
                        if J > 0 then
                          Trk.Data[Length(Trk.Data) - 1].Value :=
                            Trk.Data[Length(Trk.Data) - 1].Value shl 8;
                        Trk.Data[Length(Trk.Data) - 1].Value :=
                          Trk.Data[Length(Trk.Data) - 1].Value or B;
                      end;
                  end;
                  81:
                  begin  // Tempo
                    Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                    if Err = 2 then
                    begin
                      LogOutput(
                        '[-] Error: Incomplete event (end of track reached).');
                      SetLength(Trk.Data, Length(Trk.Data) - 1);
                      LogOutput('[+] Fixed.');
                      Break;
                    end;
                    if Err = 1 then
                    begin
                      LogOutput('[-] Error: Length overflow at offset ' +
                        IntToStr(F.Position) + '.');
                      Trk.Data[Length(Trk.Data) - 1].Len := 0;
                    end
                    else
                      for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                      begin
                        ReadBuf(B, 1);
                        if J > 0 then
                          Trk.Data[Length(Trk.Data) - 1].Value :=
                            Trk.Data[Length(Trk.Data) - 1].Value shl 8;
                        Trk.Data[Length(Trk.Data) - 1].Value :=
                          Trk.Data[Length(Trk.Data) - 1].Value or B;
                      end;
                  end;
                  84:
                  begin  // SMPTE Offset
                    Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                    if Err = 2 then
                    begin
                      LogOutput(
                        '[-] Error: Incomplete event (end of track reached).');
                      SetLength(Trk.Data, Length(Trk.Data) - 1);
                      LogOutput('[+] Fixed.');
                      Break;
                    end;
                    if Err = 1 then
                    begin
                      LogOutput('[-] Error: Length overflow at offset ' +
                        IntToStr(F.Position) + '.');
                      Trk.Data[Length(Trk.Data) - 1].Len := 0;
                    end
                    else
                    begin
                      if Trk.Data[Length(Trk.Data) - 1].Len <> 5 then
                        LogOutput(
                          '[-] Error: SMPTE Offset is wrong formatted at offset ' + IntToStr(
                          F.Position - Trk.Data[Length(Trk.Data) - 1].Len) + '.');
                      for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                      begin
                        SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray,
                          Length(Trk.Data[Length(Trk.Data) - 1].DataArray) + 1);
                        ReadBuf(B, 1);
                        Trk.Data[Length(Trk.Data) - 1].DataArray[Length(
                          Trk.Data[Length(Trk.Data) - 1].DataArray) - 1] := B;
                      end;
                      if Trk.Data[Length(Trk.Data) - 1].Len < 5 then
                      begin
                        SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray, 5);
                        for J := Trk.Data[Length(Trk.Data) - 1].Len to 4 do
                          Trk.Data[Length(Trk.Data) - 1].DataArray[J] := 0;
                        Trk.Data[Length(Trk.Data) - 1].Len := 5;
                        LogOutput('[+] Fixed.');
                      end;
                      if Trk.Data[Length(Trk.Data) - 1].Len > 5 then
                      begin
                        SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray, 5);
                        Trk.Data[Length(Trk.Data) - 1].Len := 5;
                        LogOutput('[+] Fixed.');
                      end;
                    end;
                  end;
                  88:
                  begin  // TimeSignature
                    Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                    if Err = 2 then
                    begin
                      LogOutput(
                        '[-] Error: Incomplete event (end of track reached).');
                      SetLength(Trk.Data, Length(Trk.Data) - 1);
                      LogOutput('[+] Fixed.');
                      Break;
                    end;
                    if Err = 1 then
                    begin
                      LogOutput('[-] Error: Length overflow at offset ' +
                        IntToStr(F.Position) + '.');
                      Trk.Data[Length(Trk.Data) - 1].Len := 0;
                    end
                    else
                    begin
                      if Trk.Data[Length(Trk.Data) - 1].Len <> 4 then
                        LogOutput(
                          '[-] Error: Time Signature is wrong formatted at offset ' + IntToStr(
                          F.Position - Trk.Data[Length(Trk.Data) - 1].Len) + '.');
                      for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                      begin
                        SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray,
                          Length(Trk.Data[Length(Trk.Data) - 1].DataArray) + 1);
                        ReadBuf(B, 1);
                        Trk.Data[Length(Trk.Data) - 1].DataArray[Length(
                          Trk.Data[Length(Trk.Data) - 1].DataArray) - 1] := B;
                      end;
                      if Trk.Data[Length(Trk.Data) - 1].Len < 4 then
                      begin
                        SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray, 4);
                        for J := Trk.Data[Length(Trk.Data) - 1].Len to 3 do
                          Trk.Data[Length(Trk.Data) - 1].DataArray[J] := 0;
                        Trk.Data[Length(Trk.Data) - 1].Len := 4;
                        LogOutput('[+] Fixed.');
                      end;
                      if Trk.Data[Length(Trk.Data) - 1].Len > 4 then
                      begin
                        SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray, 4);
                        Trk.Data[Length(Trk.Data) - 1].Len := 4;
                        LogOutput('[+] Fixed.');
                      end;
                    end;
                  end;
                  89:
                  begin  // KeySignature
                    Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                    if Err = 2 then
                    begin
                      LogOutput(
                        '[-] Error: Incomplete event (end of track reached).');
                      SetLength(Trk.Data, Length(Trk.Data) - 1);
                      LogOutput('[+] Fixed.');
                      Break;
                    end;
                    if Err = 1 then
                    begin
                      LogOutput('[-] Error: Length overflow at offset ' +
                        IntToStr(F.Position) + '.');
                      Trk.Data[Length(Trk.Data) - 1].Len := 0;
                    end
                    else
                    begin
                      if Trk.Data[Length(Trk.Data) - 1].Len <> 2 then
                        LogOutput(
                          '[-] Error: Key Signature is wrong formatted at offset ' + IntToStr(
                          F.Position - Trk.Data[Length(Trk.Data) - 1].Len) + '.');
                      for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                      begin
                        SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray,
                          Length(Trk.Data[Length(Trk.Data) - 1].DataArray) + 1);
                        ReadBuf(B, 1);
                        Trk.Data[Length(Trk.Data) - 1].DataArray[Length(
                          Trk.Data[Length(Trk.Data) - 1].DataArray) - 1] := B;
                      end;
                      if Trk.Data[Length(Trk.Data) - 1].Len < 2 then
                      begin
                        SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray, 2);
                        for J := Trk.Data[Length(Trk.Data) - 1].Len to 1 do
                          Trk.Data[Length(Trk.Data) - 1].DataArray[J] := 0;
                        Trk.Data[Length(Trk.Data) - 1].Len := 2;
                        LogOutput('[+] Fixed.');
                      end;
                      if Trk.Data[Length(Trk.Data) - 1].Len > 2 then
                      begin
                        SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray, 2);
                        Trk.Data[Length(Trk.Data) - 1].Len := 2;
                        LogOutput('[+] Fixed.');
                      end;
                    end;
                  end;
                  else
                  begin
                    // Custom meta event
                    Trk.Data[Length(Trk.Data) - 1].Len := ReadVarVal(F, Err);
                    if Err = 2 then
                    begin
                      LogOutput(
                        '[-] Error: Incomplete event (end of track reached).');
                      SetLength(Trk.Data, Length(Trk.Data) - 1);
                      LogOutput('[+] Fixed.');
                      Break;
                    end;
                    if Err = 1 then
                    begin
                      LogOutput('[-] Error: Length overflow at offset ' +
                        IntToStr(F.Position) + '.');
                      Trk.Data[Length(Trk.Data) - 1].Len := 0;
                    end
                    else
                      for J := 0 to Trk.Data[Length(Trk.Data) - 1].Len - 1 do
                      begin
                        SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray,
                          Length(Trk.Data[Length(Trk.Data) - 1].DataArray) + 1);
                        ReadBuf(B, 1);
                        Trk.Data[Length(Trk.Data) - 1].DataArray[Length(
                          Trk.Data[Length(Trk.Data) - 1].DataArray) - 1] := B;
                      end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

//==============================================================================
//
// TXMICore.ReadTrackData_SOP
//
//==============================================================================
procedure TXMICore.ReadTrackData_SOP(var F: TMemoryStream; var Trk: Chunk);
var
  I: integer;
  W: word;
  B: byte;
begin
  while F.Position < F.Size do
  begin
    SetLength(Trk.Data, Length(Trk.Data) + 1);
    I := Length(Trk.Data) - 1;
    F.ReadBuffer(W, 2); // delta ticks
    Trk.Data[I].Ticks := W;
    F.ReadBuffer(B, 1); // event code
    case B of
      1: // Special Event
      begin
        Trk.Data[I].Status := $B0;
        F.ReadBuffer(B, 1); // value
        Trk.Data[I].BParm1 := 18;
        Trk.Data[I].BParm2 := B;
      end;
      2: // Play Note
      begin
        Trk.Data[I].Status := $90;
        F.ReadBuffer(B, 1); // note
        Trk.Data[I].BParm1 := B;
        Trk.Data[I].BParm2 := 127;
        F.ReadBuffer(W, 2); // duration
        Trk.Data[I].Len := W;
      end;
      3: // Change Tempo
      begin
        Trk.Data[I].Status := $B0;
        F.ReadBuffer(B, 1); // tempo
        Trk.Data[I].BParm1 := 17;
        Trk.Data[I].BParm2 := B;
      end;
      4: // Change Volume
      begin
        Trk.Data[I].Status := $B0;
        F.ReadBuffer(B, 1); // volume
        Trk.Data[I].BParm1 := 7;
        Trk.Data[I].BParm2 := B;
      end;
      5: // Change Pitch
      begin
        Trk.Data[I].Status := $B0;
        F.ReadBuffer(B, 1); // pitch
        Trk.Data[I].BParm1 := 9;
        Trk.Data[I].BParm2 := B;
      end;
      6: // Program Change
      begin
        Trk.Data[I].Status := $C0;
        F.ReadBuffer(B, 1); // instrument
        Trk.Data[I].BParm1 := B;
      end;
      7: // Set Panning
      begin
        Trk.Data[I].Status := $B0;
        F.ReadBuffer(B, 1); // panning
        Trk.Data[I].BParm1 := 10;
        case B of
          0: Trk.Data[I].BParm2 := 127;
          1: Trk.Data[I].BParm2 := 64;
          2: Trk.Data[I].BParm2 := 0;
        end;
      end;
      8: // Global Volume
      begin
        Trk.Data[I].Status := $B0;
        F.ReadBuffer(B, 1); // volume
        Trk.Data[I].BParm1 := 16;
        Trk.Data[I].BParm2 := B;
      end;
    end;
  end;
end;

//==============================================================================
//
// TXMICore.ReadTrackData_HERAD
//
//==============================================================================
procedure TXMICore.ReadTrackData_HERAD(var F: TMemoryStream; var Trk: Chunk);
var
  isM32, isV2: boolean;
  S: string;
  Ch, B: byte;
  Ticks: UInt64;
  Err: byte;

  procedure ReadBuf(var Buf; Size: cardinal);
  begin
    try
      F.ReadBuffer(Buf, Size);
    except
      LogOutput('[-] Error: Incomplete event (end of track reached).');
    end;
  end;

begin
  isM32 := not SongData_GetStr('HERAD_Inst#0', S);
  B := 0;
  SongData_GetByte('HERAD_V2', B);
  isV2 := B > 0;
  if not SongData_GetByte('HERAD_TmpIdx', Ch) then
    Ch := 0;
  if Ch >= 9 then
    Ch := (Ch + 1) mod 16;
  while F.Position < F.Size do
  begin
    // Reading ticks count
    Ticks := ReadVarVal(F, Err);
    if Err = 1 then
    begin
      LogOutput('[-] Error: Ticks count overflow at offset ' +
        IntToStr(F.Position) + '.');
      Continue;
    end;
    if Err = 2 then
    begin
      LogOutput('[-] Error: Incomplete event (end of track reached).');
      Break;
    end;
    // Adding event
    SetLength(Trk.Data, Length(Trk.Data) + 1);
    // Initializing structure
    Trk.Data[Length(Trk.Data) - 1].Ticks := Ticks;
    Trk.Data[Length(Trk.Data) - 1].Status := 0;
    Trk.Data[Length(Trk.Data) - 1].BParm1 := 0;
    Trk.Data[Length(Trk.Data) - 1].BParm2 := 0;
    Trk.Data[Length(Trk.Data) - 1].Value := 0;
    Trk.Data[Length(Trk.Data) - 1].Len := 0;
    Trk.Data[Length(Trk.Data) - 1].RunStatMode := False;
    ReadBuf(B, 1);
    if B >= 128 then
    begin
      // Status byte found
      if (not isM32) and (B shr 4 < $F) then
        B := (B and $F0) or (Ch and $F);
      Trk.Data[Length(Trk.Data) - 1].Status := B;
    end
    else
    begin
      LogOutput('[-] Error: Status byte expected at offset ' +
        IntToStr(F.Position - 1) + '.');
      Continue;
    end;
    if isV2 and (B shr 4 = 8) then
    begin
      // NoteOff for HERAD v2
      ReadBuf(B, 1);
      if B > 127 then
      begin
        LogOutput('[-] Error: Parameter greater than 127 at offset ' +
          IntToStr(F.Position - 1) + '.');
        B := 127;
      end;
      Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
      Trk.Data[Length(Trk.Data) - 1].BParm2 := 0;
      Continue;
    end;
    case B shr 4 of
      8..11:
      begin // NoteOff, NoteOn, PolyAfterTouch, Control (2 params)
        ReadBuf(B, 1);
        if B > 127 then
        begin
          LogOutput('[-] Error: Parameter greater than 127 at offset ' +
            IntToStr(F.Position - 1) + '.');
          B := 127;
        end;
        Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
        ReadBuf(B, 1);
        if B > 127 then
        begin
          LogOutput('[-] Error: Parameter greater than 127 at offset ' +
            IntToStr(F.Position - 1) + '.');
          B := 127;
        end;
        Trk.Data[Length(Trk.Data) - 1].BParm2 := B;
      end;
      12..13:
      begin // ProgramChange, ChanAfterTouch (1 param)
        ReadBuf(B, 1);
        if B > 127 then
        begin
          LogOutput('[-] Error: Parameter greater than 127 at offset ' +
            IntToStr(F.Position - 1) + '.');
          B := 127;
        end;
        Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
      end;
      14:
      begin // Pitch Bend - HERAD
        ReadBuf(B, 1);
        if not isM32 then
        begin // 1 param / 1 byte
          // SDB / AGD pitch bend 0x00..0x80 (can be greater)
          Trk.Data[Length(Trk.Data) - 1].BParm1 := 0;
          Trk.Data[Length(Trk.Data) - 1].BParm2 := B;
        end
        else
        begin // 1 param / 2 byte
          if B > $7F then
          begin
            LogOutput('[-] Error: Parameter greater than 127 at offset ' +
              IntToStr(F.Position - 1) + '.');
            B := $7F;
          end;
          Trk.Data[Length(Trk.Data) - 1].BParm1 := B;
          ReadBuf(B, 1);
          if B > $7F then
          begin
            LogOutput('[-] Error: Parameter greater than 127 at offset ' +
              IntToStr(F.Position - 1) + '.');
            B := $7F;
          end;
          Trk.Data[Length(Trk.Data) - 1].BParm2 := B;
        end;
        Trk.Data[Length(Trk.Data) - 1].Value :=
          Trk.Data[Length(Trk.Data) - 1].BParm1 +
          Trk.Data[Length(Trk.Data) - 1].BParm2 * 128;
      end;
      15:
      begin // Treat any 0xFx as track end
        Trk.Data[Length(Trk.Data) - 1].Status := $FF;
        Trk.Data[Length(Trk.Data) - 1].BParm1 := $2F;
        Trk.Data[Length(Trk.Data) - 1].BParm2 := 0;
        Trk.Data[Length(Trk.Data) - 1].Len := 0;
        Trk.Data[Length(Trk.Data) - 1].Value := F.Size - F.Position;
        SetLength(Trk.Data[Length(Trk.Data) - 1].DataArray, 0);
        Trk.Data[Length(Trk.Data) - 1].DataString := '';
        Break;
      end;
    end;
  end;
end;

//==============================================================================
//
// TXMICore.WriteMIDI
//
//==============================================================================
procedure TXMICore.WriteMIDI(var F: TMemoryStream);
label
  Done;
const
  Header = 'MThd';
  Track = 'MTrk';
var
  Ver, Division: word;
  SMPTE: shortint;
  C, Cnt: cardinal;
  I: integer;
  TrkOffset: cardinal;
  TrackStream: TMemoryStream;
begin
  LogOutput('[*] Writing MIDI header...');
  F.WriteBuffer(PAnsiChar(Header)^, 4);
  C := $06000000;
  F.WriteBuffer(C, 4);
  if not SongData_GetWord('MIDIType', Ver) then
  begin
    LogOutput('[-] MIDI Type is not defined.');
    Exit;
  end;
  C := (Ver shr 8) or ((Ver and $FF) shl 8);
  F.WriteBuffer(C, 2);
  C := Length(TrackData);
  C := (C shr 8) or ((C and $FF) shl 8);
  F.WriteBuffer(C, 2);
  if not SongData_GetSInt('SMPTE', SMPTE) then
    SMPTE := 0;
  if not SongData_GetWord('Division', Division) then
  begin
    LogOutput('[-] Division is not defined.');
    Exit;
  end;
  if SMPTE >= 0 then
  begin
    C := (Division shr 8) or ((Division and $FF) shl 8);
    F.WriteBuffer(C, 2);
  end
  else
  begin
    F.WriteBuffer(SMPTE, 1);
    F.WriteBuffer(Division, 1);
  end;
  Cnt := 0;
  if Length(TrackData) = 0 then
    goto Done;
  for I := 0 to Length(TrackData) - 1 do
  begin
    if (Ver = 0) and (I <> TrkCh.ItemIndex) then
      Continue;
    LogOutput('[*] Writing track ' + IntToStr(I) + '...');
    F.WriteBuffer(PAnsiChar(Track)^, 4);
    C := 0;
    F.WriteBuffer(C, 4);
    TrkOffset := F.Position;
    TrackStream := TMemoryStream.Create;
    WriteTrackData(TrackStream, TrackData[I]);
    F.WriteBuffer(TrackStream.Memory^, TrackStream.Size);
    TrackStream.Free;
    C := F.Position - TrkOffset;
    Inc(Cnt);
    LogOutput('[+] Wrote ' + IntToStr(C) + ' bytes.');
    F.Seek(TrkOffset - 4, 0);
    C := (C shr 24) or ((C shr 8) and $FF00) or ((C shl 8) and $FF0000) or
      ((C and $FF) shl 24);
    F.WriteBuffer(C, 4);
    F.Seek(F.Size, 0);
  end;
  Done:
    // Write final track count
    F.Seek($A, soFromBeginning);
  C := Cnt;
  C := (C shr 8) or ((C and $FF) shl 8);
  F.WriteBuffer(C, 2);
  F.Seek(0, soFromEnd);
end;

//==============================================================================
//
// TXMICore.WriteRMI
//
//==============================================================================
procedure TXMICore.WriteRMI(var F: TMemoryStream);
const
  RIFF = 'RIFF';
  RMID = 'RMID';
  Data = 'data';
var
  dw: DWORD;
  MIDI: TMemoryStream;
begin
  LogOutput('[*] Writing RIFF header...');

  F.WriteBuffer(PAnsiChar(RIFF)^, 4);
  dw := 0;
  F.WriteBuffer(dw, 4);
  F.WriteBuffer(PAnsiChar(RMID)^, 4);
  F.WriteBuffer(PAnsiChar(Data)^, 4);

  LogOutput('[*] Writing RIFF data...');

  MIDI := TMemoryStream.Create;
  WriteMIDI(MIDI);

  dw := MIDI.Size;
  F.WriteBuffer(dw, 4);
  F.WriteBuffer(MIDI.Memory^, dw);
  if Odd(dw) then
  begin
    dw := 0;
    F.WriteBuffer(dw, 1);
  end;

  LogOutput('[*] Finalizing RIFF header...');

  F.Seek(4, soFromBeginning);
  dw := F.Size - 8;
  F.WriteBuffer(dw, 4);
  F.Seek(F.Size, soFromBeginning);
end;

//==============================================================================
//
// TXMICore.WriteXMI
//
//==============================================================================
procedure TXMICore.WriteXMI(var F: TMemoryStream);
const
  FORM = 'FORM';
  XDIR = 'XDIR';
  INFO = 'INFO';
  CAT = 'CAT ';
  XMID = 'XMID';
  TIMB = 'TIMB';
  EVNT = 'EVNT';
var
  Ver: word;
  C, Cnt: cardinal;
  I, J: integer;
  Track: TMemoryStream;
  Timbre: array of byte;
  Events: TMemoryStream;

  function TimbreAdded(B: byte): boolean;
  var
    I: integer;
  begin
    Result := True;
    for I := 0 to Length(Timbre) - 1 do
      if (I mod 2 = 0) and (Timbre[I] = B) then
        Exit;
    Result := False;
  end;

begin
  LogOutput('[*] Writing Extended MIDI file...');

  if not SongData_GetWord('MIDIType', Ver) then
  begin
    LogOutput('[-] MIDI Type is not defined.');
    Exit;
  end;
  if Ver = 1 then
    LogOutput('[*] Warning: MIDI Type-1 is not supported by XMIDI.');

  LogOutput('[*] Writing XMIDI header...');
  F.WriteBuffer(PAnsiChar(FORM)^, 4);
  C := 14;
  C := (C shr 24) or ((C shr 8) and $FF00) or ((C shl 8) and $FF0000) or
    ((C and $FF) shl 24);
  F.WriteBuffer(C, 4);
  F.WriteBuffer(PAnsiChar(XDIR)^, 4);
  F.WriteBuffer(PAnsiChar(INFO)^, 4);
  C := 2;
  C := (C shr 24) or ((C shr 8) and $FF00) or ((C shl 8) and $FF0000) or
    ((C and $FF) shl 24);
  F.WriteBuffer(C, 4);
  C := 0; // track count
  F.WriteBuffer(C, 2);

  F.WriteBuffer(PAnsiChar(CAT)^, 4);
  C := 0; // catalog size
  F.WriteBuffer(C, 4);
  F.WriteBuffer(PAnsiChar(XMID)^, 4);

  Cnt := 0;
  for I := 0 to Length(TrackData) - 1 do
  begin
    if (Ver = 0) and (I <> TrkCh.ItemIndex) then
      Continue;
    LogOutput('[*] Writing track ' + IntToStr(I) + '...');
    F.WriteBuffer(PAnsiChar(FORM)^, 4);
    Track := TMemoryStream.Create;
    SetLength(Timbre, 0);
    for J := 0 to Length(TrackData[I].Data) - 1 do
      case TrackData[I].Data[J].Status shr 4 of
        11:
          if TrackData[I].Data[J].BParm1 = 114 then
            if Odd(Length(Timbre)) then
            begin
              SetLength(Timbre, Length(Timbre) + 1);
              Timbre[Length(Timbre) - 1] := TrackData[I].Data[J].BParm2;
            end;
        12:
          if not TimbreAdded(TrackData[I].Data[J].BParm1) then
          begin
            if Odd(Length(Timbre)) then
            begin
              SetLength(Timbre, Length(Timbre) + 1);
              Timbre[Length(Timbre) - 1] := 0;
            end;
            SetLength(Timbre, Length(Timbre) + 1);
            Timbre[Length(Timbre) - 1] := TrackData[I].Data[J].BParm1;
          end;
      end;
    if Length(Timbre) > 0 then
    begin
      if Odd(Length(Timbre)) then
      begin
        SetLength(Timbre, Length(Timbre) + 1);
        Timbre[Length(Timbre) - 1] := 0;
      end;
      Track.WriteBuffer(PAnsiChar(TIMB)^, 4);
      C := 2 + Length(Timbre);
      C := (C shr 24) or ((C shr 8) and $FF00) or ((C shl 8) and $FF0000) or
        ((C and $FF) shl 24);
      Track.WriteBuffer(C, 4);
      C := Length(Timbre) div 2;
      Track.WriteBuffer(C, 2);
      Track.WriteBuffer(Timbre[0], Length(Timbre));
    end;
    Track.WriteBuffer(PAnsiChar(EVNT)^, 4);
    C := 0;
    Events := TMemoryStream.Create;
    WriteTrackData_XMI(Events, TrackData[I]);
    if Odd(Events.Size) then
      Events.WriteBuffer(C, 1);
    C := Events.Size;
    C := (C shr 24) or ((C shr 8) and $FF00) or ((C shl 8) and $FF0000) or
      ((C and $FF) shl 24);
    Track.WriteBuffer(C, 4);
    Track.WriteBuffer(Events.Memory^, Events.Size);
    Events.Free;
    C := Track.Size + 4;
    C := (C shr 24) or ((C shr 8) and $FF00) or ((C shl 8) and $FF0000) or
      ((C and $FF) shl 24);
    F.WriteBuffer(C, 4);
    F.WriteBuffer(PAnsiChar(XMID)^, 4);
    F.WriteBuffer(Track.Memory^, Track.Size);
    LogOutput('[+] Wrote ' + IntToStr(Track.Size) + ' bytes.');
    Track.Free;
    Inc(Cnt);
  end;
  // track count
  F.Seek($14, soFromBeginning);
  F.WriteBuffer(Cnt, 2);
  // catalog size
  F.Seek($1A, soFromBeginning);
  C := F.Size - $1E;
  C := (C shr 24) or ((C shr 8) and $FF00) or ((C shl 8) and $FF0000) or
    ((C and $FF) shl 24);
  F.WriteBuffer(C, 4);

  F.Seek(F.Size, soFromBeginning);
end;

//==============================================================================
//
// TXMICore.WriteCMF
//
//==============================================================================
procedure TXMICore.WriteCMF(var F: TMemoryStream);
const
  CTMF = 'CTMF';
var
  NewVer: boolean;
  W: word;
  iInstrumentCount: word;
  iChannelInUse: array[0..15] of byte;
  MIDIData: TMemoryStream;
  CMFInst: array[0..15] of byte;
  S: string;
  I, J: integer;
begin
  LogOutput('[*] Writing Creative Music File...');

  F.WriteBuffer(PAnsiChar(CTMF)^, 4);

  SongData_GetWord('CMF_Version', W);
  if W <> $0100 then
    W := $0101;
  NewVer := W <> $0100;
  F.WriteBuffer(W, 2);

  W := 0;
  F.WriteBuffer(W, 2); // iOffsetInstruments
  F.WriteBuffer(W, 2); // iOffsetMusic
  if not SongData_GetWord('CMF_TicksPerQuarter', W) then
  begin
    LogOutput('[*] Warning: Ticks Per Quarter is not defined.');
    W := 0;
  end;
  F.WriteBuffer(W, 2);
  if not SongData_GetWord('CMF_TicksPerSecond', W) then
  begin
    LogOutput('[-] Ticks Per Second is not defined.');
    Exit;
  end;
  F.WriteBuffer(W, 2);
  W := 0;
  F.WriteBuffer(W, 2); // iOffsetTitle
  F.WriteBuffer(W, 2); // iOffsetComposer
  F.WriteBuffer(W, 2); // iOffsetRemarks
  FillChar(iChannelInUse, 16, 0);
  F.WriteBuffer(iChannelInUse, 16);

  iInstrumentCount := 0;
  while SongData_GetStr('CMF_Inst#' + IntToStr(iInstrumentCount), S) do
    Inc(iInstrumentCount);

  if not NewVer then
    F.WriteBuffer(iInstrumentCount, 1)
  else
  begin
    F.WriteBuffer(iInstrumentCount, 2);
    if not SongData_GetWord('CMF_Tempo', W) then
    begin
      LogOutput('[*] Warning: Tempo is not defined.');
      W := 0;
    end;
    F.WriteBuffer(W, 2);
  end;

  if SongData_GetStr('CMF_Title', S) and (Length(S) > 0) then
  begin
    W := F.Position;
    F.Seek($E, soFromBeginning);
    F.WriteBuffer(W, 2);
    F.Seek(W, soFromBeginning);
    F.WriteBuffer(PAnsiChar(S)^, Length(S) + 1);
  end;

  if SongData_GetStr('CMF_Composer', S) and (Length(S) > 0) then
  begin
    W := F.Position;
    F.Seek($10, soFromBeginning);
    F.WriteBuffer(W, 2);
    F.Seek(W, soFromBeginning);
    F.WriteBuffer(PAnsiChar(S)^, Length(S) + 1);
  end;

  if SongData_GetStr('CMF_Remarks', S) and (Length(S) > 0) then
  begin
    W := F.Position;
    F.Seek($12, soFromBeginning);
    F.WriteBuffer(W, 2);
    F.Seek(W, soFromBeginning);
    F.WriteBuffer(PAnsiChar(S)^, Length(S) + 1);
  end;

  if iInstrumentCount > 0 then
  begin
    W := F.Position;
    F.Seek($6, soFromBeginning);
    F.WriteBuffer(W, 2);
    F.Seek(W, soFromBeginning);
    for I := 0 to iInstrumentCount - 1 do
    begin
      SongData_GetArray('CMF_Inst#' + IntToStr(I), CMFInst);
      F.WriteBuffer(CMFInst, SizeOf(CMFInst));
    end;
  end;

  // MIDI Track
  I := TrkCh.ItemIndex;
  if Length(TrackData[I].Data) > 0 then
  begin
    MIDIData := TMemoryStream.Create;
    WriteTrackData(MIDIData, TrackData[I]);

    W := F.Position;
    F.Seek($8, soFromBeginning);
    F.WriteBuffer(W, 2);
    F.Seek(W, soFromBeginning);
    F.WriteBuffer(MIDIData.Memory^, MIDIData.Size);

    LogOutput('[+] Wrote ' + IntToStr(MIDIData.Size) + ' bytes.');
    MIDIData.Free;
    for J := 0 to Length(TrackData[I].Data) - 1 do
      if TrackData[I].Data[J].Status shr 4 = 9 then
        iChannelInUse[TrackData[I].Data[J].Status and $F] := 1;
    F.Seek($14, soFromBeginning);
    F.WriteBuffer(iChannelInUse, 16);
  end;
  F.Seek(0, soFromEnd);
end;

//==============================================================================
//
// ROL_SaveBank
//
//==============================================================================
function ROL_SaveBank(FileName: string): boolean;
var
  M: TMemoryStream;
  S: string;
  numInstruments, W: word;
  dw: DWord;
  Pad: array[0..7] of byte;
  I, J: integer;
  B: byte;
  A: ansistring;
  Name: array[0..8] of AnsiChar;
  Inst: array[0..13 + 13 + 2 - 1] of byte;
  SL: TStringList;
begin
  Result := False;
  numInstruments := 0;
  while SongData_GetStr('BNK_Idx#' + IntToStr(numInstruments), S) do
    Inc(numInstruments);

  if numInstruments = 0 then
    Exit;

  M := TMemoryStream.Create;
  if not SongData_GetWord('BNK_Version', W) then
    W := 1;
  M.WriteBuffer(W, 2);
  if not SongData_GetStr('BNK_Signature', S) then
    S := 'ADLIB-';
  M.WriteBuffer(ansistring(S)[1], 6);
  if not SongData_GetWord('BNK_Used', W) then
    W := numInstruments;
  M.WriteBuffer(W, 2);
  M.WriteBuffer(numInstruments, 2);
  dw := 0;
  M.WriteBuffer(dw, 4);
  M.WriteBuffer(dw, 4);
  FillChar(Pad, SizeOf(Pad), 0);
  M.WriteBuffer(Pad, SizeOf(Pad));

  dw := M.Position;
  M.Seek(12, soFromBeginning);
  M.WriteBuffer(dw, 4);
  M.Seek(dw, soFromBeginning);

  // Important: Sort in alphabetical order
  SL := TStringList.Create;
  for I := 0 to numInstruments - 1 do
  begin
    SongData_GetStr('BNK_Name#' + IntToStr(I), S);
    SL.Add(S);
  end;
  SL.Sort;
  for I := 0 to SL.Count - 1 do
  begin
    for J := 0 to numInstruments - 1 do
    begin
      SongData_GetStr('BNK_Name#' + IntToStr(J), S);
      if S = SL[I] then
        Break;
    end;
    if not SongData_GetWord('BNK_Idx#' + IntToStr(J), W) then
      W := 0;
    M.WriteBuffer(W, 2);
    if not SongData_GetByte('BNK_Flags#' + IntToStr(J), B) then
      B := 0;
    M.WriteBuffer(B, 1);
    FillChar(Name, Length(Name), 0);
    A := ansistring(S);
    Move(A[1], Name[0], Length(A));
    M.WriteBuffer(Name, Length(Name));
  end;
  SL.Free;

  dw := M.Position;
  M.Seek(16, soFromBeginning);
  M.WriteBuffer(dw, 4);
  M.Seek(dw, soFromBeginning);

  for I := 0 to numInstruments - 1 do
  begin
    if not SongData_GetByte('BNK_Perc#' + IntToStr(I), B) then
      B := 0;
    M.WriteBuffer(B, 1);
    if not SongData_GetByte('BNK_Chan#' + IntToStr(I), B) then
      B := 0;
    M.WriteBuffer(B, 1);
    SongData_GetArray('BNK_Data#' + IntToStr(I), Inst);
    M.WriteBuffer(Inst, SizeOf(Inst));
  end;

  Result := True;
  try
    M.SaveToFile(FileName);
  except
    Result := False;
  end;
  M.Free;
end;

//==============================================================================
//
// TXMICore.WriteROL
//
//==============================================================================
procedure TXMICore.WriteROL(var F: TMemoryStream; FileName: string);
var
  I, J: integer;
  S: string;
  Meta: array[0..40 - 1] of byte;
  W: word;
  B: byte;
  Vars: array[0..45 - 1] of word;
  Pad: array[0..38 - 1] of byte;
  Name: array[0..15 - 1] of byte;
  Fl: single;
  Inst: array[0..9 + 1 + 2 - 1] of byte;
begin
  LogOutput('[*] Writing AdLib ROL file...');
  if not SongData_GetInt('ROL_Version', I) then
    I := $40000;
  F.WriteBuffer(I, 4);
  if not SongData_GetStr('ROL_Signature', S) then
    S := '\roll\default';
  if Length(S) > SizeOf(Meta) - 1 then
    SetLength(S, SizeOf(Meta) - 1);
  FillChar(Meta, SizeOf(Meta), 0);
  Move(ansistring(S)[1], Meta, Length(S));
  F.WriteBuffer(Meta, SizeOf(Meta));
  if not SongData_GetWord('ROL_TicksPerBeat', W) then
    W := 4;
  F.WriteBuffer(W, 2);
  if not SongData_GetWord('ROL_BeatPerMeasure', W) then
    W := 4;
  F.WriteBuffer(W, 2);
  if not SongData_GetWord('ROL_ScaleY', W) then
    W := 48;
  F.WriteBuffer(W, 2);
  if not SongData_GetWord('ROL_ScaleX', W) then
    W := 56;
  F.WriteBuffer(W, 2);
  B := 0; // reserved
  F.WriteBuffer(B, 1);
  if not SongData_GetByte('ROL_Melodic', B) then
    B := 0;
  F.WriteBuffer(B, 1);
  FillChar(Vars, SizeOf(Vars), 0);
  F.WriteBuffer(Vars, SizeOf(Vars));
  FillChar(Pad, SizeOf(Pad), 0);
  F.WriteBuffer(Pad, SizeOf(Pad));
  for I := 0 to Length(TrackData) - 1 do
  begin
    S := TrackData[I].Title;
    if Length(S) > SizeOf(Name) - 1 then
      SetLength(S, SizeOf(Name) - 1);
    FillChar(Name, SizeOf(Name), 0);
    Move(ansistring(S)[1], Name, Length(S));
    F.WriteBuffer(Name, SizeOf(Name));
    if I = 0 then
    begin // Write Tempo track
      if not SongData_GetFloat('ROL_BasicTempo', Fl) then
        if not SongData_GetFloat('Division', Fl) then
          Fl := 120;
      F.WriteBuffer(Fl, 4);
      if Length(TrackData[I].Data) > $FFFF then
        SetLength(TrackData[I].Data, $FFFF);
      W := Length(TrackData[I].Data);
      Vars[44] := W;
      F.WriteBuffer(W, 2);
      // Convert ticks from relative to absolute
      ConvertTicks(True, TrackData[I].Data);
      for J := 0 to Length(TrackData[I].Data) - 1 do
      begin
        if TrackData[I].Data[J].Ticks > $FFFF then
          TrackData[I].Data[J].Ticks := $FFFF;
        W := TrackData[I].Data[J].Ticks;
        F.WriteBuffer(W, 2);
        if Length(TrackData[I].Data[J].DataArray) < 1 + 4 then
        begin
          SetLength(TrackData[I].Data[J].DataArray, 1 + 4);
          TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);
        end;
        Move(TrackData[I].Data[J].DataArray[1], Fl, 4);
        F.WriteBuffer(Fl, 4);
      end;
      // Revert
      ConvertTicks(False, TrackData[I].Data);
    end
    else
    if (I - 1) mod 4 = 0 then
    begin // Write Voix # track
      W := 0;
      for J := 0 to Length(TrackData[I].Data) - 1 do
        W := Min($FFFF, W + TrackData[I].Data[J].Ticks);
      Vars[(I - 1) div 4] := W;
      F.WriteBuffer(W, 2);
      if W >= 2 then
      begin
        if (TrackData[I].Data[0].Ticks > 0) and
          (TrackData[I].Data[0].BParm1 > 0) then
        begin
          W := 0;
          F.WriteBuffer(W, 2);
          if TrackData[I].Data[0].Ticks > $FFFF then
            TrackData[I].Data[0].Ticks := $FFFF;
          W := TrackData[I].Data[0].Ticks;
          F.WriteBuffer(W, 2);
        end;
        for J := 0 to Length(TrackData[I].Data) - 2 do
        begin
          W := TrackData[I].Data[J].BParm1;
          F.WriteBuffer(W, 2);
          if TrackData[I].Data[J + 1].Ticks > $FFFF then
            TrackData[I].Data[J + 1].Ticks := $FFFF;
          W := TrackData[I].Data[J + 1].Ticks;
          F.WriteBuffer(W, 2);
        end;
      end;
    end
    else
    begin
      if Length(TrackData[I].Data) > $FFFF then
        SetLength(TrackData[I].Data, $FFFF);
      W := Length(TrackData[I].Data);
      if 11 * ((I - 1) mod 4) + ((I - 1) div 4) < Length(Vars) then
        Vars[11 * ((I - 1) mod 4) + ((I - 1) div 4)] := W;
      F.WriteBuffer(W, 2);
      // Convert ticks from relative to absolute
      ConvertTicks(True, TrackData[I].Data);
      for J := 0 to Length(TrackData[I].Data) - 1 do
        case (I - 1) mod 4 of
          1: // Timbre #
          begin
            if TrackData[I].Data[J].Ticks > $FFFF then
              TrackData[I].Data[J].Ticks := $FFFF;
            W := TrackData[I].Data[J].Ticks;
            F.WriteBuffer(W, 2);
            if Length(TrackData[I].Data[J].DataArray) < 1 + SizeOf(Inst) then
            begin
              SetLength(TrackData[I].Data[J].DataArray, 1 + SizeOf(Inst));
              TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);
            end;
            Move(TrackData[I].Data[J].DataArray[1], Inst, SizeOf(Inst));
            F.WriteBuffer(Inst, SizeOf(Inst));
          end;
          2, 3: // Volume #, Pitch #
          begin
            if TrackData[I].Data[J].Ticks > $FFFF then
              TrackData[I].Data[J].Ticks := $FFFF;
            W := TrackData[I].Data[J].Ticks;
            F.WriteBuffer(W, 2);
            if Length(TrackData[I].Data[J].DataArray) < 1 + 4 then
            begin
              SetLength(TrackData[I].Data[J].DataArray, 1 + 4);
              TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);
            end;
            Move(TrackData[I].Data[J].DataArray[1], Fl, 4);
            F.WriteBuffer(Fl, 4);
          end;
        end;
      // Revert
      ConvertTicks(False, TrackData[I].Data);
    end;
    if I >= 11 * 4 then
      Break;
  end;
  F.Seek($36, soFromBeginning);
  F.WriteBuffer(Vars, SizeOf(Vars));
  F.Seek(0, soFromEnd);

  LogOutput('[*] Saving instrument bank file...');
  S := ChangeFileExt(FileName, '.bnk');
  if not ROL_SaveBank(S) then
    LogOutput('[-] Failed to save instrument bank file.')
  else
    LogOutput('[*] Saved instrument bank: ' + S);
end;

//==============================================================================
//
// MUS_SaveBank
//
//==============================================================================
function MUS_SaveBank(FileName: string): boolean;
var
  M: TMemoryStream;
  W, InstCnt: word;
  I: integer;
  S: string;
  A: ansistring;
  Name: array[0..8] of AnsiChar;
  Inst: array[0..13 + 13 + 2 - 1] of word;
begin
  Result := False;
  InstCnt := 0;
  while SongData_GetStr('SND_Name#' + IntToStr(InstCnt), S) and
    SongData_GetStr('SND_Data#' + IntToStr(InstCnt), S) do
    Inc(InstCnt);

  if InstCnt = 0 then
    Exit;

  M := TMemoryStream.Create;
  if not SongData_GetWord('SND_Version', W) then
    W := 1;
  M.WriteBuffer(W, 2);
  M.WriteBuffer(InstCnt, 2);
  W := InstCnt * 9 + 6;
  M.WriteBuffer(W, 2);

  for I := 0 to InstCnt - 1 do
  begin
    FillChar(Name, Length(Name), 0);
    SongData_GetStr('SND_Name#' + IntToStr(I), S);
    A := S;
    Move(A[1], Name[0], Length(A));
    M.WriteBuffer(Name, Length(Name));
  end;

  for I := 0 to InstCnt - 1 do
  begin
    SongData_GetArray('SND_Data#' + IntToStr(I), Inst);
    M.WriteBuffer(Inst, SizeOf(Inst));
  end;

  Result := True;
  try
    M.SaveToFile(FileName);
  except
    Result := False;
  end;
  M.Free;
end;

//==============================================================================
//
// TXMICore.WriteMUS
//
//==============================================================================
procedure TXMICore.WriteMUS(var F: TMemoryStream; FileName: string);
var
  I, J: integer;
  dw, totalTick, nrCommand: DWORD;
  tuneName: array[0..29] of AnsiChar;
  S, Bank: string;
  A: ansistring;
  pad: array[0..7] of byte;
  MIDIData: TMemoryStream;
  InstCnt: word;
  instName: array[0..8] of AnsiChar;
begin
  if EventProfile = 'ims' then
    LogOutput('[*] Writing AdLib IMS file...')
  else
    LogOutput('[*] Writing AdLib MUS file...');

  if not SongData_GetInt('MUS_Version', I) then
    I := 1;
  F.WriteBuffer(I, 2);
  if not SongData_GetDWord('MUS_ID', dw) then
    dw := 0;
  F.WriteBuffer(dw, 4);

  FillChar(tuneName, Length(tuneName), 0);
  SongData_GetStr('MUS_TuneName', S);
  if Length(S) > Length(tuneName) - 1 then
    SetLength(S, Length(tuneName) - 1);
  A := S;
  Move(A[1], tuneName[0], Length(A));
  F.WriteBuffer(tuneName[0], Length(tuneName));

  if not SongData_GetInt('MUS_TicksPerBeat', I) then
  begin
    LogOutput('[-] Ticks Per Beat is not defined.');
    Exit;
  end;
  F.WriteBuffer(I, 1);

  if not SongData_GetInt('MUS_BeatPerMeasure', I) then
  begin
    LogOutput('[-] Warning: Beat Per Measure is not defined.');
    I := 2;
  end;
  F.WriteBuffer(I, 1);

  dw := 0;
  F.WriteBuffer(dw, 4); // totalTick
  F.WriteBuffer(dw, 4); // dataSize
  F.WriteBuffer(dw, 4); // nrCommand

  // Padding
  FillChar(pad, 8, 0);
  F.WriteBuffer(pad, 8);

  if not SongData_GetInt('MUS_Percussive', I) then
  begin
    LogOutput('[-] Percussive is not defined.');
    Exit;
  end;
  F.WriteBuffer(I, 1);

  if not SongData_GetInt('MUS_PitchBendRange', I) then
  begin
    LogOutput('[-] Pitch Bend Range is not defined.');
    Exit;
  end;
  F.WriteBuffer(I, 1);

  if not SongData_GetInt('MUS_BasicTempo', I) then
  begin
    LogOutput('[-] Basic Tempo is not defined.');
    Exit;
  end;
  F.WriteBuffer(I, 2);

  // Padding
  F.WriteBuffer(pad, 8);

  // MIDI Track
  MIDIData := TMemoryStream.Create;
  I := TrkCh.ItemIndex;
  WriteTrackData_MUS(MIDIData, TrackData[I]);
  dw := MIDIData.Size;
  F.WriteBuffer(MIDIData.Memory^, dw);
  MIDIData.Free;
  LogOutput('[+] Wrote ' + IntToStr(dw) + ' bytes.');

  F.Seek($26, soFromBeginning);
  totalTick := 0;
  for J := 0 to Length(TrackData[I].Data) - 1 do
    totalTick := totalTick + TrackData[I].Data[J].Ticks;
  F.WriteBuffer(totalTick, 4);

  F.WriteBuffer(dw, 4); // dataSize

  nrCommand := 0;
  for J := 0 to Length(TrackData[I].Data) - 1 do
  begin
    if TrackData[I].Data[J].Ticks >= 240 then
      nrCommand := nrCommand + TrackData[I].Data[J].Ticks div 240;
    Inc(nrCommand);
  end;
  F.WriteBuffer(nrCommand, 4);

  F.Seek(0, soFromEnd);

  if EventProfile = 'mus' then
  begin
    LogOutput('[*] Saving instrument bank file...');
    Bank := ChangeFileExt(FileName, '.snd');
    if not MUS_SaveBank(Bank) then
      LogOutput('[-] Failed to save instrument bank file.')
    else
      LogOutput('[*] Saved instrument bank: ' + Bank);
  end;
  if EventProfile = 'ims' then
  begin
    I := $7777;
    F.WriteBuffer(I, 2);
    InstCnt := 0;
    while SongData_GetStr('IMS_Name#' + IntToStr(InstCnt), S) do
      Inc(InstCnt);
    F.WriteBuffer(InstCnt, 2);
    for I := 0 to InstCnt - 1 do
    begin
      FillChar(instName, Length(instName), 0);
      SongData_GetStr('IMS_Name#' + IntToStr(I), S);
      A := ansistring(S);
      if Length(A) > 8 then
        SetLength(A, 8);
      Move(A[1], instName[0], Length(A));
      F.WriteBuffer(instName, Length(instName));
    end;
  end;
end;

//==============================================================================
//
// TXMICore.WriteSOP
//
//==============================================================================
procedure TXMICore.WriteSOP(var F: TMemoryStream);
const
  sign: ansistring = 'sopepos';
var
  ver: DWORD;
  S: string;
  fname: array[0..12] of AnsiChar;
  sname: array[0..30] of AnsiChar;
  B, nInsts: byte;
  chanMode: array of byte;
  I: integer;
  i1name: array[0..7] of AnsiChar;
  i2name: array[0..18] of AnsiChar;
  op2inst: array[0..10] of byte;
  op4inst: array[0..21] of byte;
  evCount: word;
  dwDataSize: DWord;
  MIDIData: TMemoryStream;
begin
  LogOutput('[*] Writing Sopepos'' Note file...');

  F.WriteBuffer(sign[1], Length(sign));
  if not SongData_GetDWord('SOP_Version', ver) then
    ver := $100;
  F.WriteBuffer(ver, 3);

  if not SongData_GetStr('SOP_FileName', S) then
    S := '';
  if Length(S) > 12 then
    SetLength(S, 12);
  FillChar(fname, SizeOf(fname), 0);
  Move(ansistring(S)[1], fname[0], Length(S));
  F.WriteBuffer(fname, SizeOf(fname));

  if not SongData_GetStr('SOP_SongName', S) then
    S := '';
  if Length(S) > 30 then
    SetLength(S, 30);
  FillChar(sname, SizeOf(sname), 0);
  Move(ansistring(S)[1], sname[0], Length(S));
  F.WriteBuffer(sname, SizeOf(sname));

  if not SongData_GetByte('SOP_Percussive', B) then
    B := 0;
  F.WriteBuffer(B, 1);
  B := 0; // padding
  F.WriteBuffer(B, 1);
  if not SongData_GetByte('SOP_TicksPerBeat', B) then
    B := 12;
  F.WriteBuffer(B, 1);
  B := 0; // padding
  F.WriteBuffer(B, 1);
  if not SongData_GetByte('SOP_BeatPerMeasure', B) then
    B := 4;
  F.WriteBuffer(B, 1);
  if not SongData_GetByte('SOP_BasicTempo', B) then
    B := 120;
  F.WriteBuffer(B, 1);

  if not SongData_GetStr('SOP_Comment', S) then
    S := '';
  if Length(S) > 12 then
    SetLength(S, 12);
  FillChar(fname[0], Length(fname), 0);
  Move(ansistring(S)[1], fname[0], Length(S));
  F.WriteBuffer(fname, SizeOf(fname));

  if Length(TrackData) = 0 then
  begin
    SetLength(TrackData, 1);
    SetLength(TrackData[0].Data, 0);
  end;
  B := Length(TrackData) - 1; // nTracks
  F.WriteBuffer(B, 1);
  SetLength(chanMode, B);
  FillChar(chanMode[0], B, 0);
  nInsts := 0;
  while SongData_GetStr('SOP_Inst#' + IntToStr(nInsts), S) do
    Inc(nInsts);
  F.WriteBuffer(nInsts, 1);
  B := 0; // padding
  F.WriteBuffer(B, 1);
  // Channel modes
  SongData_GetArray('SOP_Channels', chanMode);
  F.WriteBuffer(chanMode[0], Length(chanMode));
  // Instruments
  for I := 0 to nInsts - 1 do
  begin
    if not SongData_GetByte('SOP_Inst#' + IntToStr(I), B) then
      B := 1;
    F.WriteBuffer(B, 1);

    FillChar(i1name, SizeOf(i1name), 0);
    if not SongData_GetStr('SOP_SName#' + IntToStr(I), S) then
      S := '';
    if Length(S) > Length(i1name) then
      SetLength(S, Length(i1name));
    Move(ansistring(S)[1], i1name[0], Length(S));
    F.WriteBuffer(i1name, SizeOf(i1name));

    FillChar(i2name, SizeOf(i2name), 0);
    if not SongData_GetStr('SOP_LName#' + IntToStr(I), S) then
      S := '';
    if Length(S) > Length(i2name) then
      SetLength(S, Length(i2name));
    Move(ansistring(S)[1], i2name[0], Length(S));
    F.WriteBuffer(i2name, SizeOf(i2name));

    if B = 12 then // comment
      Continue
    else if B = 0 then // 4OP instrument
    begin
      FillChar(op4inst[0], SizeOf(op4inst), 0);
      SongData_GetArray('SOP_Data#' + IntToStr(I), op4inst);
      F.WriteBuffer(op4inst[0], SizeOf(op4inst));
    end
    else if B <= 10 then // 2OP instrument
    begin
      FillChar(op2inst[0], SizeOf(op2inst), 0);
      SongData_GetArray('SOP_Data#' + IntToStr(I), op2inst);
      F.WriteBuffer(op2inst[0], SizeOf(op2inst));
    end;
  end;

  // Tracks
  for I := 0 to Length(TrackData) - 1 do
  begin
    LogOutput('[*] Writing track ' + IntToStr(I) + '...');
    MIDIData := TMemoryStream.Create;
    WriteTrackData_SOP(MIDIData, TrackData[I]);
    evCount := Length(TrackData[I].Data);
    F.WriteBuffer(evCount, 2);
    dwDataSize := MIDIData.Size;
    F.WriteBuffer(dwDataSize, 4);
    F.WriteBuffer(MIDIData.Memory^, dwDataSize);
    LogOutput('[+] Wrote ' + IntToStr(dwDataSize) + ' bytes.');
    MIDIData.Free;
  end;

  F.Seek(0, soFromEnd);
end;

//==============================================================================
//
// TXMICore.WriteHERAD
//
//==============================================================================
procedure TXMICore.WriteHERAD(var F: TMemoryStream);
const
  MaxTracks = 21;
  InstLen = 40;
var
  W: word;
  I, InstCnt: integer;
  Speed: single;
  Params: array[0..31] of byte;
  MIDIData: TMemoryStream;
  S: string;
  HERADInst: array[0..InstLen - 1] of byte;
begin
  LogOutput('[*] Writing Cryo HERAD music file...');

  W := 0; // InstOffset
  F.WriteBuffer(W, 2);
  for I := 0 to MaxTracks - 1 do
    F.WriteBuffer(W, 2); // Track offsets
  if not SongData_GetWord('HERAD_LoopStart', W) then
    W := 0;
  F.WriteBuffer(W, 2);
  if not SongData_GetWord('HERAD_LoopEnd', W) then
    W := 0;
  F.WriteBuffer(W, 2);
  if not SongData_GetWord('HERAD_LoopCount', W) then
    W := 0;
  F.WriteBuffer(W, 2);
  if not SongData_GetFloat('HERAD_Speed', Speed) then
    Speed := 1;
  W := (Floor(Speed) shl 8) or (Floor(Frac(Speed) * 256) and $FF);
  F.WriteBuffer(W, 2);
  if SongData_GetArray('HERAD_Params', Params) then
    F.WriteBuffer(Params, SizeOf(Params));

  for I := 0 to Length(TrackData) - 1 do
  begin
    if I >= MaxTracks - 1 then
      Break;
    LogOutput('[*] Writing track ' + IntToStr(I) + '...');
    W := F.Position - 2;
    F.Seek(2 + I * 2, soFromBeginning);
    F.WriteBuffer(W, 2);
    F.Seek(2 + W, soFromBeginning);
    MIDIData := TMemoryStream.Create;
    SongData_PutInt('HERAD_TmpIdx', I);
    WriteTrackData_HERAD(MIDIData, TrackData[I]);
    F.WriteBuffer(MIDIData.Memory^, MIDIData.Size);

    LogOutput('[+] Wrote ' + IntToStr(MIDIData.Size) + ' bytes.');
    MIDIData.Free;
  end;
  SongData_Delete('HERAD_TmpIdx');

  W := F.Position;
  F.Seek(0, soFromBeginning);
  F.WriteBuffer(W, 2);
  F.Seek(W, soFromBeginning);

  InstCnt := 0;
  while SongData_GetStr('HERAD_Inst#' + IntToStr(InstCnt), S) do
    Inc(InstCnt);
  for I := 0 to InstCnt - 1 do
  begin
    SongData_GetArray('HERAD_Inst#' + IntToStr(I), HERADInst);
    F.WriteBuffer(HERADInst, SizeOf(HERADInst));
  end;

  F.Seek(0, soFromEnd);
end;

//==============================================================================
//
// TXMICore.WriteRaw
//
//==============================================================================
procedure TXMICore.WriteRaw(var F: TMemoryStream);
var
  I: integer;
begin
  LogOutput('[*] Writing raw MIDI data...');
  I := TrkCh.ItemIndex;
  if EventFormat = 'mid' then
    WriteTrackData(F, TrackData[I]);
  LogOutput('[+] Wrote ' + IntToStr(F.Size) + ' bytes.');
end;

//==============================================================================
//
// TXMICore.WriteSYX
//
//==============================================================================
procedure TXMICore.WriteSYX(var F: TMemoryStream);
var
  I: integer;
begin
  LogOutput('[*] Writing System Exclusive data...');
  I := TrkCh.ItemIndex;
  WriteTrackData_SYX(F, TrackData[I]);
  LogOutput('[+] Wrote ' + IntToStr(F.Size) + ' bytes.');
end;

//==============================================================================
//
// TXMICore.WriteTrackData
//
//==============================================================================
procedure TXMICore.WriteTrackData(var F: TMemoryStream; var Trk: Chunk);
var
  S: PAnsiChar;
  J, K: integer;
begin
  for J := 0 to Length(Trk.Data) - 1 do
  begin
    WriteVarVal(F, Trk.Data[J].Ticks);
    if J = 0 then
      Trk.Data[J].RunStatMode := False
    else
    begin
      if Trk.Data[J].Status <> Trk.Data[J - 1].Status then
        Trk.Data[J].RunStatMode := False;
    end;
    if Trk.Data[J].Status >= $F0 then
      Trk.Data[J].RunStatMode := False;
    if not Trk.Data[J].RunStatMode then
      F.WriteBuffer(Trk.Data[J].Status, 1);
    case Trk.Data[J].Status shr 4 of
      8..11:
      begin
        F.WriteBuffer(Trk.Data[J].BParm1, 1);
        F.WriteBuffer(Trk.Data[J].BParm2, 1);
      end;
      12, 13:
        F.WriteBuffer(Trk.Data[J].BParm1, 1);
      14:
      begin
        Trk.Data[J].BParm1 := Trk.Data[J].Value and 127;
        Trk.Data[J].BParm2 := (Trk.Data[J].Value shr 7) and 127;
        F.WriteBuffer(Trk.Data[J].BParm1, 1);
        F.WriteBuffer(Trk.Data[J].BParm2, 1);
      end;
      15:
      begin
        case Trk.Data[J].Status and 15 of
          0, 7:
          begin
            WriteVarVal(F, Length(Trk.Data[J].DataArray));
            for K := 0 to Length(Trk.Data[J].DataArray) - 1 do
              F.WriteBuffer(Trk.Data[J].DataArray[K], 1);
          end;
          1, 3: F.WriteBuffer(Trk.Data[J].BParm1, 1);
          2:
          begin
            Trk.Data[J].BParm1 := (Trk.Data[J].Value shr 7) and 127;
            Trk.Data[J].BParm2 := Trk.Data[J].Value and 127;
            F.WriteBuffer(Trk.Data[J].BParm1, 1);
            F.WriteBuffer(Trk.Data[J].BParm2, 1);
          end;
          15:
          begin
            F.WriteBuffer(Trk.Data[J].BParm1, 1);
            case Trk.Data[J].BParm1 of
              0:
              begin
                SetLength(Trk.Data[J].DataArray, 2);
                Trk.Data[J].DataArray[0] :=
                  (Trk.Data[J].Value shr 8) and $FF;
                Trk.Data[J].DataArray[1] :=
                  Trk.Data[J].Value and $FF;
                WriteVarVal(F, Length(Trk.Data[J].DataArray));
                for K := 0 to Length(Trk.Data[J].DataArray) - 1 do
                  F.WriteBuffer(Trk.Data[J].DataArray[K], 1);
              end;
              1..7:
              begin
                WriteVarVal(F, Length(Trk.Data[J].DataString));
                S := PAnsiChar(Trk.Data[J].DataString);
                F.WriteBuffer(S^, Length(Trk.Data[J].DataString));
              end;
              32, 33:
              begin
                WriteVarVal(F, 1);
                F.WriteBuffer(Trk.Data[J].Value, 1);
              end;
              81:
              begin
                SetLength(Trk.Data[J].DataArray, 3);
                Trk.Data[J].DataArray[0] :=
                  (Trk.Data[J].Value shr 16) and $FF;
                Trk.Data[J].DataArray[1] :=
                  (Trk.Data[J].Value shr 8) and $FF;
                Trk.Data[J].DataArray[2] :=
                  Trk.Data[J].Value and $FF;
                WriteVarVal(F, Length(Trk.Data[J].DataArray));
                for K := 0 to Length(Trk.Data[J].DataArray) - 1 do
                  F.WriteBuffer(Trk.Data[J].DataArray[K], 1);
              end;
              else
              begin
                WriteVarVal(F, Length(Trk.Data[J].DataArray));
                for K := 0 to Length(Trk.Data[J].DataArray) - 1 do
                  F.WriteBuffer(Trk.Data[J].DataArray[K], 1);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

//==============================================================================
//
// TXMICore.WriteTrackData_XMI
//
//==============================================================================
procedure TXMICore.WriteTrackData_XMI(var F: TMemoryStream; var Trk: Chunk);
var
  S: PAnsiChar;
  J, K: integer;
begin
  for J := 0 to Length(Trk.Data) - 1 do
  begin
    WriteVarVal_XMI(F, Trk.Data[J].Ticks);
    F.WriteBuffer(Trk.Data[J].Status, 1);
    case Trk.Data[J].Status shr 4 of
      8, 9:
      begin
        F.WriteBuffer(Trk.Data[J].BParm1, 1);
        F.WriteBuffer(Trk.Data[J].BParm2, 1);
        WriteVarVal(F, Trk.Data[J].Len);
      end;
      10, 11:
      begin
        F.WriteBuffer(Trk.Data[J].BParm1, 1);
        F.WriteBuffer(Trk.Data[J].BParm2, 1);
      end;
      12, 13:
        F.WriteBuffer(Trk.Data[J].BParm1, 1);
      14:
      begin
        Trk.Data[J].BParm1 := Trk.Data[J].Value and 127;
        Trk.Data[J].BParm2 := (Trk.Data[J].Value shr 7) and 127;
        F.WriteBuffer(Trk.Data[J].BParm1, 1);
        F.WriteBuffer(Trk.Data[J].BParm2, 1);
      end;
      15:
      begin
        case Trk.Data[J].Status and 15 of
          0, 7:
          begin
            WriteVarVal(F, Length(Trk.Data[J].DataArray));
            for K := 0 to Length(Trk.Data[J].DataArray) - 1 do
              F.WriteBuffer(Trk.Data[J].DataArray[K], 1);
          end;
          1, 3: F.WriteBuffer(Trk.Data[J].BParm1, 1);
          2:
          begin
            Trk.Data[J].BParm1 := (Trk.Data[J].Value shr 7) and 127;
            Trk.Data[J].BParm2 := Trk.Data[J].Value and 127;
            F.WriteBuffer(Trk.Data[J].BParm1, 1);
            F.WriteBuffer(Trk.Data[J].BParm2, 1);
          end;
          15:
          begin
            F.WriteBuffer(Trk.Data[J].BParm1, 1);
            case Trk.Data[J].BParm1 of
              0:
              begin
                SetLength(Trk.Data[J].DataArray, 2);
                Trk.Data[J].DataArray[0] :=
                  (Trk.Data[J].Value shr 8) and $FF;
                Trk.Data[J].DataArray[1] :=
                  Trk.Data[J].Value and $FF;
                WriteVarVal(F, Length(Trk.Data[J].DataArray));
                for K := 0 to Length(Trk.Data[J].DataArray) - 1 do
                  F.WriteBuffer(Trk.Data[J].DataArray[K], 1);
              end;
              1..7:
              begin
                WriteVarVal(F, Length(Trk.Data[J].DataString));
                S := PAnsiChar(Trk.Data[J].DataString);
                F.WriteBuffer(S^, Length(Trk.Data[J].DataString));
              end;
              32, 33:
              begin
                WriteVarVal(F, 1);
                F.WriteBuffer(Trk.Data[J].Value, 1);
              end;
              81:
              begin
                SetLength(Trk.Data[J].DataArray, 3);
                Trk.Data[J].DataArray[0] :=
                  (Trk.Data[J].Value shr 16) and $FF;
                Trk.Data[J].DataArray[1] :=
                  (Trk.Data[J].Value shr 8) and $FF;
                Trk.Data[J].DataArray[2] :=
                  Trk.Data[J].Value and $FF;
                WriteVarVal(F, Length(Trk.Data[J].DataArray));
                for K := 0 to Length(Trk.Data[J].DataArray) - 1 do
                  F.WriteBuffer(Trk.Data[J].DataArray[K], 1);
              end;
              else
              begin
                WriteVarVal(F, Length(Trk.Data[J].DataArray));
                for K := 0 to Length(Trk.Data[J].DataArray) - 1 do
                  F.WriteBuffer(Trk.Data[J].DataArray[K], 1);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

//==============================================================================
//
// TXMICore.WriteTrackData_MUS
//
//==============================================================================
procedure TXMICore.WriteTrackData_MUS(var F: TMemoryStream; var Trk: Chunk);
var
  S: PAnsiChar;
  J, K: integer;
begin
  for J := 0 to Length(Trk.Data) - 1 do
  begin
    // modified (MUS format)
    WriteVarVal_MUS(F, Trk.Data[J].Ticks);
    if J = 0 then
      Trk.Data[J].RunStatMode := False
    else
    begin
      if Trk.Data[J].Status <> Trk.Data[J - 1].Status then
        Trk.Data[J].RunStatMode := False;
    end;
    if Trk.Data[J].Status >= $F0 then
      Trk.Data[J].RunStatMode := False;
    if not Trk.Data[J].RunStatMode then
      F.WriteBuffer(Trk.Data[J].Status, 1);
    case Trk.Data[J].Status shr 4 of
      // modified (MUS format)
      8, 9, 11:
      begin
        F.WriteBuffer(Trk.Data[J].BParm1, 1);
        F.WriteBuffer(Trk.Data[J].BParm2, 1);
      end;
      // modified (MUS format)
      10, 12, 13:
        F.WriteBuffer(Trk.Data[J].BParm1, 1);
      14:
      begin
        Trk.Data[J].BParm1 := Trk.Data[J].Value and 127;
        Trk.Data[J].BParm2 := (Trk.Data[J].Value shr 7) and 127;
        F.WriteBuffer(Trk.Data[J].BParm1, 1);
        F.WriteBuffer(Trk.Data[J].BParm2, 1);
      end;
      15:
      begin
        case Trk.Data[J].Status and 15 of
          // modified (MUS format)
          0:
          begin
            if ((Length(Trk.Data[J].DataArray) > 0) and
              (Trk.Data[J].DataArray[Length(Trk.Data[J].DataArray) - 1] <> $F7)) or
              (Length(Trk.Data[J].DataArray) = 0) then
            begin
              SetLength(Trk.Data[J].DataArray, Length(Trk.Data[J].DataArray) + 1);
              Trk.Data[J].DataArray[Length(Trk.Data[J].DataArray) - 1] := $F7;
              Trk.Data[J].Len := Length(Trk.Data[J].DataArray);
            end;
            for K := 0 to Length(Trk.Data[J].DataArray) - 1 do
              F.WriteBuffer(Trk.Data[J].DataArray[K], 1);
          end;
          // modified (MUS format)
          7: ;
          1, 3: F.WriteBuffer(Trk.Data[J].BParm1, 1);
          2:
          begin
            Trk.Data[J].BParm1 := (Trk.Data[J].Value shr 7) and 127;
            Trk.Data[J].BParm2 := Trk.Data[J].Value and 127;
            F.WriteBuffer(Trk.Data[J].BParm1, 1);
            F.WriteBuffer(Trk.Data[J].BParm2, 1);
          end;
          15:
          begin
            F.WriteBuffer(Trk.Data[J].BParm1, 1);
            case Trk.Data[J].BParm1 of
              0:
              begin
                SetLength(Trk.Data[J].DataArray, 2);
                Trk.Data[J].DataArray[0] :=
                  (Trk.Data[J].Value shr 8) and $FF;
                Trk.Data[J].DataArray[1] :=
                  Trk.Data[J].Value and $FF;
                WriteVarVal(F, Length(Trk.Data[J].DataArray));
                for K := 0 to Length(Trk.Data[J].DataArray) - 1 do
                  F.WriteBuffer(Trk.Data[J].DataArray[K], 1);
              end;
              1..7:
              begin
                WriteVarVal(F, Length(Trk.Data[J].DataString));
                S := PAnsiChar(Trk.Data[J].DataString);
                F.WriteBuffer(S^, Length(Trk.Data[J].DataString));
              end;
              32, 33:
              begin
                WriteVarVal(F, 1);
                F.WriteBuffer(Trk.Data[J].Value, 1);
              end;
              81:
              begin
                SetLength(Trk.Data[J].DataArray, 3);
                Trk.Data[J].DataArray[0] :=
                  (Trk.Data[J].Value shr 16) and $FF;
                Trk.Data[J].DataArray[1] :=
                  (Trk.Data[J].Value shr 8) and $FF;
                Trk.Data[J].DataArray[2] :=
                  Trk.Data[J].Value and $FF;
                WriteVarVal(F, Length(Trk.Data[J].DataArray));
                for K := 0 to Length(Trk.Data[J].DataArray) - 1 do
                  F.WriteBuffer(Trk.Data[J].DataArray[K], 1);
              end;
              else
              begin
                WriteVarVal(F, Length(Trk.Data[J].DataArray));
                for K := 0 to Length(Trk.Data[J].DataArray) - 1 do
                  F.WriteBuffer(Trk.Data[J].DataArray[K], 1);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

//==============================================================================
//
// TXMICore.WriteTrackData_SOP
//
//==============================================================================
procedure TXMICore.WriteTrackData_SOP(var F: TMemoryStream; var Trk: Chunk);
var
  I: integer;
  W: word;
  B: byte;
begin
  for I := 0 to Length(Trk.Data) - 1 do
  begin
    W := Trk.Data[I].Ticks;
    F.WriteBuffer(W, 2);
    case Trk.Data[I].Status shr 4 of
      $9:
      begin
        B := 2; // Play Note
        F.WriteBuffer(B, 1);
        F.WriteBuffer(Trk.Data[I].BParm1, 1);
        W := Trk.Data[I].Len;
        F.WriteBuffer(W, 2);
      end;
      $B:
      begin
        case Trk.Data[I].BParm1 of
          7:
          begin
            B := 4; // Change Volume
            F.WriteBuffer(B, 1);
            F.WriteBuffer(Trk.Data[I].BParm2, 1);
          end;
          9:
          begin
            B := 5; // Change Pitch
            F.WriteBuffer(B, 1);
            F.WriteBuffer(Trk.Data[I].BParm2, 1);
          end;
          10:
          begin
            B := 7; // Set Panning
            F.WriteBuffer(B, 1);
            case Trk.Data[I].BParm2 of
              0: B := 2;
              64: B := 1;
              127: B := 0;
              else
                B := 1;
            end;
            F.WriteBuffer(B, 1);
          end;
          16:
          begin
            B := 8; // Global Volume
            F.WriteBuffer(B, 1);
            F.WriteBuffer(Trk.Data[I].BParm2, 1);
          end;
          17:
          begin
            B := 3; // Change Tempo
            F.WriteBuffer(B, 1);
            F.WriteBuffer(Trk.Data[I].BParm2, 1);
          end;
          18:
          begin
            B := 1; // Special Event
            F.WriteBuffer(B, 1);
            F.WriteBuffer(Trk.Data[I].BParm2, 1);
          end;
          else
          begin
            B := 1; // Special Event
            F.WriteBuffer(B, 1);
            F.WriteBuffer(Trk.Data[I].BParm2, 1);
          end;
        end;
      end;
      $C:
      begin
        B := 6; // Program Change
        F.WriteBuffer(B, 1);
        F.WriteBuffer(Trk.Data[I].BParm1, 1);
      end;
      else
      begin
        B := 1; // Special Event
        F.WriteBuffer(B, 1);
        F.WriteBuffer(Trk.Data[I].BParm1, 1);
      end;
    end;
  end;
end;

//==============================================================================
//
// TXMICore.WriteTrackData_HERAD
//
//==============================================================================
procedure TXMICore.WriteTrackData_HERAD(var F: TMemoryStream; var Trk: Chunk);
var
  I, Ch: integer;
  isM32, isV2: boolean;
  S: string;
  B: byte;
begin
  if not SongData_GetInt('HERAD_TmpIdx', Ch) then
    Ch := 0;
  isM32 := not SongData_GetStr('HERAD_Inst#0', S);
  I := 0;
  SongData_GetInt('HERAD_V2', I);
  isV2 := I > 0;
  for I := 0 to Length(Trk.Data) - 1 do
  begin
    WriteVarVal(F, Trk.Data[I].Ticks);
    B := Trk.Data[I].Status;
    if (not isM32) and (B shr 4 < 15) and (Ch > 0) then
      B := B and $F0;
    F.WriteBuffer(B, 1);
    case Trk.Data[I].Status shr 4 of
      8:
      begin
        if not isV2 then
        begin
          F.WriteBuffer(Trk.Data[I].BParm1, 1);
          F.WriteBuffer(Trk.Data[I].BParm2, 1);
        end
        else
          F.WriteBuffer(Trk.Data[I].BParm1, 1);
      end;
      9..11:
      begin
        F.WriteBuffer(Trk.Data[I].BParm1, 1);
        F.WriteBuffer(Trk.Data[I].BParm2, 1);
      end;
      12, 13:
        F.WriteBuffer(Trk.Data[I].BParm1, 1);
      14:
      begin
        if isM32 then
        begin
          Trk.Data[I].BParm1 := Trk.Data[I].Value and 127;
          Trk.Data[I].BParm2 := (Trk.Data[I].Value shr 7) and 127;
          F.WriteBuffer(Trk.Data[I].BParm1, 1);
          F.WriteBuffer(Trk.Data[I].BParm2, 1);
        end
        else
        begin
          Trk.Data[I].BParm1 := 0;
          Trk.Data[I].BParm2 := Trk.Data[I].Value shr 7;
          F.WriteBuffer(Trk.Data[I].BParm2, 1);
        end;
      end;
      15:
      begin
        B := Trk.Data[I].Value;
        while B > 0 do
        begin
          F.WriteBuffer(Trk.Data[I].Status, 1);
          Dec(B);
        end;
      end;
    end;
  end;
end;

//==============================================================================
//
// TXMICore.WriteTrackData_SYX
//
//==============================================================================
procedure TXMICore.WriteTrackData_SYX(var F: TMemoryStream; var Trk: Chunk);
var
  I: integer;
  B: byte;
begin
  for I := 0 to Length(Trk.Data) - 1 do
    if Trk.Data[I].Status = $F0 then
    begin
      B := $F0;
      F.WriteBuffer(B, 1);
      F.WriteBuffer(Trk.Data[I].DataArray[0], Trk.Data[I].Len);
      if (Trk.Data[I].Len > 0) and
        (Trk.Data[I].DataArray[Trk.Data[I].Len - 1] = $F7) then
      begin

      end
      else
      begin
        B := $F7;
        F.WriteBuffer(B, 1);
      end;
    end;
end;

//==============================================================================
//
// ROL_MIDIDrum
//
//==============================================================================
procedure ROL_MIDIDrum(var C: Command);
var
  B: byte;
begin
  // convert AdLib ROL drums to MIDI
  case C.Status and 15 of
    6: // ch06 - Bass Drum
    begin
      C.Status := (C.Status and $F0) or 9;
      if C.BParm1 <= 36 then
        B := 35
      else
        B := 36;
      C.BParm1 := B;
    end;
    7: // ch07 - Snare Drum
    begin
      C.Status := (C.Status and $F0) or 9;
      if C.BParm1 <= 40 then
        B := 38
      else
        B := 40;
      C.BParm1 := B;
    end;
    8: // ch08 - Tom
    begin
      C.Status := (C.Status and $F0) or 9;
      B := 45;
      if C.BParm1 < 39 then
        B := 41;
      if C.BParm1 = 39 then
        B := 43;
      if C.BParm1 = 40 then
        B := 45;
      if C.BParm1 = 41 then
        B := 47;
      if C.BParm1 = 42 then
        B := 48;
      if C.BParm1 > 42 then
        B := 50;
      C.BParm1 := B;
    end;
    9: // ch09 - Cymbal
    begin
      B := 57;
      if C.BParm1 <= 50 then
        B := 49;
      if (C.BParm1 > 50) and (C.BParm1 <= 65) then
        B := 57;
      if C.BParm1 > 65 then
        B := 55;
      C.BParm1 := B;
    end;
    10: // ch10 - Hi-Hat
    begin
      C.Status := (C.Status and $F0) or 9;
      if C.BParm1 <= 44 then
        B := 42
      else
        B := 44;
      C.BParm1 := B;
    end;
  end;
end;

//==============================================================================
//
// CMF_MIDIDrum
//
//==============================================================================
procedure CMF_MIDIDrum(var C: Command);
var
  B: byte;
begin
  // convert CMF drums to MIDI
  case C.Status and 15 of
    11: // ch11 - Bass Drum
    begin
      C.Status := (C.Status and $F0) or 9;
      if C.BParm1 <= 36 then
        B := 35
      else
        B := 36;
      C.BParm1 := B;
    end;
    12: // ch12 - Snare Drum
    begin
      C.Status := (C.Status and $F0) or 9;
      if C.BParm1 <= 40 then
        B := 38
      else
        B := 40;
      C.BParm1 := B;
    end;
    13: // ch13 - Tom
    begin
      C.Status := (C.Status and $F0) or 9;
      B := 45;
      if C.BParm1 < 39 then
        B := 41;
      if C.BParm1 = 39 then
        B := 43;
      if C.BParm1 = 40 then
        B := 45;
      if C.BParm1 = 41 then
        B := 47;
      if C.BParm1 = 42 then
        B := 48;
      if C.BParm1 > 42 then
        B := 50;
      C.BParm1 := B;
    end;
    14: // ch14 - Cymbal
    begin
      C.Status := (C.Status and $F0) or 9;
      B := 57;
      if C.BParm1 <= 50 then
        B := 49;
      if (C.BParm1 > 50) and (C.BParm1 <= 65) then
        B := 57;
      if C.BParm1 > 65 then
        B := 55;
      C.BParm1 := B;
    end;
    15: // ch15 - Hi-Hat
    begin
      C.Status := (C.Status and $F0) or 9;
      if C.BParm1 <= 44 then
        B := 42
      else
        B := 44;
      C.BParm1 := B;
    end;
  end;
end;

//==============================================================================
//
// TXMICore.Convert_MID_FixTempo
//
//==============================================================================
procedure TXMICore.Convert_MID_FixTempo;
var
  InitTempo: cardinal;
begin
  if not SongData_GetDWord('InitTempo', InitTempo) then
    InitTempo := MIDIStdTempo;
  if InitTempo = MIDIStdTempo then
    Exit;
  if Length(TrackData) = 0 then
    Exit;
  NewEvent(0, 0, $FF, $51);
  TrackData[0].Data[0].Value := InitTempo;
  SongData_PutDWord('InitTempo', MIDIStdTempo);
end;

//==============================================================================
//
// TXMICore.Convert_XMI_MID
//
//==============================================================================
procedure TXMICore.Convert_XMI_MID;
type
  NoteDur = packed record
    Chn: byte;
    Note: byte;
    Ticks: UInt64;
  end;
  PNoteDur = ^NoteDur;
var
  InitTempo, Tempo: cardinal;
  I, J, K: integer;
  Durations: TList;
  PDur: PNoteDur;
  MinDur: UInt64;
  MinDurFirst: boolean;
  FoundTicks: boolean;
begin
  LogOutput('[*] Converting Extended MIDI to Standard MIDI...');
  Application.ProcessMessages;
  if not SongData_GetDWord('InitTempo', InitTempo) then
    InitTempo := MIDIStdTempo;
  Tempo := InitTempo;
  for I := 0 to Length(TrackData) - 1 do
  begin
    // Step 1: Convert note events to Note On/Off
    J := 0;
    Durations := TList.Create;
    while J < Length(TrackData[I].Data) do
    begin
      while TrackData[I].Data[J].Ticks > 0 do
      begin
        // Find notes with minimum duration
        // which can be turned off now
        FoundTicks := False;
        MinDur := 0;
        for K := 0 to Durations.Count - 1 do
        begin
          PDur := Durations[K];
          if not FoundTicks then
          begin
            if (PDur^.Ticks <= TrackData[I].Data[J].Ticks) then
            begin
              MinDur := PDur^.Ticks;
              FoundTicks := True;
            end;
          end
          else if (PDur^.Ticks <= TrackData[I].Data[J].Ticks) and
            (PDur^.Ticks < MinDur) then
            MinDur := PDur^.Ticks;
        end;
        if FoundTicks then
        begin
          // Found notes which needs to off
          K := 0;
          MinDurFirst := True;
          while K < Durations.Count do
          begin
            PDur := Durations[K];
            if PDur^.Ticks = MinDur then
            begin
              // Adding NoteOff event
              NewEvent(I, J, $80, 0);
              TrackData[I].Data[J].Status := $80 or (PDur^.Chn and $F);
              TrackData[I].Data[J].BParm1 := PDur^.Note;
              if MinDurFirst then
                TrackData[I].Data[J].Ticks := PDur^.Ticks
              else
                TrackData[I].Data[J].Ticks := 0;
              Inc(J);
              MinDurFirst := False;
              Dispose(PDur);
              Durations.Delete(K);
              Continue;
            end;
            // Decreasing duration
            PDur^.Ticks := PDur^.Ticks - MinDur;
            Inc(K);
          end;
          TrackData[I].Data[J].Ticks := TrackData[I].Data[J].Ticks - MinDur;
        end
        else
        begin
          for K := 0 to Durations.Count - 1 do
          begin
            // Decrease all durations by ticks
            PDur := Durations[K];
            PDur^.Ticks := PDur^.Ticks - TrackData[I].Data[J].Ticks;
          end;
          Break;
        end;
      end;

      case TrackData[I].Data[J].Status shr 4 of
        8:
        begin // Note Off - unused
          DelEvent(I, J, True);
          Continue;
        end;
        9:
        begin // XMI Note
          // Read note duration
          if TrackData[I].Data[J].Len = 0 then
          begin
            // Adding NoteOff event
            NewEvent(I, J + 1, $80, 0);
            TrackData[I].Data[J + 1].Status := $80 or (TrackData[I].Data[J].Status and $F);
            TrackData[I].Data[J + 1].BParm1 := TrackData[I].Data[J].BParm1;
            Inc(J);
          end
          else
          begin
            PDur := New(PNoteDur);
            PDur^.Chn := TrackData[I].Data[J].Status and $F;
            PDur^.Note := TrackData[I].Data[J].BParm1;
            PDur^.Ticks := TrackData[I].Data[J].Len;
            Durations.Add(PDur);
            TrackData[I].Data[J].Len := 0;
          end;
        end;
      end;
      if (Durations.Count > 0) and (J = High(TrackData[I].Data)) and
        (TrackData[I].Data[J].Status = $FF) and (TrackData[I].Data[J].BParm1 = $2F) then
      begin
        TrackData[I].Data[J].Ticks := 0;
        for K := 0 to Durations.Count - 1 do
        begin
          PDur := Durations[K];
          if TrackData[I].Data[J].Ticks < PDur^.Ticks then
            TrackData[I].Data[J].Ticks := PDur^.Ticks;
        end;
        Continue;
      end;
      Inc(J);
    end;
    if Durations.Count > 0 then
      LogOutput('[*] Warning: There are ' + IntToStr(Durations.Count) +
        ' active notes at track end.');
    for J := 0 to Durations.Count - 1 do
    begin
      PDur := Durations[J];
      Dispose(PDur);
    end;
    Durations.Free;
    // Step 2: Shape the delay ticks using Tempo events as a reference
    for J := 0 to Length(TrackData[I].Data) - 1 do
    begin
      if TrackData[I].Data[J].Ticks > 0 then
        TrackData[I].Data[J].Ticks :=
          Round(TrackData[I].Data[J].Ticks * InitTempo / Tempo);
      if (TrackData[I].Data[J].Status = $FF) and
        (TrackData[I].Data[J].BParm1 = 81) then
        Tempo := TrackData[I].Data[J].Value;
    end;
    LogOutput('[+] Track #' + IntToStr(I) + ': ' + IntToStr(
      Length(TrackData[I].Data)) + ' events converted.');
  end;
  LogOutput('[+] Done.');
end;

//==============================================================================
//
// TXMICore.Convert_MID_XMI
//
//==============================================================================
procedure TXMICore.Convert_MID_XMI;
type
  TNote = record
    Chan, Note: byte;
    Index: integer;
    Duration: UInt64;
  end;
  PNote = ^TNote;
const
  PPQN = 60;
var
  Ver, Division: word;
  InitTempo, Tempo: cardinal;
  I, J, K: integer;
  Notes: TList;
  NoteDur: PNote;
begin
  LogOutput('[*] Converting Standard MIDI to Extended MIDI...');
  Application.ProcessMessages;
  if not SongData_GetDWord('InitTempo', InitTempo) then
    InitTempo := MIDIStdTempo;
  Tempo := InitTempo;
  SongData_GetWord('MIDIType', Ver);
  SongData_GetWord('Division', Division);
  if Ver = 1 then
  begin
    LogOutput('[*] Warning: MIDI Type-1 is not supported by XMIDI.');
    MergeTracksByTicks([]);
  end;
  for I := 0 to Length(TrackData) - 1 do
  begin
    // Step 1: Flatten frequency at 120 Hz (Tempo 500000 @ 60 PPQN)
    for J := 0 to Length(TrackData[I].Data) - 1 do
    begin
      if TrackData[I].Data[J].Ticks > 0 then
        TrackData[I].Data[J].Ticks :=
          Round(TrackData[I].Data[J].Ticks * PPQN * Tempo / (Division * InitTempo));
      if (TrackData[I].Data[J].Status = $FF) and
        (TrackData[I].Data[J].BParm1 = 81) then
        Tempo := TrackData[I].Data[J].Value;
    end;
    // Step 2: Convert note on/off to durations
    Notes := TList.Create;
    J := 0;
    while J < Length(TrackData[I].Data) do
    begin
      if TrackData[I].Data[J].Ticks > 0 then
        for K := 0 to Notes.Count - 1 do
        begin
          NoteDur := Notes[K];
          NoteDur.Duration := NoteDur.Duration + TrackData[I].Data[J].Ticks;
        end;
      case TrackData[I].Data[J].Status shr 4 of
        8:
        begin // Note Off
          K := 0;
          while K < Notes.Count do
          begin
            NoteDur := Notes[K];
            if (TrackData[I].Data[J].Status and $F = NoteDur.Chan) and
              (TrackData[I].Data[J].BParm1 = NoteDur.Note) then
            begin
              TrackData[I].Data[NoteDur.Index].Len := NoteDur.Duration;
              Dispose(NoteDur);
              Notes.Delete(K);
              Continue;
            end;
            Inc(K);
          end;
        end;
        9:
        begin // Note On
          if TrackData[I].Data[J].BParm2 = 0 then
          begin // Velocity = 0 - this is Note Off
            K := 0;
            while K < Notes.Count do
            begin
              NoteDur := Notes[K];
              if (TrackData[I].Data[J].Status and $F = NoteDur.Chan) and
                (TrackData[I].Data[J].BParm1 = NoteDur.Note) then
              begin
                TrackData[I].Data[NoteDur.Index].Len := NoteDur.Duration;
                Dispose(NoteDur);
                Notes.Delete(K);
                Continue;
              end;
              Inc(K);
            end;
          end
          else
          begin
            New(NoteDur);
            NoteDur.Chan := TrackData[I].Data[J].Status and $F;
            NoteDur.Note := TrackData[I].Data[J].BParm1;
            NoteDur.Index := J;
            NoteDur.Duration := 0;
            Notes.Add(NoteDur);
          end;
        end;
      end;
      Inc(J);
    end;
    // Step 3: Remove note off events
    J := 0;
    while J < Length(TrackData[I].Data) do
    begin
      if (TrackData[I].Data[J].Status shr 4 = 8) or
        ((TrackData[I].Data[J].Status shr 4 = 9) and
        (TrackData[I].Data[J].BParm2 = 0)) then
      begin
        DelEvent(I, J, True);
        // Update indexes
        for K := 0 to Notes.Count - 1 do
        begin
          NoteDur := Notes[K];
          if NoteDur.Index > J then
            Dec(NoteDur.Index);
        end;
        Continue;
      end;
      Inc(J);
    end;
    // Step 4: Turn off remaining notes
    for J := 0 to Notes.Count - 1 do
    begin
      NoteDur := Notes[J];
      TrackData[I].Data[NoteDur.Index].Len := NoteDur.Duration;
      Dispose(NoteDur);
    end;
    Notes.Free;
    LogOutput('[+] Track #' + IntToStr(I) + ': ' + IntToStr(
      Length(TrackData[I].Data)) + ' events converted.');
  end;
  SongData_PutInt('MIDIType', 2);
  SongData_PutInt('Division', PPQN);
  LogOutput('[+] Done.');
end;

//==============================================================================
//
// TXMICore.Convert_MUS_MID
//
//==============================================================================
procedure TXMICore.Convert_MUS_MID;
var
  TPB: word;
  NewTempo: cardinal;
  TickFactor: double;
  I, J: integer;
  B: byte;
  Division: word;
  Rhythm: boolean;
  Volumes: array[0..15] of shortint;
  SName: string;
begin
  LogOutput('[*] Converting AdLib MUS to Standard MIDI...');
  Application.ProcessMessages;
  if not SongData_GetWord('MUS_TicksPerBeat', TPB) then
  begin
    LogOutput('[-] Ticks Per Beat is not defined.');
    Exit;
  end;
  NewTempo := MIDIStdTempo;
  TickFactor := 60000000 / TPB / NewTempo;
  if SongData_GetInt('MUS_Percussive', I) then
    Rhythm := I > 0
  else
    Rhythm := False;
  for I := 0 to Length(TrackData) - 1 do
  begin
    // Step 1: Convert events
    FillChar(Volumes, SizeOf(Volumes), -1);
    J := 0;
    while J < Length(TrackData[I].Data) do
    begin
      // Filter events on unused channels
      if TrackData[I].Data[J].Status shr 4 < 15 then // Not System event
        if (Rhythm and (TrackData[I].Data[J].Status and $F > 10)) or
          ((not Rhythm) and (TrackData[I].Data[J].Status and $F > 8)) then
        begin
          DelEvent(I, J, True);
          Continue;
        end;
      // Process events
      case TrackData[I].Data[J].Status shr 4 of
        8: // Note Off
        begin
          if TrackData[I].Data[J].BParm2 > 0 then
          begin
            if Volumes[TrackData[I].Data[J].Status and $F] <>
              shortint(TrackData[I].Data[J].BParm2) then
            begin
              Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm2;
              NewEvent(I, J, $B0, 7);
              TrackData[I].Data[J].Ticks := TrackData[I].Data[J + 1].Ticks;
              TrackData[I].Data[J].Status :=
                $B0 or TrackData[I].Data[J + 1].Status and $F;
              TrackData[I].Data[J].BParm1 := 7;
              TrackData[I].Data[J].BParm2 := Volumes[TrackData[I].Data[J].Status and $F];
              Inc(J);
              TrackData[I].Data[J].Ticks := 0;
            end;
            TrackData[I].Data[J].BParm2 := 0;
          end;
          if Rhythm then
            ROL_MIDIDrum(TrackData[I].Data[J]);
        end;
        9: // Note On
        begin
          if TrackData[I].Data[J].BParm2 > 0 then
          begin
            if Volumes[TrackData[I].Data[J].Status and $F] <>
              shortint(TrackData[I].Data[J].BParm2) then
            begin
              Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm2;
              NewEvent(I, J, $B0, 7);
              TrackData[I].Data[J].Ticks := TrackData[I].Data[J + 1].Ticks;
              TrackData[I].Data[J].Status :=
                $B0 or TrackData[I].Data[J + 1].Status and $F;
              TrackData[I].Data[J].BParm1 := 7;
              TrackData[I].Data[J].BParm2 := Volumes[TrackData[I].Data[J].Status and $F];
              Inc(J);
              TrackData[I].Data[J].Ticks := 0;
            end;
            TrackData[I].Data[J].BParm2 := 127;
          end;
          if Rhythm then
            ROL_MIDIDrum(TrackData[I].Data[J]);
        end;
        10: // Poly Aftertouch -> Volume Change
        begin
          if Volumes[TrackData[I].Data[J].Status and $F] <>
            shortint(TrackData[I].Data[J].BParm1) then
          begin
            Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm1;
            TrackData[I].Data[J].Status := $B0 or TrackData[I].Data[J].Status and $F;
            TrackData[I].Data[J].BParm1 := 7;
            TrackData[I].Data[J].BParm2 := Volumes[TrackData[I].Data[J].Status and $F];
          end
          else
          begin
            DelEvent(I, J, True);
            Continue;
          end;
        end;
        12: // Program Change
        begin
          if Rhythm and (TrackData[I].Data[J].Status and $F > 5) then
          begin // Unused on rhythm channels
            DelEvent(I, J, True);
            Continue;
          end;
          if SongData_GetStr('SND_Name#' + IntToStr(TrackData[I].Data[J].BParm1),
            SName) then
          begin // Insert instrument name
            NewEvent(I, J, $FF, $04);
            TrackData[I].Data[J].DataString := SName;
            Inc(J);
          end;
        end;
        15: // System Event
        begin
          case TrackData[I].Data[J].Status and $F of
            0: // SysEx
            begin
              if (TrackData[I].Data[J].Len = 5) and
                (TrackData[I].Data[J].DataArray[0] = $7F) then
              begin
                // Tempo
                TrackData[I].Data[J].Status := $FF;
                TrackData[I].Data[J].BParm1 := 81;

                if ((TrackData[I].Data[J].DataArray[3] / 128) +
                  TrackData[I].Data[J].DataArray[2]) <> 0 then
                  TrackData[I].Data[J].Value :=
                    Round(NewTempo / ((TrackData[I].Data[J].DataArray[3] / 128) +
                    TrackData[I].Data[J].DataArray[2]))
                else
                  TrackData[I].Data[J].Value := NewTempo;

                TrackData[I].Data[J].DataArray[0] :=
                  (TrackData[I].Data[J].Value shr 16) and $FF;
                TrackData[I].Data[J].DataArray[1] :=
                  (TrackData[I].Data[J].Value shr 8) and $FF;
                TrackData[I].Data[J].DataArray[2] :=
                  (TrackData[I].Data[J].Value) and $FF;

                SetLength(TrackData[I].Data[J].DataArray, 3);
                TrackData[I].Data[J].Len := 3;
              end;
            end;
            $C: // End Of Track
            begin
              TrackData[I].Data[J].Status := $FF;
              TrackData[I].Data[J].BParm1 := $2F;
            end;
          end;
        end;
      end;
      Inc(J);
    end;
    // Step 2: Convert ticks
    for J := 0 to Length(TrackData[I].Data) - 1 do
      if TrackData[I].Data[J].Ticks > 0 then
        TrackData[I].Data[J].Ticks := Round(TrackData[I].Data[J].Ticks * TickFactor);
    // Step 3: Insert pitch bend ranges
    SongData_GetByte('MUS_PitchBendRange', B);
    if B > 1 then
    begin
      for J := 0 to 5 do
      begin
        NewEvent(I, J * 5 + 0, $B0, $64);
        TrackData[I].Data[J * 5 + 0].Status := $B0 or J;
        TrackData[I].Data[J * 5 + 0].BParm2 := 0;
        NewEvent(I, J * 5 + 1, $B0, $65);
        TrackData[I].Data[J * 5 + 1].Status := $B0 or J;
        TrackData[I].Data[J * 5 + 1].BParm2 := 0;
        NewEvent(I, J * 5 + 2, $B0, 6);
        TrackData[I].Data[J * 5 + 2].Status := $B0 or J;
        TrackData[I].Data[J * 5 + 2].BParm2 := B;
        NewEvent(I, J * 5 + 3, $B0, $64);
        TrackData[I].Data[J * 5 + 3].Status := $B0 or J;
        TrackData[I].Data[J * 5 + 3].BParm2 := $7F;
        NewEvent(I, J * 5 + 4, $B0, $65);
        TrackData[I].Data[J * 5 + 4].Status := $B0 or J;
        TrackData[I].Data[J * 5 + 4].BParm2 := $7F;
      end;
      if not Rhythm then
        for J := 6 to 8 do
        begin
          NewEvent(I, J * 5 + 0, $B0, $64);
          TrackData[I].Data[J * 5 + 0].Status := $B0 or J;
          TrackData[I].Data[J * 5 + 0].BParm2 := 0;
          NewEvent(I, J * 5 + 1, $B0, $65);
          TrackData[I].Data[J * 5 + 1].Status := $B0 or J;
          TrackData[I].Data[J * 5 + 1].BParm2 := 0;
          NewEvent(I, J * 5 + 2, $B0, 6);
          TrackData[I].Data[J * 5 + 2].Status := $B0 or J;
          TrackData[I].Data[J * 5 + 2].BParm2 := B;
          NewEvent(I, J * 5 + 3, $B0, $64);
          TrackData[I].Data[J * 5 + 3].Status := $B0 or J;
          TrackData[I].Data[J * 5 + 3].BParm2 := $7F;
          NewEvent(I, J * 5 + 4, $B0, $65);
          TrackData[I].Data[J * 5 + 4].Status := $B0 or J;
          TrackData[I].Data[J * 5 + 4].BParm2 := $7F;
        end;
    end;
    // Step 4: Insert tune name
    if (I = 0) and SongData_GetStr('MUS_TuneName', SName) then
      if SName <> '' then
      begin
        NewEvent(I, 0, $FF, $03);
        TrackData[I].Data[0].DataString := ansistring(SName);
      end;
    LogOutput('[+] Track #' + IntToStr(I) + ': ' + IntToStr(
      Length(TrackData[I].Data)) + ' events converted.');
  end;
  SongData_GetWord('Division', Division);
  SongData.Strings.Clear;

  SongData_PutInt('MIDIType', 0);
  SongData_PutDWord('InitTempo', NewTempo);
  SongData_PutInt('SMPTE', 0);
  SongData_PutInt('Division', Division);
  LogOutput('[+] Done.');
end;

//==============================================================================
//
// TXMICore.Convert_MUS_MDI
//
//==============================================================================
procedure TXMICore.Convert_MUS_MDI;
var
  TPB: word;
  NewTempo: cardinal;
  TickFactor: double;
  I, J, K: integer;
  W: word;
  SName, SData: string;
  SL: TStringList;
  Division: word;
begin
  LogOutput('[*] Converting AdLib MUS to AdLib MDI...');
  Application.ProcessMessages;
  if not SongData_GetWord('MUS_TicksPerBeat', TPB) then
  begin
    LogOutput('[-] Ticks Per Beat is not defined.');
    Exit;
  end;
  NewTempo := MIDIStdTempo;
  TickFactor := 60000000 / TPB / NewTempo;
  for I := 0 to Length(TrackData) - 1 do
  begin
    // Step 1: Convert events
    J := 0;
    while J < Length(TrackData[I].Data) do
    begin
      // Process events
      case TrackData[I].Data[J].Status shr 4 of
        10: // Volume Change A# xx -> D# xx
        begin
          TrackData[I].Data[J].Status := $D0 or TrackData[I].Data[J].Status and $F;
        end;
        12: // Program Change
        begin
          SName := '';
          if SongData_GetStr('SND_Data#' + IntToStr(TrackData[I].Data[J].BParm1),
            SData) then
          begin
            SongData_GetStr('SND_Name#' + IntToStr(TrackData[I].Data[J].BParm1), SName);
            SetLength(TrackData[I].Data[J].DataArray, 6 + 28);
            TrackData[I].Data[J].DataArray[0] := $00; // Ad Lib ID
            TrackData[I].Data[J].DataArray[1] := $00;
            TrackData[I].Data[J].DataArray[2] := $3F;
            TrackData[I].Data[J].DataArray[3] := $00; // Opcode = Load patch
            TrackData[I].Data[J].DataArray[4] := $01;
            TrackData[I].Data[J].DataArray[5] :=      // Channel
              TrackData[I].Data[J].Status and $F;
            SL := TStringList.Create;
            SL.Delimiter := ' ';
            SL.DelimitedText := SData;
            for K := 0 to (13 + 13 + 2) - 1 do
            begin
              try
                W := StrToInt(SL[K]);
              except
                W := 0;
              end;
              W := W and $FF;
              TrackData[I].Data[J].DataArray[6 + K] := W;
            end;
            SL.Free;
            TrackData[I].Data[J].Status := $FF; // Meta Event
            TrackData[I].Data[J].BParm1 := $7F; // Sequencer-Specific
            TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);
            NewEvent(I, J, $FF, $04); // Insert instrument name
            TrackData[I].Data[J].DataString := SName;
            Inc(J);
          end
          else
          begin
            // Instrument not found
            DelEvent(I, J, True);
            Continue;
          end;
        end;
        15: // System Event
        begin
          case TrackData[I].Data[J].Status and $F of
            0: // SysEx
            begin
              if (TrackData[I].Data[J].Len = 5) and
                (TrackData[I].Data[J].DataArray[0] = $7F) then
              begin
                // Tempo
                TrackData[I].Data[J].Status := $FF;
                TrackData[I].Data[J].BParm1 := 81;

                if ((TrackData[I].Data[J].DataArray[3] / 128) +
                  TrackData[I].Data[J].DataArray[2]) <> 0 then
                  TrackData[I].Data[J].Value :=
                    Round(NewTempo / ((TrackData[I].Data[J].DataArray[3] / 128) +
                    TrackData[I].Data[J].DataArray[2]))
                else
                  TrackData[I].Data[J].Value := NewTempo;

                TrackData[I].Data[J].DataArray[0] :=
                  (TrackData[I].Data[J].Value shr 16) and $FF;
                TrackData[I].Data[J].DataArray[1] :=
                  (TrackData[I].Data[J].Value shr 8) and $FF;
                TrackData[I].Data[J].DataArray[2] :=
                  (TrackData[I].Data[J].Value) and $FF;

                SetLength(TrackData[I].Data[J].DataArray, 3);
                TrackData[I].Data[J].Len := 3;
              end;
            end;
            $C: // End Of Track
            begin
              TrackData[I].Data[J].Status := $FF;
              TrackData[I].Data[J].BParm1 := $2F;
            end;
          end;
        end;
      end;
      Inc(J);
    end;
    // Step 2: Convert ticks
    for J := 0 to Length(TrackData[I].Data) - 1 do
      if TrackData[I].Data[J].Ticks > 0 then
        TrackData[I].Data[J].Ticks := Round(TrackData[I].Data[J].Ticks * TickFactor);
    // Step 3: Add MDI specific events
    NewEvent(I, 0, $FF, $7F);
    SetLength(TrackData[I].Data[0].DataArray, 6);
    TrackData[I].Data[0].Len := Length(TrackData[I].Data[0].DataArray);
    TrackData[I].Data[0].DataArray[0] := $00; // Ad Lib ID
    TrackData[I].Data[0].DataArray[1] := $00;
    TrackData[I].Data[0].DataArray[2] := $3F;
    TrackData[I].Data[0].DataArray[3] := $00; // Opcode = Pitch Bend range
    TrackData[I].Data[0].DataArray[4] := $03;
    if not SongData_GetInt('MUS_PitchBendRange', J) then
      J := 1;
    if J < 1 then
      J := 1;
    if J > 12 then
      J := 12;
    TrackData[I].Data[0].DataArray[5] := J;   // Value

    NewEvent(I, 0, $FF, $7F);
    SetLength(TrackData[I].Data[0].DataArray, 6);
    TrackData[I].Data[0].Len := Length(TrackData[I].Data[0].DataArray);
    TrackData[I].Data[0].DataArray[0] := $00; // Ad Lib ID
    TrackData[I].Data[0].DataArray[1] := $00;
    TrackData[I].Data[0].DataArray[2] := $3F;
    TrackData[I].Data[0].DataArray[3] := $00; // Opcode = Card mode
    TrackData[I].Data[0].DataArray[4] := $02;
    SongData_GetInt('MUS_Percussive', J);
    if J <> 0 then
      J := 1;
    TrackData[I].Data[0].DataArray[5] := J;   // Value
    // Step 4: Insert tune name
    if (I = 0) and SongData_GetStr('MUS_TuneName', SName) then
      if SName <> '' then
      begin
        NewEvent(I, 0, $FF, $03);
        TrackData[I].Data[0].DataString := ansistring(SName);
      end;
    LogOutput('[+] Track #' + IntToStr(I) + ': ' + IntToStr(
      Length(TrackData[I].Data)) + ' events converted.');
  end;
  SongData_GetWord('Division', Division);
  SongData.Strings.Clear;

  SongData_PutInt('MIDIType', 0);
  SongData_PutDWord('InitTempo', NewTempo);
  SongData_PutInt('SMPTE', 0);
  SongData_PutInt('Division', Division);
  LogOutput('[+] Done.');
end;

//==============================================================================
//
// TXMICore.Convert_MDI_MID
//
//==============================================================================
procedure TXMICore.Convert_MDI_MID;
type
  TInst = array[0..13 + 13 + 2 - 1] of byte;
  PInst = ^TInst;
var
  Notes: array[0..15] of shortint;
  Volumes: array[0..15] of shortint;
  Insts: TList;
  P: PInst;
  I, J, K, Idx: integer;
  Rhythm: boolean;
begin
  Rhythm := False;
  LogOutput('[*] Converting AdLib MDI to Standard MIDI...');
  Application.ProcessMessages;
  Insts := TList.Create;
  for I := 0 to Length(TrackData) - 1 do
  begin
    FillChar(Notes[0], Length(Notes), -1);
    FillChar(Volumes[0], Length(Volumes), -1);
    J := 0;
    while J < Length(TrackData[I].Data) do
    begin
      case TrackData[I].Data[J].Status shr 4 of
        8: // Note Off
        begin
          if TrackData[I].Data[J].BParm2 > 0 then
          begin
            if Volumes[TrackData[I].Data[J].Status and $F] <>
              shortint(TrackData[I].Data[J].BParm2) then
            begin
              Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm2;
              NewEvent(I, J, $B0, 7);
              TrackData[I].Data[J].Ticks := TrackData[I].Data[J + 1].Ticks;
              TrackData[I].Data[J].Status :=
                $B0 or TrackData[I].Data[J + 1].Status and $F;
              TrackData[I].Data[J].BParm1 := 7;
              TrackData[I].Data[J].BParm2 := Volumes[TrackData[I].Data[J].Status and $F];
              Inc(J);
              TrackData[I].Data[J].Ticks := 0;
            end;
            TrackData[I].Data[J].BParm2 := 0;
          end;

          if Notes[TrackData[I].Data[J].Status and $F] = -1 then
          begin // No note on channel
            DelEvent(I, J, True);
            Continue;
          end
          else
          begin
            // Standard MIDI treats Note On with zero velocity as Note Off
            TrackData[I].Data[J].Status := $90 or TrackData[I].Data[J].Status and $F;
            if TrackData[I].Data[J].BParm1 = 0 then
            begin // Restore Note value
              TrackData[I].Data[J].BParm1 := Notes[TrackData[I].Data[J].Status and $F];
              Notes[TrackData[I].Data[J].Status and $F] := -1;
            end;
          end;

          if Rhythm and (TrackData[I].Data[J].Status shr 4 <> $B) then
            ROL_MIDIDrum(TrackData[I].Data[J]);
          Inc(J);
        end;
        9: // Note On
        begin
          if TrackData[I].Data[J].BParm2 > 0 then
          begin
            if Volumes[TrackData[I].Data[J].Status and $F] <>
              shortint(TrackData[I].Data[J].BParm2) then
            begin
              Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm2;
              NewEvent(I, J, $B0, 7);
              TrackData[I].Data[J].Ticks := TrackData[I].Data[J + 1].Ticks;
              TrackData[I].Data[J].Status :=
                $B0 or TrackData[I].Data[J + 1].Status and $F;
              TrackData[I].Data[J].BParm1 := 7;
              TrackData[I].Data[J].BParm2 := Volumes[TrackData[I].Data[J].Status and $F];
              Inc(J);
              TrackData[I].Data[J].Ticks := 0;
            end;
            TrackData[I].Data[J].BParm2 := 127;
            Notes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm1;
          end
          else // Treat as Note Off
            Notes[TrackData[I].Data[J].Status and $F] := -1;

          if Rhythm then
            ROL_MIDIDrum(TrackData[I].Data[J]);
          Inc(J);
        end;
        10..12, 14: Inc(J);
        13: // Channel Aftertouch -> Volume Change
        begin
          if Volumes[TrackData[I].Data[J].Status and $F] <>
            shortint(TrackData[I].Data[J].BParm1) then
          begin
            Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm1;
            TrackData[I].Data[J].Status := $B0 or TrackData[I].Data[J].Status and $F;
            TrackData[I].Data[J].BParm1 := 7;
            TrackData[I].Data[J].BParm2 := Volumes[TrackData[I].Data[J].Status and $F];
            Inc(J);
          end
          else
          begin
            DelEvent(I, J, True);
            Continue;
          end;
        end;
        15: // System
        begin
          if (TrackData[I].Data[J].Status and 15 = 0) or
            ((TrackData[I].Data[J].Status and 15 = 15) and
            (TrackData[I].Data[J].BParm1 = $7F)) and
            (Length(TrackData[I].Data[J].DataArray) >= 6) and
            (TrackData[I].Data[J].DataArray[0] = 0) and
            (TrackData[I].Data[J].DataArray[1] = 0) and
            (TrackData[I].Data[J].DataArray[2] = $3F) and
            (TrackData[I].Data[J].DataArray[3] = 0) then
          begin
            case TrackData[I].Data[J].DataArray[4] of
              1: // Load Patch
              begin
                if TrackData[I].Data[J].DataArray[5] < 16 then
                begin
                  if Rhythm and
                    (TrackData[I].Data[J].DataArray[5] >= 6) and
                    (TrackData[I].Data[J].DataArray[5] <= 10) then
                    DelEvent(I, J, True)
                  else
                  begin
                    if Length(TrackData[I].Data[J].DataArray) < 34 then
                    begin
                      DelEvent(I, J, True);
                      Continue;
                    end;
                    Idx := -1;
                    for K := 0 to Insts.Count - 1 do
                    begin
                      P := Insts[K];
                      if CompareMem(@TrackData[I].Data[J].DataArray[6],
                        @P^[0], 13 + 13 + 2) then
                        Idx := K;
                    end;
                    TrackData[I].Data[J].Status :=
                      $C0 or (TrackData[I].Data[J].DataArray[5] and $F);
                    if Idx > -1 then
                      TrackData[I].Data[J].BParm1 := Idx mod 128
                    else
                    begin
                      New(P);
                      CopyMemory(@P^[0],
                        @TrackData[I].Data[J].DataArray[6], 13 + 13 + 2);
                      TrackData[I].Data[J].BParm1 := Insts.Count mod 128;
                      Insts.Add(P);
                    end;
                    TrackData[I].Data[J].BParm2 := 0;
                    TrackData[I].Data[J].Len := 0;
                    SetLength(TrackData[I].Data[J].DataArray, 0);
                    Inc(J);
                  end;
                end
                else
                  Inc(J);
              end;
              2: // Rhythm Mode
              begin
                Rhythm := TrackData[I].Data[J].DataArray[5] > 0;
                DelEvent(I, J, True);
              end;
              3: // Pitch Bend Range
              begin
                Idx := TrackData[I].Data[J].DataArray[5];
                if Idx > 1 then
                begin
                  for K := 0 to 5 do
                  begin
                    NewEvent(I, J + (K * 5) + 1, $B0, $64);
                    TrackData[I].Data[J + (K * 5) + 1].Status := $B0 or K;
                    TrackData[I].Data[J + (K * 5) + 1].BParm2 := 0;
                    NewEvent(I, J + (K * 5) + 2, $B0, $65);
                    TrackData[I].Data[J + (K * 5) + 2].Status := $B0 or K;
                    TrackData[I].Data[J + (K * 5) + 2].BParm2 := 0;
                    NewEvent(I, J + (K * 5) + 3, $B0, 6);
                    TrackData[I].Data[J + (K * 5) + 3].Status := $B0 or K;
                    TrackData[I].Data[J + (K * 5) + 3].BParm2 := Idx;
                    NewEvent(I, J + (K * 5) + 4, $B0, $64);
                    TrackData[I].Data[J + (K * 5) + 4].Status := $B0 or K;
                    TrackData[I].Data[J + (K * 5) + 4].BParm2 := $7F;
                    NewEvent(I, J + (K * 5) + 5, $B0, $65);
                    TrackData[I].Data[J + (K * 5) + 5].Status := $B0 or K;
                    TrackData[I].Data[J + (K * 5) + 5].BParm2 := $7F;
                  end;
                  if not Rhythm then
                    for K := 6 to 8 do
                    begin
                      NewEvent(I, J + (K * 5) + 1, $B0, $64);
                      TrackData[I].Data[J + (K * 5) + 1].Status := $B0 or K;
                      TrackData[I].Data[J + (K * 5) + 1].BParm2 := 0;
                      NewEvent(I, J + (K * 5) + 2, $B0, $65);
                      TrackData[I].Data[J + (K * 5) + 2].Status := $B0 or K;
                      TrackData[I].Data[J + (K * 5) + 2].BParm2 := 0;
                      NewEvent(I, J + (K * 5) + 3, $B0, 6);
                      TrackData[I].Data[J + (K * 5) + 3].Status := $B0 or K;
                      TrackData[I].Data[J + (K * 5) + 3].BParm2 := Idx;
                      NewEvent(I, J + (K * 5) + 4, $B0, $64);
                      TrackData[I].Data[J + (K * 5) + 4].Status := $B0 or K;
                      TrackData[I].Data[J + (K * 5) + 4].BParm2 := $7F;
                      NewEvent(I, J + (K * 5) + 5, $B0, $65);
                      TrackData[I].Data[J + (K * 5) + 5].Status := $B0 or K;
                      TrackData[I].Data[J + (K * 5) + 5].BParm2 := $7F;
                    end;
                end;
                DelEvent(I, J, True);
                if Idx > 1 then
                  if Rhythm then
                    J := J + 6 * 5
                  else
                    J := J + 9 * 5;
              end;
              else
                Inc(J);
            end;
          end
          else
            Inc(J);
        end;
      end;
    end;
    if not ((TrackData[I].Data[Length(TrackData[I].Data) - 1].Status = $FF) and
      (TrackData[I].Data[Length(TrackData[I].Data) - 1].BParm1 = $2F)) then
    begin
      SetLength(TrackData[I].Data, Length(TrackData[I].Data) + 1);
      TrackData[I].Data[High(TrackData[I].Data)].Status := $FF;
      TrackData[I].Data[High(TrackData[I].Data)].BParm1 := $2F;
    end;
    LogOutput('[+] Track #' + IntToStr(I) + ': ' + IntToStr(
      Length(TrackData[I].Data)) + ' events converted.');
  end;

  if Insts.Count > 0 then
  begin
    for I := 0 to Insts.Count - 1 do
    begin
      P := Insts[I];
      Dispose(P);
    end;
    Insts.Clear;
  end;
  Insts.Free;

  LogOutput('[+] Done.');
end;

//==============================================================================
//
// TXMICore.Convert_MID_MUS
//
//==============================================================================
procedure TXMICore.Convert_MID_MUS;
var
  InitTempo: cardinal;
  I, J: integer;
  Speed: double;
  Division: word;
  TuneName: string;
  Volumes: array[0..15] of shortint;
begin
  LogOutput('[*] Converting Standard MIDI to AdLib MUS...');
  Application.ProcessMessages;
  if not SongData_GetDWord('InitTempo', InitTempo) then
  begin
    LogOutput('[-] Initial Tempo is not defined.');
    Exit;
  end;
  TuneName := '';
  for I := 0 to Length(TrackData) - 1 do
  begin
    FillChar(Volumes, SizeOf(Volumes), -1);
    J := 0;
    while J < Length(TrackData[I].Data) do
    begin
      case TrackData[I].Data[J].Status shr 4 of
        8: // Note Off
        begin
          if Volumes[TrackData[I].Data[J].Status and $F] > -1 then
            TrackData[I].Data[J].BParm2 := Volumes[TrackData[I].Data[J].Status and $F]
          else
            TrackData[I].Data[J].BParm2 := 127;
        end;
        9: // Note On
        begin
          if TrackData[I].Data[J].BParm2 > 0 then
            if Volumes[TrackData[I].Data[J].Status and $F] > -1 then
              TrackData[I].Data[J].BParm2 := Volumes[TrackData[I].Data[J].Status and $F]
            else
              TrackData[I].Data[J].BParm2 := 127;
        end;
        10: // Poly Aftertouch
        begin
          // Not compatible with MUS A# xx event
          DelEvent(I, J, True);
          Continue;
        end;
        11: // Control Change
        begin
          if (TrackData[I].Data[J].BParm1 = 7) and
            (Volumes[TrackData[I].Data[J].Status and $F] <>
            shortint(TrackData[I].Data[J].BParm2)) then
          begin // Volume Change
            Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm2;
            TrackData[I].Data[J].Status := $A0 or TrackData[I].Data[J].Status and $F;
            TrackData[I].Data[J].BParm1 := Volumes[TrackData[I].Data[J].Status and $F];
            TrackData[I].Data[J].BParm2 := 0;
          end
          else
          begin
            DelEvent(I, J, True);
            Continue;
          end;
        end;
        15: // System Event
        begin
          case TrackData[I].Data[J].Status and $F of
            15: // Meta Event
            begin
              case TrackData[I].Data[J].BParm1 of
                3: // Track Name -> Tune Name
                begin
                  if TuneName = '' then
                    TuneName := TrackData[I].Data[J].DataString;
                  DelEvent(I, J, True);
                  Continue;
                end;
                81: // Set Tempo -> Set Speed
                begin
                  Speed := InitTempo / TrackData[I].Data[J].Value;
                  TrackData[I].Data[J].Status := $F0;
                  SetLength(TrackData[I].Data[J].DataArray, 5);
                  TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);
                  TrackData[I].Data[J].DataArray[0] := $7F;
                  TrackData[I].Data[J].DataArray[1] := $00;
                  TrackData[I].Data[J].DataArray[2] := Floor(Speed);
                  TrackData[I].Data[J].DataArray[3] := Round(Frac(Speed) * 128);
                  TrackData[I].Data[J].DataArray[4] := $F7;
                end;
                $2F: // End Of Track -> Song End
                  TrackData[I].Data[J].Status := $FC;
                else
                begin
                  // Not compatible with MUS
                  DelEvent(I, J, True);
                  Continue;
                end;
              end;
            end;
          end;
        end;
      end;
      Inc(J);
    end;
    if (Length(TrackData[I].Data) > 0) and
      (TrackData[I].Data[High(TrackData[I].Data)].Status <> $FC) then
      NewEvent(I, Length(TrackData[I].Data), $FC, 0);
    LogOutput('[+] Track #' + IntToStr(I) + ': ' + IntToStr(
      Length(TrackData[I].Data)) + ' events converted.');
  end;
  SongData_PutInt('MUS_Version', 1);
  SongData_PutInt('MUS_ID', 0);
  SongData_PutStr('MUS_TuneName', TuneName);
  SongData_PutInt('MUS_TicksPerBeat', 60000000 div InitTempo);
  SongData_PutInt('MUS_Percussive', 1);
  SongData_PutInt('MUS_PitchBendRange', 1);
  SongData_GetWord('Division', Division);
  SongData_PutInt('MUS_BasicTempo', Division);
  SongData_PutDWord('InitTempo', InitTempo);
  LogOutput('[+] Done.');
end;

//==============================================================================
//
// TXMICore.Convert_MDI_MUS
//
//==============================================================================
procedure TXMICore.Convert_MDI_MUS;
type
  TInst = record
    Name: string;
    Data: array[0..13 + 13 + 2 - 1] of byte;
  end;
  PInst = ^TInst;
var
  Notes: array[0..15] of shortint;
  Volumes: array[0..15] of shortint;
  InitTempo: cardinal;
  I, J, K, Idx: integer;
  Speed: double;
  Division: word;
  Perc, PBend, TuneName, Instr: string;
  Insts: TList;
  P: PInst;
begin
  LogOutput('[*] Converting AdLib MDI to AdLib MUS...');
  Application.ProcessMessages;
  if not SongData_GetDWord('InitTempo', InitTempo) then
  begin
    LogOutput('[-] Initial Tempo is not defined.');
    Exit;
  end;
  Perc := '';
  PBend := '';
  TuneName := '';
  Instr := '';
  Insts := TList.Create;
  for I := 0 to Length(TrackData) - 1 do
  begin
    FillChar(Notes[0], Length(Notes), -1);
    FillChar(Volumes[0], Length(Volumes), -1);
    J := 0;
    while J < Length(TrackData[I].Data) do
    begin
      case TrackData[I].Data[J].Status shr 4 of
        8: // Note Off
        begin
          if TrackData[I].Data[J].BParm2 > 0 then
            if Volumes[TrackData[I].Data[J].Status and $F] <>
              shortint(TrackData[I].Data[J].BParm2) then
            begin
              Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm2;
              NewEvent(I, J, $A0, 0);
              TrackData[I].Data[J].Ticks := TrackData[I].Data[J + 1].Ticks;
              TrackData[I].Data[J].Status :=
                $A0 or TrackData[I].Data[J + 1].Status and $F;
              TrackData[I].Data[J].BParm1 := Volumes[TrackData[I].Data[J].Status and $F];
              TrackData[I].Data[J].BParm2 := 0;
              Inc(J);
              TrackData[I].Data[J].Ticks := 0;
            end;

          if Notes[TrackData[I].Data[J].Status and $F] = -1 then
          begin // No note on channel
            DelEvent(I, J, True);
            Continue;
          end
          else
          begin
            // MUS treats Note On with zero velocity as Note Off
            TrackData[I].Data[J].Status := $90 or TrackData[I].Data[J].Status and $F;
            TrackData[I].Data[J].BParm2 := 0;
            if TrackData[I].Data[J].BParm1 = 0 then
            begin // Restore Note value
              TrackData[I].Data[J].BParm1 := Notes[TrackData[I].Data[J].Status and $F];
              Notes[TrackData[I].Data[J].Status and $F] := -1;
            end;
          end;
        end;
        9: // Note On
        begin
          Notes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm1;
          Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm2;
          if TrackData[I].Data[J].BParm2 = 0 then // Treat as Note Off
            Notes[TrackData[I].Data[J].Status and $F] := -1;
        end;
        10: // Poly Aftertouch
        begin
          // Not compatible with MUS A# xx event
          DelEvent(I, J, True);
          Continue;
        end;
        13: // Volume Change D# -> Volume Change A#
        begin
          if Volumes[TrackData[I].Data[J].Status and $F] <>
            shortint(TrackData[I].Data[J].BParm1) then
          begin
            TrackData[I].Data[J].Status := $A0 or TrackData[I].Data[J].Status and $F;
            Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm1;
          end
          else
          begin
            DelEvent(I, J, True);
            Continue;
          end;
        end;
        15: // System Event
        begin
          case TrackData[I].Data[J].Status and $F of
            15: // Meta Event
            begin
              case TrackData[I].Data[J].BParm1 of
                3: // Track Name -> Tune Name
                begin
                  if TuneName = '' then
                    TuneName := TrackData[I].Data[J].DataString;
                  DelEvent(I, J, True);
                  Continue;
                end;
                4: // Instrument Name
                begin
                  Instr := TrackData[I].Data[J].DataString;
                  DelEvent(I, J, True);
                  Continue;
                end;
                81: // Set Tempo -> Set Speed
                begin
                  Speed := InitTempo / TrackData[I].Data[J].Value;
                  TrackData[I].Data[J].Status := $F0;
                  SetLength(TrackData[I].Data[J].DataArray, 5);
                  TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);
                  TrackData[I].Data[J].DataArray[0] := $7F;
                  TrackData[I].Data[J].DataArray[1] := $00;
                  TrackData[I].Data[J].DataArray[2] := Floor(Speed);
                  TrackData[I].Data[J].DataArray[3] := Round(Frac(Speed) * 128);
                  TrackData[I].Data[J].DataArray[4] := $F7;
                end;
                $2F: // End Of Track -> Song End
                  TrackData[I].Data[J].Status := $FC;
                $7F: // Sequencer Specific
                begin
                  if (Length(TrackData[I].Data[J].DataArray) >= 6) and
                    (TrackData[I].Data[J].DataArray[0] = $00) and
                    (TrackData[I].Data[J].DataArray[1] = $00) and
                    (TrackData[I].Data[J].DataArray[2] = $3F) then
                    case TrackData[I].Data[J].DataArray[4] of
                      1: // Load patch
                      begin
                        if Length(TrackData[I].Data[J].DataArray) < 34 then
                        begin
                          Instr := '';
                          DelEvent(I, J, True);
                          Continue;
                        end;
                        Idx := -1;
                        for K := 0 to Insts.Count - 1 do
                        begin
                          P := Insts[K];
                          if CompareMem(@TrackData[I].Data[J].DataArray[6],
                            @P^.Data[0], 13 + 13 + 2) then
                            Idx := K;
                        end;
                        TrackData[I].Data[J].Status :=
                          $C0 or (TrackData[I].Data[J].DataArray[5] and $F);
                        if Idx > -1 then
                          TrackData[I].Data[J].BParm1 := Idx mod 128
                        else
                        begin
                          New(P);
                          if Instr = '' then
                            Instr := Format('inst%.4d', [Insts.Count]);
                          P^.Name := Instr;
                          CopyMemory(@P^.Data[0],
                            @TrackData[I].Data[J].DataArray[6], 13 + 13 + 2);
                          TrackData[I].Data[J].BParm1 := Insts.Count mod 128;
                          Insts.Add(P);
                        end;
                        TrackData[I].Data[J].BParm2 := 0;
                        TrackData[I].Data[J].Len := 0;
                        SetLength(TrackData[I].Data[J].DataArray, 0);
                        Instr := '';
                        Inc(J);
                        Continue;
                      end;
                      2: // Card mode
                        if Perc = '' then
                          if TrackData[I].Data[J].DataArray[5] = 0 then
                            Perc := '-'
                          else
                            Perc := '+';
                      3: // Pitch bend range
                        if PBend = '' then
                          PBend := IntToStr(TrackData[I].Data[J].DataArray[5]);
                    end;
                  DelEvent(I, J, True);
                  Continue;
                end;
                else
                begin
                  // Not compatible with MUS
                  DelEvent(I, J, True);
                  Continue;
                end;
              end;
            end;
          end;
        end;
      end;
      Instr := '';
      Inc(J);
    end;
    if (Length(TrackData[I].Data) > 0) and
      (TrackData[I].Data[High(TrackData[I].Data)].Status <> $FC) then
      NewEvent(I, Length(TrackData[I].Data), $FC, 0);
    LogOutput('[+] Track #' + IntToStr(I) + ': ' + IntToStr(
      Length(TrackData[I].Data)) + ' events converted.');
  end;
  SongData_PutInt('MUS_Version', 1);
  SongData_PutInt('MUS_ID', 0);
  SongData_PutStr('MUS_TuneName', TuneName);
  SongData_PutInt('MUS_TicksPerBeat', 60000000 div InitTempo);
  if (Perc = '+') or (Perc = '') then
    SongData_PutInt('MUS_Percussive', 1)
  else
    SongData_PutInt('MUS_Percussive', 0);
  if PBend = '' then
    SongData_PutInt('MUS_PitchBendRange', 1)
  else
    SongData_PutStr('MUS_PitchBendRange', PBend);
  SongData_GetWord('Division', Division);
  SongData_PutInt('MUS_BasicTempo', Division);
  SongData_PutDWord('InitTempo', InitTempo);

  if Insts.Count > 0 then
  begin
    SongData_PutInt('SND_Version', 1);
    for I := 0 to Insts.Count - 1 do
    begin
      P := Insts[I];
      SongData_PutStr('SND_Name#' + IntToStr(I), P^.Name);
      Instr := '';
      for J := 0 to (13 + 13 + 2) - 1 do
        if P^.Data[J] < $80 then
          Instr := Instr + IntToStr(P^.Data[J]) + ' '
        else
          Instr := Instr + IntToStr(P^.Data[J] or $FF00) + ' ';
      SongData_PutStr('SND_Data#' + IntToStr(I), Instr);
      Dispose(P);
    end;
    Insts.Clear;
  end;
  Insts.Free;

  LogOutput('[+] Done.');
end;

//==============================================================================
//
// TXMICore.Convert_CMF_MID
//
//==============================================================================
procedure TXMICore.Convert_CMF_MID;
var
  I, J: integer;
  Rhythm: boolean;
  Division: word;
begin
  LogOutput('[*] Converting Creative Music File to Standard MIDI...');
  Application.ProcessMessages;
  Rhythm := False;
  for I := 0 to Length(TrackData) - 1 do
  begin
    J := 0;
    while J < Length(TrackData[I].Data) do
    begin
      case TrackData[I].Data[J].Status shr 4 of
        8: // Note Off
        begin
          if Rhythm then // convert drums
            CMF_MIDIDrum(TrackData[I].Data[J]);
        end;
        9: // Note On
        begin
          if Rhythm then // convert drums
            CMF_MIDIDrum(TrackData[I].Data[J]);
        end;
        11: // Control Change
        begin
          case TrackData[I].Data[J].BParm1 of
            $63: // AM+VIB: Unsupported by MIDI
            begin
              DelEvent(I, J, True);
              Continue;
            end;
            $66: // Set Marker Byte
            begin
              TrackData[I].Data[J].Status := $FF;
              TrackData[I].Data[J].BParm1 := $06;
              TrackData[I].Data[J].DataString := IntToStr(TrackData[I].Data[J].BParm2);
              TrackData[I].Data[J].BParm2 := 0;
            end;
            $67: // Rhythm Mode
            begin
              Rhythm := TrackData[I].Data[J].BParm2 > 0;
              DelEvent(I, J, True);
              Continue;
            end;
            $68: // Transpose Up
            begin
              DelEvent(I, J, True);
              Continue;
              // TODO
              TrackData[I].Data[J].Status :=
                $E0 or (TrackData[I].Data[J].Status and $F);
              TrackData[I].Data[J].Value :=
                8192 + (TrackData[I].Data[J].BParm2 * 32);
            end;
            $69: // Transpose Down
            begin
              DelEvent(I, J, True);
              Continue;
              // TODO
              TrackData[I].Data[J].Status :=
                $E0 or (TrackData[I].Data[J].Status and $F);
              TrackData[I].Data[J].Value :=
                8192 - (TrackData[I].Data[J].BParm2 * 32);
            end;
          end;
        end;
        12: // Program Change
          if Rhythm and (TrackData[I].Data[J].Status and $F >= 11) then
          begin
            DelEvent(I, J, True);
            Continue;
          end;
        15: // System
        begin
          if (TrackData[I].Data[J].Status = $FF) and
            (TrackData[I].Data[J].BParm1 = 81) then
            // convert tempo
            TrackData[I].Data[J].Value := TrackData[I].Data[J].Value * 2;
        end;
      end;
      Inc(J);
    end;
    NewEvent(I, 0, $FF, $51);
    TrackData[I].Data[0].Value := 1000000;
    LogOutput('[+] Track #' + IntToStr(I) + ': ' + IntToStr(
      Length(TrackData[I].Data)) + ' events converted.');
  end;
  SongData_GetWord('Division', Division);
  SongData.Strings.Clear;

  SongData_PutInt('MIDIType', 0);
  SongData_PutDWord('InitTempo', MIDIStdTempo);
  SongData_PutInt('SMPTE', 0);
  SongData_PutInt('Division', Division);
  LogOutput('[+] Done.');
end;

//==============================================================================
//
// TXMICore.Convert_CMF_MDI
//
//==============================================================================
procedure TXMICore.Convert_CMF_MDI;
type
  TOPLRegs = packed record
    ksl: byte;
    multiplier: byte;
    feedback: byte;
    attack: byte;
    sustain: byte;
    eg: byte;
    decay: byte;
    Release: byte;
    output: byte;
    vibam: byte;
    vibfq: byte;
    envscale: byte;
    conn: byte;
  end;

  TMDIInstrument = packed record
    oplModulator: TOPLRegs;
    oplCarrier: TOPLRegs;
    iModWaveSel: byte;
    iCarWaveSel: byte;
  end;
  PMDIInstrument = ^TMDIInstrument;

  TCMFInstrument = packed record
    iModChar: byte;
    iCarChar: byte;
    iModScale: byte;
    iCarScale: byte;
    iModAttack: byte;
    iCarAttack: byte;
    iModSustain: byte;
    iCarSustain: byte;
    iModWaveSel: byte;
    iCarWaveSel: byte;
    iFeedback: byte;
  end;
  PCMFInstrument = ^TCMFInstrument;
var
  I, J: integer;
  Rhythm: boolean;
  Division: word;
  CMFInst: array[0..15] of byte;
  MDIInst: array[0..13 + 13 + 2 - 1] of byte;
  PCMF: PCMFInstrument;
  PMDI: PMDIInstrument;
begin
  LogOutput('[*] Converting Creative Music File to AdLib MDI...');
  Application.ProcessMessages;
  Rhythm := False;
  for I := 0 to Length(TrackData) - 1 do
  begin
    J := 0;
    while J < Length(TrackData[I].Data) do
    begin
      if TrackData[I].Data[J].Status shr 4 < 15 then
        if (Rhythm and (TrackData[I].Data[J].Status and
          $F in [6..10])) or ((not Rhythm) and
          (TrackData[I].Data[J].Status and $F in [9..15])) then
        begin
          DelEvent(I, J, True);
          Continue;
        end;
      if Rhythm and (TrackData[I].Data[J].Status shr 4 < 15) then
      begin
        // convert drums
        case TrackData[I].Data[J].Status and $F of
          11: // ch11 -> ch06 - Bass Drum
            TrackData[I].Data[J].Status := (TrackData[I].Data[J].Status and $F0) or 6;
          12: // ch12 -> ch07 - Snare Drum
            TrackData[I].Data[J].Status := (TrackData[I].Data[J].Status and $F0) or 7;
          13: // ch13 -> ch08 - Tom
            TrackData[I].Data[J].Status := (TrackData[I].Data[J].Status and $F0) or 8;
          14: // ch14 -> ch09 - Cymbal
            TrackData[I].Data[J].Status := (TrackData[I].Data[J].Status and $F0) or 9;
          15: // ch15 -> ch10 - Hi-Hat
            TrackData[I].Data[J].Status := (TrackData[I].Data[J].Status and $F0) or 10;
        end;
      end;
      case TrackData[I].Data[J].Status shr 4 of
        10, 13: // Poly Aftertouch, Channel Aftertouch
        begin
          DelEvent(I, J, True);
          Continue;
        end;
        11: // Control Change
        begin
          case TrackData[I].Data[J].BParm1 of
            7: // Volume Change
            begin
              TrackData[I].Data[J].Status := $D0 or TrackData[I].Data[J].Status and $F;
              TrackData[I].Data[J].BParm1 := TrackData[I].Data[J].BParm2;
              TrackData[I].Data[J].BParm2 := 0;
            end;
            $63: // AM+VIB: Unsupported by MDI
            begin
              DelEvent(I, J, True);
              Continue;
            end;
            $66: // Set Marker Byte
            begin
              TrackData[I].Data[J].Status := $FF;
              TrackData[I].Data[J].BParm1 := $06;
              TrackData[I].Data[J].DataString := IntToStr(TrackData[I].Data[J].BParm2);
              TrackData[I].Data[J].BParm2 := 0;
            end;
            $67: // Rhythm Mode
            begin
              Rhythm := TrackData[I].Data[J].BParm2 > 0;
              TrackData[I].Data[J].Status := $FF;
              TrackData[I].Data[J].BParm1 := $7F;
              TrackData[I].Data[J].BParm2 := 0;
              TrackData[I].Data[J].Len := 6;
              SetLength(TrackData[I].Data[J].DataArray, 6);
              TrackData[I].Data[J].DataArray[0] := $00;
              TrackData[I].Data[J].DataArray[1] := $00;
              TrackData[I].Data[J].DataArray[2] := $3F; // Ad Lib ID
              TrackData[I].Data[J].DataArray[3] := $00;
              TrackData[I].Data[J].DataArray[4] := $02; // Card mode
              TrackData[I].Data[J].DataArray[5] := byte(Rhythm);
            end;
            $68: // Transpose Up
            begin
              DelEvent(I, J, True);
              Continue;
              // TODO
              TrackData[I].Data[J].Status :=
                $E0 or (TrackData[I].Data[J].Status and $F);
              TrackData[I].Data[J].Value :=
                8192 + (TrackData[I].Data[J].BParm2 * 32);
            end;
            $69: // Transpose Down
            begin
              DelEvent(I, J, True);
              Continue;
              // TODO
              TrackData[I].Data[J].Status :=
                $E0 or (TrackData[I].Data[J].Status and $F);
              TrackData[I].Data[J].Value :=
                8192 - (TrackData[I].Data[J].BParm2 * 32);
            end;
          end;
        end;
        12: // Program Change
        begin
          FillChar(CMFInst, SizeOf(CMFInst), 0);
          if not SongData_GetArray('CMF_Inst#' + IntToStr(TrackData[I].Data[J].BParm1),
            CMFInst) then
          begin
            DelEvent(I, J, True);
            Continue;
          end;
          FillChar(MDIInst, SizeOf(MDIInst), 0);
          PCMF := @CMFInst[0];
          PMDI := @MDIInst[0];
          PMDI^.oplModulator.ksl := PCMF^.iModScale shr 6;
          PMDI^.oplModulator.multiplier := PCMF^.iModChar and $F;
          PMDI^.oplModulator.feedback := (PCMF^.iFeedback shr 1) and 7;
          PMDI^.oplModulator.attack := PCMF^.iModAttack shr 4;
          PMDI^.oplModulator.sustain := PCMF^.iModSustain shr 4;
          PMDI^.oplModulator.eg := (PCMF^.iModChar shr 5) and 1;
          PMDI^.oplModulator.decay := PCMF^.iModAttack and $F;
          PMDI^.oplModulator.Release := PCMF^.iModSustain and $F;
          PMDI^.oplModulator.output := PCMF^.iModScale and $3F;
          PMDI^.oplModulator.vibam := PCMF^.iModChar shr 7;
          PMDI^.oplModulator.vibfq := (PCMF^.iModChar shr 6) and 1;
          PMDI^.oplModulator.envscale := (PCMF^.iModChar shr 4) and 1;
          PMDI^.oplModulator.conn := (not PCMF^.iFeedback) and 1;
          PMDI^.oplCarrier.ksl := PCMF^.iCarScale shr 6;
          PMDI^.oplCarrier.multiplier := PCMF^.iCarChar and $F;
          PMDI^.oplCarrier.feedback := (not PMDI^.oplModulator.feedback) and $7F;
          PMDI^.oplCarrier.attack := PCMF^.iCarAttack shr 4;
          PMDI^.oplCarrier.sustain := PCMF^.iCarSustain shr 4;
          PMDI^.oplCarrier.eg := (PCMF^.iCarChar shr 5) and 1;
          PMDI^.oplCarrier.decay := PCMF^.iCarAttack and $F;
          PMDI^.oplCarrier.Release := PCMF^.iCarSustain and $F;
          PMDI^.oplCarrier.output := PCMF^.iCarScale and $3F;
          PMDI^.oplCarrier.vibam := PCMF^.iCarChar shr 7;
          PMDI^.oplCarrier.vibfq := (PCMF^.iCarChar shr 6) and 1;
          PMDI^.oplCarrier.envscale := (PCMF^.iCarChar shr 4) and 1;
          PMDI^.oplCarrier.conn := 1;
          PMDI^.iModWaveSel := PCMF^.iModWaveSel;
          PMDI^.iCarWaveSel := PCMF^.iCarWaveSel;
          TrackData[I].Data[J].Len := 6;
          SetLength(TrackData[I].Data[J].DataArray, 5 + 1 + 13 + 13 + 2);
          TrackData[I].Data[J].DataArray[0] := $00;
          TrackData[I].Data[J].DataArray[1] := $00;
          TrackData[I].Data[J].DataArray[2] := $3F; // Ad Lib ID
          TrackData[I].Data[J].DataArray[3] := $00;
          TrackData[I].Data[J].DataArray[4] := $01; // Load Patch
          TrackData[I].Data[J].DataArray[5] := TrackData[I].Data[J].Status and $F;
          CopyMemory(@TrackData[I].Data[J].DataArray[6], @MDIInst[0], 13 + 13 + 2);
          TrackData[I].Data[J].Status := $FF;
          TrackData[I].Data[J].BParm1 := $7F;
          TrackData[I].Data[J].BParm2 := 0;
        end;
        15: // System
        begin
          if (TrackData[I].Data[J].Status = $FF) and
            (TrackData[I].Data[J].BParm1 = 81) then
            // convert tempo
            TrackData[I].Data[J].Value := TrackData[I].Data[J].Value * 2;
        end;
      end;
      Inc(J);
    end;
    NewEvent(I, 0, $FF, $51);
    TrackData[I].Data[0].Value := 1000000;
    LogOutput('[+] Track #' + IntToStr(I) + ': ' + IntToStr(
      Length(TrackData[I].Data)) + ' events converted.');
  end;
  SongData_GetWord('Division', Division);
  SongData.Strings.Clear;

  SongData_PutInt('MIDIType', 0);
  SongData_PutDWord('InitTempo', MIDIStdTempo);
  SongData_PutInt('SMPTE', 0);
  SongData_PutInt('Division', Division);
  LogOutput('[+] Done.');
end;

//==============================================================================
//
// TXMICore.Convert_MDI_CMF
//
//==============================================================================
procedure TXMICore.Convert_MDI_CMF;
type
  TOPLRegs = packed record
    ksl: byte;
    multiplier: byte;
    feedback: byte;
    attack: byte;
    sustain: byte;
    eg: byte;
    decay: byte;
    Release: byte;
    output: byte;
    vibam: byte;
    vibfq: byte;
    envscale: byte;
    conn: byte;
  end;

  TMDIInstrument = packed record
    oplModulator: TOPLRegs;
    oplCarrier: TOPLRegs;
    iModWaveSel: byte;
    iCarWaveSel: byte;
  end;
  PMDIInstrument = ^TMDIInstrument;

  TCMFInstrument = packed record
    iModChar: byte;
    iCarChar: byte;
    iModScale: byte;
    iCarScale: byte;
    iModAttack: byte;
    iCarAttack: byte;
    iModSustain: byte;
    iCarSustain: byte;
    iModWaveSel: byte;
    iCarWaveSel: byte;
    iFeedback: byte;
  end;
  PCMFInstrument = ^TCMFInstrument;
  TInst = array[0..13 + 13 + 2 - 1] of byte;
  PInst = ^TInst;
var
  Notes: array[0..15] of shortint;
  Volumes: array[0..15] of shortint;
  Insts: TList;
  P: PInst;
  I, J, K, Idx: integer;
  Rhythm: boolean;
  Division: word;
  Tempo: string;
  CMFInst: array[0..11 - 1] of byte;
  PCMF: PCMFInstrument;
  PMDI: PMDIInstrument;
begin
  Rhythm := False;
  LogOutput('[*] Converting AdLib MDI to Creative Music File...');
  Application.ProcessMessages;
  Insts := TList.Create;
  Tempo := '';
  for I := 0 to Length(TrackData) - 1 do
  begin
    FillChar(Notes[0], Length(Notes), -1);
    FillChar(Volumes[0], Length(Volumes), -1);
    J := 0;
    while J < Length(TrackData[I].Data) do
    begin
      if Rhythm and (TrackData[I].Data[J].Status shr 4 < 15) then
      begin
        // convert drums
        case TrackData[I].Data[J].Status and 15 of
          6: // ch06 -> ch11 - Bass Drum
            TrackData[I].Data[J].Status := (TrackData[I].Data[J].Status and $F0) or 11;
          7: // ch07 -> ch12 - Snare Drum
            TrackData[I].Data[J].Status := (TrackData[I].Data[J].Status and $F0) or 12;
          8: // ch08 -> ch13 - Tom
            TrackData[I].Data[J].Status := (TrackData[I].Data[J].Status and $F0) or 13;
          9: // ch09 -> ch14 - Cymbal
            TrackData[I].Data[J].Status := (TrackData[I].Data[J].Status and $F0) or 14;
          10: // ch10 -> ch15 - Hi-Hat
            TrackData[I].Data[J].Status := (TrackData[I].Data[J].Status and $F0) or 15;
        end;
      end;
      case TrackData[I].Data[J].Status shr 4 of
        8: // Note Off
        begin
          if TrackData[I].Data[J].BParm2 > 0 then
          begin
            if Volumes[TrackData[I].Data[J].Status and $F] <>
              shortint(TrackData[I].Data[J].BParm2) then
            begin
              Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm2;
              NewEvent(I, J, $B0, 7);
              TrackData[I].Data[J].Ticks := TrackData[I].Data[J + 1].Ticks;
              TrackData[I].Data[J].Status :=
                $B0 or TrackData[I].Data[J + 1].Status and $F;
              TrackData[I].Data[J].BParm1 := 7;
              TrackData[I].Data[J].BParm2 := Volumes[TrackData[I].Data[J].Status and $F];
              Inc(J);
              TrackData[I].Data[J].Ticks := 0;
            end;
            TrackData[I].Data[J].BParm2 := 0;
          end;

          if Notes[TrackData[I].Data[J].Status and $F] = -1 then
          begin // No note on channel
            DelEvent(I, J, True);
            Continue;
          end
          else
          begin
            // CMF treats Note On with zero velocity as Note Off
            TrackData[I].Data[J].Status := $90 or TrackData[I].Data[J].Status and $F;
            if TrackData[I].Data[J].BParm1 = 0 then
            begin // Restore Note value
              TrackData[I].Data[J].BParm1 := Notes[TrackData[I].Data[J].Status and $F];
              Notes[TrackData[I].Data[J].Status and $F] := -1;
            end;
          end;
          Inc(J);
        end;
        9: // Note On
        begin
          if TrackData[I].Data[J].BParm2 > 0 then
          begin
            if Volumes[TrackData[I].Data[J].Status and $F] <>
              shortint(TrackData[I].Data[J].BParm2) then
            begin
              Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm2;
              NewEvent(I, J, $B0, 7);
              TrackData[I].Data[J].Ticks := TrackData[I].Data[J + 1].Ticks;
              TrackData[I].Data[J].Status :=
                $B0 or TrackData[I].Data[J + 1].Status and $F;
              TrackData[I].Data[J].BParm1 := 7;
              TrackData[I].Data[J].BParm2 := Volumes[TrackData[I].Data[J].Status and $F];
              Inc(J);
              TrackData[I].Data[J].Ticks := 0;
            end;
            TrackData[I].Data[J].BParm2 := 127;
            Notes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm1;
          end
          else // Treat as Note Off
            Notes[TrackData[I].Data[J].Status and $F] := -1;

          Inc(J);
        end;
        10..12: Inc(J);
        13: // Channel Aftertouch -> Volume Change
        begin
          if Volumes[TrackData[I].Data[J].Status and $F] <>
            shortint(TrackData[I].Data[J].BParm1) then
          begin
            Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm1;
            TrackData[I].Data[J].Status := $B0 or TrackData[I].Data[J].Status and $F;
            TrackData[I].Data[J].BParm1 := 7;
            TrackData[I].Data[J].BParm2 := Volumes[TrackData[I].Data[J].Status and $F];
            Inc(J);
          end
          else
          begin
            DelEvent(I, J, True);
            Continue;
          end;
        end;
        14: // Pitch Bend
        begin
          // TODO
          Inc(J);
        end;
        15: // System
        begin
          if (TrackData[I].Data[J].Status and 15 = 0) or
            ((TrackData[I].Data[J].Status and 15 = 15) and
            (TrackData[I].Data[J].BParm1 = $7F)) and
            (Length(TrackData[I].Data[J].DataArray) >= 6) and
            (TrackData[I].Data[J].DataArray[0] = 0) and
            (TrackData[I].Data[J].DataArray[1] = 0) and
            (TrackData[I].Data[J].DataArray[2] = $3F) and
            (TrackData[I].Data[J].DataArray[3] = 0) then
          begin
            case TrackData[I].Data[J].DataArray[4] of
              1: // Load Patch
              begin
                if TrackData[I].Data[J].DataArray[5] < 16 then
                begin
                  if Length(TrackData[I].Data[J].DataArray) < 34 then
                  begin
                    DelEvent(I, J, True);
                    Continue;
                  end;
                  Idx := -1;
                  for K := 0 to Insts.Count - 1 do
                  begin
                    P := Insts[K];
                    if CompareMem(@TrackData[I].Data[J].DataArray[6],
                      @P^[0], 13 + 13 + 2) then
                      Idx := K;
                  end;
                  TrackData[I].Data[J].Status :=
                    $C0 or (TrackData[I].Data[J].DataArray[5] and $F);
                  if Idx > -1 then
                    TrackData[I].Data[J].BParm1 := Idx mod 128
                  else
                  begin
                    New(P);
                    CopyMemory(@P^[0],
                      @TrackData[I].Data[J].DataArray[6], 13 + 13 + 2);
                    TrackData[I].Data[J].BParm1 := Insts.Count mod 128;
                    Insts.Add(P);
                  end;
                  if Rhythm then
                    case TrackData[I].Data[J].Status and 15 of
                      6: TrackData[I].Data[J].Status :=
                          (TrackData[I].Data[J].Status and $F0) or 11;
                      7: TrackData[I].Data[J].Status :=
                          (TrackData[I].Data[J].Status and $F0) or 12;
                      8: TrackData[I].Data[J].Status :=
                          (TrackData[I].Data[J].Status and $F0) or 13;
                      9: TrackData[I].Data[J].Status :=
                          (TrackData[I].Data[J].Status and $F0) or 14;
                      10: TrackData[I].Data[J].Status :=
                          (TrackData[I].Data[J].Status and $F0) or 15;
                    end;
                  TrackData[I].Data[J].BParm2 := 0;
                  TrackData[I].Data[J].Len := 0;
                  SetLength(TrackData[I].Data[J].DataArray, 0);
                end;
                Inc(J);
              end;
              2: // Rhythm Mode
              begin
                Rhythm := TrackData[I].Data[J].DataArray[5] > 0;
                TrackData[I].Data[J].Status := $B0;
                TrackData[I].Data[J].BParm1 := $67;
                TrackData[I].Data[J].BParm2 := byte(Rhythm);
                TrackData[I].Data[J].Len := 0;
                SetLength(TrackData[I].Data[J].DataArray, 0);
                Inc(J);
              end;
              else
              begin
                DelEvent(I, J, True);
                Continue;
              end;
            end;
          end
          else
            case TrackData[I].Data[J].BParm1 of
              81: // Tempo
              begin
                if Tempo = '' then
                  Tempo := IntToStr(TrackData[I].Data[J].Value);
                DelEvent(I, J, True);
                Continue;
              end;
              else
              begin
                DelEvent(I, J, True);
                Continue;
              end;
            end;
        end;
      end;
    end;
    if not ((TrackData[I].Data[Length(TrackData[I].Data) - 1].Status = $FF) and
      (TrackData[I].Data[Length(TrackData[I].Data) - 1].BParm1 = $2F)) then
    begin
      SetLength(TrackData[I].Data, Length(TrackData[I].Data) + 1);
      TrackData[I].Data[High(TrackData[I].Data)].Status := $FF;
      TrackData[I].Data[High(TrackData[I].Data)].BParm1 := $2F;
    end;
    LogOutput('[+] Track #' + IntToStr(I) + ': ' + IntToStr(
      Length(TrackData[I].Data)) + ' events converted.');
  end;

  SongData_GetWord('Division', Division);
  SongData.Strings.Clear;

  SongData_PutInt('MIDIType', 0);
  SongData_PutDWord('InitTempo', 1000000);
  SongData_PutInt('SMPTE', 0);
  if Tempo <> '' then
  begin
    Division := Round(Division * (1000000 / StrToInt(Tempo)));
    SongData_PutInt('CMF_Tempo', StrToInt(Tempo) div 1000);
  end;
  SongData_PutInt('Division', Division);
  SongData_PutInt('CMF_TicksPerSecond', Division);
  SongData_PutInt('CMF_TicksPerQuarter', Division div 2);

  if Insts.Count > 0 then
  begin
    for I := 0 to Insts.Count - 1 do
    begin
      P := Insts[I];
      PMDI := Pointer(P);
      PCMF := @CMFInst[0];
      FillChar(CMFInst, SizeOf(CMFInst), 0);
      PCMF^.iModChar :=
        (PMDI^.oplModulator.multiplier and $F) or
        (PMDI^.oplModulator.envscale and 1 shl 4) or
        (PMDI^.oplModulator.eg and 1 shl 5) or
        (PMDI^.oplModulator.vibfq and 1 shl 6) or
        (PMDI^.oplModulator.vibam and 1 shl 7);
      PCMF^.iModScale :=
        (PMDI^.oplModulator.output and $3F) or
        (PMDI^.oplModulator.ksl and 3 shl 6);
      PCMF^.iModAttack :=
        (PMDI^.oplModulator.decay and $F) or
        (PMDI^.oplModulator.attack and $F shl 4);
      PCMF^.iModSustain :=
        (PMDI^.oplModulator.Release and $F) or
        (PMDI^.oplModulator.sustain and $F shl 4);
      PCMF^.iModWaveSel := PMDI^.iModWaveSel and 3;
      PCMF^.iCarChar :=
        (PMDI^.oplCarrier.multiplier and $F) or
        (PMDI^.oplCarrier.envscale and 1 shl 4) or
        (PMDI^.oplCarrier.eg and 1 shl 5) or
        (PMDI^.oplCarrier.vibfq and 1 shl 6) or
        (PMDI^.oplCarrier.vibam and 1 shl 7);
      PCMF^.iCarScale :=
        (PMDI^.oplCarrier.output and $3F) or
        (PMDI^.oplCarrier.ksl and 3 shl 6);
      PCMF^.iCarAttack :=
        (PMDI^.oplCarrier.decay and $F) or
        (PMDI^.oplCarrier.attack and $F shl 4);
      PCMF^.iCarSustain :=
        (PMDI^.oplCarrier.Release and $F) or
        (PMDI^.oplCarrier.sustain and $F shl 4);
      PCMF^.iCarWaveSel := PMDI^.iCarWaveSel and 3;
      PCMF^.iFeedback :=
        ((not PMDI^.oplModulator.conn) and 1) or
        (PMDI^.oplModulator.feedback and 7 shl 1);
      SongData_PutArray('CMF_Inst#' + IntToStr(I), CMFInst);
      Dispose(P);
    end;
    Insts.Clear;
  end;
  Insts.Free;

  LogOutput('[+] Done.');
end;

//==============================================================================
//
// TXMICore.Convert_ROL_MID
//
//==============================================================================
procedure TXMICore.Convert_ROL_MID;
var
  I, J, K: integer;
  Rhythm: boolean;
  Fl: single;
  TPB: word;
  NewTempo: cardinal;
  TickFactor: double;
  LastNote: byte;
  Insts: TStringList;
  Track: Chunk;
  Tracks: array of integer;
  Cmd: Command;
  Division: word;
begin
  LogOutput('[*] Converting AdLib ROL to Standard MIDI...');
  if not SongData_GetInt('ROL_Melodic', I) then
    I := 0;
  Rhythm := I = 0;
  if not SongData_GetWord('ROL_TicksPerBeat', TPB) then
    TPB := 4;
  NewTempo := MIDIStdTempo;
  TickFactor := 60000000 / TPB / NewTempo;
  Application.ProcessMessages;

  if Length(TrackData) >= 1 then
  begin
    // Step 1: Convert tempo
    I := 0;
    for J := 0 to Length(TrackData[I].Data) - 1 do
      if (TrackData[I].Data[J].Status = $FF) and
        (TrackData[I].Data[J].BParm1 = $7F) and
        (Length(TrackData[I].Data[J].DataArray) >= 1 + 4) and
        (TrackData[I].Data[J].DataArray[0] = 0) then
      begin
        // Tempo
        Move(TrackData[I].Data[J].DataArray[1], Fl, 4);

        if Fl > 0 then
          TrackData[I].Data[J].Value := Round(NewTempo / Fl)
        else
          TrackData[I].Data[J].Value := NewTempo;

        TrackData[I].Data[J].Status := $FF;
        TrackData[I].Data[J].BParm1 := 81;

        TrackData[I].Data[J].DataArray[0] := (TrackData[I].Data[J].Value shr 16) and $FF;
        TrackData[I].Data[J].DataArray[1] := (TrackData[I].Data[J].Value shr 8) and $FF;
        TrackData[I].Data[J].DataArray[2] := (TrackData[I].Data[J].Value) and $FF;

        SetLength(TrackData[I].Data[J].DataArray, 3);
        TrackData[I].Data[J].Len := 3;
      end;
    // Step 2: Convert events
    Insts := TStringList.Create;
    for I := 1 to Length(TrackData) - 1 do
    begin
      LastNote := 0;
      J := 0;
      while J < Length(TrackData[I].Data) do
      begin
        case (I - 1) mod 4 of
          0: // Voix #
          begin
            if (TrackData[I].Data[J].BParm1 = 0) and
              (TrackData[I].Data[J].BParm2 = 0) then
            begin
              TrackData[I].Data[J].BParm1 := LastNote;
              LastNote := 0;
            end
            else
            begin
              if LastNote > 0 then
              begin
                NewEvent(I, J, $90, 0);
                TrackData[I].Data[J].Ticks := TrackData[I].Data[J + 1].Ticks;
                TrackData[I].Data[J].Status := TrackData[I].Data[J + 1].Status;
                TrackData[I].Data[J].BParm1 := LastNote;
                TrackData[I].Data[J].BParm2 := 0;
                if Rhythm then
                  ROL_MIDIDrum(TrackData[I].Data[J]);
                Inc(J);
                TrackData[I].Data[J].Ticks := 0;
              end;
              LastNote := TrackData[I].Data[J].BParm1;
            end;
            if Rhythm then
              ROL_MIDIDrum(TrackData[I].Data[J]);
          end;
          1: // Timbre #
            if (TrackData[I].Data[J].Status = $FF) and
              (TrackData[I].Data[J].BParm1 = $7F) and
              (Length(TrackData[I].Data[J].DataArray) >= 1 + 9 + 1 + 2) and
              (TrackData[I].Data[J].DataArray[0] = 1) then
            begin
              K := Insts.IndexOf(PAnsiChar(@TrackData[I].Data[J].DataArray[1]));
              if K < 0 then
                K := Insts.Add(PAnsiChar(@TrackData[I].Data[J].DataArray[1]));
              TrackData[I].Data[J].Status := $C0 or (((I - 1) div 4) and $F);
              TrackData[I].Data[J].BParm1 := K;
            end;
          2: // Volume #
            if (TrackData[I].Data[J].Status = $FF) and
              (TrackData[I].Data[J].BParm1 = $7F) and
              (Length(TrackData[I].Data[J].DataArray) >= 1 + 4) and
              (TrackData[I].Data[J].DataArray[0] = 2) then
            begin
              Move(TrackData[I].Data[J].DataArray[1], Fl, 4);
              TrackData[I].Data[J].Status := $B0 or (((I - 1) div 4) and $F);
              TrackData[I].Data[J].BParm1 := 7;
              TrackData[I].Data[J].BParm2 := Round(127 * Fl);
              SetLength(TrackData[I].Data[J].DataArray, 0);
              TrackData[I].Data[J].Len := 0;
            end;
          3: // Pitch #
            if (TrackData[I].Data[J].Status = $FF) and
              (TrackData[I].Data[J].BParm1 = $7F) and
              (Length(TrackData[I].Data[J].DataArray) >= 1 + 4) and
              (TrackData[I].Data[J].DataArray[0] = 3) then
            begin
              Move(TrackData[I].Data[J].DataArray[1], Fl, 4);
              TrackData[I].Data[J].Status := $E0 or (((I - 1) div 4) and $F);
              TrackData[I].Data[J].Value := Floor(8192 * Fl);
              TrackData[I].Data[J].BParm1 := TrackData[I].Data[J].Value and 127;
              TrackData[I].Data[J].BParm2 := (TrackData[I].Data[J].Value shr 7) and 127;
              SetLength(TrackData[I].Data[J].DataArray, 0);
              TrackData[I].Data[J].Len := 0;
            end;
        end;
        Inc(J);
      end;
    end;
    Insts.Free;
    // Step 3: Merge tracks by channel using ticks
    TrackData[0].Title := '';
    I := 1;
    while Length(TrackData) > I do
    begin
      SetLength(Tracks, 0);
      // Timbre #
      if I + 1 < Length(TrackData) then
      begin
        SetLength(Tracks, Length(Tracks) + 1);
        Tracks[High(Tracks)] := I + 1;
      end;
      // Volume #
      if I + 2 < Length(TrackData) then
      begin
        SetLength(Tracks, Length(Tracks) + 1);
        Tracks[High(Tracks)] := I + 2;
      end;
      // Pitch #
      if I + 3 < Length(TrackData) then
      begin
        SetLength(Tracks, Length(Tracks) + 1);
        Tracks[High(Tracks)] := I + 3;
      end;
      // Voix #
      SetLength(Tracks, Length(Tracks) + 1);
      Tracks[High(Tracks)] := I;
      Track := MergeTracksUsingTicks(Tracks, False);
      // Higher priority for Note Off
      J := 1;
      while J < Length(Track.Data) do
      begin
        if (Track.Data[J].Ticks = 0) and
          (Track.Data[J].Status shr 4 = $9) and (Track.Data[J].BParm2 = 0) and
          (Track.Data[J - 1].Status shr 4 <> $9) then
        begin
          Track.Data[J].Ticks := Track.Data[J - 1].Ticks;
          Track.Data[J - 1].Ticks := 0;
          Cmd := Track.Data[J - 1];
          Track.Data[J - 1] := Track.Data[J];
          Track.Data[J] := Cmd;
          Dec(J);
          Continue;
        end;
        Inc(J);
      end;
      for J := Length(Tracks) - 1 downto 0 do
        if Tracks[J] <> I then
          DelTrack(Tracks[J]);
      TrackData[I].Title := '';
      SetLength(TrackData[I].Data, 0);
      TrackData[I] := Track;
      Inc(I);
    end;
    // Step 4: Convert ticks
    for I := 0 to Length(TrackData) - 1 do
      for J := 0 to Length(TrackData[I].Data) - 1 do
        if TrackData[I].Data[J].Ticks > 0 then
          TrackData[I].Data[J].Ticks := Round(TrackData[I].Data[J].Ticks * TickFactor);
    // Step 5: Add End of Track events
    for I := 0 to Length(TrackData) - 1 do
    begin
      SetLength(TrackData[I].Data, Length(TrackData[I].Data) + 1);
      J := High(TrackData[I].Data);
      TrackData[I].Data[J].Status := $FF;
      TrackData[I].Data[J].BParm1 := $2F;
    end;
  end;

  SongData_GetWord('Division', Division);
  SongData.Strings.Clear;

  SongData_PutInt('MIDIType', 1);
  SongData_PutDWord('InitTempo', NewTempo);
  SongData_PutInt('Division', Division);
  SongData_PutInt('SMPTE', 0);

  LogOutput('[+] Done.');
end;

//==============================================================================
//
// TXMICore.Convert_MUS_ROL
//
//==============================================================================
procedure TXMICore.Convert_MUS_ROL;
type
  TROLInst = record
    Name: string;
    Perc,
    Chan: byte;
    Data: array[0..13 + 13 + 2 - 1] of byte;
  end;
  PROLInst = ^TROLInst;

  TTickData = record
    Ticks: UInt64;
    New: UInt64;
  end;
  PTickData = ^TTickData;
var
  TPB, BPM: byte;
  Division, numInst: word;
  Rhythm: boolean;
  S: string;
  Insts: TList;
  Inst: PROLInst;
  I, J, K, Idx: integer;
  L: TStringList;
  CheckTPB: array of byte;
  Ticks: TList;
  Tick: PTickData;
  MinTicksTPB, NewTempo: word;
  MinError, Error: double;
  MinTick: UInt64;
  Volumes: array[0..15] of shortint;
  Fl: single;
begin
  LogOutput('[*] Converting AdLib MUS to AdLib ROL...');
  if not SongData_GetByte('MUS_TicksPerBeat', TPB) then
    TPB := 240;
  if not SongData_GetByte('MUS_BeatPerMeasure', BPM) then
    BPM := 4;
  if not SongData_GetInt('MUS_Percussive', I) then
    I := 1;
  Rhythm := I = 1;
  if not SongData_GetWord('MUS_BasicTempo', Division) then
    Division := 120;
  numInst := 0;
  while SongData_GetStr('SND_Name#' + IntToStr(numInst), S) do
    Inc(numInst);
  // Read instruments from SND
  Insts := TList.Create;
  for I := 0 to numInst - 1 do
  begin
    New(Inst);
    SongData_GetStr('SND_Name#' + IntToStr(I), Inst^.Name);
    SongData_GetArray('SND_Data#' + IntToStr(I), Inst^.Data);
    Insts.Add(Inst);
  end;
  I := TrkCh.ItemIndex;
  if (I >= 0) and (I < Length(TrackData)) then
  begin
    // Step 1: Detect percussive instruments
    if Rhythm then
      for J := 0 to Length(TrackData[I].Data) - 1 do
        if (TrackData[I].Data[J].Status shr 4 = $C) and
          (TrackData[I].Data[J].Status and $F >= 6) and
          (TrackData[I].Data[J].BParm1 < Insts.Count) then
        begin
          Inst := Insts[TrackData[I].Data[J].BParm1];
          Inst^.Perc := 1;
          Inst^.Chan := TrackData[I].Data[J].Status and $F;
        end;
    // Step 2.0: Collecting delta ticks information
    Ticks := TList.Create;
    for J := 0 to Length(TrackData[I].Data) - 1 do
      if TrackData[I].Data[J].Ticks > 0 then
      begin
        Tick := nil;
        for K := 0 to Ticks.Count - 1 do
        begin
          Tick := Ticks[K];
          if Tick^.Ticks = TrackData[I].Data[J].Ticks then
            Break;
          if Tick^.Ticks > TrackData[I].Data[J].Ticks then
          begin
            New(Tick);
            Tick^.Ticks := TrackData[I].Data[J].Ticks;
            Ticks.Insert(K, Tick);
            Break;
          end;
          Tick := nil;
        end;
        if Tick = nil then
        begin
          New(Tick);
          Tick^.Ticks := TrackData[I].Data[J].Ticks;
          Ticks.Add(Tick);
        end;
      end;
    // Step 2.1: Optimize ticks with basic tempo
    if (Ticks.Count > 0) and ((Division > 220) or
      (TPB mod PTickData(Ticks[0])^.Ticks > 0)) then
    begin
      SetLength(CheckTPB, 8);
      // Probe TPBs by priority
      CheckTPB[0] := 4;
      CheckTPB[1] := 3;
      CheckTPB[2] := 2;
      CheckTPB[3] := 5;
      CheckTPB[4] := 6;
      CheckTPB[5] := 8;
      CheckTPB[6] := 10;
      CheckTPB[7] := 12;
      for J := 0 to Length(CheckTPB) - 1 do
      begin
        // Minimum delta ticks with selected TPB
        MinTicksTPB := TPB div CheckTPB[J];
        Tick := Ticks[0];
        // Supposed basic tempo
        NewTempo := Round(Division * MinTicksTPB / Tick^.Ticks);
        // Error correction
        if NewTempo mod 5 = 4 then
          Inc(NewTempo);
        if NewTempo mod 5 = 1 then
          Dec(NewTempo);
        // Is calculated basic tempo optimal?
        if (NewTempo < 90) or (NewTempo > 220) or (NewTempo mod 5 > 0) then
          CheckTPB[J] := 0; // don't use it
      end;
      Idx := -1;
      MinError := 9000;
      // Calculate TPB with minimal error
      for J := 0 to Length(CheckTPB) - 1 do
      begin
        if CheckTPB[J] = 0 then
          Continue;
        MinTicksTPB := TPB div CheckTPB[J];
        Tick := Ticks[0];
        NewTempo := Round(Division * MinTicksTPB / Tick^.Ticks);
        if NewTempo mod 5 = 4 then
          Inc(NewTempo);
        if NewTempo mod 5 = 1 then
          Dec(NewTempo);
        // Calculate error sum for selected TPB
        Error := 0;
        for K := 0 to Ticks.Count - 1 do
        begin
          Tick := Ticks[0];
          MinTick := Tick^.Ticks;
          Tick := Ticks[K];
          Error := Error + Abs(MinTicksTPB * Round(Tick^.Ticks / MinTick) -
            (Tick^.Ticks * NewTempo / Division));
        end;
        if Error < MinError then
        begin
          MinError := Error;
          Idx := J;
        end;
      end;
      if Idx > -1 then
      begin // Optimal Tempo found, convert ticks
        MinTicksTPB := TPB div CheckTPB[Idx];
        Tick := Ticks[0];
        // Update basic tempo
        Division := Round(Division * MinTicksTPB / Tick^.Ticks);
        if Division mod 5 = 4 then
          Inc(Division);
        if Division mod 5 = 1 then
          Dec(Division);
        // Update ticks reference
        MinTick := Tick^.Ticks;
        for J := 0 to Ticks.Count - 1 do
        begin
          Tick := Ticks[J];
          Tick^.Ticks := MinTicksTPB * Round(Tick^.Ticks / MinTick);
        end;
        // Update delta ticks
        for J := 0 to Length(TrackData[I].Data) - 1 do
          if TrackData[I].Data[J].Ticks > 0 then
            TrackData[I].Data[J].Ticks :=
              MinTicksTPB * Round(TrackData[I].Data[J].Ticks / MinTick);
      end;
    end;
    // Step 2.2: Optimize ticks per beat
    if (Ticks.Count > 0) and (TPB > 12) and
      (TPB mod PTickData(Ticks[0])^.Ticks = 0) then
    begin
      Tick := Ticks[0];
      MinTick := Tick^.Ticks;
      // Update ticks per beat
      TPB := TPB div MinTick;
      // Update ticks reference
      for J := 0 to Ticks.Count - 1 do
      begin
        Tick := Ticks[J];
        Tick^.Ticks := Tick^.Ticks div MinTick;
      end;
      // Update delta ticks
      for J := 0 to Length(TrackData[I].Data) - 1 do
        if TrackData[I].Data[J].Ticks > 0 then
          TrackData[I].Data[J].Ticks :=
            TrackData[I].Data[J].Ticks div MinTick;
    end;
    // Step 2.3: Free the references
    for J := 0 to Ticks.Count - 1 do
    begin
      Tick := Ticks[J];
      Dispose(Tick);
    end;
    Ticks.Free;
    // Step 3: Insert omitted volume changes
    J := 0;
    FillChar(Volumes, SizeOf(Volumes), -1);
    while J < Length(TrackData[I].Data) do
    begin
      case TrackData[I].Data[J].Status shr 4 of
        8: // Note Off
        begin
          if TrackData[I].Data[J].BParm2 > 0 then
          begin
            if Volumes[TrackData[I].Data[J].Status and $F] <>
              shortint(TrackData[I].Data[J].BParm2) then
            begin
              Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm2;
              NewEvent(I, J, $A0, 0);
              TrackData[I].Data[J].Ticks := TrackData[I].Data[J + 1].Ticks;
              TrackData[I].Data[J].Status :=
                $A0 or TrackData[I].Data[J + 1].Status and $F;
              TrackData[I].Data[J].BParm1 := Volumes[TrackData[I].Data[J].Status and $F];
              TrackData[I].Data[J].BParm2 := 0;
              Inc(J);
              TrackData[I].Data[J].Ticks := 0;
            end;
            TrackData[I].Data[J].Status := $90 or TrackData[I].Data[J].Status and $F;
            TrackData[I].Data[J].BParm2 := 0;
          end;
          TrackData[I].Data[J].BParm1 := 0;
        end;
        9: // Note On
        begin
          if TrackData[I].Data[J].BParm2 > 0 then
          begin
            if Volumes[TrackData[I].Data[J].Status and $F] <>
              shortint(TrackData[I].Data[J].BParm2) then
            begin
              Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm2;
              NewEvent(I, J, $A0, 0);
              TrackData[I].Data[J].Ticks := TrackData[I].Data[J + 1].Ticks;
              TrackData[I].Data[J].Status :=
                $A0 or TrackData[I].Data[J + 1].Status and $F;
              TrackData[I].Data[J].BParm1 := Volumes[TrackData[I].Data[J].Status and $F];
              TrackData[I].Data[J].BParm2 := 0;
              Inc(J);
              TrackData[I].Data[J].Ticks := 0;
            end;
            TrackData[I].Data[J].BParm2 := 127;
          end
          else
            TrackData[I].Data[J].BParm1 := 0;
        end;
        10: // Volume Change
        begin
          if Volumes[TrackData[I].Data[J].Status and $F] =
            shortint(TrackData[I].Data[J].BParm1) then
          begin // Remove excess
            DelEvent(I, J, True);
            Continue;
          end
          else
            Volumes[TrackData[I].Data[J].Status and $F] :=
              shortint(TrackData[I].Data[J].BParm1);
        end;
      end;
      Inc(J);
    end;
    // Step 4: Convert events
    J := 0;
    while J < Length(TrackData[I].Data) do
    begin
      case TrackData[I].Data[J].Status shr 4 of
        10: // Volume Change
        begin
          Fl := TrackData[I].Data[J].BParm1 / 127;
          TrackData[I].Data[J].BParm2 := TrackData[I].Data[J].Status and $F;
          TrackData[I].Data[J].Status := $FF;
          TrackData[I].Data[J].BParm1 := $7F;

          SetLength(TrackData[I].Data[J].DataArray, 5);
          TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);

          TrackData[I].Data[J].DataArray[0] := 2;
          Move(Fl, TrackData[I].Data[J].DataArray[1], 4);
        end;
        12: // Program Change
        begin
          SetLength(TrackData[I].Data[J].DataArray, 1 + 9 + 1 + 2);
          TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);

          FillChar(TrackData[I].Data[J].DataArray[0], TrackData[I].Data[J].Len, 0);
          TrackData[I].Data[J].DataArray[0] := 1;
          if TrackData[I].Data[J].BParm1 < Insts.Count then
          begin
            Inst := Insts[TrackData[I].Data[J].BParm1];
            if Length(Inst^.Name) > 8 then
              SetLength(Inst^.Name, 8);
            Move(ansistring(Inst^.Name)[1], TrackData[I].Data[J].DataArray[1], 8);
            TrackData[I].Data[J].DataArray[11] := TrackData[I].Data[J].BParm1;
          end;
          TrackData[I].Data[J].BParm2 := TrackData[I].Data[J].Status and $F;
          TrackData[I].Data[J].Status := $FF;
          TrackData[I].Data[J].BParm1 := $7F;
        end;
        14: // Pitch Bend
        begin
          Fl := TrackData[I].Data[J].Value / 8192;
          TrackData[I].Data[J].BParm2 := TrackData[I].Data[J].Status and $F;
          TrackData[I].Data[J].Status := $FF;
          TrackData[I].Data[J].BParm1 := $7F;

          SetLength(TrackData[I].Data[J].DataArray, 5);
          TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);

          TrackData[I].Data[J].DataArray[0] := 3;
          Move(Fl, TrackData[I].Data[J].DataArray[1], 4);
        end;
        15: // System Event
        begin
          case TrackData[I].Data[J].Status and $F of
            0: // SysEx
            begin
              if (TrackData[I].Data[J].Len = 5) and
                (TrackData[I].Data[J].DataArray[0] = $7F) then
              begin
                // Tempo
                TrackData[I].Data[J].Status := $FF;
                TrackData[I].Data[J].BParm1 := $7F;
                if ((TrackData[I].Data[J].DataArray[3] / 128) +
                  TrackData[I].Data[J].DataArray[2]) <> 0 then
                  Fl := (TrackData[I].Data[J].DataArray[3] / 128) +
                    TrackData[I].Data[J].DataArray[2]
                else
                  Fl := 1;

                SetLength(TrackData[I].Data[J].DataArray, 5);
                TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);

                TrackData[I].Data[J].DataArray[0] := 0;
                Move(Fl, TrackData[I].Data[J].DataArray[1], 4);
              end;
            end;
            $C: // Song End
            begin
              DelEvent(I, J, True);
              Continue;
            end;
          end;
        end;
      end;
      Inc(J);
    end;
    // Step 5: Split track by channels
    Idx := I;
    ConvertTicks(True, TrackData[Idx].Data);
    // Add tempo track
    SetLength(TrackData, Length(TrackData) + 1);
    I := High(TrackData);
    TrackData[I].Title := 'Tempo';
    SetLength(TrackData[I].Data, 0);
    // Add ROL channel tracks
    for J := 0 to 10 do
    begin
      SetLength(TrackData, Length(TrackData) + 1);
      I := High(TrackData);
      TrackData[I].Title := ansistring(Format('Voix %2d', [J]));
      SetLength(TrackData[I].Data, 0);

      SetLength(TrackData, Length(TrackData) + 1);
      I := High(TrackData);
      TrackData[I].Title := ansistring(Format('Timbre %2d', [J]));
      SetLength(TrackData[I].Data, 0);

      SetLength(TrackData, Length(TrackData) + 1);
      I := High(TrackData);
      TrackData[I].Title := ansistring(Format('Volume %2d', [J]));
      SetLength(TrackData[I].Data, 0);

      SetLength(TrackData, Length(TrackData) + 1);
      I := High(TrackData);
      TrackData[I].Title := ansistring(Format('Pitch %2d', [J]));
      SetLength(TrackData[I].Data, 0);
    end;
    // Move events
    for J := 0 to Length(TrackData[Idx].Data) - 1 do
      case TrackData[Idx].Data[J].Status shr 4 of
        9: // Voix
        begin
          I := (TrackData[Idx].Data[J].Status and $F) * 4 + 1;
          I := I + (Length(TrackData) - 45);
          SetLength(TrackData[I].Data, Length(TrackData[I].Data) + 1);
          TrackData[I].Data[High(TrackData[I].Data)] := TrackData[Idx].Data[J];
        end;
        15:
        begin
          case TrackData[Idx].Data[J].DataArray[0] of
            0: // Tempo
              I := Length(TrackData) - 45;
            1: // Timbre
            begin
              I := (TrackData[Idx].Data[J].BParm2 and $F) * 4 + 2;
              I := I + (Length(TrackData) - 45);
              TrackData[Idx].Data[J].BParm2 := 0;
            end;
            2: // Volume
            begin
              I := (TrackData[Idx].Data[J].BParm2 and $F) * 4 + 3;
              I := I + (Length(TrackData) - 45);
              TrackData[Idx].Data[J].BParm2 := 0;
            end;
            3: // Pitch
            begin
              I := (TrackData[Idx].Data[J].BParm2 and $F) * 4 + 4;
              I := I + (Length(TrackData) - 45);
              TrackData[Idx].Data[J].BParm2 := 0;
            end;
          end;
          SetLength(TrackData[I].Data, Length(TrackData[I].Data) + 1);
          TrackData[I].Data[High(TrackData[I].Data)] := TrackData[Idx].Data[J];
        end;
      end;
    for I := High(TrackData) downto High(TrackData) - 44 do
      ConvertTicks(False, TrackData[I].Data);
    // Delete old track
    DelTrack(Idx);
    // Step 6: Delete unnecessary Note Offs
    for K := 0 to 10 do
    begin
      I := (Length(TrackData) - 45) + K * 4 + 1;
      J := 0;
      while J < Length(TrackData[I].Data) - 1 do
      begin
        if (TrackData[I].Data[J].BParm1 = 0) and
          (TrackData[I].Data[J + 1].BParm1 > 0) and
          (TrackData[I].Data[J + 1].Ticks = 0) then
        begin
          DelEvent(I, J, True);
          Continue;
        end;
        Inc(J);
      end;
    end;
  end;

  SongData.Strings.Clear;

  SongData_PutInt('ROL_Version', 262144);
  SongData_PutStr('ROL_Signature', '\roll\default');
  SongData_PutInt('ROL_TicksPerBeat', TPB);
  SongData_PutInt('ROL_BeatPerMeasure', BPM);
  SongData_PutInt('ROL_ScaleY', 48);
  SongData_PutInt('ROL_ScaleX', 56);
  SongData_PutInt('ROL_Melodic', byte(not Rhythm));
  SongData_PutInt('ROL_BasicTempo', Division);

  // Write instruments to BNK
  SongData_PutInt('BNK_Version', 1);
  SongData_PutStr('BNK_Signature', 'ADLIB-');
  SongData_PutInt('BNK_Used', Insts.Count);
  for I := 0 to Insts.Count - 1 do
  begin
    Inst := Insts[I];
    SongData_PutInt('BNK_Idx#' + IntToStr(I), I);
    SongData_PutInt('BNK_Flags#' + IntToStr(I), 1);
    SongData_PutStr('BNK_Name#' + IntToStr(I), Inst^.Name);
    SongData_PutInt('BNK_Perc#' + IntToStr(I), Inst^.Perc);
    SongData_PutInt('BNK_Chan#' + IntToStr(I), Inst^.Chan);
    SongData_PutArray('BNK_Data#' + IntToStr(I), Inst^.Data);
    Dispose(Inst);
  end;
  Insts.Free;

  SongData_PutInt('MIDIType', 1);
  SongData_PutInt('InitTempo', 60000000 div TPB);
  SongData_PutInt('Division', Division);
  SongData_PutInt('SMPTE', 0);

  LogOutput('[+] Done.');
end;

//==============================================================================
//
// TXMICore.Convert_MDI_ROL
//
//==============================================================================
procedure TXMICore.Convert_MDI_ROL;
type
  TROLInst = record
    Name: string;
    Perc,
    Chan: byte;
    Data: array[0..13 + 13 + 2 - 1] of byte;
  end;
  PROLInst = ^TROLInst;

  TTickData = record
    Ticks: UInt64;
    New: UInt64;
  end;
  PTickData = ^TTickData;
var
  InitTempo: DWord;
  TPB, BPM: byte;
  Division: word;
  Rhythm: boolean;
  S: string;
  Insts: TList;
  Inst: PROLInst;
  I, J, K, Idx: integer;
  L: TStringList;
  CheckTPB: array of byte;
  Ticks: TList;
  Tick: PTickData;
  MinTicksTPB, NewTempo: word;
  MinError, Error: double;
  MinTick: UInt64;
  Notes: array[0..15] of shortint;
  Volumes: array[0..15] of shortint;
  Fl: single;
begin
  LogOutput('[*] Converting AdLib MDI to AdLib ROL...');
  if not SongData_GetDWord('InitTempo', InitTempo) then
    InitTempo := MIDIStdTempo;
  TPB := 60000000 div InitTempo;
  if not SongData_GetWord('Division', Division) then
    Division := 420;
  Rhythm := True;
  Insts := TList.Create;

  I := TrkCh.ItemIndex;
  if (I >= 0) and (I < Length(TrackData)) then
  begin
    // Step 1: Detect rhythm mode
    for J := 0 to Length(TrackData[I].Data) - 1 do
      if (TrackData[I].Data[J].Status = $FF) and
        (TrackData[I].Data[J].BParm1 = $7F) and
        (Length(TrackData[I].Data[J].DataArray) >= 6) and
        (TrackData[I].Data[J].DataArray[0] = $00) and
        (TrackData[I].Data[J].DataArray[1] = $00) and
        (TrackData[I].Data[J].DataArray[2] = $3F) and
        (TrackData[I].Data[J].DataArray[3] = 0) and
        (TrackData[I].Data[J].DataArray[4] = 2) then
      begin
        Rhythm := TrackData[I].Data[J].DataArray[5] > 0;
        Break;
      end;
    // Step 2: Detect unique instruments
    for J := 0 to Length(TrackData[I].Data) - 1 do
      if (TrackData[I].Data[J].Status = $FF) and
        (TrackData[I].Data[J].BParm1 = $7F) and
        (Length(TrackData[I].Data[J].DataArray) >= 6 + 28) and
        (TrackData[I].Data[J].DataArray[0] = $00) and
        (TrackData[I].Data[J].DataArray[1] = $00) and
        (TrackData[I].Data[J].DataArray[2] = $3F) and
        (TrackData[I].Data[J].DataArray[3] = 0) and
        (TrackData[I].Data[J].DataArray[4] = 1) then
      begin
        Idx := -1;
        for K := 0 to Insts.Count - 1 do
        begin
          Inst := Insts[K];
          // Compare instrument data
          if CompareMem(@TrackData[I].Data[J].DataArray[6],
            @Inst^.Data[0], 28) then
          begin
            Idx := K;
            Break;
          end;
        end;
        if Idx = -1 then
        begin
          // Store new instrument
          New(Inst);
          if Rhythm and (TrackData[I].Data[J].DataArray[5] >= 6) then
          begin
            Inst^.Perc := 1;
            Inst^.Chan := TrackData[I].Data[J].DataArray[5];
          end;
          CopyMemory(@Inst^.Data[0], @TrackData[I].Data[J].DataArray[6], 28);
          Idx := Insts.Count;
          Inst^.Name := Format('inst%.4d', [Idx]);
          Insts.Add(Inst);
        end;
        // Update event
        Inst := Insts[Idx];
        TrackData[I].Data[J].BParm2 := TrackData[I].Data[J].DataArray[5];

        SetLength(TrackData[I].Data[J].DataArray, 1 + 9 + 1 + 2);
        TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);

        FillChar(TrackData[I].Data[J].DataArray[0], TrackData[I].Data[J].Len, 0);
        TrackData[I].Data[J].DataArray[0] := 1;

        Move(ansistring(Inst^.Name)[1], TrackData[I].Data[J].DataArray[1], 8);
        TrackData[I].Data[J].DataArray[11] := Idx;
      end;
    // Step 3.0: Collecting delta ticks information
    Ticks := TList.Create;
    for J := 0 to Length(TrackData[I].Data) - 1 do
      if TrackData[I].Data[J].Ticks > 0 then
      begin
        Tick := nil;
        for K := 0 to Ticks.Count - 1 do
        begin
          Tick := Ticks[K];
          if Tick^.Ticks = TrackData[I].Data[J].Ticks then
            Break;
          if Tick^.Ticks > TrackData[I].Data[J].Ticks then
          begin
            New(Tick);
            Tick^.Ticks := TrackData[I].Data[J].Ticks;
            Ticks.Insert(K, Tick);
            Break;
          end;
          Tick := nil;
        end;
        if Tick = nil then
        begin
          New(Tick);
          Tick^.Ticks := TrackData[I].Data[J].Ticks;
          Ticks.Add(Tick);
        end;
      end;
    // Step 3.1: Optimize ticks with basic tempo
    if (Ticks.Count > 0) and ((Division > 220) or
      (TPB mod PTickData(Ticks[0])^.Ticks > 0)) then
    begin
      SetLength(CheckTPB, 8);
      // Probe TPBs by priority
      CheckTPB[0] := 4;
      CheckTPB[1] := 3;
      CheckTPB[2] := 2;
      CheckTPB[3] := 5;
      CheckTPB[4] := 6;
      CheckTPB[5] := 8;
      CheckTPB[6] := 10;
      CheckTPB[7] := 12;
      for J := 0 to Length(CheckTPB) - 1 do
      begin
        // Minimum delta ticks with selected TPB
        MinTicksTPB := TPB div CheckTPB[J];
        Tick := Ticks[0];
        // Supposed basic tempo
        NewTempo := Round(Division * MinTicksTPB / Tick^.Ticks);
        // Error correction
        if NewTempo mod 5 = 4 then
          Inc(NewTempo);
        if NewTempo mod 5 = 1 then
          Dec(NewTempo);
        // Is calculated basic tempo optimal?
        if (NewTempo < 90) or (NewTempo > 220) or (NewTempo mod 5 > 0) then
          CheckTPB[J] := 0; // don't use it
      end;
      Idx := -1;
      MinError := 9000;
      // Calculate TPB with minimal error
      for J := 0 to Length(CheckTPB) - 1 do
      begin
        if CheckTPB[J] = 0 then
          Continue;
        MinTicksTPB := TPB div CheckTPB[J];
        Tick := Ticks[0];
        NewTempo := Round(Division * MinTicksTPB / Tick^.Ticks);
        if NewTempo mod 5 = 4 then
          Inc(NewTempo);
        if NewTempo mod 5 = 1 then
          Dec(NewTempo);
        // Calculate error sum for selected TPB
        Error := 0;
        for K := 0 to Ticks.Count - 1 do
        begin
          Tick := Ticks[0];
          MinTick := Tick^.Ticks;
          Tick := Ticks[K];
          Error := Error + Abs(MinTicksTPB * Round(Tick^.Ticks / MinTick) -
            (Tick^.Ticks * NewTempo / Division));
        end;
        if Error < MinError then
        begin
          MinError := Error;
          Idx := J;
        end;
      end;
      if Idx > -1 then
      begin // Optimal Tempo found, convert ticks
        MinTicksTPB := TPB div CheckTPB[Idx];
        Tick := Ticks[0];
        // Update basic tempo
        Division := Round(Division * MinTicksTPB / Tick^.Ticks);
        if Division mod 5 = 4 then
          Inc(Division);
        if Division mod 5 = 1 then
          Dec(Division);
        // Update ticks reference
        MinTick := Tick^.Ticks;
        for J := 0 to Ticks.Count - 1 do
        begin
          Tick := Ticks[J];
          Tick^.Ticks := MinTicksTPB * Round(Tick^.Ticks / MinTick);
        end;
        // Update delta ticks
        for J := 0 to Length(TrackData[I].Data) - 1 do
          if TrackData[I].Data[J].Ticks > 0 then
            TrackData[I].Data[J].Ticks :=
              MinTicksTPB * Round(TrackData[I].Data[J].Ticks / MinTick);
      end;
    end;
    // Step 3.2: Optimize ticks per beat
    if (Ticks.Count > 0) and (TPB > 12) and
      (TPB mod PTickData(Ticks[0])^.Ticks = 0) then
    begin
      Tick := Ticks[0];
      MinTick := Tick^.Ticks;
      // Update ticks per beat
      TPB := TPB div MinTick;
      // Update ticks reference
      for J := 0 to Ticks.Count - 1 do
      begin
        Tick := Ticks[J];
        Tick^.Ticks := Tick^.Ticks div MinTick;
      end;
      // Update delta ticks
      for J := 0 to Length(TrackData[I].Data) - 1 do
        if TrackData[I].Data[J].Ticks > 0 then
          TrackData[I].Data[J].Ticks :=
            TrackData[I].Data[J].Ticks div MinTick;
    end;
    // Step 3.3: Free the references
    for J := 0 to Ticks.Count - 1 do
    begin
      Tick := Ticks[J];
      Dispose(Tick);
    end;
    Ticks.Free;
    // Step 4: Insert omitted volume changes
    J := 0;
    FillChar(Notes, SizeOf(Notes), 0);
    FillChar(Volumes, SizeOf(Volumes), -1);
    while J < Length(TrackData[I].Data) do
    begin
      case TrackData[I].Data[J].Status shr 4 of
        8: // Note Off
        begin
          if TrackData[I].Data[J].BParm2 > 0 then
          begin
            if Volumes[TrackData[I].Data[J].Status and $F] <>
              shortint(TrackData[I].Data[J].BParm2) then
            begin
              Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm2;
              NewEvent(I, J, $D0, 0);
              TrackData[I].Data[J].Ticks := TrackData[I].Data[J + 1].Ticks;
              TrackData[I].Data[J].Status :=
                $D0 or TrackData[I].Data[J + 1].Status and $F;
              TrackData[I].Data[J].BParm1 := Volumes[TrackData[I].Data[J].Status and $F];
              TrackData[I].Data[J].BParm2 := 0;
              Inc(J);
              TrackData[I].Data[J].Ticks := 0;
            end;
            TrackData[I].Data[J].BParm2 := 0;
          end;
          TrackData[I].Data[J].Status := $90 or TrackData[I].Data[J].Status and $F;
          TrackData[I].Data[J].BParm1 := 0;
          if Notes[TrackData[I].Data[J].Status and $F] = 0 then
          begin
            // Remove excess
            DelEvent(I, J, True);
            Continue;
          end;
          Notes[TrackData[I].Data[J].Status and $F] := 0;
        end;
        9: // Note On
        begin
          if TrackData[I].Data[J].BParm2 > 0 then
          begin
            if Volumes[TrackData[I].Data[J].Status and $F] <>
              shortint(TrackData[I].Data[J].BParm2) then
            begin
              Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm2;
              NewEvent(I, J, $D0, 0);
              TrackData[I].Data[J].Ticks := TrackData[I].Data[J + 1].Ticks;
              TrackData[I].Data[J].Status :=
                $D0 or TrackData[I].Data[J + 1].Status and $F;
              TrackData[I].Data[J].BParm1 := Volumes[TrackData[I].Data[J].Status and $F];
              TrackData[I].Data[J].BParm2 := 0;
              Inc(J);
              TrackData[I].Data[J].Ticks := 0;
            end;
            TrackData[I].Data[J].BParm2 := 127;
            Notes[TrackData[I].Data[J].Status and $F] := 1;
          end
          else
          begin
            TrackData[I].Data[J].BParm1 := 0;
            if Notes[TrackData[I].Data[J].Status and $F] = 0 then
            begin
              // Remove excess
              DelEvent(I, J, True);
              Continue;
            end;
            Notes[TrackData[I].Data[J].Status and $F] := 0;
          end;
        end;
        13: // Volume Change
        begin
          if Volumes[TrackData[I].Data[J].Status and $F] =
            shortint(TrackData[I].Data[J].BParm1) then
          begin // Remove excess
            DelEvent(I, J, True);
            Continue;
          end
          else
            Volumes[TrackData[I].Data[J].Status and $F] :=
              shortint(TrackData[I].Data[J].BParm1);
        end;
      end;
      Inc(J);
    end;
    // Step 5: Convert events
    J := 0;
    while J < Length(TrackData[I].Data) do
    begin
      case TrackData[I].Data[J].Status shr 4 of
        13: // Volume Change
        begin
          Fl := TrackData[I].Data[J].BParm1 / 127;
          TrackData[I].Data[J].BParm2 := TrackData[I].Data[J].Status and $F;
          TrackData[I].Data[J].Status := $FF;
          TrackData[I].Data[J].BParm1 := $7F;

          SetLength(TrackData[I].Data[J].DataArray, 5);
          TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);

          TrackData[I].Data[J].DataArray[0] := 2;
          Move(Fl, TrackData[I].Data[J].DataArray[1], 4);
        end;
        14: // Pitch Bend
        begin
          Fl := TrackData[I].Data[J].Value / 8192;
          TrackData[I].Data[J].BParm2 := TrackData[I].Data[J].Status and $F;
          TrackData[I].Data[J].Status := $FF;
          TrackData[I].Data[J].BParm1 := $7F;

          SetLength(TrackData[I].Data[J].DataArray, 5);
          TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);

          TrackData[I].Data[J].DataArray[0] := 3;
          Move(Fl, TrackData[I].Data[J].DataArray[1], 4);
        end;
        15: // System Event
        begin
          if TrackData[I].Data[J].Status = $FF then
          begin
            case TrackData[I].Data[J].BParm1 of
              $51: // Tempo
              begin
                TrackData[I].Data[J].Status := $FF;
                TrackData[I].Data[J].BParm1 := $7F;
                if TrackData[I].Data[J].Value > 0 then
                  Fl := InitTempo / TrackData[I].Data[J].Value
                else
                  Fl := 1;

                SetLength(TrackData[I].Data[J].DataArray, 5);
                TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);

                TrackData[I].Data[J].DataArray[0] := 0;
                Move(Fl, TrackData[I].Data[J].DataArray[1], 4);
              end;
              $7F: // Sequencer Specific
              begin
                if (Length(TrackData[I].Data[J].DataArray) >= 6) and
                  (TrackData[I].Data[J].DataArray[0] = $00) and
                  (TrackData[I].Data[J].DataArray[1] = $00) and
                  (TrackData[I].Data[J].DataArray[2] = $3F) then
                begin
                  DelEvent(I, J, True);
                  Continue;
                end;
              end;
              else
              begin
                DelEvent(I, J, True);
                Continue;
              end;
            end;
          end
          else
          begin
            DelEvent(I, J, True);
            Continue;
          end;
        end;
      end;
      Inc(J);
    end;
    // Step 6: Split track by channels
    Idx := I;
    ConvertTicks(True, TrackData[Idx].Data);
    // Add tempo track
    SetLength(TrackData, Length(TrackData) + 1);
    I := High(TrackData);
    TrackData[I].Title := 'Tempo';
    SetLength(TrackData[I].Data, 0);
    // Add ROL channel tracks
    for J := 0 to 10 do
    begin
      SetLength(TrackData, Length(TrackData) + 1);
      I := High(TrackData);
      TrackData[I].Title := ansistring(Format('Voix %2d', [J]));
      SetLength(TrackData[I].Data, 0);

      SetLength(TrackData, Length(TrackData) + 1);
      I := High(TrackData);
      TrackData[I].Title := ansistring(Format('Timbre %2d', [J]));
      SetLength(TrackData[I].Data, 0);

      SetLength(TrackData, Length(TrackData) + 1);
      I := High(TrackData);
      TrackData[I].Title := ansistring(Format('Volume %2d', [J]));
      SetLength(TrackData[I].Data, 0);

      SetLength(TrackData, Length(TrackData) + 1);
      I := High(TrackData);
      TrackData[I].Title := ansistring(Format('Pitch %2d', [J]));
      SetLength(TrackData[I].Data, 0);
    end;
    // Move events
    for J := 0 to Length(TrackData[Idx].Data) - 1 do
      case TrackData[Idx].Data[J].Status shr 4 of
        9: // Voix
        begin
          I := (TrackData[Idx].Data[J].Status and $F) * 4 + 1;
          I := I + (Length(TrackData) - 45);
          SetLength(TrackData[I].Data, Length(TrackData[I].Data) + 1);
          TrackData[I].Data[High(TrackData[I].Data)] := TrackData[Idx].Data[J];
        end;
        15:
        begin
          case TrackData[Idx].Data[J].DataArray[0] of
            0: // Tempo
              I := Length(TrackData) - 45;
            1: // Timbre
            begin
              I := (TrackData[Idx].Data[J].BParm2 and $F) * 4 + 2;
              I := I + (Length(TrackData) - 45);
              TrackData[Idx].Data[J].BParm2 := 0;
            end;
            2: // Volume
            begin
              I := (TrackData[Idx].Data[J].BParm2 and $F) * 4 + 3;
              I := I + (Length(TrackData) - 45);
              TrackData[Idx].Data[J].BParm2 := 0;
            end;
            3: // Pitch
            begin
              I := (TrackData[Idx].Data[J].BParm2 and $F) * 4 + 4;
              I := I + (Length(TrackData) - 45);
              TrackData[Idx].Data[J].BParm2 := 0;
            end;
          end;
          SetLength(TrackData[I].Data, Length(TrackData[I].Data) + 1);
          TrackData[I].Data[High(TrackData[I].Data)] := TrackData[Idx].Data[J];
        end;
      end;
    for I := High(TrackData) downto High(TrackData) - 44 do
      ConvertTicks(False, TrackData[I].Data);
    // Delete old track
    DelTrack(Idx);
    // Step 7: Delete unnecessary Note Offs
    for K := 0 to 10 do
    begin
      I := (Length(TrackData) - 45) + K * 4 + 1;
      J := 0;
      while J < Length(TrackData[I].Data) - 1 do
      begin
        if (TrackData[I].Data[J].BParm1 = 0) and
          (TrackData[I].Data[J + 1].BParm1 > 0) and
          (TrackData[I].Data[J + 1].Ticks = 0) then
        begin
          DelEvent(I, J, True);
          Continue;
        end;
        Inc(J);
      end;
    end;
  end;

  SongData.Strings.Clear;

  SongData_PutInt('ROL_Version', 262144);
  SongData_PutStr('ROL_Signature', '\roll\default');
  SongData_PutInt('ROL_TicksPerBeat', TPB);
  SongData_PutInt('ROL_BeatPerMeasure', 4);
  SongData_PutInt('ROL_ScaleY', 48);
  SongData_PutInt('ROL_ScaleX', 56);
  SongData_PutInt('ROL_Melodic', byte(not Rhythm));
  SongData_PutInt('ROL_BasicTempo', Division);

  // Write instruments to BNK
  SongData_PutInt('BNK_Version', 1);
  SongData_PutStr('BNK_Signature', 'ADLIB-');
  SongData_PutInt('BNK_Used', Insts.Count);
  for I := 0 to Insts.Count - 1 do
  begin
    Inst := Insts[I];
    SongData_PutInt('BNK_Idx#' + IntToStr(I), I);
    SongData_PutInt('BNK_Flags#' + IntToStr(I), 1);
    SongData_PutStr('BNK_Name#' + IntToStr(I), Inst^.Name);
    SongData_PutInt('BNK_Perc#' + IntToStr(I), Inst^.Perc);
    SongData_PutInt('BNK_Chan#' + IntToStr(I), Inst^.Chan);
    SongData_PutArray('BNK_Data#' + IntToStr(I), Inst^.Data);
    Dispose(Inst);
  end;
  Insts.Free;

  SongData_PutInt('MIDIType', 1);
  SongData_PutInt('InitTempo', 60000000 div TPB);
  SongData_PutInt('Division', Division);
  SongData_PutInt('SMPTE', 0);

  LogOutput('[+] Done.');
end;

//==============================================================================
//
// TXMICore.Convert_IMS_MID
//
//==============================================================================
procedure TXMICore.Convert_IMS_MID;
var
  TPB: word;
  NewTempo: cardinal;
  TickFactor: double;
  I, J: integer;
  B: byte;
  Division: word;
  Rhythm: boolean;
  Notes: array[0..15] of shortint;
  Volumes: array[0..15] of shortint;
  SName: string;
begin
  LogOutput('[*] Converting AdLib IMS to Standard MIDI...');
  Application.ProcessMessages;
  if not SongData_GetWord('MUS_TicksPerBeat', TPB) then
  begin
    LogOutput('[-] Ticks Per Beat is not defined.');
    Exit;
  end;
  NewTempo := MIDIStdTempo;
  TickFactor := 60000000 / TPB / NewTempo;
  if SongData_GetInt('MUS_Percussive', I) then
    Rhythm := I > 0
  else
    Rhythm := False;
  for I := 0 to Length(TrackData) - 1 do
  begin
    // Step 1: Convert events
    FillChar(Notes, SizeOf(Notes), -1);
    FillChar(Volumes, SizeOf(Volumes), -1);
    J := 0;
    while J < Length(TrackData[I].Data) do
    begin
      // Filter events on unused channels
      if TrackData[I].Data[J].Status shr 4 < 15 then // Not System event
        if (Rhythm and (TrackData[I].Data[J].Status and $F > 10)) or
          ((not Rhythm) and (TrackData[I].Data[J].Status and $F > 8)) then
        begin
          DelEvent(I, J, True);
          Continue;
        end;
      // Process events
      case TrackData[I].Data[J].Status shr 4 of
        8: // Note Off / Note Retrigger
        begin
          if TrackData[I].Data[J].BParm2 > 0 then
          begin
            if Volumes[TrackData[I].Data[J].Status and $F] <>
              shortint(TrackData[I].Data[J].BParm2) then
            begin
              Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm2;
              NewEvent(I, J, $B0, 7);
              TrackData[I].Data[J].Ticks := TrackData[I].Data[J + 1].Ticks;
              TrackData[I].Data[J].Status :=
                $B0 or TrackData[I].Data[J + 1].Status and $F;
              TrackData[I].Data[J].BParm1 := 7;
              TrackData[I].Data[J].BParm2 := Volumes[TrackData[I].Data[J].Status and $F];
              Inc(J);
              TrackData[I].Data[J].Ticks := 0;
            end;
            if Notes[TrackData[I].Data[J].Status and $F] > -1 then
            begin
              NewEvent(I, J, $90, 0);
              TrackData[I].Data[J].Ticks := TrackData[I].Data[J + 1].Ticks;
              TrackData[I].Data[J].Status :=
                $90 or TrackData[I].Data[J + 1].Status and $F;
              TrackData[I].Data[J].BParm1 := Notes[TrackData[I].Data[J].Status and $F];
              TrackData[I].Data[J].BParm2 := 0;
              if Rhythm then
                ROL_MIDIDrum(TrackData[I].Data[J]);
              Inc(J);
              TrackData[I].Data[J].Ticks := 0;
            end;
            TrackData[I].Data[J].Status := $90 or (TrackData[I].Data[J].Status and $F);
            TrackData[I].Data[J].BParm2 := $7F;
            Notes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm1;
          end;
          if Rhythm then
            ROL_MIDIDrum(TrackData[I].Data[J]);
        end;
        9: // Note On
        begin
          if TrackData[I].Data[J].BParm2 > 0 then
          begin
            if Volumes[TrackData[I].Data[J].Status and $F] <>
              shortint(TrackData[I].Data[J].BParm2) then
            begin
              Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm2;
              NewEvent(I, J, $B0, 7);
              TrackData[I].Data[J].Ticks := TrackData[I].Data[J + 1].Ticks;
              TrackData[I].Data[J].Status :=
                $B0 or TrackData[I].Data[J + 1].Status and $F;
              TrackData[I].Data[J].BParm1 := 7;
              TrackData[I].Data[J].BParm2 := Volumes[TrackData[I].Data[J].Status and $F];
              Inc(J);
              TrackData[I].Data[J].Ticks := 0;
            end;
            TrackData[I].Data[J].BParm2 := 127;
            Notes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm1;
          end;
          if Rhythm then
            ROL_MIDIDrum(TrackData[I].Data[J]);
        end;
        10: // Poly Aftertouch -> Volume Change
        begin
          if Volumes[TrackData[I].Data[J].Status and $F] <>
            shortint(TrackData[I].Data[J].BParm1) then
          begin
            Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm1;
            TrackData[I].Data[J].Status := $B0 or TrackData[I].Data[J].Status and $F;
            TrackData[I].Data[J].BParm1 := 7;
            TrackData[I].Data[J].BParm2 := Volumes[TrackData[I].Data[J].Status and $F];
          end
          else
          begin
            DelEvent(I, J, True);
            Continue;
          end;
        end;
        12: // Program Change
        begin
          if Rhythm and (TrackData[I].Data[J].Status and $F > 5) then
          begin // Unused on rhythm channels
            DelEvent(I, J, True);
            Continue;
          end;
          if SongData_GetStr('IMS_Name#' + IntToStr(TrackData[I].Data[J].BParm1),
            SName) then
          begin // Insert instrument name
            NewEvent(I, J, $FF, $04);
            TrackData[I].Data[J].DataString := ansistring(SName);
            Inc(J);
          end;
        end;
        15: // System Event
        begin
          case TrackData[I].Data[J].Status and $F of
            0: // SysEx
            begin
              if (TrackData[I].Data[J].Len = 5) and
                (TrackData[I].Data[J].DataArray[0] = $7F) then
              begin
                // Tempo
                TrackData[I].Data[J].Status := $FF;
                TrackData[I].Data[J].BParm1 := 81;

                if ((TrackData[I].Data[J].DataArray[3] / 128) +
                  TrackData[I].Data[J].DataArray[2]) <> 0 then
                  TrackData[I].Data[J].Value :=
                    Round(NewTempo / ((TrackData[I].Data[J].DataArray[3] / 128) +
                    TrackData[I].Data[J].DataArray[2]))
                else
                  TrackData[I].Data[J].Value := NewTempo;

                TrackData[I].Data[J].DataArray[0] :=
                  (TrackData[I].Data[J].Value shr 16) and $FF;
                TrackData[I].Data[J].DataArray[1] :=
                  (TrackData[I].Data[J].Value shr 8) and $FF;
                TrackData[I].Data[J].DataArray[2] :=
                  (TrackData[I].Data[J].Value) and $FF;

                SetLength(TrackData[I].Data[J].DataArray, 3);
                TrackData[I].Data[J].Len := 3;
              end;
            end;
            $C: // End Of Track
            begin
              TrackData[I].Data[J].Status := $FF;
              TrackData[I].Data[J].BParm1 := $2F;
            end;
          end;
        end;
      end;
      Inc(J);
    end;
    // Step 2: Convert ticks
    for J := 0 to Length(TrackData[I].Data) - 1 do
      if TrackData[I].Data[J].Ticks > 0 then
        TrackData[I].Data[J].Ticks := Round(TrackData[I].Data[J].Ticks * TickFactor);
    // Step 3: Insert pitch bend ranges
    SongData_GetByte('MUS_PitchBendRange', B);
    if B > 1 then
    begin
      for J := 0 to 5 do
      begin
        NewEvent(I, J * 5 + 0, $B0, $64);
        TrackData[I].Data[J * 5 + 0].Status := $B0 or J;
        TrackData[I].Data[J * 5 + 0].BParm2 := 0;
        NewEvent(I, J * 5 + 1, $B0, $65);
        TrackData[I].Data[J * 5 + 1].Status := $B0 or J;
        TrackData[I].Data[J * 5 + 1].BParm2 := 0;
        NewEvent(I, J * 5 + 2, $B0, 6);
        TrackData[I].Data[J * 5 + 2].Status := $B0 or J;
        TrackData[I].Data[J * 5 + 2].BParm2 := B;
        NewEvent(I, J * 5 + 3, $B0, $64);
        TrackData[I].Data[J * 5 + 3].Status := $B0 or J;
        TrackData[I].Data[J * 5 + 3].BParm2 := $7F;
        NewEvent(I, J * 5 + 4, $B0, $65);
        TrackData[I].Data[J * 5 + 4].Status := $B0 or J;
        TrackData[I].Data[J * 5 + 4].BParm2 := $7F;
      end;
      if not Rhythm then
        for J := 6 to 8 do
        begin
          NewEvent(I, J * 5 + 0, $B0, $64);
          TrackData[I].Data[J * 5 + 0].Status := $B0 or J;
          TrackData[I].Data[J * 5 + 0].BParm2 := 0;
          NewEvent(I, J * 5 + 1, $B0, $65);
          TrackData[I].Data[J * 5 + 1].Status := $B0 or J;
          TrackData[I].Data[J * 5 + 1].BParm2 := 0;
          NewEvent(I, J * 5 + 2, $B0, 6);
          TrackData[I].Data[J * 5 + 2].Status := $B0 or J;
          TrackData[I].Data[J * 5 + 2].BParm2 := B;
          NewEvent(I, J * 5 + 3, $B0, $64);
          TrackData[I].Data[J * 5 + 3].Status := $B0 or J;
          TrackData[I].Data[J * 5 + 3].BParm2 := $7F;
          NewEvent(I, J * 5 + 4, $B0, $65);
          TrackData[I].Data[J * 5 + 4].Status := $B0 or J;
          TrackData[I].Data[J * 5 + 4].BParm2 := $7F;
        end;
    end;
    // Step 4: Insert tune name
    if (I = 0) and SongData_GetStr('MUS_TuneName', SName) then
      if SName <> '' then
      begin
        NewEvent(I, 0, $FF, $03);
        TrackData[I].Data[0].DataString := ansistring(SName);
      end;
    LogOutput('[+] Track #' + IntToStr(I) + ': ' + IntToStr(
      Length(TrackData[I].Data)) + ' events converted.');
  end;
  SongData_GetWord('Division', Division);
  SongData.Strings.Clear;

  SongData_PutInt('MIDIType', 0);
  SongData_PutDWord('InitTempo', NewTempo);
  SongData_PutInt('SMPTE', 0);
  SongData_PutInt('Division', Division);
  LogOutput('[+] Done.');
end;

//==============================================================================
//
// TXMICore.Convert_IMS_ROL
//
//==============================================================================
procedure TXMICore.Convert_IMS_ROL;
type
  TTickData = record
    Ticks: UInt64;
    New: UInt64;
  end;
  PTickData = ^TTickData;
var
  TPB, BPM: byte;
  Division: word;
  Rhythm: boolean;
  S: string;
  I, J, K, Idx: integer;
  CheckTPB: array of byte;
  Ticks: TList;
  Tick: PTickData;
  MinTicksTPB, NewTempo: word;
  MinError, Error: double;
  MinTick: UInt64;
  Volumes: array[0..15] of shortint;
  Fl: single;
begin
  LogOutput('[*] Converting AdLib IMS to AdLib ROL...');
  if not SongData_GetByte('MUS_TicksPerBeat', TPB) then
    TPB := 240;
  if not SongData_GetByte('MUS_BeatPerMeasure', BPM) then
    BPM := 4;
  if not SongData_GetInt('MUS_Percussive', I) then
    I := 1;
  Rhythm := I = 1;
  if not SongData_GetWord('MUS_BasicTempo', Division) then
    Division := 120;
  I := TrkCh.ItemIndex;
  if (I >= 0) and (I < Length(TrackData)) then
  begin
    // Step 1.0: Collecting delta ticks information
    Ticks := TList.Create;
    for J := 0 to Length(TrackData[I].Data) - 1 do
      if TrackData[I].Data[J].Ticks > 0 then
      begin
        Tick := nil;
        for K := 0 to Ticks.Count - 1 do
        begin
          Tick := Ticks[K];
          if Tick^.Ticks = TrackData[I].Data[J].Ticks then
            Break;
          if Tick^.Ticks > TrackData[I].Data[J].Ticks then
          begin
            New(Tick);
            Tick^.Ticks := TrackData[I].Data[J].Ticks;
            Ticks.Insert(K, Tick);
            Break;
          end;
          Tick := nil;
        end;
        if Tick = nil then
        begin
          New(Tick);
          Tick^.Ticks := TrackData[I].Data[J].Ticks;
          Ticks.Add(Tick);
        end;
      end;
    // Step 1.1: Optimize ticks with basic tempo
    if (Ticks.Count > 0) and ((Division > 220) or
      (TPB mod PTickData(Ticks[0])^.Ticks > 0)) then
    begin
      SetLength(CheckTPB, 8);
      // Probe TPBs by priority
      CheckTPB[0] := 4;
      CheckTPB[1] := 3;
      CheckTPB[2] := 2;
      CheckTPB[3] := 5;
      CheckTPB[4] := 6;
      CheckTPB[5] := 8;
      CheckTPB[6] := 10;
      CheckTPB[7] := 12;
      for J := 0 to Length(CheckTPB) - 1 do
      begin
        // Minimum delta ticks with selected TPB
        MinTicksTPB := TPB div CheckTPB[J];
        Tick := Ticks[0];
        // Supposed basic tempo
        NewTempo := Round(Division * MinTicksTPB / Tick^.Ticks);
        // Error correction
        if NewTempo mod 5 = 4 then
          Inc(NewTempo);
        if NewTempo mod 5 = 1 then
          Dec(NewTempo);
        // Is calculated basic tempo optimal?
        if (NewTempo < 90) or (NewTempo > 220) or (NewTempo mod 5 > 0) then
          CheckTPB[J] := 0; // don't use it
      end;
      Idx := -1;
      MinError := 9000;
      // Calculate TPB with minimal error
      for J := 0 to Length(CheckTPB) - 1 do
      begin
        if CheckTPB[J] = 0 then
          Continue;
        MinTicksTPB := TPB div CheckTPB[J];
        Tick := Ticks[0];
        NewTempo := Round(Division * MinTicksTPB / Tick^.Ticks);
        if NewTempo mod 5 = 4 then
          Inc(NewTempo);
        if NewTempo mod 5 = 1 then
          Dec(NewTempo);
        // Calculate error sum for selected TPB
        Error := 0;
        for K := 0 to Ticks.Count - 1 do
        begin
          Tick := Ticks[0];
          MinTick := Tick^.Ticks;
          Tick := Ticks[K];
          Error := Error + Abs(MinTicksTPB * Round(Tick^.Ticks / MinTick) -
            (Tick^.Ticks * NewTempo / Division));
        end;
        if Error < MinError then
        begin
          MinError := Error;
          Idx := J;
        end;
      end;
      if Idx > -1 then
      begin // Optimal Tempo found, convert ticks
        MinTicksTPB := TPB div CheckTPB[Idx];
        Tick := Ticks[0];
        // Update basic tempo
        Division := Round(Division * MinTicksTPB / Tick^.Ticks);
        if Division mod 5 = 4 then
          Inc(Division);
        if Division mod 5 = 1 then
          Dec(Division);
        // Update ticks reference
        MinTick := Tick^.Ticks;
        for J := 0 to Ticks.Count - 1 do
        begin
          Tick := Ticks[J];
          Tick^.Ticks := MinTicksTPB * Round(Tick^.Ticks / MinTick);
        end;
        // Update delta ticks
        for J := 0 to Length(TrackData[I].Data) - 1 do
          if TrackData[I].Data[J].Ticks > 0 then
            TrackData[I].Data[J].Ticks :=
              MinTicksTPB * Round(TrackData[I].Data[J].Ticks / MinTick);
      end;
    end;
    // Step 1.2: Optimize ticks per beat
    if (Ticks.Count > 0) and (TPB > 12) and
      (TPB mod PTickData(Ticks[0])^.Ticks = 0) then
    begin
      Tick := Ticks[0];
      MinTick := Tick^.Ticks;
      // Update ticks per beat
      TPB := TPB div MinTick;
      // Update ticks reference
      for J := 0 to Ticks.Count - 1 do
      begin
        Tick := Ticks[J];
        Tick^.Ticks := Tick^.Ticks div MinTick;
      end;
      // Update delta ticks
      for J := 0 to Length(TrackData[I].Data) - 1 do
        if TrackData[I].Data[J].Ticks > 0 then
          TrackData[I].Data[J].Ticks :=
            TrackData[I].Data[J].Ticks div MinTick;
    end;
    // Step 1.3: Free the references
    for J := 0 to Ticks.Count - 1 do
    begin
      Tick := Ticks[J];
      Dispose(Tick);
    end;
    Ticks.Free;
    // Step 2: Insert omitted volume changes
    J := 0;
    FillChar(Volumes, SizeOf(Volumes), -1);
    while J < Length(TrackData[I].Data) do
    begin
      case TrackData[I].Data[J].Status shr 4 of
        8: // Note Off / Note Retrigger
        begin
          if TrackData[I].Data[J].BParm2 > 0 then
          begin
            if Volumes[TrackData[I].Data[J].Status and $F] <>
              shortint(TrackData[I].Data[J].BParm2) then
            begin
              Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm2;
              NewEvent(I, J, $A0, 0);
              TrackData[I].Data[J].Ticks := TrackData[I].Data[J + 1].Ticks;
              TrackData[I].Data[J].Status :=
                $A0 or TrackData[I].Data[J + 1].Status and $F;
              TrackData[I].Data[J].BParm1 := Volumes[TrackData[I].Data[J].Status and $F];
              TrackData[I].Data[J].BParm2 := 0;
              Inc(J);
              TrackData[I].Data[J].Ticks := 0;
            end;
            TrackData[I].Data[J].Status := $90 or TrackData[I].Data[J].Status and $F;
            TrackData[I].Data[J].BParm2 := 127;
          end
          else
            TrackData[I].Data[J].BParm1 := 0;
        end;
        9: // Note On
        begin
          if TrackData[I].Data[J].BParm2 > 0 then
          begin
            if Volumes[TrackData[I].Data[J].Status and $F] <>
              shortint(TrackData[I].Data[J].BParm2) then
            begin
              Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm2;
              NewEvent(I, J, $A0, 0);
              TrackData[I].Data[J].Ticks := TrackData[I].Data[J + 1].Ticks;
              TrackData[I].Data[J].Status :=
                $A0 or TrackData[I].Data[J + 1].Status and $F;
              TrackData[I].Data[J].BParm1 := Volumes[TrackData[I].Data[J].Status and $F];
              TrackData[I].Data[J].BParm2 := 0;
              Inc(J);
              TrackData[I].Data[J].Ticks := 0;
            end;
            TrackData[I].Data[J].BParm2 := 127;
          end
          else
            TrackData[I].Data[J].BParm1 := 0;
        end;
        10: // Volume Change
        begin
          if Volumes[TrackData[I].Data[J].Status and $F] =
            shortint(TrackData[I].Data[J].BParm1) then
          begin // Remove excess
            DelEvent(I, J, True);
            Continue;
          end
          else
            Volumes[TrackData[I].Data[J].Status and $F] :=
              shortint(TrackData[I].Data[J].BParm1);
        end;
      end;
      Inc(J);
    end;
    // Step 3: Convert events
    J := 0;
    while J < Length(TrackData[I].Data) do
    begin
      case TrackData[I].Data[J].Status shr 4 of
        10: // Volume Change
        begin
          Fl := TrackData[I].Data[J].BParm1 / 127;
          TrackData[I].Data[J].BParm2 := TrackData[I].Data[J].Status and $F;
          TrackData[I].Data[J].Status := $FF;
          TrackData[I].Data[J].BParm1 := $7F;

          SetLength(TrackData[I].Data[J].DataArray, 5);
          TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);

          TrackData[I].Data[J].DataArray[0] := 2;
          Move(Fl, TrackData[I].Data[J].DataArray[1], 4);
        end;
        12: // Program Change
        begin
          SetLength(TrackData[I].Data[J].DataArray, 1 + 9 + 1 + 2);
          TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);

          FillChar(TrackData[I].Data[J].DataArray[0], TrackData[I].Data[J].Len, 0);
          TrackData[I].Data[J].DataArray[0] := 1;
          if SongData_GetStr('IMS_Name#' + IntToStr(TrackData[I].Data[J].BParm1), S) then
          begin
            if Length(S) > 8 then
              SetLength(S, 8);
            Move(ansistring(S)[1], TrackData[I].Data[J].DataArray[1], 8);
            TrackData[I].Data[J].DataArray[11] := TrackData[I].Data[J].BParm1;
          end;
          TrackData[I].Data[J].BParm2 := TrackData[I].Data[J].Status and $F;
          TrackData[I].Data[J].Status := $FF;
          TrackData[I].Data[J].BParm1 := $7F;
        end;
        14: // Pitch Bend
        begin
          Fl := TrackData[I].Data[J].Value / 8192;
          TrackData[I].Data[J].BParm2 := TrackData[I].Data[J].Status and $F;
          TrackData[I].Data[J].Status := $FF;
          TrackData[I].Data[J].BParm1 := $7F;

          SetLength(TrackData[I].Data[J].DataArray, 5);
          TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);

          TrackData[I].Data[J].DataArray[0] := 3;
          Move(Fl, TrackData[I].Data[J].DataArray[1], 4);
        end;
        15: // System Event
        begin
          case TrackData[I].Data[J].Status and $F of
            0: // SysEx
            begin
              if (TrackData[I].Data[J].Len = 5) and
                (TrackData[I].Data[J].DataArray[0] = $7F) then
              begin
                // Tempo
                TrackData[I].Data[J].Status := $FF;
                TrackData[I].Data[J].BParm1 := $7F;
                if ((TrackData[I].Data[J].DataArray[3] / 128) +
                  TrackData[I].Data[J].DataArray[2]) <> 0 then
                  Fl := (TrackData[I].Data[J].DataArray[3] / 128) +
                    TrackData[I].Data[J].DataArray[2]
                else
                  Fl := 1;

                SetLength(TrackData[I].Data[J].DataArray, 5);
                TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);

                TrackData[I].Data[J].DataArray[0] := 0;
                Move(Fl, TrackData[I].Data[J].DataArray[1], 4);
              end;
            end;
            $C: // Song End
            begin
              DelEvent(I, J, True);
              Continue;
            end;
          end;
        end;
      end;
      Inc(J);
    end;
    // Step 4: Split track by channels
    Idx := I;
    ConvertTicks(True, TrackData[Idx].Data);
    // Add tempo track
    SetLength(TrackData, Length(TrackData) + 1);
    I := High(TrackData);
    TrackData[I].Title := 'Tempo';
    SetLength(TrackData[I].Data, 0);
    // Add ROL channel tracks
    for J := 0 to 10 do
    begin
      SetLength(TrackData, Length(TrackData) + 1);
      I := High(TrackData);
      TrackData[I].Title := ansistring(Format('Voix %2d', [J]));
      SetLength(TrackData[I].Data, 0);

      SetLength(TrackData, Length(TrackData) + 1);
      I := High(TrackData);
      TrackData[I].Title := ansistring(Format('Timbre %2d', [J]));
      SetLength(TrackData[I].Data, 0);

      SetLength(TrackData, Length(TrackData) + 1);
      I := High(TrackData);
      TrackData[I].Title := ansistring(Format('Volume %2d', [J]));
      SetLength(TrackData[I].Data, 0);

      SetLength(TrackData, Length(TrackData) + 1);
      I := High(TrackData);
      TrackData[I].Title := ansistring(Format('Pitch %2d', [J]));
      SetLength(TrackData[I].Data, 0);
    end;
    // Move events
    for J := 0 to Length(TrackData[Idx].Data) - 1 do
      case TrackData[Idx].Data[J].Status shr 4 of
        9: // Voix
        begin
          I := (TrackData[Idx].Data[J].Status and $F) * 4 + 1;
          I := I + (Length(TrackData) - 45);
          SetLength(TrackData[I].Data, Length(TrackData[I].Data) + 1);
          TrackData[I].Data[High(TrackData[I].Data)] := TrackData[Idx].Data[J];
        end;
        15:
        begin
          case TrackData[Idx].Data[J].DataArray[0] of
            0: // Tempo
              I := Length(TrackData) - 45;
            1: // Timbre
            begin
              I := (TrackData[Idx].Data[J].BParm2 and $F) * 4 + 2;
              I := I + (Length(TrackData) - 45);
              TrackData[Idx].Data[J].BParm2 := 0;
            end;
            2: // Volume
            begin
              I := (TrackData[Idx].Data[J].BParm2 and $F) * 4 + 3;
              I := I + (Length(TrackData) - 45);
              TrackData[Idx].Data[J].BParm2 := 0;
            end;
            3: // Pitch
            begin
              I := (TrackData[Idx].Data[J].BParm2 and $F) * 4 + 4;
              I := I + (Length(TrackData) - 45);
              TrackData[Idx].Data[J].BParm2 := 0;
            end;
          end;
          SetLength(TrackData[I].Data, Length(TrackData[I].Data) + 1);
          TrackData[I].Data[High(TrackData[I].Data)] := TrackData[Idx].Data[J];
        end;
      end;
    for I := High(TrackData) downto High(TrackData) - 44 do
      ConvertTicks(False, TrackData[I].Data);
    // Delete old track
    DelTrack(Idx);
    // Step 5: Delete unnecessary Note Offs
    for K := 0 to 10 do
    begin
      I := (Length(TrackData) - 45) + K * 4 + 1;
      J := 0;
      while J < Length(TrackData[I].Data) - 1 do
      begin
        if (TrackData[I].Data[J].BParm1 = 0) and
          (TrackData[I].Data[J + 1].BParm1 > 0) and
          (TrackData[I].Data[J + 1].Ticks = 0) then
        begin
          DelEvent(I, J, True);
          Continue;
        end;
        Inc(J);
      end;
    end;
  end;

  SongData.Strings.Clear;

  SongData_PutInt('ROL_Version', 262144);
  SongData_PutStr('ROL_Signature', '\roll\default');
  SongData_PutInt('ROL_TicksPerBeat', TPB);
  SongData_PutInt('ROL_BeatPerMeasure', BPM);
  SongData_PutInt('ROL_ScaleY', 48);
  SongData_PutInt('ROL_ScaleX', 56);
  SongData_PutInt('ROL_Melodic', byte(not Rhythm));
  SongData_PutInt('ROL_BasicTempo', Division);

  SongData_PutInt('MIDIType', 1);
  SongData_PutInt('InitTempo', 60000000 div TPB);
  SongData_PutInt('Division', Division);
  SongData_PutInt('SMPTE', 0);

  LogOutput('[+] Done.');
end;

//==============================================================================
//
// TXMICore.Convert_SOP_MID
//
//==============================================================================
procedure TXMICore.Convert_SOP_MID;
type
  NoteDur = packed record
    Chn: byte;
    Note: byte;
    Ticks: UInt64;
  end;
  PNoteDur = ^NoteDur;
var
  Division, BT: byte;
  Rhythm: boolean;
  I, J, K: integer;
  Durations: TList;
  PDur: PNoteDur;
  MinDur: UInt64;
  MinDurFirst: boolean;
  S: string;
  FoundTicks: boolean;
begin
  LogOutput('[*] Converting Sopepos'' Note to Standard MIDI...');
  Application.ProcessMessages;
  if not SongData_GetByte('SOP_TicksPerBeat', Division) then
    Division := 12;
  if not SongData_GetInt('SOP_Percussive', I) then
    I := 0;
  Rhythm := I = 1;
  if not SongData_GetByte('SOP_BasicTempo', BT) then
    BT := 120;
  for I := 0 to Length(TrackData) - 1 do
  begin
    // Convert note events to Note On/Off
    J := 0;
    Durations := TList.Create;
    while J < Length(TrackData[I].Data) do
    begin
      while TrackData[I].Data[J].Ticks > 0 do
      begin
        // Find notes with minimum duration
        // which can be turned off now
        FoundTicks := False;
        MinDur := 0;
        for K := 0 to Durations.Count - 1 do
        begin
          PDur := Durations[K];
          if not FoundTicks then
          begin
            if (PDur^.Ticks <= TrackData[I].Data[J].Ticks) then
            begin
              MinDur := PDur^.Ticks;
              FoundTicks := True;
            end;
          end
          else if (PDur^.Ticks <= TrackData[I].Data[J].Ticks) and
            (PDur^.Ticks < MinDur) then
            MinDur := PDur^.Ticks;
        end;
        if FoundTicks then
        begin
          // Found notes which needs to off
          K := 0;
          MinDurFirst := True;
          while K < Durations.Count do
          begin
            PDur := Durations[K];
            if PDur^.Ticks = MinDur then
            begin
              // Adding NoteOff event
              NewEvent(I, J, $80, 0);
              TrackData[I].Data[J].Status := $80 or (PDur^.Chn and $F);
              TrackData[I].Data[J].BParm1 := PDur^.Note;
              if MinDurFirst then
                TrackData[I].Data[J].Ticks := PDur^.Ticks
              else
                TrackData[I].Data[J].Ticks := 0;
              if Rhythm then
                ROL_MIDIDrum(TrackData[I].Data[J]);
              Inc(J);
              MinDurFirst := False;
              Dispose(PDur);
              Durations.Delete(K);
              Continue;
            end;
            // Decreasing duration
            PDur^.Ticks := PDur^.Ticks - MinDur;
            Inc(K);
          end;
          TrackData[I].Data[J].Ticks := TrackData[I].Data[J].Ticks - MinDur;
        end
        else
        begin
          for K := 0 to Durations.Count - 1 do
          begin
            // Decrease all durations by ticks
            PDur := Durations[K];
            PDur^.Ticks := PDur^.Ticks - TrackData[I].Data[J].Ticks;
          end;
          Break;
        end;
      end;

      case TrackData[I].Data[J].Status shr 4 of
        8:
        begin // Note Off - unused
          DelEvent(I, J, True);
          Continue;
        end;
        9:
        begin // SOP Note
          // Read note duration
          if TrackData[I].Data[J].Len = 0 then
          begin
            // Adding NoteOff event
            NewEvent(I, J + 1, $80, 0);
            TrackData[I].Data[J + 1].Status := $80 or (TrackData[I].Data[J].Status and $F);
            TrackData[I].Data[J + 1].BParm1 := TrackData[I].Data[J].BParm1;
            if Rhythm then
              ROL_MIDIDrum(TrackData[I].Data[J]);
            Inc(J);
          end
          else
          begin
            PDur := New(PNoteDur);
            PDur^.Chn := TrackData[I].Data[J].Status and $F;
            PDur^.Note := TrackData[I].Data[J].BParm1;
            PDur^.Ticks := TrackData[I].Data[J].Len;
            Durations.Add(PDur);
            TrackData[I].Data[J].Len := 0;
            if Rhythm then
              ROL_MIDIDrum(TrackData[I].Data[J]);
          end;
        end;
        11:
        begin
          case TrackData[I].Data[J].BParm1 of
            9: // Change Pitch
            begin
              TrackData[I].Data[J].Status := $E0 or (TrackData[I].Data[J].Status and $F);
              TrackData[I].Data[J].Value := (TrackData[I].Data[J].BParm2 * 8192) div 100;
              if TrackData[I].Data[J].Value > $3FFF then
                TrackData[I].Data[J].Value := $3FFF;
              TrackData[I].Data[J].BParm1 := TrackData[I].Data[J].Value and $FF;
              TrackData[I].Data[J].BParm2 := TrackData[I].Data[J].Value shr 8;
            end;
            17: // Change Tempo
            begin
              TrackData[I].Data[J].Status := $FF;
              TrackData[I].Data[J].BParm1 := $51;

              TrackData[I].Data[J].Value := 60000000 div TrackData[I].Data[J].BParm2;
              TrackData[I].Data[J].BParm2 := 0;

              SetLength(TrackData[I].Data[J].DataArray, 3);
              TrackData[I].Data[J].Len := 3;

              TrackData[I].Data[J].DataArray[0] :=
                (TrackData[I].Data[J].Value shr 16) and $FF;
              TrackData[I].Data[J].DataArray[1] :=
                (TrackData[I].Data[J].Value shr 8) and $FF;
              TrackData[I].Data[J].DataArray[2] := (TrackData[I].Data[J].Value) and $FF;
            end;
          end;
        end;
      end;
      if (Durations.Count > 0) and (J = High(TrackData[I].Data)) and
        (TrackData[I].Data[J].Status = $FF) and (TrackData[I].Data[J].BParm1 = $2F) then
      begin
        TrackData[I].Data[J].Ticks := 0;
        for K := 0 to Durations.Count - 1 do
        begin
          PDur := Durations[K];
          if TrackData[I].Data[J].Ticks < PDur^.Ticks then
            TrackData[I].Data[J].Ticks := PDur^.Ticks;
        end;
        Continue;
      end;
      Inc(J);
    end;
    for J := 0 to Durations.Count - 1 do
    begin
      PDur := Durations[J];
      K := Length(TrackData[I].Data);
      SetLength(TrackData[I].Data, Length(TrackData[I].Data) + 1);
      TrackData[I].Data[K].Ticks := PDur.Ticks;
      TrackData[I].Data[K].Status := $80 or PDur.Chn;
      TrackData[I].Data[K].BParm1 := PDur.Note;
      TrackData[I].Data[K].BParm2 := 0;
      if Rhythm then
        ROL_MIDIDrum(TrackData[I].Data[K]);
      Dispose(PDur);
    end;
    Durations.Free;
    K := High(TrackData[I].Data);
    if K >= 0 then
    begin
      if (TrackData[I].Data[K].Status <> $FF) or
        (TrackData[I].Data[K].BParm1 <> $2F) then
        NewEvent(I, K + 1, $FF, $2F);
    end
    else
      NewEvent(I, 0, $FF, $2F);
    LogOutput('[+] Track #' + IntToStr(I) + ': ' + IntToStr(
      Length(TrackData[I].Data)) + ' events converted.');
  end;

  // Move last track to first place
  SetLength(TrackData, Length(TrackData) + 1);
  for I := Length(TrackData) - 2 downto 0 do
    TrackData[I + 1].Data := TrackData[I].Data;
  TrackData[0].Data := TrackData[Length(TrackData) - 1].Data;
  SetLength(TrackData, Length(TrackData) - 1);

  if 60000000 div BT <> MIDIStdTempo then
  begin
    // Basic tempo
    NewEvent(0, 0, $FF, $51);
    TrackData[0].Data[0].Value := 60000000 div BT;

    SetLength(TrackData[0].Data[J].DataArray, 3);
    TrackData[0].Data[J].Len := 3;

    TrackData[0].Data[J].DataArray[0] := (TrackData[0].Data[J].Value shr 16) and $FF;
    TrackData[0].Data[J].DataArray[1] := (TrackData[0].Data[J].Value shr 8) and $FF;
    TrackData[0].Data[J].DataArray[2] := (TrackData[0].Data[J].Value) and $FF;
  end;
  // Insert file name as comment
  if SongData_GetStr('SOP_FileName', S) then
    if S <> '' then
    begin
      NewEvent(0, 0, $FF, 1);
      TrackData[0].Data[0].DataString := ansistring(S);
    end;
  // Insert song name
  if SongData_GetStr('SOP_SongName', S) then
    if S <> '' then
    begin
      NewEvent(0, 0, $FF, 3);
      TrackData[0].Data[0].DataString := ansistring(S);
    end;

  SongData.Strings.Clear;

  SongData_PutInt('MIDIType', 1);
  SongData_PutInt('InitTempo', MIDIStdTempo);
  SongData_PutInt('Division', Division);
  SongData_PutInt('SMPTE', 0);

  LogOutput('[+] Done.');
end;

//==============================================================================
//
// TXMICore.Convert_SOP_MUS
//
//==============================================================================
procedure TXMICore.Convert_SOP_MUS;
type
  NoteDur = packed record
    Chn: byte;
    Note: byte;
    Ticks: UInt64;
  end;
  PNoteDur = ^NoteDur;

  SOPInst2OP = record
    Name: string;
    Data: array[0..10] of byte;
  end;

  TSOPData = packed record
    iModChar: byte;
    iModScale: byte;
    iModAttack: byte;
    iModSustain: byte;
    iModWaveSel: byte;
    iFeedback: byte;
    iCarChar: byte;
    iCarScale: byte;
    iCarAttack: byte;
    iCarSustain: byte;
    iCarWaveSel: byte;
  end;
  PSOPData = ^TSOPData;
var
  SongName, S: string;
  Division, BT, BPM: byte;
  Rhythm: boolean;
  Speed: double;
  Volumes: array[0..15] of byte;
  I, J, K: integer;
  Durations: TList;
  PDur: PNoteDur;
  MinDur: UInt64;
  MinDurFirst: boolean;
  nInsts: byte;
  Insts: array of SOPInst2OP;
  SNDInst: array[0..13 + 13 + 2 - 1] of byte;
  PSOP: PSOPData;
  FoundTicks: boolean;
begin
  LogOutput('[*] Converting Sopepos'' Note to AdLib MUS...');
  Application.ProcessMessages;
  SongData_GetStr('SOP_SongName', SongName);
  if not SongData_GetByte('SOP_TicksPerBeat', Division) then
    Division := 12;
  if not SongData_GetInt('SOP_Percussive', I) then
    I := 0;
  Rhythm := I = 1;
  if not SongData_GetByte('SOP_BeatPerMeasure', BPM) then
    BPM := 0;
  if not SongData_GetByte('SOP_BasicTempo', BT) then
    BT := 120;

  // Move last track to first place
  SetLength(TrackData, Length(TrackData) + 1);
  for I := Length(TrackData) - 2 downto 0 do
    TrackData[I + 1].Data := TrackData[I].Data;
  TrackData[0].Data := TrackData[Length(TrackData) - 1].Data;
  SetLength(TrackData, Length(TrackData) - 1);

  // Drop incompatible channel tracks
  if Rhythm and (Length(TrackData) > 1 + 11) then
    SetLength(TrackData, 1 + 11);
  if (not Rhythm) and (Length(TrackData) > 1 + 9) then
    SetLength(TrackData, 1 + 9);

  FillChar(Volumes, 16, 127);
  for I := 0 to Length(TrackData) - 1 do
  begin
    // Convert note events to Note On/Off
    J := 0;
    Durations := TList.Create;
    while J < Length(TrackData[I].Data) do
    begin
      while TrackData[I].Data[J].Ticks > 0 do
      begin
        // Find notes with minimum duration
        // which can be turned off now
        FoundTicks := False;
        MinDur := 0;
        for K := 0 to Durations.Count - 1 do
        begin
          PDur := Durations[K];
          if not FoundTicks then
          begin
            if (PDur^.Ticks <= TrackData[I].Data[J].Ticks) then
            begin
              MinDur := PDur^.Ticks;
              FoundTicks := True;
            end;
          end
          else if (PDur^.Ticks <= TrackData[I].Data[J].Ticks) and
            (PDur^.Ticks < MinDur) then
            MinDur := PDur^.Ticks;
        end;
        if FoundTicks then
        begin
          // Found notes which needs to off
          K := 0;
          MinDurFirst := True;
          while K < Durations.Count do
          begin
            PDur := Durations[K];
            if PDur^.Ticks = MinDur then
            begin
              // Adding NoteOff event
              NewEvent(I, J, $90, 0);
              TrackData[I].Data[J].RunStatMode := True;
              TrackData[I].Data[J].Status := $90 or (PDur^.Chn and $F);
              TrackData[I].Data[J].BParm1 := PDur^.Note;
              TrackData[I].Data[J].BParm2 := 0;
              if MinDurFirst then
                TrackData[I].Data[J].Ticks := PDur^.Ticks
              else
                TrackData[I].Data[J].Ticks := 0;
              Inc(J);
              MinDurFirst := False;
              Dispose(PDur);
              Durations.Delete(K);
              Continue;
            end;
            // Decreasing duration
            PDur^.Ticks := PDur^.Ticks - MinDur;
            Inc(K);
          end;
          TrackData[I].Data[J].Ticks := TrackData[I].Data[J].Ticks - MinDur;
        end
        else
        begin
          for K := 0 to Durations.Count - 1 do
          begin
            // Decrease all durations by ticks
            PDur := Durations[K];
            PDur^.Ticks := PDur^.Ticks - TrackData[I].Data[J].Ticks;
          end;
          Break;
        end;
      end;

      case TrackData[I].Data[J].Status shr 4 of
        8:
        begin // Note Off - unused
          DelEvent(I, J, True);
          Continue;
        end;
        9:
        begin // SOP Note
          // Update volume
          TrackData[I].Data[J].RunStatMode := True;
          TrackData[I].Data[J].BParm2 := Volumes[TrackData[I].Data[J].Status and $F];
          // Read note duration
          if TrackData[I].Data[J].Len = 0 then
          begin
            // Adding NoteOff event
            NewEvent(I, J + 1, $90, 0);
            TrackData[I].Data[J + 1].RunStatMode := True;
            TrackData[I].Data[J + 1].Status := $90 or (TrackData[I].Data[J].Status and $F);
            TrackData[I].Data[J + 1].BParm1 := TrackData[I].Data[J].BParm1;
            TrackData[I].Data[J + 1].BParm2 := 0;
            Inc(J);
          end
          else
          begin
            PDur := New(PNoteDur);
            PDur^.Chn := TrackData[I].Data[J].Status and $F;
            PDur^.Note := TrackData[I].Data[J].BParm1;
            PDur^.Ticks := TrackData[I].Data[J].Len;
            Durations.Add(PDur);
            TrackData[I].Data[J].Len := 0;
          end;
        end;
        11:
        begin
          case TrackData[I].Data[J].BParm1 of
            7: // Volume Change
            begin
              TrackData[I].Data[J].Status := $A0 or (TrackData[I].Data[J].Status and $F);
              TrackData[I].Data[J].BParm1 := TrackData[I].Data[J].BParm2;
              TrackData[I].Data[J].BParm2 := 0;
              Volumes[TrackData[I].Data[J].Status and $F] := TrackData[I].Data[J].BParm1;
            end;
            9: // Change Pitch
            begin
              TrackData[I].Data[J].Status := $E0 or (TrackData[I].Data[J].Status and $F);
              TrackData[I].Data[J].Value := (TrackData[I].Data[J].BParm2 * 8192) div 100;
              if TrackData[I].Data[J].Value > $3FFF then
                TrackData[I].Data[J].Value := $3FFF;
              TrackData[I].Data[J].BParm1 := TrackData[I].Data[J].Value and $FF;
              TrackData[I].Data[J].BParm2 := TrackData[I].Data[J].Value shr 8;
            end;
            10: // Set Panning
            begin
              // Ignore
              DelEvent(I, J, True);
              Continue;
            end;
            16: // Global Volume
            begin
              // Ignore
              DelEvent(I, J, True);
              Continue;
            end;
            17: // Change Tempo
            begin
              Speed := TrackData[I].Data[J].BParm2 / BT;
              TrackData[I].Data[J].Status := $F0;
              SetLength(TrackData[I].Data[J].DataArray, 5);
              TrackData[I].Data[J].Len := Length(TrackData[I].Data[J].DataArray);
              TrackData[I].Data[J].DataArray[0] := $7F;
              TrackData[I].Data[J].DataArray[1] := $00;
              TrackData[I].Data[J].DataArray[2] := Floor(Speed);
              TrackData[I].Data[J].DataArray[3] := Round(Frac(Speed) * 128);
              TrackData[I].Data[J].DataArray[4] := $F7;
            end;
          end;
        end;
      end;
      if (Durations.Count > 0) and (J = High(TrackData[I].Data)) then
      begin
        TrackData[I].Data[J].Ticks := 0;
        for K := 0 to Durations.Count - 1 do
        begin
          PDur := Durations[K];
          if TrackData[I].Data[J].Ticks < PDur^.Ticks then
            TrackData[I].Data[J].Ticks := PDur^.Ticks;
        end;
        Continue;
      end;
      Inc(J);
    end;
    for J := 0 to Durations.Count - 1 do
    begin
      PDur := Durations[J];
      K := Length(TrackData[I].Data);
      SetLength(TrackData[I].Data, Length(TrackData[I].Data) + 1);
      TrackData[I].Data[K].Ticks := PDur.Ticks;
      TrackData[I].Data[J].RunStatMode := True;
      TrackData[I].Data[K].Status := $90 or PDur.Chn;
      TrackData[I].Data[K].BParm1 := PDur.Note;
      TrackData[I].Data[K].BParm2 := 0;
      Dispose(PDur);
    end;
    Durations.Free;
    LogOutput('[+] Track #' + IntToStr(I) + ': ' + IntToStr(
      Length(TrackData[I].Data)) + ' events converted.');
  end;

  MergeTracksByTicks([]);

  K := High(TrackData[0].Data);
  if K >= 0 then
  begin
    if (TrackData[0].Data[K].Status = $FF) and
      (TrackData[0].Data[K].BParm1 = $2F) then
      TrackData[0].Data[K].Status := $FC
    else
    if TrackData[0].Data[K].Status <> $FC then
      NewEvent(0, K + 1, $FC, 0);
  end
  else
    NewEvent(0, 0, $FC, 0);

  // Read instruments
  nInsts := 0;
  while SongData_GetStr('SOP_Inst#' + IntToStr(nInsts), S) do
    Inc(nInsts);
  SetLength(Insts, nInsts);
  for I := 0 to nInsts - 1 do
  begin
    Insts[I].Name := '';
    SongData_GetStr('SOP_SName#' + IntToStr(I), Insts[I].Name);
    if not SongData_GetArray('SOP_Data#' + IntToStr(I), Insts[I].Data) then
      FillChar(Insts[I].Data, 11, 0);
  end;

  SongData.Strings.Clear;

  // Convert instruments
  if nInsts > 0 then
  begin
    SongData_PutInt('SND_Version', 1);
    for I := 0 to nInsts - 1 do
    begin
      SongData_PutStr('SND_Name#' + IntToStr(I), Insts[I].Name);
      PSOP := @Insts[I].Data[0];
      SNDInst[0] := PSOP^.iModScale shr 6;
      SNDInst[1] := PSOP^.iModChar and $F;
      SNDInst[2] := (PSOP^.iFeedback shr 1) and 7;
      SNDInst[3] := PSOP^.iModAttack shr 4;
      SNDInst[4] := PSOP^.iModSustain shr 4;
      SNDInst[5] := (PSOP^.iModChar shr 5) and 1;
      SNDInst[6] := PSOP^.iModAttack and $F;
      SNDInst[7] := PSOP^.iModSustain and $F;
      SNDInst[8] := PSOP^.iModScale and $3F;
      SNDInst[9] := PSOP^.iModChar shr 7;
      SNDInst[10] := (PSOP^.iModChar shr 6) and 1;
      SNDInst[11] := (PSOP^.iModChar shr 4) and 1;
      SNDInst[12] := (not PSOP^.iFeedback) and 1;
      SNDInst[13] := PSOP^.iCarScale shr 6;
      SNDInst[14] := PSOP^.iCarChar and $F;
      SNDInst[15] := (not SNDInst[2]) and $7F;
      SNDInst[16] := PSOP^.iCarAttack shr 4;
      SNDInst[17] := PSOP^.iCarSustain shr 4;
      SNDInst[18] := (PSOP^.iCarChar shr 5) and 1;
      SNDInst[19] := PSOP^.iCarAttack and $F;
      SNDInst[20] := PSOP^.iCarSustain and $F;
      SNDInst[21] := PSOP^.iCarScale and $3F;
      SNDInst[22] := PSOP^.iCarChar shr 7;
      SNDInst[23] := (PSOP^.iCarChar shr 6) and 1;
      SNDInst[24] := (PSOP^.iCarChar shr 4) and 1;
      SNDInst[25] := 1;
      SNDInst[26] := PSOP^.iModWaveSel;
      SNDInst[27] := PSOP^.iCarWaveSel;
      SongData_PutArray('SND_Data#' + IntToStr(I), SNDInst);
    end;
  end;

  SongData_PutInt('MUS_Version', 1);
  SongData_PutInt('MUS_ID', 0);
  SongData_PutStr('MUS_TuneName', SongName);
  TrackData[0].Title := ansistring(SongName);
  SongData_PutInt('MUS_TicksPerBeat', Division);
  SongData_PutInt('MUS_BeatPerMeasure', BPM);
  SongData_PutInt('MUS_Percussive', byte(Rhythm));
  SongData_PutInt('MUS_PitchBendRange', 1);
  SongData_PutInt('MIDIType', 0);
  SongData_PutInt('InitTempo', MIDIStdTempo);
  SongData_PutInt('Division', Division);
  SongData_PutInt('MUS_BasicTempo', BT);
  SongData_PutInt('SMPTE', 0);

  LogOutput('[+] Done.');
end;

//==============================================================================
//
// TXMICore.Convert_HERAD_MID
//
//==============================================================================
procedure TXMICore.Convert_HERAD_MID;
begin
  LogOutput('[*] Converting Cryo HERAD to Standard MIDI...');
  Convert_MID_FixTempo;
  LogOutput('[+] Done.');
end;

//==============================================================================
//
// TXMICore.Convert_HERAD_HERAD_V2
//
//==============================================================================
procedure TXMICore.Convert_HERAD_HERAD_V2;
var
  B: byte;
  nInsts, I: integer;
  S: string;
begin
  SongData_GetByte('HERAD_V2', B);
  if B > 0 then
    Exit;
  LogOutput('[*] Converting Cryo HERAD to version 2...');
  nInsts := 0;
  while SongData_GetStr('HERAD_Inst#' + IntToStr(nInsts), S) do
    Inc(nInsts);
  for I := 0 to nInsts - 1 do
  begin
    S := '';
    SongData_GetStr('HERAD_Inst#' + IntToStr(I), S);
    if Pos('0 ', S) = 1 then
    begin
      S[1] := '1';
      SongData_PutStr('HERAD_Inst#' + IntToStr(I), S);
    end;
  end;
  SongData_PutInt('HERAD_V2', 1);
  LogOutput('[+] Done.');
end;

//==============================================================================
//
// TXMICore.RefTrackList
//
//==============================================================================
procedure TXMICore.RefTrackList;
var
  I: integer;
begin
  TrkCh.Items.Clear;
  for I := 0 to Length(TrackData) - 1 do
    if TrackData[I].Title = '' then
      TrkCh.Items.Add(Format('%.' + IntToStr(Length(IntToStr(Length(TrackData)))) + 'd', [I]) +
        ' |')
    else
      TrkCh.Items.Add(Format('%.' + IntToStr(Length(IntToStr(Length(TrackData)))) + 'd', [I]) +
        ' | ' + TrackData[I].Title);
end;

//==============================================================================
//
// GetSongLengthTicks
//
//==============================================================================
function GetSongLengthTicks: UInt64;
var
  Ver: word;
  I, J: integer;
  X: UInt64;
begin
  Result := 0;
  if not SongData_GetWord('MIDIType', Ver) then
    Exit;
  if Ver = 0 then
  begin
    if Length(TrackData) < 1 then
      Exit;
    for I := 0 to Length(TrackData[0].Data) - 1 do
      Result := Result + TrackData[0].Data[I].Ticks;
  end;
  if Ver = 1 then
  begin
    for J := 0 to Length(TrackData) - 1 do
    begin
      X := 0;
      for I := 0 to Length(TrackData[J].Data) - 1 do
        X := X + TrackData[J].Data[I].Ticks;
      if X > Result then
        Result := X;
    end;
  end;
  if Ver = 2 then
  begin
    for J := 0 to Length(TrackData) - 1 do
      for I := 0 to Length(TrackData[J].Data) - 1 do
        Result := Result + TrackData[J].Data[I].Ticks;
  end;
end;

//==============================================================================
//
// TXMICore.MCalcLenClick
//
//==============================================================================
procedure TXMICore.MCalcLenClick(Sender: TObject);
var
  SMPTE: shortint;
  Division: word;
  Len, Time: UInt64;
  S: string;
begin
  Len := GetSongLengthTicks;
  S := 'Total ticks: ' + IntToStr(Len) + #13#10;
  if not SongData_GetSInt('SMPTE', SMPTE) then
    SMPTE := 0;
  if not SongData_GetWord('Division', Division) then
  begin
    LogOutput('[-] Division is not defined.');
    Exit;
  end;
  if SMPTE >= 0 then
  begin
    Time := Round(Len / Division * 500);
  end
  else
  begin
    Time := 0;
  end;
  S := S + 'Time: ' + Format('%.2d', [Time div (1000 * 60 * 60)]) +
    ':' + Format('%.2d', [(Time div (1000 * 60)) mod 60]) + ':' +
    Format('%.2d', [(Time div 1000) mod 60]) + '.' + Format('%.3d', [Time mod 1000]);
  MessageBox(Handle, PChar(S), 'Song length', mb_IconInformation or mb_Ok);
end;

//==============================================================================
//
// TXMICore.Changechannel1Click
//
//==============================================================================
procedure TXMICore.Changechannel1Click(Sender: TObject);
var
  I, Trk: integer;
begin
  Trk := TrkCh.ItemIndex;
  for I := 0 to Length(TrackData[Trk].Data) - 1 do
  begin
  end;
  FillEvents(TrkCh.ItemIndex);
  ChkButtons;
end;

//==============================================================================
//
// TXMICore.ChkButtons
//
//==============================================================================
procedure TXMICore.ChkButtons;
begin
  bPlay.Enabled := Opened and (MIDIThrId = 0) and (PlayerMode <> PLAY_REC);
  bPlayPos.Enabled := Opened and (MIDIThrId = 0) and (PlayerMode <> PLAY_REC);
  bStop.Enabled := Opened and ((MIDIThrId > 0) or (PlayerMode = PLAY_REC));
  bStep.Enabled := Opened and ((MIDIThrId = 0) or (PlayerMode = PLAY_STEP)) and
    (PlayerMode <> PLAY_REC);
  bRecord.Enabled := Opened and (MIDIThrId = 0) and (PlayerMode <> PLAY_REC);
  Label1.Enabled := Length(TrackData) > 0;
  TrkCh.Enabled := Length(TrackData) > 0;
  Events.Enabled := Length(TrackData) > 0;
  if Length(TrackData) = 0 then
  begin
    Events.ColCount := 1;
    Events.RowCount := 1;
    Events.DefaultColWidth := 64;
    Events.Cells[0, 0] := 'No tracks';
  end;
end;

//==============================================================================
//
// TXMICore.NewEvent
//
//==============================================================================
procedure TXMICore.NewEvent(Trk, Idx: integer; E, Parm: byte);
var
  I: integer;
  Chn: smallint;
  InitTempo: cardinal;
begin
  if not SongData_GetDWord('InitTempo', InitTempo) then
  begin
    LogOutput('[-] Initial Tempo is not defined.');
    Exit;
  end;
  Chn := -1;
  for I := 0 to Length(TrackData[Trk].Data) - 1 do
    if (TrackData[Trk].Data[I].Status = 15) and
      (TrackData[Trk].Data[I].BParm1 = 32) then
    begin
      Chn := TrackData[Trk].Data[I].DataArray[0];
      Break;
    end;
  if Chn = -1 then
    for I := 0 to Length(TrackData[Trk].Data) - 1 do
      if (TrackData[Trk].Data[I].Status shr 4) in [8..14] then
      begin
        Chn := TrackData[Trk].Data[I].Status and 15;
        Break;
      end;
  if Chn = -1 then
    if Trk in [0..15] then
      Chn := Trk
    else
      Chn := 0;
  if E < 240 then
    E := E or Chn;
  SetLength(TrackData[Trk].Data, Length(TrackData[Trk].Data) + 1);
  for I := Length(TrackData[Trk].Data) - 1 downto Idx + 1 do
    TrackData[Trk].Data[I] := TrackData[Trk].Data[I - 1];
  TrackData[Trk].Data[Idx].Ticks := 0;
  TrackData[Trk].Data[Idx].Status := E;
  TrackData[Trk].Data[Idx].RunStatMode := False;
  SetLength(TrackData[Trk].Data[Idx].DataArray, 0);
  TrackData[Trk].Data[Idx].DataString := '';
  if (E < $F0) and (Idx > 0) and (E = TrackData[Trk].Data[Idx - 1].Status) then
    TrackData[Trk].Data[Idx].RunStatMode := True;
  case E shr 4 of
    // default values
    8:
    begin  // NoteOff
      TrackData[Trk].Data[Idx].BParm1 := 48;
      TrackData[Trk].Data[Idx].BParm2 := 0;
    end;
    9:
    begin  // NoteOn
      TrackData[Trk].Data[Idx].BParm1 := 48;
      TrackData[Trk].Data[Idx].BParm2 := 127;
    end;
    10:
    begin // KeyPressure
      TrackData[Trk].Data[Idx].BParm1 := 48;
      TrackData[Trk].Data[Idx].BParm2 := 64;
    end;
    12:       // ProgramChange
      TrackData[Trk].Data[Idx].BParm1 := 0;
    13:       // ChannelPressure
      TrackData[Trk].Data[Idx].BParm1 := 64;
    14:       // PitchBend
      TrackData[Trk].Data[Idx].Value := 8192;
    11:
    begin // ControlChange
      TrackData[Trk].Data[Idx].BParm1 := Parm;
      case Parm of
        $07, $27, $75:
          TrackData[Trk].Data[Idx].BParm2 := 127;
        $08, $0A, $28, $2A:
          TrackData[Trk].Data[Idx].BParm2 := 64;
        else
          TrackData[Trk].Data[Idx].BParm2 := 0;
      end;
    end;
    15:       // System
      case E and 15 of
        0:
        begin // SysEx
          TrackData[Trk].Data[Idx].Len := 1;
          SetLength(TrackData[Trk].Data[Idx].DataArray, 1);
          TrackData[Trk].Data[Idx].DataArray[0] := $F7;
        end;
        1:       // QuarterFrame
          TrackData[Trk].Data[Idx].BParm1 := 0;
        2:       // SongPosPtr
          TrackData[Trk].Data[Idx].Value := 0;
        3:       // SongSelect
          TrackData[Trk].Data[Idx].BParm1 := 0;
        7:
        begin // EOX
          TrackData[Trk].Data[Idx].Len := 0;
          SetLength(TrackData[Trk].Data[Idx].DataArray, 0);
        end;
        15:
        begin // Meta
          TrackData[Trk].Data[Idx].BParm1 := Parm;
          case Parm of
            0:
            begin  // Sequence Number
              TrackData[Trk].Data[Idx].Len := 2;
              TrackData[Trk].Data[Idx].Value := Trk;
              SetLength(TrackData[Trk].Data[Idx].DataArray, 2);
              TrackData[Trk].Data[Idx].DataArray[0] := (Trk shr 8) and $FF;
              TrackData[Trk].Data[Idx].DataArray[1] := Trk and $FF;
            end;
            1..7:
            begin  // Strings
              TrackData[Trk].Data[Idx].Len := 0;
              TrackData[Trk].Data[Idx].DataString := '';
            end;
            $40:
            begin  // MIDI Channel
              TrackData[Trk].Data[Idx].Len := 1;
              TrackData[Trk].Data[Idx].Value := Chn;
              SetLength(TrackData[Trk].Data[Idx].DataArray, 1);
              TrackData[Trk].Data[Idx].DataArray[0] := Chn;
            end;
            $41:
            begin  // MIDI Port
              TrackData[Trk].Data[Idx].Len := 1;
              TrackData[Trk].Data[Idx].Value := 0;
              SetLength(TrackData[Trk].Data[Idx].DataArray, 1);
              TrackData[Trk].Data[Idx].DataArray[0] := 0;
            end;
            $51:
            begin  // Tempo
              TrackData[Trk].Data[Idx].Len := 3;
              TrackData[Trk].Data[Idx].Value := InitTempo;
              SetLength(TrackData[Trk].Data[Idx].DataArray, 3);
              TrackData[Trk].Data[Idx].DataArray[0] := $07;
              TrackData[Trk].Data[Idx].DataArray[1] := $A1;
              TrackData[Trk].Data[Idx].DataArray[2] := $20;
            end;
            $54:
            begin  // SMPTE Offset
              TrackData[Trk].Data[Idx].Len := 5;
              SetLength(TrackData[Trk].Data[Idx].DataArray, 5);
              TrackData[Trk].Data[Idx].DataArray[0] := $60;
              TrackData[Trk].Data[Idx].DataArray[1] := $00;
              TrackData[Trk].Data[Idx].DataArray[2] := $03;
              TrackData[Trk].Data[Idx].DataArray[3] := $00;
              TrackData[Trk].Data[Idx].DataArray[4] := $00;
            end;
            $58:
            begin  // TimeSignature
              TrackData[Trk].Data[Idx].Len := 4;
              SetLength(TrackData[Trk].Data[Idx].DataArray, 4);
              TrackData[Trk].Data[Idx].DataArray[0] := $06;
              TrackData[Trk].Data[Idx].DataArray[1] := $03;
              TrackData[Trk].Data[Idx].DataArray[2] := $24;
              TrackData[Trk].Data[Idx].DataArray[3] := $08;
            end;
            $59:
            begin  // KeySignature
              TrackData[Trk].Data[Idx].Len := 2;
              SetLength(TrackData[Trk].Data[Idx].DataArray, 2);
              TrackData[Trk].Data[Idx].DataArray[0] := $00;
              TrackData[Trk].Data[Idx].DataArray[1] := $00;
            end;
            else
            begin
              TrackData[Trk].Data[Idx].Len := 0;
              SetLength(TrackData[Trk].Data[Idx].DataArray, 0);
            end;
          end;
        end;
      end;
  end;
end;

//==============================================================================
//
// TXMICore.AddEvent
//
//==============================================================================
procedure TXMICore.AddEvent(E, Parm: byte);
var
  Trk, Pos: UInt64;
begin
  Trk := TrkCh.ItemIndex;
  Pos := 0;
  if (Events.Row - 1) >= 0 then
    Pos := Events.Row - 1;
  NewEvent(Trk, Pos, E, Parm);
  FillEvents(TrkCh.ItemIndex);
  if Length(TrackData[Trk].Data) = 1 then
    Events.Row := 1
  else
    Events.Row := Events.Row + 1;
  ChkButtons;
end;

//==============================================================================
//
// TXMICore.MDelTracksClick
//
//==============================================================================
procedure TXMICore.MDelTracksClick(Sender: TObject);
var
  Idx, I: integer;
begin
  Idx := TrkCh.ItemIndex;
  for I := Length(TrackData) - 1 downto 0 do
    if I <> Idx then
      DelTrack(I);
  RefTrackList;
  if Length(TrackData) > 0 then
  begin
    TrkCh.ItemIndex := 0;
    FillEvents(TrkCh.ItemIndex);
  end;
  ChkButtons;
end;

//==============================================================================
//
// TXMICore.OnEventChange
//
//==============================================================================
procedure TXMICore.OnEventChange(var Msg: TMessage);
begin
  if Msg.LParam <> TrkCh.ItemIndex then
    Exit;
  vEvntIndex := Msg.WParam;
  vTrkIndex := Msg.LParam;
end;

//==============================================================================
//
// TXMICore.OnTrackChange
//
//==============================================================================
procedure TXMICore.OnTrackChange(var Msg: TMessage);
begin
  vTrkIndex := Msg.WParam;
  vChangeTrack := True;
end;

//==============================================================================
//
// TXMICore.OnVUChange
//
//==============================================================================
procedure TXMICore.OnVUChange(var Msg: TMessage);
begin
  if not Msg.WParam in [0..15] then
    Exit;
  if not Msg.LParam in [0..127] then
    Exit;
  if VU[Msg.WParam] < Msg.LParam then
    VU[Msg.WParam] := Msg.LParam;
end;

//==============================================================================
//
// TXMICore.OnPlayerChange
//
//==============================================================================
procedure TXMICore.OnPlayerChange(var Msg: TMessage);
begin
  case Msg.WParam of
    PLAYER_PLAY:
    begin
      bPlay.Enabled := False;
      bPlayPos.Enabled := False;
      bStop.Enabled := True;
      bStep.Enabled := PlayerMode = PLAY_STEP;
      bRecord.Enabled := False;
    end;
    PLAYER_STOP:
    begin
      bPlay.Enabled := True;
      bPlayPos.Enabled := True;
      bStop.Enabled := False;
      bStep.Enabled := True;
      bRecord.Enabled := True;
    end;
    PLAYER_REC:
    begin
      bPlay.Enabled := False;
      bPlayPos.Enabled := False;
      bStop.Enabled := True;
      bStep.Enabled := False;
      bRecord.Enabled := False;
    end;
  end;
end;

//==============================================================================
//
// TXMICore.OnPlayerRefresh
//
//==============================================================================
procedure TXMICore.OnPlayerRefresh(var Msg: TMessage);
begin
  if Msg.WParam <> TrkCh.ItemIndex then
    Exit;
  if Length(TrackData[TrkCh.ItemIndex].Data) > 0 then
    Events.RowCount := Length(TrackData[TrkCh.ItemIndex].Data) + 1;
  Events.Row := Events.RowCount - 1;
  FillEvents(TrkCh.ItemIndex);
  ChkButtons;
end;

//==============================================================================
//
// TXMICore.OnEventRequest
//
//==============================================================================
procedure TXMICore.OnEventRequest(var Msg: TMessage);
var
  StepEvent: PStepEvent;
begin
  New(StepEvent);
  StepEvent^ := gStepEvent;
  Pointer(Pointer(Msg.WParam)^) := StepEvent;
  gStepEvent.Send := False;
end;

//==============================================================================
//
// MIDIPlayer
//
//==============================================================================
procedure MIDIPlayer(Param: PPlaySet);
const
  PROFILE_MID = 0;
  PROFILE_XMI = 1;
  PROFILE_ROL = 2;
  PROFILE_MUS = 3;
  PROFILE_IMS = 4;
  PROFILE_MDI = 5;
  PROFILE_CMF = 6;
  PROFILE_SOP = 7;
label
  play, loop, stop;
var
  PlaySet: TPlaySet;
  Ver, Division: word;
  InitTempo, Tempo: cardinal;
  SecDelay: double;
  lpFrequency, lpPerfomanceCountStart, lpPerfomanceCount: int64;
  TickCounter, TickPos, LoopStartTick: UInt64;
  LoopStartTrack: integer;
  Data: array of array of Command;
  DPos: array of integer;
  I, J: integer;
  NoEvents, LoopReq, Rhythm: boolean;
  PlayerProfile: byte;
  Notes: array[0..15] of array of record
    Note: byte;
    Tick: UInt64;
  end;
  Volumes: array[0..15] of byte;
  StepPtr: Pointer;
  StepEvent: PStepEvent;

  procedure UpdateTempo(Value: cardinal);
  begin
    if Value = Tempo then
      Exit;
    Tempo := Value;
    SecDelay := Value / Division / 1000000;
    if lpPerfomanceCount > 0 then
      lpPerfomanceCountStart :=
        lpPerfomanceCount - Round(lpFrequency * SecDelay * TickCounter);
  end;

  function PlaySysEx(B: array of byte): boolean;
  var
    Buf: array of byte;
    MIDIData: MIDIHDR;
  begin
    Result := True;
    if B[Length(B) - 1] = $F7 then
    begin
      SetLength(Buf, Length(B) + 1);
      Buf[0] := $F0;
      Move(B[0], Buf[1], Length(B));
    end
    else
    begin
      SetLength(Buf, Length(B) + 2);
      Buf[0] := $F0;
      Move(B[0], Buf[1], Length(B));
      Buf[Length(Buf) - 1] := $F7;
    end;
    FillChar(MIDIData, SizeOf(MIDIData), 0);
    MIDIData.lpData := @Buf[0];
    MIDIData.dwBufferLength := Length(Buf);
    MIDIData.dwBytesRecorded := Length(Buf);
    if midiOutPrepareHeader(MIDIOut, @MIDIData, SizeOf(MIDIData)) <>
      MMSYSERR_NOERROR then
      Result := False;
    if Result then
      if midiOutLongMsg(MIDIOut, @MIDIData, SizeOf(MIDIData)) <> MMSYSERR_NOERROR then
        Result := False;
    if Result then
      midiOutUnprepareHeader(MIDIOut, @MIDIData, SizeOf(MIDIData));
  end;

  procedure PerTickProcess();
  var
    I, J, K: integer;
    dwMsg: DWord;
  begin
    case PlayerProfile of
      PROFILE_XMI,
      PROFILE_SOP:
        for I := 0 to 15 do
        begin // Turn off needed Notes
          J := 0;
          while J < Length(Notes[I]) do
          begin
            if Notes[I][J].Tick > 0 then
            begin
              if Notes[I][J].Tick > TickCounter then
                Break;
              dwMsg := $90 or I or (Notes[I][J].Note shl 8);
              midiOutShortMsg(MIDIOut, dwMsg);
            end;
            Inc(J);
          end;
          if J > 0 then
          begin // Remove from array
            for K := J to Length(Notes[I]) - 1 do
              Notes[I][K - J] := Notes[I][K];
            SetLength(Notes[I], Length(Notes[I]) - J);
          end;
        end;
    end;
  end;

  function EventPreload(Trk: integer; E: Command): Command;
  var
    Fl: single;
  begin
    case PlayerProfile of
      PROFILE_ROL: // AdLib ROL
        if Length(E.DataArray) = 5 then
          case E.DataArray[0] of
            0: // Set Tempo
            begin
              E.Status := $FF;
              E.BParm1 := $51;
              E.Value := Round(InitTempo / PSingle(@E.DataArray[1])^);
            end;
            1: // Timbre Event
            begin
              E.Status := $C0 or (((Trk - 1) div 4) and $F);
              E.BParm1 := E.DataArray[11];
            end;
            2: // Volume Event
            begin
              Move(E.DataArray[1], Fl, 4);
              E.Status := $B0 or (((Trk - 1) div 4) and $F);
              E.BParm1 := $7;
              E.BParm2 := Round(127 * Fl);
            end;
            3: // Pitch Event
            begin
              Move(E.DataArray[1], Fl, 4);
              E.Status := $E0 or (((Trk - 1) div 4) and $F);
              E.Value := Floor(8192 * Fl);
              E.BParm1 := E.Value and $7F;
              E.BParm2 := (E.Value shr 7) and $7F;
            end;
          end;
      PROFILE_MUS, // AdLib MUS
      PROFILE_IMS: // AdLib IMS
      begin
        if E.Status shr 4 = $A then
        begin // Volume Change
          Volumes[E.Status and $F] := E.BParm1;
          E.Status := $B0 or (E.Status and $F);
          E.BParm2 := E.BParm1;
          E.BParm1 := $7;
        end;
        if E.Status shr 4 = $E then
        begin // Pitch Bend
          E.Value := ((E.Value - 8192) div 2) + 8192;
          E.BParm1 := E.Value and 127;
          E.BParm2 := (E.Value shr 7) and 127;
        end;
        if (E.Status = $F0) and (E.Len = 5) and (E.DataArray[0] = $7F) then
        begin
          // Set Speed
          E.Status := $FF;
          E.BParm1 := $51;
          Fl := E.DataArray[2] + (E.DataArray[3] / 128);
          if Fl <> 0 then
            E.Value := Round(InitTempo / Fl)
          else
            E.Value := InitTempo;
        end;
      end;
      PROFILE_MDI: // AdLib MDI
      begin
        if E.Status shr 4 = $D then
        begin // Volume Change
          Volumes[E.Status and $F] := E.BParm1;
          E.Status := $B0 or (E.Status and $F);
          E.BParm2 := E.BParm1;
          E.BParm1 := $7;
        end;
        if (E.Status = $FF) and (E.BParm1 = $7F) and
          (E.Len = 6) and (E.DataArray[0] = $00) and
          (E.DataArray[1] = $00) and (E.DataArray[2] = $3F) and
          (E.DataArray[3] = $00) and (E.DataArray[4] = $02) then
          Rhythm := E.DataArray[5] > 0;
      end;
      PROFILE_CMF: // Creative Music File
        if (E.Status shr 4 = $B) and (E.BParm1 = $67) then
          Rhythm := E.BParm2 > 0;
    end;
    if Rhythm then
      if PlayerProfile = PROFILE_CMF then
        CMF_MIDIDrum(E)
      else
        ROL_MIDIDrum(E);

    if ((PlayerProfile = PROFILE_ROL) or
      (PlayerProfile = PROFILE_MUS) or (PlayerProfile = PROFILE_IMS) or
      (PlayerProfile = PROFILE_SOP) or (PlayerProfile = PROFILE_CMF)) and
      (E.Status = $C9) then
      E.Status := $F1; // Ignore event (prevent drum bank changing)

    Result := E;
  end;

  function PlayEvent(Trk: integer; E: Command): boolean;
  var
    dwMsg: DWord;
    I: integer;
  begin
    // Set VU Meters
    if PlayerProfile = PROFILE_IMS then
    begin
      if (E.Status shr 4 = 8) or (E.Status shr 4 = 9) then
        PostMessage(XMICore.Handle, WM_SETVU, E.Status and $F, E.BParm2);
    end
    else
    if E.Status shr 4 = 9 then
      PostMessage(XMICore.Handle, WM_SETVU, E.Status and $F, E.BParm2);

    if PlayerProfile <> PROFILE_MID then
      E := EventPreload(Trk, E);
    Result := True;
    dwMsg := 0;
    if E.Status shr 4 < $F then
    begin
      case PlayerProfile of
        PROFILE_ROL: // AdLib ROL
        begin
          // Turn off all notes on channel on new note or on "Note Off"
          if E.Status shr 4 = 9 then
          begin
            for I := 0 to Length(Notes[E.Status and $F]) - 1 do
            begin
              dwMsg := E.Status or (Notes[E.Status and $F][I].Note shl 8);
              midiOutShortMsg(MIDIOut, dwMsg);
            end;
            SetLength(Notes[E.Status and $F], 0);
          end;
        end;

        PROFILE_MUS: // AdLib MUS
        begin
          case E.Status shr 4 of
            $9:
            begin
              if E.BParm2 > 0 then
              begin
                // Update Volume
                if E.BParm2 <> Volumes[E.Status and $F] then
                begin
                  Volumes[E.Status and $F] := E.BParm2;
                  dwMsg := $B0 or (E.Status and $F) or ($7 shl 8) or (E.BParm2 shl 16);
                  midiOutShortMsg(MIDIOut, dwMsg);
                end;
                E.BParm2 := $7F;
              end;
            end;
          end;
        end;

        PROFILE_IMS: // AdLib IMS
        begin
          if (E.Status shr 4 = 8) and (E.BParm2 > 0) then
          begin // Note Retrigger
            if Length(Notes[E.Status and $F]) > 0 then
            begin
              dwMsg := $80 or (E.Status and $F) or
                (Notes[E.Status and $F][0].Note shl 8);
              SetLength(Notes[E.Status and $F], 0);
              midiOutShortMsg(MIDIOut, dwMsg);
            end;
            E.Status := $90 or (E.Status and $F);
          end;
          case E.Status shr 4 of
            $9:
            begin
              if E.BParm2 > 0 then
              begin
                // Update Volume
                if E.BParm2 <> Volumes[E.Status and $F] then
                begin
                  Volumes[E.Status and $F] := E.BParm2;
                  dwMsg := $B0 or (E.Status and $F) or ($7 shl 8) or (E.BParm2 shl 16);
                  midiOutShortMsg(MIDIOut, dwMsg);
                end;
                E.BParm2 := $7F;
                SetLength(Notes[E.Status and $F], 1);
                Notes[E.Status and $F][0].Note := E.BParm1;
              end;
            end;
          end;
        end;

        PROFILE_MDI: // AdLib MDI
        begin
          case E.Status shr 4 of
            $8:
            begin
              // Update Volume
              if E.BParm2 <> Volumes[E.Status and $F] then
              begin
                Volumes[E.Status and $F] := E.BParm2;
                dwMsg := $B0 or (E.Status and $F) or ($7 shl 8) or (E.BParm2 shl 16);
                midiOutShortMsg(MIDIOut, dwMsg);
              end;
              // Turn off all notes on channel
              if E.BParm1 = 0 then
              begin
                for I := 0 to Length(Notes[E.Status and $F]) - 1 do
                begin
                  dwMsg := E.Status or (Notes[E.Status and $F][I].Note shl 8);
                  midiOutShortMsg(MIDIOut, dwMsg);
                end;
                SetLength(Notes[E.Status and $F], 0);
              end;
            end;
            $9:
            begin
              // Update Volume
              if E.BParm2 <> Volumes[E.Status and $F] then
              begin
                Volumes[E.Status and $F] := E.BParm2;
                dwMsg := $B0 or (E.Status and $F) or ($7 shl 8) or (E.BParm2 shl 16);
                midiOutShortMsg(MIDIOut, dwMsg);
              end;
              E.BParm2 := $7F;
            end;
          end;
        end;

        PROFILE_SOP: // Sopepos' Note
        begin
          case E.Status shr 4 of
            11:
            begin
              case E.BParm1 of
                9: // Change Pitch
                begin
                  E.Status := $E0 or (E.Status and $F);
                  E.Value := (E.BParm2 * 8192) div 100;
                  if E.Value > $3FFF then
                    E.Value := $3FFF;
                  E.BParm1 := E.Value and $FF;
                  E.BParm2 := E.Value shr 8;
                end;
                17: // Change Tempo
                  UpdateTempo(60000000 div E.BParm2);
              end;
            end;
          end;
        end;
      end;

      case E.Status shr 4 of
        $8, $9, $A, $B, $E:
          dwMsg := E.Status or (E.BParm1 shl 8) or (E.BParm2 shl 16);
        $C, $D:
          dwMsg := E.Status or (E.BParm1 shl 8);
      end;
      if midiOutShortMsg(MIDIOut, dwMsg) <> MMSYSERR_NOERROR then
        Result := False;

      if Result then
        case PlayerProfile of
          PROFILE_ROL: // AdLib ROL
          begin
            // Store played notes
            if E.Status shr 4 = 9 then
            begin
              if E.BParm1 > 0 then
              begin
                SetLength(Notes[E.Status and $F], Length(Notes[E.Status and $F]) + 1);
                Notes[E.Status and $F][High(Notes[E.Status and $F])].Note := E.BParm1;
              end;
            end;
          end;

          PROFILE_MDI: // AdLib MDI
          begin
            // Store played notes
            if E.Status shr 4 = 9 then
            begin
              if E.BParm1 > 0 then
              begin
                SetLength(Notes[E.Status and $F], Length(Notes[E.Status and $F]) + 1);
                Notes[E.Status and $F][High(Notes[E.Status and $F])].Note := E.BParm1;
              end;
            end;
          end;

          PROFILE_XMI, // Miles XMIDI
          PROFILE_SOP: // Sopepos' Note
          begin
            // Store Note to off it when needed
            if (E.Status shr 4 = 9) and (E.BParm2 > 0) then
            begin
              // Check whether Note is already on
              for I := 0 to Length(Notes[E.Status and $F]) - 1 do
                if Notes[E.Status and $F][I].Note = E.BParm1 then
                  Notes[E.Status and $F][I].Tick := 0; // Mark to ignore
              // Store new Note with stop time
              SetLength(Notes[E.Status and $F], Length(Notes[E.Status and $F]) + 1);
              Notes[E.Status and $F][High(Notes[E.Status and $F])].Note := E.BParm1;
              Notes[E.Status and $F][High(Notes[E.Status and $F])].Tick :=
                TickCounter + E.Len;
            end;
          end;
        end;

      // Detect loop start
      if (E.Status shr 4 = $B) and (E.BParm1 = $74) and
        (E.BParm2 = $00) then // Loop Start (XMI)
      begin
        LoopStartTick := E.Ticks;
        LoopStartTrack := Trk;
      end;
      if (E.Status shr 4 = $B) and (E.BParm1 = $76) and
        (E.BParm2 = $00) then // Loop Point Start
      begin
        LoopStartTick := E.Ticks;
        LoopStartTrack := Trk;
      end;

      // Detect loop end
      if (E.Status shr 4 = $B) and (E.BParm1 = $75) and
        (E.BParm2 = $7F) then // Loop End (XMI)
        LoopReq := True;
      if (E.Status shr 4 = $B) and (E.BParm1 = $76) and
        (E.BParm2 = $7F) then // Loop Point End
        LoopReq := True;
    end
    else
    begin
      case E.Status of
        $F0: // System Exclusive
          if Length(E.DataArray) > 0 then
            Result := PlaySysEx(E.DataArray);
        $FF: // Meta Event
          case E.BParm1 of
            $51: // Change Tempo
              if PlayerProfile <> PROFILE_XMI then
                UpdateTempo(E.Value);
            $7F: // Sequencer Specific (process the same as SysEx)
              if Length(E.DataArray) > 0 then
                Result := PlaySysEx(E.DataArray);
          end;
      end;
    end;
  end;

begin
  if not SongData_GetWord('MIDIType', Ver) then
    Ver := 1;
  if not SongData_GetDWord('InitTempo', InitTempo) then
    InitTempo := MIDIStdTempo;
  if not SongData_GetWord('Division', Division) then
    Division := 96;

  PlaySet := Param^;
  Dispose(Param);

  if Length(TrackData) = 0 then
    goto stop;

  if PlaySet.Mode < PLAY_STEP then
  begin
    SetLength(Data, Length(TrackData));
    SetLength(DPos, Length(Data));
    for I := 0 to Length(Data) - 1 do
    begin
      SetLength(Data[I], Length(TrackData[I].Data));
      for J := 0 to Length(Data[I]) - 1 do
        Data[I][J] := TrackData[I].Data[J];
      XMICore.ConvertTicks(True, Data[I]);
      DPos[I] := 0;
    end;
  end;

  Tempo := InitTempo;
  SecDelay := Tempo / Division / 1000000;
  PlayerProfile := PROFILE_MID;

  if EventProfile = 'xmi' then
    PlayerProfile := PROFILE_XMI
  else if EventProfile = 'rol' then
    PlayerProfile := PROFILE_ROL
  else if EventProfile = 'mus' then
    PlayerProfile := PROFILE_MUS
  else if EventProfile = 'ims' then
    PlayerProfile := PROFILE_IMS
  else if EventProfile = 'mdi' then
    PlayerProfile := PROFILE_MDI
  else if EventProfile = 'cmf' then
    PlayerProfile := PROFILE_CMF
  else if EventProfile = 'sop' then
    PlayerProfile := PROFILE_SOP;

  Rhythm := False;
  if PlayerProfile = PROFILE_ROL then
    if SongData_GetInt('ROL_Melodic', I) then
      Rhythm := I = 0;
  if (PlayerProfile = PROFILE_MUS) or (PlayerProfile = PROFILE_IMS) then
    if SongData_GetInt('MUS_Percussive', I) then
      Rhythm := I > 0;
  if PlayerProfile = PROFILE_SOP then
    if SongData_GetInt('SOP_Percussive', I) then
      Rhythm := I > 0;
  FillChar(Volumes, Length(Volumes), $FF);

  if not QueryPerformanceFrequency(lpFrequency) then
    goto stop; // QueryPerformanceFrequency failed

  if PlaySet.Mode = PLAY_DEF then
    I := 0
  else
    I := PlaySet.TrackIdx;
  LoopReq := False;
  LoopStartTick := 0;
  LoopStartTrack := I;
  case Ver of
    0:
    begin
      I := PlaySet.TrackIdx;
      if (I < 0) or (I >= Length(Data)) then
        I := 0;
      LoopStartTrack := I;
      PostMessage(XMICore.Handle, WM_TRACKIDX, I, 0);
    end;
    2:
      PostMessage(XMICore.Handle, WM_TRACKIDX, I, 0);
  end;
  TickCounter := 0;
  TickPos := 0;

  if PlaySet.Mode = PLAY_POS then // Seek to position
    case Ver of
      0, 2: // MIDI Type-0, Type-2
      begin
        // Preload non-note events
        if PlaySet.EventIdx > 0 then
          for J := 0 to PlaySet.EventIdx - 1 do
            if Data[PlaySet.TrackIdx][J].Status shr 4 >= $A then
              PlayEvent(PlaySet.TrackIdx, Data[PlaySet.TrackIdx][J]);
        // Set tick and event positions
        TickPos := Data[PlaySet.TrackIdx][PlaySet.EventIdx].Ticks;
        DPos[PlaySet.TrackIdx] := PlaySet.EventIdx;
      end;
      1: // MIDI Type-1
      begin
        // Preload non-note events
        TickPos := 0;
        while TickPos < Data[PlaySet.TrackIdx][PlaySet.EventIdx].Ticks do
        begin
          for J := 0 to Length(Data) - 1 do
            while (DPos[J] < Length(Data[J])) and
              (Data[J][DPos[J]].Ticks <= TickPos) do
            begin
              if Data[J][DPos[J]].Status shr 4 >= $A then
                PlayEvent(PlaySet.TrackIdx, Data[J][DPos[J]]);
              // All event positions will be correct after loop
              Inc(DPos[J]);
            end;
          Inc(TickPos);
        end;
        // Set tick position
        TickPos := Data[PlaySet.TrackIdx][PlaySet.EventIdx].Ticks;
      end;
    end;

  if not QueryPerformanceCounter(lpPerfomanceCountStart) then
    goto stop; // QueryPerformanceCounter failed
  play:
    while MIDIThrId > 0 do
    begin
      if (PlayerProfile = PROFILE_XMI) or (PlayerProfile = PROFILE_SOP) then
        PerTickProcess();
      if PlaySet.Mode = PLAY_STEP then
      begin // Play events step by step on button click
        SendMessage(XMICore.Handle, WM_EVENTREQ, wParam(@StepPtr), 0);
        StepEvent := Pointer(StepPtr);
        if StepEvent^.Send then
        begin
          if not PlayEvent(StepEvent^.TrackIdx, StepEvent^.E) then
          begin
            Dispose(StepEvent);
            goto stop;
          end;
          PostMessage(XMICore.Handle, WM_EVENTIDX, StepEvent^.EventIdx + 1,
          StepEvent^.TrackIdx);
        end;
        Dispose(StepEvent);
        Sleep(1);
        Continue;
      end;
      case Ver of
        0: // MIDI Type-0
        begin
          if DPos[I] >= Length(Data[I]) then
            Break;
          while (DPos[I] < Length(Data[I])) and
          (Data[I][DPos[I]].Ticks <= TickPos) do
          begin
            if not PlayEvent(I, Data[I][DPos[I]]) then
              goto stop;
            if (DPos[I] = 0) or (Data[I][DPos[I]].Ticks >
            Data[I][DPos[I] - 1].Ticks) then
              PostMessage(XMICore.Handle, WM_EVENTIDX, DPos[I], I);
            if LoopReq and LoopEnabled then
              goto loop;
            Inc(DPos[I]);
          end;
        end;
        1: // MIDI Type-1
        begin
          NoEvents := True;
          for I := 0 to Length(Data) - 1 do
          begin
            if DPos[I] < Length(Data[I]) then
              NoEvents := False;
            while (DPos[I] < Length(Data[I])) and
            (Data[I][DPos[I]].Ticks <= TickPos) do
            begin
              if not PlayEvent(I, Data[I][DPos[I]]) then
                goto stop;
              if (DPos[I] = 0) or (Data[I][DPos[I]].Ticks >
              Data[I][DPos[I] - 1].Ticks) then
                PostMessage(XMICore.Handle, WM_EVENTIDX, DPos[I], I);
              if LoopReq and LoopEnabled then
                goto loop;
              Inc(DPos[I]);
            end;
          end;
          I := 0;
          if NoEvents then
            Break;
        end;
        2: // MIDI Type-2
        begin
          NoEvents := False;
          while DPos[I] >= Length(Data[I]) do
          begin
            Inc(I);
            if I >= Length(Data) then
            begin
              NoEvents := True;
              Break;
            end
            else
            begin
              DPos[I] := 0;
              TickPos := 0;
              if DPos[I] < Length(Data[I]) then
                PostMessage(XMICore.Handle, WM_TRACKIDX, I, 0);
            end;
          end;
          if NoEvents then
            Break;
          while (DPos[I] < Length(Data[I])) and (Data[I][DPos[I]].Ticks <= TickPos) do
          begin
            if not PlayEvent(I, Data[I][DPos[I]]) then
              goto stop;
            if (DPos[I] = 0) or (Data[I][DPos[I]].Ticks > Data[I][DPos[I] - 1].Ticks) then
              PostMessage(XMICore.Handle, WM_EVENTIDX, DPos[I], I);
            if LoopReq and LoopEnabled then
              goto loop;
            Inc(DPos[I]);
          end;
        end;
      end;
      Inc(TickCounter);
      Inc(TickPos);
      repeat
        if not QueryPerformanceCounter(lpPerfomanceCount) then
          goto stop; // QueryPerformanceCounter failed
      until lpPerfomanceCount - lpPerfomanceCountStart >= lpFrequency *
      SecDelay * TickCounter;
    end;
  loop:
    if (MIDIThrId > 0) and (PlaySet.Mode < PLAY_STEP) and LoopEnabled then
    begin
      LoopReq := False;
      I := 0;
      case Ver of
        0:
        begin
          I := PlaySet.TrackIdx;
          J := 0;
          while (J < Length(Data[I])) and (Data[I][J].Ticks < LoopStartTick) do
            Inc(J);
          DPos[I] := J;
        end;
        1:
        begin
          for I := 0 to Length(DPos) - 1 do
          begin
            J := 0;
            while (J < Length(Data[I])) and (Data[I][J].Ticks < LoopStartTick) do
              Inc(J);
            DPos[I] := J;
          end;
          I := 0;
        end;
        2:
        begin
          I := LoopStartTrack;
          J := 0;
          while (J < Length(Data[I])) and (Data[I][J].Ticks < LoopStartTick) do
            Inc(J);
          DPos[I] := J;
          PostMessage(XMICore.Handle, WM_TRACKIDX, I, 0);
        end;
      end;
      TickPos := LoopStartTick;
      goto play;
    end;
  stop:
    midiOutClose(MIDIOut);
  MIDIThrId := 0;
  PostMessage(XMICore.Handle, WM_PLAYCTRL, PLAYER_STOP, 0);
end;

//==============================================================================
//
// midiInCallback
//
//==============================================================================
procedure midiInCallback(Handle: HMIDIIn; uMsg: uint;
  dwInstance, dwParam1, dwParam2: dword); stdcall;
var
  Pos, Tick: UInt64;
  RecCount: int64;
  hdr: PMIDIHDR;
  Wnd: HWND;
begin
  Wnd := XMICore.Handle;
  Pos := Length(TrackData[MidInTrk].Data);
  QueryPerformanceCounter(RecCount);
  if RecDelayCoef > 0 then
    Tick := Round((RecCount - RecCountStart) / RecDelayCoef)
  else
    Tick := 0;
  case uMsg of
    MIM_OPEN: ;
    MIM_CLOSE: ;
    MIM_DATA:
    begin
      with TrackData[MidInTrk] do
      begin
        SetLength(Data, Pos + 1);
        Data[Pos].Ticks := Tick - RecLastTick;
        RecLastTick := Tick;
        Data[Pos].Status := dwParam1 and $FF;
        Data[Pos].BParm1 := dwParam1 shr 8 and $FF;
        Data[Pos].BParm2 := dwParam1 shr 16 and $FF;
        if Data[Pos].Status shr 4 = 9 then
          PostMessage(Wnd, WM_SETVU, Data[Pos].Status and $F, Data[Pos].BParm2);
      end;
      if MidInEcho then
        midiOutShortMsg(MIDIOut, dwParam1);
      if GetTickCount() - UpdTicks >= MinUpdDelay then
      begin
        UpdTicks := GetTickCount();
        PostMessage(Wnd, WM_EVENTREF, MidInTrk, 0);
      end;
    end;
    MIM_LONGDATA:
    begin
      hdr := PMIDIHDR(dwParam1);
      with TrackData[MidInTrk] do
      begin // SysEx
        SetLength(Data, Pos + 1);
        Data[Pos].Ticks := Tick - RecLastTick;
        RecLastTick := Tick;
        Data[Pos].Status := $F0;
        Data[Pos].Len := hdr^.dwBytesRecorded;
        SetLength(Data[Pos].DataArray, Data[Pos].Len);
        Move(hdr^.lpData^, Data[Pos].DataArray[0], Data[Pos].Len);
      end;
      if MidInEcho then
      begin

      end;
      if GetTickCount() - UpdTicks >= MinUpdDelay then
      begin
        UpdTicks := GetTickCount();
        PostMessage(Wnd, WM_EVENTREF, MidInTrk, 0);
      end;
    end;
    MIM_ERROR: ;
    MIM_LONGERROR: ;
    MIM_MOREDATA: ;
  end;
end;

//==============================================================================
//
// TXMICore.MMSysError
//
//==============================================================================
procedure TXMICore.MMSysError(Err: DWord);
var
  Str: string;
begin
  case Err of
    MMSYSERR_NOERROR: Str := 'No error.';
    MMSYSERR_ERROR: Str := 'Unspecified error.';
    MMSYSERR_BADDEVICEID: Str := 'Device ID out of range.';
    MMSYSERR_NOTENABLED: Str := 'Driver is not enabled.';
    MMSYSERR_ALLOCATED: Str := 'Device is already allocated.';
    MMSYSERR_INVALHANDLE: Str := 'Invalid device handle.';
    MMSYSERR_NODRIVER: Str := 'Device driver not found.';
    MMSYSERR_NOMEM: Str := 'Out of memory.';
    MMSYSERR_NOTSUPPORTED: Str := 'Unsupported function.';
    MMSYSERR_BADERRNUM: Str := 'Error value out of range.';
    MMSYSERR_INVALFLAG: Str := 'Invalid flag passed.';
    MMSYSERR_INVALPARAM: Str := 'Invalid parameter passed.';
    MMSYSERR_HANDLEBUSY: Str := 'Handle is already in use.';
    MMSYSERR_INVALIDALIAS: Str := 'Specified alias not found.';
    MMSYSERR_BADDB: Str := 'Bad registry database.';
    MMSYSERR_KEYNOTFOUND: Str := 'Registry key not found.';
    MMSYSERR_READERROR: Str := 'Registry read error.';
    MMSYSERR_WRITEERROR: Str := 'Registry write error.';
    MMSYSERR_DELETEERROR: Str := 'Registry delete error.';
    MMSYSERR_VALNOTFOUND: Str := 'Registry value not found.';
    MMSYSERR_NODRIVERCB: Str := 'Driver doesn''t use callback.';
  else
    Str := 'Unknown error.';
  end;
  MessageBox(Handle, PChar(Str), 'Multimedia System Error', MB_ICONERROR or MB_OK);
end;

//==============================================================================
//
// TXMICore.bPlayClick
//
//==============================================================================
procedure TXMICore.bPlayClick(Sender: TObject);
var
  Err: DWORD;
  NewMIDIOut: THandle;
  Ver, Division: word;
  InitTempo: cardinal;
  PlaySet: PPlaySet;
begin
  if Length(TrackData) = 0 then
    Exit;
  if not SongData_GetWord('MIDIType', Ver) then
  begin
    LogOutput('[-] MIDI Type is not defined.');
    Exit;
  end;
  if not SongData_GetDWord('InitTempo', InitTempo) then
  begin
    LogOutput('[-] Initial Tempo is not defined.');
    Exit;
  end;
  if not SongData_GetWord('Division', Division) then
  begin
    LogOutput('[-] Division is not defined.');
    Exit;
  end;

  Err := midiOutOpen(@NewMIDIOut, MIDIDev, 0, 0, CALLBACK_NULL);
  if Err <> MMSYSERR_NOERROR then
  begin
    MMSysError(Err);
    Exit;
  end;
  MIDIOut := NewMIDIOut;

  PlayerMode := PLAY_DEF;
  SendMessage(Handle, WM_PLAYCTRL, PLAYER_PLAY, 0);

  New(PlaySet);
  PlaySet^.Mode := PlayerMode;
  PlaySet^.TrackIdx := TrkCh.ItemIndex;
  PlaySet^.EventIdx := Events.Row - 1;
  PlayerMode := PlaySet^.Mode;
  MIDIThr := BeginThread(nil, 0, @MIDIPlayer, PlaySet, CREATE_SUSPENDED, MIDIThrId);
  SetThreadPriority(MIDIThr, THREAD_PRIORITY_HIGHEST);
  ResumeThread(MIDIThr);
end;

//==============================================================================
//
// TXMICore.bPlayPosClick
//
//==============================================================================
procedure TXMICore.bPlayPosClick(Sender: TObject);
var
  Err: DWORD;
  NewMIDIOut: THandle;
  Ver, Division: word;
  InitTempo: cardinal;
  PlaySet: PPlaySet;
begin
  if Length(TrackData) = 0 then
    Exit;
  if not SongData_GetWord('MIDIType', Ver) then
  begin
    LogOutput('[-] MIDI Type is not defined.');
    Exit;
  end;
  if not SongData_GetDWord('InitTempo', InitTempo) then
  begin
    LogOutput('[-] Initial Tempo is not defined.');
    Exit;
  end;
  if not SongData_GetWord('Division', Division) then
  begin
    LogOutput('[-] Division is not defined.');
    Exit;
  end;

  Err := midiOutOpen(@NewMIDIOut, MIDIDev, 0, 0, CALLBACK_NULL);
  if Err <> MMSYSERR_NOERROR then
  begin
    MMSysError(Err);
    Exit;
  end;
  MIDIOut := NewMIDIOut;

  PlayerMode := PLAY_POS;
  SendMessage(Handle, WM_PLAYCTRL, PLAYER_PLAY, 0);

  New(PlaySet);
  PlaySet^.Mode := PlayerMode;
  PlaySet^.TrackIdx := TrkCh.ItemIndex;
  PlaySet^.EventIdx := Events.Row - 1;
  MIDIThr := BeginThread(nil, 0, @MIDIPlayer, PlaySet, CREATE_SUSPENDED, MIDIThrId);
  SetThreadPriority(MIDIThr, THREAD_PRIORITY_HIGHEST);
  ResumeThread(MIDIThr);
end;

//==============================================================================
//
// TXMICore.bStepClick
//
//==============================================================================
procedure TXMICore.bStepClick(Sender: TObject);
var
  Err: DWORD;
  NewMIDIOut: THandle;
  PlaySet: PPlaySet;
begin
  if (MIDIThrId > 0) and not gStepEvent.Send then
  begin
    gStepEvent.TrackIdx := TrkCh.ItemIndex;
    gStepEvent.EventIdx := Events.Row - 1;
    gStepEvent.E := TrackData[gStepEvent.TrackIdx].Data[gStepEvent.EventIdx];
    gStepEvent.Send := True;
    Exit;
  end;

  if Length(TrackData) = 0 then
    Exit;

  if not gStepEvent.Send then
  begin
    gStepEvent.TrackIdx := TrkCh.ItemIndex;
    gStepEvent.EventIdx := Events.Row - 1;
    gStepEvent.E := TrackData[gStepEvent.TrackIdx].Data[gStepEvent.EventIdx];
    gStepEvent.Send := True;
  end;

  Err := midiOutOpen(@NewMIDIOut, MIDIDev, 0, 0, CALLBACK_NULL);
  if Err <> MMSYSERR_NOERROR then
  begin
    MMSysError(Err);
    Exit;
  end;
  MIDIOut := NewMIDIOut;

  PlayerMode := PLAY_STEP;
  SendMessage(Handle, WM_PLAYCTRL, PLAYER_PLAY, 0);

  New(PlaySet);
  PlaySet^.Mode := PlayerMode;
  MIDIThr := BeginThread(nil, 0, @MIDIPlayer, PlaySet, CREATE_SUSPENDED, MIDIThrId);
  SetThreadPriority(MIDIThr, THREAD_PRIORITY_HIGHEST);
  ResumeThread(MIDIThr);
end;

//==============================================================================
//
// GetTempo
//
//==============================================================================
function GetTempo(Trk, Idx: integer): cardinal;
var
  I: integer;
begin
  if not SongData_GetDWord('InitTempo', Result) then
    Result := MIDIStdTempo;
  if (Trk < 0) or (Trk >= Length(TrackData)) then
    Exit;
  if (Idx < 0) or (Idx >= Length(TrackData[Trk].Data)) then
    Exit;
  for I := Idx downto 0 do
    if (TrackData[Trk].Data[I].Status = $FF) and
      (TrackData[Trk].Data[I].BParm1 = $51) then
    begin
      Result := TrackData[Trk].Data[I].Value;
      Break;
    end;
end;

//==============================================================================
//
// TXMICore.bRecordClick
//
//==============================================================================
procedure TXMICore.bRecordClick(Sender: TObject);
var
  Err: DWORD;
  NewMIDIOut, NewMIDIIn: THandle;
  Division: word;
  Tempo: cardinal;
  RecFrequency: int64;
begin
  if Length(TrackData) = 0 then
    Exit;

  Err := midiOutOpen(@NewMIDIOut, MIDIDev, 0, 0, CALLBACK_NULL);
  if Err <> MMSYSERR_NOERROR then
  begin
    MMSysError(Err);
    Exit;
  end;
  MIDIOut := NewMIDIOut;

  Err := midiInOpen(@NewMIDIIn, MIDIIDev, cardinal(@midiInCallback),
    0, CALLBACK_FUNCTION);
  if Err <> MMSYSERR_NOERROR then
  begin
    MMSysError(Err);
    midiOutClose(MIDIOut);
    Exit;
  end;
  MIDIIn := NewMIDIIn;
  MidInTrk := TrkCh.ItemIndex;
  if not QueryPerformanceFrequency(RecFrequency) then
  begin
    MessageBox(Handle, 'QueryPerformanceFrequency failed.', 'Error',
      MB_ICONERROR or MB_OK);
    midiInClose(MIDIIn);
    midiOutClose(MIDIOut);
    Exit;
  end;
  if not QueryPerformanceCounter(RecCountStart) then
  begin
    MessageBox(Handle, 'QueryPerformanceCounter failed.', 'Error',
      MB_ICONERROR or MB_OK);
    midiInClose(MIDIIn);
    midiOutClose(MIDIOut);
    Exit;
  end;
  if not SongData_GetWord('Division', Division) then
  begin
    MessageBox(Handle, 'Division is not defined.', 'Error', MB_ICONERROR or MB_OK);
    midiInClose(MIDIIn);
    midiOutClose(MIDIOut);
    Exit;
  end;
  Tempo := GetTempo(MidInTrk, Length(TrackData[MidInTrk].Data) - 1);
  RecDelayCoef := RecFrequency * (Tempo / Division / 1000000);
  RecLastTick := 0;

  if Length(TrackData[MidInTrk].Data) > 0 then
    with TrackData[MidInTrk] do
    begin
      if (Data[High(Data)].Status = $FF) and (Data[High(Data)].BParm1 = $2F) then
        // delete End of Track
        SetLength(Data, Length(Data) - 1);

      SetLength(Data, Length(Data) + 1);
      Data[High(Data)].Status := $FF;
      Data[High(Data)].BParm1 := $06; // Marker
      Data[High(Data)].DataString := 'Record take';
      Data[High(Data)].Len := Length(Data[High(Data)].DataString);
    end;
  UpdTicks := 0;
  PostMessage(Handle, WM_EVENTREF, MidInTrk, 0);

  PlayerMode := PLAY_REC;
  SendMessage(Handle, WM_PLAYCTRL, PLAYER_REC, 0);
  midiInStart(MIDIIn);
end;

//==============================================================================
//
// TXMICore.bStopClick
//
//==============================================================================
procedure TXMICore.bStopClick(Sender: TObject);
begin
  MIDIThrId := 0;
  if PlayerMode = PLAY_REC then
  begin
    midiInStop(MIDIIn);
    midiInClose(MIDIIn);
    midiOutClose(MIDIOut);

    with TrackData[MidInTrk] do
    begin
      SetLength(Data, Length(Data) + 1);
      Data[High(Data)].Status := $FF;
      Data[High(Data)].BParm1 := $2F;
      Data[High(Data)].Len := 0;
    end;
    PostMessage(Handle, WM_EVENTREF, MidInTrk, 0);

    PlayerMode := PLAY_DEF;
    SendMessage(Handle, WM_PLAYCTRL, PLAYER_STOP, 0);
  end;
end;

//==============================================================================
//
// TXMICore.LoadFile
//
//==============================================================================
function TXMICore.LoadFile(FileName, Fmt: string): boolean;
var
  Ext: string;
  M: TMemoryStream;
begin
  LogOutput('[*] Opening file ' + FileName + ' ...');
  Container := '';
  if Fmt = '' then
  begin
    // Detect container by extension
    Ext := LowerCase(ExtractFileExt(FileName));
    if (Ext = '.mid') or (Ext = '.midi') or (Ext = '.kar') or
      (Ext = '.mdi') then
    begin
      Container := 'smf';
      EventFormat := 'mid';
      if (Ext <> '.mdi') then
        EventProfile := 'mid'
      else
        EventProfile := 'mdi';
    end;
    if (Ext = '.rmi') or (Ext = '.rmid') or (Ext = '.orc') then
    begin
      Container := 'rmi';
      EventFormat := 'mid';
      EventProfile := 'mid';
    end;
    if (Ext = '.mds') or (Ext = '.mids') then
    begin
      Container := 'mids';
      EventFormat := 'mid';
      EventProfile := 'mid';
    end;
    if (Ext = '.xmi') then
    begin
      Container := 'xmi';
      EventFormat := 'xmi';
      EventProfile := 'xmi';
    end;
    if (Ext = '.cmf') then
    begin
      Container := 'cmf';
      EventFormat := 'mid';
      EventProfile := 'cmf';
    end;
    if (Ext = '.rol') then
    begin
      Container := 'rol';
      EventFormat := 'rol';
      EventProfile := 'rol';
    end;
    if (Ext = '.mus') then
    begin
      Container := 'mus';
      EventFormat := 'mus';
      EventProfile := 'mus';
    end;
    if (Ext = '.ims') then
    begin
      Container := 'ims';
      EventFormat := 'mus';
      EventProfile := 'ims';
    end;
    if (Ext = '.sop') then
    begin
      Container := 'sop';
      EventFormat := 'sop';
      EventProfile := 'sop';
    end;
    if (Ext = '.hsq') or (Ext = '.sqx') or (Ext = '.sdb') or
      (Ext = '.agd') or (Ext = '.m32') or (Ext = '.ha2') then
    begin
      Container := 'herad';
      EventFormat := 'herad';
      EventProfile := 'herad';
    end;
    if (Ext = '.raw') then
    begin
      Container := 'raw';
      EventFormat := 'mid';
      EventProfile := 'mid';
    end;
    if (Ext = '.syx') then
    begin
      Container := 'syx';
      EventFormat := 'mid';
      EventProfile := 'mid';
    end;
  end
  else
  begin
    // Container specified
    if Fmt = 'smf' then
    begin
      Container := 'smf';
      EventFormat := 'mid';
      EventProfile := 'mid';
    end;
    if Fmt = 'rmi' then
    begin
      Container := 'rmi';
      EventFormat := 'mid';
      EventProfile := 'mid';
    end;
    if Fmt = 'mids' then
    begin
      Container := 'mids';
      EventFormat := 'mid';
      EventProfile := 'mid';
    end;
    if Fmt = 'xmi' then
    begin
      Container := 'xmi';
      EventFormat := 'xmi';
      EventProfile := 'xmi';
    end;
    if Fmt = 'cmf' then
    begin
      Container := 'cmf';
      EventFormat := 'mid';
      EventProfile := 'cmf';
    end;
    if Fmt = 'mdi' then
    begin
      Container := 'smf';
      EventFormat := 'mid';
      EventProfile := 'mdi';
    end;
    if Fmt = 'rol' then
    begin
      Container := 'rol';
      EventFormat := 'rol';
      EventProfile := 'rol';
    end;
    if Fmt = 'mus' then
    begin
      Container := 'mus';
      EventFormat := 'mus';
      EventProfile := 'mus';
    end;
    if Fmt = 'ims' then
    begin
      Container := 'ims';
      EventFormat := 'mus';
      EventProfile := 'ims';
    end;
    if Fmt = 'sop' then
    begin
      Container := 'sop';
      EventFormat := 'sop';
      EventProfile := 'sop';
    end;
    if Fmt = 'herad' then
    begin
      Container := 'herad';
      EventFormat := 'herad';
      EventProfile := 'herad';
    end;
    if Fmt = 'raw' then
    begin
      Container := 'raw';
      EventFormat := 'mid';
      EventProfile := 'mid';
    end;
    if Fmt = 'syx' then
    begin
      Container := 'syx';
      EventFormat := 'mid';
      EventProfile := 'mid';
    end;
  end;

  M := TMemoryStream.Create;
  try
    M.LoadFromFile(FileName);
  except
    M.Free;
    Result := False;
    LogOutput('[-] Load file failed.');
    Exit;
  end;
  if Container = '' then
  begin
    // Detect container by data
    Container := DetectFile(M);
  end;
  EventViewProfile := EventProfile;

  if Container <> '' then
  begin
    SongData.Strings.Clear;
    if Container = 'smf' then
    begin
      Opened := ReadMIDI(M);
    end;
    if Container = 'rmi' then
    begin
      Opened := ReadRMI(M);
    end;
    if Container = 'mids' then
    begin
      Opened := ReadMIDS(M);
    end;
    if Container = 'xmi' then
    begin
      Opened := ReadXMI(M);
    end;
    if Container = 'cmf' then
    begin
      Opened := ReadCMF(M);
    end;
    if Container = 'rol' then
    begin
      Opened := ReadROL(M, FileName);
    end;
    if Container = 'mus' then
    begin
      Opened := ReadMUS(M, FileName);
    end;
    if Container = 'ims' then
    begin
      Opened := ReadMUS(M, FileName);
    end;
    if Container = 'sop' then
    begin
      Opened := ReadSOP(M);
    end;
    if Container = 'herad' then
    begin
      Opened := ReadHERAD(M);
    end;
    if Container = 'raw' then
    begin
      Opened := ReadRaw(M);
    end;
    if Container = 'syx' then
    begin
      Opened := ReadSYX(M);
    end;
    if not Opened then
      SongData.Strings.Clear;
  end
  else
    LogOutput('[-] Unknown file format.');
  M.Free;
  Result := Opened;
end;

//==============================================================================
//
// TXMICore.LoadMemory
//
//==============================================================================
function TXMICore.LoadMemory(const adata: Pointer; const asize: integer; Fmt: string): boolean;
var
  M: TMemoryStream;
begin
  LogOutput('[*] Opening memory ...');
  Container := '';
  if Fmt = '' then
  begin
    if DetectXMIMemory(adata, asize) then
      Fmt := 'xmi'
    else if DetectCMFMemory(adata, asize) then
      Fmt := 'cmf'
    else if DetectSOPMemory(adata, asize) then
      Fmt := 'sop';
  end;

  // Container specified
  if Fmt = 'smf' then
  begin
    Container := 'smf';
    EventFormat := 'mid';
    EventProfile := 'mid';
  end
  else if Fmt = 'rmi' then
  begin
    Container := 'rmi';
    EventFormat := 'mid';
    EventProfile := 'mid';
  end
  else if Fmt = 'mids' then
  begin
    Container := 'mids';
    EventFormat := 'mid';
    EventProfile := 'mid';
  end
  else if Fmt = 'xmi' then
  begin
    Container := 'xmi';
    EventFormat := 'xmi';
    EventProfile := 'xmi';
  end
  else if Fmt = 'cmf' then
  begin
    Container := 'cmf';
    EventFormat := 'mid';
    EventProfile := 'cmf';
  end
  else if Fmt = 'mdi' then
  begin
    Container := 'smf';
    EventFormat := 'mid';
    EventProfile := 'mdi';
  end
  else if Fmt = 'rol' then
  begin
    Container := 'rol';
    EventFormat := 'rol';
    EventProfile := 'rol';
  end
  else if Fmt = 'mus' then
  begin
    Container := 'mus';
    EventFormat := 'mus';
    EventProfile := 'mus';
  end
  else if Fmt = 'ims' then
  begin
    Container := 'ims';
    EventFormat := 'mus';
    EventProfile := 'ims';
  end
  else if Fmt = 'sop' then
  begin
    Container := 'sop';
    EventFormat := 'sop';
    EventProfile := 'sop';
  end
  else if Fmt = 'herad' then
  begin
    Container := 'herad';
    EventFormat := 'herad';
    EventProfile := 'herad';
  end
  else if Fmt = 'raw' then
  begin
    Container := 'raw';
    EventFormat := 'mid';
    EventProfile := 'mid';
  end
  else if Fmt = 'syx' then
  begin
    Container := 'syx';
    EventFormat := 'mid';
    EventProfile := 'mid';
  end;

  M := TMemoryStream.Create;
  try
    M.Write(adata^, asize);
  except
    M.Free;
    Result := False;
    LogOutput('[-] Load memory failed.');
    Exit;
  end;
  if Container = '' then
  begin
    // Detect container by data
    Container := DetectFile(M);
  end;
  EventViewProfile := EventProfile;

  if Container <> '' then
  begin
    SongData.Strings.Clear;
    if Container = 'smf' then
    begin
      Opened := ReadMIDI(M);
    end;
    if Container = 'rmi' then
    begin
      Opened := ReadRMI(M);
    end;
    if Container = 'mids' then
    begin
      Opened := ReadMIDS(M);
    end;
    if Container = 'xmi' then
    begin
      Opened := ReadXMI(M);
    end;
    if Container = 'cmf' then
    begin
      Opened := ReadCMF(M);
    end;
    if Container = 'sop' then
    begin
      Opened := ReadSOP(M);
    end;
    if Container = 'herad' then
    begin
      Opened := ReadHERAD(M);
    end;
    if Container = 'raw' then
    begin
      Opened := ReadRaw(M);
    end;
    if Container = 'syx' then
    begin
      Opened := ReadSYX(M);
    end;
    if not Opened then
      SongData.Strings.Clear;
  end
  else
    LogOutput('[-] Unknown file format.');
  M.Free;
  Result := Opened;
end;

//==============================================================================
//
// TXMICore.SaveFileToStream
//
//==============================================================================
function TXMICore.SaveFileToStream(FileName: string; M: TMemoryStream): boolean;
var
  Ext: string;
  TargetContainer, TargetEventFormat, TargetEventProfile: ansistring;
begin
  Result := False;
  LogOutput('[*] Saving to ' + FileName + '...');
  Ext := LowerCase(ExtractFileExt(FileName));
  TargetContainer := '';
  TargetEventFormat := '';
  TargetEventProfile := '';
  if (Ext = '.mid') or (Ext = '.midi') or (Ext = '.kar') then
  begin
    TargetContainer := 'smf';
    TargetEventFormat := 'mid';
    TargetEventProfile := 'mid';
  end;
  if (Ext = '.rmi') or (Ext = '.rmid') then
  begin
    TargetContainer := 'rmi';
    TargetEventFormat := 'mid';
    TargetEventProfile := 'mid';
  end;
  if (Ext = '.mdi') then
  begin
    TargetContainer := 'smf';
    TargetEventFormat := 'mid';
    TargetEventProfile := 'mdi';
  end;
  if (Ext = '.xmi') then
  begin
    TargetContainer := 'xmi';
    TargetEventFormat := 'xmi';
    TargetEventProfile := 'xmi';
  end;
  if (Ext = '.rol') then
  begin
    TargetContainer := 'rol';
    TargetEventFormat := 'rol';
    TargetEventProfile := 'rol';
  end;
  if (Ext = '.mus') then
  begin
    TargetContainer := 'mus';
    TargetEventFormat := 'mus';
    TargetEventProfile := 'mus';
  end;
  if (Ext = '.ims') then
  begin
    TargetContainer := 'ims';
    TargetEventFormat := 'mus';
    TargetEventProfile := 'ims';
  end;
  if (Ext = '.cmf') then
  begin
    TargetContainer := 'cmf';
    TargetEventFormat := 'mid';
    TargetEventProfile := 'cmf';
  end;
  if (Ext = '.sop') then
  begin
    TargetContainer := 'sop';
    TargetEventFormat := 'sop';
    TargetEventProfile := 'sop';
  end;
  if (Ext = '.sdb') or (Ext = '.agd') or (Ext = '.m32') then
  begin
    TargetContainer := 'herad';
    TargetEventFormat := 'herad';
    TargetEventProfile := 'herad';
  end;
  if (Ext = '.ha2') then
  begin
    TargetContainer := 'herad';
    TargetEventFormat := 'herad';
    TargetEventProfile := 'herad_v2';
  end;
  if (Ext = '.raw') then
  begin
    TargetContainer := 'raw';
    TargetEventFormat := 'mid';
    TargetEventProfile := 'mid';
  end;
  if (Ext = '.syx') then
  begin
    TargetContainer := 'syx';
    TargetEventFormat := 'mid';
    TargetEventProfile := 'mid';
  end;
  if TargetContainer = '' then
    TargetContainer := Container;
  if TargetEventFormat = '' then
    TargetEventFormat := EventFormat;
  if TargetEventProfile = '' then
    TargetEventProfile := EventProfile;

  if TargetContainer = 'smf' then
  begin
    if TargetEventProfile = 'mid' then
      ConvertEvents('mid');
    if TargetEventProfile = 'mdi' then
      ConvertEvents('mdi');
    WriteMIDI(M);
    Result := True;
  end;
  if TargetContainer = 'rmi' then
  begin
    if TargetEventProfile = 'mid' then
      ConvertEvents('mid');
    WriteRMI(M);
    Result := True;
  end;
  if TargetContainer = 'xmi' then
  begin
    if TargetEventProfile = 'xmi' then
      ConvertEvents('xmi');
    WriteXMI(M);
    Result := True;
  end;
  if TargetContainer = 'cmf' then
  begin
    if TargetEventProfile = 'cmf' then
      ConvertEvents('cmf');
    WriteCMF(M);
    Result := True;
  end;
  if TargetContainer = 'rol' then
  begin
    if TargetEventProfile = 'rol' then
      ConvertEvents('rol');
    WriteROL(M, FileName);
    Result := True;
  end;
  if TargetContainer = 'mus' then
  begin
    if TargetEventProfile = 'mus' then
      ConvertEvents('mus');
    WriteMUS(M, FileName);
    Result := True;
  end;
  if TargetContainer = 'ims' then
  begin
    if TargetEventFormat = 'mus' then
      EventFormat := 'mus';
    if TargetEventProfile = 'ims' then
      ConvertEvents('ims');
    WriteMUS(M, FileName);
    Result := True;
  end;
  if TargetContainer = 'sop' then
  begin
    if TargetEventFormat = 'sop' then
      EventFormat := 'sop';
    if TargetEventProfile = 'sop' then
      ConvertEvents('sop');
    WriteSOP(M);
    Result := True;
  end;
  if TargetContainer = 'herad' then
  begin
    if TargetEventFormat = 'herad' then
      EventFormat := 'herad';
    if TargetEventProfile = 'herad' then
      ConvertEvents('herad');
    if TargetEventProfile = 'herad_v2' then
      ConvertEvents('herad_v2');
    WriteHERAD(M);
    Result := True;
  end;
  if TargetContainer = 'raw' then
  begin
    if TargetEventFormat = 'mid' then
      EventFormat := 'mid';
    if TargetEventProfile = 'mid' then
      ConvertEvents('mid');
    WriteRaw(M);
    Result := True;
  end;
  if TargetContainer = 'syx' then
  begin
    WriteSYX(M);
    Result := True;
  end;
end;

//==============================================================================
//
// TXMICore.SaveFile
//
//==============================================================================
function TXMICore.SaveFile(FileName: string): boolean;
var
  Idx: integer;
  M: TMemoryStream;
begin
  Idx := TrkCh.ItemIndex;

  M := TMemoryStream.Create;

  Result := SaveFileToStream(FileName, M);

  try
    if Result then
      M.SaveToFile(FileName);
  except
    Result := False;
  end;
  if Result then
    LogOutput('[+] Saved.')
  else
    LogOutput('[-] Save failed.');

  M.Free;

  RefTrackList;
  TrkCh.ItemIndex := Idx;
  if (TrkCh.Items.Count > 0) and (TrkCh.ItemIndex = -1) then
    TrkCh.ItemIndex := 0;
  FillEvents(TrkCh.ItemIndex);
  ChkButtons;
end;

//==============================================================================
//
// TXMICore.SaveFileToPointer
//
//==============================================================================
function TXMICore.SaveFileToPointer(FileName: string; var p: pointer; var sz: integer): boolean;
var
  Idx: integer;
  M: TMemoryStream;
begin
  Idx := TrkCh.ItemIndex;

  M := TMemoryStream.Create;

  Result := SaveFileToStream(FileName, M);

  try
    if Result then
    begin
      sz := M.Size;
      M.Position := 0;
      GetMem(p, sz);
      M.Read(p^, sz);
    end;
  except
    Result := False;
  end;
  if Result then
    LogOutput('[+] Saved.')
  else
    LogOutput('[-] Save failed.');

  M.Free;

  RefTrackList;
  TrkCh.ItemIndex := Idx;
  if (TrkCh.Items.Count > 0) and (TrkCh.ItemIndex = -1) then
    TrkCh.ItemIndex := 0;
  FillEvents(TrkCh.ItemIndex);
  ChkButtons;
end;

//==============================================================================
//
// TXMICore.DetectFile
//
//==============================================================================
function TXMICore.DetectFile(var F: TMemoryStream): string;
begin
  Result := '';
  if DetectMIDI(F) then
  begin
    Result := 'smf';
    EventFormat := 'mid';
    EventProfile := 'mid';
    Exit;
  end;
  if DetectRMI(F) then
  begin
    Result := 'rmi';
    EventFormat := 'mid';
    EventProfile := 'mid';
    Exit;
  end;
  if DetectMIDS(F) then
  begin
    Result := 'mids';
    EventFormat := 'mid';
    EventProfile := 'mid';
    Exit;
  end;
  if DetectXMI(F) then
  begin
    Result := 'xmi';
    EventFormat := 'xmi';
    EventProfile := 'xmi';
    Exit;
  end;
  if DetectCMF(F) then
  begin
    Result := 'cmf';
    EventFormat := 'mid';
    EventProfile := 'cmf';
    Exit;
  end;
  if DetectROL(F) then
  begin
    Result := 'rol';
    EventFormat := 'rol';
    EventProfile := 'rol';
    Exit;
  end;
  if DetectMUS(F) then
  begin
    Result := 'mus';
    EventFormat := 'mus';
    EventProfile := 'mus';
    Exit;
  end;
  if DetectSOP(F) then
  begin
    Result := 'sop';
    EventFormat := 'sop';
    EventProfile := 'sop';
    Exit;
  end;
end;

//==============================================================================
//
// TXMICore.FormCreate
//
//==============================================================================
procedure TXMICore.FormCreate(Sender: TObject);
begin
  SongData := TValueListEditor.Create(nil);

  FillChar(vFS, SizeOf(vFS), 0);
  vFS.DecimalSeparator := '.';

  SearchEv.dtOp := 0;
  SearchEv.dt := 0;
  SearchEv.chan := $FF;
  SearchEv.evnt := 0;
  SearchEv.v1Op := 1;
  SearchEv.v1 := 48;
  SearchEv.v2Op := 1;
  SearchEv.v2 := 127;
  SearchEv.Text := '';

  ChkButtons;
end;

//==============================================================================
//
// TXMICore.FormDestroy
//
//==============================================================================
procedure TXMICore.FormDestroy(Sender: TObject);
begin
  SongData.Free;
end;

//==============================================================================
//
// TXMICore.FormResize
//
//==============================================================================
procedure TXMICore.FormResize(Sender: TObject);
begin
  if Events.VisibleRowCount <> VisRows then
  begin
    if VisRows < Events.VisibleRowCount then
      FillEvents(TrkCh.ItemIndex);
    VisRows := Events.VisibleRowCount;
  end;
end;

//==============================================================================
//
// TXMICore.TrkChChange
//
//==============================================================================
procedure TXMICore.TrkChChange(Sender: TObject);
begin
  FillEvents(TrkCh.ItemIndex);
  ChkButtons;
end;

//==============================================================================
//
// TXMICore.EventsClick
//
//==============================================================================
procedure TXMICore.EventsClick(Sender: TObject);
begin
  ChkButtons;
end;

//==============================================================================
//
// TXMICore.EventsKeyPress
//
//==============================================================================
procedure TXMICore.EventsKeyPress(Sender: TObject; var Key: char);
begin
  ChkButtons;
end;

//==============================================================================
//
// TXMICore.MFindNextClick
//
//==============================================================================
procedure TXMICore.MFindNextClick(Sender: TObject);
var
  I, J: integer;
  Ev: Command;
  Found: boolean;
begin
  if SearchEv.evnt < 8 then
    Exit;
  I := TrkCh.ItemIndex;
  while I < Length(TrackData) do
  begin
    if I = TrkCh.ItemIndex then
      J := Events.Row // search from next event
    else
      J := 0;
    while J < Length(TrackData[I].Data) do
    begin
      Ev := TrackData[I].Data[J];
      // check event
      Found := Ev.Status shr 4 = SearchEv.evnt;
      // check delta time
      if Found and (SearchEv.dtOp > 0) then
        case SearchEv.dtOp of
          1: // equal
            Found := Ev.Ticks = SearchEv.dt;
          2: // not equal
            Found := Ev.Ticks <> SearchEv.dt;
          3: // greater
            Found := Ev.Ticks > SearchEv.dt;
          4: // less
            Found := Ev.Ticks < SearchEv.dt;
          5: // greater or equal
            Found := Ev.Ticks >= SearchEv.dt;
          6: // less or equal
            Found := Ev.Ticks <= SearchEv.dt;
        end;
      // check channel
      if Found and (SearchEv.evnt < $F) and (SearchEv.chan <= $F) then
        Found := Ev.Status and $F = SearchEv.chan;
      // check values
      if Found and (SearchEv.evnt in [$8, $9, $A, $B, $C, $D]) and
        (SearchEv.v1Op > 0) then
        case SearchEv.v1Op of
          1: // equal
            Found := Ev.BParm1 = SearchEv.v1;
          2: // not equal
            Found := Ev.BParm1 <> SearchEv.v1;
          3: // greater
            Found := Ev.BParm1 > SearchEv.v1;
          4: // less
            Found := Ev.BParm1 < SearchEv.v1;
          5: // greater or equal
            Found := Ev.BParm1 >= SearchEv.v1;
          6: // less or equal
            Found := Ev.BParm1 <= SearchEv.v1;
        end;
      if Found and (SearchEv.evnt in [$8, $9, $A, $B]) and (SearchEv.v2Op > 0) then
        case SearchEv.v2Op of
          1: // equal
            Found := Ev.BParm2 = SearchEv.v2;
          2: // not equal
            Found := Ev.BParm2 <> SearchEv.v2;
          3: // greater
            Found := Ev.BParm2 > SearchEv.v2;
          4: // less
            Found := Ev.BParm2 < SearchEv.v2;
          5: // greater or equal
            Found := Ev.BParm2 >= SearchEv.v2;
          6: // less or equal
            Found := Ev.BParm2 <= SearchEv.v2;
        end;
      if Found and (SearchEv.evnt = $E) and (SearchEv.v1Op > 0) then
        case SearchEv.v1Op of
          1: // equal
            Found := Ev.Value = SearchEv.v1;
          2: // not equal
            Found := Ev.Value <> SearchEv.v1;
          3: // greater
            Found := Ev.Value > SearchEv.v1;
          4: // less
            Found := Ev.Value < SearchEv.v1;
          5: // greater or equal
            Found := Ev.Value >= SearchEv.v1;
          6: // less or equal
            Found := Ev.Value <= SearchEv.v1;
        end;
      if Found and (SearchEv.evnt = $F) then
      begin
        Found := Ev.Status and $F = SearchEv.v1;
        if Found and (SearchEv.v1 in [2, 3]) and (SearchEv.v2Op > 0) then
          case SearchEv.v2Op of
            1: // equal
              Found := Ev.BParm1 = SearchEv.v2;
            2: // not equal
              Found := Ev.BParm1 <> SearchEv.v2;
            3: // greater
              Found := Ev.BParm1 > SearchEv.v2;
            4: // less
              Found := Ev.BParm1 < SearchEv.v2;
            5: // greater or equal
              Found := Ev.BParm1 >= SearchEv.v2;
            6: // less or equal
              Found := Ev.BParm1 <= SearchEv.v2;
          end;
        if Found and (SearchEv.v1 = $F) then
        begin
          Found := Ev.BParm1 = SearchEv.v2;
          if Found and (SearchEv.v2 in [1..7]) and
            (SearchEv.Text <> '') then
            Found := Pos(SearchEv.Text, Ev.DataString) > 0;
        end;
      end;

      if Found then
      begin
        TrkCh.ItemIndex := I;
        TrkChChange(Sender);
        Events.Row := J + 1;
        Exit;
      end;
      Inc(J);
    end;
    Inc(I);
  end;
  LogOutput('[*] Search complete.');
end;

//==============================================================================
//
// TXMICore.MFormatMIDClick
//
//==============================================================================
procedure TXMICore.MFormatMIDClick(Sender: TObject);
var
  Idx: integer;
begin
  Idx := TrkCh.ItemIndex;
  ConvertEvents('mid');
  RefTrackList;
  TrkCh.ItemIndex := Idx;
  FillEvents(TrkCh.ItemIndex);
  ChkButtons;
end;

//==============================================================================
//
// TXMICore.MFormatMDIClick
//
//==============================================================================
procedure TXMICore.MFormatMDIClick(Sender: TObject);
var
  Idx: integer;
begin
  Idx := TrkCh.ItemIndex;
  ConvertEvents('mdi');
  RefTrackList;
  TrkCh.ItemIndex := Idx;
  FillEvents(TrkCh.ItemIndex);
  ChkButtons;
end;

//==============================================================================
//
// TXMICore.MFormatCMFClick
//
//==============================================================================
procedure TXMICore.MFormatCMFClick(Sender: TObject);
var
  Idx: integer;
begin
  Idx := TrkCh.ItemIndex;
  ConvertEvents('cmf');
  RefTrackList;
  TrkCh.ItemIndex := Idx;
  FillEvents(TrkCh.ItemIndex);
  ChkButtons;
end;

//==============================================================================
//
// TXMICore.MFormatXMIClick
//
//==============================================================================
procedure TXMICore.MFormatXMIClick(Sender: TObject);
var
  Idx: integer;
begin
  Idx := TrkCh.ItemIndex;
  ConvertEvents('xmi');
  RefTrackList;
  TrkCh.ItemIndex := Idx;
  FillEvents(TrkCh.ItemIndex);
  ChkButtons;
end;

//==============================================================================
//
// TXMICore.MFormatROLClick
//
//==============================================================================
procedure TXMICore.MFormatROLClick(Sender: TObject);
var
  Idx: integer;
begin
  Idx := TrkCh.ItemIndex;
  ConvertEvents('rol');
  RefTrackList;
  TrkCh.ItemIndex := Idx;
  FillEvents(TrkCh.ItemIndex);
  ChkButtons;
end;

//==============================================================================
//
// TXMICore.MFormatMUSClick
//
//==============================================================================
procedure TXMICore.MFormatMUSClick(Sender: TObject);
var
  Idx: integer;
begin
  Idx := TrkCh.ItemIndex;
  ConvertEvents('mus');
  RefTrackList;
  TrkCh.ItemIndex := Idx;
  FillEvents(TrkCh.ItemIndex);
  ChkButtons;
end;

//==============================================================================
//
// TXMICore.MFormatIMSClick
//
//==============================================================================
procedure TXMICore.MFormatIMSClick(Sender: TObject);
var
  Idx: integer;
begin
  Idx := TrkCh.ItemIndex;
  ConvertEvents('ims');
  RefTrackList;
  TrkCh.ItemIndex := Idx;
  FillEvents(TrkCh.ItemIndex);
  ChkButtons;
end;

//==============================================================================
//
// TXMICore.MFormatSOPClick
//
//==============================================================================
procedure TXMICore.MFormatSOPClick(Sender: TObject);
var
  Idx: integer;
begin
  Idx := TrkCh.ItemIndex;
  ConvertEvents('sop');
  RefTrackList;
  TrkCh.ItemIndex := Idx;
  FillEvents(TrkCh.ItemIndex);
  ChkButtons;
end;

//==============================================================================
//
// TXMICore.MFormatHERADClick
//
//==============================================================================
procedure TXMICore.MFormatHERADClick(Sender: TObject);
var
  Idx: integer;
begin
  Idx := TrkCh.ItemIndex;
  ConvertEvents('herad');
  RefTrackList;
  TrkCh.ItemIndex := Idx;
  FillEvents(TrkCh.ItemIndex);
  ChkButtons;
end;

//==============================================================================
//
// TXMICore.GetInstType
//
//==============================================================================
function TXMICore.GetInstType(Chn: byte): string;
begin
  if Chn = 9 then
    Result := 'Drum Kit'
  else
    Result := 'Instrument';
  if (EventViewProfile = 'mus') or (EventViewProfile = 'ims') or
    (EventViewProfile = 'mdi') or (EventViewProfile = 'sop') or
    (EventViewProfile = 'cmf') then
    Result := 'Instrument';
  if (EventViewProfile = 'herad') then
    if SongData_GetStr('HERAD_Inst#0', Result) then // is not M32
      Result := 'Instrument';
end;

//==============================================================================
//
// TXMICore.GetInstName
//
//==============================================================================
function TXMICore.GetInstName(Prog: byte; Chn: byte): string;
begin
  if Chn = 9 then
    Result := DrumKits[Prog]
  else
    Result := InstrumentTable[Prog];
  if EventViewProfile = 'mus' then
  begin
    Result := '';
    SongData_GetStr('SND_Name#' + IntToStr(Prog), Result);
  end;
  if EventViewProfile = 'ims' then
  begin
    Result := '';
    SongData_GetStr('IMS_Name#' + IntToStr(Prog), Result);
  end;
  if (EventViewProfile = 'mdi') or (EventViewProfile = 'cmf') then
    Result := '';
  if EventViewProfile = 'sop' then
  begin
    Result := '';
    SongData_GetStr('SOP_LName#' + IntToStr(Prog), Result);
    if Result = '' then
      SongData_GetStr('SOP_SName#' + IntToStr(Prog), Result);
  end;
  if EventViewProfile = 'herad' then
    if SongData_GetStr('HERAD_Inst#0', Result) then
      Result := '';
end;

//==============================================================================
//
// TXMICore.FillEvents
//
//==============================================================================
procedure TXMICore.FillEvents(Idx: integer);
var
  I, J, Cnt: integer;
  S: string;
  Speed: double;
  Fl: single;
  Rhythm, HERAD_V2: boolean;
  InitTempo: cardinal;
begin
  if (Idx < 0) or (Idx >= Length(TrackData)) then
    Exit;
  if Length(TrackData[Idx].Data) = 0 then
  begin
    Events.ColCount := 1;
    Events.RowCount := 1;
    Events.DefaultColWidth := 64;
    Events.Cells[0, 0] := 'No events';
    Exit;
  end;
  Events.ColCount := 5;
  Events.RowCount := Length(TrackData[Idx].Data) + 1;
  Events.FixedCols := 1;
  Events.FixedRows := 1;
  Events.DefaultColWidth := 64;
  Events.Cells[0, 0] := '#';
  Events.Cells[1, 0] := 'Delta-time';
  Events.Cells[2, 0] := 'Chn';
  Events.Cells[3, 0] := 'Event';
  Events.Cells[4, 0] := 'Data';

  if not SongData_GetDWord('InitTempo', InitTempo) then
  begin
    LogOutput('[-] Initial Tempo is not defined.');
    Exit;
  end;

  Rhythm := False;
  if EventViewProfile = 'rol' then
    if SongData_GetInt('ROL_Melodic', I) then
      Rhythm := I = 0;
  if (EventViewProfile = 'mus') or (EventViewProfile = 'ims') then
    if SongData_GetInt('MUS_Percussive', I) then
      Rhythm := I > 0;
  if EventViewProfile = 'sop' then
    if SongData_GetInt('SOP_Percussive', I) then
      Rhythm := I > 0;

  // Pre-process events to get current state
  for I := 0 to Length(TrackData[Idx].Data) - 1 do
  begin
    if I = Events.TopRow - 1 then
      Break;
    if EventViewProfile = 'mdi' then
    begin
      if (TrackData[Idx].Data[I].Status = $F0) or
        ((TrackData[Idx].Data[I].Status = $FF) and
        (TrackData[Idx].Data[I].BParm1 = $7F)) then
        if (TrackData[Idx].Data[I].Len = 6) and
          (TrackData[Idx].Data[I].DataArray[0] = $00) and
          (TrackData[Idx].Data[I].DataArray[1] = $00) and
          (TrackData[Idx].Data[I].DataArray[2] = $3F) and
          (TrackData[Idx].Data[I].DataArray[3] = $00) and
          (TrackData[Idx].Data[I].DataArray[4] = $02) then
        begin
          if TrackData[Idx].Data[I].DataArray[5] = 0 then
            Rhythm := False;
          if TrackData[Idx].Data[I].DataArray[5] = 1 then
            Rhythm := True;
        end;
    end;

    if EventViewProfile = 'cmf' then
    begin
      if (TrackData[Idx].Data[I].Status shr 4 = $B) and
        (TrackData[Idx].Data[I].BParm1 = 103) then
        Rhythm := TrackData[Idx].Data[I].BParm2 > 0;
    end;

  end;
  // Fill displayed events
  Cnt := 0;
  for I := Events.TopRow - 1 to Length(TrackData[Idx].Data) - 1 do
  begin
    if Cnt > Events.VisibleRowCount then
      Break;
    Events.Cells[0, I + 1] := IntToStr(I + 1);
    Events.Cells[1, I + 1] := IntToStr(TrackData[Idx].Data[I].Ticks);
    Events.Cells[2, I + 1] := '';
    Events.Cells[4, I + 1] := '';
    if (TrackData[Idx].Data[I].Status shr 4) < 15 then
      Events.Cells[2, I + 1] := IntToStr(TrackData[Idx].Data[I].Status and 15 + 1);

    case TrackData[Idx].Data[I].Status shr 4 of
      8:
      begin // NoteOff
        Events.Cells[3, I + 1] := 'Note Off';
        if (TrackData[Idx].Data[I].Status and 15) <> 9 then
          Events.Cells[4, I + 1] := 'Note = ' + IntToStr(TrackData[Idx].Data[I].BParm1) +
            ' (' + NoteNum(TrackData[Idx].Data[I].BParm1) +
            '), velocity = ' + IntToStr(TrackData[Idx].Data[I].BParm2)
        else
          Events.Cells[4, I + 1] := 'Note = ' + IntToStr(TrackData[Idx].Data[I].BParm1) +
            ' (' + DrumTable[TrackData[Idx].Data[I].BParm1] +
            '), velocity = ' + IntToStr(TrackData[Idx].Data[I].BParm2);
      end;
      9:
      begin // NoteOn
        if TrackData[Idx].Data[I].BParm2 > 0 then
          Events.Cells[3, I + 1] := 'Note On'
        else
          Events.Cells[3, I + 1] := 'Note Off';
        if (TrackData[Idx].Data[I].Status and 15) <> 9 then
          Events.Cells[4, I + 1] := 'Note = ' + IntToStr(TrackData[Idx].Data[I].BParm1) +
            ' (' + NoteNum(TrackData[Idx].Data[I].BParm1) +
            '), velocity = ' + IntToStr(TrackData[Idx].Data[I].BParm2)
        else
          Events.Cells[4, I + 1] := 'Note = ' + IntToStr(TrackData[Idx].Data[I].BParm1) +
            ' (' + DrumTable[TrackData[Idx].Data[I].BParm1] +
            '), velocity = ' + IntToStr(TrackData[Idx].Data[I].BParm2);
      end;
      10:
      begin // Polyphonic Aftertouch
        Events.Cells[3, I + 1] := 'Polyphonic Aftertouch';
        if (TrackData[Idx].Data[I].Status and 15) <> 9 then
          Events.Cells[4, I + 1] := 'Note = ' + IntToStr(TrackData[Idx].Data[I].BParm1) +
            ' (' + NoteNum(TrackData[Idx].Data[I].BParm1) +
            '), pressure = ' + IntToStr(TrackData[Idx].Data[I].BParm2)
        else
          Events.Cells[4, I + 1] := 'Note = ' + IntToStr(TrackData[Idx].Data[I].BParm1) +
            ' (' + DrumTable[TrackData[Idx].Data[I].BParm1] +
            '), pressure = ' + IntToStr(TrackData[Idx].Data[I].BParm2);
      end;
      11:
      begin // Control Change
        Events.Cells[3, I + 1] := 'Control: ' + ControlTable[TrackData[Idx].Data[I].BParm1];
        case TrackData[Idx].Data[I].BParm1 of
          64..66, 67, 69:
          begin  // Switches
            if TrackData[Idx].Data[I].BParm2 <= 63 then
              Events.Cells[4, I + 1] := 'Off';
            if TrackData[Idx].Data[I].BParm2 >= 64 then
              Events.Cells[4, I + 1] := 'On';
          end;
          68:
          begin  // Legato Footswitch
            if TrackData[Idx].Data[I].BParm2 <= 63 then
              Events.Cells[4, I + 1] := 'Normal';
            if TrackData[Idx].Data[I].BParm2 >= 64 then
              Events.Cells[4, I + 1] := 'Legato';
          end;
          84:
          begin  // Portamento control
            if (TrackData[Idx].Data[I].Status and 15) <> 9 then
              Events.Cells[4, I + 1] :=
                'Note = ' + IntToStr(TrackData[Idx].Data[I].BParm2) +
                ' (' + NoteNum(TrackData[Idx].Data[I].BParm2) + ')'
            else
              Events.Cells[4, I + 1] :=
                'Note = ' + IntToStr(TrackData[Idx].Data[I].BParm2) +
                ' (' + DrumTable[TrackData[Idx].Data[I].BParm2] + ')';
          end;
          96..97, 120, 121, 123..125, 127:  // value ignored
            Events.Cells[4, I + 1] := '';
          118:
          begin  // Loop Point
            Events.Cells[4, I + 1] := 'Value = ' + IntToStr(TrackData[Idx].Data[I].BParm2);
            if TrackData[Idx].Data[I].BParm2 = 0 then
              Events.Cells[4, I + 1] := 'Loop Start';
            if TrackData[Idx].Data[I].BParm2 = 127 then
              Events.Cells[4, I + 1] := 'Loop End';
          end;
          122:
          begin  // Local control switch
            Events.Cells[4, I + 1] := 'Value = ' + IntToStr(TrackData[Idx].Data[I].BParm2);
            if TrackData[Idx].Data[I].BParm2 = 0 then
              Events.Cells[4, I + 1] := 'Off';
            if TrackData[Idx].Data[I].BParm2 = 127 then
              Events.Cells[4, I + 1] := 'On';
          end;
          126:
          begin  // Mono mode on
            if TrackData[Idx].Data[I].BParm2 > 0 then
              Events.Cells[4, I + 1] :=
                'Number of channels = ' + IntToStr(TrackData[Idx].Data[I].BParm2)
            else
              Events.Cells[4, I + 1] :=
                'Number of channels = ' + IntToStr(TrackData[Idx].Data[I].BParm2) +
                ' (same as number of voices in the receiver)';
          end;
          else  // others
            Events.Cells[4, I + 1] := 'Value = ' + IntToStr(TrackData[Idx].Data[I].BParm2);
        end;
      end;
      12:
      begin // Program Change
        Events.Cells[3, I + 1] := 'Program Change';
        Events.Cells[4, I + 1] := GetInstType(TrackData[Idx].Data[I].Status and 15);
        Events.Cells[4, I + 1] :=
          Events.Cells[4, I + 1] + ' = ' + IntToStr(TrackData[Idx].Data[I].BParm1);
        S := GetInstName(TrackData[Idx].Data[I].BParm1,
          TrackData[Idx].Data[I].Status and 15);
        if S <> '' then
          Events.Cells[4, I + 1] := Events.Cells[4, I + 1] + ' (' + S + ')';
      end;
      13:
      begin // Channel Aftertouch
        Events.Cells[3, I + 1] := 'Channel Aftertouch';
        Events.Cells[4, I + 1] := 'Pressure = ' + IntToStr(TrackData[Idx].Data[I].BParm1);
      end;
      14:
      begin // Pitch Bend
        Events.Cells[3, I + 1] := 'Pitch Bend';
        Events.Cells[4, I + 1] := 'Value = ' + IntToStr(TrackData[Idx].Data[I].Value);
      end;
      15:
      begin // System
        Events.Cells[3, I + 1] := 'System: ' + SystemTable[TrackData[Idx].Data[I].Status
          and 15];
        case TrackData[Idx].Data[I].Status and 15 of
          0, 7:
          begin // SysEx and EOX
            S := SysExProcess(TrackData[Idx].Data[I].DataArray);
            if S <> '' then
              Events.Cells[4, I + 1] := S
            else
              Events.Cells[4, I + 1] := 'No data';
          end;
          1:   // QuarterFrame
            Events.Cells[4, I + 1] :=
              'Message type = ' + IntToStr(TrackData[Idx].Data[I].BParm1 shr 4) + ', value = ' +
              IntToStr(TrackData[Idx].Data[I].BParm1 and 15);
          2:   // Song Position Pointer
            Events.Cells[4, I + 1] := IntToStr(TrackData[Idx].Data[I].Value) +
              ' beats (' + IntToStr(TrackData[Idx].Data[I].Value * 6) +
              ' MIDI clocks) from start';
          3:   // Song Select
            Events.Cells[4, I + 1] := 'Value = ' + IntToStr(TrackData[Idx].Data[I].BParm1);
          6: ; // Tune Request
          8: ; // TimingClock
          10: ; // Start
          11: ; // Continue
          12: ; // Stop
          14: ; // ActiveSens
          15:
          begin  // Meta Event
            Events.Cells[4, I + 1] := MetaTable[TrackData[Idx].Data[I].BParm1] +
              ': ';
            case TrackData[Idx].Data[I].BParm1 of
              // Sequence Number
              0: Events.Cells[4, I + 1] :=
                  Events.Cells[4, I + 1] + IntToStr(TrackData[Idx].Data[I].Value);
              // Strings
              1..7: Events.Cells[4, I + 1] := Events.Cells[4, I + 1] +
                  TrackData[Idx].Data[I].DataString;
              // MIDI Channel
              32: Events.Cells[4, I + 1] :=
                  Events.Cells[4, I + 1] + IntToStr(TrackData[Idx].Data[I].Value + 1);
              // MIDI Port
              33: Events.Cells[4, I + 1] :=
                  Events.Cells[4, I + 1] + IntToStr(TrackData[Idx].Data[I].Value);
              // Tempo
              81:
                Events.Cells[4, I + 1] :=
                  Events.Cells[4, I + 1] + IntToStr(TrackData[Idx].Data[I].Value div 1000) +
                  '.' + Format('%.3d',
                  [TrackData[Idx].Data[I].Value mod 1000]) + ' ms/4th (' +
                  IntToStr(60000000 div TrackData[Idx].Data[I].Value) +
                  ' BPM, ' + IntToStr(Round(InitTempo /
                  TrackData[Idx].Data[I].Value * 100)) + '%)';
              // SMPTE Offset
              84:
                Events.Cells[4, I + 1] := Events.Cells[4, I + 1] + 'Hour = ' +
                  IntToStr(TrackData[Idx].Data[I].DataArray[0]) +
                  ', min = ' + IntToStr(TrackData[Idx].Data[I].DataArray[1]) +
                  ', sec = ' + IntToStr(TrackData[Idx].Data[I].DataArray[2]) +
                  ', fr = ' + IntToStr(TrackData[Idx].Data[I].DataArray[3]) +
                  ', ff = ' + IntToStr(TrackData[Idx].Data[I].DataArray[4]);
              // Time Signature
              88:
              begin
                Events.Cells[4, I + 1] :=
                  Events.Cells[4, I + 1] + IntToStr(TrackData[Idx].Data[I].DataArray[0]) + '/' +
                  IntToStr(Round(Power(2, TrackData[Idx].Data[I].DataArray[1]))) +
                  ' time, ' + IntToStr(TrackData[Idx].Data[I].DataArray[2]) +
                  ' MIDI clock(s) per dotted-quarter, ' +
                  IntToStr(TrackData[Idx].Data[I].DataArray[3]) +
                  ' notated 32nd-notes per MIDI quarter note';
              end;
              // Key Signature
              89:
              begin
                if shortint(TrackData[Idx].Data[I].DataArray[0]) = 0 then
                  Events.Cells[4, I + 1] := Events.Cells[4, I + 1] + 'Key of C';
                if shortint(TrackData[Idx].Data[I].DataArray[0]) < 0 then
                  Events.Cells[4, I + 1] :=
                    Events.Cells[4, I + 1] + IntToStr(
                    -shortint(TrackData[Idx].Data[I].DataArray[0])) + ' flat(s)';
                if shortint(TrackData[Idx].Data[I].DataArray[0]) > 0 then
                  Events.Cells[4, I + 1] :=
                    Events.Cells[4, I + 1] + IntToStr(
                    shortint(TrackData[Idx].Data[I].DataArray[0])) + ' sharp(s)';
                case TrackData[Idx].Data[I].DataArray[1] of
                  0: Events.Cells[4, I + 1] := Events.Cells[4, I + 1] + ' major';
                  1: Events.Cells[4, I + 1] := Events.Cells[4, I + 1] + ' minor';
                end;
              end;
              // Sequencer-Specific
              127:
              begin
                S := SysExProcess(TrackData[Idx].Data[I].DataArray);
                if S = '' then
                  S := 'No data';
                Events.Cells[4, I + 1] := Events.Cells[4, I + 1] + S;
              end;
                // other meta-event
              else
              begin
                for J := 0 to TrackData[Idx].Data[I].Len - 1 do
                  Events.Cells[4, I + 1] :=
                    Events.Cells[4, I + 1] + IntToHex(TrackData[Idx].Data[I].DataArray[J], 2);
                if (Copy(Events.Cells[4, I + 1], Length(Events.Cells[4, I + 1]) - 1,
                  2) = ': ') and ((TrackData[Idx].Data[I].Len = 0) and
                  (Length(TrackData[Idx].Data[I].DataString) = 0)) then
                begin
                  S := Events.Cells[4, I + 1];
                  Delete(S, Length(S) - 1, 2);
                  Events.Cells[4, I + 1] := S;
                end;
              end; // other meta-event
            end; // case meta-event
          end; // meta-event
        end; // system event type
      end; // system event
    end; // status byte check

    if EventViewProfile = 'xmi' then
    begin
      case TrackData[Idx].Data[I].Status shr 4 of
        8: // Note Off (unused)
          Events.Cells[4, I + 1] := 'Ignored';
        9: // XMI Note
        begin
          Events.Cells[3, I + 1] := 'Play Note';
          Events.Cells[4, I + 1] :=
            Events.Cells[4, I + 1] + ', duration = ' + IntToStr(TrackData[Idx].Data[I].Len);
        end;
        11: // Control Change
        begin
          case TrackData[Idx].Data[I].BParm1 of
            32: Events.Cells[3, I + 1] := 'Control: AIL sysex start address MSB (queue 0)';
            33: Events.Cells[3, I + 1] := 'Control: AIL sysex start address KSB (queue 0)';
            34: Events.Cells[3, I + 1] := 'Control: AIL sysex start address LSB (queue 0)';
            35: Events.Cells[3, I + 1] := 'Control: AIL sysex data byte (queue 0)';
            36: Events.Cells[3, I + 1] := 'Control: AIL final sysex data byte (queue 0)';
            37: Events.Cells[3, I + 1] := 'Control: AIL sysex start address MSB (queue 1)';
            38: Events.Cells[3, I + 1] := 'Control: AIL sysex start address KSB (queue 1)';
            39: Events.Cells[3, I + 1] := 'Control: AIL sysex start address LSB (queue 1)';
            40: Events.Cells[3, I + 1] := 'Control: AIL sysex data byte (queue 1)';
            41: Events.Cells[3, I + 1] := 'Control: AIL final sysex data byte (queue 1)';
            42: Events.Cells[3, I + 1] := 'Control: AIL sysex start address MSB (queue 2)';
            43: Events.Cells[3, I + 1] := 'Control: AIL sysex start address KSB (queue 2)';
            44: Events.Cells[3, I + 1] := 'Control: AIL sysex start address LSB (queue 2)';
            45: Events.Cells[3, I + 1] := 'Control: AIL sysex data byte (queue 2)';
            46: Events.Cells[3, I + 1] := 'Control: AIL final sysex data byte (queue 2)';

            58: Events.Cells[3, I + 1] := 'Control: AIL Rhythm Setup Timbre';
            59: Events.Cells[3, I + 1] := 'Control: AIL MT-32 Patch Reverb SW';
            60: Events.Cells[3, I + 1] := 'Control: AIL MT-32 Pitch Bend range';
            61: Events.Cells[3, I + 1] := 'Control: AIL MT-32 Reverb Mode';
            62: Events.Cells[3, I + 1] := 'Control: AIL MT-32 Reverb Time';
            63: Events.Cells[3, I + 1] := 'Control: AIL MT-32 Reverb Level';

            110: Events.Cells[3, I + 1] := 'Control: AIL Channel Release';
            111: Events.Cells[3, I + 1] := 'Control: AIL Channel Lock';
            112: Events.Cells[3, I + 1] := 'Control: AIL Voice Protection';
            113: Events.Cells[3, I + 1] := 'Control: AIL Timbre Protection';
            114: Events.Cells[3, I + 1] := 'Control: AIL Select Bank';
            115: Events.Cells[3, I + 1] := 'Control: AIL Indirect Prefix';
            116: Events.Cells[3, I + 1] := 'Control: AIL Loop Start';
            117: Events.Cells[3, I + 1] := 'Control: AIL Loop End';
            118: Events.Cells[3, I + 1] := 'Control: AIL Clear Beat Count';
            119: Events.Cells[3, I + 1] := 'Control: AIL Callback Trigger';
            120: Events.Cells[3, I + 1] := 'Control: AIL Sequence Index';
          end;
        end;
        15: // System
          if TrackData[Idx].Data[I].Status = $FF then // Meta
            case TrackData[Idx].Data[I].BParm1 of
              81: // Tempo
                Events.Cells[4, I + 1] := Events.Cells[4, I + 1] + ' (ignored)';
            end;
      end;
    end;

    if EventViewProfile = 'rol' then
    begin
      case TrackData[Idx].Data[I].Status shr 4 of
        9: // Note
        begin
          Events.Cells[4, I + 1] := 'Note = ' + IntToStr(TrackData[Idx].Data[I].BParm1) +
            ' (' + NoteNum(TrackData[Idx].Data[I].BParm1) + ')';
          S := '';
          if Rhythm then
            case TrackData[Idx].Data[I].Status and 15 of
              6: S := 'Bass drum';
              7: S := 'Snare drum';
              8: S := 'Tom tom';
              9: S := 'Top cymbal';
              10: S := 'Hi-hat cymbal';
            end;
          if S <> '' then
            Events.Cells[4, I + 1] := Events.Cells[4, I + 1] + ' (' + S + ')';
          if TrackData[Idx].Data[I].BParm1 = 0 then
            Events.Cells[4, I + 1] := '';
        end;
        15: // System
        begin
          if (TrackData[Idx].Data[I].Status = $FF) and
            (TrackData[Idx].Data[I].BParm1 = $7F) and (TrackData[Idx].Data[I].Len > 1)
          then
            case TrackData[Idx].Data[I].DataArray[0] of
              0: // Tempo
              begin
                Events.Cells[3, I + 1] := 'System: Set Tempo';
                Fl := PSingle(@TrackData[Idx].Data[I].DataArray[1])^;
                Events.Cells[4, I + 1] := 'Value = ' + FormatFloat('0.00#', Fl, vFS);
              end;
              1: // Timbre
              begin
                Events.Cells[3, I + 1] := 'System: Set Timbre';
                Events.Cells[4, I + 1] :=
                  'Name = ' + PAnsiChar(@TrackData[Idx].Data[I].DataArray[1]);
              end;
              2: // Volume
              begin
                Events.Cells[3, I + 1] := 'System: Set Volume';
                Fl := PSingle(@TrackData[Idx].Data[I].DataArray[1])^;
                Events.Cells[4, I + 1] := 'Value = ' + FormatFloat('0.00#', Fl, vFS);
              end;
              3: // Pitch
              begin
                Events.Cells[3, I + 1] := 'System: Set Pitch';
                Fl := PSingle(@TrackData[Idx].Data[I].DataArray[1])^;
                Events.Cells[4, I + 1] := 'Value = ' + FormatFloat('0.00#', Fl, vFS);
              end;
            end;
        end;
      end;
    end;

    if (EventViewProfile = 'mus') or (EventViewProfile = 'ims') then
    begin
      case TrackData[Idx].Data[I].Status shr 4 of
        8: // Note Off
        begin
          if (EventViewProfile = 'ims') and
            (TrackData[Idx].Data[I].BParm2 > 0) then
            Events.Cells[3, I + 1] := 'Note Retrigger';
          Events.Cells[4, I + 1] := 'Note = ' + IntToStr(TrackData[Idx].Data[I].BParm1) +
            ' (' + NoteNum(TrackData[Idx].Data[I].BParm1) +
            '), velocity = ' + IntToStr(TrackData[Idx].Data[I].BParm2);
          if TrackData[Idx].Data[I].BParm1 = 0 then
            Events.Cells[4, I + 1] :=
              'Velocity = ' + IntToStr(TrackData[Idx].Data[I].BParm2);
          S := '';
          if Rhythm then
            case TrackData[Idx].Data[I].Status and 15 of
              6: S := 'Bass drum';
              7: S := 'Snare drum';
              8: S := 'Tom tom';
              9: S := 'Top cymbal';
              10: S := 'Hi-hat cymbal';
            end;
          if S <> '' then
            Events.Cells[4, I + 1] := Events.Cells[4, I + 1] + ' (' + S + ')';
        end;
        9: // Note On
        begin
          Events.Cells[4, I + 1] := 'Note = ' + IntToStr(TrackData[Idx].Data[I].BParm1) +
            ' (' + NoteNum(TrackData[Idx].Data[I].BParm1) +
            '), velocity = ' + IntToStr(TrackData[Idx].Data[I].BParm2);
          S := '';
          if Rhythm then
            case TrackData[Idx].Data[I].Status and 15 of
              6: S := 'Bass drum';
              7: S := 'Snare drum';
              8: S := 'Tom tom';
              9: S := 'Top cymbal';
              10: S := 'Hi-hat cymbal';
            end;
          if S <> '' then
            Events.Cells[4, I + 1] := Events.Cells[4, I + 1] + ' (' + S + ')';
        end;
        10: // Volume Change
        begin
          Events.Cells[3, I + 1] := 'Volume Change';
          Events.Cells[4, I + 1] :=
            'Value = ' + IntToStr(TrackData[Idx].Data[I].BParm1);
        end;
        15: // System
        begin
          if (TrackData[Idx].Data[I].Status = $F0) and
            (TrackData[Idx].Data[I].Len = 5) and
            (TrackData[Idx].Data[I].DataArray[0] = $7F) then
          begin
            Events.Cells[3, I + 1] := 'System: Set Speed';
            Speed :=
              (TrackData[Idx].Data[I].DataArray[3] / 128) +
              TrackData[Idx].Data[I].DataArray[2];
            Events.Cells[4, I + 1] := 'Value = ' + FormatFloat('0.00#', Speed, vFS);
          end;
          if (TrackData[Idx].Data[I].Status = $FC) then
          begin
            Events.Cells[3, I + 1] := 'System: Song End';
          end;
        end;
      end;
    end;

    if EventViewProfile = 'mdi' then
    begin
      case TrackData[Idx].Data[I].Status shr 4 of
        8:
        begin // NoteOff
          if TrackData[Idx].Data[I].BParm1 = 0 then
            Events.Cells[4, I + 1] :=
              'Velocity = ' + IntToStr(TrackData[Idx].Data[I].BParm2)
          else
            Events.Cells[4, I + 1] :=
              'Note = ' + IntToStr(TrackData[Idx].Data[I].BParm1) +
              ' (' + NoteNum(TrackData[Idx].Data[I].BParm1) + '), velocity = ' +
              IntToStr(TrackData[Idx].Data[I].BParm2);
          S := '';
          if Rhythm then
            case TrackData[Idx].Data[I].Status and 15 of
              6: S := 'Bass drum';
              7: S := 'Snare drum';
              8: S := 'Tom tom';
              9: S := 'Top cymbal';
              10: S := 'Hi-hat cymbal';
            end;
          if S <> '' then
            Events.Cells[4, I + 1] := Events.Cells[4, I + 1] + ' (' + S + ')';
        end;
        9:
        begin // NoteOn
          Events.Cells[4, I + 1] := 'Note = ' + IntToStr(TrackData[Idx].Data[I].BParm1) +
            ' (' + NoteNum(TrackData[Idx].Data[I].BParm1) +
            '), velocity = ' + IntToStr(TrackData[Idx].Data[I].BParm2);
          S := '';
          if Rhythm then
            case TrackData[Idx].Data[I].Status and 15 of
              6: S := 'Bass drum';
              7: S := 'Snare drum';
              8: S := 'Tom tom';
              9: S := 'Top cymbal';
              10: S := 'Hi-hat cymbal';
            end;
          if S <> '' then
            Events.Cells[4, I + 1] := Events.Cells[4, I + 1] + ' (' + S + ')';
        end;
        13:
        begin // Volume Change
          Events.Cells[3, I + 1] := 'Volume Change';
          Events.Cells[4, I + 1] :=
            'Value = ' + IntToStr(TrackData[Idx].Data[I].BParm1);
        end;
        15:
        begin // System
          if (TrackData[Idx].Data[I].Status and 15 = 0) or
            ((TrackData[Idx].Data[I].Status and 15 = 15) and
            (TrackData[Idx].Data[I].BParm1 = 127)) then
            if (TrackData[Idx].Data[I].Len = 6) and
              (TrackData[Idx].Data[I].DataArray[0] = $00) and
              (TrackData[Idx].Data[I].DataArray[1] = $00) and
              (TrackData[Idx].Data[I].DataArray[2] = $3F) and
              (TrackData[Idx].Data[I].DataArray[3] = $00) and
              (TrackData[Idx].Data[I].DataArray[4] = $02) then
            begin
              if TrackData[Idx].Data[I].DataArray[5] = 0 then
                Rhythm := False;
              if TrackData[Idx].Data[I].DataArray[5] = 1 then
                Rhythm := True;
            end;
        end;
      end;
    end;

    if EventViewProfile = 'sop' then
    begin
      case TrackData[Idx].Data[I].Status shr 4 of
        9: // Note On
        begin
          Events.Cells[3, I + 1] := 'Play Note';
          Events.Cells[4, I + 1] :=
            'Note = ' + IntToStr(TrackData[Idx].Data[I].BParm1) +
            ' (' + NoteNum(TrackData[Idx].Data[I].BParm1) + ')';
          S := '';
          if Rhythm then
            case TrackData[Idx].Data[I].Status and 15 of
              6: S := 'Bass drum';
              7: S := 'Snare drum';
              8: S := 'Tom tom';
              9: S := 'Top cymbal';
              10: S := 'Hi-hat cymbal';
            end;
          if S <> '' then
            Events.Cells[4, I + 1] := Events.Cells[4, I + 1] + ' (' + S + ')';
          Events.Cells[4, I + 1] :=
            Events.Cells[4, I + 1] + ', duration = ' + IntToStr(TrackData[Idx].Data[I].Len);
        end;
        11: // Control Change
        begin
          case TrackData[Idx].Data[I].BParm1 of
            7: // Volume Change
            begin
              Events.Cells[3, I + 1] := 'Volume Change';
              Events.Cells[4, I + 1] :=
                'Value = ' + IntToStr(TrackData[Idx].Data[I].BParm2);
            end;
            9: // Pitch Change
            begin
              Events.Cells[3, I + 1] := 'Pitch Change';
              Events.Cells[4, I + 1] := IntToStr(TrackData[Idx].Data[I].BParm2 - 100) + '%';
              if TrackData[Idx].Data[I].BParm2 <= 100 then
                Events.Cells[4, I + 1] := 'Value = ' + Events.Cells[4, I + 1]
              else
                Events.Cells[4, I + 1] := 'Value = +' + Events.Cells[4, I + 1];
            end;
            10: // Set Panning
            begin
              Events.Cells[3, I + 1] := 'Set Panning';
              case TrackData[Idx].Data[I].BParm2 of
                0:
                  Events.Cells[4, I + 1] := 'Left';
                64:
                  Events.Cells[4, I + 1] := 'Middle';
                127:
                  Events.Cells[4, I + 1] := 'Right';
                else
                  Events.Cells[4, I + 1] :=
                    'Value = ' + IntToStr(TrackData[Idx].Data[I].BParm2);
              end;
            end;
            16: // Global Volume
            begin
              Events.Cells[3, I + 1] := 'Global Volume';
              Events.Cells[4, I + 1] :=
                'Value = ' + IntToStr(TrackData[Idx].Data[I].BParm2);
            end;
            17: // Change Tempo
            begin
              Events.Cells[3, I + 1] := 'Change Tempo';
              Events.Cells[4, I + 1] :=
                'Value = ' + IntToStr(TrackData[Idx].Data[I].BParm2) + ' BPM';
            end;
            18: // Special Event
            begin
              Events.Cells[3, I + 1] := 'Special Event';
              Events.Cells[4, I + 1] :=
                'Value = ' + IntToStr(TrackData[Idx].Data[I].BParm2);
            end;
          end;
        end;
      end;
    end;

    if EventViewProfile = 'cmf' then
    begin
      case TrackData[Idx].Data[I].Status shr 4 of
        8:
        begin // NoteOff
          Events.Cells[4, I + 1] :=
            'Note = ' + IntToStr(TrackData[Idx].Data[I].BParm1) +
            ' (' + NoteNum(TrackData[Idx].Data[I].BParm1) +
            '), velocity = ' + IntToStr(TrackData[Idx].Data[I].BParm2);
          S := '';
          if Rhythm then
            case TrackData[Idx].Data[I].Status and 15 of
              11: S := 'Bass drum';
              12: S := 'Snare drum';
              13: S := 'Tom tom';
              14: S := 'Top cymbal';
              15: S := 'Hi-hat cymbal';
            end;
          if S <> '' then
            Events.Cells[4, I + 1] := Events.Cells[4, I + 1] + ' (' + S + ')';
        end;
        9:
        begin // NoteOn
          Events.Cells[4, I + 1] :=
            'Note = ' + IntToStr(TrackData[Idx].Data[I].BParm1) +
            ' (' + NoteNum(TrackData[Idx].Data[I].BParm1) +
            '), velocity = ' + IntToStr(TrackData[Idx].Data[I].BParm2);
          S := '';
          if Rhythm then
            case TrackData[Idx].Data[I].Status and 15 of
              11: S := 'Bass drum';
              12: S := 'Snare drum';
              13: S := 'Tom tom';
              14: S := 'Top cymbal';
              15: S := 'Hi-hat cymbal';
            end;
          if S <> '' then
            Events.Cells[4, I + 1] := Events.Cells[4, I + 1] + ' (' + S + ')';
        end;
        11:
        begin // Control Change
          case TrackData[Idx].Data[I].BParm1 of
            99:
            begin // Depth control
              Events.Cells[3, I + 1] := 'Control: Amplitude+Vibrato Depth';
              case TrackData[Idx].Data[I].BParm2 of
                0: Events.Cells[4, I + 1] := 'Amplitude = 1 dB, vibrato = 7 cents';
                1: Events.Cells[4, I + 1] := 'Amplitude = 1 dB, vibrato = 14 cents';
                2: Events.Cells[4, I + 1] := 'Amplitude = 4.8 dB, vibrato = 7 cents';
                3: Events.Cells[4, I + 1] := 'Amplitude = 4.8 dB, vibrato = 14 cents';
              end;
            end;
            102:
            begin // Set marker byte
              Events.Cells[3, I + 1] := 'Control: Set Marker Byte';
            end;
            103:
            begin // Rhythm mode change
              Events.Cells[3, I + 1] := 'Control: Rhythm Mode';
              Rhythm := TrackData[Idx].Data[I].BParm2 > 0;
              if Rhythm then
                Events.Cells[4, I + 1] := 'On'
              else
                Events.Cells[4, I + 1] := 'Off';
            end;
            104..105:
            begin // Transpose Up/Down
              if TrackData[Idx].Data[I].BParm1 = 104 then
                Events.Cells[3, I + 1] := 'Control: Transpose Up';
              if TrackData[Idx].Data[I].BParm1 = 105 then
                Events.Cells[3, I + 1] := 'Control: Transpose Down';
              if TrackData[Idx].Data[I].BParm2 = 0 then
                Events.Cells[4, I + 1] := 'Not transposed';
              if (TrackData[Idx].Data[I].BParm2 > 0) and
                (TrackData[Idx].Data[I].BParm2 < 127) then
                Events.Cells[4, I + 1] :=
                  IntToStr(TrackData[Idx].Data[I].BParm2) + '/128 semitones';
              if TrackData[Idx].Data[I].BParm2 = 127 then
                Events.Cells[4, I + 1] := 'Full semitone';
            end;
          end;
        end;
      end;
    end;

    if EventViewProfile = 'herad' then
    begin
      if not SongData_GetInt('HERAD_V2', J) then
        J := 0;
      HERAD_V2 := J > 0;
      case TrackData[Idx].Data[I].Status shr 4 of
        8:
        begin // NoteOff
          if HERAD_V2 then
            Events.Cells[4, I + 1] :=
              'Note = ' + IntToStr(TrackData[Idx].Data[I].BParm1) +
              ' (' + NoteNum(TrackData[Idx].Data[I].BParm1) + ')';
        end;
      end;
    end;
    Inc(Cnt);
  end; // track read
end;

//==============================================================================
//
// TXMICore.EventsTopLeftChanged
//
//==============================================================================
procedure TXMICore.EventsTopLeftChanged(Sender: TObject);
begin
  FillEvents(TrkCh.ItemIndex);
end;

//==============================================================================
//
// TXMICore.DelEvent
//
//==============================================================================
function TXMICore.DelEvent(Trk, Idx: integer; TimeSave: boolean): boolean;
var
  I: integer;
  Delta: UInt64;
begin
  Result := False;
  if (Trk < 0) or (Trk >= Length(TrackData)) then
    Exit;
  if TimeSave then
    Delta := TrackData[Trk].Data[Idx].Ticks
  else
    Delta := 0;
  for I := Idx + 1 to Length(TrackData[Trk].Data) - 1 do
    TrackData[Trk].Data[I - 1] := TrackData[Trk].Data[I];
  SetLength(TrackData[Trk].Data, Length(TrackData[Trk].Data) - 1);
  if TimeSave and (Idx < Length(TrackData[Trk].Data)) then
    TrackData[Trk].Data[Idx].Ticks :=
      TrackData[Trk].Data[Idx].Ticks + Delta;
  Result := True;
end;

//==============================================================================
//
// TXMICore.DelRange
//
//==============================================================================
function TXMICore.DelRange(Trk, From, Count: integer; TimeSave: boolean): boolean;
var
  I: integer;
  Delta: UInt64;
  Store: boolean;
begin
  Result := False;
  if Count = 0 then
    Exit;
  if (Trk < 0) or (Trk >= Length(TrackData)) then
    Exit;
  Store := True;
  Delta := 0;
  if From + Count = Length(TrackData[Trk].Data) then
    Store := False;
  if TimeSave then
    for I := From to From + Count - 1 do
      Delta := Delta + TrackData[Trk].Data[I].Ticks;
  for I := From + Count to Length(TrackData[Trk].Data) - 1 do
    TrackData[Trk].Data[I - Count] := TrackData[Trk].Data[I];
  SetLength(TrackData[Trk].Data,
    Length(TrackData[Trk].Data) - Count);
  if TimeSave and Store then
    TrackData[Trk].Data[From].Ticks := TrackData[Trk].Data[From].Ticks + Delta;
  Result := True;
end;

//==============================================================================
//
// TXMICore.LogSongInfo
//
//==============================================================================
procedure TXMICore.LogSongInfo;
var
  Ver, Division: string;
  SMPTE: shortint;
begin
  SongData_GetStr('MIDIType', Ver);
  SongData_GetStr('Division', Division);
  LogOutput('MIDI Type   ' + Ver);
  LogOutput('Track count ' + IntToStr(Length(TrackData)));
  if not SongData_GetSInt('SMPTE', SMPTE) then
    SMPTE := 0;
  if SMPTE >= 0 then
    LogOutput('Division    ' + Division + ' PPQN')
  else
  begin
    LogOutput('Division    ' + Division + ' TPF');
    LogOutput('SMPTE       ' + IntToStr(-SMPTE) + ' FPS');
  end;
end;

//==============================================================================
//
// TXMICore.AddTrack
//
//==============================================================================
procedure TXMICore.AddTrack;
begin
  SetLength(TrackData, Length(TrackData) + 1);
  TrackData[Length(TrackData) - 1].Title := '';
  SetLength(TrackData[Length(TrackData) - 1].Data, 1);
  TrackData[Length(TrackData) - 1].Data[0].Status := $FF;
  TrackData[Length(TrackData) - 1].Data[0].BParm1 := $2F;
  TrackData[Length(TrackData) - 1].Data[0].Len := 0;
  LogOutput('[*] Track ' + IntToStr(Length(TrackData) - 1) + ' added.');
end;

//==============================================================================
//
// TXMICore.DelTrack
//
//==============================================================================
procedure TXMICore.DelTrack(Idx: integer);
var
  I: integer;
  Title: ansistring;
begin
  Title := TrackData[Idx].Title;
  for I := Idx + 1 to Length(TrackData) - 1 do
    TrackData[I - 1] := TrackData[I];
  SetLength(TrackData, Length(TrackData) - 1);
  if Title = '' then
    LogOutput('[*] Track ' + IntToStr(Idx) + ' deleted.')
  else
    LogOutput('[*] Track ' + IntToStr(Idx) + ' (' + Title + ') deleted.');
end;

//==============================================================================
//
// TXMICore.TimeBetween
//
//==============================================================================
function TXMICore.TimeBetween(Trk, Idx1, Idx2: integer): UInt64;
var
  I: integer;
begin
  Result := 0;
  for I := Idx1 + 1 to Idx2 do
    Result := Result + TrackData[Trk].Data[I].Ticks;
end;

//==============================================================================
//
// TXMICore.ConvertEvents
//
//==============================================================================
procedure TXMICore.ConvertEvents(DestProfile: ansistring);
begin
  if EventProfile = 'mid' then
  begin
    if (DestProfile = 'mid') or (DestProfile = 'mdi') then
    begin
      Convert_MID_FixTempo;
    end;
    if DestProfile = 'xmi' then
    begin
      Convert_MID_XMI;
      EventProfile := 'xmi';
      EventViewProfile := 'xmi';
    end;
    if DestProfile = 'mus' then
    begin
      Convert_MID_MUS;
      EventProfile := 'mus';
      EventViewProfile := 'mus';
    end;
  end;
  if EventProfile = 'mdi' then
  begin
    if DestProfile = 'mid' then
    begin
      Convert_MDI_MID;
      EventProfile := 'mid';
      EventViewProfile := 'mid';
    end;
    if DestProfile = 'rol' then
    begin
      Convert_MDI_ROL;
      EventProfile := 'rol';
      EventViewProfile := 'rol';
    end;
    if DestProfile = 'mus' then
    begin
      Convert_MDI_MUS;
      EventProfile := 'mus';
      EventViewProfile := 'mus';
    end;
    if DestProfile = 'cmf' then
    begin
      Convert_MDI_CMF;
      EventProfile := 'cmf';
      EventViewProfile := 'cmf';
    end;
  end;
  if EventProfile = 'xmi' then
  begin
    if DestProfile = 'mid' then
    begin
      Convert_XMI_MID;
      EventProfile := 'mid';
      EventViewProfile := 'mid';
    end;
  end;
  if EventProfile = 'cmf' then
  begin
    if DestProfile = 'mid' then
    begin
      Convert_CMF_MID;
      EventProfile := 'mid';
      EventViewProfile := 'mid';
    end;
    if DestProfile = 'mdi' then
    begin
      Convert_CMF_MDI;
      EventProfile := 'mdi';
      EventViewProfile := 'mdi';
    end;
  end;
  if EventProfile = 'rol' then
  begin
    if DestProfile = 'mid' then
    begin
      Convert_ROL_MID;
      EventProfile := 'mid';
      EventViewProfile := 'mid';
    end;
  end;
  if EventProfile = 'mus' then
  begin
    if DestProfile = 'mid' then
    begin
      Convert_MUS_MID;
      EventProfile := 'mid';
      EventViewProfile := 'mid';
    end;
    if DestProfile = 'mdi' then
    begin
      Convert_MUS_MDI;
      EventProfile := 'mdi';
      EventViewProfile := 'mdi';
    end;
    if DestProfile = 'rol' then
    begin
      Convert_MUS_ROL;
      EventProfile := 'rol';
      EventViewProfile := 'rol';
    end;
  end;
  if EventProfile = 'ims' then
  begin
    if DestProfile = 'mid' then
    begin
      Convert_IMS_MID;
      EventProfile := 'mid';
      EventViewProfile := 'mid';
    end;
    if DestProfile = 'rol' then
    begin
      Convert_IMS_ROL;
      EventProfile := 'rol';
      EventViewProfile := 'rol';
    end;
  end;
  if EventProfile = 'sop' then
  begin
    if DestProfile = 'mid' then
    begin
      Convert_SOP_MID;
      EventProfile := 'mid';
      EventViewProfile := 'mid';
    end;
    if DestProfile = 'mus' then
    begin
      Convert_SOP_MUS;
      EventProfile := 'mus';
      EventViewProfile := 'mus';
    end;
  end;
  if EventProfile = 'herad' then
  begin
    if DestProfile = 'mid' then
    begin
      Convert_HERAD_MID;
      EventProfile := 'mid';
      EventViewProfile := 'mid';
    end;
    if DestProfile = 'herad_v2' then
    begin
      Convert_HERAD_HERAD_V2;
      EventProfile := 'herad';
      EventViewProfile := 'herad';
    end;
  end;
end;

//==============================================================================
//
// TXMICore.ConvertTicks
//
//==============================================================================
procedure TXMICore.ConvertTicks(RelToAbs: boolean; var Data: array of Command);
var
  I: integer;
  TicksPos: UInt64;
begin
  TicksPos := 0;
  if RelToAbs then
  begin
    for I := 0 to Length(Data) - 1 do
    begin
      Data[I].Ticks := Data[I].Ticks + TicksPos;
      TicksPos := Data[I].Ticks;
    end;
  end
  else
    for I := 0 to Length(Data) - 1 do
    begin
      Data[I].Ticks := Data[I].Ticks - TicksPos;
      TicksPos := TicksPos + Data[I].Ticks;
    end;
end;

//==============================================================================
//
// TXMICore.MergeTracksUsingTicks
//
//==============================================================================
function TXMICore.MergeTracksUsingTicks(Tracks: array of integer;
  EOT: boolean): Chunk;
var
  I: integer;
  Positions: array of UInt64;
  Cmd: Command;

  function GetTick(Idx: integer; var Tick: UInt64): boolean;
  begin
    Result := False;
    if Positions[Idx] >= Length(TrackData[Tracks[Idx]].Data) then
      Exit;
    Tick := TrackData[Tracks[Idx]].Data[Positions[Idx]].Ticks;
    Result := True;
  end;

  function GetNextEvent: boolean;
  var
    I, Idx: integer;
    MinTick, Tick: UInt64;
    FoundTick: boolean;
  begin
    Result := False;
    Idx := -1;
    MinTick := 0;
    FoundTick := False;
    for I := 0 to Length(Tracks) - 1 do
      if GetTick(I, Tick) then
        if (Idx = -1) or ((Tick < MinTick) or not FoundTick) then
        begin
          MinTick := Tick;
          FoundTick := True;
          Idx := I;
        end;
    if Idx = -1 then
      Exit;
    Cmd := TrackData[Tracks[Idx]].Data[Positions[Idx]];
    Inc(Positions[Idx]);
    Result := True;
  end;

begin
  SetLength(Positions, Length(Tracks));
  for I := 0 to Length(Positions) - 1 do
    Positions[I] := 0;
  Result.Title := '';
  SetLength(Result.Data, 0);
  for I := 0 to Length(Tracks) - 1 do
    ConvertTicks(True, TrackData[Tracks[I]].Data);
  while GetNextEvent do
  begin
    SetLength(Result.Data, Length(Result.Data) + 1);
    Result.Data[High(Result.Data)] := Cmd;
  end;
  if EOT then
  begin
    SetLength(Result.Data, Length(Result.Data) + 1);
    if Length(Result.Data) > 1 then
      Result.Data[High(Result.Data)].Ticks := Result.Data[High(Result.Data) - 1].Ticks
    else
      Result.Data[High(Result.Data)].Ticks := 0;
    Result.Data[High(Result.Data)].Status := $FF;
    Result.Data[High(Result.Data)].BParm1 := $2F;
  end;
  for I := 0 to Length(Tracks) - 1 do
    ConvertTicks(False, TrackData[Tracks[I]].Data);
  ConvertTicks(False, Result.Data);
end;

//==============================================================================
//
// TXMICore.MergeTracksByTicks
//
//==============================================================================
procedure TXMICore.MergeTracksByTicks(Trk: array of integer);
var
  I, Idx: integer;
  Track: Chunk;
  Tracks: array of integer;
begin
  SetLength(Tracks, Length(Trk));
  if Length(Tracks) = 0 then
  begin
    SetLength(Tracks, Length(TrackData));
    for I := 0 to Length(Tracks) - 1 do
      Tracks[I] := I;
  end
  else
    for I := 0 to Length(Trk) - 1 do
      Tracks[I] := Trk[I];
  if Length(Tracks) < 2 then
  begin
    LogOutput('[-] Merge failed. At least two tracks are required.');
    Exit;
  end;
  LogOutput('[*] Clearing EOT events...');
  for I := 0 to Length(Tracks) - 1 do
  begin
    Idx := Tracks[I];
    if (Length(TrackData[Idx].Data) > 0) and
      (TrackData[Idx].Data[High(TrackData[Idx].Data)].Status = $FF) and
      (TrackData[Idx].Data[High(TrackData[Idx].Data)].BParm1 = $2F) then
      DelEvent(I, High(TrackData[Idx].Data), False);
  end;
  LogOutput('[*] Merging tracks...');
  Track := MergeTracksUsingTicks(Tracks, True);
  for I := Length(Tracks) - 1 downto 1 do
    DelTrack(Tracks[I]);
  LogOutput('[*] Assigning to track ' + IntToStr(Tracks[0]) + '...');
  TrackData[Tracks[0]] := Track;
  LogOutput('[+] Done.');
end;

//==============================================================================
//
// TXMICore.MergeTracksByOrder
//
//==============================================================================
procedure TXMICore.MergeTracksByOrder(Trk: array of integer);
var
  I, J, Idx: integer;
  Ticks: UInt64;
  Tracks: array of integer;
begin
  SetLength(Tracks, Length(Trk));
  if Length(Tracks) = 0 then
  begin
    SetLength(Tracks, Length(TrackData));
    for I := 0 to Length(Tracks) - 1 do
      Tracks[I] := I;
  end
  else
    for I := 0 to Length(Trk) - 1 do
      Tracks[I] := Trk[I];
  if Length(Tracks) < 2 then
  begin
    LogOutput('[-] Merge failed. At least two tracks are required.');
    Exit;
  end;
  LogOutput('[*] Clearing EOT events...');
  for I := 0 to Length(Tracks) - 2 do
  begin
    Idx := Tracks[I];
    if (Length(TrackData[Idx].Data) > 0) and
      (TrackData[Idx].Data[High(TrackData[Idx].Data)].Status = $FF) and
      (TrackData[Idx].Data[High(TrackData[Idx].Data)].BParm1 = $2F) then
    begin
      Ticks := TrackData[Idx].Data[High(TrackData[Idx].Data)].Ticks;
      TrackData[Tracks[I + 1]].Data[0].Ticks :=
        TrackData[Tracks[I + 1]].Data[0].Ticks + Ticks;
      DelEvent(Idx, Length(TrackData[Idx].Data) - 1, False);
    end;
  end;
  LogOutput('[*] Merging tracks...');
  for I := 1 to Length(Tracks) - 1 do
  begin
    Idx := Length(TrackData[Tracks[0]].Data);
    SetLength(TrackData[Tracks[0]].Data, Idx + Length(TrackData[Tracks[I]].Data));
    for J := 0 to Length(TrackData[Tracks[I]].Data) - 1 do
      TrackData[Tracks[0]].Data[Idx + J] := TrackData[Tracks[I]].Data[J];
  end;
  for I := Length(Tracks) - 1 downto 1 do
    DelTrack(Tracks[I]);
  LogOutput('[+] Done.');
end;

//==============================================================================
//
// TXMICore.CopyEvent
//
//==============================================================================
procedure TXMICore.CopyEvent(SrcTrk, SrcIdx, DestTrk, DestIdx: integer);
begin
  TrackData[DestTrk].Data[DestIdx] := TrackData[SrcTrk].Data[SrcIdx];
end;

//==============================================================================
//
// TXMICore.MOptimizeClick
//
//==============================================================================
procedure TXMICore.MOptimizeClick(Sender: TObject);
var
  I, J, K, L: integer;
  Notes: array[0..15] of array of byte;
  Pitch: array[0..15] of longint;
  Prg: array[0..15] of smallint;
  Ctrl: array[0..15] of array[0..127] of smallint;
  Counter: UInt64;
  NoteFnd: boolean;
begin
  if MessageBox(Handle, 'This action will delete unnecessary events and excess ' +
    'status bytes (with time saving). You will see effect on resulting file size. ' +
    'Do you wish to continue?', 'Optimize song', mb_IconQuestion or mb_YesNo) <> mrYes then
    Exit;
  LogOutput('[*] Optimizing note off events...');
  Counter := 0;
  for I := 0 to Length(TrackData) - 1 do
  begin
    J := 0;
    while J < Length(TrackData[I].Data) do
      case TrackData[I].Data[J].Status shr 4 of
        8:
        begin
          NoteFnd := False;
          for K := 0 to Length(Notes[TrackData[I].Data[J].Status and 15]) - 1 do
            if Notes[TrackData[I].Data[J].Status and 15][K] =
              TrackData[I].Data[J].BParm1 then
            begin
              NoteFnd := True;
              Break;
            end;
          if not NoteFnd then
          begin
            DelEvent(I, J, True);
            Inc(Counter);
          end
          else
          begin
            for L := K + 1 to Length(Notes[TrackData[I].Data[J].Status and 15]) - 1 do
              Notes[TrackData[I].Data[J].Status and 15][L - 1] :=
                Notes[TrackData[I].Data[J].Status and 15][L];
            SetLength(Notes[TrackData[I].Data[J].Status and 15],
              Length(Notes[TrackData[I].Data[J].Status and 15]) - 1);
            Inc(J);
          end;
        end;
        9: if TrackData[I].Data[J].BParm2 = 0 then
          begin
            NoteFnd := False;
            for K := 0 to Length(Notes[TrackData[I].Data[J].Status and 15]) - 1 do
              if Notes[TrackData[I].Data[J].Status and 15][K] =
                TrackData[I].Data[J].BParm1 then
              begin
                NoteFnd := True;
                Break;
              end;
            if not NoteFnd then
            begin
              DelEvent(I, J, True);
              Inc(Counter);
            end
            else
            begin
              for L := K + 1 to Length(Notes[TrackData[I].Data[J].Status and 15]) - 1 do
                Notes[TrackData[I].Data[J].Status and 15][L - 1] :=
                  Notes[TrackData[I].Data[J].Status and 15][L];
              SetLength(Notes[TrackData[I].Data[J].Status and 15],
                Length(Notes[TrackData[I].Data[J].Status and 15]) - 1);
              Inc(J);
            end;
          end
          else
          begin
            NoteFnd := False;
            for K := 0 to Length(Notes[TrackData[I].Data[J].Status and 15]) - 1 do
              if Notes[TrackData[I].Data[J].Status and 15][K] =
                TrackData[I].Data[J].BParm1 then
              begin
                NoteFnd := True;
                Break;
              end;
            if not NoteFnd then
            begin
              SetLength(Notes[TrackData[I].Data[J].Status and 15],
                Length(Notes[TrackData[I].Data[J].Status and 15]) + 1);
              Notes[TrackData[I].Data[J].Status and 15]
                [Length(Notes[TrackData[I].Data[J].Status and 15]) - 1] :=
                TrackData[I].Data[J].BParm1;
            end;
            Inc(J);
          end;
        else
          Inc(J);
      end;
  end;
  LogOutput('[+] Deleted ' + IntToStr(Counter) + ' events.');
  LogOutput('[*] Optimizing control change events...');
  Counter := 0;
  for I := 0 to 15 do
    for J := 0 to 127 do
      Ctrl[I][J] := -1;
  for I := 0 to Length(TrackData) - 1 do
  begin
    J := 0;
    while J < Length(TrackData[I].Data) do
      if TrackData[I].Data[J].Status shr 4 = 11 then
      begin
        if (TrackData[I].Data[J].BParm2 =
          Ctrl[TrackData[I].Data[J].Status and 15][TrackData[I].Data[J].BParm1]) and
          (not TrackData[I].Data[J].BParm1 in [$06, $26, $60, $61, $62, $63, $64, $65]) then
        begin
          DelEvent(I, J, True);
          Inc(Counter);
        end
        else
        begin
          Ctrl[TrackData[I].Data[J].Status and 15][TrackData[I].Data[J].BParm1] :=
            TrackData[I].Data[J].BParm2;
          Inc(J);
        end;
      end
      else
        Inc(J);
  end;
  LogOutput('[+] Deleted ' + IntToStr(Counter) + ' events.');
  LogOutput('[*] Optimizing program change events...');
  Counter := 0;
  for I := 0 to 15 do
    Prg[I] := -1;
  for I := 0 to Length(TrackData) - 1 do
  begin
    J := 0;
    while J < Length(TrackData[I].Data) do
      if TrackData[I].Data[J].Status shr 4 = 12 then
      begin
        if TrackData[I].Data[J].BParm1 = Prg[TrackData[I].Data[J].Status and 15] then
        begin
          DelEvent(I, J, True);
          Inc(Counter);
        end
        else
        begin
          Prg[TrackData[I].Data[J].Status and 15] := TrackData[I].Data[J].BParm1;
          Inc(J);
        end;
      end
      else
        Inc(J);
  end;
  LogOutput('[+] Deleted ' + IntToStr(Counter) + ' events.');
  LogOutput('[*] Optimizing pitch bends...');
  Counter := 0;
  for I := 0 to 15 do
    Pitch[I] := -1;
  for I := 0 to Length(TrackData) - 1 do
  begin
    J := 0;
    while J < Length(TrackData[I].Data) do
      if TrackData[I].Data[J].Status shr 4 = 14 then
      begin
        if TrackData[I].Data[J].Value = Pitch[TrackData[I].Data[J].Status and 15] then
        begin
          DelEvent(I, J, True);
          Inc(Counter);
        end
        else
        begin
          Pitch[TrackData[I].Data[J].Status and 15] := TrackData[I].Data[J].Value;
          Inc(J);
        end;
      end
      else
        Inc(J);
  end;
  LogOutput('[+] Deleted ' + IntToStr(Counter) + ' events.');
  LogOutput('[*] Optimizing status bytes...');
  Counter := 0;
  for I := 0 to Length(TrackData) - 1 do
    for J := 1 to Length(TrackData[I].Data) - 1 do
      if (TrackData[I].Data[J].Status = TrackData[I].Data[J - 1].Status) and
        (TrackData[I].Data[J].Status < $F0) and not TrackData[I].Data[J].RunStatMode then
      begin
        TrackData[I].Data[J].RunStatMode := True;
        Inc(Counter);
      end;
  LogOutput('[+] Deleted ' + IntToStr(Counter) + ' excess status bytes.');
  LogOutput('[+] Done.');
  FillEvents(TrkCh.ItemIndex);
  ChkButtons;
end;

//==============================================================================
//
// TXMICore.LogOutput
//
//==============================================================================
procedure TXMICore.LogOutput(const s: string);
begin
  Log.Lines.Add(s);
end;

end.

