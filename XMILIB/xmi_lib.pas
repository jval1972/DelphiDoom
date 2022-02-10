unit xmi_lib;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

//==============================================================================
//
// XMI_Init
//
//==============================================================================
procedure XMI_Init;

//==============================================================================
//
// XMI_ShutDown
//
//==============================================================================
procedure XMI_ShutDown;

//==============================================================================
//
// XMI_OpenMusicFile
//
//==============================================================================
function XMI_OpenMusicFile(const fname: string): Boolean;

//==============================================================================
//
// XMI_GetNumTracks
//
//==============================================================================
function XMI_GetNumTracks: Integer;

//==============================================================================
//
// XMI_ConvertTrackToFile
//
//==============================================================================
function XMI_ConvertTrackToFile(const trNo: integer; const fname: string): Boolean;

//==============================================================================
//
// XMI_ConvertMemoryToFile
//
//==============================================================================
function XMI_ConvertMemoryToFile(const adata: pointer; const asize: integer; const trNo: integer; const fname: string): Boolean;

//==============================================================================
//
// XMI_ConvertTrackToMemory
//
//==============================================================================
function XMI_ConvertTrackToMemory(const trNo: integer; const typ: string; var p: pointer; var sz: integer): Boolean;

//==============================================================================
//
// XMI_FreeMem
//
//==============================================================================
procedure XMI_FreeMem(var p: pointer; var sz: integer);

//==============================================================================
//
// XMI_PlayTrack
//
//==============================================================================
function XMI_PlayTrack(const trNo: integer): Boolean;

//==============================================================================
//
// XMI_StopPlayback
//
//==============================================================================
procedure XMI_StopPlayback;

implementation

uses
  d_delphi,
  xmi_core;

var
  XMIfile: string = '';

//==============================================================================
//
// XMI_Init
//
//==============================================================================
procedure XMI_Init;
begin
  XMICore := TXMICore.Create(nil);
end;

//==============================================================================
//
// XMI_ShutDown
//
//==============================================================================
procedure XMI_ShutDown;
begin
  XMICore.Free;
end;

//==============================================================================
//
// XMI_OpenMusicFile
//
//==============================================================================
function XMI_OpenMusicFile(const fname: string): Boolean;
begin
  if fname = '' then
  begin
    Result := False;
    Exit;
  end;
  if not fexists(fname) then
  begin
    Result := False;
    Exit;
  end;

  XMIfile := fname;
  Result := XMICore.LoadFile(fname, '');
  if XMICore.TrkCh.Items.Count > 0 then
  begin
    XMICore.TrkCh.ItemIndex := 0;
    XMICore.FillEvents(XMICore.TrkCh.ItemIndex);
  end;
  XMICore.ChkButtons;
end;

//==============================================================================
//
// XMI_GetNumTracks
//
//==============================================================================
function XMI_GetNumTracks: integer;
begin
  Result := XMICore.TrkCh.Items.Count;
end;

//==============================================================================
//
// XMI_ConvertTrackToFile
//
//==============================================================================
function XMI_ConvertTrackToFile(const trNo: integer; const fname: string): Boolean;
var
  Idx, I: Integer;
begin
  XMI_StopPlayback;
  Result := XMI_OpenMusicFile(XMIfile);
  if not Result then
    Exit;

  LoopEnabled := True;
  Idx := trNo;
  if Idx >= XMICore.TrkCh.Items.Count then
  begin
    Result := False;
    Exit;
  end;

  XMICore.TrkCh.ItemIndex := MaxI(0, Idx);
  XMICore.FillEvents(XMICore.TrkCh.ItemIndex);
  XMICore.ChkButtons;

  if Idx >= 0 then
    for I := Length(TrackData) - 1 downto 0 do
      if I <> Idx then
        XMICore.DelTrack(I);

  XMICore.RefTrackList;
  if Length(TrackData) > 0 then
  begin
    XMICore.TrkCh.ItemIndex := 0;
    XMICore.FillEvents(XMICore.TrkCh.ItemIndex);
  end;

  XMICore.ChkButtons;
  Result := XMICore.SaveFile(fname);
end;

//==============================================================================
//
// XMI_ConvertMemoryToFile
//
//==============================================================================
function XMI_ConvertMemoryToFile(const adata: pointer; const asize: integer; const trNo: integer; const fname: string): Boolean;
var
  Idx, I: Integer;
begin
  XMI_StopPlayback;

  Result := XMICore.LoadMemory(adata, asize, '');
  if not Result then
    Exit;

  if XMICore.TrkCh.Items.Count > 0 then
  begin
    XMICore.TrkCh.ItemIndex := 0;
    XMICore.FillEvents(XMICore.TrkCh.ItemIndex);
  end;
  XMICore.ChkButtons;

  LoopEnabled := True;
  Idx := trNo;
  if Idx >= XMICore.TrkCh.Items.Count then
  begin
    Result := False;
    Exit;
  end;

  XMICore.TrkCh.ItemIndex := MaxI(0, Idx);
  XMICore.FillEvents(XMICore.TrkCh.ItemIndex);
  XMICore.ChkButtons;

  if Idx >= 0 then
    for I := Length(TrackData) - 1 downto 0 do
      if I <> Idx then
        XMICore.DelTrack(I);

  XMICore.RefTrackList;
  if Length(TrackData) > 0 then
  begin
    XMICore.TrkCh.ItemIndex := 0;
    XMICore.FillEvents(XMICore.TrkCh.ItemIndex);
  end;

  XMICore.ChkButtons;
  Result := XMICore.SaveFile(fname);
end;

//==============================================================================
//
// XMI_ConvertTrackToMemory
//
//==============================================================================
function XMI_ConvertTrackToMemory(const trNo: integer; const typ: string; var p: pointer; var sz: integer): Boolean;
var
  Idx, I: Integer;
begin
  XMI_StopPlayback;
  Result := XMI_OpenMusicFile(XMIfile);
  if not Result then
    Exit;

  LoopEnabled := True;
  Idx := trNo;

  XMICore.TrkCh.ItemIndex := MaxI(0, Idx);
  XMICore.FillEvents(XMICore.TrkCh.ItemIndex);
  XMICore.ChkButtons;

  if Idx >= 0 then
    for I := Length(TrackData) - 1 downto 0 do
      if I <> Idx then
        XMICore.DelTrack(I);

  XMICore.RefTrackList;
  if Length(TrackData) > 0 then
  begin
    XMICore.TrkCh.ItemIndex := 0;
    XMICore.FillEvents(XMICore.TrkCh.ItemIndex);
  end;

  XMICore.ChkButtons;
  Result := XMICore.SaveFileToPointer('tmp.' + typ, p, sz);
end;

//==============================================================================
//
// XMI_FreeMem
//
//==============================================================================
procedure XMI_FreeMem(var p: pointer; var sz: integer);
begin
  FreeMem(p, sz);
  p := nil;
end;

//==============================================================================
//
// XMI_PlayTrack
//
//==============================================================================
function XMI_PlayTrack(const trNo: integer): Boolean;
var
  Idx, I: Integer;
begin
  XMI_StopPlayback;
  Result := XMI_OpenMusicFile(XMIfile);
  if not Result then
    Exit;

  LoopEnabled := True;
  Idx := trNo;
  XMICore.TrkCh.ItemIndex := Idx;
  XMICore.FillEvents(XMICore.TrkCh.ItemIndex);
  XMICore.ChkButtons;

  for I := Length(TrackData) - 1 downto 0 do
    if I <> Idx then
      XMICore.DelTrack(I);
  XMICore.RefTrackList;
  if Length(TrackData) > 0 then
  begin
    XMICore.TrkCh.ItemIndex := 0;
    XMICore.FillEvents(XMICore.TrkCh.ItemIndex);
  end;

  XMICore.ChkButtons;
  XMICore.bPlayClick(nil);

  Result := True;
end;

//==============================================================================
//
// XMI_StopPlayback
//
//==============================================================================
procedure XMI_StopPlayback;
begin
  XMICore.bStopClick(nil);
end;

end.
