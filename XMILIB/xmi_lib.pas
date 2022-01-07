unit xmi_lib;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

procedure XMI_Init;
procedure XMI_ShutDown;
function XMI_OpenMusicFile(const fname: string): Boolean;
function XMI_GetNumTracks: Integer;
function XMI_ConvertTrackToFile(const trNo: integer; const fname: string): Boolean;
function XMI_ConvertTrackToMemory(const trNo: integer; const typ: string; var p: pointer; var sz: integer): Boolean;
procedure XMI_FreeMem(var p: pointer; var sz: integer);
function XMI_PlayTrack(const trNo: integer): Boolean;
procedure XMI_StopPlayback;

implementation

uses
  d_delphi,
  xmi_core;

var
  XMIfile: string = '';

procedure XMI_Init;
begin
  XMICore := TXMICore.Create(nil);
end;

procedure XMI_ShutDown;
begin
  XMICore.Free;
end;

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

function XMI_GetNumTracks: integer;
begin
  Result := XMICore.TrkCh.Items.Count;
end;

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
  Result := XMICore.SaveFile(fname);
end;

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
  Result := XMICore.SaveFileToPointer('tmp.' + typ, p, sz);
end;

procedure XMI_FreeMem(var p: pointer; var sz: integer);
begin
  FreeMem(p, sz);
  p := nil;
end;

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

procedure XMI_StopPlayback;
begin
  XMICore.bStopClick(nil);
end;

end.
