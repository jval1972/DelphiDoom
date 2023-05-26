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
//  StartUp Form
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit i_startup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TStartUpConsoleForm = class(TForm)
    Memo1: TMemo;
    GamePanel: TPanel;
    GameLabel: TLabel;
    StartUpProgressBar: TProgressBar;
    StartUpProgressBar2: TProgressBar;
    NetPanel: TPanel;
    NetMsgLabel: TLabel;
    AbortNetButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure AbortNetButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

//==============================================================================
//
// SUC_Open
//
//==============================================================================
procedure SUC_Open;

//==============================================================================
//
// SUC_Close
//
//==============================================================================
procedure SUC_Close;

//==============================================================================
//
// SUC_Outproc
//
//==============================================================================
procedure SUC_Outproc(const s: string);

//==============================================================================
//
// SUC_Progress
//
//==============================================================================
procedure SUC_Progress(const p: integer);

//==============================================================================
//
// SUC_SetGameMode
//
//==============================================================================
procedure SUC_SetGameMode(const s: string);

//==============================================================================
//
// SUC_GetHandle
//
//==============================================================================
function SUC_GetHandle: integer;

//==============================================================================
//
// SUC_Enable
//
//==============================================================================
procedure SUC_Enable;

//==============================================================================
//
// SUC_Disable
//
//==============================================================================
procedure SUC_Disable;

//==============================================================================
//
// SUC_SecondaryProgressInit
//
//==============================================================================
procedure SUC_SecondaryProgressInit(const p: integer);

//==============================================================================
//
// SUC_SecondaryProgressDone
//
//==============================================================================
procedure SUC_SecondaryProgressDone;

//==============================================================================
//
// SUC_SecondaryProgress
//
//==============================================================================
procedure SUC_SecondaryProgress(const p: integer);

//==============================================================================
//
// SUC_StartingNetwork
//
//==============================================================================
procedure SUC_StartingNetwork(const msg: string);

//==============================================================================
//
// SUC_FinishedNetwork
//
//==============================================================================
procedure SUC_FinishedNetwork;

implementation

{$R *.dfm}

uses
  d_delphi,
  d_main,
  i_io,
  i_system;

var
  StartUpConsoleForm: TStartUpConsoleForm;
  startupformactive: boolean = false;
  suc_enabled: boolean = true;

//==============================================================================
//
// SUC_Open
//
//==============================================================================
procedure SUC_Open;
begin
  Screen.Cursor := crHourGlass;
  StartUpConsoleForm := TStartUpConsoleForm.Create(nil);
  StartUpConsoleForm.Show;
  startupformactive := true;
end;

//==============================================================================
//
// SUC_Close
//
//==============================================================================
procedure SUC_Close;
begin
  if startupformactive then
  begin
    startupformactive := false;
    StartUpConsoleForm.Free;
    Screen.Cursor := crDefault;
  end;
end;

var
  suc_wasdisabled: boolean = false;

//==============================================================================
//
// SUC_Outproc
//
//==============================================================================
procedure SUC_Outproc(const s: string);
var
  s1: string;
  i, j: integer;
begin
  SetLength(s1, Length(s));
  j := 0;
  for i := 1 to Length(s) do
    if not (s[i] in [#8, #10]) then
    begin
      inc(j);
      s1[j] := s[i];
    end;
  SetLength(s1, j);
  I_IOprintf(s1);

  if suc_enabled then
  begin
    StartUpConsoleForm.Memo1.Lines.BeginUpdate;

    if suc_wasdisabled then
    begin
      StartUpConsoleForm.Memo1.Lines.Text := stdoutbuffer.Text;
      suc_wasdisabled := false;
    end
    else
    begin
      if StartUpConsoleForm.Memo1.Lines.Count > 0 then
      begin
        i := StartUpConsoleForm.Memo1.Lines.Count - 1;
        StartUpConsoleForm.Memo1.Lines.Delete(i);
        StartUpConsoleForm.Memo1.Lines.Add(stdoutbuffer[i]);
      end;
      i := StartUpConsoleForm.Memo1.Lines.Count;
      while i < stdoutbuffer.Count do
      begin
        StartUpConsoleForm.Memo1.Lines.Add(stdoutbuffer[i]);
        inc(i);
      end;
    end;

    StartUpConsoleForm.Memo1.Lines.EndUpdate;

    StartUpConsoleForm.Memo1.SelStart := Length(StartUpConsoleForm.Memo1.Lines.Text);
    StartUpConsoleForm.Memo1.SelLength := 0;
  end
  else if not suc_wasdisabled then
  begin
    suc_wasdisabled := true;
    StartUpConsoleForm.Memo1.SelStart := Length(StartUpConsoleForm.Memo1.Lines.Text);
    StartUpConsoleForm.Memo1.SelLength := 0;
  end;

end;

//==============================================================================
//
// SUC_Progress
//
//==============================================================================
procedure SUC_Progress(const p: integer);
begin
  StartUpConsoleForm.StartUpProgressBar.Position := p;
  StartUpConsoleForm.StartUpProgressBar.Repaint;
end;

var
  gamemodeset: boolean = false;

//==============================================================================
//
// SUC_SetGameMode
//
//==============================================================================
procedure SUC_SetGameMode(const s: string);
begin
  if not gamemodeset then
  begin
    gamemodeset := true;
    StartUpConsoleForm.GameLabel.Caption := s;
    StartUpConsoleForm.GamePanel.Visible := true;
    StartUpConsoleForm.GamePanel.Repaint;
  end;
end;

//==============================================================================
//
// SUC_GetHandle
//
//==============================================================================
function SUC_GetHandle: integer;
begin
  if startupformactive then
    result := StartUpConsoleForm.Handle
  else
    result := 0;
end;

//==============================================================================
//
// TStartUpConsoleForm.FormCreate
//
//==============================================================================
procedure TStartUpConsoleForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  Caption := D_Version + ' - ' + D_VersionBuilt;

  DoubleBuffered := True;
  for i := 0 to ComponentCount - 1 do
    if Components[i].InheritsFrom(TWinControl) then
      if not (Components[i] is TListBox) then
        (Components[i] as TWinControl).DoubleBuffered := True;
end;

//==============================================================================
//
// TStartUpConsoleForm.AbortNetButtonClick
//
//==============================================================================
procedure TStartUpConsoleForm.AbortNetButtonClick(Sender: TObject);
begin
  NetPanel.Visible := False;
  I_Error('D_CheckAbort(): Network game synchronization aborted.');
end;

//==============================================================================
//
// SUC_Enable
//
//==============================================================================
procedure SUC_Enable;
begin
  suc_enabled := true;
end;

//==============================================================================
//
// SUC_Disable
//
//==============================================================================
procedure SUC_Disable;
begin
  suc_enabled := false;
end;

var
  suc_progress2parm: integer;

//==============================================================================
//
// SUC_SecondaryProgressInit
//
//==============================================================================
procedure SUC_SecondaryProgressInit(const p: integer);
begin
  if p = 0 then
    exit;

  suc_progress2parm := p;
  StartUpConsoleForm.StartUpProgressBar2.Visible := true;
  StartUpConsoleForm.StartUpProgressBar2.Position := 0;
  StartUpConsoleForm.StartUpProgressBar2.Repaint;
end;

//==============================================================================
//
// SUC_SecondaryProgressDone
//
//==============================================================================
procedure SUC_SecondaryProgressDone;
begin
  StartUpConsoleForm.StartUpProgressBar2.Visible := false;
  StartUpConsoleForm.Repaint;
  suc_progress2parm := 0;
end;

//==============================================================================
//
// SUC_SecondaryProgress
//
//==============================================================================
procedure SUC_SecondaryProgress(const p: integer);
var
  newpos: integer;
begin
  if suc_progress2parm = 0 then
    exit;

  newpos := round(p * StartUpConsoleForm.StartUpProgressBar2.Max / suc_progress2parm);
  if newpos > StartUpConsoleForm.StartUpProgressBar2.Max then
    newpos := StartUpConsoleForm.StartUpProgressBar2.Max;
  if newpos <> StartUpConsoleForm.StartUpProgressBar2.Position then
  begin
    StartUpConsoleForm.StartUpProgressBar2.Position := newpos;
    StartUpConsoleForm.StartUpProgressBar2.Repaint;
  end;
end;

//==============================================================================
//
// SUC_StartingNetwork
//
//==============================================================================
procedure SUC_StartingNetwork(const msg: string);
begin
  printf(msg + #13#10);
  StartUpConsoleForm.NetMsgLabel.Caption := msg;
  StartUpConsoleForm.NetPanel.Visible := True;
end;

//==============================================================================
//
// SUC_FinishedNetwork
//
//==============================================================================
procedure SUC_FinishedNetwork;
begin
  StartUpConsoleForm.NetPanel.Visible := False;
end;

end.
