(*  MikWin player example for Delphi
	(c) 1999 Jörg Mensmann

	This program is free software; you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation; either version 2 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

        Last edited 15.02.99
*)
unit MikPlayF;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TMikPlayForm = class(TForm)
    bPlay: TButton;
    PlayTimer: TTimer;
    bStop: TButton;
    bOpen: TButton;
    lFilename: TLabel;
    OpenDialog: TOpenDialog;
    bPause: TButton;
    procedure bPlayClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PlayTimerTimer(Sender: TObject);
    procedure bStopClick(Sender: TObject);
    procedure bPauseClick(Sender: TObject);
    procedure bOpenClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    procedure LoadModule(Filename : String);
  end;

var
  MikPlayForm: TMikPlayForm;

implementation

{$R *.DFM}

uses MikMod;

const
  Titel = 'MikPlay';

procedure TMikPlayForm.FormShow(Sender: TObject);
begin
  if not MikWin_Init(44100, True, True, True, Handle, 0) then
        raise Exception.Create(MikWin_GetErrorText);
  if ParamCount > 0 then begin
    LoadModule(ParamStr(1));
    bPlayClick(Sender);
  end;
end;

procedure TMikPlayForm.FormDestroy(Sender: TObject);
begin
  PlayTimer.Enabled := False;
  MikWin_Free;
end;

procedure TMikPlayForm.LoadModule(Filename : String);
begin
  Screen.Cursor := crHourglass;
  PlayTimer.Enabled := False;
  bPlay.Enabled := False;
  bStop.Enabled := False; bPause.Enabled := False;
  Caption := Titel;
  try
    lFilename.Caption := '<Nothing loaded>';
    if not MikWin_Load(PChar(Filename)) then
      raise Exception.Create(MikWin_GetErrorText);
    lFilename.Caption := PChar(MikWin_GetModule^);
    bPlay.Enabled := True; bStop.Enabled := False; bPause.Enabled := False;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMikPlayForm.bPlayClick(Sender: TObject);
begin
  MikWin_Play(False);
  PlayTimer.Enabled := True;
  bPlay.Enabled := False;
  bStop.Enabled := True; bPause.Enabled := True;
end;

procedure TMikPlayForm.PlayTimerTimer(Sender: TObject);
begin
  MikWin_Update;
  bPlay.Enabled := not MikWin_Playing;
  bStop.Enabled := not bPlay.Enabled;
  bPause.Enabled := not bPlay.Enabled;
end;

procedure TMikPlayForm.bStopClick(Sender: TObject);
begin
  MikWin_Stop;
  Caption := Titel;
end;

procedure TMikPlayForm.bPauseClick(Sender: TObject);
begin
  MikWin_Pause;
  if MikWin_Paused then Caption := Titel + ' (Paused)'
    else Caption := Titel;
end;

procedure TMikPlayForm.bOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    LoadModule(OpenDialog.FileName);
end;

procedure TMikPlayForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close;
end;

end.
