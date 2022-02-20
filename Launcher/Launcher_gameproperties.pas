unit Launcher_gameproperties;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Launcher_defs;

type
  TGamePropertiesForm = class(TForm)
    Panel1: TPanel;
    RunDelphiDoomButton: TButton;
    Button2: TButton;
    DescriptionEdit: TEdit;
    Label1: TLabel;
    MainWADEdit: TEdit;
    Label2: TLabel;
    SelectMainWADButton: TButton;
    SelectPWADsButton: TButton;
    PWADEdit: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    GameEngineComboBox: TComboBox;
    OpenPWADDialog: TOpenDialog;
    OpenMainWADDialog: TOpenDialog;
    AdditionalParamsEdit: TEdit;
    Label5: TLabel;
    procedure SelectMainWADButtonClick(Sender: TObject);
    procedure SelectPWADsButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function GamePropertiesDialog(gi: Pgameinfo_t): boolean;

implementation

{$R *.dfm}

function GamePropertiesDialog(gi: Pgameinfo_t): boolean;
var
  frm: TGamePropertiesForm;
begin
  result := false;
  frm := TGamePropertiesForm.Create(nil);
  try
    frm.DescriptionEdit.Text := gi.description;
    frm.MainWADEdit.Text := gi.mainwad;
    frm.PWADEdit.Text := gi.pwad;
    frm.AdditionalParamsEdit.Text := gi.extracmdline;
    if Ord(gi.gameengine) < frm.GameEngineComboBox.Items.Count then
      frm.GameEngineComboBox.ItemIndex := Ord(gi.gameengine)
    else
      frm.GameEngineComboBox.ItemIndex := 0;
    frm.ShowModal;
    if frm.ModalResult = mrOK then
    begin
      result := true;
      gi.description := frm.DescriptionEdit.Text;
      gi.mainwad := frm.MainWADEdit.Text;
      gi.pwad := frm.PWADEdit.Text;
      gi.extracmdline := frm.AdditionalParamsEdit.Text;
      gi.gameengine := gameengine_t(frm.GameEngineComboBox.ItemIndex);

    end;
  finally
    frm.Free;
  end;
end;

procedure TGamePropertiesForm.SelectMainWADButtonClick(Sender: TObject);
begin
  if OpenMainWADDialog.Execute then
    MainWADEdit.Text := OpenMainWADDialog.FileName;
end;

procedure TGamePropertiesForm.SelectPWADsButtonClick(Sender: TObject);
var
  i: integer;
  s: string;
begin
  if OpenPWADDialog.Execute then
  begin
    if OpenPWADDialog.Files.Count = 1 then
      PWADEdit.Text := OpenPWADDialog.FileName
    else
    begin
      s := '';
      for i := 0 to OpenPWADDialog.Files.Count - 1 do
        s := '"' + OpenPWADDialog.Files.Strings[i] + '" ';
      PWADEdit.Text := Trim(s);
    end;
  end;
end;

end.
