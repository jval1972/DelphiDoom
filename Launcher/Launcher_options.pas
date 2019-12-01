unit Launcher_options;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, ImgList;

type
  TOptionsForm = class(TForm)
    Panel1: TPanel;
    RunDelphiDoomButton: TButton;
    Button2: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    DelphiDoomDirEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    DelphiHereticDirEdit: TEdit;
    Label3: TLabel;
    DelphiHexenDirEdit: TEdit;
    DelphiDoomDirButton: TButton;
    DelphiHereticDirButton: TButton;
    DelphiHexenDirButton: TButton;
    TabSheet2: TTabSheet;
    ListView1: TListView;
    IconsImageList: TImageList;
    Label4: TLabel;
    DelphiStrifeDirEdit: TEdit;
    DelphiStrifeDirButton: TButton;
    procedure DelphiDoomDirButtonClick(Sender: TObject);
    procedure DelphiHereticDirButtonClick(Sender: TObject);
    procedure DelphiHexenDirButtonClick(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DelphiStrifeDirButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function OptionsFormDlg: boolean;

implementation

{$R *.dfm}

uses
  FileCtrl,
  Launcher_defs,
  Launcher_gameproperties;

function OptionsFormDlg: boolean;
var
  frm: TOptionsForm;
  i: integer;
  idx: integer;
  item: TListItem;
begin
  result := false;
  frm := TOptionsForm.Create(nil);
  try
    frm.DelphiDoomDirEdit.Text := gamepaths[Ord(ge_doom)];
    frm.DelphiHereticDirEdit.Text := gamepaths[Ord(ge_heretic)];
    frm.DelphiHexenDirEdit.Text := gamepaths[Ord(ge_hexen)];
    frm.DelphiStrifeDirEdit.Text := gamepaths[Ord(ge_strife)];

    frm.ListView1.Items.Clear;
    idx := 0;
    for i := 0 to Ord(NUMGAMETYPES) - 1 do
    begin
      item := frm.ListView1.Items.Insert(idx);
      item.Caption := gameinfo[idx].description;
      item.ImageIndex := Ord(gameinfo[idx].gameengine);
      inc(idx);
    end;

    frm.ShowModal;
    if frm.ModalResult = mrOK then
    begin
      gamepaths[Ord(ge_doom)] := frm.DelphiDoomDirEdit.Text;
      gamepaths[Ord(ge_heretic)] := frm.DelphiHereticDirEdit.Text;
      gamepaths[Ord(ge_hexen)] := frm.DelphiHexenDirEdit.Text;
      gamepaths[Ord(ge_strife)] := frm.DelphiStrifeDirEdit.Text;
      result := true;
    end;
  finally
    frm.Free;
  end;
end;

procedure TOptionsForm.DelphiDoomDirButtonClick(Sender: TObject);
var
  s: string;
begin
  if SelectDirectory('Choose DelphiDoom directory', '', s) then
    DelphiDoomDirEdit.Text := s;
end;

procedure TOptionsForm.DelphiHereticDirButtonClick(Sender: TObject);
var
  s: string;
begin
  if SelectDirectory('Choose DelphiHeretic directory', '', s) then
    DelphiHereticDirEdit.Text := s;
end;

procedure TOptionsForm.DelphiHexenDirButtonClick(Sender: TObject);
var
  s: string;
begin
  if SelectDirectory('Choose DelphiHexen directory', '', s) then
    DelphiHexenDirEdit.Text := s;
end;

procedure TOptionsForm.ListView1DblClick(Sender: TObject);
var
  idx: integer;
begin
  if ListView1.Selected <> nil then
  begin
    idx := ListView1.Selected.Index;
    if (idx >= 0) and (idx < Ord(NUMGAMETYPES)) then
    begin
      if GamePropertiesDialog(@gameinfo[idx]) then
      begin
        ListView1.Items[idx].Caption := gameinfo[idx].description;
        ListView1.Items[idx].ImageIndex := Ord(gameinfo[idx].gameengine);
      end;
      exit;
    end;
  end;
  MessageBeep(0);
end;

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
end;

procedure TOptionsForm.DelphiStrifeDirButtonClick(Sender: TObject);
var
  s: string;
begin
  if SelectDirectory('Choose DelphiStrife directory', '', s) then
    DelphiStrifeDirEdit.Text := s;
end;

end.
