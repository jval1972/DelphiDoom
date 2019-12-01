unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ExtDlgs, Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit14: TEdit;
    Edit15: TEdit;
    Edit16: TEdit;
    Edit17: TEdit;
    Edit18: TEdit;
    Edit19: TEdit;
    Edit2: TEdit;
    Edit20: TEdit;
    Edit21: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SaveDialog1: TSaveDialog;
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label12Click(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure Label7Click(Sender: TObject);
  private
    { private declarations }
    procedure AddGroup(const E1, E2: TEdit; var cnt: integer);
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Edit1.Text := '';
  Edit2.Text := '';
  Edit3.Text := '';
  Edit4.Text := '';
  Edit5.Text := '';
  Edit6.Text := '';
  Edit7.Text := '';
  Edit8.Text := '';
  Edit9.Text := '';
  Edit10.Text := '';
  Edit11.Text := '';
  Edit12.Text := '';
  Edit13.Text := '';
  Edit14.Text := '';
  Edit15.Text := '';
  Edit16.Text := '';
  Edit17.Text := '';
  Edit18.Text := '';
  Edit19.Text := '';
  Edit20.Text := '';
  Edit21.Text := '';
end;

function EditValue(const e: TEdit): integer;
var
  code: integer;
begin
  val(e.Text, result, code);
  if code > 0 then
     result := -1;
end;

procedure TForm1.AddGroup(const E1, E2: TEdit; var cnt: integer);
var
  i: integer;
begin
  if E1.Text = '' then
     exit;
  if E2.Text = '' then
     exit;
  for i := EditValue(E1) to EditValue(E2) do
  begin
    Memo1.Lines.Add('state S_' + Edit3.Text + IntToStr(cnt) + ' { model "' + Edit1.Text + '" texture "' + Edit2.Text + '" frame ' + IntToStr(i) + '}');
    inc(cnt);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  cnt: integer;
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('modeldef "' + Edit1.Text + '"');
  Memo1.Lines.Add('{');
  Memo1.Lines.Add('  offset 0.0');
  Memo1.Lines.Add('  scale 1.0');
  Memo1.Lines.Add('}');
  Memo1.Lines.Add('');

  cnt := 0;
  AddGroup(Edit4, Edit5, cnt);
  AddGroup(Edit6, Edit7, cnt);
  AddGroup(Edit8, Edit9, cnt);
  AddGroup(Edit10, Edit11, cnt);
  AddGroup(Edit12, Edit13, cnt);
  AddGroup(Edit14, Edit15, cnt);
  AddGroup(Edit16, Edit17, cnt);
  AddGroup(Edit18, Edit19, cnt);
  AddGroup(Edit20, Edit21, cnt);
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    Memo1.Lines.SaveToFile(SaveDialog1.FileName);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Edit1.Text := '';
  Edit2.Text := '';
  Edit3.Text := '';
  Edit4.Text := '';
  Edit5.Text := '';
  Edit6.Text := '';
  Edit7.Text := '';
  Edit8.Text := '';
  Edit9.Text := '';
  Edit10.Text := '';
  Edit11.Text := '';
  Edit12.Text := '';
  Edit13.Text := '';
  Edit14.Text := '';
  Edit15.Text := '';
  Edit16.Text := '';
  Edit17.Text := '';
  Edit18.Text := '';
  Edit19.Text := '';
  Edit20.Text := '';
  Edit21.Text := '';
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Edit1.Text := ExtractFileName(OpenDialog1.FileName);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    Edit2.Text := ExtractFileName(OpenPictureDialog1.FileName);
end;

procedure TForm1.Label12Click(Sender: TObject);
begin

end;

procedure TForm1.Label3Click(Sender: TObject);
begin

end;

procedure TForm1.Label7Click(Sender: TObject);
begin

end;

end.

