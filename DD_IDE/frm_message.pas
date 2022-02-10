unit frm_message;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmMessage = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    Button1: TButton;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

//==============================================================================
//
// IDEMessage
//
//==============================================================================
procedure IDEMessage(const x: string);

implementation

{$R *.dfm}

//==============================================================================
//
// TfrmMessage.FormCreate
//
//==============================================================================
procedure TfrmMessage.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
end;

//==============================================================================
//
// IDEMessage
//
//==============================================================================
procedure IDEMessage(const x: string);
var
  s: TStringList;
  f: TfrmMessage;
begin
  s := TStringList.Create;
  s.Text := x;
  while s.Count < 6 do
    s.Add('');
  f := TfrmMessage.Create(nil);
  try
    f.Label1.Caption := s.Strings[0];
    f.Label2.Caption := s.Strings[1];
    f.Label3.Caption := s.Strings[2];
    f.Label4.Caption := s.Strings[3];
    f.Label5.Caption := s.Strings[4];
    f.Label6.Caption := s.Strings[5];
    f.ShowModal;
  finally
    f.Free;
  end;
  s.Free;
end;

end.
