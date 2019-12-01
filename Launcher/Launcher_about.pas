unit Launcher_about;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, XPMan;

type
  TAboutForm = class(TForm)
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Image1: TImage;
    XPManifest1: TXPManifest;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TAboutForm.Button2Click(Sender: TObject);
begin
  Close;
end;

function I_VersionBuilt(fname: string = ''): string;
var
  vsize: LongWord;
  zero: LongWord;
  buffer: PByteArray;
  res: pointer;
  len: LongWord;
  i: integer;
begin
  if fname = '' then
    fname := ParamStr(0);
  vsize := GetFileVersionInfoSize(PChar(fname), zero);
  if vsize = 0 then
  begin
    result := '';
    exit;
  end;

  GetMem(buffer, vsize + 1);
  GetFileVersionInfo(PChar(fname), 0, vsize, buffer);
  VerQueryValue(buffer, '\StringFileInfo\040904E4\FileVersion', res, len);
  result := '';
  for i := 0 to len - 1 do
  begin
    if PChar(res)^ = #0 then
      break;
    result := result + PChar(res)^;
    res := pointer(integer(res) + 1);
  end;
  FreeMem(pointer(buffer), vsize + 1);
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  Label1.Caption := 'DelphiDoom Launcher version ' + I_VersionBuilt;
end;

end.
