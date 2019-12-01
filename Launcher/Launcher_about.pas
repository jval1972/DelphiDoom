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

end.
