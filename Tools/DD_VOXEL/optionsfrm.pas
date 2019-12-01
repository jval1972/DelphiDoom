unit optionsfrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TOptionsForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.dfm}

uses
  vxe_defs;


procedure TOptionsForm.Button1Click(Sender: TObject);
begin
  opt_useglpixels := CheckBox1.Checked;
end;

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  CheckBox1.Checked := opt_useglpixels;
end;

end.
