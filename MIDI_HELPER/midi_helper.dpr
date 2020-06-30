program midi_helper;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'DelphiDoom Midi Device Helper';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
