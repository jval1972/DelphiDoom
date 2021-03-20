program MikPlay;

uses
  Forms,
  MikPlayF in 'MikPlayF.pas' {MikPlayForm},
  mikmod in '..\..\include\mikmod.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'MikPlay';
  Application.CreateForm(TMikPlayForm, MikPlayForm);
  Application.Run;
end.
