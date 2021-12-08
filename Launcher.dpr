program Launcher;

uses
  Forms,
  Launcher_main in 'Launcher\Launcher_main.pas' {Form1},
  Launcher_utils in 'Launcher\Launcher_utils.pas',
  Launcher_wads in 'Launcher\Launcher_wads.pas',
  Launcher_defs in 'Launcher\Launcher_defs.pas',
  Launcher_about in 'Launcher\Launcher_about.pas' {AboutForm},
  Launcher_options in 'Launcher\Launcher_options.pas' {OptionsForm},
  Launcher_gameproperties in 'Launcher\Launcher_gameproperties.pas' {GamePropertiesForm};

{$R *.res}

begin
  //I_SetDPIAwareness;

  Application.Initialize;
  Application.Title := 'DelphiDoom Launcher';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
