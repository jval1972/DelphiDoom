program DD_VOXEL;

uses
  FastMM4 in 'FastMM4.pas',
  FastMM4Messages in 'FastMM4Messages.pas',
  Forms,
  main in 'main.pas' {Form1},
  newfrm in 'newfrm.pas' {NewForm},
  voxels in 'voxels.pas',
  vxe_gl in 'vxe_gl.pas',
  vxe_system in 'vxe_system.pas',
  vxe_script in 'vxe_script.pas',
  vxe_rotate in 'vxe_rotate.pas',
  vxe_undo in 'vxe_undo.pas',
  vxe_binary in 'vxe_binary.pas',
  vxe_defs in 'vxe_defs.pas',
  optionsfrm in 'optionsfrm.pas' {OptionsForm},
  vxe_mesh in 'vxe_mesh.pas',
  kvx2mesh in 'kvx2mesh.pas',
  vxe_kvx in 'vxe_kvx.pas',
  progressfrm in 'progressfrm.pas' {ProgressForm},
  ddvox2mesh in 'ddvox2mesh.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
