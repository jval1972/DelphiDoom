unit ddvox2mesh;

interface

function ConvertDDVOX2DDMESH(const sf, st: string): boolean;

implementation

uses
  vxe_mesh,
  voxels,
  SysUtils,
  Classes,
  vxe_script;

function ConvertDDVOX2DDMESH(const sf, st: string): boolean;
var
  xx, yy, zz: integer;
  vmo: TVoxelMeshOptimizer;
  fvoxelsize: integer;
  voxelbuffer: voxelbuffer_p;
  buf: TStringList;
  sc: TScriptEngine;
begin
  if not FileExists(sf) then
  begin
    Result := false;
    exit;
  end;

  GetMem(voxelbuffer, SizeOf(voxelbuffer_t));
  FillChar(voxelbuffer^, SizeOf(voxelbuffer_t), Chr(0));

  buf := TStringList.Create;
  try
    buf.LoadFromFile(sf);
    sc := TScriptEngine.Create(buf.Text);
    sc.MustGetInteger;
    fvoxelsize := sc._Integer;
    xx := 0;
    yy := 0;
    zz := 0;
    while sc.GetString do
    begin
      if sc.MatchString('skip') then
      begin
        sc.MustGetInteger;
        inc(zz, sc._Integer);
      end
      else
      begin
        sc.UnGet;
        sc.MustGetInteger;
        voxelbuffer[xx, yy, zz] := sc._Integer;
        Inc(zz);
      end;
      if zz = fvoxelsize then
      begin
        zz := 0;
        Inc(yy);
        if yy = fvoxelsize then
        begin
          yy := 0;
          Inc(xx);
          if xx = fvoxelsize then
            Break;
        end;
      end;
    end;
    sc.Free;
  finally
    buf.Free;
  end;

  vmo := TVoxelMeshOptimizer.Create;
  vmo.LoadVoxel(fvoxelsize, voxelbuffer);
  vmo.Optimize;
  vmo.SaveToFile(st);
  vmo.Free;

  FreeMem(voxelbuffer, SizeOf(voxelbuffer_t));

  result := True;

end;

end.
