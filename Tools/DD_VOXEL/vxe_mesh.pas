//
// JVAL
//  Create optimized mesh from voxel data
//  Good results, but a bit slow for runtime.
//

unit vxe_mesh;

interface

uses
  dglOpenGL,
  Classes,
  voxels;

type
  voxelvertex_t = record
    x, y, z: integer;
  end;
  voxelvertex_p = ^voxelvertex_t;

  voxelquad_t = record
    color: GLuint;
    vertexes: array[0..3] of voxelvertex_t;
    valid: Boolean;
    orientation: Char;
  end;

  voxelquad_p = ^voxelquad_t;
  voxelquad_a = array[0..$FFF] of voxelquad_t;
  voxelquad_pa = ^voxelquad_a;

  TVoxelMeshOptimizer = class
  private
    fquads: voxelquad_pa;
    fnumquads: integer;
    fpasscount: integer;
    frealnumquads: integer;
    fmessage: string;
    fvoxsize: integer;
    fvox: voxelbuffer_p;
    fnumvoxels: integer;
    procedure Grow;
    procedure TryMergeQuads(const q1, q2: voxelquad_p);
    procedure SortQuadVertexes(const q: voxelquad_p);
    procedure InnerOptimize;
  protected
    procedure RenderVertexGL(const v: voxelvertex_p); virtual;
    procedure AddQuad(const c: GLuint; const x0, y0, z0, x1, y1, z1, x2, y2, z2, x3, y3, z3: integer); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadVoxel(const voxsize: Integer; const vox: voxelbuffer_p);
    procedure Optimize;
    procedure RenderGL;
    procedure SaveToFile(const fname: string); virtual;
    procedure SaveToStream(const s: TStream); virtual;
    property quads: voxelquad_pa read fquads;
    property numquads: integer read fnumquads;
    property passcount: Integer read fpasscount write fpasscount;
    property message: string read fmessage;
  end;


implementation

uses
  SysUtils;

constructor TVoxelMeshOptimizer.Create;
begin
  fquads := nil;
  fnumquads := 0;
  frealnumquads := 0;
  fpasscount := 3;
  fmessage := '';
  fvoxsize := 0;
  fnumvoxels := 0;
  GetMem(fvox, SizeOf(voxelbuffer_t));
end;

procedure TVoxelMeshOptimizer.Clear;
begin
  fnumquads := 0;
  fvoxsize := 0;
  fnumvoxels := 0;
end;

destructor TVoxelMeshOptimizer.Destroy;
begin
  FreeMem(fquads, frealnumquads * SizeOf(voxelquad_t));
  FreeMem(fvox, SizeOf(voxelbuffer_t));
end;

procedure TVoxelMeshOptimizer.AddQuad(const c: GLuint; const x0, y0, z0, x1, y1, z1, x2, y2, z2, x3, y3, z3: integer);
var
  q: voxelquad_p;
begin
  Grow;
  q := @fquads[fnumquads - 1];
  q.color := c;
  q.vertexes[0].x := x0;
  q.vertexes[0].y := y0;
  q.vertexes[0].z := z0;
  q.vertexes[1].x := x1;
  q.vertexes[1].y := y1;
  q.vertexes[1].z := z1;
  q.vertexes[2].x := x2;
  q.vertexes[2].y := y2;
  q.vertexes[2].z := z2;
  q.vertexes[3].x := x3;
  q.vertexes[3].y := y3;
  q.vertexes[3].z := z3;
  q.valid := true;
  if (x0 = x1) and (x0 = x2) and (x0 = x3) then
    q.orientation := 'x'
  else if (y0 = y1) and (y0 = y2) and (y0 = y3) then
    q.orientation := 'y'
  else if (z0 = z1) and (z0 = z2) and (z0 = z3) then
    q.orientation := 'z'
  else
  begin
    q.orientation := ' ';
    q.valid := false;
  end;
end;

const
  VXOGROWSTEP = 256;

procedure TVoxelMeshOptimizer.Grow;
begin
  Inc(fnumquads);
  if fnumquads > frealnumquads then
  begin
    ReallocMem(fquads, (frealnumquads + VXOGROWSTEP) * SizeOf(voxelquad_t));
    frealnumquads := frealnumquads + VXOGROWSTEP;
  end;
end;

procedure TVoxelMeshOptimizer.SortQuadVertexes(const q: voxelquad_p);
var
  i, si: integer;
  sorttbl: array[0..3] of integer;
  mid1, mid2: single;
  v: voxelvertex_t;
  vp: voxelvertex_p;
  done: boolean;
begin
  mid1 := 0;
  mid2 := 0;
  if q.orientation = 'x' then
  begin
    for i := 0 to 3 do
    begin
      mid1 := mid1 + q.vertexes[i].y;
      mid2 := mid2 + q.vertexes[i].z;
    end;
    mid1 := mid1 / 4;
    mid2 := mid2 / 4;
    vp := @q.vertexes[0];
    for i := 0 to 3 do
    begin
      if vp.y < mid1 then
      begin
        if vp.z < mid2 then
          sorttbl[i] := 0
        else
          sorttbl[i] := 1;
      end
      else
      begin
        if vp.z < mid2 then
          sorttbl[i] := 3
        else
          sorttbl[i] := 2;
      end;
      inc(vp);
    end;
    repeat
      done := true;
      for i := 0 to 3 do
        if sorttbl[i] <> i then
        begin
          v := q.vertexes[i];
          si := sorttbl[i];
          q.vertexes[i] := q.vertexes[si];
          q.vertexes[si] := v;
          sorttbl[si] := si;
          sorttbl[i] := i;
          done := false;
          break;
        end;
    until done;
  end
  else if q.orientation = 'y' then
  begin
    for i := 0 to 3 do
    begin
      mid1 := mid1 + q.vertexes[i].x;
      mid2 := mid2 + q.vertexes[i].z;
    end;
    mid1 := mid1 / 4;
    mid2 := mid2 / 4;
    vp := @q.vertexes[0];
    for i := 0 to 3 do
    begin
      if vp.x < mid1 then
      begin
        if vp.z < mid2 then
          sorttbl[i] := 0
        else
          sorttbl[i] := 1;
      end
      else
      begin
        if vp.z < mid2 then
          sorttbl[i] := 3
        else
          sorttbl[i] := 2;
      end;
      inc(vp);
    end;
    repeat
      done := true;
      for i := 0 to 3 do
        if sorttbl[i] <> i then
        begin
          v := q.vertexes[i];
          si := sorttbl[i];
          q.vertexes[i] := q.vertexes[si];
          q.vertexes[si] := v;
          sorttbl[si] := si;
          sorttbl[i] := i;
          done := false;
          break;
        end;
    until done;
  end
  else if q.orientation = 'z' then
  begin
    for i := 0 to 3 do
    begin
      mid1 := mid1 + q.vertexes[i].x;
      mid2 := mid2 + q.vertexes[i].y;
    end;
    mid1 := mid1 / 4;
    mid2 := mid2 / 4;
    vp := @q.vertexes[0];
    for i := 0 to 3 do
    begin
      if vp.x < mid1 then
      begin
        if vp.y < mid2 then
          sorttbl[i] := 0
        else
          sorttbl[i] := 1;
      end
      else
      begin
        if vp.y < mid2 then
          sorttbl[i] := 3
        else
          sorttbl[i] := 2;
      end;
      inc(vp);
    end;
    repeat
      done := true;
      for i := 0 to 3 do
        if sorttbl[i] <> i then
        begin
          v := q.vertexes[i];
          si := sorttbl[i];
          q.vertexes[i] := q.vertexes[si];
          q.vertexes[si] := v;
          sorttbl[si] := si;
          sorttbl[i] := i;
          done := false;
          break;
        end;
    until done;
  end;

end;

procedure TVoxelMeshOptimizer.LoadVoxel(const voxsize: Integer; const vox: voxelbuffer_p);
var
  xx, yy, zz: integer;
  xx1, yy1, zz1: integer;
  flags: voxelrenderflags_p;
  b: byte;
  skip_x0, skip_x1: boolean;
  skip_y0, skip_y1: boolean;
  skip_z0, skip_z1: boolean;
  c: LongWord;
begin
  fvoxsize := voxsize;
  GetMem(flags, SizeOf(voxelrenderflags_t));

  for xx := 0 to voxsize - 1 do
    for yy := 0 to voxsize - 1 do
      for zz := 0 to voxsize - 1 do
      begin
        if (xx > 0) and (xx < voxsize - 1) and
           (yy > 0) and (yy < voxsize - 1) and
           (zz > 0) and (zz < voxsize - 1) then
        begin
          b := 0;
          if vox[xx - 1, yy, zz] <> 0 then
            b := FLG_SKIPX0;
          if vox[xx + 1, yy, zz] <> 0 then
            b := b + FLG_SKIPX1;
          if vox[xx, yy - 1, zz] <> 0 then
            b := b + FLG_SKIPY0;
          if vox[xx, yy + 1, zz] <> 0 then
            b := b + FLG_SKIPY1;
          if vox[xx, yy, zz - 1] <> 0 then
            b := b + FLG_SKIPZ0;
          if vox[xx, yy, zz + 1] <> 0 then
            b := b + FLG_SKIPZ1;
          flags[xx, yy, zz] := b;
        end
        else
          flags[xx, yy, zz] := 0;
      end;

  for xx := 0 to voxsize - 1 do
    for yy := 0 to voxsize - 1 do
      for zz := 0 to voxsize - 1 do
      begin
        c := vox[xx, yy, zz];
        if c = 0 then
          continue;
          
        b := flags[xx, yy, zz];
        skip_x0 := b and FLG_SKIPX0 <> 0;
        skip_x1 := b and FLG_SKIPX1 <> 0;
        skip_y0 := b and FLG_SKIPY0 <> 0;
        skip_y1 := b and FLG_SKIPY1 <> 0;
        skip_z0 := b and FLG_SKIPZ0 <> 0;
        skip_z1 := b and FLG_SKIPZ1 <> 0;
        xx1 := xx + 1;
        yy1 := yy + 1;
        zz1 := zz + 1;

        if not skip_z0 then
        begin
          AddQuad(c,
                  xx, yy, zz,
                  xx, yy1, zz,
                  xx1, yy1, zz,
                  xx1, yy, zz);
        end;

        if not skip_y0 then
        begin
          AddQuad(c,
                  xx, yy, zz,
                  xx, yy, zz1,
                  xx1, yy, zz1,
                  xx1, yy, zz);
        end;

        if not skip_x0 then
        begin
          AddQuad(c,
                  xx, yy, zz,
                  xx, yy, zz1,
                  xx, yy1, zz1,
                  xx, yy1, zz);
        end;

        if not skip_z1 then
        begin
          AddQuad(c,
                  xx, yy, zz1,
                  xx, yy1, zz1,
                  xx1, yy1, zz1,
                  xx1, yy, zz1);
        end;

        if not skip_y1 then
        begin
          AddQuad(c,
                  xx, yy1, zz,
                  xx, yy1, zz1,
                  xx1, yy1, zz1,
                  xx1, yy1, zz);
        end;

        if not skip_x1 then
        begin
          AddQuad(c,
                  xx1, yy, zz,
                  xx1, yy1, zz,
                  xx1, yy1, zz1,
                  xx1, yy , zz1);
        end;


      end;

  fnumvoxels := 0;
  for xx := 0 to voxsize - 1 do
    for yy := 0 to voxsize - 1 do
      for zz := 0 to voxsize - 1 do
      begin
        if flags[xx, yy, zz] = FLG_SKIPX0 + FLG_SKIPX1 + FLG_SKIPY0 + FLG_SKIPY1 + FLG_SKIPZ0 + FLG_SKIPZ1 then
          fvox[xx, yy, zz] := 0
        else
        begin
          fvox[xx, yy, zz] := vox[xx, yy, zz];
          Inc(fnumvoxels);
        end;
      end;

  FreeMem(flags, SizeOf(voxelrenderflags_t));

end;

procedure TVoxelMeshOptimizer.Optimize;
var
  cnt: integer;
  oldnumquads: integer;
  passc: integer;
begin
  if fpasscount <= 0 then
  begin
    fpasscount := 0;
    fmessage := 'TVoxelMeshOptimizer.Optimize(): Can not start optimized, vx_maxoptimizerpasscount = 0.'#13#10;
    exit;
  end;

  oldnumquads := fnumquads;
  passc := 0;
  repeat
    cnt := fnumquads;
    InnerOptimize;
    inc(passc);
    if passc = fpasscount then
      break;
  until cnt = fnumquads;
  if oldnumquads > fnumquads then
    fmessage := Format('TVoxelMeshOptimizer.Optimize(): Mesh reduced from %d to %d quads, pass count = %d.'#13#10, [oldnumquads, fnumquads, passc]);
end;

procedure TVoxelMeshOptimizer.InnerOptimize;
var
  i, j: integer;
  q1, q2: voxelquad_p;
  ori: Char;
  c: GLuint;
  fnewquads: voxelquad_pa;
  fnewnumquads: integer;
  fnewrealnumquads: integer;
begin
  for i := 0 to fnumquads - 2 do
  begin
    q1 := @fquads[i];
    if q1.valid then
    begin
      ori := q1.orientation;
      c := q1.color;
      for j := i + 1 to fnumquads - 1 do
      begin
        q2 := @fquads[j];
        if q2.valid then
          if (q2.orientation = ori) and (q2.color = c) then
            TryMergeQuads(q1, q2);
      end;
    end;
  end;

  fnewnumquads := 0;
  for i := 0 to fnumquads - 1 do
    if fquads[i].valid then
      Inc(fnewnumquads);

  fnewrealnumquads := (fnewnumquads + VXOGROWSTEP) and (not (VXOGROWSTEP - 1));

  if fnewrealnumquads = frealnumquads then
  begin
    j := 0;
    for i := 0 to fnumquads - 1 do
    begin
      q1 := @fquads[i];
      if q1.valid then
        if i <> j then
        begin
          fquads[j] := q1^;
          SortQuadVertexes(@fquads[j]);
          Inc(j);
        end;
    end;
  end
  else
  begin
    GetMem(fnewquads, fnewrealnumquads * SizeOf(voxelquad_t));

    q2 := @fnewquads[0];

    for i := 0 to fnumquads - 1 do
    begin
      q1 := @fquads[i];
      if q1.valid then
      begin
        q2^ := q1^;
        SortQuadVertexes(q2);
        Inc(q2);
      end;
    end;

    FreeMem(fquads, frealnumquads * SizeOf(voxelquad_t));
    fquads := fnewquads;
    fnumquads := fnewnumquads;
    frealnumquads := fnewrealnumquads;
  end;
end;

procedure TVoxelMeshOptimizer.TryMergeQuads(const q1, q2: voxelquad_p);
var
  match1, match2: array[0..3] of integer;
  cnt: integer;
  i, j: integer;
  i2a, i2b: integer;
  x, y, z: integer;
  vp: voxelvertex_p;
begin
  for i := 0 to 3 do
  begin
    match1[i] := -1;
    match2[i] := -1;
  end;
  if q1.orientation = 'x' then
  begin
    if q1.vertexes[0].x <> q2.vertexes[0].x then
      exit;
    for i := 0 to 3 do
    begin
      y := q1.vertexes[i].y;
      z := q1.vertexes[i].z;
      vp := @q2.vertexes[0];
      for j := 0 to 3 do
      begin
        if (y = vp.y) and (z = vp.z) then
        begin
          match1[i] := j;
          match2[j] := i;
        end;
        inc(vp);
      end;
    end;

    cnt := 0;
    for i := 0 to 3 do
      if match1[i] = -1 then
        Inc(cnt);
    if cnt = 2 then
    begin
      i2a := -1;
      for i := 0 to 3 do
        if match2[i] = -1 then
        begin
          i2a := i;
          Break;
        end;
      i2b := -1;
      for i := 3 downto 0 do
        if match2[i] = -1 then
        begin
          i2b := i;
          Break;
        end;

      if (i2a <> -1) and (i2a <> i2b) then
      begin
        for i := 0 to 3 do
          if match1[i] <> -1 then
          begin
            q1.vertexes[i].y := q2.vertexes[i2a].y;
            q1.vertexes[i].z := q2.vertexes[i2a].z;
            break;
          end;
        for i := 3 downto 0 do
          if match1[i] <> -1 then
          begin
            q1.vertexes[i].y := q2.vertexes[i2b].y;
            q1.vertexes[i].z := q2.vertexes[i2b].z;
            break;
          end;
        q2.valid := false;
      end;
    end;
  end
  else if q1.orientation = 'y' then
  begin
    if q1.vertexes[0].y <> q2.vertexes[0].y then
      exit;
    for i := 0 to 3 do
    begin
      x := q1.vertexes[i].x;
      z := q1.vertexes[i].z;
      vp := @q2.vertexes[0];
      for j := 0 to 3 do
      begin
        if (x = vp.x) and (z = vp.z) then
        begin
          match1[i] := j;
          match2[j] := i;
        end;
        inc(vp);
      end;
    end;

    cnt := 0;
    for i := 0 to 3 do
      if match1[i] = -1 then
        Inc(cnt);
    if cnt = 2 then
    begin
      i2a := -1;
      for i := 0 to 3 do
        if match2[i] = -1 then
        begin
          i2a := i;
          Break;
        end;
      i2b := -1;
      for i := 3 downto 0 do
        if match2[i] = -1 then
        begin
          i2b := i;
          Break;
        end;

      if (i2a <> -1) and (i2a <> i2b) then
      begin
        for i := 0 to 3 do
          if match1[i] <> -1 then
          begin
            q1.vertexes[i].x := q2.vertexes[i2a].x;
            q1.vertexes[i].z := q2.vertexes[i2a].z;
            break;
          end;
        for i := 3 downto 0 do
          if match1[i] <> -1 then
          begin
            q1.vertexes[i].x := q2.vertexes[i2b].x;
            q1.vertexes[i].z := q2.vertexes[i2b].z;
            break;
          end;
        q2.valid := false;
      end;
    end;
  end
  else if q1.orientation = 'z' then
  begin
    if q1.vertexes[0].z <> q2.vertexes[0].z then
      exit;
    for i := 0 to 3 do
    begin
      x := q1.vertexes[i].x;
      y := q1.vertexes[i].y;
      vp := @q2.vertexes[0];
      for j := 0 to 3 do
      begin
        if (x = vp.x) and (y = vp.y) then
        begin
          match1[i] := j;
          match2[j] := i;
        end;
        inc(vp);
      end;
    end;

    cnt := 0;
    for i := 0 to 3 do
      if match1[i] = -1 then
        Inc(cnt);
    if cnt = 2 then
    begin
      i2a := -1;
      for i := 0 to 3 do
        if match2[i] = -1 then
        begin
          i2a := i;
          Break;
        end;
      i2b := -1;
      for i := 3 downto 0 do
        if match2[i] = -1 then
        begin
          i2b := i;
          Break;
        end;

      if (i2a <> -1) and (i2a <> i2b) then
      begin
        for i := 0 to 3 do
          if match1[i] <> -1 then
          begin
            q1.vertexes[i].x := q2.vertexes[i2a].x;
            q1.vertexes[i].y := q2.vertexes[i2a].y;
            break;
          end;
        for i := 3 downto 0 do
          if match1[i] <> -1 then
          begin
            q1.vertexes[i].x := q2.vertexes[i2b].x;
            q1.vertexes[i].y := q2.vertexes[i2b].y;
            break;
          end;
        q2.valid := false;
      end;
    end;
  end;

end;

procedure TVoxelMeshOptimizer.RenderVertexGL(const v: voxelvertex_p);
begin
  glVertex3f(v.x, v.y, v.z);
end;

procedure TVoxelMeshOptimizer.RenderGL;
var
  i: integer;
  q: voxelquad_p;
  c, oldc: GLuint;
begin
  glBegin(GL_QUADS);

  oldc := $FFFFFFFF;
  for i := 0 to fnumquads - 1 do
  begin
    q := @fquads[i];
    if q.valid then
    begin
      c := q.color;
      if c <> oldc then
      begin
        glColor3b((c and $FF),
                  (c shr 8) and $FF,
                  (c shr 16));
        oldc := c;
      end;

      RenderVertexGL(@q.vertexes[0]);
      RenderVertexGL(@q.vertexes[1]);
      RenderVertexGL(@q.vertexes[2]);
      RenderVertexGL(@q.vertexes[3]);
    end;
  end;

  glEnd;
end;

procedure TVoxelMeshOptimizer.SaveToFile(const fname: string);
var
  f: TFileStream;
begin
  f := TFileStream.Create(fname, fmCreate or fmShareDenyWrite);
  try
    SaveToStream(f);
  finally
    f.Free;
  end;
end;

var
  HDR: LongWord = Ord('D') + Ord('D') shl 8 + Ord('M') shl 16 + Ord('S') shl 24;

procedure TVoxelMeshOptimizer.SaveToStream(const s: TStream);
var
  i, j: integer;
  version: integer;
  b: byte;
  xx, yy, zz: integer;
begin
  s.Write(HDR, SizeOf(LongWord));
  version := 1;
  s.Write(version, SizeOf(integer));
  s.Write(fvoxsize, SizeOf(integer));
  s.Write(fnumquads, SizeOf(integer));

  for i := 0 to fnumquads - 1 do
  begin
    s.Write(fquads[i].color, SizeOf(LongWord));
    for j := 0 to 3 do
      s.Write(fquads[i].vertexes[j], SizeOf(voxelvertex_t));
  end;


  fnumvoxels := 0;
  for xx := 0 to fvoxsize - 1 do
    for yy := 0 to fvoxsize - 1 do
      for zz := 0 to fvoxsize - 1 do
        if fvox[xx, yy, zz] <> 0 then
          Inc(fnumvoxels);

  s.Write(fnumvoxels, SizeOf(integer));

  for xx := 0 to fvoxsize - 1 do
    for yy := 0 to fvoxsize - 1 do
      for zz := 0 to fvoxsize - 1 do
        if fvox[xx, yy, zz] <> 0 then
        begin
          b := xx;
          s.Write(b, SizeOf(byte));
          b := yy;
          s.Write(b, SizeOf(byte));
          b := zz;
          s.Write(b, SizeOf(byte));
          s.Write(fvox[xx, yy, zz], SizeOf(LongWord));
        end;

end;

end.
