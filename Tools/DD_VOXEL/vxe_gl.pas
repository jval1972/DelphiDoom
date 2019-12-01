unit vxe_gl;

interface

uses
  Windows,
  dglOpenGL,
  voxels;

var
  gld_max_texturesize: integer = 0;
  gl_tex_format: integer = GL_RGBA8;
  gl_tex_filter: integer = GL_LINEAR;

procedure glInit;

procedure ResetCamera(const axis: Char);

procedure glBeginScene(const Width, Height: integer);
procedure glEndScene(dc: HDC);
procedure glRenderAxes(const voxsize: Integer; const level: string);
procedure glRenderVoxel_Boxes(const voxsize: Integer; const vox: voxelbuffer_p);
procedure glRenderVoxel_Points(const voxsize: Integer; const vox: voxelbuffer_p; const windowsize: integer);

type
  TCDCamera = record
    x, y, z: glfloat;
    ax, ay, az: glfloat;
  end;

var
  camera: TCDCamera;

implementation

uses
  SysUtils,
  Math;

procedure ResetCamera(const axis: Char);
begin
  camera.x := 0.0;
  camera.y := 0.0;
  camera.z := -3.0;
  camera.ax := 0.0;
  camera.ay := 0.0;
  camera.az := 0.0;
end;


{------------------------------------------------------------------}
{  Initialise OpenGL                                               }
{------------------------------------------------------------------}
procedure glInit;
begin
  glClearColor(0.0, 0.0, 0.0, 0.0);   // Black Background
  glShadeModel(GL_SMOOTH);            // Enables Smooth Color Shading
  glClearDepth(1.0);                  // Depth Buffer Setup
  glEnable(GL_DEPTH_TEST);            // Enable Depth Buffer
  glDepthFunc(GL_LESS);		            // The Type Of Depth Test To Do
  glEnable(GL_POINT_SIZE);

  glGetIntegerv(GL_MAX_TEXTURE_SIZE, @gld_max_texturesize);

  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);   //Realy Nice perspective calculations
  glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);
  glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
  glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);


end;

procedure infinitePerspective(fovy: GLdouble; aspect: GLdouble; znear: GLdouble);
var
  left, right, bottom, top: GLdouble;
  m: array[0..15] of GLdouble;
begin
  top := znear * tan(fovy * pi / 360.0);
  bottom := -top;
  left := bottom * aspect;
  right := top * aspect;

  m[ 0] := (2 * znear) / (right - left);
  m[ 4] := 0;
  m[ 8] := (right + left) / (right - left);
  m[12] := 0;

  m[ 1] := 0;
  m[ 5] := (2 * znear) / (top - bottom);
  m[ 9] := (top + bottom) / (top - bottom);
  m[13] := 0;

  m[ 2] := 0;
  m[ 6] := 0;
  m[10] := -1;
  m[14] := -2 * znear;

  m[ 3] := 0;
  m[ 7] := 0;
  m[11] := -1;
  m[15] := 0;

  glMultMatrixd(@m);
end;

procedure glBeginScene(const Width, Height: integer);
begin
  glDisable(GL_CULL_FACE);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;

  infinitePerspective(64.0, width / height, 0.01);

  glMatrixMode(GL_MODELVIEW);

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);    // Clear The Screen And The Depth Buffer
  glLoadIdentity;                                       // Reset The View

  glTranslatef(camera.x, camera.y, camera.z);
  glRotatef(camera.az, 0, 0, 1);
  glRotatef(camera.ay, 0, 1, 0);
  glRotatef(camera.ax, 1, 0, 0);
end;

procedure glEndScene(dc: HDC);
begin
  SwapBuffers(dc);                                // Display the scene
end;

var
  step: single;
  mid: single;

function xCoord(const x, y, z: Integer): Single;
begin
  result := mid - x * step;
end;

function yCoord(const x, y, z: Integer): Single;
begin
  result := y * step - mid;
end;

function zCoord(const x, y, z: Integer): Single;
begin
  result := z * step - mid;
end;

procedure glRenderAxes(const voxsize: Integer; const level: string);
const
  DRUNIT = 0.5;
var
  c: char;
  s: string;
  pos, xpos, ypos, zpos: integer;
  i: integer;
  xx, yy, zz: integer;
begin
  glColor3f(1.0, 1.0, 1.0);
  glBegin(GL_LINE_STRIP);
    glVertex3f(-DRUNIT, -DRUNIT, -DRUNIT);
    glVertex3f(-DRUNIT,  DRUNIT, -DRUNIT);
    glVertex3f( DRUNIT,  DRUNIT, -DRUNIT);
    glVertex3f( DRUNIT, -DRUNIT, -DRUNIT);
    glVertex3f(-DRUNIT, -DRUNIT, -DRUNIT);
  glEnd;

  glBegin(GL_LINE_STRIP);
    glVertex3f(-DRUNIT, -DRUNIT,  DRUNIT);
    glVertex3f(-DRUNIT,  DRUNIT,  DRUNIT);
    glVertex3f( DRUNIT,  DRUNIT,  DRUNIT);
    glVertex3f( DRUNIT, -DRUNIT,  DRUNIT);
    glVertex3f(-DRUNIT, -DRUNIT,  DRUNIT);
  glEnd;

  glBegin(GL_LINE_STRIP);
    glVertex3f(-DRUNIT, -DRUNIT, -DRUNIT);
    glVertex3f(-DRUNIT, -DRUNIT,  DRUNIT);
    glVertex3f( DRUNIT, -DRUNIT,  DRUNIT);
    glVertex3f( DRUNIT, -DRUNIT, -DRUNIT);
    glVertex3f(-DRUNIT, -DRUNIT, -DRUNIT);
  glEnd;

  glBegin(GL_LINE_STRIP);
    glVertex3f(-DRUNIT,  DRUNIT, -DRUNIT);
    glVertex3f(-DRUNIT,  DRUNIT,  DRUNIT);
    glVertex3f( DRUNIT,  DRUNIT,  DRUNIT);
    glVertex3f( DRUNIT,  DRUNIT, -DRUNIT);
    glVertex3f(-DRUNIT,  DRUNIT, -DRUNIT);
  glEnd;

  glBegin(GL_LINES);
    glVertex3f(0.0, 2 * DRUNIT, 0.0);
    glVertex3f(0.0, -2 * DRUNIT, 0.0);
    glVertex3f(2 * DRUNIT, 0.0, 0.0);
    glVertex3f(-2 * DRUNIT, 0.0, 0.0);
    glVertex3f(0.0, 0.0, 2 * DRUNIT);
    glVertex3f(0.0, 0.0, -2 * DRUNIT);
  glEnd;


  s := Trim(level);
  if s = '' then
    exit;

  c := s[1];
  s[1] := ' ';
  pos := StrToIntDef(s, 0);
  if c = 'x' then
  begin
    xpos := pos;
    ypos := -1;
    zpos := -1;
  end
  else if c = 'y' then
  begin
    xpos := -1;
    ypos := pos;
    zpos := -1;
  end
  else if c = 'z' then
  begin
    xpos := -1;
    ypos := -1;
    zpos := pos;
  end
  else
    exit;

  step := -1.0 / voxsize;
  mid := step * voxsize / 2;

  glColor3f(1.0, 1.0, 0.0);
  glBegin(GL_LINES);
    if xpos >= 0 then
    begin
      for xx := xpos to xpos + 1 do
      begin
        for i := 0 to voxsize do
        begin
          glVertex3f(xCoord(xx, -1, i),  yCoord(xx, -1, i),  zCoord(xx, -1, i));
          glVertex3f(xCoord(xx, voxsize + 1, i),  yCoord(xx, voxsize + 1, i),  zCoord(xx, voxsize + 1, i));
        end;
        for i := 0 to voxsize do
        begin
          glVertex3f(xCoord(xx, i, -1),  yCoord(xx, i, -1),  zCoord(xx, i, -1));
          glVertex3f(xCoord(xx, i, voxsize + 1),  yCoord(xx, i, voxsize + 1),  zCoord(xx, i, voxsize + 1));
        end;
      end;
    end;

    if ypos >= 0 then
    begin
      for yy := ypos to ypos + 1 do
      begin
        for i := 0 to voxsize do
        begin
          glVertex3f(xCoord(-1, yy, i),  yCoord(-1, yy, i), zCoord(-1, yy, i));
          glVertex3f(xCoord(voxsize + 1, yy, i),  yCoord(voxsize + 1, yy, i), zCoord(voxsize + 1, yy, i));
        end;
        for i := 0 to voxsize do
        begin
          glVertex3f(xCoord(i, yy, -1),  yCoord(i, yy, -1), zCoord(i, yy, -1));
          glVertex3f(xCoord(i, yy, voxsize + 1),  yCoord(i, yy, voxsize + 1), zCoord(i, yy, voxsize + 1));
        end;
      end;
    end;

    if zpos >= 0 then
    begin
      for zz := zpos to zpos + 1 do
      begin
        for i := 0 to voxsize do
        begin
          glVertex3f(xCoord(-1, i, zz),  yCoord(-1, i, zz), zCoord(-1, i, zz));
          glVertex3f(xCoord(voxsize + 1, i, zz),  yCoord(voxsize + 1, i, zz), zCoord(voxsize + 1, i, zz));
        end;
        for i := 0 to voxsize do
        begin
          glVertex3f(xCoord(i, -1, zz),  yCoord(i, -1, zz), zCoord(i, -1, zz));
          glVertex3f(xCoord(i, voxsize + 1, zz),  yCoord(i, voxsize + 1, zz), zCoord(i, voxsize + 1, zz));
        end;
      end;
    end;
  glEnd;

end;

// jval: make it static
var
  flags: voxelrenderflags_t;

procedure glRenderVoxel_Boxes(const voxsize: Integer; const vox: voxelbuffer_p);
var
  xx, yy, zz: integer;
  xxx, yyy, zzz,
  xxx1, yyy1, zzz1: single;
  vp: LongWord;
  oldc: LongWord;
  b: byte;
  i: integer;
  skip: Integer;
  old_skip_x0, old_skip_x1: boolean;
  old_skip_y0, old_skip_y1: boolean;
  old_skip_z0, old_skip_z1: boolean;
  skip_x0, skip_x1: boolean;
  skip_y0, skip_y1: boolean;
  skip_z0, skip_z1: boolean;

begin
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



  step := -1.0 / voxsize;
  mid := step * voxsize / 2;
  oldc := $FFFFFFFF;
  skip := 0;
  old_skip_x0 := true;
  old_skip_x1 := true;
  old_skip_y0 := true;
  old_skip_y1 := true;
  old_skip_z0 := true;
  old_skip_z1 := true;


  glBegin(GL_QUADS);

    for xx := 0 to voxsize - 1 do
    begin
      xxx := mid - xx * step;
      xxx1 := mid - (1 + xx) * step;
      for yy := 0 to voxsize - 1 do
      begin
        yyy := yy * step - mid;
        yyy1 := (1 + yy) * step - mid;
        for zz := 0 to voxsize - 1 do
        begin
          if skip > 0 then
          begin
            dec(skip);
            Continue;
          end;
          skip := 0;

          if vox[xx, yy, zz] <> 0 then
          begin
            b := flags[xx, yy, zz];
            skip_x0 := b and FLG_SKIPX0 <> 0;
            skip_x1 := b and FLG_SKIPX1 <> 0;
            skip_y0 := b and FLG_SKIPY0 <> 0;
            skip_y1 := b and FLG_SKIPY1 <> 0;
            skip_z0 := b and FLG_SKIPZ0 <> 0;
            skip_z1 := b and FLG_SKIPZ1 <> 0;
            vp := vox[xx, yy, zz];
            for i := zz to voxsize - 1 do
              if (old_skip_x0 = skip_x0) and
                 (old_skip_x1 = skip_x1) and
                 (old_skip_y0 = skip_y0) and
                 (old_skip_y1 = skip_y1) and
                 (old_skip_z0 = skip_z0) and
                 (old_skip_z1 = skip_z1) and
                 (vox[xx, yy, i] = vp) then
                Inc(skip)
              else
                break;

            old_skip_x0 := skip_x0;
            old_skip_x1 := skip_x1;
            old_skip_y0 := skip_y0;
            old_skip_y1 := skip_y1;
            old_skip_z0 := skip_z0;
            old_skip_z1 := skip_z1;

            zzz := zz * step - mid;
            if skip > 0 then
              dec(skip);
            zzz1 := (1 + skip + zz) * step - mid;
            if oldc <> vp then
            begin
              glColor3f((vp and $FF) / 255,
                        ((vp shr 8) and $FF) / 255,
                        (vp shr 16) / 255);
              oldc := vp;
            end;

            if b and FLG_SKIPZ0 = 0 then
            begin
              glvertex3f(xxx, yyy, zzz);
              glvertex3f(xxx, yyy1, zzz);
              glvertex3f(xxx1, yyy1, zzz);
              glvertex3f(xxx1, yyy, zzz);
            end;

            if b and FLG_SKIPY0 = 0 then
            begin
              glvertex3f(xxx, yyy, zzz);
              glvertex3f(xxx, yyy, zzz1);
              glvertex3f(xxx1, yyy, zzz1);
              glvertex3f(xxx1, yyy, zzz);
            end;

            if b and FLG_SKIPX0 = 0 then
            begin
              glvertex3f(xxx, yyy, zzz);
              glvertex3f(xxx, yyy, zzz1);
              glvertex3f(xxx, yyy1, zzz1);
              glvertex3f(xxx, yyy1, zzz);
            end;

            if b and FLG_SKIPZ1 = 0 then
            begin
              glvertex3f(xxx, yyy, zzz1);
              glvertex3f(xxx, yyy1, zzz1);
              glvertex3f(xxx1, yyy1, zzz1);
              glvertex3f(xxx1, yyy, zzz1);
            end;

            if b and FLG_SKIPY1 = 0 then
            begin
              glvertex3f(xxx, yyy1, zzz);
              glvertex3f(xxx, yyy1, zzz1);
              glvertex3f(xxx1, yyy1, zzz1);
              glvertex3f(xxx1, yyy1, zzz);
            end;

            if b and FLG_SKIPX1 = 0 then
            begin
              glvertex3f(xxx1, yyy, zzz);
              glvertex3f(xxx1, yyy1, zzz);
              glvertex3f(xxx1, yyy1, zzz1);
              glvertex3f(xxx1, yyy , zzz1);
            end;
          end;
        end;
      end;
    end;

  glEnd;

end;

type
  pointsizeinfo_t = record
    z: single;
    size: integer;
  end;

const
  POINTSIZES = 16;

  pointsizeinfo: array[0..POINTSIZES - 1] of pointsizeinfo_t = (
    (z: -1.0; size: 97),
    (z: -1.2; size: 70),
    (z: -1.4; size: 55),
    (z: -1.6; size: 45),
    (z: -1.8; size: 48),
    (z: -1.8; size: 48),
    (z: -2.0; size: 33),
    (z: -2.2; size: 29),
    (z: -2.4; size: 26),
    (z: -2.6; size: 24),
    (z: -2.8; size: 22),
    (z: -3.0; size: 20),
    (z: -3.3; size: 18),
    (z: -3.6; size: 16),
    (z: -4.5; size: 13),
    (z: -6.0; size:  9)
  );

function glGetPointSize(const voxelsize, windowsize: Integer): Single;
var
  i: integer;
begin
  result := 0;

  if camera.z >= pointsizeinfo[0].z then
    result := pointsizeinfo[0].size + 1
  else if camera.z <= pointsizeinfo[POINTSIZES - 1].z then
    result := pointsizeinfo[POINTSIZES - 1].size + 1
  else
  begin
    i := 1;
    while i < POINTSIZES do
    begin
      if camera.z >= pointsizeinfo[i].z then
      begin
        result :=  pointsizeinfo[i].size * (pointsizeinfo[i - 1].z - camera.z) / (pointsizeinfo[i - 1].z - pointsizeinfo[i].z) +
                   pointsizeinfo[i - 1].size * (camera.z - pointsizeinfo[i].z) / (pointsizeinfo[i - 1].z - pointsizeinfo[i].z) + 1;
        i := POINTSIZES;
      end;
      Inc(i);
    end;
  end;

  Result := Result * (16 / voxelsize) * (windowsize / 1024) * Sqrt(2);
end;

procedure glRenderVoxel_Points(const voxsize: Integer; const vox: voxelbuffer_p; const windowsize: integer);
var
  xx, yy, zz: integer;
  xxx, yyy, zzz: single;
  ps: single;
begin
  step := -1.0 / voxsize;
  mid := step * voxsize / 2;

  ps := glGetPointSize(voxsize, windowsize);
  glPointSize(ps);
  glBegin(GL_POINTS);

    for xx := 0 to voxsize - 1 do
    begin
      xxx := mid - (0.5 + xx) * step;
      for yy := 0 to voxsize - 1 do
      begin
        yyy := (0.5 + yy) * step - mid;
        for zz := 0 to voxsize - 1 do
          if vox[xx, yy, zz] <> 0 then
          begin
            zzz := (0.5 + zz) * step - mid;
            glColor3f((vox[xx, yy, zz] and $FF) / 255,
                      ((vox[xx, yy, zz] shr 8) and $FF) / 255,
                      (vox[xx, yy, zz] shr 16) / 255);

            glvertex3f(xxx, yyy, zzz);
          end;
      end;
    end;

  glEnd;

end;

end.
