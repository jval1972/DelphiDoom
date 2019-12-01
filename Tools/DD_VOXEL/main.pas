unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, pngimage, xTGA, jpeg, zBitmap, ComCtrls, ExtCtrls, Buttons,
  voxels, Menus, StdCtrls, AppEvnts, ExtDlgs, vxe_undo, dglOpenGL;

type
  TForm1 = class(TForm)
    SaveDialog1: TSaveDialog;
    ColorDialog1: TColorDialog;
    Panel1: TPanel;
    NewButton1: TSpeedButton;
    OpenButton1: TSpeedButton;
    SaveButton1: TSpeedButton;
    SaveAsButton1: TSpeedButton;
    AboutButton1: TSpeedButton;
    ExitButton1: TSpeedButton;
    Panel2: TPanel;
    Panel4: TPanel;
    FormPanel: TPanel;
    Splitter1: TSplitter;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    PaintBox1: TPaintBox;
    Panel3: TPanel;
    Palette: TImage;
    DrawColor1Panel: TPanel;
    DrawColor2Panel: TPanel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Open2: TMenuItem;
    N1: TMenuItem;
    Save1: TMenuItem;
    Savesa1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Bevel1: TBevel;
    Panel5: TPanel;
    ComboBox1: TComboBox;
    Label1: TLabel;
    ZoomInButton1: TSpeedButton;
    ZoomOutButton1: TSpeedButton;
    OpenGLPanel: TPanel;
    ApplicationEvents1: TApplicationEvents;
    OpenDialog1: TOpenDialog;
    Voxel1: TMenuItem;
    Importimage1: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    Panel6: TPanel;
    UndoButton1: TSpeedButton;
    RedoButton1: TSpeedButton;
    Timer1: TTimer;
    StatusBar1: TStatusBar;
    N3: TMenuItem;
    Options1: TMenuItem;
    RotateRightButton1: TSpeedButton;
    RotateLeftButton1: TSpeedButton;
    Panel7: TPanel;
    ExportCurrentLevelAsBitmap1: TMenuItem;
    SavePictureDialog1: TSavePictureDialog;
    Import1: TMenuItem;
    N4: TMenuItem;
    KVXVoxel1: TMenuItem;
    OpenDialog2: TOpenDialog;
    Export1: TMenuItem;
    Optimizedmesh1: TMenuItem;
    SaveDialog2: TSaveDialog;
    BatchConvert1: TMenuItem;
    N5: TMenuItem;
    OpenDialog3: TOpenDialog;
    BatchConvert2: TMenuItem;
    DDVOXtoDDMESH1: TMenuItem;
    DDMESH1: TMenuItem;
    OpenDialog4: TOpenDialog;
    OpenDialog5: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure NewButton1Click(Sender: TObject);
    procedure SaveButton1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AboutButton1Click(Sender: TObject);
    procedure SaveAsButton1Click(Sender: TObject);
    procedure ExitButton1Click(Sender: TObject);
    procedure DrawColor1PanelDblClick(Sender: TObject);
    procedure DrawColor2PanelDblClick(Sender: TObject);
    procedure OpenButton1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure ZoomInButton1Click(Sender: TObject);
    procedure ZoomOutButton1Click(Sender: TObject);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure PaletteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure OpenGLPanelResize(Sender: TObject);
    procedure ApplicationEvents1Message(var Msg: tagMSG;
      var Handled: Boolean);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure OpenGLPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure OpenGLPanelDblClick(Sender: TObject);
    procedure Importimage1Click(Sender: TObject);
    procedure RotateLeftButton1Click(Sender: TObject);
    procedure RotateRightButton1Click(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure Redo1Click(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Options1Click(Sender: TObject);
    procedure ExportCurrentLevelAsBitmap1Click(Sender: TObject);
    procedure KVXVoxel1Click(Sender: TObject);
    procedure Optimizedmesh1Click(Sender: TObject);
    procedure BatchConvert1Click(Sender: TObject);
    procedure DDVOXtoDDMESH1Click(Sender: TObject);
    procedure DDMESH1Click(Sender: TObject);
  private
    { Private declarations }
    ffilename: string;
    changed: Boolean;
    fvoxelsize: Integer;
    voxelbuffer: voxelbuffer_p;
    xpos, ypos, zpos: integer;
    curpalcolor: LongWord;
    drawcolor1, drawcolor2: LongWord;
    mousedown1, mousedown2: Boolean;
    zoom: integer;
    buffer: TBitmap;
    rc: HGLRC;   // Rendering Context
    dc: HDC;     // Device Context
    Keys: array[0..255] of boolean;
    ElapsedTime, AppStart, LastTime : integer;  // Timing variables
    glpanx, glpany: integer;
    glpandown: Boolean;
    undoManager: TUndoRedoManager;
    glneedsupdate: boolean;
    gllist: GLUint;
    procedure Idle(Sender: TObject; var Done: Boolean);
    function CheckCanClose: boolean;
    procedure DoNewVoxel(const size: integer);
    procedure DoSaveVoxel(const fname: string);
    function DoLoadVoxel(const fname: string): boolean;
    function DoLoadKVX(const fname: string): boolean;
    function DoLoadDDMESH(const fname: string): boolean;
    procedure SetFileName(const fname: string);
    procedure SetDrawColors(const col1, col2: LongWord);
    procedure DoSaveVoxelBinaryUndo(s: TStream);
    procedure DoLoadVoxelBinaryUndo(s: TStream);
    procedure UpdateStausbar;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  qforms,
  vxe_gl,
  vxe_system,
  vxe_script,
  vxe_rotate,
  newfrm,
  vxe_defs,
  optionsfrm,
  vxe_mesh,
  vxe_kvx,
  kvx2mesh,
  progressfrm, ddvox2mesh;

{$R *.dfm}

resourcestring
  rsTitle = 'DelphiDOOM Voxel Editor';

function axis(x, y, z: integer): char;
begin
  if x >= 0 then
    result := 'x'
  else if y >= 0 then
    result := 'y'
  else if z >= 0 then
    result := 'z'
  else
    axis := ' ';
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  pfd: TPIXELFORMATDESCRIPTOR;
  pf: Integer;
begin
  vxe_LoadSettingFromFile(ChangeFileExt(ParamStr(0), '.ini'));
  undoManager := TUndoRedoManager.Create;
  undoManager.OnLoadFromStream := DoLoadVoxelBinaryUndo;
  undoManager.OnSaveToStream := DoSaveVoxelBinaryUndo;
  GetMem(voxelbuffer, SizeOf(voxelbuffer_t));
  glpanx := 0;
  glpany := 0;
  glpandown := false;
  curpalcolor := 0;
  zoom := 16;
  mousedown1 := false;
  mousedown2 := false;
  SetDrawColors(RGB(255, 255, 255), 0);
  if ParamCount > 0 then
  begin
    if not DoLoadVoxel(ParamStr(1)) then
      DoNewVoxel(32);
  end
  else
    DoNewVoxel(32);
  buffer := TBitmap.Create;

  InitOpenGL;
  ReadExtensions;
  ReadImplementationProperties;

  // OpenGL initialisieren
  dc := GetDC(OpenGLPanel.Handle);

  // PixelFormat
  pfd.nSize := SizeOf(pfd);
  pfd.nVersion := 1;
  pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER or 0;
  pfd.iPixelType := PFD_TYPE_RGBA;      // PFD_TYPE_RGBA or PFD_TYPEINDEX
  pfd.cColorBits := 32;

  pf := ChoosePixelFormat(dc, @pfd);   // Returns format that most closely matches above pixel format
  SetPixelFormat(dc, pf, @pfd);

  rc := wglCreateContext(dc);    // Rendering Context = window-glCreateContext
  wglMakeCurrent(dc, rc);        // Make the DC (Form1) the rendering Context

  // Initialize GL environment variables
  glInit;

 // PAK_AddFile('Camelot3000_1.cbz');
//  glInitTextures;

  ZeroMemory(@keys, SizeOf(keys));
  ResetCamera(axis(xpos, ypos, zpos));

  OpenGLPanelResize(sender);    // sets up the perspective
  AppStart := I_GetTime;

  glneedsupdate := true;
  gllist := glGenLists(1);
  // when the app has spare time, render the GL scene
  Application.OnIdle := Idle;

end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckCanClose;
end;

function TForm1.CheckCanClose: boolean;
var
  ret: integer;
begin
  if changed then
  begin
    ret := MessageBox(Handle, 'Do you want to save changes?', PChar(rsTitle), MB_YESNOCANCEL or MB_ICONQUESTION or MB_APPLMODAL);
    if ret = IDCANCEL	then
    begin
      result := false;
      exit;
    end;
    if ret = IDNO	then
    begin
      result := true;
      exit;
    end;
    if ret = IDYES then
    begin
      SaveButton1Click(self);
      result := not changed;
      exit;
    end;
  end;
  result := true;
end;

procedure TForm1.NewButton1Click(Sender: TObject);
var
  frm: TNewForm;
begin
  if not CheckCanClose then
    Exit;

  frm := TNewForm.Create(self);
  frm.ShowModal;
  if frm.ModalResult = mrOK then
  begin
    DoNewVoxel(1 shl (frm.RadioGroup1.ItemIndex + 2));
    ResetCamera(axis(xpos, ypos, zpos));
  end;
  frm.Free;
end;

procedure TForm1.DoNewVoxel(const size: integer);
var
  i: integer;
  c: char;
begin
  fvoxelsize := size;
  PaintBox1.Width := fvoxelsize * zoom + 1;
  PaintBox1.Height := fvoxelsize * zoom + 1;
  SetFileName('');
  changed := false;
  glneedsupdate := True;
  xpos := -1;
  ypos := -1;
  zpos := fvoxelsize - 1;
  ComboBox1.Items.Clear;
  for c := 'x' to 'z' do
    for i := 0 to fvoxelsize - 1 do
      ComboBox1.Items.Add(c + IntToStr(i));
  ComboBox1.ItemIndex := ComboBox1.Items.Count - 1;
  FillChar(voxelbuffer^, SizeOf(voxelbuffer_t), Chr(0));
  PaintBox1.Invalidate;
  undoManager.Clear;
end;

const
  MAXDISPFNAME = 30;

function MkSortName(const fname: string): string;
var
  i: integer;
begin
  if Length(fname) < MAXDISPFNAME then
  begin
    result := fname;
    exit;
  end;
  result := '';
  for i := Length(fname) downto Length(fname) - (MAXDISPFNAME - 6) do
    Result := fname[i] + Result;
  Result := '...' + Result;
  for i := 3 downto 1 do
    Result := fname[i] + Result;
end;

procedure TForm1.SetFileName(const fname: string);
begin
  ffilename := fname;
  Caption := rsTitle;
  if ffilename <> '' then
    Caption := Caption + ' - ' + MkSortName(ffilename);
end;

procedure TForm1.SaveButton1Click(Sender: TObject);
begin
  if ffilename = '' then
  begin
    if SaveDialog1.Execute then
      ffilename := SaveDialog1.FileName
    else
    begin
      Beep;
      Exit;
    end;
  end;
  DoSaveVoxel(ffilename);
end;

procedure TForm1.DoSaveVoxel(const fname: string);
var
  t: TextFile;
  xx, yy, zz: integer;
  skip: integer;
begin
  SetFileName(fname);

  AssignFile(t, fname);
  Rewrite(t);
  Writeln(t, fvoxelsize);
  for xx := 0 to fvoxelsize - 1 do
    for yy := 0 to fvoxelsize - 1 do
    begin
      skip := 0;
      for zz := 0 to fvoxelsize - 1 do
      begin
        if voxelbuffer[xx, yy, zz] = 0 then
        begin
          Inc(skip);
          if zz = fvoxelsize - 1 then
            Writeln(t, 'skip ', skip);
        end
        else
        begin
          if skip > 0 then
          begin
            Write(t, 'skip ', skip, ', ');
            skip := 0;
          end;
          if zz = fvoxelsize - 1 then
            Writeln(t, voxelbuffer[xx, yy, zz])
          else
            Write(t, voxelbuffer[xx, yy, zz], ', ');
        end;
      end;
    end;

  CloseFile(t);

  changed := false;
end;

function TForm1.DoLoadVoxel(const fname: string): boolean;
var
  buf: TStringList;
  sc: TScriptEngine;
  xx, yy, zz: integer;
  s: string;
begin

  if not FileExists(fname) then
  begin
    s := Format('File %s does not exist!', [MkSortName(fname)]);
    MessageBox(Handle, PChar(s), PChar(rsTitle), MB_OK or MB_ICONEXCLAMATION or MB_APPLMODAL);
    Result := false;
    exit;
  end;

  undoManager.Clear;

  buf := TStringList.Create;
  try
    buf.LoadFromFile(fname);
    sc := TScriptEngine.Create(buf.Text);
    sc.MustGetInteger;
    DoNewVoxel(sc._Integer);
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

  SetFileName(fname);
  PaintBox1.Invalidate;
  glneedsupdate := True;
  result := true;
end;

function TForm1.DoLoadKVX(const fname: string): boolean;
var
  strm: TFileStream;
  pal: array[0..255] of LongWord;
  i: integer;
  r, g, b: byte;
  numbytes: integer;
  xsiz, ysiz, zsiz, xpivot, ypivot, zpivot: integer;
	xoffset: PIntegerArray;
	xyoffset: PSmallIntPArray;
  offs: integer;
  voxdatasize: integer;
  voxdata: PByteArray;
  size: integer;
  xx, yy, zz: integer;
  x1, y1, z1: integer;
  endptr: PByte;
  slab: kvxslab_p;
  s: string;
  kvxbuffer: kvxbuffer_p;
  maxpal: integer;
  cc: integer;
  palfactor: double;
begin

  if not FileExists(fname) then
  begin
    s := Format('File %s does not exist!', [MkSortName(fname)]);
    MessageBox(Handle, PChar(s), PChar(rsTitle), MB_OK or MB_ICONEXCLAMATION or MB_APPLMODAL);
    Result := false;
    exit;
  end;

  strm := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);

  strm.Seek(strm.size - 768, soFromBeginning);
  maxpal := 0;
  for i := 0 to 255 do
  begin
    strm.Read(b, SizeOf(Byte));
    if b > maxpal then
      maxpal := b;
    strm.Read(g, SizeOf(Byte));
    if g > maxpal then
      maxpal := g;
    strm.Read(r, SizeOf(Byte));
    if r > maxpal then
      maxpal := r;
    pal[i] := r shl 16 + g shl 8 + b;
    if pal[i] = 0 then
      pal[i] := $01;
  end;
  if (maxpal < 255) and (maxpal > 0) then
  begin
    palfactor := 255 / maxpal;
    if palfactor > 4.0 then
      palfactor := 4.0;
    for i := 0 to 255 do
    begin
      r := pal[i] shr 16;
      g := pal[i] shr 8;
      b := pal[i];
      cc := round(palfactor * r);
      if cc < 0 then
        cc := 0
      else if cc > 255 then
        cc := 255;
      r := cc;
      cc := round(palfactor * g);
      if cc < 0 then
        cc := 0
      else if cc > 255 then
        cc := 255;
      g := cc;
      cc := round(palfactor * b);
      if cc < 0 then
        cc := 0
      else if cc > 255 then
        cc := 255;
      b := cc;
      pal[i] := r shl 16 + g shl 8 + b;
    end;
  end;

  strm.Seek(0, soFromBeginning);
  strm.Read(numbytes, SizeOf(Integer));
  strm.Read(xsiz, SizeOf(Integer));
  strm.Read(ysiz, SizeOf(Integer));
  strm.Read(zsiz, SizeOf(Integer));

  size := 1;

  while xsiz > size do
    size := size * 2;
  while ysiz > size do
    size := size * 2;
  while zsiz > size do
    size := size * 2;

  if size < 256 then
    size := size * 2;

  DoNewVoxel(size);

  strm.Read(xpivot, SizeOf(Integer));
  strm.Read(ypivot, SizeOf(Integer));
  strm.Read(zpivot, SizeOf(Integer));

  GetMem(xoffset, (xsiz + 1) * SizeOf(Integer));
  GetMem(xyoffset, xsiz * SizeOf(PSmallIntArray));
  for i := 0 to xsiz - 1 do
    GetMem(xyoffset[i], (ysiz + 1) * SizeOf(SmallInt));

  strm.Read(xoffset^, (xsiz + 1) * SizeOf(Integer));

  for i := 0 to xsiz - 1 do
    strm.Read(xyoffset[i]^, (ysiz + 1) * SizeOf(SmallInt));

  offs := xoffset[0];

  voxdatasize := numbytes - 24 - (xsiz + 1) * 4 - xsiz * (ysiz + 1) * 2;
  GetMem(voxdata, voxdatasize);
  strm.Read(voxdata^, voxdatasize);
  strm.Free;

  GetMem(kvxbuffer, SizeOf(kvxbuffer_t));
  for xx := 0 to xsiz - 1 do
    for yy := 0 to ysiz - 1 do
       for zz := 0 to zsiz - 1 do
         kvxbuffer[xx, yy, zz] := $FFFF;

  for xx := 0 to xsiz - 1 do
  begin
    for yy := 0 to ysiz - 1 do
    begin
      endptr := @voxdata[xoffset[xx] + xyoffset[xx][yy + 1] - offs];
      slab := @voxdata[xoffset[xx] + xyoffset[xx][yy] - offs];
      while Integer(slab) < integer(endptr) do
      begin
        for zz := slab.ztop to slab.zleng + slab.ztop - 1 do
          kvxbuffer[xx, yy, zz] := slab.col[zz - slab.ztop];
        slab := kvxslab_p(integer(slab) + slab.zleng + 3);
      end;
    end;
  end;

  x1 := size div 2 - xpivot div 256;
  y1 := size div 2 - ypivot div 256;
  z1 := size{ div 2} - zpivot div 256;
  if x1 < 0 then
    x1 := 0;
  if y1 < 0 then
    y1 := 0;
  if z1 < 0 then
    z1 := 0;
  while x1 + xsiz >= size do
    dec(x1);
  while y1 + ysiz >= size do
    dec(y1);
  while z1 + zsiz >= size do
    dec(z1);
  for xx := x1 to x1 + xsiz - 1 do
    for yy := y1 to y1 + ysiz - 1 do
      for zz := z1 to z1 + zsiz - 1 do
        if kvxbuffer[xx - x1, yy - y1, zz - z1] <> $FFFF then
          voxelbuffer[xx, zz, size - yy - 1] := pal[kvxbuffer[xx - x1, yy - y1, zz - z1]];


  FreeMem(xoffset, (xsiz + 1) * SizeOf(Integer));
  for i := 0 to xsiz - 1 do
    FreeMem(xyoffset[i], (ysiz + 1) * SizeOf(SmallInt));
  FreeMem(xyoffset, xsiz * SizeOf(PSmallIntArray));
  FreeMem(voxdata, voxdatasize);
  FreeMem(kvxbuffer, SizeOf(kvxbuffer_t));

  PaintBox1.Invalidate;
  glneedsupdate := True;
  result := true;
end;

function TForm1.DoLoadDDMESH(const fname: string): boolean;
var
  strm: TFileStream;
  i: integer;
  xb, yb, zb: byte;
  HDR: LongWord;
  version: integer;
  size: integer;
  quads: integer;
  fnumvoxels: Integer;
  c: LongWord;
  s: string;
begin
  if not FileExists(fname) then
  begin
    s := Format('File %s does not exist!', [MkSortName(fname)]);
    MessageBox(Handle, PChar(s), PChar(rsTitle), MB_OK or MB_ICONEXCLAMATION or MB_APPLMODAL);
    Result := false;
    exit;
  end;

  strm := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);

  strm.Read(HDR, SizeOf(LongWord));
  if HDR <> Ord('D') + Ord('D') shl 8 + Ord('M') shl 16 + Ord('S') shl 24 then
  begin
    s := Format('File %s does not have DDMESH magic header!', [MkSortName(fname)]);
    MessageBox(Handle, PChar(s), PChar(rsTitle), MB_OK or MB_ICONEXCLAMATION or MB_APPLMODAL);
    Result := false;
    strm.Free;
    exit;
  end;

  strm.Read(version, SizeOf(integer));
  if version <> 1 then
  begin
    s := Format('File %s is from unsupported version = %d!', [MkSortName(fname), version]);
    MessageBox(Handle, PChar(s), PChar(rsTitle), MB_OK or MB_ICONEXCLAMATION or MB_APPLMODAL);
    Result := false;
    strm.Free;
    exit;
  end;

  strm.Read(size, SizeOf(integer));
  DoNewVoxel(size);

  strm.Read(quads, SizeOf(integer));

  strm.Position := 16 + quads * (4 * SizeOf(voxelvertex_t) + SizeOf(LongWord));

  strm.Read(fnumvoxels, SizeOf(integer));

  for i := 0 to fnumvoxels - 1 do
  begin
    strm.Read(xb, SizeOf(byte));
    strm.Read(yb, SizeOf(byte));
    strm.Read(zb, SizeOf(byte));
    strm.Read(c, SizeOf(LongWord));
    voxelbuffer[xb, yb, zb] := c;
  end;

  strm.Free;

  PaintBox1.Invalidate;
  glneedsupdate := True;
  result := true;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := false;
  glDeleteLists(gllist, 1);
  FreeMem(voxelbuffer);
  buffer.Free;
  undoManager.Free;
  wglMakeCurrent(0, 0);
  wglDeleteContext(rc);
  vxe_SaveSettingsToFile(ChangeFileExt(ParamStr(0), '.ini'));
end;

procedure TForm1.AboutButton1Click(Sender: TObject);
begin
  MessageBox(
    Handle,
    PChar(Format('%s'#13#10'Version 1.1'#13#10#13#10'A tool for creating VOXELS.'#13#10'2013 - jvalavanis@gmail.com', [rsTitle])),
    PChar(rsTitle),
    MB_OK or MB_ICONINFORMATION or MB_APPLMODAL);
end;

procedure TForm1.SaveAsButton1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    DoSaveVoxel(SaveDialog1.FileName);
end;

procedure TForm1.ExitButton1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.SetDrawColors(const col1, col2: LongWord);
begin
  drawcolor1 := col1;
  drawcolor2 := col2;
  DrawColor1Panel.Color := drawcolor1;
  DrawColor2Panel.Color := drawcolor2;
end;

procedure TForm1.DrawColor1PanelDblClick(Sender: TObject);
begin
  ColorDialog1.Color := drawcolor1;
  if ColorDialog1.Execute then
  begin
    drawcolor1 := ColorDialog1.Color;
    DrawColor1Panel.Color := drawcolor1;
  end;
end;

procedure TForm1.DrawColor2PanelDblClick(Sender: TObject);
begin
  ColorDialog1.Color := drawcolor2;
  if ColorDialog1.Execute then
  begin
    drawcolor2 := ColorDialog1.Color;
    DrawColor2Panel.Color := drawcolor2;
  end;
end;

procedure TForm1.OpenButton1Click(Sender: TObject);
begin
  if not CheckCanClose then
    Exit;

  if OpenDialog1.Execute then
  begin
    DoLoadVoxel(OpenDialog1.FileName);
    ResetCamera(axis(xpos, ypos, zpos));
  end;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
var
  idx: integer;
  c: Char;
  pos: integer;
  s: string;
begin
  idx := ComboBox1.ItemIndex;
  if idx < 0 then
    exit;

  ActiveControl := Panel4;
  s := ComboBox1.Items.Strings[idx];
  c := s[1];
  s[1] := ' ';
  s := Trim(s);
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
  end;
  PaintBox1.Invalidate;
  glneedsupdate := True;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  i, j: integer;
begin
  buffer.Width := PaintBox1.Width;
  buffer.Height := PaintBox1.Height;
  buffer.Canvas.Brush.Style := bsSolid;

  if xpos >= 0 then
  begin

    for i := 0 to fvoxelsize - 1 do
      for j := 0 to fvoxelsize - 1 do
      begin
        buffer.Canvas.Brush.Color := voxelbuffer[xpos, i, j];
        buffer.Canvas.Rectangle(i * zoom,
                                j * zoom,
                                (1 + i) * zoom,
                                (1 + j) * zoom);
      end;
  end;

  if ypos >= 0 then
  begin

    for i := 0 to fvoxelsize - 1 do
      for j := 0 to fvoxelsize - 1 do
      begin
        buffer.Canvas.Brush.Color := voxelbuffer[j, ypos, i];
        buffer.Canvas.Rectangle(j * zoom,
                                i * zoom,
                                (1 + j) * zoom,
                                (1 + i) * zoom);
      end;
  end;

  if zpos >= 0 then
  begin

    for i := 0 to fvoxelsize - 1 do
      for j := 0 to fvoxelsize - 1 do
      begin
        buffer.Canvas.Brush.Color := voxelbuffer[i, j, zpos];
        buffer.Canvas.Rectangle(i * zoom,
                                j * zoom,
                                (1 + i) * zoom,
                                (1 + j) * zoom);
      end;
  end;

  buffer.Canvas.Pen.Color := clBlack;
  i := 0;
  while i < buffer.Width do
  begin
    buffer.Canvas.MoveTo(i, 0);
    buffer.Canvas.LineTo(i, buffer.Height - 1);
    i := i + zoom;
  end;

  j := 0;
  while j < buffer.Height do
  begin
    buffer.Canvas.MoveTo(0, j);
    buffer.Canvas.LineTo(buffer.Width - 1, j);
    j := j + zoom;
  end;
  PaintBox1.Canvas.Draw(0, 0, buffer);
end;

procedure TForm1.ZoomInButton1Click(Sender: TObject);
begin
  if zoom >= 32 then
    exit;
  Inc(zoom);
  PaintBox1.Width := fvoxelsize * zoom + 1;
  PaintBox1.Height := fvoxelsize * zoom + 1;
  PaintBox1.Invalidate;
end;

procedure TForm1.ZoomOutButton1Click(Sender: TObject);
begin
  if zoom <= 4 then
    exit;
  Dec(zoom);
  PaintBox1.Width := fvoxelsize * zoom + 1;
  PaintBox1.Height := fvoxelsize * zoom + 1;
  PaintBox1.Invalidate;
end;

procedure TForm1.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  pt: TPoint;
  r: TRect;
begin
  pt := PaintBox1.Parent.Parent.ScreenToClient(MousePos);
  r := PaintBox1.ClientRect;
  if r.Right > ScrollBox2.Width then
    r.Right := ScrollBox2.Width;
  if r.Bottom > ScrollBox2.Height then
    r.Bottom := ScrollBox2.Height;
  if PtInRect(r, pt) then
    ZoomOutButton1Click(Sender);

  pt := OpenGLPanel.Parent.ScreenToClient(MousePos);
  r := OpenGLPanel.ClientRect;
  if r.Right > ScrollBox1.Width then
    r.Right := ScrollBox1.Width;
  if r.Bottom > ScrollBox1.Height then
    r.Bottom := ScrollBox1.Height;
  if PtInRect(r, pt) then
  begin
    camera.z := camera.z - 0.05;
    if camera.z < -6.0 then
      camera.z := -6.0;
    glneedsupdate := True;
  end;
end;

procedure TForm1.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  pt: TPoint;
  r: TRect;
begin
  pt := PaintBox1.Parent.Parent.ScreenToClient(MousePos);
  r := PaintBox1.ClientRect;
  if r.Right > ScrollBox2.Width then
    r.Right := ScrollBox2.Width;
  if r.Bottom > ScrollBox2.Height then
    r.Bottom := ScrollBox2.Height;
  if PtInRect(r, pt) then
    ZoomInButton1Click(Sender);

  pt := OpenGLPanel.Parent.ScreenToClient(MousePos);
  r := OpenGLPanel.ClientRect;
  if r.Right > ScrollBox1.Width then
    r.Right := ScrollBox1.Width;
  if r.Bottom > ScrollBox1.Height then
    r.Bottom := ScrollBox1.Height;
  if PtInRect(r, pt) then
  begin
    camera.z := camera.z + 0.05;
    if camera.z > 0.0 then
      camera.z := 0.0;
    glneedsupdate := True;
  end;
end;

procedure TForm1.PaletteMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not (Button in [mbLeft, mbRight]) then
    Exit;

  if (X < 8) and (Y < 8) then
    curpalcolor := 0
  else
    curpalcolor := Palette.Canvas.Pixels[X, Y];

  if Button = mbLeft then
    SetDrawColors(curpalcolor, drawcolor2)
  else
    SetDrawColors(drawcolor1, curpalcolor);
end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    mousedown1 := True;
    mousedown2 := False;
    undoManager.SaveUndo;
    PaintBox1MouseMove(Sender, Shift, X, Y);
  end
  else if Button = mbRight then
  begin
    mousedown1 := False;
    mousedown2 := True;
    undoManager.SaveUndo;
    PaintBox1MouseMove(Sender, Shift, X, Y);
  end
  else
  begin
    mousedown1 := False;
    mousedown2 := False;
  end;
end;

procedure TForm1.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mousedown1 := False;
  mousedown2 := False;
  glneedsupdate := True;
end;

procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  vx, vy, vz: integer;
begin
  StatusBar1.Panels[0].Text := Format('x=%d, y=%d', [X div zoom, Y div zoom]);

  if not (mousedown1 or mousedown2) then
    exit;

  if xpos >= 0 then
  begin
    vx := xpos;
    vy := X div zoom;
    vz := Y div zoom;
  end
  else if ypos >= 0 then
  begin
    vx := X div zoom;
    vy := ypos;
    vz := Y div zoom;
  end
  else if zpos >= 0 then
  begin
    vx := X div zoom;
    vy := Y div zoom;
    vz := zpos
  end
  else
    Exit;

  if vx < 0 then
    vx := 0
  else if vx >= fvoxelsize then
    vx := fvoxelsize - 1;

  if vy < 0 then
    vy := 0
  else if vy >= fvoxelsize then
    vy := fvoxelsize - 1;

  if vz < 0 then
    vz := 0
  else if vz >= fvoxelsize then
    vz := fvoxelsize - 1;

  if mousedown1 then
    voxelbuffer[vx, vy, vz] := drawcolor1
  else if mousedown2 then
    voxelbuffer[vx, vy, vz] := drawcolor2;
  changed := true;
  glneedsupdate := True;

  PaintBox1.Canvas.Brush.Style := bsSolid;

  PaintBox1.Canvas.Brush.Color := voxelbuffer[vx, vy, vz];
  PaintBox1.Canvas.Rectangle((X div zoom) * zoom,
                             (Y div zoom) * zoom,
                             (X div zoom + 1) * zoom,
                             (Y div zoom + 1) * zoom);
end;

procedure TForm1.OpenGLPanelResize(Sender: TObject);
begin
  glViewport(0, 0, OpenGLPanel.Width, OpenGLPanel.Height);    // Set the viewport for the OpenGL window
  glMatrixMode(GL_PROJECTION);        // Change Matrix Mode to Projection
  glLoadIdentity;                     // Reset View
  gluPerspective(45.0, OpenGLPanel.Width/OpenGLPanel.Height, 1.0, 500.0);  // Do the perspective calculations. Last value = max clipping depth

  glMatrixMode(GL_MODELVIEW);         // Return to the modelview matrix
  glneedsupdate := True;
end;

var
  hasrender: boolean = false;

{------------------------------------------------------------------}
{  Application onIdle event                                        }
{------------------------------------------------------------------}
procedure TForm1.Idle(Sender: TObject; var Done: Boolean);
var
  i: integer;
begin
  hasrender := true;
  Done := false;

  if glneedsupdate then
  begin
{    glDeleteLists(gllist, 1);
    gllist := glGenLists(1);}
  end
  else
// jval: We don't need to render :)
    Exit;

  LastTime := ElapsedTime;
  ElapsedTime := I_GetTime - AppStart;      // Calculate Elapsed Time
  ElapsedTime := (LastTime + ElapsedTime) div 2; // Average it out for smoother movement

  if LastTime = ElapsedTime then
    exit;

  UpdateStausbar;
  for i := LastTime to ElapsedTime - 1 do
  begin

  end;

  glBeginScene(OpenGLPanel.Width, OpenGLPanel.Height);
  if glneedsupdate then
  begin
//    glNewList(gllist, GL_COMPILE_AND_EXECUTE);

    if xpos >= 0 then
      glRenderAxes(fvoxelsize, 'x' + IntToStr(xpos))
    else if ypos >= 0 then
      glRenderAxes(fvoxelsize, 'y' + IntToStr(ypos))
    else if zpos >= 0 then
      glRenderAxes(fvoxelsize, 'z' + IntToStr(zpos));
    if opt_useglpixels then
      glRenderVoxel_Points(fvoxelsize, voxelbuffer, OpenGLPanel.Width)
    else
      glRenderVoxel_Boxes(fvoxelsize, voxelbuffer);

//    glEndList;
  end;{
  else
    glCallList(gllist);}

  glEndScene(dc);

  glneedsupdate := false;
end;

procedure TForm1.ApplicationEvents1Message(var Msg: tagMSG;
  var Handled: Boolean);
begin
  if Msg.message = WM_KEYDOWN then
  begin
    case Msg.wParam of
      VK_RIGHT: keys[VK_RIGHT] := true;
      VK_LEFT: keys[VK_LEFT] := true;
      VK_UP: keys[VK_UP] := true;
      VK_DOWN: keys[VK_DOWN] := true;
    end;
  end;
  if Msg.message = WM_KEYUP then
  begin
    case Msg.wParam of
      VK_RIGHT: keys[VK_RIGHT] := false;
      VK_LEFT: keys[VK_LEFT] := false;
      VK_UP: keys[VK_UP] := false;
      VK_DOWN: keys[VK_DOWN] := false;
    end;
  end;
end;

procedure TForm1.ApplicationEvents1Idle(Sender: TObject;
  var Done: Boolean);
begin
  Idle(Sender, Done);
end;

procedure TForm1.OpenGLPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  glpanx := X;
  glpany := Y;
  glpandown := true;
  glneedsupdate := True;
end;

procedure TForm1.OpenGLPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  glpandown := false;
  glneedsupdate := True;
end;

procedure TForm1.OpenGLPanelMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if not glpandown then
    exit;

  camera.ay := camera.ay + (glpanx - X) ;/// OpenGLPanel.Width {* 2 * pi};
  camera.ax := camera.ax + (glpany - Y) ; // / OpenGLPanel.Height {* 2 * pi};
  glneedsupdate := True;

  glpanx := X;
  glpany := Y;

end;

procedure TForm1.OpenGLPanelDblClick(Sender: TObject);
begin
  ResetCamera(axis(xpos, ypos, zpos));
  glneedsupdate := True;
end;

procedure TForm1.Importimage1Click(Sender: TObject);
var
  p: TPicture;
  b: TBitmap;
  i, j: integer;
begin
  if OpenPictureDialog1.Execute then
  begin
    p := TPicture.Create;
    b := TBitmap.Create;
    try
      undoManager.SaveUndo;
      p.LoadFromFile(OpenPictureDialog1.FileName);
      b.Width := fvoxelsize;
      b.Height := fvoxelsize;
      b.Canvas.StretchDraw(Rect(0, 0, fvoxelsize, fvoxelsize), p.Graphic);
      if xpos >= 0 then
      begin
        for i := 0 to fvoxelsize - 1 do
          for j := 0 to fvoxelsize - 1 do
            voxelbuffer[xpos, i, j] := b.Canvas.Pixels[i, j];
      end;
      if ypos >= 0 then
      begin
        for i := 0 to fvoxelsize - 1 do
          for j := 0 to fvoxelsize - 1 do
            voxelbuffer[i, ypos, j] := b.Canvas.Pixels[i, j];
      end;
      if zpos >= 0 then
      begin
        for i := 0 to fvoxelsize - 1 do
          for j := 0 to fvoxelsize - 1 do
            voxelbuffer[i, j, zpos] := b.Canvas.Pixels[i, j];
      end;
      changed := True;
      glneedsupdate := True;
      PaintBox1.Invalidate;
    finally
      p.Free;
      b.Free;
    end;
  end;
end;

procedure TForm1.RotateLeftButton1Click(Sender: TObject);
var
  b: TBitmap;
  i, j: integer;
begin
  b := TBitmap.Create;
  b.Width := fvoxelsize;
  b.Height := fvoxelsize;

  undoManager.SaveUndo;

  if xpos >= 0 then
  begin
    for i := 0 to fvoxelsize - 1 do
      for j := 0 to fvoxelsize - 1 do
        b.Canvas.Pixels[i, j] := voxelbuffer[xpos, i, j];
  end;
  if ypos >= 0 then
  begin
    for i := 0 to fvoxelsize - 1 do
      for j := 0 to fvoxelsize - 1 do
        b.Canvas.Pixels[i, j] := voxelbuffer[i, ypos, j];
  end;
  if zpos >= 0 then
  begin
    for i := 0 to fvoxelsize - 1 do
      for j := 0 to fvoxelsize - 1 do
        b.Canvas.Pixels[i, j] := voxelbuffer[i, j, zpos];
  end;

  RotateBitmap90DegreesCounterClockwise(b);

  if xpos >= 0 then
  begin
    for i := 0 to fvoxelsize - 1 do
      for j := 0 to fvoxelsize - 1 do
        voxelbuffer[xpos, i, j] := b.Canvas.Pixels[i, j];
  end;
  if ypos >= 0 then
  begin
    for i := 0 to fvoxelsize - 1 do
      for j := 0 to fvoxelsize - 1 do
        voxelbuffer[i, ypos, j] := b.Canvas.Pixels[i, j];
  end;
  if zpos >= 0 then
  begin
    for i := 0 to fvoxelsize - 1 do
      for j := 0 to fvoxelsize - 1 do
        voxelbuffer[i, j, zpos] := b.Canvas.Pixels[i, j];
  end;

  PaintBox1.Invalidate;

  b.Free;

  changed := true;
  glneedsupdate := True;
end;

procedure TForm1.RotateRightButton1Click(Sender: TObject);
var
  b: TBitmap;
  i, j: integer;
begin
  b := TBitmap.Create;
  b.Width := fvoxelsize;
  b.Height := fvoxelsize;

  undoManager.SaveUndo;

  if xpos >= 0 then
  begin
    for i := 0 to fvoxelsize - 1 do
      for j := 0 to fvoxelsize - 1 do
        b.Canvas.Pixels[i, j] := voxelbuffer[xpos, i, j];
  end;
  if ypos >= 0 then
  begin
    for i := 0 to fvoxelsize - 1 do
      for j := 0 to fvoxelsize - 1 do
        b.Canvas.Pixels[i, j] := voxelbuffer[i, ypos, j];
  end;
  if zpos >= 0 then
  begin
    for i := 0 to fvoxelsize - 1 do
      for j := 0 to fvoxelsize - 1 do
        b.Canvas.Pixels[i, j] := voxelbuffer[i, j, zpos];
  end;

  RotateBitmap90DegreesClockwise(b);

  if xpos >= 0 then
  begin
    for i := 0 to fvoxelsize - 1 do
      for j := 0 to fvoxelsize - 1 do
        voxelbuffer[xpos, i, j] := b.Canvas.Pixels[i, j];
  end;
  if ypos >= 0 then
  begin
    for i := 0 to fvoxelsize - 1 do
      for j := 0 to fvoxelsize - 1 do
        voxelbuffer[i, ypos, j] := b.Canvas.Pixels[i, j];
  end;
  if zpos >= 0 then
  begin
    for i := 0 to fvoxelsize - 1 do
      for j := 0 to fvoxelsize - 1 do
        voxelbuffer[i, j, zpos] := b.Canvas.Pixels[i, j];
  end;

  PaintBox1.Invalidate;

  b.Free;

  changed := true;
  glneedsupdate := True;
end;

procedure TForm1.Edit1Click(Sender: TObject);
begin
  Undo1.Enabled := undoManager.CanUndo;
  Redo1.Enabled := undoManager.CanRedo;
end;

procedure TForm1.Undo1Click(Sender: TObject);
begin
  if undoManager.CanUndo then
  begin
    undoManager.Undo;
    PaintBox1.Invalidate;
  end;
end;

procedure TForm1.Redo1Click(Sender: TObject);
begin
  if undoManager.CanRedo then
  begin
    undoManager.Redo;
    PaintBox1.Invalidate;
  end;
end;

procedure TForm1.DoSaveVoxelBinaryUndo(s: TStream);
var
  x, y, z: integer;
  it: voxelitem_t;
  ax: Char;
begin
  ax := axis(xpos, ypos, zpos);
  s.Write(fvoxelsize, SizeOf(fvoxelsize));
  s.Write(ax, SizeOf(ax));

  if ax = 'x' then
  begin
    x := xpos;
    s.Write(x, SizeOf(x));
    for y := 0 to fvoxelsize - 1 do
      for z := 0 to fvoxelsize - 1 do
        if voxelbuffer[x, y, z] <> 0 then
        begin
          s.Write(y, SizeOf(y));
          s.Write(z, SizeOf(z));
          s.Write(voxelbuffer[x, y, z], SizeOf(voxelbuffer[x, y, z]));
        end;
  end;

  if ax = 'y' then
  begin
    y := ypos;
    s.Write(y, SizeOf(y));
    for x := 0 to fvoxelsize - 1 do
      for z := 0 to fvoxelsize - 1 do
        if voxelbuffer[x, y, z] <> 0 then
        begin
          s.Write(x, SizeOf(x));
          s.Write(z, SizeOf(z));
          s.Write(voxelbuffer[x, y, z], SizeOf(voxelbuffer[x, y, z]));
        end;
  end;

  if ax = 'z' then
  begin
    z := zpos;
    s.Write(z, SizeOf(z));
    for x := 0 to fvoxelsize - 1 do
      for y := 0 to fvoxelsize - 1 do
        if voxelbuffer[x, y, z] <> 0 then
        begin
          s.Write(x, SizeOf(y));
          s.Write(y, SizeOf(y));
          s.Write(voxelbuffer[x, y, z], SizeOf(voxelbuffer[x, y, z]));
        end;
  end;

  x := 0;
  y := 0;
  s.Write(x, SizeOf(x));
  s.Write(y, SizeOf(y));
  it := 0;
  s.Write(it, SizeOf(it));
//  s.Write(voxelbuffer^, SizeOf(voxelbuffer_t));
end;

procedure TForm1.DoLoadVoxelBinaryUndo(s: TStream);
var
  x, y, z: integer;
  it: voxelitem_t;
  ax: Char;
begin
  s.Read(fvoxelsize, SizeOf(fvoxelsize));
  s.Read(ax, SizeOf(ax));

  if ax = 'x' then
  begin
    s.Read(x, SizeOf(x));
    for y := 0 to fvoxelsize - 1 do
      for z := 0 to fvoxelsize - 1 do
        voxelbuffer[x, y, z] := 0;
    while true do
    begin
      s.Read(y, SizeOf(y));
      s.Read(z, SizeOf(z));
      s.Read(it, SizeOf(it));
      if it <> 0 then
        voxelbuffer[x, y, z] := it
      else
        break;
    end;
  end;

  if ax = 'y' then
  begin
    s.Read(y, SizeOf(y));
    for x := 0 to fvoxelsize - 1 do
      for z := 0 to fvoxelsize - 1 do
        voxelbuffer[x, y, z] := 0;
    while true do
    begin
      s.Read(x, SizeOf(x));
      s.Read(z, SizeOf(z));
      s.Read(it, SizeOf(it));
      if it <> 0 then
        voxelbuffer[x, y, z] := it
      else
        break;
    end;
  end;

  if ax = 'z' then
  begin
    s.Read(z, SizeOf(z));
    for x := 0 to fvoxelsize - 1 do
      for y := 0 to fvoxelsize - 1 do
        voxelbuffer[x, y, z] := 0;
    while true do
    begin
      s.Read(x, SizeOf(x));
      s.Read(y, SizeOf(y));
      s.Read(it, SizeOf(it));
      if it <> 0 then
        voxelbuffer[x, y, z] := it
      else
        break;
    end;
  end;

//  s.Read(voxelbuffer^, SizeOf(voxelbuffer_t));
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  glneedsupdate := true;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  glneedsupdate := true;
end;

procedure TForm1.UpdateStausbar;
begin
  StatusBar1.Panels[1].Text := Format('Camera(x=%2.2f, y=%2.2f, z=%2.2f)', [camera.x, camera.y, camera.z]);
end;

procedure TForm1.Options1Click(Sender: TObject);
var
  frm: TOptionsForm;
begin
  frm := TOptionsForm.Create(self);
  frm.ShowModal;
  if frm.ModalResult = mrOK then
  begin
  end;
  frm.Free;
end;

procedure TForm1.ExportCurrentLevelAsBitmap1Click(Sender: TObject);
var
  b: TBitmap;
  i, j: integer;
begin
  if SavePictureDialog1.Execute then
  begin
    b := TBitmap.Create;
    try
      b.PixelFormat := pf24bit;
      b.Width := fvoxelsize;
      b.Height := fvoxelsize;

      if xpos >= 0 then
      begin
        for i := 0 to fvoxelsize - 1 do
          for j := 0 to fvoxelsize - 1 do
            b.Canvas.Pixels[i, j] := voxelbuffer[xpos, i, j];
      end;

      if ypos >= 0 then
      begin
        for i := 0 to fvoxelsize - 1 do
          for j := 0 to fvoxelsize - 1 do
            b.Canvas.Pixels[i, j] := voxelbuffer[i, ypos, j];
      end;

     if zpos >= 0 then
     begin
       for i := 0 to fvoxelsize - 1 do
         for j := 0 to fvoxelsize - 1 do
           b.Canvas.Pixels[i, j] := voxelbuffer[i, j, zpos];
     end;

     b.SaveToFile(SavePictureDialog1.FileName);
    finally
      b.Free;
    end;
  end;
end;

procedure TForm1.KVXVoxel1Click(Sender: TObject);
begin
  if not CheckCanClose then
    Exit;

  if OpenDialog2.Execute then
  begin
    DoLoadKVX(OpenDialog2.FileName);
    ResetCamera(axis(xpos, ypos, zpos));
  end;
end;

procedure TForm1.Optimizedmesh1Click(Sender: TObject);
var
  vmo: TVoxelMeshOptimizer;
begin
  if SaveDialog2.Execute then
  begin
    Screen.Cursor := crHourGlass;
    vmo := TVoxelMeshOptimizer.Create;
    vmo.LoadVoxel(fvoxelsize, voxelbuffer);
    vmo.Optimize;
    vmo.SaveToFile(SaveDialog2.FileName);
    MessageBox(Handle, PChar(vmo.Message), PChar(rsTitle), MB_OK or MB_ICONINFORMATION or MB_APPLMODAL);
    vmo.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.BatchConvert1Click(Sender: TObject);
var
  i: integer;
  sf, st: string;
  frm: TProgressForm;
begin
  if OpenDialog3.Execute then
  begin
    frm := TProgressForm.Create(nil);
    frm.ProgressBar1.Max := OpenDialog3.Files.Count;
    frm.ProgressBar1.Position := 0;
    frm.Show;
    Screen.Cursor := crHourGlass;
    for i := 0 to OpenDialog3.Files.Count - 1 do
    begin
      frm.BringToFront;
      sf := OpenDialog3.Files.Strings[i];
      frm.Label1.Caption := 'Converting ' + MkSortName(sf);
      frm.Refresh;
      st := ChangeFileExt(sf, '.ddmesh');
      ConvertKVF2DDMESH(sf, st);
      frm.ProgressBar1.Position := frm.ProgressBar1.Position + 1;
      frm.Refresh;
    end;
    Screen.Cursor := crDefault;
    frm.Free;
  end;
end;

procedure TForm1.DDVOXtoDDMESH1Click(Sender: TObject);
var
  i: integer;
  sf, st: string;
  frm: TProgressForm;
begin
  if OpenDialog4.Execute then
  begin
    frm := TProgressForm.Create(nil);
    frm.ProgressBar1.Max := OpenDialog4.Files.Count;
    frm.ProgressBar1.Position := 0;
    frm.Show;
    Screen.Cursor := crHourGlass;
    for i := 0 to OpenDialog4.Files.Count - 1 do
    begin
      frm.BringToFront;
      sf := OpenDialog4.Files.Strings[i];
      frm.Label1.Caption := 'Converting ' + MkSortName(sf);
      frm.Refresh;
      st := ChangeFileExt(sf, '.ddmesh');
      ConvertDDVOX2DDMESH(sf, st);
      frm.ProgressBar1.Position := frm.ProgressBar1.Position + 1;
      frm.Refresh;
    end;
    Screen.Cursor := crDefault;
    frm.Free;
  end;
end;

procedure TForm1.DDMESH1Click(Sender: TObject);
begin
  if not CheckCanClose then
    Exit;

  if OpenDialog5.Execute then
  begin
    DoLoadDDMESH(OpenDialog5.FileName);
    ResetCamera(axis(xpos, ypos, zpos));
  end;
end;

end.
