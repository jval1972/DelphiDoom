//------------------------------------------------------------------------------
//
//  DelphiDoom is a source port of the game Doom and it is
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2022 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//  DESCRIPTION:
//  DDMODEL loader
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit mdl_script_model;

interface

uses
  d_delphi,
  dglOpenGL;

type
  modelcmd_t = record
    cmd: integer;
    params: array[0..3] of GLfloat;
    frame: integer;
  end;
  modelcmd_p = ^modelcmd_t;
  modelcmd_a = array[0..$FFF] of modelcmd_t;
  modelcmd_pa = ^modelcmd_a;

const
  C_glBegin = 0;
  C_glEnd = 1;
  C_glTexCoord2f = 2;
  C_glVertex3f = 3;
  C_glColor3f = 4;
  C_glColor4f = 5;
  C_glNormal3f = 6;
  C_glMatrixMode = 7;
  C_glPushMatrix = 8;
  C_glPopMatrix = 9;
  C_glTranslatef = 10;
  C_glRotatef = 11;
  C_glLoadIdentity = 12;
  C_glScalef = 13;
  C_SetFrame = 14;
  C_CallFrame = 15;

const
  MDL_MAGIC = $1;

type
  TDDModelLoader = class(TObject)
  private
    fNumCmds: integer;
    fRealNumCmds: integer;
    fCmds: modelcmd_pa;
    fcurrentframe: integer;
    frecursiondepth: integer;
    fmaxrecursiondepth: integer;
    frecursionframes: TDNumberList;
    FFrames: TDHashNumberList;
    function Grow: modelcmd_p;
  protected
    procedure AddCmd(const cmd: integer; const parm0: Single = 0.0;
      const parm1: Single = 0.0; const parm2: Single = 0.0; const parm3: Single = 0.0);
    procedure DoRenderFrame(const frm: integer); virtual;
    procedure DoRenderTextureMask(const frm: integer; const W, H: integer); virtual;
    procedure IdentifyFrames;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function LoadFromScript(const aScript: string): boolean;
    function AppendFromScript(const aScript: string): boolean;
    function LoadFromStream(const aStream: TDStream): boolean;
    function AppendFromStream(const aStream: TDStream): boolean;
    function LoadFromFile(const aFileName: string): boolean;
    function AppendFromFile(const aFileName: string): boolean;
    procedure SaveToStream(const aStream: TDStream);
    procedure SaveToFile(const aFileName: string);
    procedure RenderFrame(const frm: integer);
    procedure RenderTextureMask(const frm: integer; const W, H: integer);
    property MaxRecursionDepth: Integer read fmaxrecursiondepth write fmaxrecursiondepth;
    property CurrentFrame: Integer read fcurrentframe write fcurrentframe;
    property Frames: TDHashNumberList read FFrames;
  end;

//==============================================================================
//
// MDLS_glBegin
//
//==============================================================================
procedure MDLS_glBegin(const mode: GLenum);

//==============================================================================
//
// MDLS_glEnd
//
//==============================================================================
procedure MDLS_glEnd;

//==============================================================================
//
// MDLS_glTexCoord2f
//
//==============================================================================
procedure MDLS_glTexCoord2f(const s, t: GLfloat);

//==============================================================================
//
// MDLS_glVertex3f
//
//==============================================================================
procedure MDLS_glVertex3f(const x, y, z: GLfloat);

//==============================================================================
//
// MDLS_glColor3f
//
//==============================================================================
procedure MDLS_glColor3f(const r, g, b: GLfloat);

//==============================================================================
//
// MDLS_glColor4f
//
//==============================================================================
procedure MDLS_glColor4f(const r, g, b, a: GLfloat);

//==============================================================================
//
// MDLS_glNormal3f
//
//==============================================================================
procedure MDLS_glNormal3f(const nx, ny, nz: GLfloat);

//==============================================================================
//
// MDLS_glMatrixMode
//
//==============================================================================
procedure MDLS_glMatrixMode(const mode: LongWord);

//==============================================================================
//
// MDLS_glPushMatrix
//
//==============================================================================
procedure MDLS_glPushMatrix;

//==============================================================================
//
// MDLS_glPopMatrix
//
//==============================================================================
procedure MDLS_glPopMatrix;

//==============================================================================
//
// MDLS_glTranslatef
//
//==============================================================================
procedure MDLS_glTranslatef(const x, y, z: GLfloat);

//==============================================================================
//
// MDLS_glRotatef
//
//==============================================================================
procedure MDLS_glRotatef(const a, x, y, z: GLfloat);

//==============================================================================
//
// MDLS_glLoadIdentity
//
//==============================================================================
procedure MDLS_glLoadIdentity;

//==============================================================================
//
// MDLS_glScalef
//
//==============================================================================
procedure MDLS_glScalef(const x, y, z: GLfloat);

//==============================================================================
//
// MDLS_SetFrame
//
//==============================================================================
procedure MDLS_SetFrame(const frm: integer);

//==============================================================================
//
// MDLS_CallFrame
//
//==============================================================================
procedure MDLS_CallFrame(const frm: integer);

implementation

uses
  mdl_script,
  mdl_script_functions;

var
  currentmodelloader: TDDModelLoader;

//==============================================================================
//
// TDDModelLoader.Create
//
//==============================================================================
constructor TDDModelLoader.Create;
begin
  fNumCmds := 0;
  fRealNumCmds := 0;
  fCmds := nil;
  fcurrentframe := 0;
  frecursiondepth := 0;
  fmaxrecursiondepth := 20;
  FFrames := TDHashNumberList.Create;
  frecursionframes := TDNumberList.Create;
  inherited;
end;

//==============================================================================
//
// TDDModelLoader.Destroy
//
//==============================================================================
destructor TDDModelLoader.Destroy;
begin
  Clear;
  FFrames.Free;
  frecursionframes.Free;
  inherited;
end;

//==============================================================================
//
// TDDModelLoader.Grow
//
//==============================================================================
function TDDModelLoader.Grow: modelcmd_p;
begin
  Inc(fNumCmds);
  if fNumCmds >= fRealNumCmds then
  begin
    fRealNumCmds := fRealNumCmds + 16;
    ReallocMem(fCmds, fRealNumCmds * SizeOf(modelcmd_t));
  end;
  Result := @fCmds[fNumCmds - 1];
end;

//==============================================================================
//
// TDDModelLoader.AddCmd
//
//==============================================================================
procedure TDDModelLoader.AddCmd(const cmd: integer; const parm0: Single = 0.0;
  const parm1: Single = 0.0; const parm2: Single = 0.0; const parm3: Single = 0.0);
var
  pc: modelcmd_p;
begin
  pc := Grow;
  pc.cmd := cmd;
  pc.params[0] := parm0;
  pc.params[1] := parm1;
  pc.params[2] := parm2;
  pc.params[3] := parm3;
  pc.frame := fcurrentframe;
end;

//==============================================================================
//
// TDDModelLoader.Clear
//
//==============================================================================
procedure TDDModelLoader.Clear;
begin
  ReallocMem(fCmds, 0);
  fNumCmds := 0;
  fRealNumCmds := 0;
  fcurrentframe := 0;
  frecursiondepth := 0;
  FFrames.Clear;
end;

//==============================================================================
//
// TDDModelLoader.LoadFromScript
//
//==============================================================================
function TDDModelLoader.LoadFromScript(const aScript: string): boolean;
begin
  Clear;
  Result := AppendFromScript(aScript);
end;

//==============================================================================
//
// TDDModelLoader.AppendFromScript
//
//==============================================================================
function TDDModelLoader.AppendFromScript(const aScript: string): boolean;
begin
  currentmodelloader := Self;
  Result := MDL_ExecuteScript(aScript);
  IdentifyFrames;
end;

//==============================================================================
//
// TDDModelLoader.LoadFromStream
//
//==============================================================================
function TDDModelLoader.LoadFromStream(const aStream: TDStream): boolean;
begin
  Clear;
  Result := AppendFromStream(aStream);
end;

//==============================================================================
//
// TDDModelLoader.AppendFromStream
//
//==============================================================================
function TDDModelLoader.AppendFromStream(const aStream: TDStream): boolean;
var
  header: integer;
  sz: integer;
begin
  aStream.Read(header, SizeOf(integer));
  if header <> MDL_MAGIC then
  begin
    Result := False;
    Exit;
  end;
  aStream.Read(sz, SizeOf(integer));
  if sz < 0 then
  begin
    Result := False;
    Exit;
  end;
  fNumCmds := sz;
  fRealNumCmds := sz;
  ReallocMem(fCmds, fNumCmds * SizeOf(modelcmd_t));
  aStream.Read(fCmds^, fNumCmds * SizeOf(modelcmd_t));
  IdentifyFrames;
  Result := True;
end;

//==============================================================================
//
// TDDModelLoader.LoadFromFile
//
//==============================================================================
function TDDModelLoader.LoadFromFile(const aFileName: string): boolean;
var
  fs: TFile;
begin
  fs := TFile.Create(aFileName, fOpenReadWrite);
  try
    Result := LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

//==============================================================================
//
// TDDModelLoader.AppendFromFile
//
//==============================================================================
function TDDModelLoader.AppendFromFile(const aFileName: string): boolean;
var
  fs: TFile;
begin
  fs := TFile.Create(aFileName, fOpenReadWrite);
  try
    Result := AppendFromStream(fs);
  finally
    fs.Free;
  end;
end;

//==============================================================================
//
// TDDModelLoader.SaveToStream
//
//==============================================================================
procedure TDDModelLoader.SaveToStream(const aStream: TDStream);
var
  header: integer;
  sz: integer;
begin
  header := MDL_MAGIC;
  aStream.Write(header, SizeOf(integer));
  sz := fNumCmds;
  aStream.Write(sz, SizeOf(integer));
  aStream.Write(fCmds^, sz * SizeOf(modelcmd_t));
end;

//==============================================================================
//
// TDDModelLoader.SaveToFile
//
//==============================================================================
procedure TDDModelLoader.SaveToFile(const aFileName: string);
var
  fs: TFile;
begin
  fs := TFile.Create(aFileName, fCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

//==============================================================================
//
// TDDModelLoader.RenderFrame
//
//==============================================================================
procedure TDDModelLoader.RenderFrame(const frm: integer);
begin
  frecursiondepth := 0;
  frecursionframes.Clear;
  DoRenderFrame(frm);
end;

//==============================================================================
//
// TDDModelLoader.DoRenderFrame
//
//==============================================================================
procedure TDDModelLoader.DoRenderFrame(const frm: integer);
var
  i: integer;
  pc: modelcmd_p;
  idx: integer;
begin
  if frecursiondepth >= fmaxrecursiondepth then
    Exit;
  if frecursionframes.IndexOf(frm) >= 0 then
    Exit;
  frecursionframes.Add(frm);

  Inc(frecursiondepth);
  for i := 0 to fNumCmds - 1 do
  begin
    pc := @fCmds[i];
    if frm <> pc.frame then
      Continue;

    case pc.cmd of
      C_glBegin:
        glBegin(Round(pc.params[0]));
      C_glEnd:
        glEnd;
      C_glTexCoord2f:
        glTexCoord2f(pc.params[0], pc.params[1]);
      C_glVertex3f:
        glVertex3f(pc.params[0], pc.params[1], pc.params[2]);
      C_glColor3f:
        glColor3f(pc.params[0], pc.params[1], pc.params[2]);
      C_glColor4f:
        glColor4f(pc.params[0], pc.params[1], pc.params[2], pc.params[3]);
      C_glNormal3f:
        glNormal3f(pc.params[0], pc.params[1], pc.params[2]);
      C_glMatrixMode:
        glMatrixMode(Round(pc.params[0]));
      C_glPushMatrix:
        glPushMatrix;
      C_glPopMatrix:
        glPopMatrix;
      C_glTranslatef:
        glTranslatef(pc.params[0], pc.params[1], pc.params[2]);
      C_glRotatef:
        glRotatef(pc.params[0], pc.params[1], pc.params[2], pc.params[3]);
      C_glLoadIdentity:
        glLoadIdentity;
      C_glScalef:
        glScalef(pc.params[0], pc.params[1], pc.params[2]);
      C_CallFrame:
        DoRenderFrame(Round(pc.params[0]));
    end;
  end;

  idx := frecursionframes.IndexOf(frm);
  if idx >= 0 then
    frecursionframes.Delete(idx);

  Dec(frecursiondepth);
end;

//==============================================================================
//
// TDDModelLoader.RenderTextureMask
//
//==============================================================================
procedure TDDModelLoader.RenderTextureMask(const frm: integer; const W, H: integer);
begin
  frecursiondepth := 0;
  frecursionframes.Clear;
  DoRenderTextureMask(frm, W, H);
end;

//==============================================================================
//
// TDDModelLoader.DoRenderTextureMask
//
//==============================================================================
procedure TDDModelLoader.DoRenderTextureMask(const frm: integer; const W, H: integer);
var
  x, y: GLfloat;
  i: integer;
  pc: modelcmd_p;
  idx: integer;
begin
  if frecursiondepth >= fmaxrecursiondepth then
    Exit;
  if frecursionframes.IndexOf(frm) >= 0 then
    Exit;
  frecursionframes.Add(frm);

  Inc(frecursiondepth);
  x := 0.0;
  y := 0.0;
  for i := 0 to fNumCmds - 1 do
  begin
    pc := @fCmds[i];
    if pc.frame <> frm then
      Continue;
    case pc.cmd of
      C_glBegin:
        glBegin(Round(pc.params[0]));
      C_glEnd:
        glEnd;
      C_glTexCoord2f:
        begin
          x := pc.params[0];
          y := pc.params[1];
          glTexCoord2f(x, y);
        end;
      C_glVertex3f:
        glVertex2i(Round(x * W), Round(H - y * H));
      C_CallFrame:
        DoRenderTextureMask(Round(pc.params[0]), W, H);
    end;
  end;

  idx := frecursionframes.IndexOf(frm);
  if idx >= 0 then
    frecursionframes.Delete(idx);

  Dec(frecursiondepth);
end;

//==============================================================================
//
// TDDModelLoader.IdentifyFrames
//
//==============================================================================
procedure TDDModelLoader.IdentifyFrames;
var
  i: integer;
  pc: modelcmd_p;
  frm: integer;
begin
  FFrames.Clear;
  for i := 0 to fNumCmds - 1 do
  begin
    pc := @fCmds[i];
    if FFrames.IndexOf(pc.frame) < 0 then
      FFrames.Add(pc.frame);
    if pc.cmd = C_CallFrame then
    begin
      frm := Round(pc.params[0]);
      if FFrames.IndexOf(frm) < 0 then
        FFrames.Add(frm);
    end;
  end;
  FFrames.Sort;
end;

//==============================================================================
// MDLS_glBegin
//
//-------------------------- PascalScript Functions ----------------------------
//
//==============================================================================
procedure MDLS_glBegin(const mode: GLenum);
begin
  if currentmodelloader = nil then
  begin
    printf('MDLS_glBegin(): No model loader available'#13#10);
    Exit;
  end;
  currentmodelloader.AddCmd(C_glBegin, mode);
end;

//==============================================================================
//
// MDLS_glEnd
//
//==============================================================================
procedure MDLS_glEnd;
begin
  if currentmodelloader = nil then
  begin
    printf('MDLS_glEnd(): No model loader available'#13#10);
    Exit;
  end;
  currentmodelloader.AddCmd(C_glEnd);
end;

//==============================================================================
//
// MDLS_glTexCoord2f
//
//==============================================================================
procedure MDLS_glTexCoord2f(const s, t: GLfloat);
begin
  if currentmodelloader = nil then
  begin
    printf('MDLS_glTexCoord2f(): No model loader available'#13#10);
    Exit;
  end;
  currentmodelloader.AddCmd(C_glTexCoord2f, s, t);
end;

//==============================================================================
//
// MDLS_glVertex3f
//
//==============================================================================
procedure MDLS_glVertex3f(const x, y, z: GLfloat);
begin
  if currentmodelloader = nil then
  begin
    printf('MDLS_glVertex3f(): No model loader available'#13#10);
    Exit;
  end;
  currentmodelloader.AddCmd(C_glVertex3f, x, y, z);
end;

//==============================================================================
//
// MDLS_glColor3f
//
//==============================================================================
procedure MDLS_glColor3f(const r, g, b: GLfloat);
begin
  if currentmodelloader = nil then
  begin
    printf('MDLS_glColor3f(): No model loader available'#13#10);
    Exit;
  end;
  currentmodelloader.AddCmd(C_glColor3f, r, g, b);
end;

//==============================================================================
//
// MDLS_glColor4f
//
//==============================================================================
procedure MDLS_glColor4f(const r, g, b, a: GLfloat);
begin
  if currentmodelloader = nil then
  begin
    printf('MDLS_glColor4f(): No model loader available'#13#10);
    Exit;
  end;
  currentmodelloader.AddCmd(C_glColor4f, r, g, b, a);
end;

//==============================================================================
//
// MDLS_glNormal3f
//
//==============================================================================
procedure MDLS_glNormal3f(const nx, ny, nz: GLfloat);
begin
  if currentmodelloader = nil then
  begin
    printf('MDLS_glNormal3f(): No model loader available'#13#10);
    Exit;
  end;
  currentmodelloader.AddCmd(C_glNormal3f, nx, ny, nz);
end;

//==============================================================================
//
// MDLS_glMatrixMode
//
//==============================================================================
procedure MDLS_glMatrixMode(const mode: LongWord);
begin
  if currentmodelloader = nil then
  begin
    printf('MDLS_glMatrixMode(): No model loader available'#13#10);
    Exit;
  end;
  currentmodelloader.AddCmd(C_glMatrixMode, mode);
end;

//==============================================================================
//
// MDLS_glPushMatrix
//
//==============================================================================
procedure MDLS_glPushMatrix;
begin
  if currentmodelloader = nil then
  begin
    printf('MDLS_glPushMatrix(): No model loader available'#13#10);
    Exit;
  end;
  currentmodelloader.AddCmd(C_glPushMatrix);
end;

//==============================================================================
//
// MDLS_glPopMatrix
//
//==============================================================================
procedure MDLS_glPopMatrix;
begin
  if currentmodelloader = nil then
  begin
    printf('MDLS_glPopMatrix(): No model loader available'#13#10);
    Exit;
  end;
  currentmodelloader.AddCmd(C_glPopMatrix);
end;

//==============================================================================
//
// MDLS_glTranslatef
//
//==============================================================================
procedure MDLS_glTranslatef(const x, y, z: GLfloat);
begin
  if currentmodelloader = nil then
  begin
    printf('MDLS_glTranslatef(): No model loader available'#13#10);
    Exit;
  end;
  currentmodelloader.AddCmd(C_glTranslatef, x, y, z);
end;

//==============================================================================
//
// MDLS_glRotatef
//
//==============================================================================
procedure MDLS_glRotatef(const a, x, y, z: GLfloat);
begin
  if currentmodelloader = nil then
  begin
    printf('MDLS_glRotatef(): No model loader available'#13#10);
    Exit;
  end;
  currentmodelloader.AddCmd(C_glRotatef, a, x, y, z);
end;

//==============================================================================
//
// MDLS_glLoadIdentity
//
//==============================================================================
procedure MDLS_glLoadIdentity;
begin
  if currentmodelloader = nil then
  begin
    printf('MDLS_glLoadIdentity(): No model loader available'#13#10);
    Exit;
  end;
  currentmodelloader.AddCmd(C_glLoadIdentity);
end;

//==============================================================================
//
// MDLS_glScalef
//
//==============================================================================
procedure MDLS_glScalef(const x, y, z: GLfloat);
begin
  if currentmodelloader = nil then
  begin
    printf('MDLS_glScalef(): No model loader available'#13#10);
    Exit;
  end;
  currentmodelloader.AddCmd(C_glScalef, x, y, z);
end;

//==============================================================================
//
// MDLS_SetFrame
//
//==============================================================================
procedure MDLS_SetFrame(const frm: integer);
begin
  if currentmodelloader = nil then
  begin
    printf('MDLS_SetFrame(): No model loader available'#13#10);
    Exit;
  end;
  currentmodelloader.CurrentFrame := frm;
  currentmodelloader.AddCmd(C_SetFrame, frm);
end;

//==============================================================================
//
// MDLS_CallFrame
//
//==============================================================================
procedure MDLS_CallFrame(const frm: integer);
begin
  if currentmodelloader = nil then
  begin
    printf('MDLS_CallFrame(): No model loader available'#13#10);
    Exit;
  end;
  currentmodelloader.AddCmd(C_CallFrame, frm);
end;

end.
