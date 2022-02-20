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
//    External MD2 model support
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit mdl_md2;

interface

uses
  d_delphi,
  mdl_base,
  gl_types;

type
  TMD2Model = class(TBaseModel)
  protected
    fNumFrames: integer;
    fNumVertexes: integer;
    frameNames: TDStringList;
    TheVectorsArray: PGLVertexArraysP;
    UV: PGLTexcoordArray;
    precalc: PGLuintArray;
    fname: string;
  public
    constructor Create(const name: string;
      const xoffset, yoffset, zoffset: float;
      const xscale, yscale, zscale: float;
      const additionalframes: TDStringList); override;
    destructor Destroy; override;
    function MergeFrames(const m: TBaseModel): boolean; virtual;
    procedure Draw(const frm1, frm2: integer; const offset: float); override;
    procedure DrawSimple(const frm: integer); override;
    function StartFrame(const i: integer): integer; overload; virtual;
    function EndFrame(const i: integer): integer; overload; virtual;
    function StartFrame(const frame: string): integer; overload; virtual;
    function EndFrame(const frame: string): integer; overload; virtual;
    function FrameName(const i: integer): string; virtual;
    function FrameIndex(const frame: string): integer; virtual;
  end;

implementation

uses
  dglOpenGL,
  doomdef,
  i_system,
  w_folders,
  w_pak,
  gl_defs;

//------------------------------------------------------------------------------
//--------------------------- MD2 File Format ----------------------------------
//------------------------------------------------------------------------------

const
  // Magic number that identifies MD2 files (ASCII: 'IDP2').
  MD2_MAGIC = $32504449;

type
  TMD2_Index_List = record
    a, b, c: Integer;
    a_s, a_t,
    b_s, b_t,
    c_s, c_t: Single;
  end;
  TMD2_Index_List_Array = array[0..$FFFF] of TMD2_Index_List;
  PMD2_Index_List_Array = ^TMD2_Index_List_Array;

  TMD2_Vertex_List = record
    x, y, z: Single;
  end;
  TMD2_Vertex_List_Array = array[0..$FFFF] of TMD2_Vertex_List;
  PMD2_Vertex_List_Array = ^TMD2_Vertex_List_Array;

  TMD2_Frame_List = record
    Vertex: PMD2_Vertex_List_Array;
  end;
  TMD2_Frame_List_Array = array[0..$FFFF] of TMD2_Frame_List;
  PMD2_Frame_List_Array = ^TMD2_Frame_List_Array;

  TMD2DstVert_T = record
    s: SmallInt;
    t: SmallInt;
  end;
  TMD2DstVert_TArray = array[0..$FFFF] of TMD2DstVert_T;
  PMD2DstVert_TArray = ^TMD2DstVert_TArray;

  TMD2Triangle_T = record
    index_xyz: array[0..2] of SmallInt;
    index_st: array[0..2] of SmallInt;
  end;

  TMD2Trivertx_T = record
    v: array[0..2] of Byte;
    lightnormalindex: byte;
  end;

  PMD2AliasFrame_T = ^TMD2AliasFrame_T;
  TMD2AliasFrame_T = record
    scale: array[0..2] of Single;
    translate: array[0..2] of Single;
    name: array[0..15] of Char;
    verts: array[0..0] of TMD2Trivertx_T;
  end;

  TDmd2_T = record
    ident: Integer;
    version: Integer;

    skinWidth: Integer;
    skinHeight: Integer;
    framesize: Integer;

    num_skins: Integer;
    num_xyz: Integer;
    num_st: Integer;
    num_tris: Integer;
    num_glcmds: Integer;
    num_frames: Integer;

    ofs_skins: Integer;
    ofs_st: Integer;
    ofs_tris: Integer;
    ofs_frames: Integer;
    ofs_glcmds: Integer;
    ofs_end: Integer;
  end;

//==============================================================================
//
// TMD2Model.Create
//
//==============================================================================
constructor TMD2Model.Create(const name: string;
  const xoffset, yoffset, zoffset: float;
  const xscale, yscale, zscale: float;
  const additionalframes: TDStringList);
var
  strm: TPakStream;
  base_st: PMD2DstVert_TArray;
  tri: TMD2Triangle_T;
  out_t: PMD2AliasFrame_T;
  i, j, k, idx1: Integer;
  vert: PGLVertex;
  frameName: string;
  m_index_list: PMD2_Index_List_Array;
  m_frame_list: PMD2_Frame_List_Array;
  fm_iTriangles: integer;
  fxoffset, fyoffset, fzoffset: single;
  fxscale, fyscale, fzscale: single;
  modelheader: TDmd2_T;
  m: TMD2Model;
begin
  Inherited;
  printf('  Found external model %s'#13#10, [name]);
  fname := name;
  frameNames := TDStringList.Create;
  UV := nil;
  TheVectorsArray := nil;
  fNumFrames := 0;
  fNumVertexes := 0;
  fxoffset := xoffset / MAP_COEFF;
  fyoffset := yoffset / MAP_COEFF;
  fzoffset := zoffset / MAP_COEFF;
  fxscale := xscale / MAP_COEFF;
  fyscale := yscale / MAP_COEFF;
  fzscale := zscale / MAP_COEFF;
  strm := TPakStream.Create(fname, pm_prefered, gamedirectories);
  if strm.IOResult <> 0 then
  begin
    strm.Free;
    strm := TPakStream.Create(fname, pm_short, '', FOLDER_MODELS);
  end;
  if strm.IOResult <> 0 then
    I_Error('TMD2Model.Create(): Can not find model %s!', [fname]);

  strm.Read(modelheader, SizeOf(modelheader));

  if modelheader.ident <> MD2_MAGIC then
    I_Error('TMD2Model.Create(): Invalid MD2 model magic (%d)!', [modelheader.ident]);

  fNumFrames := modelheader.num_frames;
  fm_iTriangles := modelheader.num_tris;
  fNumVertexes := fm_iTriangles * 3;

  m_index_list := malloc(SizeOf(TMD2_Index_List) * modelheader.num_tris);
  m_frame_list := malloc(SizeOf(TMD2_Frame_List) * modelheader.num_frames);

  for i := 0 to modelheader.num_frames - 1 do
    m_frame_list[i].vertex := malloc(SizeOf(TMD2_Vertex_List) * modelheader.num_xyz);

  strm.Seek(modelheader.ofs_st, sFromBeginning);
  base_st := malloc(modelheader.num_st * SizeOf(base_st[0]));
  strm.Read(base_st^, modelheader.num_st * SizeOf(base_st[0]));

  for i := 0 to modelheader.num_tris - 1 do
  begin
    strm.Read(Tri, SizeOf(TMD2Triangle_T));
    with m_index_list[i] do
    begin
      a := tri.index_xyz[2];
      b := tri.index_xyz[1];
      c := tri.index_xyz[0];
      a_s := base_st[tri.index_st[2]].s / modelheader.skinWidth;
      a_t := base_st[tri.index_st[2]].t / modelheader.skinHeight;
      b_s := base_st[tri.index_st[1]].s / modelheader.skinWidth;
      b_t := base_st[tri.index_st[1]].t / modelheader.skinHeight;
      c_s := base_st[tri.index_st[0]].s / modelheader.skinWidth;
      c_t := base_st[tri.index_st[0]].t / modelheader.skinHeight;
    end;
  end;
  memfree(pointer(base_st), modelheader.num_st * SizeOf(base_st[0]));

  out_t := malloc(modelheader.framesize);
  for i := 0 to modelheader.num_frames - 1 do
  begin
    strm.Read(out_t^, modelheader.framesize);
    frameName := strupper(strtrim(out_t^.name));
    if Copy(frameName, Length(frameName) - 1, 1)[1] in ['0'..'9'] then
      frameName := Copy(frameName, 1, Length(frameName) - 2)
    else
      frameName := Copy(frameName, 1, Length(frameName) - 1);
    if frameNames.IndexOf(frameName) < 0 then
    begin
      frameNames.AddObject(frameName, TFrameIndexInfo.Create);
      (frameNames.Objects[frameNames.Count - 1] as TFrameIndexInfo).StartFrame := i;
      (frameNames.Objects[frameNames.Count - 1] as TFrameIndexInfo).EndFrame := i;
    end
    else
      (frameNames.Objects[frameNames.IndexOf(frameName)] as TFrameIndexInfo).EndFrame := i;

    for j := 0 to modelheader.num_xyz - 1 do
    begin
      with m_frame_list[i].vertex[j] do
      begin
        x := (out_t^.verts[j].v[0] * out_t^.scale[0] + out_t^.translate[0]) * fxscale + fxoffset;
        y := (out_t^.verts[j].v[1] * out_t^.scale[1] + out_t^.translate[1]) * fyscale + fyoffset;
        z := (out_t^.verts[j].v[2] * out_t^.scale[2] + out_t^.translate[2]) * fzscale + fzoffset;
      end;
    end;
  end;
  memfree(pointer(out_t), modelheader.framesize);

  UV := malloc(fNumVertexes * SizeOf(GLTexcoord));
  TheVectorsArray := malloc(fNumFrames * SizeOf(PGLVertexArray));

  k := 0;
  for j := 0 to fm_iTriangles - 1 do
  begin
    UV[k].u := m_index_list[j].a_s;
    UV[k].v := m_index_list[j].a_t;
    inc(k);

    UV[k].u := m_index_list[j].b_s;
    UV[k].v := m_index_list[j].b_t;
    inc(k);

    UV[k].u := m_index_list[j].c_s;
    UV[k].v := m_index_list[j].c_t;
    inc(k);
  end;

  for i := 0 to fNumFrames - 1 do
  begin
    TheVectorsArray[i] := malloc(fNumVertexes * SizeOf(TGLVectorf3));
    vert := @TheVectorsArray[i][0];
    for j := 0 to fm_iTriangles - 1 do
    begin
      idx1 := m_index_list[j].a;
      vert.x := m_frame_list[i].vertex[idx1].y;
      vert.y := m_frame_list[i].vertex[idx1].z;
      vert.z := m_frame_list[i].vertex[idx1].x;
      inc(vert);

      idx1 := m_index_list[j].b;
      vert.x := m_frame_list[i].vertex[idx1].y;
      vert.y := m_frame_list[i].vertex[idx1].z;
      vert.z := m_frame_list[i].vertex[idx1].x;
      inc(vert);

      idx1 := m_index_list[j].c;
      vert.x := m_frame_list[i].vertex[idx1].y;
      vert.y := m_frame_list[i].vertex[idx1].z;
      vert.z := m_frame_list[i].vertex[idx1].x;
      inc(vert);
    end;
  end;

  for i := 0 to fNumFrames - 1 do
    memfree(pointer(m_frame_list[i].vertex), SizeOf(TMD2_Vertex_List) * modelheader.num_xyz);
  memfree(pointer(m_frame_list), SizeOf(TMD2_Frame_List) * modelheader.num_frames);
  memfree(pointer(m_index_list), SizeOf(TMD2_Index_List) * modelheader.num_tris);

  precalc := mallocz(fNumFrames * SizeOf(GLuint));

  strm.Free;

  if additionalframes <> nil then
    for i := 0 to additionalframes.Count - 1 do
    begin
      m := TMD2Model.Create(additionalframes.strings[i],
        xoffset, yoffset, zoffset,
        xscale, yscale, zscale, nil);
      if not MergeFrames(m) then
        I_Error('TMD2Model.MergeFrames(): Can not merge model "%s" into model "%s"', [additionalframes.strings[i], fname]);
      m.Free;
    end;

end;

//==============================================================================
//
// TMD2Model.MergeFrames
//
//==============================================================================
function TMD2Model.MergeFrames(const m: TBaseModel): boolean;
var
  i: integer;
  model: TMD2Model;
begin
  if not (m is TMD2Model) then
  begin
    Result := False;
    Exit;
  end;

  model := m as TMD2Model;
  if model.fNumVertexes <> fNumVertexes then
  begin
    Result := False;
    Exit;
  end;

  for i := 0 to model.frameNames.Count - 1 do
  begin
    frameNames.AddObject(model.frameNames.Strings[i], TFrameIndexInfo.Create);
      (frameNames.Objects[frameNames.Count - 1] as TFrameIndexInfo).StartFrame := (model.frameNames.Objects[i] as TFrameIndexInfo).StartFrame + fNumFrames;
      (frameNames.Objects[frameNames.Count - 1] as TFrameIndexInfo).EndFrame := (model.frameNames.Objects[i] as TFrameIndexInfo).EndFrame + fNumFrames;
  end;

  realloc(pointer(TheVectorsArray),
    fNumFrames * SizeOf(PGLVertexArray),
    (model.fNumFrames + fNumFrames) * SizeOf(PGLVertexArray));

  realloc(pointer(precalc),
    fNumFrames * SizeOf(GLuint),
    (model.fNumFrames + fNumFrames) * SizeOf(GLuint));

  for i := fNumFrames to model.fNumFrames + fNumFrames - 1 do
  begin
    precalc[i] := model.precalc[fNumFrames - i];
    TheVectorsArray[i] := malloc(fNumVertexes * SizeOf(TGLVectorf3));
    memcpy(TheVectorsArray[i], model.TheVectorsArray[fNumFrames - i], fNumVertexes * SizeOf(TGLVectorf3));
  end;

  fNumFrames := model.fNumFrames + fNumFrames;

  Result := True;
end;

//==============================================================================
//
// TMD2Model.Destroy
//
//==============================================================================
destructor TMD2Model.Destroy;
var
  i: integer;
begin
  for i := 0 to fNumFrames - 1 do
    if precalc[i] > 0 then
      glDeleteLists(precalc[i], 1);
  memfree(pointer(precalc), fNumFrames * SizeOf(GLuint));
  memfree(pointer(UV), fNumVertexes * SizeOf(GLTexcoord));
  for i := 0 to fNumFrames - 1 do
    memfree(pointer(TheVectorsArray[i]), fNumVertexes * SizeOf(TGLVectorf3));
  memfree(pointer(TheVectorsArray), fNumFrames * SizeOf(GLVertexArraysP));

  for i := 0 to frameNames.Count - 1 do
    frameNames.Objects[i].Free;
  frameNames.Free;

  Inherited;
end;

//==============================================================================
//
// TMD2Model.Draw
//
//==============================================================================
procedure TMD2Model.Draw(const frm1, frm2: integer; const offset: float);
var
  w2: float;
  v1, v2, mark: PGLVertex;
  x, y, z: float;
  coord: PGLTexcoord;
begin
{$IFDEF DEBUG}
  printf('gametic=%d, frm1=%d, frm2=%d, offset=%2.2f'#13#10, [gametic, frm1, frm2, offset]);
{$ENDIF}
  if (frm1 = frm2) or (frm2 < 0) or (offset < 0.01) then
  begin
    DrawSimple(frm1);
    exit;
  end;

  if offset > 0.99 then
  begin
    DrawSimple(frm2);
    exit;
  end;

  w2 := 1.0 - offset;

  v1 := @TheVectorsArray[frm1][0];
  mark := @TheVectorsArray[frm1][fNumVertexes];
  v2 := @TheVectorsArray[frm2][0];
  coord := @UV[0];

  glBegin(GL_TRIANGLES);
    while integer(v1) < integer(mark) do
    begin
      x := v1.x * w2 + v2.x * offset;
      y := v1.y * w2 + v2.y * offset;
      z := v1.z * w2 + v2.z * offset;
      glTexCoord2fv(@coord.u);
      glVertex3f(x, y, z);
      inc(v1);
      inc(v2);
      inc(coord);
    end;
  glEnd;
end;

//==============================================================================
//
// TMD2Model.DrawSimple
//
//==============================================================================
procedure TMD2Model.DrawSimple(const frm: integer);
var
  i: integer;
begin
  if precalc[frm] > 0 then
    glCallList(precalc[frm])
  else
  begin
    precalc[frm] := glGenLists(1);

    glNewList(precalc[frm], GL_COMPILE_AND_EXECUTE);

      glBegin(GL_TRIANGLES);
        for i := 0 to fNumVertexes - 1 do
        begin
          glTexCoord2f(UV[i].u, UV[i].v);
          glVertex3fv(@TheVectorsArray[frm][i]);
        end;
      glEnd;

    glEndList;
  end;
end;

//==============================================================================
//
// TMD2Model.StartFrame
//
//==============================================================================
function TMD2Model.StartFrame(const i: integer): integer;
begin
  result := -1;
  if IsIntegerInRange(i, 0, frameNames.Count - 1) then
    if frameNames.Objects[i] <> nil then
      result := (frameNames.Objects[i] as TFrameIndexInfo).StartFrame;
end;

//==============================================================================
//
// TMD2Model.EndFrame
//
//==============================================================================
function TMD2Model.EndFrame(const i: integer): integer;
begin
  result := -1;
  if IsIntegerInRange(i, 0, frameNames.Count - 1) then
    if frameNames.Objects[i] <> nil then
      result := (frameNames.Objects[i] as TFrameIndexInfo).EndFrame;
end;

//==============================================================================
//
// TMD2Model.StartFrame
//
//==============================================================================
function TMD2Model.StartFrame(const frame: string): integer;
begin
  result := StartFrame(frameNames.IndexOf(frame));
  if result = -1 then
    result := StartFrame(frameNames.IndexOf(strupper(frame)));
end;

//==============================================================================
//
// TMD2Model.EndFrame
//
//==============================================================================
function TMD2Model.EndFrame(const frame: string): integer;
begin
  result := EndFrame(frameNames.IndexOf(frame));
  if result = -1 then
    result := EndFrame(frameNames.IndexOf(strupper(frame)));
end;

//==============================================================================
//
// TMD2Model.FrameName
//
//==============================================================================
function TMD2Model.FrameName(const i: integer): string;
begin
  if IsIntegerInRange(i, 0, frameNames.Count - 1) then
    result := frameNames.Strings[i]
  else
    result := '';
end;

//==============================================================================
//
// TMD2Model.FrameIndex
//
//==============================================================================
function TMD2Model.FrameIndex(const frame: string): integer;
begin
  result := frameNames.IndexOf(frame);
  if result = -1 then
    result := frameNames.IndexOf(strupper(frame));
end;

//------------------------------------------------------------------------------

end.
