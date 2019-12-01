//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2012 by Jim Valavanis
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
//    Dynamic lights for OpenGL rendering (why not in software mode??)
//    LIGHTDEF lump parsing, light animation
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://delphidoom.sitesled.com/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit gl_models;

// JVAL: Support for MD2 models

interface

uses
  d_delphi,
  m_fixed,
  dglOpenGL,
  gl_types;

var
  gl_drawmodels: boolean = true;
  gl_smoothmodelmovement: boolean = true;
  gl_precachemodeltextures: boolean = true;

type
  TFrameIndexInfo = class
  private
    fStartFrame,
    fEndFrame: integer;
  public
    property StartFrame: integer read fStartFrame write fStartFrame;
    property EndFrame: integer read fEndFrame write fEndFrame;
  end;

  TModel = class
  protected
    fNumFrames: integer;
    fNumVertexes: integer;
    frameNames: TDStringList;
    TheVectorsArray: PGLVertexArraysP;
    UV: PGLTexcoordArray;
    precalc: PGLuintArray;
    fname: string;
  public
    constructor Create(const name: string; const offset, scale: float; const additionalframes: TDStringList); virtual;
    destructor Destroy; override;
    function MergeFrames(const model: TModel): boolean;
    procedure Draw(const frm1, frm2: integer; const offset: float);
    procedure DrawSimple(const frm: integer);
    function StartFrame(const i: integer): integer; overload; virtual;
    function EndFrame(const i: integer): integer; overload; virtual;
    function StartFrame(const frame: string): integer; overload; virtual;
    function EndFrame(const frame: string): integer; overload; virtual;
    function FrameName(const i: integer): string; virtual;
    function FrameIndex(const frame: string): integer; virtual;
  end;

procedure gld_InitModels;

procedure gld_CleanModelTextures;

procedure gld_ModelsDone;

const
  MDSTRLEN = 64;

type
  modelmanageritem_t = record
    name: string[MDSTRLEN];
    offset: float;
    scale: float;
    framemerge: TDStringList;
    model: TModel;
  end;
  Pmodelmanageritem_t = ^modelmanageritem_t;
  modelmanageritem_tArray = array[0..$FFF] of modelmanageritem_t;
  Pmodelmanageritem_tArray = ^modelmanageritem_tArray;

  modelmanager_t = record
    size: integer;
    items: Pmodelmanageritem_tArray;
  end;

type
  texturemanagetitem_t = record
    name: string[MDSTRLEN];
    tex: GLUint;
  end;
  Ptexturemanagetitem_t = ^texturemanagetitem_t;
  texturemanagetitem_tArray = array[0..$FFF] of texturemanagetitem_t;
  Ptexturemanagetitem_tArray = ^texturemanagetitem_tArray;

  modeltexturemanager_t = record
    size: integer;
    items: Ptexturemanagetitem_tArray;
  end;


type
  modelstate_t = record
    modelidx: integer;  // index to modelmanager item
    texture: integer;   // index to modeltexturemanager item
    startframe,
    endframe: integer;
    nextframe: integer;
    state: integer;
    transparency: float;
  end;
  Pmodelstate_t = ^modelstate_t;
  modelstate_tArray = array[0..$FFF] of modelstate_t;
  Pmodelstate_tArray = ^modelstate_tArray;

var
  modelmanager: modelmanager_t;
  modeltexturemanager: modeltexturemanager_t;
  modelstates: Pmodelstate_tArray;
  nummodelstates: integer;

const
  MODELINTERPOLATERANGE = 512 * FRACUNIT;

implementation

uses
  {$IFDEF HEXEN}
  xn_defs,
  {$ELSE}
  doomdef,
  {$ENDIF}
  d_main,
  g_game,
  i_system,
  info,
  gl_md2,
  gl_tex,
  gl_defs,
  sc_engine,
  sc_tokens,
  sc_states,
  w_pak,
  w_wad;

function gld_AddModel(const item: modelmanageritem_t): integer;
var
  i: integer;
  modelinf: Pmodelmanageritem_t;
begin
  i := 0;
  while i < modelmanager.size do
  begin
    if modelmanager.items[i].name = item.name then
    begin
      result := i;
      exit;
    end;
    inc(i);
  end;
  realloc(pointer(modelmanager.items), modelmanager.size * SizeOf(modelmanageritem_t), (1 + modelmanager.size) * SizeOf(modelmanageritem_t));
  result := modelmanager.size;
  modelinf := @modelmanager.items[result];
  modelinf.name := item.name;
  modelinf.offset := item.offset;
  modelinf.scale := item.scale;
  if item.framemerge.Count > 0 then
  begin
    modelinf.framemerge := TDStringList.Create;
    modelinf.framemerge.AddStrings(item.framemerge);
  end
  else
    modelinf.framemerge := nil;
  modelinf.model := TModel.Create(modelinf.name, modelinf.offset, modelinf.scale, modelinf.framemerge);

  inc(modelmanager.size);
end;

procedure gld_AddModelState(const item: modelstate_t);
begin
  if item.state < 0 then
    exit;

  realloc(pointer(modelstates), nummodelstates * SizeOf(modelstate_t), (1 + nummodelstates) * SizeOf(modelstate_t));
  modelstates[nummodelstates] := item;
  if states[item.state].models = nil then
    states[item.state].models := TDNumberList.Create;
  states[item.state].models.Add(nummodelstates);
  inc(nummodelstates);
end;

function gld_AddModelTexture(const texturename: string): integer;
var
  i: integer;
begin
  i := 0;
  while i < modeltexturemanager.size do
  begin
    if modeltexturemanager.items[i].name = texturename then
    begin
      result := i;
      exit;
    end;
    inc(i);
  end;
  realloc(pointer(modeltexturemanager.items), modeltexturemanager.size * SizeOf(texturemanagetitem_t), (1 + modeltexturemanager.size) * SizeOf(texturemanagetitem_t));
  result := modeltexturemanager.size;
  modeltexturemanager.items[result].name := texturename;
  if gl_precachemodeltextures then
    modeltexturemanager.items[result].tex := gld_LoadExternalTexture(texturename, true, GL_CLAMP)
  else
    modeltexturemanager.items[result].tex := 0;
  inc(modeltexturemanager.size);
end;

const
  MODELDEFLUMPNAME = 'MODELDEF';

//
// SC_ParseModelDefinition
// JVAL: Parse MODELDEF LUMP
//
procedure SC_ParseModelDefinition(const in_text: string);
var
  sc: TScriptEngine;
  tokens: TTokenList;
  slist: TDStringList;
  token: string;
  token_idx: integer;
  modelstate: modelstate_t;
  modelitem: modelmanageritem_t;
  modelpending: boolean;
  statepending: boolean;
  i: integer;
begin
  tokens := TTokenList.Create;
  modelitem.framemerge := TDStringList.Create;
  tokens.Add('MODELDEF, MODELDEFINITION');
  tokens.Add('STATE');
  tokens.Add('OFFSET');
  tokens.Add('SCALE');
  tokens.Add('TEXTURE');
  tokens.Add('FRAME, FRAME1, STARTFRAME');
  tokens.Add('FRAME2, ENDFRAME, NEXTFRAME'); // JVAL: unused :(
  tokens.Add('MODEL');
  tokens.Add('FRAMEMERGE');
  tokens.Add('TRANSPARENCY');

  if devparm then
  begin
    printf('--------'#13#10);
    printf('SC_ParseModelDefinition(): Parsing %s lump:'#13#10, [MODELDEFLUMPNAME]);

    slist := TDStringList.Create;
    try
      slist.Text := in_text;
      for i := 0 to slist.Count - 1 do
        printf('%s: %s'#13#10, [IntToStrZFill(6, i + 1), slist[i]]);
    finally
      slist.Free;
    end;

    printf('--------'#13#10);
  end;

  sc := TScriptEngine.Create(in_text);

  modelpending := false;
  statepending := false;

  while sc.GetString do
  begin
    token := strupper(sc._String);
    token_idx := tokens.IndexOfToken(token);
    case token_idx of
      0: // MODEL DEFINITION
        begin
          modelpending := true;
          modelitem.name := '';
          modelitem.offset := 0.0;
          modelitem.scale := 1.0;
          modelitem.framemerge.Clear;
          if not sc.GetString then
          begin
            I_Warning('SC_ParseModelDefinition(): Token expected at line %d'#13#10, [sc._Line]);
            break;
          end;
          modelitem.name := strupper(sc._String);

          while sc.GetString do
          begin
            token := strupper(sc._String);
            token_idx := tokens.IndexOfToken(token);
            case token_idx of
              2:  // Offset
                begin
                  sc.MustGetFloat;
                  modelitem.offset := sc._Float;
                end;
              3:  // Scale
                begin
                  sc.MustGetFloat;
                  modelitem.scale := sc._Float;
                end;
              8:  // FRAMEMERGE
                begin
                  sc.MustGetString;
                  modelitem.framemerge.Add(strupper(sc._String));
                end;
            else
              begin
                gld_AddModel(modelitem);
                modelpending := false;
                sc.UnGet;
                break;
              end;
            end;
          end;
        end;

      1: // STATE DEFINITION
        begin
          statepending := true;
          ZeroMemory(@modelstate, SizeOf(modelstate_t));
          modelitem.offset := 0.0;
          modelitem.scale := 1.0;
          modelitem.framemerge.Clear;

          modelstate.modelidx := -1;
          modelstate.texture := -1;
          modelstate.startframe := -1;
          modelstate.endframe := -1;
          modelstate.nextframe := -1; // JVAL: runtime only!
          modelstate.transparency := 1.0;
          if not sc.GetString then
          begin
            I_Warning('SC_ParseModelDefinition(): Token expected at line %d'#13#10, [sc._Line]);
            break;
          end;
          modelstate.state := statenames.IndexOfToken(strupper(sc._String));
          if modelstate.state < 0 then
          begin
            I_Warning('SC_ParseModelDefinition(): Unknown state "%s" at line %d'#13#10, [sc._String, sc._Line]);
            modelstate.state := 0; // S_NULL
          end;

          while sc.GetString do
          begin
            token := strupper(sc._String);
            token_idx := tokens.IndexOfToken(token);
            case token_idx of
              7:  // Model
                begin
                  sc.MustGetString;
                  modelitem.name := strupper(sc._String);
                  modelitem.offset := 0.0;
                  modelitem.scale := 1.0;
                  modelstate.modelidx := gld_AddModel(modelitem);
                end;
              4:  // Texture
                begin
                  sc.MustGetString;
                  modelstate.texture := gld_AddModelTexture(strupper(sc._String));
                end;
              5:  // startframe
                begin
                  sc.MustGetInteger;
                  modelstate.startframe := sc._Integer;
                  if modelstate.endframe < 0 then
                    modelstate.endframe := modelstate.startframe;
                end;
              6:  // endframe
                begin
                  sc.MustGetInteger;
                  modelstate.endframe := sc._Integer;
                end;
              9:  // transparency
                begin
                  sc.MustGetFloat;
                  modelstate.transparency := sc._Float;
                end;
            else
              begin
                gld_AddModelState(modelstate);
                statepending := false;
                sc.UnGet;
                break;
              end;
            end;
          end;
        end;
        
      else
        begin
          I_Warning('SC_ParseModelDefinition(): Unknown token "%s" at line %d'#13#10, [token, sc._Line]);
        end;
    end;
  end;

  if modelpending then
    gld_AddModel(modelitem);
  if statepending then
    gld_AddModelState(modelstate);

  sc.Free;
  tokens.Free;
  modelitem.framemerge.Free;
end;

//
// SC_ParceDynamicLights
// JVAL: Parse all MODELDEF lumps
//
procedure SC_ParseModelDefinitions;
var
  i: integer;
begin
// Retrive modeldef lumps
  for i := 0 to W_NumLumps - 1 do
    if char8tostring(W_GetNameForNum(i)) = MODELDEFLUMPNAME then
      SC_ParseModelDefinition(W_TextLumpNum(i));

  PAK_StringIterator(MODELDEFLUMPNAME, SC_ParseModelDefinition);
  PAK_StringIterator(MODELDEFLUMPNAME + '.txt', SC_ParseModelDefinition);
end;


procedure gld_InitModels;
begin
  modelmanager.size := 0;
  modelmanager.items := nil;
  modeltexturemanager.size := 0;
  modeltexturemanager.items := nil;
  nummodelstates := 0;
  modelstates := nil;
  printf('SC_ParseModelDefinitions: Parsing MODELDEF lumps.'#13#10);
  SC_ParseModelDefinitions;
end;

procedure gld_CleanModelTextures;
var
  i: integer;
begin
  if gl_precachemodeltextures then
    exit;

  for i := 0 to modeltexturemanager.size - 1 do
  begin
    if modeltexturemanager.items[i].tex <> 0 then
    begin
      glDeleteTextures(1, @modeltexturemanager.items[i].tex);
      modeltexturemanager.items[i].tex := 0;
    end;
  end;
end;

procedure gld_ModelsDone;
var
  i: integer;
begin
  for i := 0 to modelmanager.size - 1 do
  begin
    if modelmanager.items[i].model <> nil then
    begin
      modelmanager.items[i].model.Free;
      modelmanager.items[i].model := nil;
    end;
    modelmanager.items[i].framemerge.Free;
  end;

  memfree(pointer(modelmanager.items), modelmanager.size * SizeOf(modelmanageritem_t));
  modelmanager.size := 0;

  for i := 0 to modeltexturemanager.size - 1 do
  begin
    if modeltexturemanager.items[i].tex <> 0 then
    begin
      glDeleteTextures(1, @modeltexturemanager.items[i].tex);
      modeltexturemanager.items[i].tex := 0;
    end;
  end;
  memfree(pointer(modeltexturemanager.items), modeltexturemanager.size * SizeOf(texturemanagetitem_t));
  modeltexturemanager.size := 0;

  if nummodelstates > 0 then
  begin
    memfree(pointer(modelstates), nummodelstates * SizeOf(modelstate_t));
    nummodelstates := 0;
  end;
end;

//------------------------------------------------------------------------------
//--------------------------- MD2 Model Class ----------------------------------
//------------------------------------------------------------------------------

constructor TModel.Create(const name: string; const offset, scale: float; const additionalframes: TDStringList);
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
  foffset, fscale: single;
  modelheader: TDmd2_T;
  m: TModel;
begin
  fname := name;
  printf('  Found external model %s'#13#10, [fname]);
  frameNames := TDStringList.Create;
  UV := nil;
  TheVectorsArray := nil;
  fNumFrames := 0;
  fNumVertexes := 0;
  foffset := offset / MAP_COEFF;
  fscale := scale / MAP_COEFF;
  strm := TPakStream.Create(fname, pm_prefered, gamedirectories);
  if strm.IOResult <> 0 then
    I_Error('TModel.Create(): Can not find model %s!', [fname]);

  strm.Read(modelheader, SizeOf(modelheader));

  if modelheader.ident <> MD2_MAGIC then
    I_Error('TModel.Create(): Invalid MD2 model magic (%d)!', [modelheader.ident]);

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
        x := (out_t^.verts[j].v[0] * out_t^.scale[0] + out_t^.translate[0]) * fscale;
        y := (out_t^.verts[j].v[1] * out_t^.scale[1] + out_t^.translate[1]) * fscale;
        z := (out_t^.verts[j].v[2] * out_t^.scale[2] + out_t^.translate[2]) * fscale + foffset;
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
      m := TModel.Create(additionalframes.strings[i], offset, scale, nil);
      if not MergeFrames(m) then
        I_Error('TModel.MergeFrames(): Can not merge model "%s" into model "%s"', [additionalframes.strings[i], fname]);
      m.Free;
    end;

end;

//------------------------------------------------------------------------------

function TModel.MergeFrames(const model: TModel): boolean;
var
  i: integer;
begin
  if model.fNumVertexes <> fNumVertexes then
  begin
    result := false;
    exit;
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

  result := true;
end;

//------------------------------------------------------------------------------

destructor TModel.Destroy;
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
end;

//------------------------------------------------------------------------------

procedure TModel.Draw(const frm1, frm2: integer; const offset: float);
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

//------------------------------------------------------------------------------

procedure TModel.DrawSimple(const frm: integer);
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

//------------------------------------------------------------------------------

function TModel.StartFrame(const i: integer): integer;
begin
  result := -1;
  if IsIntegerInRange(i, 0, frameNames.Count - 1) then
    if frameNames.Objects[i] <> nil then
      result := (frameNames.Objects[i] as TFrameIndexInfo).StartFrame;
end;

//------------------------------------------------------------------------------

function TModel.EndFrame(const i: integer): integer;
begin
  result := -1;
  if IsIntegerInRange(i, 0, frameNames.Count - 1) then
    if frameNames.Objects[i] <> nil then
      result := (frameNames.Objects[i] as TFrameIndexInfo).EndFrame;
end;

//------------------------------------------------------------------------------

function TModel.StartFrame(const frame: string): integer;
begin
  result := StartFrame(frameNames.IndexOf(frame));
  if result = -1 then
    result := StartFrame(frameNames.IndexOf(strupper(frame)));
end;

//------------------------------------------------------------------------------

function TModel.EndFrame(const frame: string): integer;
begin
  result := EndFrame(frameNames.IndexOf(frame));
  if result = -1 then
    result := EndFrame(frameNames.IndexOf(strupper(frame)));
end;

//------------------------------------------------------------------------------

function TModel.FrameName(const i: integer): string;
begin
  if IsIntegerInRange(i, 0, frameNames.Count - 1) then
    result := frameNames.Strings[i]
  else
    result := '';
end;

//------------------------------------------------------------------------------

function TModel.FrameIndex(const frame: string): integer;
begin
  result := frameNames.IndexOf(frame);
  if result = -1 then
    result := frameNames.IndexOf(strupper(frame));
end;

//------------------------------------------------------------------------------

end.
