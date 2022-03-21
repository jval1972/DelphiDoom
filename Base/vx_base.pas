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
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  Basic Voxel Definitions
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit vx_base;

interface

uses
{$IFDEF OPENGL}
  gl_defs,
  gl_voxels,
{$ELSE}
  m_fixed,
  r_voxels,
{$ENDIF}
  d_delphi;

const
  VX_FLG_CLIPPED = 1;

type
  voxelmanageritem_t = record
    name: string[64];
    offset: {$IFDEF OPENGL}float{$ELSE}fixed_t{$ENDIF};
    scale: {$IFDEF OPENGL}float{$ELSE}fixed_t{$ENDIF};
    angleoffset: integer;
    droppedspin: integer;
    placedspin: integer;
    flags: LongWord;
    voxel: TVoxelModel;
  end;

  Pvoxelmanageritem_t = ^voxelmanageritem_t;
  voxelmanageritem_tArray = array[0..$FFF] of voxelmanageritem_t;
  Pvoxelmanageritem_tArray = ^voxelmanageritem_tArray;

  voxelmanager_t = record
    size: integer;
    items: Pvoxelmanageritem_tArray;
  end;

type
  voxelstate_t = record
    voxelidx: integer;  // index to voxelmanager item
    frame: integer;
    state: integer;
  end;
  Pvoxelstate_t = ^voxelstate_t;
  voxelstate_tArray = array[0..$FFF] of voxelstate_t;
  Pvoxelstate_tArray = ^voxelstate_tArray;

var
  voxelmanager: voxelmanager_t;
  voxelstates: Pvoxelstate_tArray;
  numvoxelstates: integer;

//==============================================================================
//
// VX_InitVoxels
//
//==============================================================================
procedure VX_InitVoxels;

//==============================================================================
//
// VX_VoxelsDone
//
//==============================================================================
procedure VX_VoxelsDone;

var
  voxelspritenames: TDStringList;

implementation

uses
  d_main,
  i_system,
  sc_engine,
  sc_tokens,
  sc_states,
  sc_utils,
  info,
  info_fnd,
  {$IFNDEF OPENGL}
  r_softgl,
  {$ENDIF}
  c_cmds,
  w_wad,
  w_pak;

//==============================================================================
//
// VX_AddVoxel
//
//==============================================================================
function VX_AddVoxel(const item: voxelmanageritem_t): integer;
var
  i: integer;
  voxelinf: Pvoxelmanageritem_t;
begin
  i := 0;
  while i < voxelmanager.size do
  begin
    if voxelmanager.items[i].name = item.name then
    begin
      result := i;
      exit;
    end;
    inc(i);
  end;

  realloc(pointer(voxelmanager.items), voxelmanager.size * SizeOf(voxelmanageritem_t), (1 + voxelmanager.size) * SizeOf(voxelmanageritem_t));
  result := voxelmanager.size;
  voxelinf := @voxelmanager.items[result];
  voxelinf.name := item.name;
  voxelinf.offset := item.offset;
  voxelinf.scale := item.scale;
  voxelinf.angleoffset := item.angleoffset;
  voxelinf.droppedspin := item.droppedspin;
  voxelinf.placedspin := item.placedspin;
  voxelinf.flags := item.flags;
  voxelinf.voxel := TVoxelModel.Create(voxelinf.name, voxelinf.offset, voxelinf.scale{$IFNDEF OPENGL}, voxelinf.flags{$ENDIF});

  inc(voxelmanager.size);
end;

//==============================================================================
//
// VX_AddVoxelState
//
//==============================================================================
procedure VX_AddVoxelState(const item: voxelstate_t);
begin
  if item.state < 0 then
    exit;

  realloc(pointer(voxelstates), numvoxelstates * SizeOf(voxelstate_t), (1 + numvoxelstates) * SizeOf(voxelstate_t));
  voxelstates[numvoxelstates] := item;
  if states[item.state].voxels = nil then
    states[item.state].voxels := TDNumberList.Create;
  states[item.state].voxels.Add(numvoxelstates);
  inc(numvoxelstates);
end;

const
  SPRITEFRAMECHARS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]';

//==============================================================================
//
// VX_CheckVoxelSprite
// JVAL 20191204
//    Add the sprname and the it's voxel source
//    Only one voxel (first parsed) per sprite though
//
//==============================================================================
procedure VX_CheckVoxelSprite(const sprname: string; const voxelsrc: string);
var
  s1, s2, check: string;
  i: integer;
begin
  splitstring_ch(fname(sprname), s1, s2, '.');
  check := strupper(s1);
  if Length(check) = 4 then
  begin
    for i := 1 to Length(SPRITEFRAMECHARS) do
      if voxelspritenames.IndexOfName(check + SPRITEFRAMECHARS[i]) < 0 then
        voxelspritenames.Add(check + SPRITEFRAMECHARS[i] + '0' + '=' + strupper(voxelsrc));
  end
  else if Length(check) = 5 then
  begin
    if voxelspritenames.IndexOfName(check) < 0 then
      voxelspritenames.Add(check + '0' + '=' + strupper(voxelsrc));
  end
  else if Length(check) = 6 then
  begin
    if check[6] = '0' then
      if voxelspritenames.IndexOfName(check) < 0 then
        voxelspritenames.Add(check + '=' + strupper(voxelsrc));
  end;
end;

const
  VOXELDEFLUMPNAME = 'VOXELDEF';

//==============================================================================
// SC_DoParseVoxelDefinition
//
// SC_ParseVoxelDefinition
// JVAL: Parse VOXELDEF LUMP
//
//==============================================================================
procedure SC_DoParseVoxelDefinition(const in_text: string);
var
  sc: TScriptEngine;
  tokens: TTokenList;
  slist: TDStringList;
  token: string;
  token_idx: integer;
  voxelstate: voxelstate_t;
  voxelitem: voxelmanageritem_t;
  voxelpending: boolean;
  statepending: boolean;
  i: integer;
  nList: TDNumberList;
  vxidx: integer;
  sprname: string;
begin
  tokens := TTokenList.Create;
  tokens.Add('VOXELDEF, VOXELDEFINITION');    //  0
  tokens.Add('STATE');                        //  1
  tokens.Add('OFFSET');                       //  2
  tokens.Add('SCALE');                        //  3
  tokens.Add('FRAME');                        //  4
  tokens.Add('VOXEL');                        //  5
  tokens.Add('REPLACE, REPLACES, REPLACING'); //  6
  tokens.Add('SPRITE');                       //  7
  tokens.Add('CLIPPED');                      //  8
  tokens.Add('ANGLEOFFSET');                  //  9
  tokens.Add('DROPPEDSPIN');                  // 10
  tokens.Add('PLACEDSPIN');                   // 11
  tokens.Add('SPIN');                         // 12

  if devparm then
  begin
    printf('--------'#13#10);
    printf('SC_ParseVoxelDefinition(): Parsing %s lump:'#13#10, [VOXELDEFLUMPNAME]);

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
  sc.AddIgnoreToken('=');

  voxelpending := false;
  statepending := false;

  while sc.GetString do
  begin
    token := strupper(sc._String);
    token_idx := tokens.IndexOfToken(token);
    case token_idx of
      0: // VOXEL DEFINITION
        begin
          voxelpending := true;
          voxelitem.name := '';
          voxelitem.offset := {$IFNDEF OPENGL}FloatToFixed({$ENDIF}0.0{$IFNDEF OPENGL}){$ENDIF};
          voxelitem.scale := {$IFNDEF OPENGL}FloatToFixed({$ENDIF}1.0{$IFNDEF OPENGL}){$ENDIF};
          voxelitem.angleoffset := 0;
          voxelitem.droppedspin := 0;
          voxelitem.placedspin := 0;
          voxelitem.flags := 0;
          if not sc.GetString then
          begin
            I_Warning('SC_ParseVoxelDefinition(): Token expected at line %d'#13#10, [sc._Line]);
            break;
          end;
          voxelitem.name := strupper(sc._String);

          while sc.GetString do
          begin
            token := strupper(sc._String);
            token_idx := tokens.IndexOfToken(token);
            case token_idx of
              2:  // Offset
                begin
                  sc.MustGetFloat;
                  voxelitem.offset := {$IFNDEF OPENGL}FloatToFixed(MAP_COEFF * {$ENDIF}sc._Float{$IFNDEF OPENGL}){$ENDIF};
                end;
              3:  // Scale
                begin
                  sc.MustGetFloat;
                  voxelitem.scale := {$IFNDEF OPENGL}FloatToFixed({$ENDIF}sc._Float{$IFNDEF OPENGL}){$ENDIF};
                  if voxelitem.scale < {$IFNDEF OPENGL}FloatToFixed({$ENDIF}0.01{$IFNDEF OPENGL}){$ENDIF} then
                    voxelitem.scale := {$IFNDEF OPENGL}FloatToFixed({$ENDIF}0.01{$IFNDEF OPENGL}){$ENDIF};
                end;
              8:  // CLIPPED
                begin
                  voxelitem.flags := voxelitem.flags or VX_FLG_CLIPPED;
                end;
              9:  // ANGLEOFFSET
                begin
                  sc.MustGetInteger;
                  voxelitem.angleoffset := sc._Integer;
                end;
             10:  // DROPPEDSPIN
                begin
                  sc.MustGetInteger;
                  voxelitem.droppedspin := sc._Integer;
                end;
             11:  // PLACEDDSPIN
                begin
                  sc.MustGetInteger;
                  voxelitem.placedspin := sc._Integer;
                end;
             12:  // SPIN
                begin
                  sc.MustGetInteger;
                  voxelitem.droppedspin := sc._Integer;
                  voxelitem.placedspin := sc._Integer;
                end;
              6:  // Replace
                begin
                  if not sc.GetString then
                  begin
                    I_Warning('SC_ParseVoxelDefinition(): Token expected at line %d'#13#10, [sc._Line]);
                    break;
                  end;
                  if tokens.IndexOfToken(strupper(sc._String)) <> 7 then
                  begin
                    I_Warning('SC_ParseVoxelDefinition(): "SPRITE" expected at line %d'#13#10, [sc._Line]);
                    break;
                  end;
                  sc.MustGetString;
                  sprname := sc._String;
                  VX_CheckVoxelSprite(sprname, voxelitem.name);
                  nList := Info_FindStatesFromSprite(sprname);
                  vxidx := VX_AddVoxel(voxelitem);
                  voxelpending := false;
                  if nList.Count > 0 then
                  begin
                    for i := 0 to nList.Count - 1 do
                    begin
                      ZeroMemory(@voxelstate, SizeOf(voxelstate_t));
                      voxelstate.state := nList.Numbers[i];
                      voxelstate.voxelidx := vxidx;
                      voxelstate.frame := 0;
                      VX_AddVoxelState(voxelstate);
                    end;
                  end
                  else
                  begin
                    I_Warning('SC_ParseVoxelDefinition(): Can not find sprite %s at line %d'#13#10, [sprname, sc._Line]);
                  end;
                  nList.Free;
                end;
            else
              begin
                VX_AddVoxel(voxelitem);
                voxelpending := false;
                sc.UnGet;
                break;
              end;
            end;
          end;
        end;

      1: // STATE DEFINITION
        begin
          statepending := true;
          ZeroMemory(@voxelstate, SizeOf(voxelstate_t));
          voxelitem.offset := {$IFNDEF OPENGL}FloatToFixed({$ENDIF}0.0{$IFNDEF OPENGL}){$ENDIF};
          voxelitem.scale := {$IFNDEF OPENGL}FloatToFixed({$ENDIF}1.0{$IFNDEF OPENGL}){$ENDIF};
          voxelitem.angleoffset := 0;
          voxelitem.droppedspin := 0;
          voxelitem.placedspin := 0;
          voxelstate.voxelidx := -1;
          if not sc.GetString then
          begin
            I_Warning('SC_ParseVoxelDefinition(): Token expected at line %d'#13#10, [sc._Line]);
            break;
          end;
          voxelstate.state := statenames.IndexOfToken(strupper(sc._String));
          if voxelstate.state < 0 then
          begin
            I_Warning('SC_ParseVoxelDefinition(): Unknown state "%s" at line %d'#13#10, [sc._String, sc._Line]);
            voxelstate.state := 0; // S_NULL
          end;

          while sc.GetString do
          begin
            token := strupper(sc._String);
            token_idx := tokens.IndexOfToken(token);
            case token_idx of
              5:  // Voxel
                begin
                  sc.MustGetString;
                  voxelitem.name := strupper(sc._String);
                  voxelitem.offset := {$IFNDEF OPENGL}FloatToFixed({$ENDIF}0.0{$IFNDEF OPENGL}){$ENDIF};
                  voxelitem.scale := {$IFNDEF OPENGL}FloatToFixed({$ENDIF}1.0{$IFNDEF OPENGL}){$ENDIF};
                  voxelitem.angleoffset := 0;
                  voxelitem.flags := 0;
                  voxelstate.voxelidx := VX_AddVoxel(voxelitem);
                end;
              4:  // frame
                begin
                  sc.MustGetInteger;
                  voxelstate.frame := sc._Integer;
                end;
            else
              begin
                VX_AddVoxelState(voxelstate);
                statepending := false;
                sc.UnGet;
                break;
              end;
            end;
          end;
        end;

      else
        begin
          I_Warning('SC_ParseVoxelDefinition(): Unknown token "%s" at line %d'#13#10, [token, sc._Line]);
        end;
    end;
  end;

  if voxelpending then
    VX_AddVoxel(voxelitem);
  if statepending then
    VX_AddVoxelState(voxelstate);

  sc.Free;
  tokens.Free;
end;

//==============================================================================
//
// SC_ParseVoxelDefinition
//
//==============================================================================
procedure SC_ParseVoxelDefinition(const in_text: string);
begin
  SC_DoParseVoxelDefinition(SC_Preprocess(in_text, false));
end;

//==============================================================================
//
// SC_ParseVoxelDefinitions
// JVAL: Parse all VOXELDEF lumps
//
//==============================================================================
procedure SC_ParseVoxelDefinitions;
var
  i: integer;
{$IFNDEF OPENGL}
  j: integer;
  radius: integer;
{$ENDIF}
begin
// Retrive voxeldef lumps
  for i := 0 to W_NumLumps - 1 do
    if char8tostring(W_GetNameForNum(i)) = VOXELDEFLUMPNAME then
      SC_ParseVoxelDefinition(W_TextLumpNum(i));

  PAK_StringIterator(VOXELDEFLUMPNAME, SC_ParseVoxelDefinition);
  PAK_StringIterator(VOXELDEFLUMPNAME + '.txt', SC_ParseVoxelDefinition);

{$IFNDEF OPENGL}
  for i := 0 to numstates - 1 do
  begin
    radius := 0;
    if states[i].voxels <> nil then
      for j := 0 to states[i].voxels.Count - 1 do
        if voxelmanager.items[voxelstates[states[i].voxels.Numbers[i]].voxelidx].voxel.radius > radius then
          radius := voxelmanager.items[voxelstates[states[i].voxels.Numbers[i]].voxelidx].voxel.radius;
    states[i].voxelradius := 2 * radius;
  end;
{$ENDIF}
end;

//==============================================================================
//
// Cmd_VoxelMapping
//
//==============================================================================
procedure Cmd_VoxelMapping;
var
  i: integer;
  mapped: boolean;
begin
  for i := 0 to nummobjtypes - 1 do
  begin
    mapped := false;
    if mobjinfo[i].spawnstate > 0 then
      if states[mobjinfo[i].spawnstate].voxels <> nil then
        if states[mobjinfo[i].spawnstate].voxels.Count > 0 then
          mapped := True;
    if mapped then
      printf('%s -> voxel mapped'#13#10, [mobjinfo[i].name])
    else
      printf('%s -> voxel unmapped'#13#10, [mobjinfo[i].name]);
  end;
end;

//==============================================================================
//
// VX_InitVoxels
//
//==============================================================================
procedure VX_InitVoxels;
begin
  voxelmanager.size := 0;
  voxelmanager.items := nil;
  numvoxelstates := 0;
  voxelstates := nil;
  voxelspritenames := TDStringList.Create;
  printf('SC_ParseVoxelDefinitions: Parsing VOXELDEF lumps.'#13#10);
  SC_ParseVoxelDefinitions;
  C_AddCmd('voxelmapping', @Cmd_VoxelMapping);
end;

//==============================================================================
//
// VX_VoxelsDone
//
//==============================================================================
procedure VX_VoxelsDone;
var
  i: integer;
begin
  for i := 0 to voxelmanager.size - 1 do
  begin
    if voxelmanager.items[i].voxel <> nil then
    begin
      voxelmanager.items[i].voxel.Free;
      voxelmanager.items[i].voxel := nil;
    end;
  end;

  memfree(pointer(voxelmanager.items), voxelmanager.size * SizeOf(voxelmanageritem_t));
  voxelmanager.size := 0;

  if numvoxelstates > 0 then
  begin
    memfree(pointer(voxelstates), numvoxelstates * SizeOf(voxelstate_t));
    numvoxelstates := 0;
  end;

  voxelspritenames.Free;
end;

end.

