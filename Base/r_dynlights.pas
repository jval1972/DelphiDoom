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
//    Dynamic lights (OpenGL & software rendering)
//    LIGHTDEF lump parsing, light animation
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_dynlights;

interface

uses
  d_delphi,
  {$IFNDEF OPENGL}
  r_vislight,
  p_mobj_h,
  {$ENDIF}
  m_fixed;

const
  DLSTRLEN = 32;

type
  GLDlightType = (
    GLDL_POINT,   // Point light
    GLDL_FLICKER, // Flicker light
    GLDL_PULSE,   // pulse light
    GLDL_NUMDLIGHTTYPES,
    GLDL_UNKNOWN  // unknown light
  );

  GLDRenderLight = record
    r, g, b: float;     // Color
    radius: float;      // radius
    x, y, z: float;     // Offset
    shadow: boolean;    // Shadow
  end;
  PGLDRenderLight = ^GLDRenderLight;
  GLDRenderLightArray = array[0..$FFF] of GLDRenderLight;
  PGLDRenderLightArray = ^GLDRenderLightArray;

  GLDLight = record
    name: string[DLSTRLEN];             // Light name
    lighttype: GLDlightType;            // Light type
    r1, g1, b1: float;                  // Color
    r2, g2, b2: float;                  // Color
    colorinterval: integer;             // color interval in TICRATE * FRACUNIT
    colorchance: integer;
    size1: float;                       // Size
    size2: float;                       // Secondarysize for flicker and pulse lights
    interval: integer;                  // interval in TICRATE * FRACUNIT
    chance: integer;
    offsetx1, offsety1, offsetz1: float;   // Offset
    offsetx2, offsety2, offsetz2: float;   // Offset
    randomoffset: boolean;
    validcount: integer;
    randomseed: integer;
    renderinfo: GLDRenderLight;
  end;

  PGLDLight = ^GLDLight;
  GLDLightArray = array[0..$FFFF] of GLDLight;
  PGLDLightArray = ^GLDLightArray;

//==============================================================================
//
// R_InitDynamicLights
//
//==============================================================================
procedure R_InitDynamicLights;

//==============================================================================
//
// R_DynamicLightsDone
//
//==============================================================================
procedure R_DynamicLightsDone;

//==============================================================================
//
// R_AddDynamicLight
//
//==============================================================================
function R_AddDynamicLight(const l: GLDLight): integer;

//==============================================================================
//
// R_FindDynamicLight
//
//==============================================================================
function R_FindDynamicLight(const check: string): integer;

//==============================================================================
//
// R_GetDynamicLight
//
//==============================================================================
function R_GetDynamicLight(const index: integer): PGLDRenderLight;

var
  lightdeflumppresent: boolean = false;

type
  dlsortitem_t = record
    l: PGLDRenderLight;
    squaredist: single;
    x, y, z: {$IFDEF OPENGL}float{$ELSE}fixed_t{$ENDIF};
    {$IFNDEF OPENGL}
    radius: fixed_t;
    vis: Pvislight_t;
    mo: Pmobj_t;
    {$ENDIF}
  end;
  Pdlsortitem_t = ^dlsortitem_t;
  dlsortitem_tArray = array[0..$FFFF] of dlsortitem_t;
  Pdlsortitem_tArray = ^dlsortitem_tArray;

var
  dlbuffer: Pdlsortitem_tArray = nil;
  numdlitems: integer = 0;
  realdlitems: integer = 0;

const
  DLIGHTSDRAWRANGE = 2048 * FRACUNIT;

var
  gldefs_as_lightdef: boolean = true;

implementation

uses
  d_main,
  doomdef,
  m_stack,
  m_rnd,
  {$IFDEF OPENGL}
  gl_defs,
  {$ENDIF}
  p_tick,
  w_wad,
  info,
  info_common,
  i_system,
  p_pspr,
  sc_engine,
  sc_tokens,
  sc_utils,
  w_pak;

const
  LIGHTSLUMPNAME = 'LIGHTDEF';
  GLDEFSLUMPNAME = 'GLDEFS';

var
// JVAL: Random index for light animation operations,
//       don't bother reseting it....
  lightrnd: integer = 0;

//==============================================================================
// SC_DoParceDynamicLight
//
// SC_ParceDynamicLights
// JVAL: Parse LIGHTDEF
//
//==============================================================================
procedure SC_DoParceDynamicLight(const in_text: string);
var
  sc: TScriptEngine;
  tokens: TTokenList;
  slist: TDStringList;
  i, j, k: integer;
  l: GLDLight;
  token: string;
  token1, token2, token3, token4: string;
  token_idx: integer;
  objectsfound: boolean;
  stmp: string;
  lidx: integer;
  frame: integer;
  sprite: integer;
  foundstate: boolean;
  mobjno: integer;
  mobjname: string;
  N: TDNumberList;
  curstates: TDNumberList;
begin
  tokens := TTokenList.Create;
  tokens.Add('POINTLIGHT');
  tokens.Add('FLICKERLIGHT, FLICKERLIGHT2');
  tokens.Add('PULSELIGHT');
  tokens.Add('OBJECT');
  tokens.Add('COLOR, COLOR1, PRIMARYCOLOR');
  tokens.Add('COLOR2, SECONDARYCOLOR');
  tokens.Add('SIZE, SIZE1, PRIMARYSIZE');
  tokens.Add('SIZE2, SECONDARYSIZE');
  tokens.Add('INTERVAL');
  tokens.Add('COLORINTERVAL');
  tokens.Add('CHANCE');
  tokens.Add('COLORCHANCE');
  tokens.Add('OFFSET, OFFSET1, PRIMARYOFFSET');
  tokens.Add('OFFSET2, SECONDARYOFFSET');
  tokens.Add('RANDOMOFFSET');

  if devparm then
  begin
    printf('--------'#13#10);
    printf('SC_ParceDynamicLight(): Parsing light definitions'#13#10);

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

  objectsfound := false;

  sc := TScriptEngine.Create(in_text);

  while sc.GetString do
  begin
    token := strupper(sc._String);
    token_idx := tokens.IndexOfToken(token);
    case token_idx of
      0, 1, 2:
        begin
          ZeroMemory(@l, SizeOf(GLDLight));
          l.lighttype := GLDlightType(token_idx);
          l.validcount := -1;
          l.randomseed := C_Random(lightrnd) * 256 + C_Random(lightrnd);
          if not sc.GetString then
          begin
            I_Warning('SC_ParceDynamicLight(): Token expected at line %d'#13#10, [sc._Line]);
            break;
          end;
          l.name := strupper(sc._String);

          while sc.GetString do
          begin
            token := strupper(sc._String);
            token_idx := tokens.IndexOfToken(token);
            case token_idx of
               4:  // Color
                begin
                  sc.MustGetFloat;
                  l.r1 := sc._Float;
                  l.r2 := l.r1;
                  sc.MustGetFloat;
                  l.g1 := sc._Float;
                  l.g2 := l.g1;
                  sc.MustGetFloat;
                  l.b1 := sc._Float;
                  l.b2 := l.b1;
                end;
               5:  // Secondary Color
                begin
                  sc.MustGetFloat;
                  l.r2 := sc._Float;
                  sc.MustGetFloat;
                  l.g2 := sc._Float;
                  sc.MustGetFloat;
                  l.b2 := sc._Float;
                end;
               6:  // Size
                begin
                  sc.MustGetInteger;
                  l.size1 := sc._Integer{$IFDEF OPENGL} / MAP_COEFF{$ENDIF};
                  l.size2 := l.size1;
                end;
               7:  // Secondary Size
                begin
                  sc.MustGetInteger;
                  l.size2 := sc._Integer{$IFDEF OPENGL} / MAP_COEFF{$ENDIF};
                end;
               8:  // Interval
                begin
                  sc.MustGetFloat;
                  if sc._float > 0.0 then
                    l.interval := round(1 / sc._float * FRACUNIT * TICRATE);
                end;
               9:  // ColorInterval
                begin
                  sc.MustGetFloat;
                  if sc._float > 0.0 then
                    l.colorinterval := round(1 / sc._float * FRACUNIT * TICRATE);
                end;
              10:  // Chance
                begin
                  sc.MustGetFloat;
                  l.chance := round(sc._float * 255);
                  if l.chance < 0 then
                    l.chance := 0
                  else if l.chance > 255 then
                    l.chance := 255;
                end;
              11:  // ColorChance
                begin
                  sc.MustGetFloat;
                  l.colorchance := round(sc._float * 255);
                  if l.colorchance < 0 then
                    l.colorchance := 0
                  else if l.colorchance > 255 then
                    l.colorchance := 255;
                end;
              12:  // Offset
                begin
                  sc.MustGetInteger;
                  l.offsetx1 := sc._Integer{$IFDEF OPENGL} / MAP_COEFF{$ENDIF};
                  l.offsetx2 := l.offsetx1;
                  sc.MustGetInteger;
                  l.offsety1 := sc._Integer{$IFDEF OPENGL} / MAP_COEFF{$ENDIF};
                  l.offsety2 := l.offsety1;
                  sc.MustGetInteger;
                  l.offsetz1 := sc._Integer{$IFDEF OPENGL} / MAP_COEFF{$ENDIF};
                  l.offsetz2 := l.offsetz1;
                end;
              13:  // Offset2
                begin
                  sc.MustGetInteger;
                  l.offsetx2 := sc._Integer{$IFDEF OPENGL} / MAP_COEFF{$ENDIF};
                  sc.MustGetInteger;
                  l.offsety2 := sc._Integer{$IFDEF OPENGL} / MAP_COEFF{$ENDIF};
                  sc.MustGetInteger;
                  l.offsetz2 := sc._Integer{$IFDEF OPENGL} / MAP_COEFF{$ENDIF};
                end;
              14:
                begin
                  l.randomoffset := true;
                end;
            else
              begin
                R_AddDynamicLight(l);
                sc.UnGet;
                break;
              end;
            end;
          end;

        end;

      3:
        begin
          objectsfound := true;
        end;
//    else
//      I_Warning('SC_ParceDynamicLight(): Unknown token %s at line %d'#13#10, [sc._String, sc._Line]);

    end;
  end;

  tokens.Free;

  mobjno := -1;
  mobjname := '';
  curstates := TDNumberList.Create;

  // JVAL: Simplified parsing for frames keyword
  if objectsfound then
  begin
    slist := TDStringList.Create;
    slist.Text := in_text;

    for i := 0 to slist.Count - 1 do
    begin
      stmp := strupper(strtrim(slist.Strings[i]));
      if firstword_ch(stmp) = 'OBJECT' then
      begin
        sc.SetText(stmp);
        sc.MustGetString;
        token1 := sc._String;
        sc.MustGetString;
        mobjname := sc._String;
        if mobjname = '-1' then
        begin
          mobjno := -1;
          mobjname := '';
        end
        else
        begin
          mobjno := Info_GetMobjNumForName(mobjname);
          if mobjno < 0 then
          begin
            I_Warning('SC_ParceDynamicLight(): Unknown object "%s" at line %d'#13#10, [mobjname, i]);
            mobjname := '';
          end;
        end;
      end
      else if firstword_ch(stmp) = 'FRAME' then
      begin
        sc.SetText(stmp);

        sc.MustGetString;
        token1 := sc._String;
        sc.MustGetString;
        token2 := sc._String;
        sc.MustGetString;
        token3 := sc._String;
        sc.MustGetString;
        token4 := sc._String;

        if token3 = 'LIGHT' then
        begin
          lidx := R_FindDynamicLight(token4);
          if lidx >= 0 then
          begin
            if Length(token2) >= 4 then
            begin
              token := '';
              for j := 1 to 4 do
                token := token + token2[j];
              sprite := Info_CheckSpriteNumForName(token);
              if sprite >= 0 then
              begin
                foundstate := false;
                N := Info_GetStatesForMobjInfo(mobjno);

                if Length(token2) > 4 then
                begin
                  frame := Ord(token2[5]) - Ord('A');

                  while true do
                  begin
                    for j := 0 to numstates - 1 do
                      if (N.Count = 0) or (N.IndexOf(j) >= 0) then
                        if (states[j].sprite = sprite) and
                           ((states[j].frame and FF_FRAMEMASK) = frame) then
                        begin
                          if states[j].dlights = nil then
                          begin
                            states[j].dlights := T2DNumberList.Create;
                            curstates.Add(j);
                          end
                          else if curstates.IndexOf(j) < 0 then
                          begin
                            if mobjno >= 0 then
                              for k := states[j].dlights.Count - 1 downto 0 do
                                if states[j].dlights[k].num1 = mobjno then
                                  states[j].dlights.Delete(k);
                            curstates.Add(j);
                          end;

                          states[j].dlights.Add(mobjno, lidx);
                          foundstate := true;
                        end;

                    if foundstate then
                      break;
                    if N.Count = 0 then
                      break;
                    N.FastClear;
                  end;
                end
                else
                begin
                  while true do
                  begin
                    for j := 0 to numstates - 1 do
                      if (N.Count = 0) or (N.IndexOf(j) >= 0) then
                        if states[j].sprite = sprite then
                        begin
                          if states[j].dlights = nil then
                          begin
                            states[j].dlights := T2DNumberList.Create;
                            curstates.Add(j);
                          end
                          else if curstates.IndexOf(j) < 0 then
                          begin
                            if mobjno >= 0 then
                              for k := states[j].dlights.Count - 1 downto 0 do
                                if states[j].dlights[k].num1 = mobjno then
                                  states[j].dlights.Delete(k);
                            curstates.Add(j);
                          end;

                          states[j].dlights.Add(mobjno, lidx);
                          foundstate := true;
                        end;

                    if foundstate then
                      break;
                    if N.Count = 0 then
                      break;
                    N.FastClear;
                  end;
                end;

                N.Free;
                if not foundstate then
                begin
                  if mobjno < 0 then
                    I_Warning('SC_ParceDynamicLight(): Can not determine light owner, line %d: "%s",'#13#10, [i + 1, stmp])
                  else
                    I_Warning('SC_ParceDynamicLight(): Can not determine light for object "%s", line %d: "%s",'#13#10, [mobjname, i + 1, stmp]);
                end;
              end
              else
                I_Warning('SC_ParceDynamicLight(): Unknown sprite %s at line %d'#13#10, [token, i + 1]);
            end
            else
              I_Warning('SC_ParceDynamicLight(): Unknown sprite %s at line %d'#13#10, [token2, i + 1]);
          end
          else
            I_Warning('SC_ParceDynamicLight(): Unknown light %s at line %d'#13#10, [token4, i + 1]);
        end
        else
          I_Warning('SC_ParceDynamicLight(): Unknown token %s at line %d'#13#10, [token3, i + 1]);
      end;
    end;
    slist.Free;

  end;

  curstates.Free;
  sc.Free;
end;

//==============================================================================
//
// SC_ParceDynamicLight
//
//==============================================================================
procedure SC_ParceDynamicLight(const in_text: string);
begin
  SC_DoParceDynamicLight(SC_Preprocess(in_text, false));
end;

//==============================================================================
//
// SC_ParceDynamicLights
// JVAL: Parse all LIGHTDEF lumps
//
//==============================================================================
procedure SC_ParceDynamicLights;
var
  i: integer;
begin
// Retrive lightdef lumps
  for i := 0 to W_NumLumps - 1 do
    if char8tostring(W_GetNameForNum(i)) = LIGHTSLUMPNAME then
    begin
      lightdeflumppresent := true;
      SC_ParceDynamicLight(W_TextLumpNum(i));
    end;

  if gldefs_as_lightdef then
  begin
    for i := 0 to W_NumLumps - 1 do
      if char8tostring(W_GetNameForNum(i)) = GLDEFSLUMPNAME then
      begin
        lightdeflumppresent := true;
        SC_ParceDynamicLight(W_TextLumpNum(i));
      end;
  end;

// JVAL: 2011/05 Now check inside PAK/PK3 files for LIGHTDEF entries
  i := PAK_StringIterator(LIGHTSLUMPNAME, SC_ParceDynamicLight);
  i := i + PAK_StringIterator(LIGHTSLUMPNAME + '.txt', SC_ParceDynamicLight);

  if gldefs_as_lightdef then
  begin
    i := i + PAK_StringIterator(GLDEFSLUMPNAME, SC_ParceDynamicLight);
    i := i + PAK_StringIterator(GLDEFSLUMPNAME + '.txt', SC_ParceDynamicLight);
  end;

  lightdeflumppresent := lightdeflumppresent or (i > 0);
end;

var
  numdlights: integer;
  realnumdlights: integer;
  dlightslist: PGLDLightArray;

//==============================================================================
//
// R_InitDynamicLights
//
//==============================================================================
procedure R_InitDynamicLights;
begin
  numdlights := 0;
  realnumdlights := 0;
  dlightslist := nil;
  {$IFDEF OPENGL}
  printf('SC_ParceDynamicLights: Parsing LIGHTDEF lumps.'#13#10);
  {$ENDIF}
  SC_ParceDynamicLights;
end;

//==============================================================================
//
// R_DynamicLightsDone
//
//==============================================================================
procedure R_DynamicLightsDone;
begin
  memfree(pointer(dlightslist), realnumdlights * SizeOf(GLDLight));
  numdlights := 0;
  realnumdlights := 0;

  memfree(pointer(dlbuffer), realdlitems * SizeOf(dlsortitem_t));
  numdlitems := 0;
  realdlitems := 0;
end;

//==============================================================================
//
// R_GrowDynlightsArray
//
//==============================================================================
procedure R_GrowDynlightsArray;
begin
  if numdlights >= realnumdlights then
  begin
    if realnumdlights = 0 then
    begin
      realnumdlights := 32;
      dlightslist := malloc(realnumdlights * SizeOf(GLDLight));
    end
    else
    begin
      M_PushValue(realnumdlights);
      realnumdlights := realnumdlights + realnumdlights div 4;
      realloc(pointer(dlightslist), M_PopValue * SizeOf(GLDLight), realnumdlights * SizeOf(GLDLight));
    end;
  end;
end;

//==============================================================================
//
// R_AddDynamicLight
//
//==============================================================================
function R_AddDynamicLight(const l: GLDLight): integer;
var
  i: integer;
begin
  R_GrowDynlightsArray;
  result := numdlights;
  dlightslist[result] := l;
  for i := 1 to DLSTRLEN do
    dlightslist[result].name[i] := toupper(dlightslist[result].name[i]);
  inc(numdlights);
end;

//==============================================================================
//
// R_FindDynamicLight
//
//==============================================================================
function R_FindDynamicLight(const check: string): integer;
var
  i: integer;
  tmp: string;
begin
  tmp := strupper(check);
  for i := numdlights - 1 downto 0 do
    if tmp = dlightslist[i].name then
    begin
      result := i;
      exit;
    end;

  result := -1;
end;

//==============================================================================
//
// R_GetDynamicLight
// JVAL: Retrieving rendering information for lights
//       Dynamic lights animations
//
//==============================================================================
function R_GetDynamicLight(const index: integer): PGLDRenderLight;
var
  l: PGLDLight;
  frac: integer;
  frac1, frac2: single;

  procedure _CalcOffset(prl: PGLDRenderLight; const recalc: boolean);
  var
    rnd: integer;
  begin
    if l.randomoffset then
    begin
      rnd := C_Random(lightrnd);
      prl.x := (l.offsetx1 * rnd + l.offsetx2 * (255 - rnd)) / 255;
      rnd := C_Random(lightrnd);
      prl.y := (l.offsety1 * rnd + l.offsety2 * (255 - rnd)) / 255;
      rnd := C_Random(lightrnd);
      prl.z := (l.offsetz1 * rnd + l.offsetz2 * (255 - rnd)) / 255;
    end
    else if recalc then
    begin
      prl.x := l.offsetx1;
      prl.y := l.offsety1;
      prl.z := l.offsetz1;
    end;
  end;

begin
  l := @dlightslist[index];
  result := @l.renderinfo;
  result.shadow := false;

  // JVAL: Point lights, a bit static :)
  if l.lighttype = GLDL_POINT then
  begin
    if l.validcount < 0 then
    begin
      result.r := l.r1;
      result.g := l.g1;
      result.b := l.b1;
      result.radius := l.size1;
      _CalcOffset(result, true);
    end
    else if l.validcount <> leveltime then
      _CalcOffset(result, false);
    l.validcount := leveltime;
    exit;
  end;

  // JVAL: Flicker lights, use chance/colorchance to switch size/color
  if l.lighttype = GLDL_FLICKER then
  begin
    if l.validcount <> leveltime div TICRATE then
    begin

      // Determine color
      if l.colorchance > 0 then
      begin
        if C_Random(lightrnd) < l.colorchance then
        begin
          result.r := l.r1;
          result.g := l.g1;
          result.b := l.b1;
        end
        else
        begin
          result.r := l.r2;
          result.g := l.g2;
          result.b := l.b2;
        end;
      end
      else if l.colorinterval > 0 then
      begin
        if Odd(FixedDiv(l.randomseed + leveltime * FRACUNIT, l.colorinterval)) then
        begin
          result.r := l.r1;
          result.g := l.g1;
          result.b := l.b1;
        end
        else
        begin
          result.r := l.r2;
          result.g := l.g2;
          result.b := l.b2;
        end;
      end
      else
      begin
        result.r := l.r1;
        result.g := l.g1;
        result.b := l.b1;
      end;

      // Determine size
      if l.chance > 0 then
      begin
        if C_Random(lightrnd) < l.chance then
          result.radius := l.size1
        else
          result.radius := l.size2;
      end
      else if l.interval > 0 then
      begin
        if Odd(FixedDiv(l.randomseed + leveltime * FRACUNIT, l.interval)) then
          result.radius := l.size1
        else
          result.radius := l.size2;
      end
      else
        result.radius := l.size1;

      _CalcOffset(result, l.validcount < 0);
      l.validcount := leveltime div TICRATE;
    end;
    exit;
  end;

  // JVAL: Pulse lights, use leveltime to switch smoothly size/color
  if l.lighttype = GLDL_PULSE then
  begin
    if l.validcount <> leveltime then
    begin
      // Determine color,
      if l.colorinterval > 0 then
      begin
        frac := FixedDiv(l.randomseed + leveltime * FRACUNIT, l.colorinterval) mod l.colorinterval;
        frac1 := frac / l.colorinterval;
        frac2 := 1.0 - frac1;
        result.r := l.r1 * frac1 + l.r2 * frac2;
        result.g := l.g1 * frac1 + l.g2 * frac2;
        result.b := l.b1 * frac1 + l.b2 * frac2;
      end
      // colorchance should not be present in pulse light but just in case
      else if l.colorchance > 0 then
      begin
        if C_Random(lightrnd) < l.colorchance then
        begin
          result.r := l.r1;
          result.g := l.g1;
          result.b := l.b1;
        end
        else
        begin
          result.r := l.r2;
          result.g := l.g2;
          result.b := l.b2;
        end;
      end
      else
      begin
        result.r := l.r1;
        result.g := l.g1;
        result.b := l.b1;
      end;

      // Determine size
      if l.interval > 0 then
      begin
        frac := FixedDiv(l.randomseed + leveltime * FRACUNIT, l.interval) mod l.interval;
        frac1 := frac / l.interval;
        frac2 := 1.0 - frac1;
        result.radius := l.size1 * frac1 + l.size2 * frac2;
      end
      // chance should be present in pulse light but just in case
      else if l.chance > 0 then
      begin
        if C_Random(lightrnd) < l.chance then
          result.radius := l.size1
        else
          result.radius := l.size2;
      end
      else
        result.radius := l.size1;

      _CalcOffset(result, l.validcount < 0);
      l.validcount := leveltime;
    end;
    exit;
  end;

end;

end.

