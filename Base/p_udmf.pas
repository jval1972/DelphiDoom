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
//  Basic UDMF loader
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_udmf;

interface

uses
  d_delphi,
  doomdata,
  m_fixed,
  tables;

procedure UDMF_Check(const mapname: string);

const
  {$IFDEF HEXEN}
  ML_UDMFSTART = Ord(ML_BEHAVIOR) + 1;
  {$ELSE}
  ML_UDMFSTART = Ord(ML_BLOCKMAP) + 1;
  {$ENDIF}
  ML_THINGS2 = ML_UDMFSTART + 0;
  ML_SECTORS2 = ML_UDMFSTART + 1;

const
  UDMF_TF_SKILL1 = 1;
  UDMF_TF_SKILL2 = 2;
  UDMF_TF_SKILL3 = 4;
  UDMF_TF_SKILL4 = 8;
  UDMF_TF_SKILL5 = $10;
  UDMF_TF_SKILL_MASK = $1F;
  UDMF_TF_HASZ = $20;

type
  extrathing_t = record
    extraflags: Integer;
    x, y, z: fixed_t;
  end;
  Pextrathing_t = ^extrathing_t;
  extrathing_tArray = array[0..$FFFF] of extrathing_t;
  Pextrathing_tArray = ^extrathing_tArray;

const
  UDMF_SF_CEILINGPLANE_A = $1;
  UDMF_SF_CEILINGPLANE_B = $2;
  UDMF_SF_CEILINGPLANE_C = $4;
  UDMF_SF_CEILINGPLANE_D = $8;
  UDMF_SF_CEILINGPLANE = UDMF_SF_CEILINGPLANE_A or UDMF_SF_CEILINGPLANE_B or UDMF_SF_CEILINGPLANE_C or UDMF_SF_CEILINGPLANE_D;
  UDMF_SF_FLOORPLANE_A = $10;
  UDMF_SF_FLOORPLANE_B = $20;
  UDMF_SF_FLOORPLANE_C = $40;
  UDMF_SF_FLOORPLANE_D = $80;
  UDMF_SF_FLOORPLANE = UDMF_SF_FLOORPLANE_A or UDMF_SF_FLOORPLANE_B or UDMF_SF_FLOORPLANE_C or UDMF_SF_FLOORPLANE_D;
  UDMF_SF_RIPPLECEILING = $100;
  UDMF_SF_RIPPLEFLOOR = $200;

type
  extrasector_t = record
    extraflags: Integer;
    {$IFDEF DOOM_OR_STRIFE}
    xpanningfloor: fixed_t;   // X texture offset of floor texture, Default = 0.0.
    ypanningfloor: fixed_t;   // Y texture offset of floor texture, Default = 0.0.
    xpanningceiling: fixed_t; // X texture offset of ceiling texture, Default = 0.0.
    ypanningceiling: fixed_t; // Y texture offset of ceiling texture, Default = 0.0.
    {$ENDIF}
    rotationfloor: angle_t;   // Rotation of floor texture (degrees in TEXMAP)
    rotationfloorx: fixed_t;  // x/y rover for rotating floor
    rotationfloory: fixed_t;
    rotationceiling: angle_t; // Rotation of ceiling texture (degrees in TEXMAP)
    rotationceilingx: fixed_t;// x/y rover for rotating ceiling
    rotationceilingy: fixed_t;// x/y rover for rotating ceiling
    gravity: fixed_t;         // Default is 1.0
    ceilingplane_a: float;    // Define the plane equation for the sector's ceiling. Default is a horizontal plane at 'heightceiling'.
    ceilingplane_b: float;    // 'heightceiling' will still be used to calculate texture alignment.
    ceilingplane_c: float;    // The plane equation will only be used if all 4 values are given.
    ceilingplane_d: float;
    floorplane_a: float;      // Define the plane equation for the sector's floor. Default is a horizontal plane at 'heightfloor'.
    floorplane_b: float;      // 'heightfloor' will still be used to calculate texture alignment.
    floorplane_c: float;      // The plane equation will only be used if all 4 values are given.
    floorplane_d: float;
  end;
  Pextrasector_t = ^extrasector_t;
  extrasector_tArray = array[0..$FFFF] of extrasector_t;
  Pextrasector_tArray = ^extrasector_tArray;

implementation

uses
  doomdef,
  m_argv,
  m_crc32,
  m_base,
  i_system,
  sc_engine,
  sc_tokens,
{$IFDEF HEXEN}
  z_zone,
{$ENDIF}
  w_wad;

type
  TUDMFManager = class
  private
    fthings: Pmapthing_tArray;
    fextrathings: Pextrathing_tArray;
    fnumthings: integer;
    fmaplinedefs: Pmaplinedef_tArray;
    fnummaplinedefs: integer;
    fmapsidedefs: Pmapsidedef_tArray;
    fnummapsidedefs: integer;
    fmapvertexes: Pmapvertex_tArray;
    fnummapvertexes: integer;
    fmapsectors: Pmapsector_tArray;
    fextrasectors: Pextrasector_tArray;
    fnummapsectors: integer;
    function _udmfPreproccessor(atext: string): string;
  public
    constructor Create; virtual;
    procedure LoadFromString(const atext: string);
    procedure SaveUDMFToVanilla(const amapname: string; const afilename: string{$IFDEF HEXEN};const bl: integer{$ENDIF});
    procedure Clear;
    destructor Destroy; override;
  end;


constructor TUDMFManager.Create;
begin
  fthings := nil;
  fextrathings := nil;
  fnumthings := 0;
  fmaplinedefs := nil;
  fnummaplinedefs := 0;
  fmapsidedefs := nil;
  fnummapsidedefs := 0;
  fmapvertexes := nil;
  fnummapvertexes := 0;
  fmapsectors := nil;
  fextrasectors := nil;
  fnummapsectors := 0;
end;

function TUDMFManager._udmfPreproccessor(atext: string): string;
var
  i: integer;
  len: integer;
  inquotes: boolean;
  incomments: boolean;
  inLcomments: boolean;
begin
  atext := atext + #13#10;
  result := '';
  inquotes := false;
  incomments := false;
  inLcomments := false;
  i := 1;
  len := Length(atext);
  while i < len do
  begin
    if atext[i] = '"' then
      if not (incomments or inLcomments) then
        inquotes := not inquotes;
    if not inquotes then
    begin
      if atext[i] = '/' then
      begin
        if atext[i + 1] = '/' then
          inLcomments := true
        else if atext[i + 1] = '*' then
          incomments := true;
      end;
      if atext[i] in [#13, #10] then
        inLcomments := false;
      if atext[i] = '*' then
        if atext[i + 1] = '/' then
          incomments := false;
    end;
    if not (incomments and inLcomments) then
    begin
      if inquotes then
        result := result + atext[i]
      else if atext[i] = '=' then
        result := result + ' '
      else if atext[i] = '{' then
        result := result + #13#10'_BEGINBLOCK'#13#10
      else if atext[i] = '}' then
        result := result + #13#10'_ENDBLOCK'#13#10
      else if atext[i] = ';' then
        result := result + #13#10
      else
        result := result + atext[i]
    end;
    inc(i);
  end;
end;

procedure TUDMFManager.LoadFromString(const atext: string);
var
  sc: TScriptEngine;
  token: string;
  pthing: Pmapthing_t;
  pextrathing: Pextrathing_t;
  pmaplinedef: Pmaplinedef_t;
  pmapsidedef: Pmapsidedef_t;
  pmapvertex: Pmapvertex_t;
  pmapsector: Pmapsector_t;
  pextrasector: Pextrasector_t;

  function GetToken: boolean;
  begin
    result := sc.GetString;
    if result then
      token := strupper(sc._String)
    else
      token := '';
  end;

begin
  Clear;
  sc := TScriptEngine.Create(_udmfPreproccessor(atext));
  while GetToken do
  begin
    if token = 'NAMESPACE' then
    begin
      GetToken;
      if (token <> strupper(_GAME)) and (token <> 'DELPHI' + strupper(_GAME)) and (token <> 'ZDOOMTRANSLATED') then
      begin
        I_Warning('TUDMFManager.LoadFromString(): Unknown namespace "%s"'#13#10, [sc._String]);
//        sc.Free;
//        exit;
      end;
    end;
    if token = 'THING' then
    begin
      realloc(Pointer(fthings), fnumthings * SizeOf(mapthing_t), (fnumthings + 1) * SizeOf(mapthing_t));
      pthing := @fthings[fnumthings];
      ZeroMemory(pthing, SizeOf(mapthing_t));
      realloc(Pointer(fextrathings), fnumthings * SizeOf(extrathing_t), (fnumthings + 1) * SizeOf(extrathing_t));
      pextrathing := @fextrathings[fnumthings];
      ZeroMemory(pextrathing, SizeOf(mapthing_t));
      {$IFNDEF HEXEN}pthing.options := 16{$ENDIF};
      inc(fnumthings);
      GetToken; // _BEGINBLOCK
      if token <> '_BEGINBLOCK' then
        I_Warning('TUDMFManager.LoadFromString(): Unexpected token "%s" in thing definition'#13#10, [token]);
      while token <> '_ENDBLOCK' do
      begin
        GetToken;
        if token = 'X' then
        begin
          sc.MustGetFloat;
          pthing.x := Round(sc._Float);
          pextrathing.x := Round(sc._Float * FRACUNIT);
        end
        else if token = 'Y' then
        begin
          sc.MustGetFloat;
          pthing.y := Round(sc._Float);
          pextrathing.y := Round(sc._Float * FRACUNIT);
        end
        else if token = 'Z' then
        begin
          sc.MustGetFloat;
          pextrathing.z := Round(sc._Float * FRACUNIT);
          pextrathing.extraflags := pextrathing.extraflags or UDMF_TF_HASZ;
        end
        else if token = 'ANGLE' then
        begin
          sc.MustGetFloat;
          pthing.angle := Round(sc._Float);
        end
        else if token = 'TYPE' then
        begin
          sc.MustGetInteger;
          pthing._type := sc._Integer;
        end
        else if (token = 'SKILL1') or (token = 'SKILL2') then
        begin
          GetToken;
          if token = 'TRUE' then
          begin
            pthing.options := pthing.options or 1;
            if token = 'SKILL1' then
              pextrathing.extraflags := pextrathing.extraflags or UDMF_TF_SKILL1
            else
              pextrathing.extraflags := pextrathing.extraflags or UDMF_TF_SKILL2;
          end;
        end
        else if (token = 'SKILL3') then
        begin
          GetToken;
          if token = 'TRUE' then
          begin
            pthing.options := pthing.options or 2;
            pextrathing.extraflags := pextrathing.extraflags or UDMF_TF_SKILL3;
          end;
        end
        else if (token = 'SKILL4') or (token = 'SKILL5') then
        begin
          GetToken;
          if token = 'TRUE' then
          begin
            pthing.options := pthing.options or 4;
            if token = 'SKILL4' then
              pextrathing.extraflags := pextrathing.extraflags or UDMF_TF_SKILL4
            else
              pextrathing.extraflags := pextrathing.extraflags or UDMF_TF_SKILL5;
          end;
        end
        else if (token = 'AMBUSH') then
        begin
          GetToken;
          if token = 'TRUE' then
            pthing.options := pthing.options or 8;
        end
        else if (token = 'ONMIDDLEFLOOR') then
        begin
          GetToken;
          if token = 'TRUE' then
            pthing.options := pthing.options or MTF_ONMIDSECTOR;
        end
        else if (token = 'NOTRIGGERSCRIPTS') then
        begin
          GetToken;
          if token = 'TRUE' then
            pthing.options := pthing.options or MTF_DONOTTRIGGERSCRIPTS;
        end
        else if (token = 'FRIEND') then
        begin
          GetToken;
          if token = 'TRUE' then
            pthing.options := pthing.options or MTF_FRIEND;
        end
        {$IFDEF HEXEN}
        else if (token = 'SINGLE') then
        begin
          GetToken;
          if token = 'TRUE' then
            pthing.options := pthing.options or MTF_GSINGLE;
        end
        else if (token = 'COOP') then
        begin
          GetToken;
          if token = 'TRUE' then
            pthing.options := pthing.options or MTF_GCOOP;
        end
        else if (token = 'DM') then
        begin
          GetToken;
          if token = 'TRUE' then
            pthing.options := pthing.options or MTF_GDEATHMATCH;
        end
        else if (token = 'DORMANT') then
        begin
          GetToken;
          if token = 'TRUE' then
            pthing.options := pthing.options or MTF_DORMANT;
        end
        else if (token = 'CLASS1') then
        begin
          GetToken;
          if token = 'TRUE' then
            pthing.options := pthing.options or MTF_FIGHTER;
        end
        else if (token = 'CLASS2') then
        begin
          GetToken;
          if token = 'TRUE' then
            pthing.options := pthing.options or MTF_CLERIC;
        end
        else if (token = 'CLASS3') then
        begin
          GetToken;
          if token = 'TRUE' then
            pthing.options := pthing.options or MTF_MAGE;
        end
        else if (token = 'SPECIAL') then
        begin
          sc.MustGetInteger;
          pthing.special := sc._Integer;
        end
        else if (token = 'ARG0') then
        begin
          sc.MustGetInteger;
          pthing.arg1 := sc._Integer;
        end
        else if (token = 'ARG1') then
        begin
          sc.MustGetInteger;
          pthing.arg2 := sc._Integer;
        end
        else if (token = 'ARG2') then
        begin
          sc.MustGetInteger;
          pthing.arg3 := sc._Integer;
        end
        else if (token = 'ARG3') then
        begin
          sc.MustGetInteger;
          pthing.arg4 := sc._Integer;
        end
        else if (token = 'ARG4') then
        begin
          sc.MustGetInteger;
          pthing.arg5 := sc._Integer;
        end
        {$ELSE}
        else if (token = 'SINGLE') then
        begin
          GetToken;
          if token = 'TRUE' then
            pthing.options := pthing.options and not 16;
        end
        else if (token = 'COOP') then
        begin
          GetToken;
          if token = 'TRUE' then
            pthing.options := pthing.options or 32;
        end
        else if (token = 'DM') then
        begin
          GetToken;
          if token = 'TRUE' then
            pthing.options := pthing.options or 64;
        end
        {$ENDIF}
        else if (token = 'COMMENT') then
        begin
          GetToken; // skip comment
        end;
      end;
    end
    else if token = 'LINEDEF' then
    begin
      realloc(Pointer(fmaplinedefs), fnummaplinedefs * SizeOf(maplinedef_t), (fnummaplinedefs + 1) * SizeOf(maplinedef_t));
      pmaplinedef := @fmaplinedefs[fnummaplinedefs];
      ZeroMemory(pmaplinedef, SizeOf(maplinedef_t));
      {$IFNDEF HEXEN}
      pmaplinedef.tag := -1;
      {$ENDIF}
      pmaplinedef.sidenum[0] := -1;
      pmaplinedef.sidenum[1] := -1;
      inc(fnummaplinedefs);
      GetToken; // _BEGINBLOCK
      if token <> '_BEGINBLOCK' then
        I_Warning('TUDMFManager.LoadFromString(): Unexpected token "%s" in linedef definition'#13#10, [token]);
      while token <> '_ENDBLOCK' do
      begin
        GetToken;
        if (token = 'ID') then
        begin
          sc.MustGetInteger;
          {$IFDEF HEXEN}
          pmaplinedef.special := 121;
          pmaplinedef.arg1 := sc._Integer;
          {$ELSE}
          pmaplinedef.tag := sc._Integer;
          {$ENDIF}
        end
        else if (token = 'V1') then
        begin
          sc.MustGetInteger;
          pmaplinedef.v1 := sc._Integer;
        end
        else if (token = 'V2') then
        begin
          sc.MustGetInteger;
          pmaplinedef.v2 := sc._Integer;
        end
        else if (token = 'SPECIAL') then
        begin
          sc.MustGetInteger;
          pmaplinedef.special := sc._Integer;
        end
        else if (token = 'BLOCKING') or (token = 'BLOCKEVERYTHING')  then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or ML_BLOCKING;
        end
        else if (token = 'BLOCKMONSTERS') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or ML_BLOCKMONSTERS;
        end
        else if (token = 'TWOSIDED') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or ML_TWOSIDED;
        end
        else if (token = 'DONTPEGTOP') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or ML_DONTPEGTOP;
        end
        else if (token = 'DONTPEGBOTTOM') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or ML_DONTPEGBOTTOM;
        end
        else if (token = 'SECRET') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or ML_SECRET;
        end
        else if (token = 'BLOCKSOUND') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or ML_SOUNDBLOCK;
        end
        else if (token = 'DONTDRAW') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or ML_DONTDRAW;
        end
        else if (token = 'MAPPED') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or ML_SECRET;
        end
        else if (token = 'TRIGGERSCRIPTS') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or ML_TRIGGERSCRIPTS;
        end
        {$IFDEF DOOM_OR_HERETIC}
        else if (token = 'BLOCKLANDMONSTERS') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or ML_BLOCKLANDMONSTERS;
        end
        else if (token = 'BLOCKPLAYERS') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or ML_BLOCKPLAYERS;
        end
        {$ENDIF}
        else if (token = 'NOCLIPPING') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or ML_NOCLIP;
        end
        {$IFDEF DOOM_OR_STRIFE}
        else if (token = 'PASSUSE') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or ML_PASSUSE;
        end
        {$ENDIF}
        {$IFDEF STRIFE}
        else if (token = 'TRANSLUCENT') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or ML_TRANSPARENT1;
        end
        else if (token = 'TRANSLUCENT2') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or ML_TRANSPARENT2;
        end
        else if (token = 'JUMPOVER') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or ML_JUMPOVER;
        end
        else if (token = 'BLOCKFLOATERS') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or ML_BLOCKFLOATERS;
        end
        {$ENDIF}
        {$IFDEF HEXEN}
        else if (token = 'REPEATSPECIAL') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or ML_REPEAT_SPECIAL;
        end
        else if (token = 'PLAYERCROSS') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or _SHL(SPAC_CROSS, ML_SPAC_SHIFT);
        end
        else if (token = 'PLAYERUSE') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or _SHL(SPAC_USE, ML_SPAC_SHIFT);
        end
        else if (token = 'MONSTERCROSS') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or _SHL(SPAC_MCROSS, ML_SPAC_SHIFT);
        end
        else if (token = 'IMPACT') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or _SHL(SPAC_IMPACT, ML_SPAC_SHIFT);
        end
        else if (token = 'PLAYERPUSH') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or _SHL(SPAC_PUSH, ML_SPAC_SHIFT);
        end
        else if (token = 'MISSILECROSS') then
        begin
          GetToken;
          if token = 'TRUE' then
            pmaplinedef.flags := pmaplinedef.flags or _SHL(SPAC_PCROSS, ML_SPAC_SHIFT);
        end
        else if (token = 'ARG0') then
        begin
          sc.MustGetInteger;
          pmaplinedef.arg1 := sc._Integer;
        end
        else if (token = 'ARG1') then
        begin
          sc.MustGetInteger;
          pmaplinedef.arg2 := sc._Integer;
        end
        else if (token = 'ARG2') then
        begin
          sc.MustGetInteger;
          pmaplinedef.arg3 := sc._Integer;
        end
        else if (token = 'ARG3') then
        begin
          sc.MustGetInteger;
          pmaplinedef.arg4 := sc._Integer;
        end
        else if (token = 'ARG4') then
        begin
          sc.MustGetInteger;
          pmaplinedef.arg5 := sc._Integer;
        end
        {$ENDIF}
        else if (token = 'SIDEFRONT') then
        begin
          sc.MustGetInteger;
          if sc._Integer > 32768 then
            pmaplinedef.sidenum[0] := -1
          else
            pmaplinedef.sidenum[0] := sc._Integer;
        end
        else if (token = 'SIDEBACK') then
        begin
          sc.MustGetInteger;
          if sc._Integer > 32768 then
            pmaplinedef.sidenum[1] := -1
          else
            pmaplinedef.sidenum[1] := sc._Integer;
        end
        else if (token = 'COMMENT') then
        begin
          GetToken; // skip comment
        end;
      end;
    end
    else if token = 'SIDEDEF' then
    begin
      realloc(Pointer(fmapsidedefs), fnummapsidedefs * SizeOf(mapsidedef_t), (fnummapsidedefs + 1) * SizeOf(mapsidedef_t));
      pmapsidedef := @fmapsidedefs[fnummapsidedefs];
      ZeroMemory(pmapsidedef, SizeOf(mapsidedef_t));
      pmapsidedef.toptexture[0] := '-';
      pmapsidedef.bottomtexture[0] := '-';
      pmapsidedef.midtexture[0] := '-';
      inc(fnummapsidedefs);
      GetToken; // _BEGINBLOCK
      if token <> '_BEGINBLOCK' then
        I_Warning('TUDMFManager.LoadFromString(): Unexpected token "%s" in sidedef definition'#13#10, [token]);
      while token <> '_ENDBLOCK' do
      begin
        GetToken;
        if (token = 'OFFSETX') then
        begin
          sc.MustGetFloat;
          pmapsidedef.textureoffset := Round(sc._Float);
        end
        else if (token = 'OFFSETY') then
        begin
          sc.MustGetFloat;
          pmapsidedef.rowoffset := Round(sc._Float);
        end
        else if (token = 'TEXTURETOP') then
        begin
          GetToken;
          pmapsidedef.toptexture := stringtochar8(token);
        end
        else if (token = 'TEXTUREBOTTOM') then
        begin
          GetToken;
          pmapsidedef.bottomtexture := stringtochar8(token);
        end
        else if (token = 'TEXTUREMIDDLE') then
        begin
          GetToken;
          pmapsidedef.midtexture := stringtochar8(token);
        end
        else if (token = 'SECTOR') then
        begin
          sc.MustGetInteger;
          pmapsidedef.sector := sc._Integer;
        end
        else if (token = 'COMMENT') then
        begin
          GetToken; // skip comment
        end;
      end;
    end
    else if token = 'VERTEX' then
    begin
      realloc(Pointer(fmapvertexes), fnummapvertexes * SizeOf(mapvertex_t), (fnummapvertexes + 1) * SizeOf(mapvertex_t));
      pmapvertex := @fmapvertexes[fnummapvertexes];
      ZeroMemory(pmapvertex, SizeOf(mapvertex_t));
      inc(fnummapvertexes);
      GetToken; // _BEGINBLOCK
      if token <> '_BEGINBLOCK' then
        I_Warning('TUDMFManager.LoadFromString(): Unexpected token "%s" in vertex definition'#13#10, [token]);
      while token <> '_ENDBLOCK' do
      begin
        GetToken;
        if (token = 'X') then
        begin
          sc.MustGetFloat;
          pmapvertex.x := Round(sc._Float);
        end
        else if (token = 'Y') then
        begin
          sc.MustGetFloat;
          pmapvertex.y := Round(sc._Float);
        end
        else if (token = 'COMMENT') then
        begin
          GetToken; // skip comment
        end;
      end;
    end
    else if token = 'SECTOR' then
    begin
      realloc(Pointer(fmapsectors), fnummapsectors * SizeOf(mapsector_t), (fnummapsectors + 1) * SizeOf(mapsector_t));
      pmapsector := @fmapsectors[fnummapsectors];
      ZeroMemory(pmapsector, SizeOf(mapsector_t));
      pmapsector.lightlevel := 160;
      realloc(Pointer(fextrasectors), fnummapsectors * SizeOf(extrasector_t), (fnummapsectors + 1) * SizeOf(extrasector_t));
      pextrasector := @fextrasectors[fnummapsectors];
      ZeroMemory(fextrasectors, SizeOf(mapsector_t));
      pextrasector.gravity := FRACUNIT;
      inc(fnummapsectors);
      GetToken; // _BEGINBLOCK
      if token <> '_BEGINBLOCK' then
        I_Warning('TUDMFManager.LoadFromString(): Unexpected token "%s" in sector definition'#13#10, [token]);
      while token <> '_ENDBLOCK' do
      begin
        GetToken;
        if (token = 'HEIGHTFLOOR') then
        begin
          sc.MustGetFloat;
          pmapsector.floorheight := Round(sc._Float);
        end
        else if (token = 'HEIGHTCEILING') then
        begin
          sc.MustGetFloat;
          pmapsector.ceilingheight := Round(sc._Float);
        end
        else if (token = 'TEXTUREFLOOR') then
        begin
          GetToken;
          pmapsector.floorpic := stringtochar8(token);
        end
        else if (token = 'TEXTURECEILING') then
        begin
          GetToken;
          pmapsector.ceilingpic := stringtochar8(token);
        end
        else if (token = 'LIGHTLEVEL') then
        begin
          sc.MustGetInteger;
          pmapsector.lightlevel := sc._Integer;
        end
        else if (token = 'SPECIAL') then
        begin
          sc.MustGetInteger;
          pmapsector.special := sc._Integer;
        end
        else if (token = 'ID') then
        begin
          sc.MustGetInteger;
          pmapsector.tag := sc._Integer;
        end
        {$IFDEF DOOM_OR_STRIFE}
        else if (token = 'XPANNINGFLOOR') then
        begin
          sc.MustGetFloat;
          pextrasector.xpanningfloor := Round(sc._Float * FRACUNIT);
        end
        else if (token = 'YPANNINGFLOOR') then
        begin
          sc.MustGetFloat;
          pextrasector.ypanningfloor := Round(sc._Float * FRACUNIT);
        end
        else if (token = 'XPANNINGCEILING') then
        begin
          sc.MustGetFloat;
          pextrasector.xpanningceiling := Round(sc._Float * FRACUNIT);
        end
        else if (token = 'YPANNINGCEILING') then
        begin
          sc.MustGetFloat;
          pextrasector.ypanningceiling := Round(sc._Float * FRACUNIT);
        end
        {$ENDIF}
        else if (token = 'ROTATIONFLOOR') then
        begin
          sc.MustGetFloat;
          pextrasector.rotationfloor := Round(sc._Float * ANG1);
        end
        else if (token = 'ROTATIONFLOORX') then
        begin
          sc.MustGetFloat;
          pextrasector.rotationfloorx := Round(sc._Float * FRACUNIT);
        end
        else if (token = 'ROTATIONFLOORY') then
        begin
          sc.MustGetFloat;
          pextrasector.rotationfloory := Round(sc._Float * FRACUNIT);
        end
        else if (token = 'ROTATIONCEILING') then
        begin
          sc.MustGetFloat;
          pextrasector.rotationceiling := Round(sc._Float * ANG1);
        end
        else if (token = 'ROTATIONCEILINGX') then
        begin
          sc.MustGetFloat;
          pextrasector.rotationceilingx := Round(sc._Float * FRACUNIT);
        end
        else if (token = 'ROTATIONCEILINGY') then
        begin
          sc.MustGetFloat;
          pextrasector.rotationceilingy := Round(sc._Float * FRACUNIT);
        end
        else if (token = 'GRAVITY') then
        begin
          sc.MustGetFloat;
          pextrasector.gravity := Round(sc._Float * FRACUNIT);
        end
        else if (token = 'CEILINGPLANE_A') then
        begin
          sc.MustGetFloat;
          pextrasector.ceilingplane_a := sc._Float;
          pextrasector.extraflags := pextrasector.extraflags or UDMF_SF_CEILINGPLANE_A;
        end
        else if (token = 'CEILINGPLANE_B') then
        begin
          sc.MustGetFloat;
          pextrasector.ceilingplane_b := sc._Float;
          pextrasector.extraflags := pextrasector.extraflags or UDMF_SF_CEILINGPLANE_B;
        end
        else if (token = 'CEILINGPLANE_C') then
        begin
          sc.MustGetFloat;
          pextrasector.ceilingplane_c := sc._Float;
          pextrasector.extraflags := pextrasector.extraflags or UDMF_SF_CEILINGPLANE_C;
        end
        else if (token = 'CEILINGPLANE_D') then
        begin
          sc.MustGetFloat;
          pextrasector.ceilingplane_d := sc._Float;
          pextrasector.extraflags := pextrasector.extraflags or UDMF_SF_CEILINGPLANE_D;
        end
        else if (token = 'FLOORPLANE_A') then
        begin
          sc.MustGetFloat;
          pextrasector.floorplane_a := sc._Float;
          pextrasector.extraflags := pextrasector.extraflags or UDMF_SF_FLOORPLANE_A;
        end
        else if (token = 'FLOORPLANE_B') then
        begin
          sc.MustGetFloat;
          pextrasector.floorplane_b := sc._Float;
          pextrasector.extraflags := pextrasector.extraflags or UDMF_SF_FLOORPLANE_B;
        end
        else if (token = 'FLOORPLANE_C') then
        begin
          sc.MustGetFloat;
          pextrasector.floorplane_c := sc._Float;
          pextrasector.extraflags := pextrasector.extraflags or UDMF_SF_FLOORPLANE_C;
        end
        else if (token = 'FLOORPLANE_D') then
        begin
          sc.MustGetFloat;
          pextrasector.floorplane_d := sc._Float;
          pextrasector.extraflags := pextrasector.extraflags or UDMF_SF_FLOORPLANE_D;
        end
        else if (token = 'COMMENT') then
        begin
          GetToken; // skip comment
        end;
      end;
    end
  end;
  sc.Free;
end;

procedure TUDMFManager.SaveUDMFToVanilla(const amapname: string; const afilename: string{$IFDEF HEXEN};const bl: integer{$ENDIF});
var
  header: wadinfo_t;
  infotable: array[0..{$IFDEF HEXEN}13{$ELSE}12{$ENDIF}] of filelump_t;
  f: TFile;
{$IFDEF HEXEN}
  bldata: PByteArray;
  bllen: integer;
{$ENDIF}
begin
  header.identification :=
    integer(Ord('P') or (Ord('W') shl 8) or (Ord('A') shl 16) or (Ord('D') shl 24));
  header.numlumps := {$IFDEF HEXEN}12{$ELSE}11{$ENDIF};

  f := TFile.Create(afilename, fCreate);
  f.Write(header, SizeOf(header));

  ZeroMemory(@infotable, SizeOf(infotable));

  infotable[0].filepos := f.Position;
  infotable[0].size := 0;
  infotable[0].name := stringtochar8(strupper(amapname));

  infotable[1].filepos := f.Position;
  infotable[1].size := fnumthings * SizeOf(mapthing_t);
  infotable[1].name := stringtochar8('THINGS');
  f.Write(fthings^, fnumthings * SizeOf(mapthing_t));

  infotable[2].filepos := f.Position;
  infotable[2].size := fnummaplinedefs * SizeOf(maplinedef_t);
  infotable[2].name := stringtochar8('LINEDEFS');
  f.Write(fmaplinedefs^, fnummaplinedefs * SizeOf(maplinedef_t));

  infotable[3].filepos := f.Position;
  infotable[3].size := fnummapsidedefs * SizeOf(mapsidedef_t);
  infotable[3].name := stringtochar8('SIDEDEFS');
  f.Write(fmapsidedefs^, fnummapsidedefs * SizeOf(mapsidedef_t));

  infotable[4].filepos := f.Position;
  infotable[4].size := fnummapvertexes * SizeOf(mapvertex_t);
  infotable[4].name := stringtochar8('VERTEXES');
  f.Write(fmapvertexes^, fnummapvertexes * SizeOf(mapvertex_t));

  infotable[5].filepos := f.Position;
  infotable[5].size := 0;
  infotable[5].name := stringtochar8('SEGS');

  infotable[6].filepos := f.Position;
  infotable[6].size := 0;
  infotable[6].name := stringtochar8('SSECTORS');

  infotable[7].filepos := f.Position;
  infotable[7].size := 0;
  infotable[7].name := stringtochar8('NODES');

  infotable[8].filepos := f.Position;
  infotable[8].size := fnummapsectors * SizeOf(mapsector_t);
  infotable[8].name := stringtochar8('SECTORS');
  f.Write(fmapsectors^, fnummapsectors * SizeOf(mapsector_t));

  infotable[9].filepos := f.Position;
  infotable[9].size := 0;
  infotable[9].name := stringtochar8('REJECT');

  infotable[10].filepos := f.Position;
  infotable[10].size := 0;
  infotable[10].name := stringtochar8('BLOCKMAP');

  {$IFDEF HEXEN}
  infotable[11].filepos := f.Position;
  if bl > 0 then
  begin
    bldata := W_CacheLumpNum(bl, PU_STATIC);
    bllen := W_LumpLength(bl);
    infotable[11].size := bllen;
    f.Write(bldata^, bllen);
    Z_Free(bldata);
  end
  else
    infotable[11].size := 0;
  infotable[11].name := stringtochar8('BEHAVIOR');
  {$ENDIF}

  infotable[ML_THINGS2].filepos := f.Position;
  infotable[ML_THINGS2].size := fnumthings * SizeOf(extrathing_t);
  infotable[ML_THINGS2].name := stringtochar8('THINGS2');
  f.Write(fextrathings^, fnumthings * SizeOf(extrathing_t));

  infotable[ML_SECTORS2].filepos := f.Position;
  infotable[ML_SECTORS2].size := fnummapsectors * SizeOf(extrasector_t);
  infotable[ML_SECTORS2].name := stringtochar8('SECTORS2');
  f.Write(fextrasectors^, fnummapsectors * SizeOf(extrasector_t));

  header.infotableofs := f.Position;
  f.Write(infotable, SizeOf(infotable));
  f.Seek(0, sFromBeginning);
  f.Write(header, SizeOf(header));
  f.Free;
end;

procedure TUDMFManager.Clear;
begin
  memfree(Pointer(fthings), fnumthings * SizeOf(mapthing_t));
  memfree(Pointer(fextrathings), fnumthings * SizeOf(extrathing_t));
  memfree(Pointer(fmaplinedefs), fnummaplinedefs * SizeOf(maplinedef_t));
  memfree(Pointer(fmapsidedefs), fnummapsidedefs * SizeOf(mapsidedef_t));
  memfree(Pointer(fmapvertexes), fnummapvertexes * SizeOf(mapvertex_t));
  memfree(Pointer(fmapsectors), fnummapsectors * SizeOf(mapsector_t));
  memfree(Pointer(fextrasectors), fnummapsectors * SizeOf(extrasector_t));
end;

destructor TUDMFManager.Destroy;
begin
  Clear;
  inherited;
end;

//------------------------------------------------------------------------------
procedure UDMF_Check(const mapname: string);
var
  udmf: TUDMFManager;
  lumpnum: integer;
  crc32: string;
  wadfilemap: string;
  {$IFDEF HEXEN}
  behav_lump: integer;
  {$ENDIF}
begin
  lumpnum := W_GetNumForName(mapname);
  if (lumpnum < 0) or (lumpnum >= W_NumLumps - 1) then
    exit;

  if strupper(stringtochar8(lumpinfo[lumpnum + 1].name)) <> 'TEXTMAP' then
    exit;

  {$IFDEF HEXEN}
  behav_lump := -1;
  if lumpnum + 2 < W_NumLumps then
    if strupper(stringtochar8(lumpinfo[lumpnum + 2].name)) = 'BEHAVIOR' then
      behav_lump := lumpnum + 2;
  {$ENDIF}

  inc(lumpnum);
  crc32 := GetLumpCRC32(lumpnum);

  wadfilemap := M_SaveFileName('DATA\');
  MkDir(wadfilemap);
  wadfilemap := wadfilemap + 'WADS\';
  MkDir(wadfilemap);
  wadfilemap := wadfilemap + mapname + '_' + crc32 + '.wad';

  udmf := TUDMFManager.Create;
  udmf.LoadFromString(W_TextLumpNum(lumpnum));
  udmf.SaveUDMFToVanilla(mapname, wadfilemap{$IFDEF HEXEN}, behav_lump{$ENDIF});
  udmf.Free;

  W_RuntimeLoad(wadfilemap, F_ORIGIN_WAD);
end;

end.
