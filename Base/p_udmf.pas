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

//==============================================================================
//
// UDMF_Check
//
//==============================================================================
function UDMF_Check(const mapname: string): boolean;

const
  ML_SCRIPT = Ord(ML_BEHAVIOR) + 1;
  ML_UDMFSTART = Ord(ML_BEHAVIOR) + 2;
  ML_THINGS2 = ML_UDMFSTART + 0;
  ML_SECTORS2 = ML_UDMFSTART + 1;
  ML_VERTEX2 = ML_UDMFSTART + 2;
  ML_LINEDEF2 = ML_UDMFSTART + 3;
  ML_SIDEDEF2 = ML_UDMFSTART + 4;
  ML_NSPACE = ML_UDMFSTART + 5;
  ML_NUMUDMFLUMPS = ML_UDMFSTART + 6;

  MLN_THINGS2 = 'THINGS2';
  MLN_SECTORS2 = 'SECTORS2';
  MLN_VERTEX2 = 'VERTEX2';
  MLN_LINEDEF2 = 'LINEDEF2';
  MLN_SIDEDEF2 = 'SIDEDEF2';
  MLN_NSPACE = 'NSPACE';

const
  UDMF_TF_SKILL1 = 1;
  UDMF_TF_SKILL2 = 2;
  UDMF_TF_SKILL3 = 4;
  UDMF_TF_SKILL4 = 8;
  UDMF_TF_SKILL5 = $10;
  UDMF_TF_SKILL_MASK = $1F;
  UDMF_TF_HASZ = $20;
  UDMF_TF_HASGRAVITY = $40;
  UDMF_TF_HASHEALTH = $80;
  UDMF_TF_SINGLE = $100;
  UDMF_TF_DM = $200;
  UDMF_TF_COOP = $400;

type
  extrathing_t = record
    extraflags: Integer;
    x, y, z: fixed_t;
    id: Integer;
    gravity: float;
    health: float;
    {$IFNDEF HEXEN}
    special: integer;
    arg1: integer;
    arg2: integer;
    arg3: integer;
    arg4: integer;
    arg5: integer;
    {$ENDIF}
  end;
  Pextrathing_t = ^extrathing_t;
  extrathing_tArray = array[0..$FFFF] of extrathing_t;
  Pextrathing_tArray = ^extrathing_tArray;

type
  moreids_t = set of Byte;
  Pmoreids_t = ^moreids_t;

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
  UDMF_SF_FOG = $400;
  UDMF_SF_HIDDEN = $800;
  UDMF_SF_NOJUMP = $1000;
  UDMF_SF_NOCROUCH = $2000;

type
  extrasector_t = record
    extraflags: Integer;
    floorheight: fixed_t;
    ceilingheight: fixed_t;
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
    moreids: moreids_t;
    {$IFNDEF HEXEN}
    sound: integer;
    {$ENDIF}
    windthrust: fixed_t;
    windangle: angle_t;
  end;
  Pextrasector_t = ^extrasector_t;
  extrasector_tArray = array[0..$FFFF] of extrasector_t;
  Pextrasector_tArray = ^extrasector_tArray;

const
  UDMF_VF_ZFLOOR = 1;
  UDMF_VF_ZCEILING = 2;

type
  extravertex_t = record
    extraflags: integer;
    x, y: fixed_t;
    zfloor, zceiling: fixed_t; // Absolute floor & ceiling height
  end;
  Pextravertex_t = ^extravertex_t;
  extravertex_tArray = array[0..$FFFF] of extravertex_t;
  Pextravertex_tArray = ^extravertex_tArray;

const
  ULAC_DEFAULT = 1;   // Old style
  ULAC_REPEAT = 2;    // Repeat special
  ULAC_CROSS = 4;     // when player crosses line
  ULAC_USE = 8;       // when player uses line
  ULAC_MCROSS = 16;   // when monster crosses line
  ULAC_IMPACT = 32;   // when projectile hits line
  ULAC_PUSH = 64;     // when player/monster pushes line
  ULAC_PCROSS = 128;  // when projectile crosses line
  {$IFNDEF HEXEN}
  ULAC_MPUSH = 256;   // when monster pushes line
  {$ENDIF}

type
  extraline_t = record
   {$IFNDEF HEXEN}
    arg1: integer;
    arg2: integer;
    arg3: integer;
    arg4: integer;
    arg5: integer;
    activators: integer;  // ULAC_????
    {$ENDIF}
    moreids: moreids_t;
  end;
  Pextraline_t = ^extraline_t;
  extraline_tArray = array[0..$FFFF] of extraline_t;
  Pextraline_tArray = ^extraline_tArray;

type
  extraside_t = record
    textureoffset: fixed_t;
    toptextureoffset: fixed_t;
    bottomtextureoffset: fixed_t;
    midtextureoffset: fixed_t;
    rowoffset: fixed_t;
    toprowoffset: fixed_t;
    bottomrowoffset: fixed_t;
    midrowoffset: fixed_t;
    flags: integer;
  end;
  Pextraside_t = ^extraside_t;
  extraside_tArray = array[0..$FFFF] of extraside_t;
  Pextraside_tArray = ^extraside_tArray;

var
  udmfthings: Pextrathing_tArray;
  numudmfthings: integer;
  udmfsectors: Pextrasector_tArray;
  numudmfsectors: integer;
  udmfvertexes: Pextravertex_tArray;
  numudmfvertexes: integer;
  udmflinedefs: Pextraline_tArray;
  numudmflinedefs: integer;
  udmfsidedefs: Pextraside_tArray;
  numudmfsidedefs: integer;
  hasudmfdata: boolean;
  udmfnamespace: string;
  udmflinetranslate: boolean;

//==============================================================================
//
// UDMF_MakeSectors
//
//==============================================================================
function UDMF_MakeSectors: boolean;

//==============================================================================
//
// UDMF_MakeLines
//
//==============================================================================
function UDMF_MakeLines: boolean;

//==============================================================================
//
// UDMF_MakeSides
//
//==============================================================================
function UDMF_MakeSides: boolean;

//==============================================================================
//
// UDMF_MakeSegs
//
//==============================================================================
function UDMF_MakeSegs: boolean;

const
{$IFNDEF HEXEN}
  UDMF_SPECIAL_START = 700;
  UDMF_SPECIAL_FINISH = UDMF_SPECIAL_START + 256;
  UDMF_NORMAL_ADD = 300;
{$ELSE}
  UDMF_SPECIAL_START = 0;
  UDMF_NORMAL_ADD = 0;
{$ENDIF}

implementation

uses
  doomdef,
  acs,
  m_argv,
  m_crc32,
  m_base,
  i_system,
  p_setup,
  p_slopes,
  p_common,
  r_defs,
  sounds,
  sc_engine,
  sc_tokens,
  z_zone,
  w_wad;

type
  TUDMFManager = class
  private
    // Things
    fthings: Pmapthing_tArray;
    fextrathings: Pextrathing_tArray;
    fnumthings: integer;
    // Lines
    fmaplinedefs: Pmaplinedef_tArray;
    fextralinedefs: Pextraline_tArray;
    fnummaplinedefs: integer;
    // Sides
    fmapsidedefs: Pmapsidedef_tArray;
    fextrasidedefs: Pextraside_tArray;
    fnummapsidedefs: integer;
    // Vertexes
    fmapvertexes: Pmapvertex_tArray;
    fextravertexes: Pextravertex_tArray;
    fnummapvertexes: integer;
    // Sectors
    fmapsectors: Pmapsector_tArray;
    fextrasectors: Pextrasector_tArray;
    fnummapsectors: integer;
    // Namespace
    fnamespace: string;
    flinetranslate: boolean;
    function _udmfPreproccessor(atext: string): string;
  public
    constructor Create; virtual;
    procedure LoadFromString(const atext: string);
    procedure UpdateUDMFGlobalStructs;
    procedure SaveUDMFToVanilla(const amapname: string; const afilename: string;const bl, sl: integer);
    procedure Clear;
    destructor Destroy; override;
    property namespace: string read fnamespace;
  end;

//==============================================================================
//
// TUDMFManager.Create
//
//==============================================================================
constructor TUDMFManager.Create;
begin
  fthings := nil;
  fextrathings := nil;
  fnumthings := 0;
  fmaplinedefs := nil;
  fextralinedefs := nil;
  fnummaplinedefs := 0;
  fmapsidedefs := nil;
  fextrasidedefs := nil;
  fnummapsidedefs := 0;
  fmapvertexes := nil;
  fextravertexes := nil;
  fnummapvertexes := 0;
  fmapsectors := nil;
  fextrasectors := nil;
  fnummapsectors := 0;
  fnamespace := '';
  flinetranslate := false;
end;

//==============================================================================
//
// TUDMFManager._udmfPreproccessor
//
//==============================================================================
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
    if not (incomments or inLcomments) then
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

//==============================================================================
//
// TUDMFManager.LoadFromString
//
//==============================================================================
procedure TUDMFManager.LoadFromString(const atext: string);
var
  sc: TScriptEngine;
  sc2: TScriptEngine;
  token: string;
  pthing: Pmapthing_t;
  pextrathing: Pextrathing_t;
  pmaplinedef: Pmaplinedef_t;
  pextraline: Pextraline_t;
  pmapsidedef: Pmapsidedef_t;
  pextraside: Pextraside_t;
  pmapvertex: Pmapvertex_t;
  pextravertex: Pextravertex_t;
  pmapsector: Pmapsector_t;
  pextrasector: Pextrasector_t;
  sk: string;

  function GetToken: boolean;
  begin
    result := sc.GetString;
    if result then
      token := strupper(sc._String)
    else
      token := '';
  end;

  procedure _LoadThing;
  begin
    realloc(Pointer(fthings), fnumthings * SizeOf(mapthing_t), (fnumthings + 1) * SizeOf(mapthing_t));
    pthing := @fthings[fnumthings];
    ZeroMemory(pthing, SizeOf(mapthing_t));
    realloc(Pointer(fextrathings), fnumthings * SizeOf(extrathing_t), (fnumthings + 1) * SizeOf(extrathing_t));
    pextrathing := @fextrathings[fnumthings];
    ZeroMemory(pextrathing, SizeOf(extrathing_t));
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
      else if token = 'ID' then
      begin
        sc.MustGetInteger;
        pextrathing.id := sc._Integer;
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
        sk := token;
        GetToken;
        if token = 'TRUE' then
        begin
          pthing.options := pthing.options or 1;
          if sk = 'SKILL1' then
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
        sk := token;
        GetToken;
        if token = 'TRUE' then
        begin
          pthing.options := pthing.options or 4;
          if sk = 'SKILL4' then
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
        begin
          pthing.options := pthing.options or MTF_GSINGLE;
          pextrathing.extraflags := pextrathing.extraflags or UDMF_TF_SINGLE;
        end;
      end
      else if (token = 'COOP') then
      begin
        GetToken;
        if token = 'TRUE' then
        begin
          pthing.options := pthing.options or MTF_GCOOP;
          pextrathing.extraflags := pextrathing.extraflags or UDMF_TF_COOP;
        end;
      end
      else if (token = 'DM') then
      begin
        GetToken;
        if token = 'TRUE' then
        begin
          pthing.options := pthing.options or MTF_GDEATHMATCH;
          pextrathing.extraflags := pextrathing.extraflags or UDMF_TF_DM;
        end;
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
        begin
          pthing.options := pthing.options and not 16;
          pextrathing.extraflags := pextrathing.extraflags or UDMF_TF_SINGLE;
        end;
      end
      else if (token = 'COOP') then
      begin
        GetToken;
        if token = 'TRUE' then
        begin
          pthing.options := pthing.options or 32;
          pextrathing.extraflags := pextrathing.extraflags or UDMF_TF_COOP;
        end;
      end
      else if (token = 'DM') then
      begin
        GetToken;
        if token = 'TRUE' then
        begin
          pthing.options := pthing.options or 64;
          pextrathing.extraflags := pextrathing.extraflags or UDMF_TF_DM;
        end;
      end
      else if (token = 'SPECIAL') then
      begin
        sc.MustGetInteger;
        pextrathing.special := sc._Integer;
        if flinetranslate then
        begin
          if IsIntegerInRange(pextrathing.special and 1023, 1, UDMF_NORMAL_ADD) then
            pextrathing.special := (pextrathing.special and 1023 + UDMF_SPECIAL_START) or (pextrathing.special and not 1023)
          else if IsIntegerInRange(pextrathing.special and 1023, UDMF_NORMAL_ADD, UDMF_SPECIAL_START + UDMF_NORMAL_ADD) then
            pextrathing.special := (pextrathing.special and 1023 - UDMF_NORMAL_ADD) or (pextrathing.special and not 1023);
        end;
      end
      else if (token = 'ARG0') then
      begin
        sc.MustGetInteger;
        pextrathing.arg1 := sc._Integer;
      end
      else if (token = 'ARG1') then
      begin
        sc.MustGetInteger;
        pextrathing.arg2 := sc._Integer;
      end
      else if (token = 'ARG2') then
      begin
        sc.MustGetInteger;
        pextrathing.arg3 := sc._Integer;
      end
      else if (token = 'ARG3') then
      begin
        sc.MustGetInteger;
        pextrathing.arg4 := sc._Integer;
      end
      else if (token = 'ARG4') then
      begin
        sc.MustGetInteger;
        pextrathing.arg5 := sc._Integer;
      end
      {$ENDIF}
      {$IFDEF STRIFE}
      else if (token = 'STRIFEALLY') then
      begin
        GetToken;
        if token = 'TRUE' then
          pthing.options := pthing.options or MTF_ALLY;
      end
      {$ENDIF}
      else if (token = 'GRAVITY') then
      begin
        sc.MustGetFloat;
        pextrathing.extraflags := pextrathing.extraflags or UDMF_TF_HASGRAVITY;
        pextrathing.gravity := sc._Float;
      end
      else if (token = 'HEALTH') then
      begin
        sc.MustGetFloat;
        pextrathing.extraflags := pextrathing.extraflags or UDMF_TF_HASHEALTH;
        pextrathing.health := sc._Float;
      end
      else if (token = 'COMMENT') then
      begin
        GetToken; // skip comment
      end;
    end;
  end;

  procedure _LoadLinedef;
  begin
    realloc(Pointer(fmaplinedefs), fnummaplinedefs * SizeOf(maplinedef_t), (fnummaplinedefs + 1) * SizeOf(maplinedef_t));
    pmaplinedef := @fmaplinedefs[fnummaplinedefs];
    ZeroMemory(pmaplinedef, SizeOf(maplinedef_t));
    realloc(Pointer(fextralinedefs), fnummaplinedefs * SizeOf(extraline_t), (fnummaplinedefs + 1) * SizeOf(extraline_t));
    pextraline := @fextralinedefs[fnummaplinedefs];
    ZeroMemory(pextraline, SizeOf(extraline_t));
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
        if IsIntegerInRange(sc._Integer, 0, 255) then
          Include(pextraline.moreids, byte(sc._Integer));
      end
      else if (token = 'MOREIDS') then
      begin
        sc.MustGetString;
        sc2 := TScriptEngine.Create(sc._String);
        while sc2.GetInteger do
          if IsIntegerInRange(sc2._Integer, 0, 255) then
            Include(pextraline.moreids, byte(sc2._Integer));
        sc2.Free;
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
      else if (token = 'BLOCKING') or (token = 'BLOCKEVERYTHING') then
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
      else if (token = 'REPEATSPECIAL') then
      begin
        GetToken;
        if token = 'TRUE' then
          {$IFDEF HEXEN}
          pmaplinedef.flags := pmaplinedef.flags or ML_REPEAT_SPECIAL;
          {$ELSE}
          pextraline.activators := pextraline.activators or ULAC_REPEAT;
          {$ENDIF}
      end
      else if (token = 'PLAYERCROSS') then
      begin
        GetToken;
        if token = 'TRUE' then
          {$IFDEF HEXEN}
          pmaplinedef.flags := pmaplinedef.flags or _SHL(SPAC_CROSS, ML_SPAC_SHIFT);
          {$ELSE}
          pextraline.activators := pextraline.activators or ULAC_CROSS;
          {$ENDIF}
      end
      else if (token = 'PLAYERUSE') then
      begin
        GetToken;
        if token = 'TRUE' then
          {$IFDEF HEXEN}
          pmaplinedef.flags := pmaplinedef.flags or _SHL(SPAC_USE, ML_SPAC_SHIFT);
          {$ELSE}
          pextraline.activators := pextraline.activators or ULAC_USE;
          {$ENDIF}
      end
      else if (token = 'MONSTERCROSS') then
      begin
        GetToken;
        if token = 'TRUE' then
          {$IFDEF HEXEN}
          pmaplinedef.flags := pmaplinedef.flags or _SHL(SPAC_MCROSS, ML_SPAC_SHIFT);
          {$ELSE}
          pextraline.activators := pextraline.activators or ULAC_MCROSS;
          {$ENDIF}
      end
      else if (token = 'IMPACT') then
      begin
        GetToken;
        if token = 'TRUE' then
          {$IFDEF HEXEN}
          pmaplinedef.flags := pmaplinedef.flags or _SHL(SPAC_IMPACT, ML_SPAC_SHIFT);
          {$ELSE}
          pextraline.activators := pextraline.activators or ULAC_IMPACT;
          {$ENDIF}
      end
      else if (token = 'PLAYERPUSH') then
      begin
        GetToken;
        if token = 'TRUE' then
          {$IFDEF HEXEN}
          pmaplinedef.flags := pmaplinedef.flags or _SHL(SPAC_PUSH, ML_SPAC_SHIFT);
          {$ELSE}
          pextraline.activators := pextraline.activators or ULAC_PUSH;
          {$ENDIF}
      end
      else if (token = 'MISSILECROSS') then
      begin
        GetToken;
        if token = 'TRUE' then
          {$IFDEF HEXEN}
          pmaplinedef.flags := pmaplinedef.flags or _SHL(SPAC_PCROSS, ML_SPAC_SHIFT);
          {$ELSE}
          pextraline.activators := pextraline.activators or ULAC_PCROSS;
          {$ENDIF}
      end
      else if (token = 'MONSTERPUSH') then
      begin
        GetToken;
        if token = 'TRUE' then
          {$IFDEF HEXEN}
          pmaplinedef.flags := pmaplinedef.flags or _SHL(SPAC_PUSH, ML_SPAC_SHIFT);
          {$ELSE}
          pextraline.activators := pextraline.activators or ULAC_MPUSH;
          {$ENDIF}
      end
      {$IFNDEF HEXEN}
      else if (token = 'OLDSTYLE') then
      begin
        GetToken;
        if token = 'TRUE' then
          pextraline.activators := pextraline.activators or ULAC_DEFAULT;
      end
      {$ENDIF}
      else if (token = 'ARG0') then
      begin
        sc.MustGetInteger;
        {$IFDEF HEXEN}
        pmaplinedef.arg1 := sc._Integer;
        {$ELSE}
        pextraline.arg1 := sc._Integer;
        {$ENDIF}
      end
      else if (token = 'ARG1') then
      begin
        sc.MustGetInteger;
        {$IFDEF HEXEN}
        pmaplinedef.arg2 := sc._Integer;
        {$ELSE}
        pextraline.arg2 := sc._Integer;
        {$ENDIF}
      end
      else if (token = 'ARG2') then
      begin
        sc.MustGetInteger;
        {$IFDEF HEXEN}
        pmaplinedef.arg3 := sc._Integer;
        {$ELSE}
        pextraline.arg3 := sc._Integer;
        {$ENDIF}
      end
      else if (token = 'ARG3') then
      begin
        sc.MustGetInteger;
        {$IFDEF HEXEN}
        pmaplinedef.arg4 := sc._Integer;
        {$ELSE}
        pextraline.arg4 := sc._Integer;
        {$ENDIF}
      end
      else if (token = 'ARG4') then
      begin
        sc.MustGetInteger;
        {$IFDEF HEXEN}
        pmaplinedef.arg5 := sc._Integer;
        {$ELSE}
        pextraline.arg5 := sc._Integer;
        {$ENDIF}
      end
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
  end;

  procedure _LoadSidedef;
  begin
    realloc(Pointer(fmapsidedefs), fnummapsidedefs * SizeOf(mapsidedef_t), (fnummapsidedefs + 1) * SizeOf(mapsidedef_t));
    pmapsidedef := @fmapsidedefs[fnummapsidedefs];
    ZeroMemory(pmapsidedef, SizeOf(mapsidedef_t));
    pmapsidedef.toptexture[0] := '-';
    pmapsidedef.bottomtexture[0] := '-';
    pmapsidedef.midtexture[0] := '-';
    realloc(Pointer(fextrasidedefs), fnummapsidedefs * SizeOf(extraside_t), (fnummapsidedefs + 1) * SizeOf(extraside_t));
    pextraside := @fextrasidedefs[fnummapsidedefs];
    ZeroMemory(pextraside, SizeOf(extraside_t));
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
        pextraside.textureoffset := Round(sc._Float * FRACUNIT);
      end
      else if (token = 'OFFSETX_BOTTOM') then
      begin
        sc.MustGetFloat;
        pextraside.bottomtextureoffset := Round(sc._Float * FRACUNIT);
      end
      else if (token = 'OFFSETX_TOP') then
      begin
        sc.MustGetFloat;
        pextraside.toptextureoffset := Round(sc._Float * FRACUNIT);
      end
      else if (token = 'OFFSETX_MID') then
      begin
        sc.MustGetFloat;
        pextraside.midtextureoffset := Round(sc._Float * FRACUNIT);
      end
      else if (token = 'OFFSETY') then
      begin
        sc.MustGetFloat;
        pmapsidedef.rowoffset := Round(sc._Float);
        pextraside.rowoffset := Round(sc._Float * FRACUNIT);
      end
      else if (token = 'OFFSETY_BOTTOM') then
      begin
        sc.MustGetFloat;
        pextraside.bottomrowoffset := Round(sc._Float * FRACUNIT);
      end
      else if (token = 'OFFSETY_TOP') then
      begin
        sc.MustGetFloat;
        pextraside.toprowoffset := Round(sc._Float * FRACUNIT);
      end
      else if (token = 'OFFSETY_MID') then
      begin
        sc.MustGetFloat;
        pextraside.midrowoffset := Round(sc._Float * FRACUNIT);
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
      else if token = 'NOFAKECONTRAST' then
      begin
        GetToken;
        if token = 'TRUE' then
          pextraside.flags := pextraside.flags or SDF_NOFAKECONTRAST;
      end
      else if (token = 'COMMENT') then
      begin
        GetToken; // skip comment
      end;
    end;
  end;

  procedure _LoadVertex;
  begin
    realloc(Pointer(fmapvertexes), fnummapvertexes * SizeOf(mapvertex_t), (fnummapvertexes + 1) * SizeOf(mapvertex_t));
    pmapvertex := @fmapvertexes[fnummapvertexes];
    ZeroMemory(pmapvertex, SizeOf(mapvertex_t));
    realloc(Pointer(fextravertexes), fnummapvertexes * SizeOf(extravertex_t), (fnummapvertexes + 1) * SizeOf(extravertex_t));
    pextravertex := @fextravertexes[fnummapvertexes];
    ZeroMemory(pextravertex, SizeOf(extravertex_t));
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
        pextravertex.x := Round(sc._Float * FRACUNIT);
      end
      else if (token = 'Y') then
      begin
        sc.MustGetFloat;
        pmapvertex.y := Round(sc._Float);
        pextravertex.y := Round(sc._Float * FRACUNIT);
      end
      else if (token = 'ZFLOOR') then
      begin
        sc.MustGetFloat;
        pextravertex.zfloor := Round(sc._Float * FRACUNIT);
        pextravertex.extraflags := pextravertex.extraflags or UDMF_VF_ZFLOOR;
      end
      else if (token = 'ZCEILING') then
      begin
        sc.MustGetFloat;
        pextravertex.zceiling := Round(sc._Float * FRACUNIT);
        pextravertex.extraflags := pextravertex.extraflags or UDMF_VF_ZCEILING;
      end
      else if (token = 'COMMENT') then
      begin
        GetToken; // skip comment
      end;
    end;
  end;

  procedure _LoadSector;
  begin
    realloc(Pointer(fmapsectors), fnummapsectors * SizeOf(mapsector_t), (fnummapsectors + 1) * SizeOf(mapsector_t));
    pmapsector := @fmapsectors[fnummapsectors];
    ZeroMemory(pmapsector, SizeOf(mapsector_t));
    pmapsector.lightlevel := 160;
    realloc(Pointer(fextrasectors), fnummapsectors * SizeOf(extrasector_t), (fnummapsectors + 1) * SizeOf(extrasector_t));
    pextrasector := @fextrasectors[fnummapsectors];
    ZeroMemory(pextrasector, SizeOf(extrasector_t));
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
        if IsIntegerInRange(sc._Integer, 0, 255) then
          Include(pextrasector.moreids, byte(sc._Integer));
      end
      else if (token = 'MOREIDS') then
      begin
        sc.MustGetString;
        sc2 := TScriptEngine.Create(sc._String);
        while sc2.GetInteger do
          if IsIntegerInRange(sc2._Integer, 0, 255) then
            Include(pextrasector.moreids, byte(sc2._Integer));
        sc2.Free;
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
      else if (token = 'RIPPLECEILING') then
      begin
        GetToken;
        if token = 'TRUE' then
          pextrasector.extraflags := pextrasector.extraflags or UDMF_SF_RIPPLECEILING;
      end
      else if (token = 'RIPPLEFLOOR') then
      begin
        GetToken;
        if token = 'TRUE' then
          pextrasector.extraflags := pextrasector.extraflags or UDMF_SF_RIPPLEFLOOR;
      end
      else if (token = 'FOG') then
      begin
        GetToken;
        if token = 'TRUE' then
          pextrasector.extraflags := pextrasector.extraflags or UDMF_SF_FOG;
      end
      else if (token = 'HIDDEN') then
      begin
        GetToken;
        if token = 'TRUE' then
          pextrasector.extraflags := pextrasector.extraflags or UDMF_SF_HIDDEN;
      end
      else if (token = 'NOJUMP') then
      begin
        GetToken;
        if token = 'TRUE' then
          pextrasector.extraflags := pextrasector.extraflags or UDMF_SF_NOJUMP;
      end
      else if (token = 'NOCROUCH') then
      begin
        GetToken;
        if token = 'TRUE' then
          pextrasector.extraflags := pextrasector.extraflags or UDMF_SF_NOCROUCH;
      end
      {$IFNDEF HEXEN}
      else if (token = 'SOUND') then
      begin
        GetToken;
        pextrasector.sound := S_GetSoundNumForName(token);
      end
      {$ENDIF}
      else if token = 'WINDTHRUST' then
      begin
        sc.MustGetFloat;
        pextrasector.windthrust := Round(sc._Float * 2048);
      end
      else if token = 'WINDANGLE' then
      begin
        sc.MustGetFloat;
        pextrasector.windangle := Round(sc._Float * ANG1);
      end
      else if (token = 'COMMENT') then
      begin
        GetToken; // skip comment
      end;
    end;
  end;

begin
  Clear;
  fnamespace := '';
  sc := TScriptEngine.Create(_udmfPreproccessor(atext));
  while GetToken do
  begin
    if token = 'NAMESPACE' then
    begin
      GetToken;
      fnamespace := token;
      flinetranslate := (fnamespace <> strupper(_GAME)) and (fnamespace <> 'ZDOOMTRANSLATED');
      if (token <> strupper(_GAME)) and (token <> 'DELPHI' + strupper(_GAME)) and (token <> 'ZDOOMTRANSLATED') then
      begin
        I_Warning('TUDMFManager.LoadFromString(): Unknown namespace "%s"'#13#10, [sc._String]);
//        sc.Free;
//        exit;
      end;
    end;

    if token = 'THING' then
      _LoadThing
    else if token = 'LINEDEF' then
      _LoadLinedef
    else if token = 'SIDEDEF' then
      _LoadSidedef
    else if token = 'VERTEX' then
      _LoadVertex
    else if token = 'SECTOR' then
      _LoadSector;
  end;
  sc.Free;
end;

//==============================================================================
//
// TUDMFManager.UpdateUDMFGlobalStructs
//
//==============================================================================
procedure TUDMFManager.UpdateUDMFGlobalStructs;
begin
  numudmfthings := fnumthings;
  if numudmfthings > 0 then
  begin
    udmfthings := Z_Malloc(fnumthings * SizeOf(extrathing_t), PU_LEVEL, nil);
    memcpy(udmfthings, fextrathings, fnumthings * SizeOf(extrathing_t));
  end;

  numudmfsectors := fnummapsectors;
  if numudmfsectors > 0 then
  begin
    udmfsectors := Z_Malloc(fnummapsectors * SizeOf(extrasector_t), PU_LEVEL, nil);
    memcpy(udmfsectors, fextrasectors, fnummapsectors * SizeOf(extrasector_t));
  end;

  numudmfvertexes := fnummapvertexes;
  if numudmfvertexes > 0 then
  begin
    udmfvertexes := Z_Malloc(fnummapvertexes * SizeOf(extravertex_t), PU_LEVEL, nil);
    memcpy(udmfvertexes, fextravertexes, fnummapvertexes * SizeOf(extravertex_t));
  end;

  numudmflinedefs := fnummaplinedefs;
  if numudmflinedefs > 0 then
  begin
    udmflinedefs := Z_Malloc(fnummaplinedefs * SizeOf(extraline_t), PU_LEVEL, nil);
    memcpy(udmflinedefs, fextralinedefs, fnummaplinedefs * SizeOf(extraline_t));
  end;

  numudmfsidedefs := fnummapsidedefs;
  if numudmfsidedefs > 0 then
  begin
    udmfsidedefs := Z_Malloc(fnummapsidedefs * SizeOf(extraside_t), PU_LEVEL, nil);
    memcpy(udmfsidedefs, fextrasidedefs, fnummapsidedefs * SizeOf(extraside_t));
  end;
end;

//==============================================================================
//
// TUDMFManager.SaveUDMFToVanilla
//
//==============================================================================
procedure TUDMFManager.SaveUDMFToVanilla(const amapname: string; const afilename: string;const bl, sl: integer);
var
  header: wadinfo_t;
  infotable: array[0..ML_NUMUDMFLUMPS - 1] of filelump_t;
  f: TFile;
  ldata: PByteArray;
  i, llen: integer;
begin
  header.identification :=
    integer(Ord('P') or (Ord('W') shl 8) or (Ord('A') shl 16) or (Ord('D') shl 24));
  header.numlumps := ML_NUMUDMFLUMPS;

  f := TFile.Create(afilename, fCreate);
  f.Write(header, SizeOf(header));

  ZeroMemory(@infotable, SizeOf(infotable));

  infotable[0].filepos := f.Position;
  infotable[0].size := 0;
  infotable[0].name := stringtochar8(strupper(amapname));

  infotable[1].filepos := f.Position;
  infotable[1].size := fnumthings * SizeOf(mapthing_t);
  infotable[1].name := stringtochar8('THINGS');
  if fnumthings > 0 then
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

  // BEHAVIOR lump
  infotable[11].filepos := f.Position;
  if bl > 0 then
  begin
    ldata := W_CacheLumpNum(bl, PU_STATIC);
    llen := W_LumpLength(bl);
    infotable[11].size := llen;
    f.Write(ldata^, llen);
    Z_Free(ldata);
  end
  else
    infotable[11].size := 0;
  infotable[11].name := stringtochar8('BEHAVIOR');

  // Scripts lump
  infotable[12].filepos := f.Position;
  if sl > 0 then
  begin
    ldata := W_CacheLumpNum(sl, PU_STATIC);
    llen := W_LumpLength(sl);
    infotable[12].size := llen;
    f.Write(ldata^, llen);
    Z_Free(ldata);
  end
  else
    infotable[12].size := 0;
  infotable[12].name := stringtochar8('SCRIPTS');

  infotable[ML_THINGS2].filepos := f.Position;
  infotable[ML_THINGS2].size := fnumthings * SizeOf(extrathing_t);
  infotable[ML_THINGS2].name := stringtochar8(MLN_THINGS2);
  if fnumthings > 0 then
    f.Write(fextrathings^, fnumthings * SizeOf(extrathing_t));

  infotable[ML_SECTORS2].filepos := f.Position;
  infotable[ML_SECTORS2].size := fnummapsectors * SizeOf(extrasector_t);
  infotable[ML_SECTORS2].name := stringtochar8(MLN_SECTORS2);
  f.Write(fextrasectors^, fnummapsectors * SizeOf(extrasector_t));

  infotable[ML_VERTEX2].filepos := f.Position;
  infotable[ML_VERTEX2].size := fnummapvertexes * SizeOf(extravertex_t);
  infotable[ML_VERTEX2].name := stringtochar8(MLN_VERTEX2);
  f.Write(fextravertexes^, fnummapvertexes * SizeOf(extravertex_t));

  infotable[ML_LINEDEF2].filepos := f.Position;
  infotable[ML_LINEDEF2].size := fnummaplinedefs * SizeOf(extraline_t);
  infotable[ML_LINEDEF2].name := stringtochar8(MLN_LINEDEF2);
  f.Write(fextralinedefs^, fnummaplinedefs * SizeOf(extraline_t));

  infotable[ML_SIDEDEF2].filepos := f.Position;
  infotable[ML_SIDEDEF2].size := fnummapsidedefs * SizeOf(extraside_t);
  infotable[ML_SIDEDEF2].name := stringtochar8(MLN_SIDEDEF2);
  f.Write(fextrasidedefs^, fnummapsidedefs * SizeOf(extraside_t));

  infotable[ML_NSPACE].filepos := f.Position;
  infotable[ML_NSPACE].size := Length(fnamespace);
  infotable[ML_NSPACE].name := stringtochar8(MLN_NSPACE);
  for i := 1 to Length(fnamespace) do
    f.Write(fnamespace[i], SizeOf(Char));

  header.infotableofs := f.Position;
  f.Write(infotable, SizeOf(infotable));
  f.Seek(0, sFromBeginning);
  f.Write(header, SizeOf(header));
  f.Free;
end;

//==============================================================================
//
// TUDMFManager.Clear
//
//==============================================================================
procedure TUDMFManager.Clear;
begin
  memfree(Pointer(fthings), fnumthings * SizeOf(mapthing_t));
  memfree(Pointer(fextrathings), fnumthings * SizeOf(extrathing_t));
  memfree(Pointer(fmaplinedefs), fnummaplinedefs * SizeOf(maplinedef_t));
  memfree(Pointer(fextralinedefs), fnummaplinedefs * SizeOf(extraline_t));
  memfree(Pointer(fmapsidedefs), fnummapsidedefs * SizeOf(mapsidedef_t));
  memfree(Pointer(fextrasidedefs), fnummapsidedefs * SizeOf(extraside_t));
  memfree(Pointer(fmapvertexes), fnummapvertexes * SizeOf(mapvertex_t));
  memfree(Pointer(fextravertexes), fnummapvertexes * SizeOf(extravertex_t));
  memfree(Pointer(fmapsectors), fnummapsectors * SizeOf(mapsector_t));
  memfree(Pointer(fextrasectors), fnummapsectors * SizeOf(extrasector_t));
end;

//==============================================================================
//
// TUDMFManager.Destroy
//
//==============================================================================
destructor TUDMFManager.Destroy;
begin
  Clear;
  inherited;
end;

//==============================================================================
// UDMF_Check
//
//------------------------------------------------------------------------------
//
//==============================================================================
function UDMF_Check(const mapname: string): boolean;
const
  MAXLUMPCHECK = 10;
var
  udmf: TUDMFManager;
  i: integer;
  lumpnum: integer;
  crc32: string;
  wadfilemap: string;
  behav_lump: integer;
  script_lump: integer;
begin
  hasudmfdata := false;
  udmfnamespace := '';
  udmflinetranslate := false;
  udmfthings := nil;
  numudmfthings := 0;
  udmfsectors := nil;
  numudmfsectors := 0;
  udmfvertexes := nil;
  numudmfvertexes := 0;
  udmflinedefs := nil;
  numudmflinedefs := 0;

  lumpnum := W_GetNumForName(mapname);
  if (lumpnum < 0) or (lumpnum >= W_NumLumps - 1) then
  begin
    result := false;
    exit;
  end;

  if strupper(stringtochar8(lumpinfo[lumpnum + 1].name)) <> 'TEXTMAP' then
  begin
    if lumpnum + ML_THINGS2 < W_NumLumps then
      if strupper(stringtochar8(lumpinfo[lumpnum + ML_THINGS2].name)) = MLN_THINGS2 then
      begin
        udmfthings := W_CacheLumpNum(lumpnum + ML_THINGS2, PU_LEVEL);
        numudmfthings := W_LumpLength(lumpnum + ML_THINGS2) div SizeOf(extrathing_t);
      end;
    if lumpnum + ML_SECTORS2 < W_NumLumps then
      if strupper(stringtochar8(lumpinfo[lumpnum + ML_SECTORS2].name)) = MLN_SECTORS2 then
      begin
        udmfsectors := W_CacheLumpNum(lumpnum + ML_SECTORS2, PU_LEVEL);
        numudmfsectors := W_LumpLength(lumpnum + ML_SECTORS2) div SizeOf(extrasector_t);
      end;
    if lumpnum + ML_VERTEX2 < W_NumLumps then
      if strupper(stringtochar8(lumpinfo[lumpnum + ML_VERTEX2].name)) = MLN_VERTEX2 then
      begin
        udmfvertexes := W_CacheLumpNum(lumpnum + ML_VERTEX2, PU_LEVEL);
        numudmfvertexes := W_LumpLength(lumpnum + ML_VERTEX2) div SizeOf(extravertex_t);
      end;
    if lumpnum + ML_LINEDEF2 < W_NumLumps then
      if strupper(stringtochar8(lumpinfo[lumpnum + ML_LINEDEF2].name)) = MLN_LINEDEF2 then
      begin
        udmflinedefs := W_CacheLumpNum(lumpnum + ML_LINEDEF2, PU_LEVEL);
        numudmflinedefs := W_LumpLength(lumpnum + ML_LINEDEF2) div SizeOf(extraline_t);
      end;
    if lumpnum + ML_SIDEDEF2 < W_NumLumps then
      if strupper(stringtochar8(lumpinfo[lumpnum + ML_SIDEDEF2].name)) = MLN_SIDEDEF2 then
      begin
        udmfsidedefs := W_CacheLumpNum(lumpnum + ML_SIDEDEF2, PU_LEVEL);
        numudmfsidedefs := W_LumpLength(lumpnum + ML_SIDEDEF2) div SizeOf(extraside_t);
      end;
    if lumpnum + ML_NSPACE < W_NumLumps then
      if strupper(stringtochar8(lumpinfo[lumpnum + ML_NSPACE].name)) = MLN_NSPACE then
      begin
        udmfnamespace := W_TextLumpNum(lumpnum + ML_NSPACE);
        udmflinetranslate := (udmfnamespace <> strupper(_GAME)) and (udmfnamespace <> 'ZDOOMTRANSLATED');
      end;
    hasudmfdata := (numudmfthings <> 0) and (numudmfsectors <> 0) and (numudmfvertexes <> 0) and (numudmflinedefs <> 0) and (numudmfsidedefs <> 0);
    result := false;
    exit;
  end;

  behav_lump := -1;
  for i := lumpnum + 1 to lumpnum + MAXLUMPCHECK do
  begin
    if i >= W_NumLumps then
      break;
    if strupper(stringtochar8(lumpinfo[i].name)) = 'ENDMAP' then
      break;
    if strupper(stringtochar8(lumpinfo[i].name)) = 'BEHAVIOR' then
    begin
      behav_lump := i;
      break;
    end;
  end;

  script_lump := -1;
  for i := lumpnum + 1 to lumpnum + MAXLUMPCHECK do
  begin
    if i >= W_NumLumps then
      break;
    if strupper(stringtochar8(lumpinfo[i].name)) = 'ENDMAP' then
      break;
    if acc_isscriptlump(i) then
    begin
      script_lump := i;
      break;
    end;
  end;

  inc(lumpnum);
  crc32 := GetLumpCRC32(lumpnum);

  wadfilemap := M_SaveFileName('DATA\');
  MkDir(wadfilemap);
  wadfilemap := wadfilemap + 'WADS\';
  MkDir(wadfilemap);
  wadfilemap := wadfilemap + mapname + '_' + crc32 + '.wad';

  udmf := TUDMFManager.Create;
  udmf.LoadFromString(W_TextLumpNum(lumpnum));
  udmf.UpdateUDMFGlobalStructs;
  udmf.SaveUDMFToVanilla(mapname, wadfilemap, behav_lump, script_lump);
  udmfnamespace := udmf.namespace;
  udmflinetranslate := (udmfnamespace <> strupper(_GAME)) and (udmfnamespace <> 'ZDOOMTRANSLATED');
  udmf.Free;

  W_RuntimeLoad(wadfilemap, F_ORIGIN_WAD);

  result := true;
  hasudmfdata := true;
end;

//==============================================================================
//
// UDMF_DoMakeAbsoluteHeights
//
//==============================================================================
procedure UDMF_DoMakeAbsoluteHeights(const secid: integer);
var
  sec: Psector_t;
  vids: TDNumberList;
  v, i: integer;
  a, b, c, d: float;
  isslope: boolean;

  function _X(id: integer): float;
  begin
    result := vertexes[vids[id]].x / FRACUNIT;
  end;

  function _Y(id: integer): float;
  begin
    result := vertexes[vids[id]].y / FRACUNIT;
  end;

  function _floor(id: integer): float;
  var
    vid: integer;
  begin
    vid := vids[id];
    if udmfvertexes[vid].extraflags and UDMF_VF_ZFLOOR <> 0 then
      result := udmfvertexes[vid].zfloor / FRACUNIT
    else
      result := sec.floorheight / FRACUNIT;
  end;

  function _ceiling(id: integer): float;
  var
    vid: integer;
  begin
    vid := vids[id];
    if udmfvertexes[vid].extraflags and UDMF_VF_ZCEILING <> 0 then
      result := udmfvertexes[vid].zceiling / FRACUNIT
    else
      result := sec.ceilingheight / FRACUNIT;
  end;

begin
  sec := @sectors[secid];
  if sec.linecount <> 3 then
    exit;

  vids := TDNumberList.Create;

  for i := 0 to 2 do
  begin
    v := pDiff(sec.lines[i].v1, vertexes, SizeOf(vertex_t));
    if vids.IndexOf(v) < 0 then
      vids.Add(v);
    v := pDiff(sec.lines[i].v2, vertexes, SizeOf(vertex_t));
    if vids.IndexOf(v) < 0 then
      vids.Add(v);
  end;

  if vids.Count <> 3 then // Not closed sector ?
  begin
    vids.Free;
    exit;
  end;

  for i := 0 to vids.Count - 1 do
    if vids[i] >= numudmfvertexes then  // Extra vertexes from ???
    begin
      vids.Free;
      exit;
    end;

  // Check floor slope
  isslope := false;
  for i := 0 to 2 do
    if udmfvertexes[vids[i]].extraflags and UDMF_VF_ZFLOOR <> 0 then
    begin
      isslope := true;
      break;
    end;

  if isslope then
  begin
    calc_slope_plane(
      _X(0), _Y(0), _floor(0),
      _X(1), _Y(1), _floor(1),
      _X(2), _Y(2), _floor(2),
      a, b, c, d
    );
    sec.fa := a;
    sec.fb := b;
    sec.fic := 1 / c;
    sec.fd := d;
    sec.renderflags := sec.renderflags or SRF_SLOPEFLOOR;
    P_SlopesAlignPlane(sec, nil, SRF_SLOPEFLOOR, false);
    sec.slopeline := sec.lines[0];
    sec.slopeline.renderflags := sec.slopeline.renderflags or LRF_SLOPED;
  end;

  // Check ceiling slope
  isslope := false;
  for i := 0 to 2 do
    if udmfvertexes[vids[i]].extraflags and UDMF_VF_ZCEILING <> 0 then
    begin
      isslope := true;
      break;
    end;

  if isslope then
  begin
    calc_slope_plane(
      _X(0), _Y(0), _ceiling(0),
      _X(1), _Y(1), _ceiling(1),
      _X(2), _Y(2), _ceiling(2),
      a, b, c, d
    );
    sec.ca := a;
    sec.cb := b;
    sec.cic := 1 / c;
    sec.cd := d;
    sec.renderflags := sec.renderflags or SRF_SLOPECEILING;
    P_SlopesAlignPlane(sec, nil, SRF_SLOPECEILING, false);
    sec.slopeline := sec.lines[0];
    sec.slopeline.renderflags := sec.slopeline.renderflags or LRF_SLOPED;
  end;

  vids.Free;
end;

//==============================================================================
//
// UDMF_DoMakeSector
//
//==============================================================================
procedure UDMF_DoMakeSector(const secid: integer);
var
  sec: Psector_t;
  usec: Pextrasector_t;
begin
  sec := @sectors[secid];
  usec := @udmfsectors[secid];

  {$IFDEF DOOM_OR_STRIFE}
  sec.floor_xoffs := usec.xpanningfloor;
  sec.floor_yoffs := usec.ypanningfloor;
  sec.ceiling_xoffs := usec.xpanningceiling;
  sec.ceiling_yoffs := usec.ypanningceiling;
  {$ENDIF}
  sec.gravity := usec.gravity;
  sec.floorangle := usec.rotationfloor;
  sec.flooranglex := usec.rotationfloorx;
  sec.floorangley := usec.rotationfloory;
  sec.ceilingangle := usec.rotationceiling;
  sec.ceilinganglex := usec.rotationceilingx;
  sec.ceilingangley := usec.rotationceilingy;
  if (usec.extraflags and UDMF_SF_CEILINGPLANE = UDMF_SF_CEILINGPLANE) and (usec.ceilingplane_c <> 0.0) then
  begin
    sec.ca := usec.ceilingplane_a;
    sec.cb := usec.ceilingplane_b;
    sec.cic := 1 / usec.ceilingplane_c;
    sec.cd := usec.ceilingplane_d;
    sec.renderflags := sectors[secid].renderflags or SRF_SLOPECEILING;
    P_SlopesAlignPlane(sec, nil, SRF_SLOPECEILING, false);
    sec.slopeline := sec.lines[0];
    sec.slopeline.renderflags := sec.slopeline.renderflags or LRF_SLOPED;
  end;
  if (usec.extraflags and UDMF_SF_FLOORPLANE = UDMF_SF_FLOORPLANE) and (usec.floorplane_c <> 0.0) then
  begin
    sec.fa := usec.floorplane_a;
    sec.fb := usec.floorplane_b;
    sec.fic := 1 / usec.floorplane_c;
    sec.fd := usec.floorplane_d;
    sec.renderflags := sectors[secid].renderflags or SRF_SLOPEFLOOR;
    P_SlopesAlignPlane(sec, nil, SRF_SLOPEFLOOR, false);
    sec.slopeline := sec.lines[0];
    sec.slopeline.renderflags := sec.slopeline.renderflags or LRF_SLOPED;
  end;
  if usec.extraflags and UDMF_SF_RIPPLECEILING <> 0 then
    sec.renderflags := sec.renderflags or SRF_RIPPLE_CEILING;
  if usec.extraflags and UDMF_SF_RIPPLEFLOOR <> 0 then
    sec.renderflags := sec.renderflags or SRF_RIPPLE_FLOOR;
  if usec.extraflags and UDMF_SF_FOG <> 0 then
    sec.renderflags := sec.renderflags or SRF_FOG;
  if usec.extraflags and UDMF_SF_HIDDEN <> 0 then
    sec.renderflags := sec.renderflags or SRF_HIDDEN;
  if usec.extraflags and UDMF_SF_NOJUMP <> 0 then
    sec.flags := sec.flags or SF_NOJUMP;
  if usec.extraflags and UDMF_SF_NOCROUCH <> 0 then
    sec.flags := sec.flags or SF_NOCROUCH;
  sec.moreids := usec.moreids;
  {$IFNDEF HEXEN}
  sec.seqType := usec.sound;
  {$ENDIF}
  sec.windthrust := usec.windthrust;
  sec.windangle := usec.windangle;
end;

//==============================================================================
//
// UDMF_MakeSectors
//
//==============================================================================
function UDMF_MakeSectors: boolean;
var
  i: integer;
begin
  if not hasudmfdata then
  begin
    result := false;
    exit;
  end;

  if numudmfsectors <> numsectors then
  begin
    result := false;
    exit;
  end;

  for i := 0 to numudmfsectors - 1 do
  begin
    UDMF_DoMakeSector(i);
    UDMF_DoMakeAbsoluteHeights(i);
  end;

  result := true;
end;

//==============================================================================
//
// UDMF_DoMakeLine
//
//==============================================================================
procedure UDMF_DoMakeLine(const lineid: integer);
var
  line: Pline_t;
  uline: Pextraline_t;
begin
  line := @lines[lineid];
  uline := @udmflinedefs[lineid];

  {$IFNDEF HEXEN}
  line.arg1 := uline.arg1;
  line.arg2 := uline.arg2;
  line.arg3 := uline.arg3;
  line.arg4 := uline.arg4;
  line.arg5 := uline.arg5;
  line.activators := uline.activators;
  if udmflinetranslate then
  begin
    if IsIntegerInRange(line.special and 1023, 1, UDMF_NORMAL_ADD) then
      line.special := (line.special and 1023 + UDMF_SPECIAL_START) or (line.special and not 1023)
    else if IsIntegerInRange(line.special and 1023, UDMF_NORMAL_ADD, UDMF_SPECIAL_START + UDMF_NORMAL_ADD) then
      line.special := (line.special and 1023 - UDMF_NORMAL_ADD) or (line.special and not 1023);
  end;
  {$ENDIF}
  line.moreids := uline.moreids;
end;

//==============================================================================
//
// UDMF_MakeLines
//
//==============================================================================
function UDMF_MakeLines: boolean;
var
  i: integer;
begin
  if not hasudmfdata then
  begin
    result := false;
    exit;
  end;

  if numudmflinedefs <> numlines then
  begin
    result := false;
    exit;
  end;

  for i := 0 to numudmflinedefs - 1 do
    UDMF_DoMakeLine(i);

  result := true;
end;

//==============================================================================
//
// UDMF_DoMakeSide
//
//==============================================================================
procedure UDMF_DoMakeSide(const sideid: integer);
var
  side: Pside_t;
  uside: Pextraside_t;
begin
  side := @sides[sideid];
  uside := @udmfsidedefs[sideid];

  side.textureoffset := uside.textureoffset;
  side.toptextureoffset := uside.toptextureoffset;
  side.midtextureoffset := uside.midtextureoffset;
  side.bottomtextureoffset := uside.bottomtextureoffset;
  side.rowoffset := uside.rowoffset;
  side.toprowoffset := uside.toprowoffset;
  side.midrowoffset := uside.midrowoffset;
  side.bottomrowoffset := uside.bottomrowoffset;
  side.flags := uside.flags;
end;

//==============================================================================
//
// UDMF_MakeSides
//
//==============================================================================
function UDMF_MakeSides: boolean;
var
  i: integer;
begin
  if not hasudmfdata then
  begin
    result := false;
    exit;
  end;

  if numudmfsidedefs <> numsides then
  begin
    result := false;
    exit;
  end;

  for i := 0 to numudmfsidedefs - 1 do
    UDMF_DoMakeSide(i);

  result := true;
end;

//==============================================================================
//
// UDMF_DoMakeSeg
//
//==============================================================================
procedure UDMF_DoMakeSeg(const segid: integer);
var
  seg: Pseg_t;
begin
  seg := @segs[segid];

  if seg.miniseg then
    Exit;

  if seg.sidedef.flags and SDF_NOFAKECONTRAST <> 0 then
    seg.fakecontrastlight := 0
  else if seg.v1.y = seg.v2.y then
    seg.fakecontrastlight := -1
  else if seg.v1.x = seg.v2.x then
    seg.fakecontrastlight := 1
  else
    seg.fakecontrastlight := 0;
end;

//==============================================================================
//
// UDMF_MakeSegs
//
//==============================================================================
function UDMF_MakeSegs: boolean;
var
  i: integer;
begin
  if not hasudmfdata then
  begin
    result := false;
    exit;
  end;

  for i := 0 to numsegs - 1 do
    UDMF_DoMakeSeg(i);

  result := true;
end;

end.
