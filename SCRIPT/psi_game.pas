//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2016 by Jim Valavanis
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
//  Pascal Script RTL - Game Definitions
//  Actors - Vertexes - Lines - Sides - Sectors - Players
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit psi_game;

interface

uses
  d_delphi,
  p_mobj_h,
  p_mobjlist,
  psi_system,
  uPSCompiler,
  uPSRuntime;

// ---------------------------- ACTORS -----------------------------------------

function PS_KeyFromMobj(const mo: Pmobj_t): LongWord;

var
  ps_currentactor: Pmobj_t = nil;

function PS_Actor: LongWord;

function PS_TActor(const key: LongWord): LongWord;

function PS_GetActorTarget(const key: LongWord): LongWord;
procedure PS_SetActorTarget(const key: LongWord; const targ: LongWord);

function PS_GetActorX(const key: LongWord): Integer;
procedure PS_SetActorX(const key: LongWord; const x: Integer);

function PS_GetActorY(const key: LongWord): Integer;
procedure PS_SetActorY(const key: LongWord; const y: Integer);

function PS_GetActorZ(const key: LongWord): Integer;
procedure PS_SetActorZ(const key: LongWord; const z: Integer);

function PS_GetActorMOMX(const key: LongWord): Integer;
procedure PS_SetActorMOMX(const key: LongWord; const x: Integer);

function PS_GetActorMOMY(const key: LongWord): Integer;
procedure PS_SetActorMOMY(const key: LongWord; const y: Integer);

function PS_GetActorMOMZ(const key: LongWord): Integer;
procedure PS_SetActorMOMZ(const key: LongWord; const z: Integer);

function PS_GetActorFloorZ(const key: LongWord): Integer;
procedure PS_SetActorFloorZ(const key: LongWord; const z: Integer);

function PS_GetActorCeilingZ(const key: LongWord): Integer;
procedure PS_SetActorCeilingZ(const key: LongWord; const z: Integer);

function PS_GetActorSpeed(const key: LongWord): Integer;
procedure PS_SetActorSpeed(const key: LongWord; const speed: Integer);

function PS_GetActorAngle(const key: LongWord): LongWord;
procedure PS_SetActorAngle(const key: LongWord; const angle: LongWord);

function PS_GetActorSector(const key: LongWord): Integer;

function PS_GetActorHealth(const key: LongWord): Integer;
procedure PS_SetActorHealth(const key: LongWord; const h: Integer);

function PS_GetActorSpawnHealth(const key: LongWord): Integer;

function PS_GetActorMass(const key: LongWord): Integer;

function PS_GetActorHeight(const key: LongWord): Integer;
procedure PS_SetActorHeight(const key: LongWord; const h: Integer);

function PS_GetActorCustomParam(const key: LongWord; const parm: string): Integer;
procedure PS_SetActorCustomParam(const key: LongWord; const parm: string; const value: Integer);

function PS_GetActorCustomDropItem(const key: LongWord): Integer;
procedure PS_SetActorCustomDropItem(const key: LongWord; const value: Integer);
procedure PS_SetActorDefaultDropItem(const key: LongWord);

function PS_CheckActorFlag(const key: LongWord; const flag: LongWord): boolean;
procedure PS_SetActorFlag(const key: LongWord; const flag: LongWord);
procedure PS_UnSetActorFlag(const key: LongWord; const flag: LongWord);

function PS_GetActorName(const key: LongWord): string;

{$IFDEF STRIFE}
function PS_GetActorName2(const key: LongWord): string;
{$ENDIF}

function PS_GetActorDistanceXY(const key1, key2: LongWord): Integer;

function PS_GetActorDistanceXYZ(const key1, key2: LongWord): Integer;

function PS_GetActorPlayer(const key: LongWord): Integer;

function PS_GetActorMobjType(const key: LongWord): Integer;

function PS_GetActorEditorNumber(const key: LongWord): Integer;

function PS_ActorTypeFromName(const name: string): Integer;

function PS_GetActorSeeSound(const key: LongWord): string;

function PS_GetActorAttackSound(const key: LongWord): string;

function PS_GetActorPainSound(const key: LongWord): string;

function PS_GetActorDeathSound(const key: LongWord): string;

function PS_GetActorActiveSound(const key: LongWord): string;

function PS_GetActorCustomSound1(const key: LongWord): string;

function PS_GetActorCustomSound2(const key: LongWord): string;

function PS_GetActorCustomSound3(const key: LongWord): string;

function PS_GetActorMeleeSound(const key: LongWord): string;

function PS_IsValidActor(const key: LongWord): boolean;

procedure PS_ActorPlaySound(const key: LongWord; const snd: string);

procedure PS_PlaySound(const snd: string);

function PS_SpawnActorType(x, y, z: Integer; const typ: Integer): LongWord;

function PS_SpawnActorEditorNumber(x, y, z: Integer; const ednum: Integer): LongWord;

function PS_SpawnActorName(x, y, z: Integer; const name: string): LongWord;

procedure PS_RemoveActor(const key: LongWord);

function PS_ActorTypeFromEditorNumber(const ednum: Integer): Integer;

type
  TActorArray = array of LongWord;

  TActorKeyList = class(TObject)
  private
    fList: PLongWordArray;
    fNumItems: Integer;
    fRealSize: Integer;
    fAllowDuplicates: boolean;
    function FormatName(const name: string): string;
  protected
    function GetActor(Index: Integer): LongWord; virtual;
    procedure PutActor(Index: Integer; const value: LongWord); virtual;
    procedure SetAllowDuplicates(const value: boolean); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(const value: LongWord);
    function Delete(const item: LongWord): boolean;
    function Exists(const value: LongWord): boolean;
    procedure AddAllActors;
    procedure AddAllMonstersAlive;
    procedure AddAllMonstersDead;
    procedure AddAllMonsters;
    procedure AddAllPlayers;
    procedure AddAllName(const name: string);
    procedure AddAllEdNum(const num: Integer);
    procedure AddAllSector(const sec: Integer);
    procedure AddAllSectorTag(const tag: Integer);
    procedure AddAllType(const typ: Integer);
    procedure AddList(const lst: TActorKeyList);
    procedure DeleteList(const lst: TActorKeyList);
    procedure RemoveAllDead;
    procedure RemoveAllAlive;
    procedure Clear;
    function GetActorArray: TActorArray;
    property Count: Integer read fNumItems;
    property Actors[Index: Integer]: LongWord read GetActor write PutActor; default;
    property AllowDuplicates: Boolean read fAllowDuplicates write SetAllowDuplicates;
  end;

// ------------------------------- MAP -----------------------------------------

// -------------------------- VERTEXES -----------------------------------------

function PS_TVertex(const id: Integer): Integer;

function PS_GetVertexX(const v: Integer): Integer;

function PS_GetVertexY(const v: Integer): Integer;

function PS_IsValidVertex(const v: Integer): boolean;

function PS_GetVertexCount: Integer;

// ----------------------------- SIDES -----------------------------------------

function PS_TSide(const id: Integer): Integer;

function PS_GetSideTextureOffset(const sd: Integer): Integer;
procedure PS_SetSideTextureOffset(const sd: Integer; const offs: Integer);

function PS_GetSideRowOffset(const sd: Integer): Integer;
procedure PS_SetSideRowOffset(const sd: Integer; const offs: Integer);

function PS_GetSideTopTexture(const sd: Integer): string;
procedure PS_SetSideTopTexture(const sd: Integer; const tex: string);

function PS_GetSideBottomTexture(const sd: Integer): string;
procedure PS_SetSideBottomTexture(const sd: Integer; const tex: string);

function PS_GetSideMiddleTexture(const sd: Integer): string;
procedure PS_SetSideMiddleTexture(const sd: Integer; const tex: string);

function PS_GetSideSector(const sd: Integer): Integer;

function PS_IsValidSide(const sd: Integer): boolean;

function PS_GetSideCount: Integer;

// ----------------------------- LINES -----------------------------------------

function PS_TLine(const id: Integer): Integer;

function PS_GetLineVertex1(const ld: Integer): Integer;

function PS_GetLineVertex2(const ld: Integer): Integer;

function PS_GetLineDX(const ld: Integer): Integer;

function PS_GetLineDY(const ld: Integer): Integer;

function PS_GetLineSpecial(const ld: Integer): Integer;
procedure PS_SetLineSpecial(const ld: Integer; const spec: Integer);

{$IFDEF HEXEN}
function PS_GetLineArg1(const ld: Integer): byte;
procedure PS_SetLineArg1(const ld: Integer; const arg: byte);

function PS_GetLineArg2(const ld: Integer): byte;
procedure PS_SetLineArg2(const ld: Integer; const arg: byte);

function PS_GetLineArg3(const ld: Integer): byte;
procedure PS_SetLineArg3(const ld: Integer; const arg: byte);

function PS_GetLineArg4(const ld: Integer): byte;
procedure PS_SetLineArg4(const ld: Integer; const arg: byte);

function PS_GetLineArg5(const ld: Integer): byte;
procedure PS_SetLineArg5(const ld: Integer; const arg: byte);
{$ELSE}
function PS_GetLineTag(const ld: Integer): Integer;
procedure PS_SetLineTag(const ld: Integer; const tg: Integer);

function PS_GetLineTransparent(const ld: Integer): Boolean;
procedure PS_SetLineTransparent(const ld: Integer; const x: Boolean);
{$ENDIF}

function PS_GetLineIsBlocking(const ld: Integer): Boolean;
procedure PS_SetLineIsBlocking(const ld: Integer; const x: Boolean);

function PS_GetLineIsBlockingMonsters(const ld: Integer): Boolean;
procedure PS_SetLineIsBlockingMonsters(const ld: Integer; const x: Boolean);

function PS_GetLineTriggerScripts(const ld: Integer): Boolean;
procedure PS_SetLineTriggerScripts(const ld: Integer; const x: Boolean);

function PS_GetLineFrontSide(const ld: Integer): Integer;

function PS_GetLineBackSide(const ld: Integer): Integer;

function PS_GetLineFrontSector(const ld: Integer): Integer;

function PS_GetLineBackSector(const ld: Integer): Integer;

function PS_IsValidLine(const ld: Integer): boolean;

function PS_GetLineCount: Integer;

{$IFNDEF HEXEN}
function PS_FindLinesFromTag(const tag: integer): TDynamicIntegerArray;
{$ENDIF}

// --------------------------- SECTORS -----------------------------------------

function PS_TSector(const id: Integer): Integer;

function PS_GetSectorFloorHeight(const sec: Integer): Integer;
procedure PS_SetSectorFloorHeight(const sec: Integer; const x: Integer);

function PS_GetSectorCeilingHeight(const sec: Integer): Integer;
procedure PS_SetSectorCeilingHeight(const sec: Integer; const x: Integer);

function PS_GetSectorFloorPicture(const sec: Integer): string;
procedure PS_SetSectorFloorPicture(const sec: Integer; const pic: string);

function PS_GetSectorCeilingPicture(const sec: Integer): string;
procedure PS_SetSectorCeilingPicture(const sec: Integer; const pic: string);

function PS_GetSectorLightLevel(const sec: Integer): Integer;
procedure PS_SetSectorLightLevel(const sec: Integer; const x: Integer);

function PS_GetSectorSpecial(const sec: Integer): Integer;

function PS_GetSectorTag(const sec: Integer): Integer;
procedure PS_SetSectorTag(const sec: Integer; const x: Integer);

function PS_GetSectorActors(const sec: Integer): TActorArray;

function PS_GetSectorNumActors(const sec: Integer): Integer;

function PS_GetSectorLines(const sec: Integer): TDynamicIntegerArray;

function PS_GetSectorNumLines(const sec: Integer): Integer;

procedure PS_SectorMoveZ(const sec: Integer; const dz: Integer);

{$IFDEF DOOM_OR_STRIFE}
function PS_GetSectorFloorXOffset(const sec: Integer): Integer;
procedure PS_SetSectorFloorXOffset(const sec: Integer; const offs: Integer);

function PS_GetSectorFloorYOffset(const sec: Integer): Integer;
procedure PS_SetSectorFloorYOffset(const sec: Integer; const offs: Integer);

function PS_GetSectorCeilingXOffset(const sec: Integer): Integer;
procedure PS_SetSectorCeilingXOffset(const sec: Integer; const offs: Integer);

function PS_GetSectorCeilingYOffset(const sec: Integer): Integer;
procedure PS_SetSectorCeilingYOffset(const sec: Integer; const offs: Integer);
{$ENDIF}

function PS_GetSectorRippleFloor(const sec: Integer): Boolean;
procedure PS_SetSectorRippleFloor(const sec: Integer; const rpl: Boolean);

function PS_GetSectorRippleCeiling(const sec: Integer): Boolean;
procedure PS_SetSectorRippleCeiling(const sec: Integer; const rpl: Boolean);

function PS_GetSectorMidSector(const sec: Integer): Integer;

function PS_GetSectorSlopeSector(const sec: Integer): Integer;

function PS_SkyPicture: string;

function PS_IsValidSector(const sec: Integer): boolean;

function PS_GetSectorCount: Integer;

function PS_FindSectorsFromTag(const tag: integer): TDynamicIntegerArray;

procedure PS_SectorPlaySound(const secid: Integer; const snd: string);

// --------------------------- PLAYERS -----------------------------------------

function PS_PlayerInGame(const plnum: Integer): boolean;

{$IFDEF DOOM_OR_STRIFE}
procedure PS_PlayerFaceMobj(const plnum: Integer; const actor: LongWord; const ticks: Integer);
{$ENDIF}

{$IFDEF DOOM_OR_STRIFE}
procedure PS_SetPlayerHasCard(const plnum: Integer; const card: Integer; const value: boolean);

function PS_GetPlayerHasCard(const plnum: Integer; const card: Integer): boolean;
{$ENDIF}

{$IFDEF HERETIC_OR_HEXEN}
procedure PS_SetPlayerHasKey(const plnum: Integer; const key: Integer; const value: boolean);

function PS_GetPlayerHasKey(const plnum: Integer; const key: Integer): boolean;
{$ENDIF}

procedure PS_SetPlayerHasWeapon(const plnum: Integer; const weapon: Integer; const value: boolean);

function PS_GetPlayerHasWeapon(const plnum: Integer; const weapon: Integer): boolean;

{$IFNDEF HEXEN}
procedure PS_SetPlayerAmmo(const plnum: Integer; const ammotype: Integer; const value: Integer);

function PS_GetPlayerAmmo(const plnum: Integer; const ammotype: Integer): Integer;
{$ENDIF}

{$IFDEF HEXEN}
procedure PS_SetPlayerMana(const plnum: Integer; const mana: Integer; const value: Integer);

function PS_GetPlayerMana(const plnum: Integer; const mana: Integer): Integer;
{$ENDIF}

procedure PS_SetPlayerMessage(const plnum: Integer; const msg: string);

function PS_GetPlayerMessage(const plnum: Integer): string;

procedure PS_SetPlayerExtraLight(const plnum: Integer; const l: Integer);

function PS_GetPlayerExtraLight(const plnum: Integer): Integer;

procedure PS_SetPlayerPowerTicks(const plnum: Integer; const powertype: Integer; const ticks: Integer);

function PS_GetPlayerPowerTicks(const plnum: Integer; const powertype: Integer): Integer;

function PS_GetPlayerViewZ(const plnum: Integer): Integer;

function PS_GetPlayerViewHeight(const plnum: Integer): Integer;

function PS_GetPlayerActor(const plnum: Integer): LongWord;

function PS_ConsolePlayer: Integer;

// -------------------------- TEXTURES -----------------------------------------

function PS_IsValidTexture(const tex: string): boolean;

function PS_GetTextureWidth(const tex: string): Integer;

function PS_GetTextureHeight(const tex: string): Integer;

// ------------------------------ GAME -----------------------------------------

{$IFDEF HEXEN}
function PS_GameMap: integer;
{$ENDIF}

function PS_Game: string;

procedure PS_GlobalEarthQuake(const tics: integer);

function PS_GameSkill: integer;

// ---------------------- REGISTRATION -----------------------------------------

procedure SIRegister_Game(C: TPSPascalCompiler);

procedure RIRegister_Game(CLI: TPSRuntimeClassImporter);

procedure RIRegisterRTL_Game(Exec: TPSExec);

procedure PS_InitGameImport;

procedure PS_ShutDownGameImport;

const
  ACTOR_INVALID = MAXKEY;
  PLAYER_INVALID = MAXKEY;
  MOBJTYPE_INVALID = MAXKEY;
  EDITORNUMBER_INVALID = MAXKEY;
  EDITORNUMBER_UNKNOWN = -1;
  TEXTURE_INVALID = MAXKEY;
  FLAT_INVALID = MAXKEY;
  SECTOR_INVALID = MAXKEY;
  LINE_INVALID = MAXKEY;
  SIDE_INVALID = MAXKEY;
  VERTEX_INVALID = MAXKEY;
  LIGHTLEVEL_INVALID = MAXKEY;

type
  TRTLActor = class(TObject)
  public
    procedure PlaySound(const snd: string);
    procedure Remove;
  end;

  TRTLActors = class(TObject)
  protected
    function GetActorKey(key: LongWord): LongWord;
  public
    function AllActors: TActorArray;
    function AllMonstersAlive: TActorArray;
    function AllMonstersDead: TActorArray;
    function AllMonsters: TActorArray;
    function AllAtSector(const sec: Integer): TActorArray;
    function AllAtSectorTag(const tag: Integer): TActorArray;
    function AllEditorNumber(const dn: Integer): TActorArray;
    function AllType(const typ: Integer): TActorArray;
    property Actor[key: LongWord]: LongWord read GetActorKey; default;
  end;

type
  TRTLVertex = class(TObject);

  TRTLVertexes = class(TObject)
  protected
    function GetVertex(id: Integer): TRTLVertex;
  public
    property Vertex[id: Integer]: TRTLVertex read GetVertex; default;
  end;

type
  TRTLSide = class(TObject);

  TRTLSides = class(TObject)
  protected
    function GetSide(id: Integer): TRTLSide;
  public
    property Side[id: Integer]: TRTLSide read GetSide; default;
  end;

type
  TRTLLine = class(TObject);

  TRTLLines = class(TObject)
  protected
    function GetLine(id: Integer): TRTLLine;
  public
    property Line[id: Integer]: TRTLLine read GetLine; default;
  end;

type
  TRTLSector = class(TObject)
  public
    procedure PlaySound(const snd: string);
    procedure MoveZ(const dz: Integer);
  end;

  TRTLSectors = class(TObject)
  protected
    function GetSector(id: Integer): TRTLSector;
  public
    property Sector[id: Integer]: TRTLSector read GetSector; default;
  end;

implementation

uses
  doomdef,
  doomdata,
  d_player,
  d_think,
  g_game,
  info_h,
  info,
  info_common,
  m_base,
  m_fixed,
  m_rnd,
  p_common,
  p_inter,
  p_map,
  p_mobj,
  p_params,
  p_setup,
  p_tick,
  p_user,
  r_data,
  r_defs,
  r_sky,
  s_sound,
  sounds,
  w_wad;

var
  rtlactors: TRTLActors;
  rtlvertexes: TRTLVertexes;
  rtlsides: TRTLSides;
  rtllines: TRTLLines;
  rtlsectors: TRTLSectors;

// ---------------------------- ACTORS -----------------------------------------

function PS_KeyFromMobj(const mo: Pmobj_t): LongWord;
begin
  if mo = nil then
    Result := ACTOR_INVALID
  else
    Result := mo.key;
end;

function PS_Actor: LongWord;
begin
  if ps_currentactor <> nil then
    Result := ps_currentactor.key
  else
    Result := ACTOR_INVALID;
end;

function mobj_from_key(const key: LongWord): Pmobj_t;
begin
  if key = 0 then
    Result := ps_currentactor
  else if key = ACTOR_INVALID then
    Result := nil
  else
    Result := mobjlist.FindMobjFromKey(key);
end;

function PS_TActor(const key: LongWord): LongWord;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := ACTOR_INVALID;
    Exit;
  end;
  Result := mo.key;
end;

function PS_GetActorTarget(const key: LongWord): LongWord;
var
  mo: Pmobj_t;
  tmo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := ACTOR_INVALID;
    Exit;
  end;
  tmo := P_ActorTarget(mo);
  if tmo <> nil then
    Result := tmo.key
  else
    Result := ACTOR_INVALID;
end;

procedure PS_SetActorTarget(const key: LongWord; const targ: LongWord);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.target := mobj_from_key(targ);
end;

function PS_GetActorX(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.x;
end;

procedure PS_SetActorX(const key: LongWord; const x: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.x := x;
end;

function PS_GetActorY(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.y;
end;

procedure PS_SetActorY(const key: LongWord; const y: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.y := y;
end;

function PS_GetActorZ(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.z;
end;

procedure PS_SetActorZ(const key: LongWord; const z: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.z := z;
end;

function PS_GetActorMOMX(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.momx;
end;

procedure PS_SetActorMOMX(const key: LongWord; const x: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.momx := x;
end;

function PS_GetActorMOMY(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.momy;
end;

procedure PS_SetActorMOMY(const key: LongWord; const y: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.momy := y;
end;

function PS_GetActorMOMZ(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.momz;
end;

procedure PS_SetActorMOMZ(const key: LongWord; const z: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.momz := z;
end;

function PS_GetActorFloorZ(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.floorz;
end;

procedure PS_SetActorFloorZ(const key: LongWord; const z: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.floorz := z;
end;

function PS_GetActorCeilingZ(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.ceilingz;
end;

procedure PS_SetActorCeilingZ(const key: LongWord; const z: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.ceilingz := z;
end;

function PS_GetActorSpeed(const key: LongWord): Integer;
var
  mo: Pmobj_t;
  x, y, z: Integer;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  x := mo.momx;
  y := mo.momy;
  z := mo.momz;
  Result := FixedSqrt(FixedMul(x, x) + FixedMul(y, y) + FixedMul(z, z));
end;

procedure PS_SetActorSpeed(const key: LongWord; const speed: Integer);
var
  mo: Pmobj_t;
  x, y, z: Integer;
  currspeed: Integer;
  ds: double;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  x := mo.momx;
  y := mo.momy;
  z := mo.momz;
  currspeed := FixedSqrt(FixedMul(x, x) + FixedMul(y, y) + FixedMul(z, z));
  if currspeed = 0 then
    Exit;
  ds := speed / currspeed;
  mo.momx := Round(x * ds);
  mo.momy := Round(y * ds);
  mo.momz := Round(z * ds);
end;

function PS_GetActorAngle(const key: LongWord): LongWord;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.angle;
end;

procedure PS_SetActorAngle(const key: LongWord; const angle: LongWord);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.angle := angle;
end;

function PS_GetActorSector(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := Psubsector_t(mo.subsector).sector.iSectorID;
end;

function PS_GetActorHealth(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.health;
end;

procedure PS_SetActorHealth(const key: LongWord; const h: Integer);
var
  mo: Pmobj_t;
  p: Pplayer_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  if h <= 0 then
    P_DamageMobj(mo, nil, nil, 10000)
  else
  begin
    mo.health := h;
    p := mo.player;
    if p <> nil then
      p.health := h;
  end;
end;

function PS_GetActorSpawnHealth(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.info.spawnhealth;
end;

function PS_GetActorMass(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.info.mass;
end;

function PS_GetActorHeight(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.height;
end;

procedure PS_SetActorHeight(const key: LongWord; const h: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.height := h;
end;

function PS_GetActorCustomParam(const key: LongWord; const parm: string): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := P_GetMobjCustomParamValue(mo, parm);
end;

procedure PS_SetActorCustomParam(const key: LongWord; const parm: string; const value: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  P_SetMobjCustomParam(mo, parm, value);
end;

function PS_GetActorCustomDropItem(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.dropitem;
end;

procedure PS_SetActorCustomDropItem(const key: LongWord; const value: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.dropitem := value;
  mo.flags2_ex := mo.flags2_ex or MF2_EX_CUSTOMDROPITEM;
end;

procedure PS_SetActorDefaultDropItem(const key: LongWord);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.dropitem := 0;
  mo.flags2_ex := mo.flags2_ex and not MF2_EX_CUSTOMDROPITEM;
end;

type
  TFlagResult = record
    flags: PInteger;
    flag: Integer;
  end;

function _flag_result(const mo: Pmobj_t; const flag: LongWord): TFlagResult;
begin
  if flag < 32 then
  begin
    Result.flags := @mo.flags;
    Result.flag := (1 shl flag);
    Exit;
  end;

  if flag < 64 then
  begin
  {$IFDEF HERETIC_OR_HEXEN}
    Result.flags := @mo.flags2;
    Result.flag := (1 shl (flag - 32));
  {$ELSE}
    Result.flags := nil;
    Result.flag := 0;
  {$ENDIF}
    Exit;
  end;

  if flag < 96 then
  begin
    Result.flags := @mo.flags_ex;
    Result.flag := (1 shl (flag - 64));
    Exit;
  end;

  if flag < 128 then
  begin
    Result.flags := @mo.flags2_ex;
    Result.flag := (1 shl (flag - 96));
    Exit;
  end;

  Result.flags := nil;
  Result.flag := 0;
end;

function PS_CheckActorFlag(const key: LongWord; const flag: LongWord): boolean;
var
  mo: Pmobj_t;
  flgresult: TFlagResult;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := False;
    Exit;
  end;

  flgresult := _flag_result(mo, flag);
  if flgresult.flags = nil then
  begin
    Result := False;
    Exit;
  end;

  Result := flgresult.flags^ and flgresult.flag <> 0;
end;

procedure PS_SetActorFlag(const key: LongWord; const flag: LongWord);
var
  mo: Pmobj_t;
  flgresult: TFlagResult;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;

  flgresult := _flag_result(mo, flag);
  if flgresult.flags = nil then
    Exit;

  flgresult.flags^ := flgresult.flags^ or flgresult.flag;
end;

procedure PS_UnSetActorFlag(const key: LongWord; const flag: LongWord);
var
  mo: Pmobj_t;
  flgresult: TFlagResult;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;

  flgresult := _flag_result(mo, flag);
  if flgresult.flags = nil then
    Exit;

  flgresult.flags^ := flgresult.flags^ and not flgresult.flag;
end;

function PS_GetActorName(const key: LongWord): string;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := '';
    Exit;
  end;
  Result := mo.info.name;
end;

{$IFDEF STRIFE}
function PS_GetActorName2(const key: LongWord): string;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := '';
    Exit;
  end;
  Result := mo.info.name2;
end;
{$ENDIF}

function PS_GetActorDistanceXY(const key1, key2: LongWord): Integer;
var
  mo1, mo2: Pmobj_t;
  dx, dy: fixed_t;
begin
  mo1 := mobj_from_key(key1);
  if mo1 = nil then
  begin
    Result := 0;
    Exit;
  end;
  mo2 := mobj_from_key(key2);
  if mo2 = nil then
  begin
    Result := 0;
    Exit;
  end;
  dx := mo1.x - mo2.x;
  dy := mo1.y - mo2.y;
  Result := FixedSqrt(dx * dx + dy * dy);
end;

function PS_GetActorDistanceXYZ(const key1, key2: LongWord): Integer;
var
  mo1, mo2: Pmobj_t;
  dx, dy, dz: fixed_t;
begin
  mo1 := mobj_from_key(key1);
  if mo1 = nil then
  begin
    Result := 0;
    Exit;
  end;
  mo2 := mobj_from_key(key2);
  if mo2 = nil then
  begin
    Result := 0;
    Exit;
  end;
  dx := mo1.x - mo2.x;
  dy := mo1.y - mo2.y;
  dz := mo1.z - mo2.z;
  Result := FixedSqrt(dx * dx + dy * dy + dz * dz);
end;

function PS_GetActorPlayer(const key: LongWord): Integer;
var
  mo: Pmobj_t;
  p: Pplayer_t;
  pnum: Integer;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := PLAYER_INVALID;
    Exit;
  end;
  p := mo.player;
  if p = nil then
  begin
    Result := PLAYER_INVALID;
    Exit;
  end;
  pnum := pDiff(p, @players[0], SizeOf(player_t));
  if (pnum < 0) or (pnum >= MAXPLAYERS) then
  begin
    Result := PLAYER_INVALID;
    Exit;
  end;
  if not playeringame[pnum] then
  begin
    Result := PLAYER_INVALID;
    Exit;
  end;
  Result := pnum;
end;

function PS_GetActorMobjType(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := MOBJTYPE_INVALID;
    Exit;
  end;
  Result := mo._type;
end;

function PS_GetActorEditorNumber(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := EDITORNUMBER_INVALID;
    Exit;
  end;
  Result := mo.info.doomednum;
end;

function PS_GetActorSeeSound(const key: LongWord): string;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := '';
    Exit;
  end;
  Result := S_GetSoundNameForNum(mo.info.seesound);
end;

function PS_GetActorAttackSound(const key: LongWord): string;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := '';
    Exit;
  end;
  Result := S_GetSoundNameForNum(mo.info.attacksound);
end;

function PS_GetActorPainSound(const key: LongWord): string;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := '';
    Exit;
  end;
  Result := S_GetSoundNameForNum(mo.info.painsound);
end;

function PS_GetActorDeathSound(const key: LongWord): string;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := '';
    Exit;
  end;
  Result := S_GetSoundNameForNum(mo.info.deathsound);
end;

function PS_GetActorActiveSound(const key: LongWord): string;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := '';
    Exit;
  end;
  Result := S_GetSoundNameForNum(mo.info.activesound);
end;

function PS_GetActorCustomSound1(const key: LongWord): string;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := '';
    Exit;
  end;
  Result := S_GetSoundNameForNum(mo.info.customsound1);
end;

function PS_GetActorCustomSound2(const key: LongWord): string;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := '';
    Exit;
  end;
  Result := S_GetSoundNameForNum(mo.info.customsound2);
end;

function PS_GetActorCustomSound3(const key: LongWord): string;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := '';
    Exit;
  end;
  Result := S_GetSoundNameForNum(mo.info.customsound3);
end;

function PS_GetActorMeleeSound(const key: LongWord): string;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := '';
    Exit;
  end;
  Result := S_GetSoundNameForNum(mo.info.meleesound);
end;

function PS_IsValidActor(const key: LongWord): boolean;
begin
  Result := mobj_from_key(key) <> nil;
end;

procedure _playsound(origin: Pointer; snd: string);
var
  randomlist: TDNumberList;
  soundnum: Integer;
begin
  snd := strupper(strtrim(snd));
  if (snd = '') or (snd = 'DS') then
    Exit;

  soundnum := S_GetSoundNumForName(snd);
  randomlist := S_GetRandomSoundList(soundnum);
  if randomlist <> nil then
    if randomlist.Count > 0 then
      soundnum := randomlist[N_Random mod randomlist.Count];

  S_StartSound(origin, soundnum);
end;

procedure PS_ActorPlaySound(const key: LongWord; const snd: string);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo <> nil then
    _playsound(mo, snd);
end;

procedure PS_PlaySound(const snd: string);
begin
  _playsound(nil, snd);
end;

function PS_SpawnActorType(x, y, z: fixed_t; const typ: Integer): LongWord;
var
  mo: Pmobj_t;
begin
  if (typ >= 0) and (typ < nummobjtypes) then
    mo := P_SpawnMobj(x, y, z, typ)
  else
    mo := nil;
  if mo <> nil then
    Result := mo.key
  else
    Result := ACTOR_INVALID;
end;

function PS_SpawnActorEditorNumber(x, y, z: fixed_t; const ednum: Integer): LongWord;
var
  mobjno: Integer;
begin
  mobjno := Info_GetMobjNumForDoomNum(ednum);
  Result := PS_SpawnActorType(x, y, z, mobjno);
end;

function PS_SpawnActorName(x, y, z: Integer; const name: string): LongWord;
var
  mobjno: Integer;
begin
  mobjno := Info_GetMobjNumForName(name);
  Result := PS_SpawnActorType(x, y, z, mobjno);
end;

procedure PS_RemoveActor(const key: LongWord);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo <> nil then
    P_RemoveMobj(mo);
end;

function PS_ActorTypeFromEditorNumber(const ednum: Integer): Integer;
begin
  Result := Info_GetMobjNumForDoomNum(ednum);
end;

function PS_ActorTypeFromName(const name: string): Integer;
begin
  Result := Info_GetMobjNumForName(name);
end;

// -------------------- TActorKeyList ------------------------------------------

constructor TActorKeyList.Create;
begin
  fList := nil;
  fNumItems := 0;
  fRealSize := 0;
  fAllowDuplicates := false;
  inherited;
end;

destructor TActorKeyList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TActorKeyList.Add(const value: LongWord);
var
  newsize: Integer;
begin
  if not fAllowDuplicates then
    if Exists(value) then // Can not add twice
      Exit;

  if fNumItems >= fRealSize then
  begin
    if fRealSize < 16 then
      newsize := fRealSize + 4
    else if fRealSize < 128 then
      newsize := fRealSize + 16
    else
      newsize := fRealSize + 128;
    realloc(pointer(fList), fRealSize * SizeOf(LongWord), newsize * SizeOf(LongWord));
    fRealSize := newsize;
  end;
  fList[fNumItems] := value;
  Inc(fNumItems);
end;

function TActorKeyList.Delete(const item: LongWord): boolean;
var
  i: Integer;
  idxs: TDNumberList;
  tmpList: PLongWordArray;
  sz: Integer;
begin
  if not fAllowDuplicates then
  begin
    for i := 0 to fNumItems - 1 do
      if fList[i] = item then
      begin
        fList[i] := fList[fNumItems - 1];
        dec(fNumItems);
        Result := True;
        Exit;
      end;

    Result := False;
    Exit;
  end;

  if fNumItems = 0 then
  begin
    Result := False;
    Exit;
  end;

  Result := Exists(item);
  if not Result then
    Exit;

  idxs := TDNumberList.Create;
  for i := 0 to fNumItems - 1 do
    if fList[i] <> item then
      idxs.Add(i);

  sz := fNumItems * SizeOf(LongWord);
  tmpList := malloc(sz);
  for i := 0 to fNumItems - 1 do
    tmpList[i] := fList[i];
  fNumItems := 0;

  for i := 0 to idxs.Count - 1 do
    Add(tmpList[idxs[i]]);
  memfree(Pointer(tmpList), sz);
  idxs.Free;
end;

function TActorKeyList.Exists(const value: LongWord): boolean;
var
  i: Integer;
  l: LongWord;
begin
  for i := 0 to fNumItems - 1 do
    if fList[i] = value then
    begin
      if i > 0 then
      begin
        l := fList[i];
        fList[i] := fList[0];
        fList[0] := l;
      end;
      Result := True;
      Exit;
    end;
  Result := False;
end;

procedure TActorKeyList.Clear;
begin
  realloc(pointer(fList), fNumItems * SizeOf(LongWord), 0);
  fList := nil;
  fNumItems := 0;
  fRealSize := 0;
end;

function TActorKeyList.GetActor(Index: Integer): LongWord;
begin
  if (Index < 0) or (Index >= fNumItems) then
    Result := ACTOR_INVALID
  else
    Result := fList[Index];
end;

procedure TActorKeyList.PutActor(Index: Integer; const value: LongWord);
begin
  if (Index >= 0) and (Index < fNumItems) then
    fList[Index] := value;
end;

procedure TActorKeyList.AddAllActors;
var
  currentthinker: Pthinker_t;
begin
  currentthinker := thinkercap.next;
  while Pointer(currentthinker) <> Pointer(@thinkercap) do
  begin
    if (@currentthinker._function.acp1 = @P_MobjThinker) then
      Add(Pmobj_t(currentthinker).key);
    currentthinker := currentthinker.next;
  end;
end;

procedure TActorKeyList.AddAllMonstersAlive;
var
  currentthinker: Pthinker_t;
  mo: Pmobj_t;
  minfo: Pmobjinfo_t;
begin
  currentthinker := thinkercap.next;
  while Pointer(currentthinker) <> Pointer(@thinkercap) do
  begin
    if (@currentthinker._function.acp1 = @P_MobjThinker) then
    begin
      mo := Pmobj_t(currentthinker);
      if mo.health > 0 then
      begin
        minfo := mo.info;
        if (minfo.doomednum > MAXPLAYERS) and // Not player
        {$IFDEF HERETIC}
           (minfo.doomednum <> 9100) and           // Not player
           (minfo.doomednum <> 9101) and           // Not player
           (minfo.doomednum <> 9102) and           // Not player
           (minfo.doomednum <> 9103) and           // Not player
        {$ENDIF}
           (minfo.flags and MF_SHOOTABLE <> 0) and  // Shootable
          ((minfo.flags and MF_COUNTKILL <> 0) or (minfo.missilestate <> 0) or (minfo.meleestate <> 0)) then  // Count kill or can attack
          Add(mo.key);
      end;
    end;
    currentthinker := currentthinker.next;
  end;
end;

procedure TActorKeyList.AddAllMonstersDead;
var
  currentthinker: Pthinker_t;
  mo: Pmobj_t;
  minfo: Pmobjinfo_t;
begin
  currentthinker := thinkercap.next;
  while Pointer(currentthinker) <> Pointer(@thinkercap) do
  begin
    if (@currentthinker._function.acp1 = @P_MobjThinker) then
    begin
      mo := Pmobj_t(currentthinker);
      if mo.health <= 0 then
      begin
        minfo := mo.info;
        if (minfo.doomednum > MAXPLAYERS) and // Not player
        {$IFDEF HERETIC}
           (minfo.doomednum <> 9100) and           // Not player
           (minfo.doomednum <> 9101) and           // Not player
           (minfo.doomednum <> 9102) and           // Not player
           (minfo.doomednum <> 9103) and           // Not player
        {$ENDIF}
           (minfo.flags and MF_SHOOTABLE <> 0) and  // Shootable
          ((minfo.flags and MF_COUNTKILL <> 0) or (minfo.missilestate <> 0) or (minfo.meleestate <> 0)) then  // Count kill or can attack
          Add(mo.key);
      end;
    end;
    currentthinker := currentthinker.next;
  end;
end;

procedure TActorKeyList.AddAllMonsters;
var
  currentthinker: Pthinker_t;
  mo: Pmobj_t;
  minfo: Pmobjinfo_t;
begin
  currentthinker := thinkercap.next;
  while Pointer(currentthinker) <> Pointer(@thinkercap) do
  begin
    if (@currentthinker._function.acp1 = @P_MobjThinker) then
    begin
      mo := Pmobj_t(currentthinker);
      minfo := mo.info;
      if (minfo.doomednum > MAXPLAYERS) and // Not player
      {$IFDEF HERETIC}
         (minfo.doomednum <> 9100) and           // Not player
         (minfo.doomednum <> 9101) and           // Not player
         (minfo.doomednum <> 9102) and           // Not player
         (minfo.doomednum <> 9103) and           // Not player
      {$ENDIF}
         (minfo.flags and MF_SHOOTABLE <> 0) and  // Shootable
        ((minfo.flags and MF_COUNTKILL <> 0) or (minfo.missilestate <> 0) or (minfo.meleestate <> 0)) then  // Count kill or can attack
        Add(mo.key);
    end;
    currentthinker := currentthinker.next;
  end;
end;

procedure TActorKeyList.AddAllPlayers; // Including voodoo dolls
var
  currentthinker: Pthinker_t;
  mo: Pmobj_t;
begin
  currentthinker := thinkercap.next;
  while Pointer(currentthinker) <> Pointer(@thinkercap) do
  begin
    if (@currentthinker._function.acp1 = @P_MobjThinker) then
    begin
      mo := Pmobj_t(currentthinker);
      if mo.player <> nil then
        Add(mo.key);
    end;
    currentthinker := currentthinker.next;
  end;
end;

procedure TActorKeyList.SetAllowDuplicates(const value: boolean);
var
  i: Integer;
  tmpList: PLongWordArray;
  sz: Integer;
begin
  if fAllowDuplicates <> value then
  begin
    fAllowDuplicates := value;
    if not value then
      if fNumItems > 1 then
      begin
        sz := fNumItems * SizeOf(LongWord);
        tmpList := malloc(sz);
        for i := 0 to fNumItems - 1 do
          tmpList[i] := fList[i];
        fNumItems := 0;
        for i := 0 to fNumItems - 1 do
          Add(tmpList[i]);
        memfree(Pointer(tmpList), sz);
      end;
  end;
end;

function TActorKeyList.FormatName(const name: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(name) do
    if not (name[i] in [' ', ',', ';', '.', '/', '\', '[', ']', '(', ')', '{', '}', #13, #10, #9]) then
      Result := Result + toupper(name[i]);
end;

procedure TActorKeyList.AddAllName(const name: string);
var
  uname: string;
  currentthinker: Pthinker_t;
  mo: Pmobj_t;
  minfo: Pmobjinfo_t;
begin
  uname := FormatName(name);
  currentthinker := thinkercap.next;
  while Pointer(currentthinker) <> Pointer(@thinkercap) do
  begin
    if (@currentthinker._function.acp1 = @P_MobjThinker) then
    begin
      mo := Pmobj_t(currentthinker);
      minfo := mo.info;
      if (FormatName(minfo.Name) = uname) {$IFDEF STRIFE} or (FormatName(minfo.Name2) = uname) {$ENDIF} then
        Add(mo.key);
    end;
    currentthinker := currentthinker.next;
  end;
end;

procedure TActorKeyList.AddAllEdNum(const num: Integer);
var
  currentthinker: Pthinker_t;
  mo: Pmobj_t;
  minfo: Pmobjinfo_t;
begin
  currentthinker := thinkercap.next;
  while Pointer(currentthinker) <> Pointer(@thinkercap) do
  begin
    if (@currentthinker._function.acp1 = @P_MobjThinker) then
    begin
      mo := Pmobj_t(currentthinker);
      minfo := mo.info;
      if minfo.doomednum = num then
        Add(mo.key);
    end;
    currentthinker := currentthinker.next;
  end;
end;

procedure TActorKeyList.AddAllSector(const sec: Integer);
var
  mo: Pmobj_t;
begin
  if (sec < 0) or (sec >= numsectors) then
    Exit;

  mo := sectors[sec].thinglist;
  while mo <> nil do
  begin
    Add(mo.key);
    mo := mo.snext;
  end;
end;

procedure TActorKeyList.AddAllSectorTag(const tag: Integer);
var
  i: Integer;
begin
  for i := 0 to numsectors - 1 do
    if sectors[i].tag = tag then
      AddAllSector(i);
end;

procedure TActorKeyList.AddAllType(const typ: Integer);
var
  currentthinker: Pthinker_t;
  mo: Pmobj_t;
begin
  currentthinker := thinkercap.next;
  while Pointer(currentthinker) <> Pointer(@thinkercap) do
  begin
    if (@currentthinker._function.acp1 = @P_MobjThinker) then
    begin
      mo := Pmobj_t(currentthinker);
      if mo._type = typ then
        Add(mo.key);
    end;
    currentthinker := currentthinker.next;
  end;
end;

procedure TActorKeyList.AddList(const lst: TActorKeyList);
var
  i: Integer;
begin
  for i := 0 to lst.Count - 1 do
    Add(lst[i]);
end;

procedure TActorKeyList.DeleteList(const lst: TActorKeyList);
var
  i: Integer;
begin
  for i := 0 to lst.Count - 1 do
    Delete(lst[i]);
end;

procedure TActorKeyList.RemoveAllDead;
var
  i, j: Integer;
  tmpList: PLongWordArray;
  sz: Integer;
begin
  if fNumItems = 0 then
    Exit;

  sz := fNumItems * SizeOf(LongWord);
  tmpList := malloc(sz);
  j := 0;
  for i := 0 to fNumItems - 1 do
    if PS_GetActorHealth(fList[i]) > 0 then
    begin
      tmpList[j] := fList[i];
      Inc(j);
    end;

  fNumItems := 0;

  for i := 0 to j - 1 do
    Add(tmpList[i]);
  memfree(Pointer(tmpList), sz);
end;

procedure TActorKeyList.RemoveAllAlive;
var
  i, j: Integer;
  tmpList: PLongWordArray;
  sz: Integer;
begin
  if fNumItems = 0 then
    Exit;

  sz := fNumItems * SizeOf(LongWord);
  tmpList := malloc(sz);
  j := 0;
  for i := 0 to fNumItems - 1 do
    if PS_GetActorHealth(fList[i]) <= 0 then
    begin
      tmpList[j] := fList[i];
      Inc(j);
    end;

  fNumItems := 0;

  for i := 0 to j - 1 do
    Add(tmpList[i]);
  memfree(Pointer(tmpList), sz);
end;

function TActorKeyList.GetActorArray: TActorArray;
var
  i: Integer;
begin
  SetLength(Result, fNumItems);
  for i := 0 to fNumItems - 1 do
    Result[i] := fList[i];
end;

// ------------------------ TRTLActor ------------------------------------------

procedure TRTLActor.PlaySound(const snd: string);
begin
  PS_ActorPlaySound(LongWord(self), snd);
end;

procedure TRTLActor.Remove;
begin
  PS_RemoveActor(LongWord(self));
end;


// ------------------------ TRTLActors -----------------------------------------

function TRTLActors.GetActorKey(key: LongWord): LongWord;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := ACTOR_INVALID;
    Exit;
  end;
  Result := mo.key;
end;

function TRTLActors.AllActors: TActorArray;
var
  akl: TActorKeyList;
begin
  akl := TActorKeyList.Create;
  akl.AddAllActors;
  Result := akl.GetActorArray;
  akl.Free;
end;

function TRTLActors.AllMonstersAlive: TActorArray;
var
  akl: TActorKeyList;
begin
  akl := TActorKeyList.Create;
  akl.AddAllMonstersAlive;
  Result := akl.GetActorArray;
  akl.Free;
end;

function TRTLActors.AllMonstersDead: TActorArray;
var
  akl: TActorKeyList;
begin
  akl := TActorKeyList.Create;
  akl.AddAllMonstersDead;
  Result := akl.GetActorArray;
  akl.Free;
end;

function TRTLActors.AllMonsters: TActorArray;
var
  akl: TActorKeyList;
begin
  akl := TActorKeyList.Create;
  akl.AddAllMonsters;
  Result := akl.GetActorArray;
  akl.Free;
end;

function TRTLActors.AllAtSector(const sec: Integer): TActorArray;
var
  akl: TActorKeyList;
begin
  akl := TActorKeyList.Create;
  akl.AddAllSector(sec);
  Result := akl.GetActorArray;
  akl.Free;
end;

function TRTLActors.AllAtSectorTag(const tag: Integer): TActorArray;
var
  akl: TActorKeyList;
begin
  akl := TActorKeyList.Create;
  akl.AddAllSectorTag(tag);
  Result := akl.GetActorArray;
  akl.Free;
end;

function TRTLActors.AllEditorNumber(const dn: Integer): TActorArray;
var
  akl: TActorKeyList;
begin
  akl := TActorKeyList.Create;
  akl.AddAllEdNum(dn);
  Result := akl.GetActorArray;
  akl.Free;
end;

function TRTLActors.AllType(const typ: Integer): TActorArray;
var
  akl: TActorKeyList;
begin
  akl := TActorKeyList.Create;
  akl.AddAllType(typ);
  Result := akl.GetActorArray;
  akl.Free;
end;

// RTLActors Runtime Registration
procedure TRTLActorkey_R(Self: TRTLActor; var T: LongWord);
begin
  T := LongWord(Self);
end;

procedure TRTLActorTarget_W(Self: TRTLActor; const T: TRTLActor);
begin
  PS_SetActorTarget(LongWord(Self), LongWord(T));
end;

procedure TRTLActorTarget_R(Self: TRTLActor; var T: TRTLActor);
begin
  T := TRTLActor(PS_GetActorTarget(LongWord(Self)));
end;

procedure TRTLActorX_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorX(LongWord(Self), T);
end;

procedure TRTLActorX_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorX(LongWord(Self));
end;

procedure TRTLActorY_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorY(LongWord(Self), T);
end;

procedure TRTLActorY_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorY(LongWord(Self));
end;

procedure TRTLActorZ_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorZ(LongWord(Self), T);
end;

procedure TRTLActorZ_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorZ(LongWord(Self));
end;

procedure TRTLActorMOMX_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorMOMX(LongWord(Self), T);
end;

procedure TRTLActorMOMX_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorMOMX(LongWord(Self));
end;

procedure TRTLActorMOMY_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorMOMY(LongWord(Self), T);
end;

procedure TRTLActorMOMY_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorMOMY(LongWord(Self));
end;

procedure TRTLActorMOMZ_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorMOMZ(LongWord(Self), T);
end;

procedure TRTLActorMOMZ_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorMOMZ(LongWord(Self));
end;

procedure TRTLActorFloorZ_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorFloorZ(LongWord(Self), T);
end;

procedure TRTLActorFloorZ_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorFloorZ(LongWord(Self));
end;

procedure TRTLActorCeilingZ_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorCeilingZ(LongWord(Self), T);
end;

procedure TRTLActorCeilingZ_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorCeilingZ(LongWord(Self));
end;

procedure TRTLActorSpeed_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorSpeed(LongWord(Self), T);
end;

procedure TRTLActorSpeed_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorSpeed(LongWord(Self));
end;

procedure TRTLActorAngle_W(Self: TRTLActor; const T: LongWord);
begin
  PS_SetActorAngle(LongWord(Self), T);
end;

procedure TRTLActorAngle_R(Self: TRTLActor; var T: LongWord);
begin
  T := PS_GetActorAngle(LongWord(Self));
end;

procedure TRTLActorSector_R(Self: TRTLActor; var T: TRTLSector);
begin
  T := rtlsectors[PS_GetActorSector(LongWord(Self))];
end;

procedure TRTLActorHealth_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorHealth(LongWord(Self), T);
end;

procedure TRTLActorHealth_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorHealth(LongWord(Self));
end;

procedure TRTLActorHeight_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorHeight(LongWord(Self), T);
end;

procedure TRTLActorSpawnHealth_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorSpawnHealth(LongWord(Self));
end;

procedure TRTLActorMass_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorMass(LongWord(Self));
end;

procedure TRTLActorHeight_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorHeight(LongWord(Self));
end;

procedure TRTLActorCustomParams_W(Self: TRTLActor; const T: Integer; const t1: string);
begin
  PS_SetActorCustomParam(LongWord(Self), t1, T);
end;

procedure TRTLActorCustomParams_R(Self: TRTLActor; var T: Integer; const t1: string);
begin
  T := PS_GetActorCustomParam(LongWord(Self), t1);
end;

procedure TRTLActorCustomDropItem_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorCustomDropItem(LongWord(Self), T);
end;

procedure TRTLActorCustomDropItem_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorCustomDropItem(LongWord(Self));
end;

procedure TRTLActorFlags_W(Self: TRTLActor; const T: boolean; const t1: LongWord);
begin
  if T then
    PS_SetActorFlag(LongWord(Self), t1)
  else
    PS_UnSetActorFlag(LongWord(Self), t1)
end;

procedure TRTLActorFlags_R(Self: TRTLActor; var T: boolean; const t1: LongWord);
begin
  T := PS_CheckActorFlag(LongWord(Self), t1);
end;

procedure TRTLActorName_R(Self: TRTLActor; var T: string);
begin
  T := PS_GetActorName(LongWord(Self));
end;

{$IFDEF STRIFE}
procedure TRTLActorName2_R(Self: TRTLActor; var T: string);
begin
  T := PS_GetActorName2(LongWord(Self));
end;
{$ENDIF}

procedure TRTLActorPlayer_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorPlayer(LongWord(Self));
end;

procedure TRTLActorMobjType_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorMobjType(LongWord(Self));
end;

procedure TRTLActorEditorNumber_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorEditorNumber(LongWord(Self));
end;

procedure TRTLActorSeeSound_R(Self: TRTLActor; var T: string);
begin
  T := PS_GetActorSeeSound(LongWord(Self));
end;

procedure TRTLActorAttackSound_R(Self: TRTLActor; var T: string);
begin
  T := PS_GetActorAttackSound(LongWord(Self));
end;

procedure TRTLActorPainSound_R(Self: TRTLActor; var T: string);
begin
  T := PS_GetActorPainSound(LongWord(Self));
end;

procedure TRTLActorDeathSound_R(Self: TRTLActor; var T: string);
begin
  T := PS_GetActorDeathSound(LongWord(Self));
end;

procedure TRTLActorActiveSound_R(Self: TRTLActor; var T: string);
begin
  T := PS_GetActorActiveSound(LongWord(Self));
end;

procedure TRTLActorCustomSound1_R(Self: TRTLActor; var T: string);
begin
  T := PS_GetActorCustomSound1(LongWord(Self));
end;

procedure TRTLActorCustomSound2_R(Self: TRTLActor; var T: string);
begin
  T := PS_GetActorCustomSound2(LongWord(Self));
end;

procedure TRTLActorCustomSound3_R(Self: TRTLActor; var T: string);
begin
  T := PS_GetActorCustomSound3(LongWord(Self));
end;

procedure TRTLActorMeleeSound_R(Self: TRTLActor; var T: string);
begin
  T := PS_GetActorMeleeSound(LongWord(Self));
end;

procedure TRTLActorsActor_R(Self: TRTLActors; var T: TRTLActor; const t1: LongWord);
begin
  T := TRTLActor(Self.Actor[t1]);
end;

// ------------------------------- MAP -----------------------------------------

// -------------------------- VERTEXES -----------------------------------------

function PS_TVertex(const id: Integer): Integer;
begin
  if (id >= 0) and (id < numvertexes) then
    Result := id + 1
  else
    Result := VERTEX_INVALID;
end;

function PS_GetVertexX(const v: Integer): Integer;
begin
  if (v >= 0) and (v < numvertexes) then
    Result := vertexes[v].x
  else
    Result := 0;
end;

function PS_GetVertexY(const v: Integer): Integer;
begin
  if (v >= 0) and (v < numvertexes) then
    Result := vertexes[v].y
  else
    Result := 0;
end;

function PS_IsValidVertex(const v: Integer): boolean;
begin
  Result := (v >= 0) and (v < numvertexes);
end;

function PS_GetVertexCount: Integer;
begin
  Result := numvertexes;
end;

function TRTLVertexes.GetVertex(id: Integer): TRTLVertex;
begin
  if (id >= 0) and (id < numvertexes) then
    Result := TRTLVertex(id + 1)
  else
    Result := TRTLVertex(VERTEX_INVALID);
end;

procedure TRTLVertexX_R(Self: TRTLVertex; var T: Integer);
begin
  T := PS_GetVertexX(Integer(Self) - 1);
end;

procedure TRTLVertexY_R(Self: TRTLVertex; var T: Integer);
begin
  T := PS_GetVertexY(Integer(Self) - 1);
end;

procedure TRTLVertexID_R(Self: TRTLVertex; var T: Integer);
begin
  T := Integer(Self) - 1;
  if (T < 0) or (T >= numvertexes) then
    T := VERTEX_INVALID;
end;

procedure TRTLVertexesVertex_R(Self: TRTLVertexes; var T: TRTLVertex; const t1: Integer);
begin
  T := Self[t1];
end;

procedure TRTLVertexesCount_R(Self: TRTLActor; var T: Integer);
begin
  T := numvertexes;
end;

// ----------------------------- SIDES -----------------------------------------

function PS_TSide(const id: Integer): Integer;
begin
  if (id >= 0) and (id < numsides) then
    Result := id + 1
  else
    Result := SIDE_INVALID;
end;

function PS_GetSideTextureOffset(const sd: Integer): Integer;
begin
  if (sd >= 0) and (sd < numsides) then
    Result := sides[sd].textureoffset
  else
    Result := 0;
end;

procedure PS_SetSideTextureOffset(const sd: Integer; const offs: Integer);
begin
  if (sd >= 0) and (sd < numsides) then
    sides[sd].textureoffset := offs;
end;

function PS_GetSideRowOffset(const sd: Integer): Integer;
begin
  if (sd >= 0) and (sd < numsides) then
    Result := sides[sd].rowoffset
  else
    Result := 0;
end;

procedure PS_SetSideRowOffset(const sd: Integer; const offs: Integer);
begin
  if (sd >= 0) and (sd < numsides) then
    sides[sd].rowoffset := offs;
end;

function _texturenamefromid(const id: Integer): string;
begin
  if id = TEXTURE_INVALID then
    Result := ''
  else
  begin
    Result := char8tostring(R_NameForSideTexture(id));
    if Result = '-' then
      Result := '';
  end;
end;

function _textureidfromname(const name: string): Integer;
begin
  if (name = '') or (name = '-') then
  begin
    Result := 0;
    Exit;
  end;

  Result := R_CheckTextureNumForName(name);
  if Result < 0 then
    Result := TEXTURE_INVALID;
end;

function PS_GetSideTopTexture(const sd: Integer): string;
begin
  if (sd >= 0) and (sd < numsides) then
    Result := _texturenamefromid(sides[sd].toptexture)
  else
    Result := '';
end;

procedure PS_SetSideTopTexture(const sd: Integer; const tex: string);
var
  tid: Integer;
begin
  if (sd >= 0) and (sd < numsides) then
  begin
    tid := _textureidfromname(tex);
    if tid <> TEXTURE_INVALID then
      sides[sd].toptexture := tid;
  end;
end;

function PS_GetSideBottomTexture(const sd: Integer): string;
begin
  if (sd >= 0) and (sd < numsides) then
    Result := _texturenamefromid(sides[sd].bottomtexture)
  else
    Result := '';
end;

procedure PS_SetSideBottomTexture(const sd: Integer; const tex: string);
var
  tid: Integer;
begin
  if (sd >= 0) and (sd < numsides) then
  begin
    tid := _textureidfromname(tex);
    if tid <> TEXTURE_INVALID then
      sides[sd].bottomtexture := tid;
  end;
end;

function PS_GetSideMiddleTexture(const sd: Integer): string;
begin
  if (sd >= 0) and (sd < numsides) then
    Result := _texturenamefromid(sides[sd].midtexture)
  else
    Result := '';
end;

procedure PS_SetSideMiddleTexture(const sd: Integer; const tex: string);
var
  tid: Integer;
begin
  if (sd >= 0) and (sd < numsides) then
  begin
    tid := _textureidfromname(tex);
    if tid <> TEXTURE_INVALID then
      sides[sd].midtexture := tid;
  end;
end;

function PS_GetSideSector(const sd: Integer): Integer;
begin
  if (sd >= 0) and (sd < numsides) then
    Result := sides[sd].sector.iSectorID
  else
    Result := SECTOR_INVALID;
end;

function PS_IsValidSide(const sd: Integer): boolean;
begin
  Result := (sd >= 0) and (sd < numsides);
end;

function PS_GetSideCount: Integer;
begin
  Result := numsides;
end;

function TRTLSides.GetSide(id: Integer): TRTLSide;
begin
  if (id >= 0) and (id < numsides) then
    Result := TRTLSide(id + 1)
  else
    Result := TRTLSide(SIDE_INVALID);
end;

procedure TRTLSideTextureOffset_W(Self: TRTLSide; const T: fixed_t);
begin
  PS_SetSideTextureOffset(Integer(Self) - 1, T);
end;

procedure TRTLSideTextureOffset_R(Self: TRTLSide; var T: fixed_t);
begin
  T := PS_GetSideTextureOffset(Integer(Self) - 1);
end;

procedure TRTLSideRowOffset_W(Self: TRTLSide; const T: fixed_t);
begin
  PS_SetSideRowOffset(Integer(Self) - 1, T);
end;

procedure TRTLSideRowOffset_R(Self: TRTLSide; var T: fixed_t);
begin
  T := PS_GetSideRowOffset(Integer(Self) - 1);
end;

procedure TRTLSideTopTexture_W(Self: TRTLSide; const T: string);
begin
  PS_SetSideTopTexture(Integer(Self) - 1, T);
end;

procedure TRTLSideTopTexture_R(Self: TRTLSide; var T: string);
begin
  T := PS_GetSideTopTexture(Integer(Self) - 1);
end;

procedure TRTLSideBottomTexture_W(Self: TRTLSide; const T: string);
begin
  PS_SetSideBottomTexture(Integer(Self) - 1, T);
end;

procedure TRTLSideBottomTexture_R(Self: TRTLSide; var T: string);
begin
  T := PS_GetSideBottomTexture(Integer(Self) - 1);
end;

procedure TRTLSideMiddleTexture_W(Self: TRTLSide; const T: string);
begin
  PS_SetSideMiddleTexture(Integer(Self) - 1, T);
end;

procedure TRTLSideMiddleTexture_R(Self: TRTLSide; var T: string);
begin
  T := PS_GetSideMiddleTexture(Integer(Self) - 1);
end;

procedure TRTLSideSector_R(Self: TRTLSide; var T: TRTLSector);
begin
  T := rtlsectors[PS_GetSideSector(Integer(Self) - 1)];
end;

procedure TRTLSideID_R(Self: TRTLSide; var T: Integer);
begin
  T := Integer(Self) - 1;
  if (T < 0) or (T >= numsides) then
    T := SIDE_INVALID;
end;

procedure TRTLSidesCount_R(Self: TRTLSides; var T: Integer);
begin
  T := numsides;
end;

procedure TRTLSidesSides_R(Self: TRTLSides; var T: TRTLSide; const t1: Integer);
begin
  // When side = 0 returns null and the script makes runtime error !
  T := Self[t1];
end;

// ----------------------------- LINES -----------------------------------------

function PS_TLine(const id: Integer): Integer;
begin
  if (id >= 0) and (id < numlines) then
    Result := id + 1
  else
    Result := LINE_INVALID;
end;

function PS_GetLineVertex1(const ld: Integer): Integer;
begin
  if (ld >= 0) and (ld < numlines) then
    Result := pDiff(lines[ld].v1, @vertexes[0], SizeOf(vertex_t))
  else
    Result := VERTEX_INVALID;
end;

function PS_GetLineVertex2(const ld: Integer): Integer;
begin
  if (ld >= 0) and (ld < numlines) then
    Result := pDiff(lines[ld].v2, @vertexes[0], SizeOf(vertex_t))
  else
    Result := VERTEX_INVALID;
end;

function PS_GetLineDX(const ld: Integer): Integer;
begin
  if (ld >= 0) and (ld < numlines) then
    Result := lines[ld].dx
  else
    Result := 0;
end;

function PS_GetLineDY(const ld: Integer): Integer;
begin
  if (ld >= 0) and (ld < numlines) then
    Result := lines[ld].dy
  else
    Result := 0;
end;

function PS_GetLineSpecial(const ld: Integer): Integer;
begin
  if (ld >= 0) and (ld < numlines) then
    Result := lines[ld].special
  else
    Result := 0;
end;

procedure PS_SetLineSpecial(const ld: Integer; const spec: Integer);
begin
  if (ld >= 0) and (ld < numlines) then
    lines[ld].special := spec;
end;

{$IFDEF HEXEN}
function PS_GetLineArg1(const ld: Integer): byte;
begin
  if (ld >= 0) and (ld < numlines) then
    Result := lines[ld].arg1
  else
    Result := 0;
end;

procedure PS_SetLineArg1(const ld: Integer; const arg: byte);
begin
  if (ld >= 0) and (ld < numlines) then
    lines[ld].arg1 := arg;
end;

function PS_GetLineArg2(const ld: Integer): byte;
begin
  if (ld >= 0) and (ld < numlines) then
    Result := lines[ld].arg2
  else
    Result := 0;
end;

procedure PS_SetLineArg2(const ld: Integer; const arg: byte);
begin
  if (ld >= 0) and (ld < numlines) then
    lines[ld].arg2 := arg;
end;

function PS_GetLineArg3(const ld: Integer): byte;
begin
  if (ld >= 0) and (ld < numlines) then
    Result := lines[ld].arg3
  else
    Result := 0;
end;

procedure PS_SetLineArg3(const ld: Integer; const arg: byte);
begin
  if (ld >= 0) and (ld < numlines) then
    lines[ld].arg3 := arg;
end;

function PS_GetLineArg4(const ld: Integer): byte;
begin
  if (ld >= 0) and (ld < numlines) then
    Result := lines[ld].arg4
  else
    Result := 0;
end;

procedure PS_SetLineArg4(const ld: Integer; const arg: byte);
begin
  if (ld >= 0) and (ld < numlines) then
    lines[ld].arg4 := arg;
end;

function PS_GetLineArg5(const ld: Integer): byte;
begin
  if (ld >= 0) and (ld < numlines) then
    Result := lines[ld].arg5
  else
    Result := 0;
end;

procedure PS_SetLineArg5(const ld: Integer; const arg: byte);
begin
  if (ld >= 0) and (ld < numlines) then
    lines[ld].arg5 := arg;
end;
{$ELSE}
function PS_GetLineTag(const ld: Integer): Integer;
begin
  if (ld >= 0) and (ld < numlines) then
    Result := lines[ld].tag
  else
    Result := 0;
end;

procedure PS_SetLineTag(const ld: Integer; const tg: Integer);
begin
  if (ld >= 0) and (ld < numlines) then
    lines[ld].tag := tg;
end;

function PS_GetLineTransparent(const ld: Integer): Boolean;
begin
  if (ld >= 0) and (ld < numlines) then
    Result := lines[ld].renderflags and LRF_TRANSPARENT <> 0
  else
    Result := False;
end;

procedure PS_SetLineTransparent(const ld: Integer; const x: Boolean);
var
  sd: integer;
begin
  if (ld >= 0) and (ld < numlines) then
  begin
    sd := lines[ld].sidenum[0];
    if (sd < 0) or (sd >= numsides) then
      Exit;
    sd := lines[ld].sidenum[1];
    if (sd < 0) or (sd >= numsides) then
      Exit;
    if x then
      lines[ld].renderflags := lines[ld].renderflags or LRF_TRANSPARENT
    else
      lines[ld].renderflags := lines[ld].renderflags and not LRF_TRANSPARENT;
  end;
end;
{$ENDIF}

function PS_GetLineIsBlocking(const ld: Integer): Boolean;
begin
  if (ld >= 0) and (ld < numlines) then
    Result := lines[ld].flags and ML_BLOCKING <> 0
  else
    Result := False;
end;

procedure PS_SetLineIsBlocking(const ld: Integer; const x: Boolean);
var
  sd: integer;
begin
  if (ld >= 0) and (ld < numlines) then
  begin
    sd := lines[ld].sidenum[0];
    if (sd < 0) or (sd >= numsides) then
      Exit;
    sd := lines[ld].sidenum[1];
    if (sd < 0) or (sd >= numsides) then
      Exit;
    if x then
      lines[ld].flags := lines[ld].flags or ML_BLOCKING
    else
      lines[ld].flags := lines[ld].flags and not ML_BLOCKING;
  end;
end;

function PS_GetLineIsBlockingMonsters(const ld: Integer): Boolean;
begin
  if (ld >= 0) and (ld < numlines) then
    Result := lines[ld].flags and ML_BLOCKMONSTERS <> 0
  else
    Result := False;
end;

procedure PS_SetLineIsBlockingMonsters(const ld: Integer; const x: Boolean);
var
  sd: integer;
begin
  if (ld >= 0) and (ld < numlines) then
  begin
    sd := lines[ld].sidenum[0];
    if (sd < 0) or (sd >= numsides) then
      Exit;
    sd := lines[ld].sidenum[1];
    if (sd < 0) or (sd >= numsides) then
      Exit;
    if x then
      lines[ld].flags := lines[ld].flags or ML_BLOCKMONSTERS
    else
      lines[ld].flags := lines[ld].flags and not ML_BLOCKMONSTERS;
  end;
end;

function PS_GetLineTriggerScripts(const ld: Integer): Boolean;
begin
  if (ld >= 0) and (ld < numlines) then
    Result := lines[ld].flags and ML_TRIGGERSCRIPTS <> 0
  else
    Result := False;
end;

procedure PS_SetLineTriggerScripts(const ld: Integer; const x: Boolean);
begin
  if (ld >= 0) and (ld < numlines) then
  begin
    if x then
      lines[ld].flags := lines[ld].flags or ML_TRIGGERSCRIPTS
    else
      lines[ld].flags := lines[ld].flags and not ML_TRIGGERSCRIPTS;
  end;
end;

function PS_GetLineFrontSide(const ld: Integer): Integer;
begin
  if (ld >= 0) and (ld < numlines) then
    Result := lines[ld].sidenum[0]
  else
    Result := SIDE_INVALID;
end;

function PS_GetLineBackSide(const ld: Integer): Integer;
begin
  if (ld >= 0) and (ld < numlines) then
    Result := lines[ld].sidenum[1]
  else
    Result := SIDE_INVALID;
end;

function PS_GetLineFrontSector(const ld: Integer): Integer;
var
  sec: Psector_t;
begin
  if (ld >= 0) and (ld < numlines) then
  begin
    sec := lines[ld].frontsector;
    if sec <> nil then
      Result := sec.iSectorID
    else
      Result := SIDE_INVALID;
  end
  else
    Result := SIDE_INVALID;
end;

function PS_GetLineBackSector(const ld: Integer): Integer;
var
  sec: Psector_t;
begin
  if (ld >= 0) and (ld < numlines) then
  begin
    sec := lines[ld].backsector;
    if sec <> nil then
      Result := sec.iSectorID
    else
      Result := SIDE_INVALID;
  end
  else
    Result := SIDE_INVALID;
end;

function PS_IsValidLine(const ld: Integer): boolean;
begin
  Result := (ld >= 0) and (ld < numlines);
end;

function PS_GetLineCount: Integer;
begin
  Result := numlines;
end;

{$IFNDEF HEXEN}
function PS_FindLinesFromTag(const tag: integer): TDynamicIntegerArray;
var
  l: TDNumberList;
  i: integer;
begin
  l := TDNumberList.Create;
  for i := 0 to numlines - 1 do
    if lines[i].tag = tag then
      l.Add(i);
  SetLength(Result, l.Count);
  for i := 0 to l.Count - 1 do
    Result[i] := l.Numbers[i];
  l.Free;
end;
{$ENDIF}

function TRTLLines.GetLine(id: Integer): TRTLLine;
begin
  if (id >= 0) and (id < numlines) then
    Result := TRTLLine(id + 1)
  else
    Result := TRTLLine(LINE_INVALID);
end;

procedure TRTLLineBackSector_R(Self: TRTLLine; var T: TRTLSector);
begin
  T := rtlsectors[PS_GetLineBackSector(Integer(Self) - 1)];
end;

procedure TRTLLineFrontSector_R(Self: TRTLLine; var T: TRTLSector);
begin
  T := rtlsectors[PS_GetLineFrontSector(Integer(Self) - 1)];
end;

procedure TRTLLineBackSide_R(Self: TRTLLine; var T: TRTLSide);
begin
  T := rtlsides[PS_GetLineBackSide(Integer(Self) - 1)];
end;

procedure TRTLLineFrontSide_R(Self: TRTLLine; var T: TRTLSide);
begin
  T := rtlsides[PS_GetLineFrontSide(Integer(Self) - 1)];
end;

{$IFNDEF HEXEN}
procedure TRTLLineTag_W(Self: TRTLLine; const T: Integer);
begin
  PS_SetLineTag(Integer(Self) - 1, T);
end;

procedure TRTLLineTag_R(Self: TRTLLine; var T: Integer);
begin
  T := PS_GetLineTag(Integer(Self) - 1);
end;

{$ELSE}
procedure TRTLLineArg5_W(Self: TRTLLine; const T: byte);
begin
  PS_SetLineArg5(Integer(Self) - 1, T);
end;

procedure TRTLLineArg5_R(Self: TRTLLine; var T: byte);
begin
  T := PS_GetLineArg5(Integer(Self) - 1);
end;

procedure TRTLLineArg4_W(Self: TRTLLine; const T: byte);
begin
  PS_SetLineArg4(Integer(Self) - 1, T);
end;

procedure TRTLLineArg4_R(Self: TRTLLine; var T: byte);
begin
  T := PS_GetLineArg4(Integer(Self) - 1);
end;

procedure TRTLLineArg3_W(Self: TRTLLine; const T: byte);
begin
  PS_SetLineArg3(Integer(Self) - 1, T);
end;

procedure TRTLLineArg3_R(Self: TRTLLine; var T: byte);
begin
  T := PS_GetLineArg3(Integer(Self) - 1);
end;

procedure TRTLLineArg2_W(Self: TRTLLine; const T: byte);
begin
  PS_SetLineArg2(Integer(Self) - 1, T);
end;

procedure TRTLLineArg2_R(Self: TRTLLine; var T: byte);
begin
  T := PS_GetLineArg2(Integer(Self) - 1);
end;

procedure TRTLLineArg1_W(Self: TRTLLine; const T: byte);
begin
  PS_SetLineArg1(Integer(Self) - 1, T);
end;

procedure TRTLLineArg1_R(Self: TRTLLine; var T: byte);
begin
  T := PS_GetLineArg1(Integer(Self) - 1);
end;
{$ENDIF}

{$IFNDEF HEXEN}
procedure TRTLLineTransparent_W(Self: TRTLLine; const T: Boolean);
begin
  PS_SetLineTransparent(Integer(Self) - 1, T);
end;

procedure TRTLLineTransparent_R(Self: TRTLLine; var T: Boolean);
begin
  T := PS_GetLineTransparent(Integer(Self) - 1);
end;
{$ENDIF}

procedure TRTLLineIsBlocking_W(Self: TRTLLine; const T: Boolean);
begin
  PS_SetLineIsBlocking(Integer(Self) - 1, T);
end;

procedure TRTLLineIsBlocking_R(Self: TRTLLine; var T: Boolean);
begin
  T := PS_GetLineIsBlocking(Integer(Self) - 1);
end;

procedure TRTLLineIsBlockingMonsters_W(Self: TRTLLine; const T: Boolean);
begin
  PS_SetLineIsBlockingMonsters(Integer(Self) - 1, T);
end;

procedure TRTLLineIsBlockingMonsters_R(Self: TRTLLine; var T: Boolean);
begin
  T := PS_GetLineIsBlockingMonsters(Integer(Self) - 1);
end;

procedure TRTLLineTriggerScripts_W(Self: TRTLLine; const T: Boolean);
begin
  PS_SetLineTriggerScripts(Integer(Self) - 1, T);
end;

procedure TRTLLineTriggerScripts_R(Self: TRTLLine; var T: Boolean);
begin
  T := PS_GetLineTriggerScripts(Integer(Self) - 1);
end;

procedure TRTLLineSpecial_W(Self: TRTLLine; const T: Integer);
begin
  PS_SetLineSpecial(Integer(Self) - 1, T);
end;

procedure TRTLLineSpecial_R(Self: TRTLLine; var T: Integer);
begin
  T := PS_GetLineSpecial(Integer(Self) - 1);
end;

procedure TRTLLineDY_R(Self: TRTLLine; var T: fixed_t);
begin
  T := PS_GetLineDY(Integer(Self) - 1);
end;

procedure TRTLLineDX_R(Self: TRTLLine; var T: fixed_t);
begin
  T := PS_GetLineDX(Integer(Self) - 1);
end;

procedure TRTLLineVertex2_R(Self: TRTLLine; var T: TRTLVertex);
begin
  T := rtlvertexes[PS_GetLineVertex2(Integer(Self) - 1)];
end;

procedure TRTLLineVertex1_R(Self: TRTLLine; var T: TRTLVertex);
begin
  T := rtlvertexes[PS_GetLineVertex1(Integer(Self) - 1)];
end;

procedure TRTLLineID_R(Self: TRTLLine; var T: Integer);
begin
  T := Integer(Self) - 1;
  if (T < 0) or (T >= numlines) then
    T := LINE_INVALID;
end;

procedure TRTLLinesCount_R(Self: TRTLLines; var T: Integer);
begin
  T := numlines;
end;

procedure TRTLLinesLine_R(Self: TRTLLines; var T: TRTLLine; const t1: Integer);
begin
  T := Self[t1];
end;

// --------------------------- SECTORS -----------------------------------------

function PS_TSector(const id: Integer): Integer;
begin
  if (id >= 0) and (id < numsectors) then
    Result := id + 1
  else
    Result := SECTOR_INVALID;
end;

function PS_GetSectorFloorHeight(const sec: Integer): Integer;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := sectors[sec].floorheight
  else
    Result := 0;
end;

procedure PS_SetSectorFloorHeight(const sec: Integer; const x: Integer);
var
  p: Psector_t;
begin
  if (sec >= 0) and (sec < numsectors) then
  begin
    p := @sectors[sec];
    p.floorheight := x;
    P_ChangeSector(p, false);
  end;
end;

function PS_GetSectorCeilingHeight(const sec: Integer): Integer;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := sectors[sec].ceilingheight
  else
    Result := 0;
end;

procedure PS_SetSectorCeilingHeight(const sec: Integer; const x: Integer);
var
  p: Psector_t;
begin
  if (sec >= 0) and (sec < numsectors) then
  begin
    p := @sectors[sec];
    p.ceilingheight := x;
    P_ChangeSector(p, false);
  end;
end;

function _getnameforflat(const id: Integer): string;
begin
  if (id >= 0) and (id < numflats) then
    Result := char8tostring(flats[id].name)
  else
    Result := '';
end;

function PS_GetSectorFloorPicture(const sec: Integer): string;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := _getnameforflat(sectors[sec].floorpic)
  else
    Result := '';
end;

procedure PS_SetSectorFloorPicture(const sec: Integer; const pic: string);
var
  flat: Integer;
begin
  if (sec >= 0) and (sec < numsectors) then
  begin
    flat := R_SafeFlatNumForName(pic);
    if (flat >= 0) and (flat < numflats) then
      sectors[sec].floorpic := flat;
  end;
end;

function PS_GetSectorCeilingPicture(const sec: Integer): string;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := _getnameforflat(sectors[sec].ceilingpic)
  else
    Result := '';
end;

procedure PS_SetSectorCeilingPicture(const sec: Integer; const pic: string);
var
  flat: Integer;
begin
  if (sec >= 0) and (sec < numsectors) then
  begin
    flat := R_SafeFlatNumForName(pic);
    if (flat >= 0) and (flat < numflats) then
      sectors[sec].ceilingpic := flat;
  end;
end;

function PS_GetSectorLightLevel(const sec: Integer): Integer;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := sectors[sec].lightlevel
  else
    Result := LIGHTLEVEL_INVALID;
end;

procedure PS_SetSectorLightLevel(const sec: Integer; const x: Integer);
begin
  if (sec >= 0) and (sec < numsectors) then
    if (x >= 0) and (x < 255) then
      sectors[sec].lightlevel := x;
end;

function PS_GetSectorSpecial(const sec: Integer): Integer;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := sectors[sec].special
  else
    Result := 0;
end;

function PS_GetSectorTag(const sec: Integer): Integer;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := sectors[sec].tag
  else
    Result := 0;
end;

procedure PS_SetSectorTag(const sec: Integer; const x: Integer);
begin
  if (sec >= 0) and (sec < numsectors) then
    sectors[sec].tag := x;
end;

function PS_GetSectorActors(const sec: Integer): TActorArray;
var
  akl: TActorKeyList;
begin
  akl := TActorKeyList.Create;
  akl.AddAllSector(sec);
  Result := akl.GetActorArray;
  akl.Free;
end;

function PS_GetSectorNumActors(const sec: Integer): Integer;
var
  mo: Pmobj_t;
begin
  Result := 0;
  if (sec < 0) or (sec >= numsectors) then
    Exit;

  mo := sectors[sec].thinglist;
  while mo <> nil do
  begin
    Inc(Result);
    mo := mo.snext;
  end;
end;

function PS_GetSectorLines(const sec: Integer): TDynamicIntegerArray;
var
  i: Integer;
begin
  if (sec >= 0) and (sec < numsectors) then
  begin
    SetLength(Result, sectors[sec].linecount);
    for i := 0 to sectors[sec].linecount - 1 do
      Result[i] := pDiff(sectors[sec].lines[i], lines, SizeOf(line_t));
  end
  else
    SetLength(Result, 0);
end;

function PS_GetSectorNumLines(const sec: Integer): Integer;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := sectors[sec].linecount
  else
    Result := 0;
end;

{$IFDEF DOOM_OR_STRIFE}
function PS_GetSectorFloorXOffset(const sec: Integer): Integer;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := sectors[sec].floor_xoffs
  else
    Result := 0;
end;

procedure PS_SetSectorFloorXOffset(const sec: Integer; const offs: Integer);
begin
  if (sec >= 0) and (sec < numsectors) then
    sectors[sec].floor_xoffs := offs;
end;

function PS_GetSectorFloorYOffset(const sec: Integer): Integer;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := sectors[sec].floor_yoffs
  else
    Result := 0;
end;

procedure PS_SetSectorFloorYOffset(const sec: Integer; const offs: Integer);
begin
  if (sec >= 0) and (sec < numsectors) then
    sectors[sec].floor_yoffs := offs;
end;

function PS_GetSectorCeilingXOffset(const sec: Integer): Integer;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := sectors[sec].ceiling_xoffs
  else
    Result := 0;
end;

procedure PS_SetSectorCeilingXOffset(const sec: Integer; const offs: Integer);
begin
  if (sec >= 0) and (sec < numsectors) then
    sectors[sec].ceiling_xoffs := offs;
end;

function PS_GetSectorCeilingYOffset(const sec: Integer): Integer;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := sectors[sec].ceiling_yoffs
  else
    Result := 0;
end;

procedure PS_SetSectorCeilingYOffset(const sec: Integer; const offs: Integer);
begin
  if (sec >= 0) and (sec < numsectors) then
    sectors[sec].ceiling_yoffs := offs;
end;
{$ENDIF}

function PS_GetSectorRippleFloor(const sec: Integer): Boolean;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := sectors[sec].renderflags and SRF_RIPPLE_FLOOR <> 0
  else
    Result := False;
end;

procedure PS_SetSectorRippleFloor(const sec: Integer; const rpl: Boolean);
begin
  if (sec >= 0) and (sec < numsectors) then
  begin
    if rpl then
      sectors[sec].renderflags := sectors[sec].renderflags or SRF_RIPPLE_FLOOR
    else
      sectors[sec].renderflags := sectors[sec].renderflags and not SRF_RIPPLE_FLOOR
  end;
end;

function PS_GetSectorRippleCeiling(const sec: Integer): Boolean;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := sectors[sec].renderflags and SRF_RIPPLE_CEILING <> 0
  else
    Result := False;
end;

procedure PS_SetSectorRippleCeiling(const sec: Integer; const rpl: Boolean);
begin
  if (sec >= 0) and (sec < numsectors) then
  begin
    if rpl then
      sectors[sec].renderflags := sectors[sec].renderflags or SRF_RIPPLE_CEILING
    else
      sectors[sec].renderflags := sectors[sec].renderflags and not SRF_RIPPLE_CEILING
  end;
end;

function PS_GetSectorMidSector(const sec: Integer): Integer;
begin
  if (sec >= 0) and (sec < numsectors) then
  begin
    Result := sectors[sec].midsec;
    if (Result < 0) or (Result >= numsectors) then
      Result := SECTOR_INVALID;
  end
  else
    Result := SECTOR_INVALID;
end;

function PS_GetSectorSlopeSector(const sec: Integer): Integer;
begin
  if (sec >= 0) and (sec < numsectors) then
  begin
    if sectors[sec].slopesec <> nil then
      Result := sectors[sec].slopesec.iSectorID
    else
      Result := SECTOR_INVALID;
  end
  else
    Result := SECTOR_INVALID;
end;

function PS_SkyPicture: string;
begin
  Result := _getnameforflat(skyflatnum);
end;

function PS_IsValidSector(const sec: Integer): boolean;
begin
  Result := (sec >= 0) and (sec < numsectors);
end;

function PS_GetSectorCount: Integer;
begin
  Result := numsectors;
end;

function PS_FindSectorsFromTag(const tag: integer): TDynamicIntegerArray;
var
  l: TDNumberList;
  i: integer;
begin
  l := TDNumberList.Create;
  for i := 0 to numsectors - 1 do
    if sectors[i].tag = tag then
      l.Add(i);
  SetLength(Result, l.Count);
  for i := 0 to l.Count - 1 do
    Result[i] := l.Numbers[i];
  l.Free;
end;

procedure PS_SectorPlaySound(const secid: Integer; const snd: string);
begin
  if (secid >= 0) and (secid < numsectors) then
    if snd <> '' then
      _playsound(Pmobj_t(@sectors[secid].soundorg), snd);
end;

procedure PS_SectorMoveZ(const sec: Integer; const dz: Integer);
var
  p: Psector_t;
begin
  if (sec >= 0) and (sec < numsectors) then
  begin
    p := @sectors[sec];
    p.floorheight := p.floorheight + dz;
    p.ceilingheight := p.ceilingheight + dz;
    P_ChangeSector(p, false);
  end;
end;

// ----------------------- TRTLSector ------------------------------------------

procedure TRTLSector.PlaySound(const snd: string);
begin
  PS_SectorPlaySound(Integer(self) - 1, snd);
end;

procedure TRTLSector.MoveZ(const dz: Integer);
begin
  PS_SectorMoveZ(Integer(self) - 1, dz);
end;

// ---------------------- TRTLSectors ------------------------------------------

function TRTLSectors.GetSector(id: Integer): TRTLSector;
begin
  if (id >= 0) and (id < numsectors) then
    Result := TRTLSector(id + 1)
  else
    Result := TRTLSector(SECTOR_INVALID);
end;

procedure TRTLSectorSlopeSector_R(Self: TRTLSector; var T: TRTLSector);
begin
  T := rtlsectors[PS_GetSectorSlopeSector(Integer(Self) - 1)];
end;

procedure TRTLSectorMidSector_R(Self: TRTLSector; var T: TRTLSector);
begin
  T := rtlsectors[PS_GetSectorMidSector(Integer(Self) - 1)];
end;

{$IFDEF DOOM_OR_STRIFE}
procedure TRTLSectorCeilingYOffset_W(Self: TRTLSector; const T: fixed_t);
begin
  PS_SetSectorCeilingYOffset(Integer(Self) - 1, T);
end;

procedure TRTLSectorCeilingYOffset_R(Self: TRTLSector; var T: fixed_t);
begin
  T := PS_GetSectorCeilingYOffset(Integer(Self) - 1);
end;

procedure TRTLSectorCeilingXOffset_W(Self: TRTLSector; const T: fixed_t);
begin
  PS_SetSectorCeilingXOffset(Integer(Self) - 1, T);
end;

procedure TRTLSectorCeilingXOffset_R(Self: TRTLSector; var T: fixed_t);
begin
  T := PS_GetSectorCeilingXOffset(Integer(Self) - 1);
end;

procedure TRTLSectorFloorYOffset_W(Self: TRTLSector; const T: fixed_t);
begin
  PS_SetSectorFloorYOffset(Integer(Self) - 1, T);
end;

procedure TRTLSectorFloorYOffset_R(Self: TRTLSector; var T: fixed_t);
begin
  T := PS_GetSectorFloorYOffset(Integer(Self) - 1);
end;

procedure TRTLSectorFloorXOffset_W(Self: TRTLSector; const T: fixed_t);
begin
  PS_SetSectorFloorXOffset(Integer(Self) - 1, T);
end;

procedure TRTLSectorFloorXOffset_R(Self: TRTLSector; var T: fixed_t);
begin
  T := PS_GetSectorFloorXOffset(Integer(Self) - 1);
end;
{$ENDIF}

procedure TRTLSectorNumLines_R(Self: TRTLSector; var T: Integer);
begin
  T := PS_GetSectorNumLines(Integer(Self) - 1);
end;

procedure TRTLSectorLines_R(Self: TRTLSector; var T: TDynamicIntegerArray);
begin
  T := PS_GetSectorLines(Integer(Self) - 1);
end;

procedure TRTLSectorNumActors_R(Self: TRTLSector; var T: Integer);
begin
  T := PS_GetSectorNumActors(Integer(Self) - 1);
end;

procedure TRTLSectorActors_R(Self: TRTLSector; var T: TActorArray);
begin
  T := PS_GetSectorActors(Integer(Self) - 1);
end;

procedure TRTLSectorTag_W(Self: TRTLSector; const T: Integer);
begin
  PS_SetSectorTag(Integer(Self) - 1, T);
end;

procedure TRTLSectorTag_R(Self: TRTLSector; var T: Integer);
begin
  T := PS_GetSectorTag(Integer(Self) - 1);
end;

procedure TRTLSectorSpecial_R(Self: TRTLSector; var T: Integer);
begin
  T := PS_GetSectorSpecial(Integer(Self) - 1);
end;

procedure TRTLSectorLightLevel_W(Self: TRTLSector; const T: Integer);
begin
  PS_SetSectorLightLevel(Integer(Self) - 1, T);
end;

procedure TRTLSectorLightLevel_R(Self: TRTLSector; var T: Integer);
begin
  T := PS_GetSectorLightLevel(Integer(Self) - 1);
end;

procedure TRTLSectorCeilingPicture_W(Self: TRTLSector; const T: string);
begin
  PS_SetSectorCeilingPicture(Integer(Self) - 1, T);
end;

procedure TRTLSectorCeilingPicture_R(Self: TRTLSector; var T: string);
begin
  T := PS_GetSectorCeilingPicture(Integer(Self) - 1);
end;

procedure TRTLSectorFloorPicture_W(Self: TRTLSector; const T: string);
begin
  PS_SetSectorFloorPicture(Integer(Self) - 1, T);
end;

procedure TRTLSectorFloorPicture_R(Self: TRTLSector; var T: string);
begin
  T := PS_GetSectorFloorPicture(Integer(Self) - 1);
end;

procedure TRTLSectorCeilingHeight_W(Self: TRTLSector; const T: fixed_t);
begin
  PS_SetSectorCeilingHeight(Integer(Self) - 1, T);
end;

procedure TRTLSectorCeilingHeight_R(Self: TRTLSector; var T: fixed_t);
begin
  T := PS_GetSectorCeilingHeight(Integer(Self) - 1);
end;

procedure TRTLSectorFloorHeight_W(Self: TRTLSector; const T: fixed_t);
begin
  PS_SetSectorFloorHeight(Integer(Self) - 1, T);
end;

procedure TRTLSectorFloorHeight_R(Self: TRTLSector; var T: fixed_t);
begin
  T := PS_GetSectorFloorHeight(Integer(Self) - 1);
end;

procedure TRTLSectorID_R(Self: TRTLSector; var T: Integer);
begin
  T := Integer(Self) - 1;
  if (T < 0) or (T >= numsectors) then
    T := SECTOR_INVALID;
end;

procedure TRTLSectorsCount_R(Self: TRTLSectors; var T: Integer);
begin
  T := numsectors;
end;

procedure TRTLSectorsSector_R(Self: TRTLSectors; var T: TRTLSector; const t1: Integer);
begin
  T := Self[t1];
end;

// --------------------------- PLAYERS -----------------------------------------

function PS_PlayerInGame(const plnum: Integer): boolean;
begin
  if plnum >= 0 then
    if plnum < MAXPLAYERS then
    begin
      Result := playeringame[plnum];
      Exit;
    end;
  Result := False;
end;

{$IFDEF DOOM_OR_STRIFE}
procedure PS_PlayerFaceMobj(const plnum: Integer; const actor: LongWord; const ticks: Integer);
var
  mo: Pmobj_t;
begin
  if not PS_PlayerInGame(plnum) then
    Exit;

  if ticks < 0 then
    Exit;

  mo := mobj_from_key(actor);
  if mo = nil then
    Exit;

  P_PlayerFaceMobj(@players[plnum], mo, ticks);
end;
{$ENDIF}

{$IFDEF DOOM_OR_STRIFE}
procedure PS_SetPlayerHasCard(const plnum: Integer; const card: Integer; const value: boolean);
begin
  if not PS_PlayerInGame(plnum) then
    Exit;

  if (card < 0) or (card >= Ord(NUMCARDS)) then
    Exit;

  players[plnum].cards[card] := value;
end;

function PS_GetPlayerHasCard(const plnum: Integer; const card: Integer): boolean;
begin
  if not PS_PlayerInGame(plnum) then
  begin
    Result := False;
    Exit;
  end;

  if (card < 0) or (card >= Ord(NUMCARDS)) then
  begin
    Result := False;
    Exit;
  end;

  Result := players[plnum].cards[card];
end;
{$ENDIF}

{$IFDEF HERETIC_OR_HEXEN}
procedure PS_SetPlayerHasKey(const plnum: Integer; const key: Integer; const value: boolean);
begin
  if not PS_PlayerInGame(plnum) then
    Exit;

  if (key < 0) or (key >= Ord(NUMKEYCARDS)) then
    Exit;

  {$IFDEF HERETIC}
  players[plnum].keys[key] := value;
  {$ENDIF}
  {$IFDEF HEXEN}
  if value then
    players[plnum].keys := players[plnum].keys or (1 shl key)
  else
    players[plnum].keys := players[plnum].keys and not (1 shl key);
  {$ENDIF}
end;

function PS_GetPlayerHasKey(const plnum: Integer; const key: Integer): boolean;
begin
  if not PS_PlayerInGame(plnum) then
  begin
    Result := False;
    Exit;
  end;

  if (key < 0) or (key >= Ord(NUMKEYCARDS)) then
  begin
    Result := False;
    Exit;
  end;

  {$IFDEF HERETIC}
  Result := players[plnum].keys[key];
  {$ENDIF}
  {$IFDEF HEXEN}
  Result := players[plnum].keys and (1 shl key) <> 0;
  {$ENDIF}
end;
{$ENDIF}

procedure PS_SetPlayerHasWeapon(const plnum: Integer; const weapon: Integer; const value: boolean);
begin
  if not PS_PlayerInGame(plnum) then
    Exit;

  if (weapon < 0) or (weapon >= Ord(NUMWEAPONS)) then
    Exit;

  players[plnum].weaponowned[weapon] := {$IFDEF DOOM_OR_HERETIC}1{$ELSE}true{$ENDIF};
end;

function PS_GetPlayerHasWeapon(const plnum: Integer; const weapon: Integer): boolean;
begin
  if not PS_PlayerInGame(plnum) then
  begin
    Result := False;
    Exit;
  end;

  if (weapon < 0) or (weapon >= Ord(NUMWEAPONS)) then
  begin
    Result := False;
    Exit;
  end;

  Result := players[plnum].weaponowned[weapon]{$IFDEF DOOM_OR_HERETIC} <> 0{$ENDIF};
end;

{$IFNDEF HEXEN}
procedure PS_SetPlayerAmmo(const plnum: Integer; const ammotype: Integer; const value: Integer);
begin
  if not PS_PlayerInGame(plnum) then
    Exit;

  if (ammotype < 0) or (ammotype >= Ord(NUMAMMO)) then
    Exit;

  players[plnum].ammo[ammotype] := value;
end;

function PS_GetPlayerAmmo(const plnum: Integer; const ammotype: Integer): Integer;
begin
  if not PS_PlayerInGame(plnum) then
  begin
    Result := 0;
    Exit;
  end;

  if (ammotype < 0) or (ammotype >= Ord(NUMAMMO)) then
  begin
    Result := 0;
    Exit;
  end;

  Result := players[plnum].ammo[ammotype];
end;
{$ENDIF}

{$IFDEF HEXEN}
procedure PS_SetPlayerMana(const plnum: Integer; const mana: Integer; const value: Integer);
begin
  if not PS_PlayerInGame(plnum) then
    Exit;

  if (mana < 0) or (mana >= Ord(NUMMANA)) then
    Exit;

  players[plnum].mana[mana] := value;
end;

function PS_GetPlayerMana(const plnum: Integer; const mana: Integer): Integer;
begin
  if not PS_PlayerInGame(plnum) then
  begin
    Result := 0;
    Exit;
  end;

  if (mana < 0) or (mana >= Ord(NUMMANA)) then
  begin
    Result := 0;
    Exit;
  end;

  Result := players[plnum].mana[mana];
end;
{$ENDIF}

procedure PS_SetPlayerMessage(const plnum: Integer; const msg: string);
begin
  if not PS_PlayerInGame(plnum) then
    Exit;

  players[plnum]._message := msg;
end;

function PS_GetPlayerMessage(const plnum: Integer): string;
begin
  if not PS_PlayerInGame(plnum) then
  begin
    Result := '';
    Exit;
  end;

  Result := players[plnum]._message;
end;

procedure PS_SetPlayerExtraLight(const plnum: Integer; const l: Integer);
begin
  if not PS_PlayerInGame(plnum) then
    Exit;

  players[plnum].extralight := l;
end;

function PS_GetPlayerExtraLight(const plnum: Integer): Integer;
begin
  if not PS_PlayerInGame(plnum) then
  begin
    Result := 0;
    Exit;
  end;

  Result := players[plnum].extralight;
end;

procedure PS_SetPlayerPowerTicks(const plnum: Integer; const powertype: Integer; const ticks: Integer);
begin
  if not PS_PlayerInGame(plnum) then
    Exit;

  if (powertype < 0) or (powertype >= Ord(NUMPOWERS)) then
    Exit;

  players[plnum].powers[powertype] := ticks;
end;


function PS_GetPlayerPowerTicks(const plnum: Integer; const powertype: Integer): Integer;
begin
  if not PS_PlayerInGame(plnum) then
  begin
    Result := 0;
    Exit;
  end;

  if (powertype < 0) or (powertype >= Ord(NUMPOWERS)) then
  begin
    Result := 0;
    Exit;
  end;

  Result := players[plnum].powers[powertype];
end;

function PS_GetPlayerViewZ(const plnum: Integer): Integer;
begin
  if not PS_PlayerInGame(plnum) then
  begin
    Result := 0;
    Exit;
  end;

  Result := players[plnum].viewz;
end;

function PS_GetPlayerViewHeight(const plnum: Integer): Integer;
begin
  if not PS_PlayerInGame(plnum) then
  begin
    Result := 0;
    Exit;
  end;

  Result := players[plnum].viewheight;
end;

function PS_GetPlayerActor(const plnum: Integer): LongWord;
begin
  if not PS_PlayerInGame(plnum) then
  begin
    Result := ACTOR_INVALID;
    Exit;
  end;

  if players[plnum].mo = nil then
  begin
    Result := ACTOR_INVALID;
    Exit;
  end;

  try
    Result := players[plnum].mo.key;
  except
    Result := ACTOR_INVALID;
  end;
end;

function PS_ConsolePlayer: Integer;
begin
  Result := consoleplayer;
end;

// -------------------------- TEXTURES -----------------------------------------

function PS_IsValidTexture(const tex: string): boolean;
var
  texid: Integer;
begin
  texid := _textureidfromname(tex);
  Result := (texid > 0) and (texid < numtextures);
end;

function PS_GetTextureWidth(const tex: string): Integer;
var
  texid: Integer;
begin
  texid := _textureidfromname(tex);
  if (texid > 0) and (texid < numtextures) then
    Result := textures[texid].width
  else
    Result := 0;
end;

function PS_GetTextureHeight(const tex: string): Integer;
var
  texid: Integer;
begin
  texid := _textureidfromname(tex);
  if (texid > 0) and (texid < numtextures) then
    Result := textures[texid].height
  else
    Result := 0;
end;

// ------------------------------ GAME -----------------------------------------

{$IFDEF HEXEN}
function PS_GameMap: integer;
begin
  Result := gamemap;
end;
{$ENDIF}

function PS_Game: string;
begin
  Result := _GAME;
end;

procedure PS_GlobalEarthQuake(const tics: integer);
var
  qtics: integer;
  i: integer;
begin
  qtics := tics * FRACUNIT;
  for i := 0 to MAXPLAYERS - 1 do
    if playeringame[i] then
      players[i].quaketics := qtics;
end;

function PS_GameSkill: integer;
begin
  Result := Ord(gameskill);
end;

// ---------------------- REGISTRATION -----------------------------------------

// Compiler Registration
procedure SIRegister_Game(C: TPSPascalCompiler);
var
  cactor: TPSCompileTimeClass;
  cactors: TPSCompileTimeClass;
  cvertex: TPSCompileTimeClass;
  cvertexes: TPSCompileTimeClass;
  cside: TPSCompileTimeClass;
  csides: TPSCompileTimeClass;
  cline: TPSCompileTimeClass;
  clines: TPSCompileTimeClass;
  csector: TPSCompileTimeClass;
  csectors: TPSCompileTimeClass;
begin
  cactor := C.AddClassN(C.FindClass('!TOBJECT'), '!TActor');
  cactors := C.AddClassN(C.FindClass('!TOBJECT'), '!TActors');
  cvertex := C.AddClassN(C.FindClass('!TOBJECT'), '!TVertex');
  cvertexes := C.AddClassN(C.FindClass('!TOBJECT'), '!TVertexes');
  cside := C.AddClassN(C.FindClass('!TOBJECT'), '!TSide');
  csides := C.AddClassN(C.FindClass('!TOBJECT'), '!TSides');
  cline := C.AddClassN(C.FindClass('!TOBJECT'), '!TLine');
  clines := C.AddClassN(C.FindClass('!TOBJECT'), '!TLines');
  csector := C.AddClassN(C.FindClass('!TOBJECT'), '!TSector');
  csectors := C.AddClassN(C.FindClass('!TOBJECT'), '!TSectors');

  cactor.RegisterProperty('key', 'LongWord', iptR);
  cactor.RegisterProperty('Target', '!TActor', iptRW);
  cactor.RegisterProperty('X', 'fixed_t', iptRW);
  cactor.RegisterProperty('Y', 'fixed_t', iptRW);
  cactor.RegisterProperty('Z', 'fixed_t', iptRW);
  cactor.RegisterProperty('MOMX', 'fixed_t', iptRW);
  cactor.RegisterProperty('MOMY', 'fixed_t', iptRW);
  cactor.RegisterProperty('MOMZ', 'fixed_t', iptRW);
  cactor.RegisterProperty('FloorZ', 'fixed_t', iptRW);
  cactor.RegisterProperty('CeilingZ', 'fixed_t', iptRW);
  cactor.RegisterProperty('Speed', 'fixed_t', iptRW);
  cactor.RegisterProperty('Angle', 'angle_t', iptRW);
  cactor.RegisterProperty('Sector', '!TSector', iptR);
  cactor.RegisterProperty('Health', 'Integer', iptRW);
  cactor.RegisterProperty('SpawnHealth', 'Integer', iptR);
  cactor.RegisterProperty('Mass', 'Integer', iptR);
  cactor.RegisterProperty('Height', 'fixed_t', iptRW);
  cactor.RegisterProperty('CustomDropItem', 'Integer', iptRW);
  cactor.RegisterProperty('CustomParams', 'Integer String', iptRW);
  cactor.RegisterProperty('Flag', 'Boolean LongWord', iptRW);
  cactor.RegisterProperty('Name', 'String', iptR);
  {$IFDEF STRIFE}
  cactor.RegisterProperty('Name2', 'String', iptR);
  {$ENDIF}
  cactor.RegisterProperty('Player', 'Integer', iptR);
  cactor.RegisterProperty('MobjType', 'Integer', iptR);
  cactor.RegisterProperty('EditorNumber', 'Integer', iptR);
  cactor.RegisterProperty('SeeSound', 'string', iptR);
  cactor.RegisterProperty('AttackSound', 'string', iptR);
  cactor.RegisterProperty('PainSound', 'string', iptR);
  cactor.RegisterProperty('DeathSound', 'string', iptR);
  cactor.RegisterProperty('ActiveSound', 'string', iptR);
  cactor.RegisterProperty('CustomSound1', 'string', iptR);
  cactor.RegisterProperty('CustomSound2', 'string', iptR);
  cactor.RegisterProperty('CustomSound3', 'string', iptR);
  cactor.RegisterProperty('MeleeSound', 'string', iptR);

  cactor.RegisterMethod('procedure PlaySound(const snd: string);');
  cactor.RegisterMethod('procedure Remove;');

  cactors.RegisterMethod('function AllActors: TActorArray;');
  cactors.RegisterMethod('function AllMonstersAlive: TActorArray;');
  cactors.RegisterMethod('function AllMonstersDead: TActorArray;');
  cactors.RegisterMethod('function AllMonsters: TActorArray;');
  cactors.RegisterMethod('function AllAtSector(const sec: Integer): TActorArray;');
  cactors.RegisterMethod('function AllAtSectorTag(const tag: Integer): TActorArray;');
  cactors.RegisterMethod('function AllEditorNumber(const dn: Integer): TActorArray;');
  cactors.RegisterMethod('function AllType(const typ: Integer): TActorArray;');
  cactors.RegisterProperty('Actor', '!TActor LongWord', iptR);
  cactors.SetDefaultPropery('Actor');

  cvertex.RegisterProperty('X', 'Integer', iptR);
  cvertex.RegisterProperty('Y', 'Integer', iptR);
  cvertex.RegisterProperty('ID', 'Integer', iptR);

  cvertexes.RegisterProperty('Vertex', '!TVertex Integer', iptR);
  cvertexes.SetDefaultPropery('Vertex');
  cvertexes.RegisterProperty('Count', 'Integer', iptR);

  cside.RegisterProperty('TextureOffset', 'fixed_t', iptRW);
  cside.RegisterProperty('RowOffset', 'fixed_t', iptRW);
  cside.RegisterProperty('TopTexture', 'string', iptRW);
  cside.RegisterProperty('BottomTexture', 'string', iptRW);
  cside.RegisterProperty('MiddleTexture', 'string', iptRW);
  cside.RegisterProperty('Sector', '!TSector', iptR);
  cside.RegisterProperty('ID', 'Integer', iptR);

  csides.RegisterProperty('Side', '!TSide Integer', iptR);
  csides.SetDefaultPropery('Side');
  csides.RegisterProperty('Count', 'Integer', iptR);

  cline.RegisterProperty('Vertex1', '!TVertex', iptR);
  cline.RegisterProperty('Vertex2', '!TVertex', iptR);
  cline.RegisterProperty('DX', 'fixed_t', iptR);
  cline.RegisterProperty('DY', 'fixed_t', iptR);
  cline.RegisterProperty('Special', 'Integer', iptRW);
  {$IFDEF HEXEN}
  cline.RegisterProperty('Arg1', 'byte', iptRW);
  cline.RegisterProperty('Arg2', 'byte', iptRW);
  cline.RegisterProperty('Arg3', 'byte', iptRW);
  cline.RegisterProperty('Arg4', 'byte', iptRW);
  cline.RegisterProperty('Arg5', 'byte', iptRW);
  {$ELSE}
  cline.RegisterProperty('Tag', 'Integer', iptRW);
  cline.RegisterProperty('Transparent', 'Boolean', iptRW);
  {$ENDIF}
  cline.RegisterProperty('IsBlocking', 'Boolean', iptRW);
  cline.RegisterProperty('IsBlockingMonsters', 'Boolean', iptRW);
  cline.RegisterProperty('TriggerScripts', 'Boolean', iptRW);
  cline.RegisterProperty('FrontSide', '!TSide', iptR);
  cline.RegisterProperty('BackSide', '!TSide', iptR);
  cline.RegisterProperty('FrontSector', '!TSector', iptR);
  cline.RegisterProperty('BackSector', '!TSector', iptR);
  cline.RegisterProperty('ID', 'Integer', iptR);

  clines.RegisterProperty('Line', '!TLine Integer', iptR);
  clines.SetDefaultPropery('Line');
  clines.RegisterProperty('Count', 'Integer', iptR);

  csector.RegisterProperty('FloorHeight', 'fixed_t', iptRW);
  csector.RegisterProperty('CeilingHeight', 'fixed_t', iptRW);
  csector.RegisterProperty('FloorPicture', 'string', iptRW);
  csector.RegisterProperty('CeilingPicture', 'string', iptRW);
  csector.RegisterProperty('LightLevel', 'Integer', iptRW);
  csector.RegisterProperty('Special', 'Integer', iptR);
  csector.RegisterProperty('Tag', 'Integer', iptRW);
  csector.RegisterProperty('Actors', 'TActorArray', iptR);
  csector.RegisterProperty('NumActors', 'Integer', iptR);
  csector.RegisterProperty('Lines', 'TIntegerArray', iptR);
  csector.RegisterProperty('NumLines', 'Integer', iptR);
  {$IFDEF DOOM_OR_STRIFE}
  csector.RegisterProperty('FloorXOffset', 'fixed_t', iptRW);
  csector.RegisterProperty('FloorYOffset', 'fixed_t', iptRW);
  csector.RegisterProperty('CeilingXOffset', 'fixed_t', iptRW);
  csector.RegisterProperty('CeilingYOffset', 'fixed_t', iptRW);
  {$ENDIF}
  csector.RegisterProperty('MidSector', '!TSector', iptR);
  csector.RegisterProperty('SlopeSector', '!TSector', iptR);
  csector.RegisterProperty('ID', 'Integer', iptR);
  csector.RegisterMethod('procedure PlaySound(const snd: string);');
  csector.RegisterMethod('procedure MoveZ(const dz: fixed_t);');

  csectors.RegisterProperty('Sector', '!TSector Integer', iptR);
  csectors.SetDefaultPropery('Sector');
  csectors.RegisterProperty('Count', 'Integer', iptR);

  AddImportedClassVariable(C, 'Actors', '!TActors');
  AddImportedClassVariable(C, 'Vertexes', '!TVertexes');
  AddImportedClassVariable(C, 'Sides', '!TSides');
  AddImportedClassVariable(C, 'Lines', '!TLines');
  AddImportedClassVariable(C, 'Sectors', '!TSectors');
end;

// Runtime Registration
procedure RIRegister_Game(CLI: TPSRuntimeClassImporter);
var
  ractor: TPSRuntimeClass;
  ractors: TPSRuntimeClass;
  rvertex: TPSRuntimeClass;
  rvertexes: TPSRuntimeClass;
  rside: TPSRuntimeClass;
  rsides: TPSRuntimeClass;
  rline: TPSRuntimeClass;
  rlines: TPSRuntimeClass;
  rsector: TPSRuntimeClass;
  rsectors: TPSRuntimeClass;
begin
  ractor := CLI.Add2(TRTLActor, '!TACTOR');
  ractors := CLI.Add2(TRTLActors, '!TACTORS');
  rvertex := CLI.Add2(TRTLVertex, '!TVERTEX');
  rvertexes := CLI.Add2(TRTLVertexes, '!TVERTEXES');
  rside := CLI.Add2(TRTLSide, '!TSIDE');
  rsides := CLI.Add2(TRTLSides, '!TSIDES');
  rline := CLI.Add2(TRTLLine, '!TLINE');
  rlines := CLI.Add2(TRTLLines, '!TLINES');
  rsector := CLI.Add2(TRTLLine, '!TSECTOR');
  rsectors := CLI.Add2(TRTLLines, '!TSECTORS');

  ractor.RegisterPropertyHelper(@TRTLActorkey_R, nil, 'key');
  ractor.RegisterPropertyHelper(@TRTLActorTarget_R, @TRTLActorTarget_W, 'Target');
  ractor.RegisterPropertyHelper(@TRTLActorX_R, @TRTLActorX_W, 'X');
  ractor.RegisterPropertyHelper(@TRTLActorY_R, @TRTLActorY_W, 'Y');
  ractor.RegisterPropertyHelper(@TRTLActorZ_R, @TRTLActorZ_W, 'Z');
  ractor.RegisterPropertyHelper(@TRTLActorMOMX_R, @TRTLActorMOMX_W, 'MOMX');
  ractor.RegisterPropertyHelper(@TRTLActorMOMY_R, @TRTLActorMOMY_W, 'MOMY');
  ractor.RegisterPropertyHelper(@TRTLActorMOMZ_R, @TRTLActorMOMZ_W, 'MOMZ');
  ractor.RegisterPropertyHelper(@TRTLActorFloorZ_R, @TRTLActorFloorZ_W, 'FloorZ');
  ractor.RegisterPropertyHelper(@TRTLActorCeilingZ_R, @TRTLActorCeilingZ_W, 'CeilingZ');
  ractor.RegisterPropertyHelper(@TRTLActorSpeed_R, @TRTLActorSpeed_W, 'Speed');
  ractor.RegisterPropertyHelper(@TRTLActorAngle_R, @TRTLActorAngle_W, 'Angle');
  ractor.RegisterPropertyHelper(@TRTLActorSector_R, nil, 'Sector');
  ractor.RegisterPropertyHelper(@TRTLActorHealth_R, @TRTLActorHealth_W, 'Health');
  ractor.RegisterPropertyHelper(@TRTLActorSpawnHealth_R, nil, 'SpawnHealth');
  ractor.RegisterPropertyHelper(@TRTLActorMass_R, nil, 'Mass');
  ractor.RegisterPropertyHelper(@TRTLActorHeight_R, @TRTLActorHeight_W, 'Height');
  ractor.RegisterPropertyHelper(@TRTLActorCustomDropItem_R, @TRTLActorCustomDropItem_W, 'CustomDropItem');
  ractor.RegisterPropertyHelper(@TRTLActorCustomParams_R, @TRTLActorCustomParams_W, 'CustomParams');
  ractor.RegisterPropertyHelper(@TRTLActorFlags_R, @TRTLActorFlags_W, 'Flag');
  ractor.RegisterPropertyHelper(@TRTLActorName_R, nil, 'Name');
  {$IFDEF STRIFE}
  ractor.RegisterPropertyHelper(@TRTLActorName2_R, nil, 'Name2');
  {$ENDIF}
  ractor.RegisterPropertyHelper(@TRTLActorPlayer_R, nil, 'Player');
  ractor.RegisterPropertyHelper(@TRTLActorMobjType_R, nil, 'MobjType');
  ractor.RegisterPropertyHelper(@TRTLActorEditorNumber_R, nil, 'EditorNumber');
  ractor.RegisterPropertyHelper(@TRTLActorSeeSound_R, nil, 'SeeSound');
  ractor.RegisterPropertyHelper(@TRTLActorAttackSound_R, nil, 'AttackSound');
  ractor.RegisterPropertyHelper(@TRTLActorPainSound_R, nil, 'PainSound');
  ractor.RegisterPropertyHelper(@TRTLActorDeathSound_R, nil, 'DeathSound');
  ractor.RegisterPropertyHelper(@TRTLActorActiveSound_R, nil, 'ActiveSound');
  ractor.RegisterPropertyHelper(@TRTLActorCustomSound1_R, nil, 'CustomSound1');
  ractor.RegisterPropertyHelper(@TRTLActorCustomSound2_R, nil, 'CustomSound2');
  ractor.RegisterPropertyHelper(@TRTLActorCustomSound3_R, nil, 'CustomSound3');
  ractor.RegisterPropertyHelper(@TRTLActorMeleeSound_R, nil, 'MeleeSound');
  ractor.RegisterMethod(@TRTLActor.PlaySound, 'PlaySound');
  ractor.RegisterMethod(@TRTLActor.Remove, 'Remove');

  ractors.RegisterMethod(@TRTLActors.AllActors, 'AllActors');
  ractors.RegisterMethod(@TRTLActors.AllMonstersAlive, 'AllMonstersAlive');
  ractors.RegisterMethod(@TRTLActors.AllMonstersDead, 'AllMonstersDead');
  ractors.RegisterMethod(@TRTLActors.AllMonsters, 'AllMonsters');
  ractors.RegisterMethod(@TRTLActors.AllAtSector, 'AllAtSector');
  ractors.RegisterMethod(@TRTLActors.AllAtSectorTag, 'AllAtSectorTag');
  ractors.RegisterMethod(@TRTLActors.AllEditorNumber, 'AllEditorNumber');
  ractors.RegisterMethod(@TRTLActors.AllType, 'AllType');
  ractors.RegisterPropertyHelper(@TRTLActorsActor_R, nil, 'actor');

  rvertex.RegisterPropertyHelper(@TRTLVertexX_R, nil, 'X');
  rvertex.RegisterPropertyHelper(@TRTLVertexY_R, nil, 'Y');
  rvertex.RegisterPropertyHelper(@TRTLVertexID_R, nil, 'ID');

  rvertexes.RegisterPropertyHelper(@TRTLVertexesVertex_R, nil, 'Vertex');
  rvertexes.RegisterPropertyHelper(@TRTLVertexesCount_R, nil, 'Count');

  rside.RegisterPropertyHelper(@TRTLSideTextureOffset_R, @TRTLSideTextureOffset_W, 'TextureOffset');
  rside.RegisterPropertyHelper(@TRTLSideRowOffset_R, @TRTLSideRowOffset_W, 'RowOffset');
  rside.RegisterPropertyHelper(@TRTLSideTopTexture_R, @TRTLSideTopTexture_W, 'TopTexture');
  rside.RegisterPropertyHelper(@TRTLSideBottomTexture_R, @TRTLSideBottomTexture_W, 'BottomTexture');
  rside.RegisterPropertyHelper(@TRTLSideMiddleTexture_R, @TRTLSideMiddleTexture_W, 'MiddleTexture');
  rside.RegisterPropertyHelper(@TRTLSideSector_R, nil, 'Sector');
  rside.RegisterPropertyHelper(@TRTLSideID_R, nil, 'ID');

  rsides.RegisterPropertyHelper(@TRTLSidesSides_R, nil, 'Side');
  rsides.RegisterPropertyHelper(@TRTLSidesCount_R, nil, 'Count');

  rline.RegisterPropertyHelper(@TRTLLineVertex1_R, nil, 'Vertex1');
  rline.RegisterPropertyHelper(@TRTLLineVertex2_R, nil, 'Vertex2');
  rline.RegisterPropertyHelper(@TRTLLineDX_R, nil, 'DX');
  rline.RegisterPropertyHelper(@TRTLLineDY_R, nil, 'DY');
  rline.RegisterPropertyHelper(@TRTLLineSpecial_R, @TRTLLineSpecial_W, 'Special');
  {$IFDEF HEXEN}
  rline.RegisterPropertyHelper(@TRTLLineArg1_R, @TRTLLineArg1_W, 'Arg1');
  rline.RegisterPropertyHelper(@TRTLLineArg2_R, @TRTLLineArg2_W, 'Arg2');
  rline.RegisterPropertyHelper(@TRTLLineArg3_R, @TRTLLineArg3_W, 'Arg3');
  rline.RegisterPropertyHelper(@TRTLLineArg4_R, @TRTLLineArg4_W, 'Arg4');
  rline.RegisterPropertyHelper(@TRTLLineArg5_R, @TRTLLineArg5_W, 'Arg5');
  {$ELSE}
  rline.RegisterPropertyHelper(@TRTLLineTag_R, @TRTLLineTag_W, 'Tag');
  rline.RegisterPropertyHelper(@TRTLLineTransparent_R, @TRTLLineTransparent_W, 'Transparent');
  {$ENDIF}
  rline.RegisterPropertyHelper(@TRTLLineIsBlocking_R, @TRTLLineIsBlocking_W, 'IsBlocking');
  rline.RegisterPropertyHelper(@TRTLLineIsBlockingMonsters_R, @TRTLLineIsBlockingMonsters_W, 'IsBlockingMonsters');
  rline.RegisterPropertyHelper(@TRTLLineTriggerScripts_R, @TRTLLineTriggerScripts_W, 'TriggerScripts');
  rline.RegisterPropertyHelper(@TRTLLineFrontSide_R, nil, 'FrontSide');
  rline.RegisterPropertyHelper(@TRTLLineBackSide_R, nil, 'BackSide');
  rline.RegisterPropertyHelper(@TRTLLineFrontSector_R, nil, 'FrontSector');
  rline.RegisterPropertyHelper(@TRTLLineBackSector_R, nil, 'BackSector');
  rline.RegisterPropertyHelper(@TRTLLineID_R, nil, 'ID');

  rlines.RegisterPropertyHelper(@TRTLLinesLine_R, nil, 'Line');
  rlines.RegisterPropertyHelper(@TRTLLinesCount_R, nil, 'Count');

  rsector.RegisterPropertyHelper(@TRTLSectorFloorHeight_R, @TRTLSectorFloorHeight_W, 'FloorHeight');
  rsector.RegisterPropertyHelper(@TRTLSectorCeilingHeight_R, @TRTLSectorCeilingHeight_W, 'CeilingHeight');
  rsector.RegisterPropertyHelper(@TRTLSectorFloorPicture_R, @TRTLSectorFloorPicture_W, 'FloorPicture');
  rsector.RegisterPropertyHelper(@TRTLSectorCeilingPicture_R, @TRTLSectorCeilingPicture_W, 'CeilingPicture');
  rsector.RegisterPropertyHelper(@TRTLSectorLightLevel_R, @TRTLSectorLightLevel_W, 'LightLevel');
  rsector.RegisterPropertyHelper(@TRTLSectorSpecial_R, nil, 'Special');
  rsector.RegisterPropertyHelper(@TRTLSectorTag_R, @TRTLSectorTag_W, 'Tag');
  rsector.RegisterPropertyHelper(@TRTLSectorActors_R, nil, 'Actors');
  rsector.RegisterPropertyHelper(@TRTLSectorNumActors_R, nil, 'NumActors');
  rsector.RegisterPropertyHelper(@TRTLSectorLines_R, nil, 'Lines');
  rsector.RegisterPropertyHelper(@TRTLSectorNumLines_R, nil, 'NumLines');
  {$IFDEF DOOM_OR_STRIFE}
  rsector.RegisterPropertyHelper(@TRTLSectorFloorXOffset_R, @TRTLSectorFloorXOffset_W, 'FloorXOffset');
  rsector.RegisterPropertyHelper(@TRTLSectorFloorYOffset_R, @TRTLSectorFloorYOffset_W, 'FloorYOffset');
  rsector.RegisterPropertyHelper(@TRTLSectorCeilingXOffset_R, @TRTLSectorCeilingXOffset_W, 'CeilingXOffset');
  rsector.RegisterPropertyHelper(@TRTLSectorCeilingYOffset_R, @TRTLSectorCeilingYOffset_W, 'CeilingYOffset');
  {$ENDIF}
  rsector.RegisterPropertyHelper(@TRTLSectorMidSector_R, nil, 'MidSector');
  rsector.RegisterPropertyHelper(@TRTLSectorSlopeSector_R, nil, 'SlopeSector');
  rsector.RegisterPropertyHelper(@TRTLSectorID_R, nil, 'ID');
  rsector.RegisterMethod(@TRTLSector.PlaySound, 'PlaySound');
  rsector.RegisterMethod(@TRTLSector.MoveZ, 'MoveZ');

  rsectors.RegisterPropertyHelper(@TRTLSectorsSector_R, nil, 'Sector');
  rsectors.RegisterPropertyHelper(@TRTLSectorsCount_R, nil, 'Count');
end;

procedure RIRegisterRTL_Game(Exec: TPSExec);
begin
  SetVariantToClass(Exec.GetVarNo(Exec.GetVar('Actors')), rtlactors);
  SetVariantToClass(Exec.GetVarNo(Exec.GetVar('Vertexes')), rtlvertexes);
  SetVariantToClass(Exec.GetVarNo(Exec.GetVar('Sides')), rtlsides);
  SetVariantToClass(Exec.GetVarNo(Exec.GetVar('Lines')), rtllines);
  SetVariantToClass(Exec.GetVarNo(Exec.GetVar('Sectors')), rtlsectors);
end;

procedure PS_InitGameImport;
begin
  rtlactors := TRTLActors.Create;
  rtlvertexes := TRTLVertexes.Create;
  rtlsides := TRTLSides.Create;
  rtllines := TRTLLines.Create;
  rtlsectors := TRTLSectors.Create;
end;

procedure PS_ShutDownGameImport;
begin
  rtlactors.Free;
  rtlvertexes.Free;
  rtlsides.Free;
  rtllines.Free;
  rtlsectors.Free;
end;

end.

