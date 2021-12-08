//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2021 by Jim Valavanis
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
//  Actors - Vertexes - Lines - Sides - Sectors - Players - Mobjinfo
//
//------------------------------------------------------------------------------
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
  ps_compiler,
  ps_runtime;

// ---------------------------- ACTORS -----------------------------------------

function PS_KeyFromMobj(const mo: Pmobj_t): LongWord;

var
  ps_currentactor: Pmobj_t = nil;

function PS_Actor: LongWord;

function PS_TActor(const key: LongWord): LongWord;

function PS_GetActorTarget(const key: LongWord): LongWord;
procedure PS_SetActorTarget(const key: LongWord; const targ: LongWord);

function PS_GetActorTracer(const key: LongWord): LongWord;
procedure PS_SetActorTracer(const key: LongWord; const trac: LongWord);

function PS_GetActorMaster(const key: LongWord): LongWord;
procedure PS_SetActorMaster(const key: LongWord; const mst: LongWord);

function PS_GetActorX(const key: LongWord): Integer;
procedure PS_SetActorX(const key: LongWord; const x: Integer);

function PS_GetActorY(const key: LongWord): Integer;
procedure PS_SetActorY(const key: LongWord; const y: Integer);

function PS_GetActorZ(const key: LongWord): Integer;
procedure PS_SetActorZ(const key: LongWord; const z: Integer);

procedure PS_SetActorPosition(const key: LongWord; const x, y, z: Integer);

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
procedure PS_SetActorMass(const key: LongWord; const m: Integer);

function PS_GetActorHeight(const key: LongWord): Integer;
procedure PS_SetActorHeight(const key: LongWord; const h: Integer);

function PS_GetActorCustomParam(const key: LongWord; const parm: string): Integer;
procedure PS_SetActorCustomParam(const key: LongWord; const parm: string; const value: Integer);

function PS_GetActorCustomDropItem(const key: LongWord): Integer;
procedure PS_SetActorCustomDropItem(const key: LongWord; const value: Integer);
procedure PS_SetActorDefaultDropItem(const key: LongWord);

function PS_CheckActorFlag(const key: LongWord; const flag: LongWord): Boolean;
procedure PS_SetActorFlag(const key: LongWord; const flag: LongWord);
procedure PS_UnSetActorFlag(const key: LongWord; const flag: LongWord);

function PS_GetActorScale(const key: LongWord): Integer;
procedure PS_SetActorScale(const key: LongWord; const value: Integer);

function PS_GetActorPushFactor(const key: LongWord): Integer;
procedure PS_SetActorPushFactor(const key: LongWord; const value: Integer);

function PS_GetActorGravity(const key: LongWord): Integer;
procedure PS_SetActorGravity(const key: LongWord; const value: Integer);

function PS_GetActorSpecial(const key: LongWord): Integer;
procedure PS_SetActorSpecial(const key: LongWord; const value: Integer);

function PS_GetActorArg1(const key: LongWord): Integer;
procedure PS_SetActorArg1(const key: LongWord; const value: Integer);

function PS_GetActorArg2(const key: LongWord): Integer;
procedure PS_SetActorArg2(const key: LongWord; const value: Integer);

function PS_GetActorArg3(const key: LongWord): Integer;
procedure PS_SetActorArg3(const key: LongWord; const value: Integer);

function PS_GetActorArg4(const key: LongWord): Integer;
procedure PS_SetActorArg4(const key: LongWord; const value: Integer);

function PS_GetActorArg5(const key: LongWord): Integer;
procedure PS_SetActorArg5(const key: LongWord; const value: Integer);

function PS_GetActorWeaveIndexXY(const key: LongWord): Integer;
procedure PS_SetActorWeaveIndexXY(const key: LongWord; const value: Integer);

function PS_GetActorWeaveIndexZ(const key: LongWord): Integer;
procedure PS_SetActorWeaveIndexZ(const key: LongWord; const value: Integer);

function PS_GetActorFriction(const key: LongWord): Integer;
procedure PS_SetActorFriction(const key: LongWord; const value: Integer);

function PS_GetActorPainChance(const key: LongWord): Integer;
procedure PS_SetActorPainChance(const key: LongWord; const value: Integer);

function PS_GetActorSpriteDX(const key: LongWord): Integer;
procedure PS_SetActorSpriteDX(const key: LongWord; const value: Integer);

function PS_GetActorSpriteDY(const key: LongWord): Integer;
procedure PS_SetActorSpriteDY(const key: LongWord; const value: Integer);

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

function PS_GetActorState(const key: LongWord): integer;
procedure PS_SetActorState(const key: LongWord; const value: Integer);

function PS_IsValidActor(const key: LongWord): Boolean;

procedure PS_ActorPlaySound(const key: LongWord; const snd: string);

procedure PS_PlaySound(const snd: string);

function PS_SpawnActorType(x, y, z: Integer; const typ: Integer): LongWord;

function PS_SpawnActorEditorNumber(x, y, z: Integer; const ednum: Integer): LongWord;

function PS_SpawnActorName(x, y, z: Integer; const name: string): LongWord;

procedure PS_RemoveActor(const key: LongWord);

function PS_CheckActorSight(const key1, key2: LongWord): Boolean;

function PS_ActorTypeFromEditorNumber(const ednum: Integer): Integer;

type
  TActorArray = array of LongWord;

  TActorKeyList = class(TObject)
  private
    fList: PLongWordArray;
    fNumItems: Integer;
    fRealSize: Integer;
    fAllowDuplicates: Boolean;
    function FormatName(const name: string): string;
  protected
    function GetActor(Index: Integer): LongWord; virtual;
    procedure PutActor(Index: Integer; const value: LongWord); virtual;
    procedure SetAllowDuplicates(const value: Boolean); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(const value: LongWord);
    function Delete(const item: LongWord): Boolean;
    function Exists(const value: LongWord): Boolean;
    procedure AddAllActors;
    procedure AddAllMonstersAlive;
    procedure AddAllMonstersDead;
    procedure AddAllMonsters;
    procedure AddAllMissiles;
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

function PS_P_PointOnLineSide(x: Integer; y: Integer; line: integer): Integer;

function PS_R_PointInSector(const x: Integer; const y: Integer): Integer;

function PS_R_PointInSubSector(const x: Integer; const y: Integer): Integer;

// -------------------------- VERTEXES -----------------------------------------

function PS_TVertex(const id: Integer): Integer;

function PS_GetVertexX(const v: Integer): Integer;

function PS_GetVertexY(const v: Integer): Integer;

function PS_IsValidVertex(const v: Integer): Boolean;

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

function PS_IsValidSide(const sd: Integer): Boolean;

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

function PS_IsValidLine(const ld: Integer): Boolean;

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

function PS_GetSectorFloorAngle(const sec: Integer): LongWord;
procedure PS_SetSectorFloorAngle(const sec: Integer; const ang: LongWord);

function PS_GetSectorFloorAngleX(const sec: Integer): Integer;
procedure PS_SetSectorFloorAngleX(const sec: Integer; const angx: Integer);

function PS_GetSectorFloorAngleY(const sec: Integer): Integer;
procedure PS_SetSectorFloorAngleY(const sec: Integer; const angy: Integer);

function PS_GetSectorCeilingAngle(const sec: Integer): LongWord;
procedure PS_SetSectorCeilingAngle(const sec: Integer; const ang: LongWord);

function PS_GetSectorCeilingAngleX(const sec: Integer): Integer;
procedure PS_SetSectorCeilingAngleX(const sec: Integer; const angx: Integer);

function PS_GetSectorCeilingAngleY(const sec: Integer): Integer;
procedure PS_SetSectorCeilingAngleY(const sec: Integer; const angy: Integer);

function PS_GetSectorRippleFloor(const sec: Integer): Boolean;
procedure PS_SetSectorRippleFloor(const sec: Integer; const rpl: Boolean);

function PS_GetSectorRippleCeiling(const sec: Integer): Boolean;
procedure PS_SetSectorRippleCeiling(const sec: Integer; const rpl: Boolean);

function PS_GetSectorInterpolate(const sec: Integer): Boolean;
procedure PS_SetSectorInterpolate(const sec: Integer; const intpl: Boolean);

function PS_GetSectorFog(const sec: Integer): Boolean;
procedure PS_SetSectorFog(const sec: Integer; const fog: Boolean);

// JVAL: sector gravity (VERSION 204)
function PS_GetSectorGravity(const sec: Integer): Integer;
procedure PS_SetSectorGravity(const sec: Integer; const grav: Integer);

function PS_GetSectorMidSector(const sec: Integer): Integer;

function PS_GetSectorSlopeSector(const sec: Integer): Integer;

function PS_SkyPicture: string;

function PS_IsValidSector(const sec: Integer): Boolean;

function PS_GetSectorCount: Integer;

function PS_FindSectorsFromTag(const tag: integer): TDynamicIntegerArray;

procedure PS_SectorPlaySound(const secid: Integer; const snd: string);

// --------------------------- PLAYERS -----------------------------------------

function PS_PlayerInGame(const plnum: Integer): Boolean;

{$IFDEF DOOM_OR_STRIFE}
procedure PS_PlayerFaceMobj(const plnum: Integer; const actor: LongWord; const ticks: Integer);
{$ENDIF}

{$IFDEF DOOM_OR_STRIFE}
procedure PS_SetPlayerHasCard(const plnum: Integer; const card: Integer; const value: Boolean);

function PS_GetPlayerHasCard(const plnum: Integer; const card: Integer): Boolean;
{$ENDIF}

{$IFDEF HERETIC_OR_HEXEN}
procedure PS_SetPlayerHasKey(const plnum: Integer; const key: Integer; const value: Boolean);

function PS_GetPlayerHasKey(const plnum: Integer; const key: Integer): Boolean;

procedure PS_PlayerUseArtifact(const plnum: Integer; const arti: Integer);

function PS_GiveArtifactToPlayer(const plnum: Integer; const arti: Integer): Boolean;

function PS_CheckPlayerArtifact(const plnum: Integer; const arti: Integer): Integer;
{$ENDIF}

procedure PS_SetPlayerHasWeapon(const plnum: Integer; const weapon: Integer; const value: Boolean);

function PS_GetPlayerHasWeapon(const plnum: Integer; const weapon: Integer): Boolean;

{$IFNDEF HEXEN}
procedure PS_SetPlayerAmmo(const plnum: Integer; const ammotype: Integer; const value: Integer);

function PS_GetPlayerAmmo(const plnum: Integer; const ammotype: Integer): Integer;
{$ENDIF}

{$IFDEF HEXEN}
procedure PS_SetPlayerMana(const plnum: Integer; const mana: Integer; const value: Integer);

function PS_GetPlayerMana(const plnum: Integer; const mana: Integer): Integer;

function PS_GetPlayerClass(const plnum: Integer): Integer;
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

function PS_IsValidTexture(const tex: string): Boolean;

function PS_GetTextureWidth(const tex: string): Integer;

function PS_GetTextureHeight(const tex: string): Integer;

// ----------------------------- MOBJS -----------------------------------------

function PS_IsValidMobjType(const typ: integer): Boolean;

function PS_GetMobjTypeFromEditorNumber(const en: integer): integer;

function PS_GetEditorNumberFromMobjType(const typ: integer): integer;

function PS_GetMobjInfoCount: integer;

function PS_GetMobjInfoName(const typ: integer): string;

{$IFDEF STRIFE}
function PS_GetMobjInfoName2(const typ: integer): string;
{$ENDIF}

function PS_GetMobjInfoInheritsFrom(const typ: integer): integer;

function PS_GetMobjInfoDoomEdNum(const typ: integer): integer;

function PS_GetMobjInfoSpawnState(const typ: integer): integer;

function PS_GetMobjInfoSpawnHealth(const typ: integer): integer;

function PS_GetMobjInfoSeeState(const typ: integer): integer;

function PS_GetMobjInfoSeeSound(const typ: integer): string;

function PS_GetMobjInfoReactionTime(const typ: integer): integer;

function PS_GetMobjInfoAttackSound(const typ: integer): string;

function PS_GetMobjInfoPainState(const typ: integer): integer;

function PS_GetMobjInfoPainChance(const typ: integer): integer;

function PS_GetMobjInfoPainSound(const typ: integer): string;

function PS_GetMobjInfoMeleeState(const typ: integer): integer;

function PS_GetMobjInfoMissileState(const typ: integer): integer;

function PS_GetMobjInfoDeathState(const typ: integer): integer;

function PS_GetMobjInfoXdeathState(const typ: integer): integer;

function PS_GetMobjInfoDeathSound(const typ: integer): string;

function PS_GetMobjInfoSpeed(const typ: integer): integer;

function PS_GetMobjInfoVSpeed(const typ: integer): integer;

function PS_GetMobjInfoMinMissileChance(const typ: integer): integer;

function PS_GetMobjInfoPushFactor(const typ: integer): integer;

function PS_GetMobjInfoScale(const typ: integer): integer;

function PS_GetMobjInfoGravity(const typ: integer): integer;

function PS_GetMobjInfoRadius(const typ: integer): integer;

function PS_GetMobjInfoHeight(const typ: integer): integer;

function PS_GetMobjInfoMass(const typ: integer): integer;

function PS_GetMobjInfoDamage(const typ: integer): integer;

function PS_GetMobjInfoActiveSound(const typ: integer): string;

function PS_GetMobjInfoFlag(const typ: Integer; const flg: integer): Boolean;

function PS_GetMobjInfoRaiseState(const typ: integer): integer;

function PS_GetMobjInfoCustomSound1(const typ: integer): string;

function PS_GetMobjInfoCustomSound2(const typ: integer): string;

function PS_GetMobjInfoCustomSound3(const typ: integer): string;

function PS_GetMobjInfoDropItem(const typ: integer): integer;

function PS_GetMobjInfoMissiletype(const typ: integer): integer;

function PS_GetMobjInfoExplosionDamage(const typ: integer): integer;

function PS_GetMobjInfoExplosionRadius(const typ: integer): integer;

function PS_GetMobjInfoMeleeDamage(const typ: integer): integer;

function PS_GetMobjInfoMeleeSound(const typ: integer): string;

function PS_GetMobjInfoRenderStyle(const typ: integer): integer;

function PS_GetMobjInfoAlpha(const typ: integer): integer;

function PS_GetMobjInfoHealState(const typ: integer): integer;

function PS_GetMobjInfoCrashState(const typ: integer): integer;

function PS_GetMobjInfoFloatSpeed(const typ: integer): integer;

function PS_GetMobjInfoNormalSpeed(const typ: integer): integer;

function PS_GetMobjInfoFastSpeed(const typ: integer): integer;

function PS_GetMobjInfoObituary(const typ: integer): string;

function PS_GetMobjInfoHitObituary(const typ: integer): string;

function PS_GetMobjInfoGender(const typ: integer): integer;

function PS_GetMobjInfoMeleeRange(const typ: integer): integer;

function PS_GetMobjInfoMaxStepHeight(const typ: integer): integer;

function PS_GetMobjInfoMaxDropOffHeight(const typ: integer): integer;

function PS_GetMobjInfoGibHealth(const typ: integer): integer;

function PS_GetMobjInfoMaxTargetRange(const typ: integer): integer;

function PS_GetMobjInfoWeaveIndexXY(const typ: integer): integer;

function PS_GetMobjInfoWeaveIndexZ(const typ: integer): integer;

function PS_GetMobjInfoSpriteDX(const typ: integer): integer;

function PS_GetMobjInfoSpriteDY(const typ: integer): integer;

function PS_GetMobjInfoFriction(const typ: integer): integer;

function PS_GetMobjInfoInteractState(const typ: integer): integer;

{$IFDEF DOOM_OR_STRIFE}
function PS_GetMobjInfoMissileHeight(const typ: integer): integer;
{$ENDIF}

// ------------------------------ GAME -----------------------------------------

function PS_GameMap: integer;

{$IFDEF DOOM_OR_HERETIC}
function PS_GameEpisode: integer;
{$ENDIF}

function PS_Game: string;

procedure PS_GlobalEarthQuake(const tics: integer);

procedure PS_LocalEarthQuake(const x, y, z: Integer; const tics: Integer; const intensity: Integer; const maxdist: Integer);

function PS_GameSkill: integer;

// ------------------ KEYBOARD CONTROL -----------------------------------------
function PS_gamekeydown(const kkey: integer): boolean;

function PS_mousebuttons(const mkey: integer): boolean;

function PS_joybuttons(const jkey: integer): boolean;

function PS_key_right: integer;

function PS_key_left: integer;

function PS_key_up: integer;

function PS_key_down: integer;

function PS_key_lookup: integer;

function PS_key_lookdown: integer;

function PS_key_lookcenter: integer;

function PS_key_lookright: integer;

function PS_key_lookleft: integer;

{$IFNDEF STRIFE}
function PS_key_lookforward: integer;
{$ENDIF}

function PS_key_strafeleft: integer;

function PS_key_straferight: integer;

function PS_key_fire: integer;

function PS_key_use: integer;

function PS_key_strafe: integer;

function PS_key_speed: integer;

function PS_key_jump: integer;

// JVAL: 20211101 - Crouch
function PS_key_crouch: integer;

function PS_key_weapon0: integer;

function PS_key_weapon1: integer;

function PS_key_weapon2: integer;

function PS_key_weapon3: integer;

{$IFNDEF HEXEN}
function PS_key_weapon4: integer;

function PS_key_weapon5: integer;

function PS_key_weapon6: integer;

function PS_key_weapon7: integer;
{$ENDIF}

{$IFDEF HERETIC_OR_HEXEN}
function PS_key_flyup: integer;

function PS_key_flydown: integer;

function PS_key_flycenter: integer;

function PS_key_invleft: integer;

function PS_key_invright: integer;

function PS_key_useartifact: integer;
{$ENDIF}

{$IFDEF STRIFE}
function PS_key_invleft: integer;

function PS_key_invright: integer;

function PS_key_weapon8: integer;

function PS_key_weapon9: integer;

function PS_key_usehealth: integer;

function PS_key_invquery: integer;

function PS_key_mission: integer;

function PS_key_invpop: integer;

function PS_key_invkey: integer;

function PS_key_invhome: integer;

function PS_key_invend: integer;

function PS_key_invuse: integer;

function PS_key_invdrop: integer;
{$ENDIF}

function PS_mousebfire: integer;

function PS_mousebstrafe: integer;

function PS_mousebforward: integer;

function PS_joybfire: integer;

function PS_joybstrafe: integer;

function PS_joybuse: integer;

function PS_joybspeed: integer;

function PS_joybjump: integer;

// JVAL: 20211101 - Crouch
function PS_joybcrouch: integer;

function PS_joyblleft: integer;

function PS_joyblright: integer;

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
  STATE_INVALID = MAXKEY;
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
    procedure SetPosition(const x, y, z: Integer);
  end;

  TRTLActors = class(TObject)
  protected
    function GetActorKey(key: LongWord): LongWord;
  public
    function AllActors: TActorArray;
    function AllMonstersAlive: TActorArray;
    function AllMonstersDead: TActorArray;
    function AllMonsters: TActorArray;
    function AllMissiles: TActorArray;
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
    procedure SetFloorSlope(const x1, y1, z1: Integer; const x2, y2, z2: Integer; const x3, y3, z3: Integer);
    procedure SetCeilingSlope(const x1, y1, z1: Integer; const x2, y2, z2: Integer; const x3, y3, z3: Integer);
  end;

  TRTLSectors = class(TObject)
  protected
    function GetSector(id: Integer): TRTLSector;
  public
    property Sector[id: Integer]: TRTLSector read GetSector; default;
  end;

type
  TRTLMobjInfoItem = class(TObject);

  TRTLMobjInfo = class(TObject)
  protected
    function GetItem(id: Integer): TRTLMobjInfoItem;
  public
    property Item[id: Integer]: TRTLMobjInfoItem read GetItem; default;
  end;

implementation

uses
  doomdef,
  doomdata,
  d_player,
  d_event,
  d_think,
  {$IFDEF HEXEN}
  g_demo,
  {$ENDIF}
  g_game,
  info_h,
  info,
  info_common,
  m_base,
  m_fixed,
  m_rnd,
  p_common,
  p_inter,
  p_local,
  p_map,
  p_maputl,
  p_mobj,
  p_params,
  p_setup,
  p_sight,
  p_slopes,
  p_tick,
  p_user,
  r_data,
  r_defs,
  r_main,
  r_sky,
  s_sound,
  sounds,
  tables,
  w_wad;

var
  rtlactors: TRTLActors;
  rtlvertexes: TRTLVertexes;
  rtlsides: TRTLSides;
  rtllines: TRTLLines;
  rtlsectors: TRTLSectors;
  rtlmobjinfo: TRTLMobjInfo;

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

function PS_GetActorTracer(const key: LongWord): LongWord;
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
  tmo := mo.tracer;
  if tmo <> nil then
    Result := tmo.key
  else
    Result := ACTOR_INVALID;
end;

procedure PS_SetActorTracer(const key: LongWord; const trac: LongWord);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.tracer := mobj_from_key(trac);
end;

function PS_GetActorMaster(const key: LongWord): LongWord;
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
  tmo := mo.master;
  if tmo <> nil then
    Result := tmo.key
  else
    Result := ACTOR_INVALID;
end;

procedure PS_SetActorMaster(const key: LongWord; const mst: LongWord);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.master := mobj_from_key(mst);
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
  P_UnsetThingPosition(mo);
  mo.x := x;
  P_SetThingPosition(mo);
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
  P_UnsetThingPosition(mo);
  mo.y := y;
  P_SetThingPosition(mo);
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

procedure PS_SetActorPosition(const key: LongWord; const x, y, z: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  P_UnsetThingPosition(mo);
  mo.x := x;
  mo.y := y;
  mo.z := z;
  P_SetThingPosition(mo);
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
  Result := mo.mass;
end;

procedure PS_SetActorMass(const key: LongWord; const m: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.mass := m;
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

function _mo_flag_result(const mo: Pmobj_t; const flag: LongWord): TFlagResult;
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

  if flag < 160 then
  begin
    Result.flags := @mo.flags3_ex;
    Result.flag := (1 shl (flag - 128));
    Exit;
  end;

  if flag < 192 then
  begin
    Result.flags := @mo.flags4_ex;
    Result.flag := (1 shl (flag - 160));
    Exit;
  end;

  Result.flags := nil;
  Result.flag := 0;
end;

function _info_flag_result(const inf: Pmobjinfo_t; const flag: LongWord): TFlagResult;
begin
  if flag < 32 then
  begin
    Result.flags := @inf.flags;
    Result.flag := (1 shl flag);
    Exit;
  end;

  if flag < 64 then
  begin
  {$IFDEF HERETIC_OR_HEXEN}
    Result.flags := @inf.flags2;
    Result.flag := (1 shl (flag - 32));
  {$ELSE}
    Result.flags := nil;
    Result.flag := 0;
  {$ENDIF}
    Exit;
  end;

  if flag < 96 then
  begin
    Result.flags := @inf.flags_ex;
    Result.flag := (1 shl (flag - 64));
    Exit;
  end;

  if flag < 128 then
  begin
    Result.flags := @inf.flags2_ex;
    Result.flag := (1 shl (flag - 96));
    Exit;
  end;

  if flag < 160 then
  begin
    Result.flags := @inf.flags3_ex;
    Result.flag := (1 shl (flag - 128));
    Exit;
  end;

  if flag < 192 then
  begin
    Result.flags := @inf.flags4_ex;
    Result.flag := (1 shl (flag - 160));
    Exit;
  end;

  Result.flags := nil;
  Result.flag := 0;
end;

function PS_CheckActorFlag(const key: LongWord; const flag: LongWord): Boolean;
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

  flgresult := _mo_flag_result(mo, flag);
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

  flgresult := _mo_flag_result(mo, flag);
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

  flgresult := _mo_flag_result(mo, flag);
  if flgresult.flags = nil then
    Exit;

  flgresult.flags^ := flgresult.flags^ and not flgresult.flag;
end;


function PS_GetActorScale(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.scale;
end;

procedure PS_SetActorScale(const key: LongWord; const value: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.scale := value;
end;

function PS_GetActorPushFactor(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.pushfactor;
end;

procedure PS_SetActorPushFactor(const key: LongWord; const value: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.pushfactor := value;
end;

function PS_GetActorGravity(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.gravity;
end;

procedure PS_SetActorGravity(const key: LongWord; const value: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.gravity := value;
end;

function _GetActorArg(const idx: integer; const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.args[idx];
end;

procedure _SetActorArg(const idx: integer; const key: LongWord; const value: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.args[idx] := value;
end;

function PS_GetActorSpecial(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.special;
end;

procedure PS_SetActorSpecial(const key: LongWord; const value: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.special := value;
end;

function PS_GetActorArg1(const key: LongWord): Integer;
begin
  Result := _GetActorArg(0, key);
end;

procedure PS_SetActorArg1(const key: LongWord; const value: Integer);
begin
  _SetActorArg(0, key, value);
end;

function PS_GetActorArg2(const key: LongWord): Integer;
begin
  Result := _GetActorArg(1, key);
end;

procedure PS_SetActorArg2(const key: LongWord; const value: Integer);
begin
  _SetActorArg(1, key, value);
end;

function PS_GetActorArg3(const key: LongWord): Integer;
begin
  Result := _GetActorArg(2, key);
end;

procedure PS_SetActorArg3(const key: LongWord; const value: Integer);
begin
  _SetActorArg(2, key, value);
end;

function PS_GetActorArg4(const key: LongWord): Integer;
begin
  Result := _GetActorArg(3, key);
end;

procedure PS_SetActorArg4(const key: LongWord; const value: Integer);
begin
  _SetActorArg(3, key, value);
end;

function PS_GetActorArg5(const key: LongWord): Integer;
begin
  Result := _GetActorArg(4, key);
end;

procedure PS_SetActorArg5(const key: LongWord; const value: Integer);
begin
  _SetActorArg(4, key, value);
end;

function PS_GetActorWeaveIndexXY(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.WeaveIndexXY;
end;

procedure PS_SetActorWeaveIndexXY(const key: LongWord; const value: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.WeaveIndexXY := value;
end;

function PS_GetActorWeaveIndexZ(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.WeaveIndexZ;
end;

procedure PS_SetActorWeaveIndexZ(const key: LongWord; const value: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.WeaveIndexXY := value;
end;

function PS_GetActorFriction(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.friction;
end;

procedure PS_SetActorFriction(const key: LongWord; const value: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.friction := value;
end;

function PS_GetActorPainChance(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.painchance;
end;

procedure PS_SetActorPainChance(const key: LongWord; const value: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.painchance := value;
end;

function PS_GetActorSpriteDX(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.spriteDX;
end;

procedure PS_SetActorSpriteDX(const key: LongWord; const value: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.spriteDX := value;
end;

function PS_GetActorSpriteDY(const key: LongWord): Integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := mo.spriteDY;
end;

procedure PS_SetActorSpriteDY(const key: LongWord; const value: Integer);
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
    Exit;
  mo.spriteDY := value;
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

function PS_GetActorState(const key: LongWord): integer;
var
  mo: Pmobj_t;
begin
  mo := mobj_from_key(key);
  if mo = nil then
  begin
    Result := STATE_INVALID;
    Exit;
  end;
  Result := pDiff(mo.state, @states[0], SizeOf(state_t));
end;

procedure PS_SetActorState(const key: LongWord; const value: Integer);
var
  mo: Pmobj_t;
begin
  if (value < 0) or (value >= numstates) then
    Exit;

  mo := mobj_from_key(key);
  if mo = nil then
    Exit;

  P_SetMobjState(mo, statenum_t(value));
end;

function PS_IsValidActor(const key: LongWord): Boolean;
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

function PS_CheckActorSight(const key1, key2: LongWord): Boolean;
var
  mo1, mo2: Pmobj_t;
begin
  mo1 := mobj_from_key(key1);
  if mo1 = nil then
  begin
    Result := False;
    Exit;
  end;

  mo2 := mobj_from_key(key2);
  if mo1 = nil then
  begin
    Result := False;
    Exit;
  end;

  Result := P_CheckSight(mo1, mo2);
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

function TActorKeyList.Delete(const item: LongWord): Boolean;
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

function TActorKeyList.Exists(const value: LongWord): Boolean;
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

procedure TActorKeyList.AddAllMissiles;
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
         (minfo.flags and MF_MISSILE <> 0) then
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

procedure TActorKeyList.SetAllowDuplicates(const value: Boolean);
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

procedure TRTLActor.SetPosition(const x, y, z: Integer);
begin
  PS_SetActorPosition(LongWord(self), x, y, z);
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

function TRTLActors.AllMissiles: TActorArray;
var
  akl: TActorKeyList;
begin
  akl := TActorKeyList.Create;
  akl.AddAllMissiles;
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

procedure TRTLActorTracer_W(Self: TRTLActor; const T: TRTLActor);
begin
  PS_SetActorTracer(LongWord(Self), LongWord(T));
end;

procedure TRTLActorTracer_R(Self: TRTLActor; var T: TRTLActor);
begin
  T := TRTLActor(PS_GetActorTracer(LongWord(Self)));
end;

procedure TRTLActorMaster_W(Self: TRTLActor; const T: TRTLActor);
begin
  PS_SetActorMaster(LongWord(Self), LongWord(T));
end;

procedure TRTLActorMaster_R(Self: TRTLActor; var T: TRTLActor);
begin
  T := TRTLActor(PS_GetActorMaster(LongWord(Self)));
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

procedure TRTLActorHeight_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorHeight(LongWord(Self));
end;

procedure TRTLActorSpawnHealth_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorSpawnHealth(LongWord(Self));
end;

procedure TRTLActorMass_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorMass(LongWord(Self), T);
end;

procedure TRTLActorMass_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorMass(LongWord(Self));
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

procedure TRTLActorPushFactor_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorPushFactor(LongWord(Self), T);
end;

procedure TRTLActorPushFactor_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorPushFactor(LongWord(Self));
end;

procedure TRTLActorScale_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorScale(LongWord(Self), T);
end;

procedure TRTLActorScale_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorScale(LongWord(Self));
end;

procedure TRTLActorGravity_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorGravity(LongWord(Self), T);
end;

procedure TRTLActorGravity_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorGravity(LongWord(Self));
end;

procedure TRTLActorSpecial_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorSpecial(LongWord(Self), T);
end;

procedure TRTLActorSpecial_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorSpecial(LongWord(Self));
end;

procedure TRTLActorArg1_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorArg1(LongWord(Self), T);
end;

procedure TRTLActorArg1_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorArg1(LongWord(Self));
end;

procedure TRTLActorArg2_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorArg2(LongWord(Self), T);
end;

procedure TRTLActorArg2_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorArg2(LongWord(Self));
end;

procedure TRTLActorArg3_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorArg3(LongWord(Self), T);
end;

procedure TRTLActorArg3_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorArg3(LongWord(Self));
end;

procedure TRTLActorArg4_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorArg4(LongWord(Self), T);
end;

procedure TRTLActorArg4_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorArg4(LongWord(Self));
end;

procedure TRTLActorArg5_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorArg5(LongWord(Self), T);
end;

procedure TRTLActorArg5_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorArg5(LongWord(Self));
end;

procedure TRTLActorWeaveIndexXY_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorWeaveIndexXY(LongWord(Self), T);
end;

procedure TRTLActorWeaveIndexXY_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorWeaveIndexXY(LongWord(Self));
end;

procedure TRTLActorWeaveIndexZ_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorWeaveIndexZ(LongWord(Self), T);
end;

procedure TRTLActorWeaveIndexZ_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorWeaveIndexZ(LongWord(Self));
end;

procedure TRTLActorFriction_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorFriction(LongWord(Self), T);
end;

procedure TRTLActorFriction_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorFriction(LongWord(Self));
end;

procedure TRTLActorPainChance_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorPainChance(LongWord(Self), T);
end;

procedure TRTLActorPainChance_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorPainChance(LongWord(Self));
end;

procedure TRTLActorSpriteDX_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorSpriteDX(LongWord(Self), T);
end;

procedure TRTLActorSpriteDX_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorSpriteDX(LongWord(Self));
end;

procedure TRTLActorSpriteDY_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorSpriteDY(LongWord(Self), T);
end;

procedure TRTLActorSpriteDY_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorSpriteDY(LongWord(Self));
end;

procedure TRTLActorFlags_W(Self: TRTLActor; const T: Boolean; const t1: LongWord);
begin
  if T then
    PS_SetActorFlag(LongWord(Self), t1)
  else
    PS_UnSetActorFlag(LongWord(Self), t1)
end;

procedure TRTLActorFlags_R(Self: TRTLActor; var T: Boolean; const t1: LongWord);
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

procedure TRTLActorState_R(Self: TRTLActor; var T: Integer);
begin
  T := PS_GetActorState(LongWord(Self));
end;

procedure TRTLActorState_W(Self: TRTLActor; const T: Integer);
begin
  PS_SetActorState(LongWord(Self), T);
end;


procedure TRTLActorsActor_R(Self: TRTLActors; var T: TRTLActor; const t1: LongWord);
begin
  T := TRTLActor(Self.Actor[t1]);
end;

// ------------------------------- MAP -----------------------------------------
function PS_P_PointOnLineSide(x: Integer; y: Integer; line: integer): Integer;
begin
  if (line >= 0) and (line < numlines) then
    Result := P_PointOnLineSide(x, y, @lines[line])
  else
    Result := 0;
end;

function PS_R_PointInSector(const x: Integer; const y: Integer): Integer;
begin
  Result := R_PointInSubsector(x, y).sector.iSectorID;
end;

function PS_R_PointInSubSector(const x: Integer; const y: Integer): Integer;
begin
  Result := pdiff(pointer(R_PointInSubsector(x, y)), subsectors, SizeOf(subsector_t));
end;

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

function PS_IsValidVertex(const v: Integer): Boolean;
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

function PS_IsValidSide(const sd: Integer): Boolean;
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
      Result := SECTOR_INVALID;
  end
  else
    Result := SECTOR_INVALID;
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
      Result := SECTOR_INVALID;
  end
  else
    Result := SECTOR_INVALID;
end;

function PS_IsValidLine(const ld: Integer): Boolean;
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

function PS_GetSectorFloorAngle(const sec: Integer): LongWord;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := sectors[sec].floorangle
  else
    Result := 0;
end;

procedure PS_SetSectorFloorAngle(const sec: Integer; const ang: LongWord);
begin
  if (sec >= 0) and (sec < numsectors) then
  begin
    sectors[sec].floorangle := ang;
    sectors[sec].renderflags := sectors[sec].renderflags or SRF_INTERPOLATE_ROTATE;
  end;
end;

function PS_GetSectorFloorAngleX(const sec: Integer): Integer;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := sectors[sec].flooranglex
  else
    Result := 0;
end;

procedure PS_SetSectorFloorAngleX(const sec: Integer; const angx: Integer);
begin
  if (sec >= 0) and (sec < numsectors) then
  begin
    sectors[sec].flooranglex := angx;
    sectors[sec].renderflags := sectors[sec].renderflags or SRF_INTERPOLATE_ROTATE;
  end;
end;

function PS_GetSectorFloorAngleY(const sec: Integer): Integer;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := sectors[sec].floorangley
  else
    Result := 0;
end;

procedure PS_SetSectorFloorAngleY(const sec: Integer; const angy: Integer);
begin
  if (sec >= 0) and (sec < numsectors) then
  begin
    sectors[sec].floorangley := angy;
    sectors[sec].renderflags := sectors[sec].renderflags or SRF_INTERPOLATE_ROTATE;
  end;
end;

function PS_GetSectorCeilingAngle(const sec: Integer): LongWord;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := sectors[sec].ceilingangle
  else
    Result := 0;
end;

procedure PS_SetSectorCeilingAngle(const sec: Integer; const ang: LongWord);
begin
  if (sec >= 0) and (sec < numsectors) then
  begin
    sectors[sec].ceilingangle := ang;
    sectors[sec].renderflags := sectors[sec].renderflags or SRF_INTERPOLATE_ROTATE;
  end;
end;

function PS_GetSectorCeilingAngleX(const sec: Integer): Integer;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := sectors[sec].ceilinganglex
  else
    Result := 0;
end;

procedure PS_SetSectorCeilingAngleX(const sec: Integer; const angx: Integer);
begin
  if (sec >= 0) and (sec < numsectors) then
  begin
    sectors[sec].ceilinganglex := angx;
    sectors[sec].renderflags := sectors[sec].renderflags or SRF_INTERPOLATE_ROTATE;
  end;
end;

function PS_GetSectorCeilingAngleY(const sec: Integer): Integer;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := sectors[sec].ceilingangley
  else
    Result := 0;
end;

procedure PS_SetSectorCeilingAngleY(const sec: Integer; const angy: Integer);
begin
  if (sec >= 0) and (sec < numsectors) then
  begin
    sectors[sec].ceilingangley := angy;
    sectors[sec].renderflags := sectors[sec].renderflags or SRF_INTERPOLATE_ROTATE;
  end;
end;

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

function PS_GetSectorInterpolate(const sec: Integer): Boolean;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := sectors[sec].renderflags and SRF_NO_INTERPOLATE = 0
  else
    Result := False;
end;

procedure PS_SetSectorInterpolate(const sec: Integer; const intpl: Boolean);
begin
  if (sec >= 0) and (sec < numsectors) then
  begin
    if intpl then
      sectors[sec].renderflags := sectors[sec].renderflags and not SRF_NO_INTERPOLATE
    else
      sectors[sec].renderflags := sectors[sec].renderflags or SRF_NO_INTERPOLATE
  end;
end;

function PS_GetSectorFog(const sec: Integer): Boolean;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := sectors[sec].renderflags and SRF_FOG = 0
  else
    Result := False;
end;

procedure PS_SetSectorFog(const sec: Integer; const fog: Boolean);
begin
  if (sec >= 0) and (sec < numsectors) then
  begin
    if fog then
      sectors[sec].renderflags := sectors[sec].renderflags and not SRF_FOG
    else
      sectors[sec].renderflags := sectors[sec].renderflags or SRF_FOG
  end;
end;

function PS_GetSectorGravity(const sec: Integer): Integer;
begin
  if (sec >= 0) and (sec < numsectors) then
    Result := sectors[sec].gravity
  else
    Result := GRAVITY;
end;

procedure PS_SetSectorGravity(const sec: Integer; const grav: Integer);
begin
  if (sec >= 0) and (sec < numsectors) then
    sectors[sec].gravity := grav;
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

function PS_IsValidSector(const sec: Integer): Boolean;
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

procedure TRTLSector.SetFloorSlope(const x1, y1, z1: Integer; const x2, y2, z2: Integer; const x3, y3, z3: Integer);
begin
  PS_SetFloorSlope(Integer(self) - 1, x1, y1, z1, x2, y2, z2, x3, y3, z3);
end;

procedure TRTLSector.SetCeilingSlope(const x1, y1, z1: Integer; const x2, y2, z2: Integer; const x3, y3, z3: Integer);
begin
  PS_SetCeilingSlope(Integer(self) - 1, x1, y1, z1, x2, y2, z2, x3, y3, z3);
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

procedure TRTLSectorFloorAngle_W(Self: TRTLSector; const T: LongWord);
begin
  PS_SetSectorFloorAngle(Integer(Self) - 1, T);
end;

procedure TRTLSectorFloorAngle_R(Self: TRTLSector; var T: LongWord);
begin
  T := PS_GetSectorFloorAngle(Integer(Self) - 1);
end;

procedure TRTLSectorFloorAngleX_W(Self: TRTLSector; const T: Integer);
begin
  PS_SetSectorFloorAngleX(Integer(Self) - 1, T);
end;

procedure TRTLSectorFloorAngleX_R(Self: TRTLSector; var T: Integer);
begin
  T := PS_GetSectorFloorAngleX(Integer(Self) - 1);
end;

procedure TRTLSectorFloorAngleY_W(Self: TRTLSector; const T: Integer);
begin
  PS_SetSectorFloorAngleY(Integer(Self) - 1, T);
end;

procedure TRTLSectorFloorAngleY_R(Self: TRTLSector; var T: Integer);
begin
  T := PS_GetSectorFloorAngleY(Integer(Self) - 1);
end;

procedure TRTLSectorCeilingAngle_W(Self: TRTLSector; const T: LongWord);
begin
  PS_SetSectorCeilingAngle(Integer(Self) - 1, T);
end;

procedure TRTLSectorCeilingAngle_R(Self: TRTLSector; var T: LongWord);
begin
  T := PS_GetSectorCeilingAngle(Integer(Self) - 1);
end;

procedure TRTLSectorCeilingAngleX_W(Self: TRTLSector; const T: Integer);
begin
  PS_SetSectorCeilingAngleX(Integer(Self) - 1, T);
end;

procedure TRTLSectorCeilingAngleX_R(Self: TRTLSector; var T: Integer);
begin
  T := PS_GetSectorCeilingAngleX(Integer(Self) - 1);
end;

procedure TRTLSectorCeilingAngleY_W(Self: TRTLSector; const T: Integer);
begin
  PS_SetSectorCeilingAngleY(Integer(Self) - 1, T);
end;

procedure TRTLSectorCeilingAngleY_R(Self: TRTLSector; var T: Integer);
begin
  T := PS_GetSectorCeilingAngleY(Integer(Self) - 1);
end;

procedure TRTLSectorRippleFloor_W(Self: TRTLSector; const T: Boolean);
begin
  PS_SetSectorRippleFloor(Integer(Self) - 1, T);
end;

procedure TRTLSectorRippleFloor_R(Self: TRTLSector; var T: Boolean);
begin
  T := PS_GetSectorRippleFloor(Integer(Self) - 1);
end;

procedure TRTLSectorRippleCeiling_W(Self: TRTLSector; const T: Boolean);
begin
  PS_SetSectorRippleCeiling(Integer(Self) - 1, T);
end;

procedure TRTLSectorRippleCeiling_R(Self: TRTLSector; var T: Boolean);
begin
  T := PS_GetSectorRippleCeiling(Integer(Self) - 1);
end;

procedure TRTLSectorInterpolate_W(Self: TRTLSector; const T: Boolean);
begin
  PS_SetSectorInterpolate(Integer(Self) - 1, T);
end;

procedure TRTLSectorInterpolate_R(Self: TRTLSector; var T: Boolean);
begin
  T := PS_GetSectorInterpolate(Integer(Self) - 1);
end;

// JVAL: Sector fog (VERSION 207)
procedure TRTLSectorFog_W(Self: TRTLSector; const T: Boolean);
begin
  PS_SetSectorFog(Integer(Self) - 1, T);
end;

procedure TRTLSectorFog_R(Self: TRTLSector; var T: Boolean);
begin
  T := PS_GetSectorFog(Integer(Self) - 1);
end;

// JVAL: sector gravity (VERSION 204)
procedure TRTLSectorGravity_W(Self: TRTLSector; const T: fixed_t);
begin
  PS_SetSectorGravity(Integer(Self) - 1, T);
end;

procedure TRTLSectorGravity_R(Self: TRTLSector; var T: fixed_t);
begin
  T := PS_GetSectorGravity(Integer(Self) - 1);
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

function PS_PlayerInGame(const plnum: Integer): Boolean;
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

  if ticks <= 0 then
    Exit;

  mo := mobj_from_key(actor);
  if mo = nil then
    Exit;

  P_PlayerFaceMobj(@players[plnum], mo, ticks);
end;
{$ENDIF}

{$IFDEF DOOM_OR_STRIFE}
procedure PS_SetPlayerHasCard(const plnum: Integer; const card: Integer; const value: Boolean);
begin
  if not PS_PlayerInGame(plnum) then
    Exit;

  if (card < 0) or (card >= Ord(NUMCARDS)) then
    Exit;

  players[plnum].cards[card] := value;
end;

function PS_GetPlayerHasCard(const plnum: Integer; const card: Integer): Boolean;
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
procedure PS_SetPlayerHasKey(const plnum: Integer; const key: Integer; const value: Boolean);
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

function PS_GetPlayerHasKey(const plnum: Integer; const key: Integer): Boolean;
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

procedure PS_PlayerUseArtifact(const plnum: Integer; const arti: Integer);
begin
  if not PS_PlayerInGame(plnum) then
    Exit;

  if (arti < 0) or (arti >= Ord(NUMARTIFACTS)) then
    Exit;

  P_PlayerUseArtifact(@players[plnum], artitype_t(arti));
end;

function PS_GiveArtifactToPlayer(const plnum: Integer; const arti: Integer): Boolean;
begin
  if not PS_PlayerInGame(plnum) then
  begin
    Result := False;
    Exit;
  end;

  if (arti < 0) or (arti >= Ord(NUMARTIFACTS)) then
  begin
    Result := False;
    Exit;
  end;

  Result := P_GiveArtifact(@players[plnum], artitype_t(arti), nil);
end;

function PS_CheckPlayerArtifact(const plnum: Integer; const arti: Integer): Integer;
var
  i: integer;
  p: Pplayer_t;
begin
  if not PS_PlayerInGame(plnum) then
  begin
    Result := 0;
    Exit;
  end;

  if (arti < 0) or (arti >= Ord(NUMARTIFACTS)) then
  begin
    Result := 0;
    Exit;
  end;

  p := @players[plnum];
  for i := 0 to p.inventorySlotNum - 1 do
    if p.inventory[i]._type = Ord(arti) then
    begin
      Result := p.inventory[i].Count;
      Exit;
    end;

  Result := 0;
end;
{$ENDIF}

procedure PS_SetPlayerHasWeapon(const plnum: Integer; const weapon: Integer; const value: Boolean);
begin
  if not PS_PlayerInGame(plnum) then
    Exit;

  if (weapon < 0) or (weapon >= Ord(NUMWEAPONS)) then
    Exit;

  players[plnum].weaponowned[weapon] := {$IFDEF DOOM_OR_HERETIC}1{$ELSE}true{$ENDIF};
end;

function PS_GetPlayerHasWeapon(const plnum: Integer; const weapon: Integer): Boolean;
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

function PS_GetPlayerClass(const plnum: Integer): Integer;
begin
  if not PS_PlayerInGame(plnum) then
  begin
    Result := 0;
    Exit;
  end;

  Result := Ord(players[plnum]._class);
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

function PS_IsValidTexture(const tex: string): Boolean;
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

// ----------------------------- MOBJS -----------------------------------------

function PS_IsValidMobjType(const typ: integer): Boolean;
begin
  Result := (typ >= 0) and (typ < nummobjtypes);
end;

function PS_GetMobjTypeFromEditorNumber(const en: integer): integer;
begin
  if en < 0 then
  begin
    Result := MOBJTYPE_INVALID;
    Exit;
  end;

  Result := Info_GetMobjNumForDoomNum(en);
  if (Result < 0) or (Result >= nummobjtypes) then
    Result := MOBJTYPE_INVALID;
end;

function PS_GetEditorNumberFromMobjType(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := EDITORNUMBER_INVALID;
    Exit;
  end;
  Result := mobjinfo[typ].doomednum;
end;

function PS_GetMobjInfoCount: integer;
begin
  Result := nummobjtypes;
end;

function PS_GetMobjInfoName(const typ: integer): string;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := '';
    Exit;
  end;
  Result := mobjinfo[typ].name;
end;

{$IFDEF STRIFE}
function PS_GetMobjInfoName2(const typ: integer): string;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := '';
    Exit;
  end;
  Result := mobjinfo[typ].name;
end;

{$ENDIF}

function PS_GetMobjInfoInheritsFrom(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := MOBJTYPE_INVALID;
    Exit;
  end;
  Result := mobjinfo[typ].inheritsfrom;
  if Result < 0 then
    Result := MOBJTYPE_INVALID;
end;

function PS_GetMobjInfoDoomEdNum(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := EDITORNUMBER_INVALID;
    Exit;
  end;
  Result := mobjinfo[typ].doomednum;
end;

function PS_GetMobjInfoSpawnState(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := STATE_INVALID;
    Exit;
  end;
  Result := mobjinfo[typ].spawnstate;
end;

function PS_GetMobjInfoSpawnHealth(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].spawnhealth;
end;

function PS_GetMobjInfoSeeState(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := STATE_INVALID;
    Exit;
  end;
  Result := mobjinfo[typ].seestate;
end;

function PS_GetMobjInfoSeeSound(const typ: integer): string;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := '';
    Exit;
  end;
  Result := S_GetSoundNameForNum(mobjinfo[typ].seesound);
end;

function PS_GetMobjInfoReactionTime(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].reactiontime;
end;

function PS_GetMobjInfoAttackSound(const typ: integer): string;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := '';
    Exit;
  end;
  Result := S_GetSoundNameForNum(mobjinfo[typ].attacksound);
end;

function PS_GetMobjInfoPainState(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := STATE_INVALID;
    Exit;
  end;
  Result := mobjinfo[typ].painstate;
end;

function PS_GetMobjInfoPainChance(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].painchance;
end;

function PS_GetMobjInfoPainSound(const typ: integer): string;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := '';
    Exit;
  end;
  Result := S_GetSoundNameForNum(mobjinfo[typ].painsound);
end;

function PS_GetMobjInfoMeleeState(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := STATE_INVALID;
    Exit;
  end;
  Result := mobjinfo[typ].meleestate;
end;

function PS_GetMobjInfoMissileState(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := STATE_INVALID;
    Exit;
  end;
  Result := mobjinfo[typ].missilestate;
end;

function PS_GetMobjInfoDeathState(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := STATE_INVALID;
    Exit;
  end;
  Result := mobjinfo[typ].deathstate;
end;

function PS_GetMobjInfoXdeathState(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := STATE_INVALID;
    Exit;
  end;
  Result := mobjinfo[typ].xdeathstate;
end;

function PS_GetMobjInfoDeathSound(const typ: integer): string;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := '';
    Exit;
  end;
  Result := S_GetSoundNameForNum(mobjinfo[typ].deathsound);
end;

function PS_GetMobjInfoSpeed(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].speed;
end;

function PS_GetMobjInfoVSpeed(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].vspeed;
end;

function PS_GetMobjInfoMinMissileChance(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].minmissilechance;
end;

function PS_GetMobjInfoPushFactor(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].pushfactor;
end;

function PS_GetMobjInfoScale(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].scale;
end;

function PS_GetMobjInfoGravity(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].gravity;
end;

function PS_GetMobjInfoRadius(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].radius;
end;

function PS_GetMobjInfoHeight(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].height;
end;

function PS_GetMobjInfoMass(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].mass;
end;

function PS_GetMobjInfoDamage(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].damage;
end;

function PS_GetMobjInfoActiveSound(const typ: integer): string;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := '';
    Exit;
  end;
  Result := S_GetSoundNameForNum(mobjinfo[typ].activesound);
end;

function PS_GetMobjInfoFlag(const typ: Integer; const flg: integer): Boolean;
var
  flgresult: TFlagResult;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := False;
    Exit;
  end;

  flgresult := _info_flag_result(@mobjinfo[typ], flg);
  if flgresult.flags = nil then
  begin
    Result := False;
    Exit;
  end;

  Result := flgresult.flags^ and flgresult.flag <> 0;
end;

function PS_GetMobjInfoRaiseState(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := STATE_INVALID;
    Exit;
  end;
  Result := mobjinfo[typ].raisestate;
end;

function PS_GetMobjInfoCustomSound1(const typ: integer): string;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := '';
    Exit;
  end;
  Result := S_GetSoundNameForNum(mobjinfo[typ].customsound1);
end;

function PS_GetMobjInfoCustomSound2(const typ: integer): string;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := '';
    Exit;
  end;
  Result := S_GetSoundNameForNum(mobjinfo[typ].customsound2);
end;

function PS_GetMobjInfoCustomSound3(const typ: integer): string;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := '';
    Exit;
  end;
  Result := S_GetSoundNameForNum(mobjinfo[typ].customsound3);
end;

function PS_GetMobjInfoDropItem(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := MOBJTYPE_INVALID;
    Exit;
  end;
  Result := mobjinfo[typ].dropitem;
end;

function PS_GetMobjInfoMissiletype(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := MOBJTYPE_INVALID;
    Exit;
  end;
  Result := mobjinfo[typ].missiletype;
end;

function PS_GetMobjInfoExplosionDamage(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].explosiondamage;
end;

function PS_GetMobjInfoExplosionRadius(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].explosionradius;
end;

function PS_GetMobjInfoMeleeDamage(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].meleedamage;
end;

function PS_GetMobjInfoMeleeSound(const typ: integer): string;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := '';
    Exit;
  end;
  Result := S_GetSoundNameForNum(mobjinfo[typ].meleesound);
end;

function PS_GetMobjInfoRenderStyle(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := Ord(mobjinfo[typ].renderstyle);
end;

function PS_GetMobjInfoAlpha(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].alpha;
end;

function PS_GetMobjInfoHealState(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := STATE_INVALID;
    Exit;
  end;
  Result := mobjinfo[typ].healstate;
end;

function PS_GetMobjInfoCrashState(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := STATE_INVALID;
    Exit;
  end;
  Result := mobjinfo[typ].crashstate;
end;

function PS_GetMobjInfoFloatSpeed(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].floatspeed;
end;

function PS_GetMobjInfoNormalSpeed(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].normalspeed;
end;

function PS_GetMobjInfoFastSpeed(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].fastspeed;
end;

function PS_GetMobjInfoObituary(const typ: integer): string;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := '';
    Exit;
  end;
  Result := mobjinfo[typ].obituary;
end;

function PS_GetMobjInfoHitObituary(const typ: integer): string;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := '';
    Exit;
  end;
  Result := mobjinfo[typ].hitobituary;
end;

function PS_GetMobjInfoGender(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := Ord(mobjinfo[typ].gender);
end;

function PS_GetMobjInfoMeleeRange(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].meleerange;
end;

function PS_GetMobjInfoMaxStepHeight(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].maxstepheight;
end;

function PS_GetMobjInfoMaxDropOffHeight(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].maxdropoffheight;
end;

function PS_GetMobjInfoGibHealth(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].gibhealth;
end;

function PS_GetMobjInfoMaxTargetRange(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].maxtargetrange;
end;

function PS_GetMobjInfoWeaveIndexXY(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].WeaveIndexXY;
end;

function PS_GetMobjInfoWeaveIndexZ(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].WeaveIndexZ;
end;

function PS_GetMobjInfoFriction(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].friction;
end;

function PS_GetMobjInfoSpriteDX(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].spriteDX;
end;

function PS_GetMobjInfoSpriteDY(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].spriteDY;
end;

function PS_GetMobjInfoInteractState(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := STATE_INVALID;
    Exit;
  end;
  Result := mobjinfo[typ].interactstate;
end;

{$IFDEF DOOM_OR_STRIFE}
function PS_GetMobjInfoMissileHeight(const typ: integer): integer;
begin
  if (typ < 0) or (typ >= nummobjtypes) then
  begin
    Result := 0;
    Exit;
  end;
  Result := mobjinfo[typ].height;
end;
{$ENDIF}

// --------------------- TRTLMobjInfo ------------------------------------------

function TRTLMobjInfo.GetItem(id: Integer): TRTLMobjInfoItem;
begin
  if (id >= 0) and (id < nummobjtypes) then
    Result := TRTLMobjInfoItem(id + 1)
  else
    Result := TRTLMobjInfoItem(MOBJTYPE_INVALID);
end;

procedure TRTLMobjInfoItem_R(Self: TRTLMobjInfo; var T: TRTLMobjInfoItem; const t1: integer);
begin
  T := Self[t1];
end;

procedure TRTLMobjInfoCount_R(Self: TRTLMobjInfo; var T: Integer);
begin
  T := nummobjtypes;
end;

procedure TRTLMobjInfoItemName_R(Self: TRTLMobjInfoItem; var T: string);
begin
  T := PS_GetMobjInfoName(Integer(Self) - 1);
end;

{$IFDEF STRIFE}
procedure TRTLMobjInfoItemName2_R(Self: TRTLMobjInfoItem; var T: string);
begin
  T := PS_GetMobjInfoName2(Integer(Self) - 1);
end;
{$ENDIF}

procedure TRTLMobjInfoItemInheritsFrom_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoInheritsFrom(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemDoomEdNum_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoDoomEdNum(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemSpawnState_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoSpawnState(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemSpawnHealth_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoSpawnHealth(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemSeeState_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoSeeState(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemSeeSound_R(Self: TRTLMobjInfoItem; var T: string);
begin
  T := PS_GetMobjInfoSeeSound(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemReactionTime_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoReactionTime(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemAttackSound_R(Self: TRTLMobjInfoItem; var T: string);
begin
  T := PS_GetMobjInfoAttackSound(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemPainState_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoPainState(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemPainChance_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoPainChance(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemPainSound_R(Self: TRTLMobjInfoItem; var T: string);
begin
  T := PS_GetMobjInfoPainSound(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemMeleeState_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoMeleeState(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemMissileState_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoMissileState(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemDeathState_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoDeathState(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemXdeathState_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoXdeathState(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemDeathSound_R(Self: TRTLMobjInfoItem; var T: string);
begin
  T := PS_GetMobjInfoDeathSound(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemSpeed_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoSpeed(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemVSpeed_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoVSpeed(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemMinMissileChance_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoMinMissileChance(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemPushFactor_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoPushFactor(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemScale_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoScale(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemGravity_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoGravity(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemRadius_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoRadius(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemHeight_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoHeight(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemMass_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoMass(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemDamage_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoDamage(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemActiveSound_R(Self: TRTLMobjInfoItem; var T: string);
begin
  T := PS_GetMobjInfoActiveSound(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemFlag_R(Self: TRTLMobjInfoItem; var T: Boolean; const t1: integer);
begin
  T := PS_GetMobjInfoFlag(Integer(Self) - 1, t1);
end;

procedure TRTLMobjInfoItemRaiseState_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoRaiseState(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemCustomSound1_R(Self: TRTLMobjInfoItem; var T: string);
begin
  T := PS_GetMobjInfoCustomSound1(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemCustomSound2_R(Self: TRTLMobjInfoItem; var T: string);
begin
  T := PS_GetMobjInfoCustomSound2(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemCustomSound3_R(Self: TRTLMobjInfoItem; var T: string);
begin
  T := PS_GetMobjInfoCustomSound3(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemMeleeSound_R(Self: TRTLMobjInfoItem; var T: string);
begin
  T := PS_GetMobjInfoMeleeSound(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemDropItem_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoDropItem(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemMissileType_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoMissiletype(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemExplosionDamage_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoExplosionDamage(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemExplosionRadius_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoExplosionRadius(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemMeleeDamage_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoMeleeDamage(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemRenderStyle_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoRenderStyle(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemAlpha_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoAlpha(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemHealState_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoHealState(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemCrashState_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoCrashState(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemFloatSpeed_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoFloatSpeed(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemNormalSpeed_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoNormalSpeed(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemFastSpeed_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoFastSpeed(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemObituary_R(Self: TRTLMobjInfoItem; var T: string);
begin
  T := PS_GetMobjInfoObituary(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemHitObituary_R(Self: TRTLMobjInfoItem; var T: string);
begin
  T := PS_GetMobjInfoHitObituary(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemGender_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoGender(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemMeleeRange_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoMeleeRange(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemMaxStepHeight_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoMaxStepHeight(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemMaxDropOffHeight_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoMaxDropOffHeight(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemGibHealth_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoGibHealth(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemMaxTargetRange_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoMaxTargetRange(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemWeaveIndexXY_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoWeaveIndexXY(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemWeaveIndexZ_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoWeaveIndexZ(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemFriction_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoFriction(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemSpriteDX_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoSpriteDX(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemSpriteDY_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoSpriteDY(Integer(Self) - 1);
end;

procedure TRTLMobjInfoItemInteractState_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoInteractState(Integer(Self) - 1);
end;

{$IFDEF DOOM_OR_STRIFE}
procedure TRTLMobjInfoItemMissileHeight_R(Self: TRTLMobjInfoItem; var T: integer);
begin
  T := PS_GetMobjInfoMissileHeight(Integer(Self) - 1);
end;
{$ENDIF}

// ------------------------------ GAME -----------------------------------------

function PS_GameMap: integer;
begin
  Result := gamemap;
end;

{$IFDEF DOOM_OR_HERETIC}
function PS_GameEpisode: integer;
begin
  Result := gameepisode;
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
    begin
      players[i].quaketics := qtics;
      players[i].quakeintensity := FRACUNIT;
    end;
end;

procedure PS_LocalEarthQuake(const x, y, z: Integer; const tics: Integer; const intensity: Integer; const maxdist: Integer);
var
  i: integer;
  dist: fixed_t;
  frac: fixed_t;
  testintensity: fixed_t;
begin
  for i := 0 to MAXPLAYERS - 1 do
    if playeringame[i] then
    begin
      dist := P_AproxDistance(x - players[i].mo.x, y - players[i].mo.y);
      dist := P_AproxDistance(z - players[i].mo.z, dist); // 3d distance
      if dist <= maxdist then
      begin
        if players[i].quaketics < tics then
          players[i].quaketics := tics;
        frac := FixedDiv(dist, maxdist) * (FINEANGLES div 4);
        testintensity := FixedMul(finecosine[frac shr ANGLETOFINESHIFT], intensity); // JVAL: 20200508 - Curved
        if players[i].quakeintensity < testintensity then
          players[i].quakeintensity := testintensity;
      end;
    end;
end;

function PS_GameSkill: integer;
begin
  Result := Ord(gameskill);
end;

// ------------------ KEYBOARD CONTROL -----------------------------------------
function PS_gamekeydown(const kkey: integer): boolean;
begin
  if demoplayback or demorecording or netgame then
    Result := False
  else if IsIntegerInRange(kkey, 0, NUMKEYS - 1) then
    Result := gamekeydown[kkey]
  else
    Result := False;
end;

function PS_mousebuttons(const mkey: integer): boolean;
begin
  if demoplayback or demorecording or netgame then
    Result := False
  else if IsIntegerInRange(mkey, 0, 2) then
    Result := mousebuttons[mkey]
  else
    Result := False;
end;

function PS_joybuttons(const jkey: integer): boolean;
begin
  if demoplayback or demorecording or netgame then
    Result := False
  else if IsIntegerInRange(jkey, 0, NUMJOYBUTTONS - 1) then
    Result := joybuttons[jkey]
  else
    Result := False;
end;

function PS_key_right: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_right;
end;

function PS_key_left: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_left;
end;

function PS_key_up: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_up;
end;

function PS_key_down: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_down;
end;

function PS_key_lookup: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_lookup;
end;

function PS_key_lookdown: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_lookdown;
end;

function PS_key_lookcenter: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_lookcenter;
end;

function PS_key_lookright: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_lookright;
end;

function PS_key_lookleft: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_lookleft;
end;

{$IFNDEF STRIFE}
function PS_key_lookforward: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_lookforward;
end;
{$ENDIF}

function PS_key_strafeleft: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_strafeleft;
end;

function PS_key_straferight: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_straferight;
end;

function PS_key_fire: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_fire;
end;

function PS_key_use: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_use;
end;

function PS_key_strafe: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_strafe;
end;

function PS_key_speed: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_speed;
end;

function PS_key_jump: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_jump;
end;

// JVAL: 20211101 - Crouch
function PS_key_crouch: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_crouch;
end;

function PS_key_weapon0: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_weapon0;
end;

function PS_key_weapon1: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_weapon1;
end;

function PS_key_weapon2: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_weapon2;
end;

function PS_key_weapon3: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_weapon3;
end;

{$IFNDEF HEXEN}
function PS_key_weapon4: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_weapon4;
end;

function PS_key_weapon5: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_weapon5;
end;

function PS_key_weapon6: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_weapon6;
end;

function PS_key_weapon7: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_weapon7;
end;
{$ENDIF}

{$IFDEF HERETIC_OR_HEXEN}
function PS_key_flyup: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_flyup;
end;

function PS_key_flydown: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_flydown;
end;

function PS_key_flycenter: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_flycenter;
end;

function PS_key_invleft: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_invleft;
end;

function PS_key_invright: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_invright;
end;

function PS_key_useartifact: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_useartifact;
end;
{$ENDIF}

{$IFDEF STRIFE}
function PS_key_invleft: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_invleft;
end;

function PS_key_invright: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_invright;
end;

function PS_key_weapon8: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_weapon8;
end;

function PS_key_weapon9: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_weapon9;
end;

function PS_key_usehealth: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_usehealth;
end;

function PS_key_invquery: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_invquery;
end;

function PS_key_mission: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_mission;
end;

function PS_key_invpop: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_invpop;
end;

function PS_key_invkey: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_invkey;
end;

function PS_key_invhome: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_invhome;
end;

function PS_key_invend: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_invend;
end;

function PS_key_invuse: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_invuse;
end;

function PS_key_invdrop: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := key_invdrop;
end;
{$ENDIF}

function PS_mousebfire: integer;
begin
  Result := mousebfire;
end;

function PS_mousebstrafe: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := mousebstrafe;
end;

function PS_mousebforward: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := mousebforward;
end;

function PS_joybfire: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := joybfire;
end;

function PS_joybstrafe: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := joybstrafe;
end;

function PS_joybuse: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := joybuse;
end;

function PS_joybspeed: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := joybspeed;
end;

function PS_joybjump: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := joybjump;
end;

// JVAL: 20211101 - Crouch
function PS_joybcrouch: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := joybcrouch;
end;

function PS_joyblleft: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := joyblleft;
end;

function PS_joyblright: integer;
begin
  if demoplayback or demorecording or netgame then
    Result := 0
  else
    Result := joyblright;
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
  cmobjinfoitem: TPSCompileTimeClass;
  cmobjinfo: TPSCompileTimeClass;
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
  cmobjinfoitem := C.AddClassN(C.FindClass('!TOBJECT'),'!TMobjInfoItem');
  cmobjinfo := C.AddClassN(C.FindClass('!TOBJECT'),'!TMobjInfo');

  cactor.RegisterProperty('key', 'LongWord', iptR);
  cactor.RegisterProperty('Target', '!TActor', iptRW);
  cactor.RegisterProperty('Tracer', '!TActor', iptRW);
  cactor.RegisterProperty('Master', '!TActor', iptRW);
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
  cactor.RegisterProperty('Mass', 'Integer', iptRW);
  cactor.RegisterProperty('Gravity', 'fixed_t', iptRW);
  cactor.RegisterProperty('Special', 'Integer', iptRW);
  cactor.RegisterProperty('Arg1', 'Integer', iptRW);
  cactor.RegisterProperty('Arg2', 'Integer', iptRW);
  cactor.RegisterProperty('Arg3', 'Integer', iptRW);
  cactor.RegisterProperty('Arg4', 'Integer', iptRW);
  cactor.RegisterProperty('Arg5', 'Integer', iptRW);
  cactor.RegisterProperty('Height', 'fixed_t', iptRW);
  cactor.RegisterProperty('WeaveIndexXY', 'Integer', iptRW);
  cactor.RegisterProperty('WeaveIndexZ', 'Integer', iptRW);
  cactor.RegisterProperty('Friction', 'fixed_t', iptRW);
  cactor.RegisterProperty('PainChance', 'fixed_t', iptRW);
  cactor.RegisterProperty('SpriteDX', 'fixed_t', iptRW);
  cactor.RegisterProperty('SpriteDY', 'fixed_t', iptRW);
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
  cactor.RegisterProperty('State', 'Integer', iptRW);

  cactor.RegisterMethod('procedure PlaySound(const snd: string);');
  cactor.RegisterMethod('procedure Remove;');
  cactor.RegisterMethod('procedure SetPosition(const x, y, z: fixed_t)');

  cactors.RegisterMethod('function AllActors: TActorArray;');
  cactors.RegisterMethod('function AllMonstersAlive: TActorArray;');
  cactors.RegisterMethod('function AllMonstersDead: TActorArray;');
  cactors.RegisterMethod('function AllMonsters: TActorArray;');
  cactors.RegisterMethod('function AllMissiles: TActorArray;');
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
  csector.RegisterProperty('FloorAngle', 'angle_t', iptRW);
  csector.RegisterProperty('FloorAngleX', 'fixed_t', iptRW);
  csector.RegisterProperty('FloorAngleY', 'fixed_t', iptRW);
  csector.RegisterProperty('CeilingAngle', 'angle_t', iptRW);
  csector.RegisterProperty('CeilingAngleX', 'fixed_t', iptRW);
  csector.RegisterProperty('CeilingAngleY', 'fixed_t', iptRW);
  csector.RegisterProperty('RippleFloor', 'boolean', iptRW);
  csector.RegisterProperty('RippleCeiling', 'boolean', iptRW);
  csector.RegisterProperty('Interpolate', 'boolean', iptRW);
  csector.RegisterProperty('Fog', 'boolean', iptRW);
  csector.RegisterProperty('Gravity', 'fixed_t', iptRW);
  csector.RegisterMethod('procedure PlaySound(const snd: string);');
  csector.RegisterMethod('procedure MoveZ(const dz: fixed_t);');
  csector.RegisterMethod('procedure SetFloorSlope(const x1, y1, z1: fixed_t; const x2, y2, z2: fixed_t; const x3, y3, z3: fixed_t);');
  csector.RegisterMethod('procedure SetCeilingSlope(const x1, y1, z1: fixed_t; const x2, y2, z2: fixed_t; const x3, y3, z3: fixed_t);');

  csectors.RegisterProperty('Sector', '!TSector Integer', iptR);
  csectors.SetDefaultPropery('Sector');
  csectors.RegisterProperty('Count', 'Integer', iptR);

  cmobjinfoitem.RegisterProperty('Name', 'string', iptR);
  cmobjinfoitem.RegisterProperty('Name2', 'string', iptR);
  cmobjinfoitem.RegisterProperty('InheritsFrom', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('DoomEdNum', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('SpawnState', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('SpawnHealth', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('SeeState', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('SeeSound', 'string', iptR);
  cmobjinfoitem.RegisterProperty('ReactionTime', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('AttackSound', 'string', iptR);
  cmobjinfoitem.RegisterProperty('PainState', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('PainChance', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('PainSound', 'string', iptR);
  cmobjinfoitem.RegisterProperty('MeleeState', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('MissileState', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('DeathState', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('XdeathState', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('DeathSound', 'string', iptR);
  cmobjinfoitem.RegisterProperty('Speed', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('Radius', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('Height', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('Mass', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('Damage', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('ActiveSound', 'string', iptR);
  cmobjinfoitem.RegisterProperty('Flag', 'Boolean Integer', iptR);
  cmobjinfoitem.RegisterProperty('RaiseState', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('CustomSound1', 'string', iptR);
  cmobjinfoitem.RegisterProperty('CustomSound2', 'string', iptR);
  cmobjinfoitem.RegisterProperty('CustomSound3', 'string', iptR);
  cmobjinfoitem.RegisterProperty('MeleeSound', 'string', iptR);
  cmobjinfoitem.RegisterProperty('DropItem', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('MissileType', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('ExplosionDamage', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('ExplosionRadius', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('MeleeDamage', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('RenderStyle', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('Alpha', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('HealState', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('CrashState', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('InteractState', 'Integer', iptR);
{$IFDEF DOOM_OR_STRIFE}
  cmobjinfoitem.RegisterProperty('MissileHeight', 'Integer', iptR);
{$ENDIF}
  cmobjinfoitem.RegisterProperty('VSpeed', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('MinMissileChance', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('PushFactor', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('Scale', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('Gravity', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('FloatSpeed', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('NormalSpeed', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('FastSpeed', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('Obituary', 'String', iptR);
  cmobjinfoitem.RegisterProperty('HitObituary', 'String', iptR);
  cmobjinfoitem.RegisterProperty('Gender', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('MeleeRange', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('MaxStepHeight', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('MaxDropOffHeight', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('GibHealth', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('MaxTargetRange', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('WeaveIndexXY', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('WeaveIndexZ', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('Friction', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('SpriteDX', 'Integer', iptR);
  cmobjinfoitem.RegisterProperty('SpriteDY', 'Integer', iptR);

  cmobjinfo.RegisterProperty('Item', '!TMobjInfoItem integer', iptR);
  cmobjinfo.SetDefaultPropery('Item');
  cmobjinfo.RegisterProperty('Count', 'Integer', iptR);

  AddImportedClassVariable(C, 'Actors', '!TActors');
  AddImportedClassVariable(C, 'Vertexes', '!TVertexes');
  AddImportedClassVariable(C, 'Sides', '!TSides');
  AddImportedClassVariable(C, 'Lines', '!TLines');
  AddImportedClassVariable(C, 'Sectors', '!TSectors');
  AddImportedClassVariable(C, 'MobjInfo', '!TMobjInfo');
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
  rmobjinfoitem: TPSRuntimeClass;
  rmobjinfo: TPSRuntimeClass;
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
  rmobjinfoitem := CLI.Add2(TRTLMobjInfoItem, '!TMOBJINFOITEM');
  rmobjinfo := CLI.Add2(TRTLMobjInfo, '!TMOBJINFO');

  ractor.RegisterPropertyHelper(@TRTLActorkey_R, nil, 'key');
  ractor.RegisterPropertyHelper(@TRTLActorTarget_R, @TRTLActorTarget_W, 'Target');
  ractor.RegisterPropertyHelper(@TRTLActorTracer_R, @TRTLActorTracer_W, 'Tracer');
  ractor.RegisterPropertyHelper(@TRTLActorMaster_R, @TRTLActorMaster_W, 'Master');
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
  ractor.RegisterPropertyHelper(@TRTLActorMass_R, @TRTLActorMass_W, 'Mass');
  ractor.RegisterPropertyHelper(@TRTLActorHeight_R, @TRTLActorHeight_W, 'Height');
  ractor.RegisterPropertyHelper(@TRTLActorCustomDropItem_R, @TRTLActorCustomDropItem_W, 'CustomDropItem');
  ractor.RegisterPropertyHelper(@TRTLActorPushFactor_R, @TRTLActorPushFactor_W, 'PushFactor');
  ractor.RegisterPropertyHelper(@TRTLActorScale_R, @TRTLActorScale_W, 'Scale');
  ractor.RegisterPropertyHelper(@TRTLActorGravity_R, @TRTLActorGravity_W, 'Gravity');
  ractor.RegisterPropertyHelper(@TRTLActorSpecial_R, @TRTLActorSpecial_W, 'Special');
  ractor.RegisterPropertyHelper(@TRTLActorArg1_R, @TRTLActorArg1_W, 'Arg1');
  ractor.RegisterPropertyHelper(@TRTLActorArg2_R, @TRTLActorArg2_W, 'Arg2');
  ractor.RegisterPropertyHelper(@TRTLActorArg3_R, @TRTLActorArg3_W, 'Arg3');
  ractor.RegisterPropertyHelper(@TRTLActorArg4_R, @TRTLActorArg4_W, 'Arg4');
  ractor.RegisterPropertyHelper(@TRTLActorArg5_R, @TRTLActorArg5_W, 'Arg5');
  ractor.RegisterPropertyHelper(@TRTLActorWeaveIndexXY_R, @TRTLActorWeaveIndexXY_W, 'WeaveIndexXY');
  ractor.RegisterPropertyHelper(@TRTLActorWeaveIndexZ_R, @TRTLActorWeaveIndexZ_W, 'WeaveIndexZ');
  ractor.RegisterPropertyHelper(@TRTLActorFriction_R, @TRTLActorFriction_W, 'Friction');
  ractor.RegisterPropertyHelper(@TRTLActorPainChance_R, @TRTLActorPainChance_W, 'PainChance');
  ractor.RegisterPropertyHelper(@TRTLActorSpriteDX_R, @TRTLActorSpriteDX_W, 'SpriteDX');
  ractor.RegisterPropertyHelper(@TRTLActorSpriteDY_R, @TRTLActorSpriteDY_W, 'SpriteDY');
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
  ractor.RegisterPropertyHelper(@TRTLActorState_R, @TRTLActorState_W, 'State');
  ractor.RegisterMethod(@TRTLActor.PlaySound, 'PlaySound');
  ractor.RegisterMethod(@TRTLActor.Remove, 'Remove');
  ractor.RegisterMethod(@TRTLActor.SetPosition, 'SetPosition');

  ractors.RegisterMethod(@TRTLActors.AllActors, 'AllActors');
  ractors.RegisterMethod(@TRTLActors.AllMonstersAlive, 'AllMonstersAlive');
  ractors.RegisterMethod(@TRTLActors.AllMonstersDead, 'AllMonstersDead');
  ractors.RegisterMethod(@TRTLActors.AllMonsters, 'AllMonsters');
  ractors.RegisterMethod(@TRTLActors.AllMissiles, 'AllMissiles');
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
  rsector.RegisterPropertyHelper(@TRTLSectorFloorAngle_R, @TRTLSectorFloorAngle_W, 'FloorAngle');
  rsector.RegisterPropertyHelper(@TRTLSectorFloorAngleX_R, @TRTLSectorFloorAngleX_W, 'FloorAngleX');
  rsector.RegisterPropertyHelper(@TRTLSectorFloorAngleY_R, @TRTLSectorFloorAngleY_W, 'FloorAngleY');
  rsector.RegisterPropertyHelper(@TRTLSectorCeilingAngle_R, @TRTLSectorCeilingAngle_W, 'CeilingAngle');
  rsector.RegisterPropertyHelper(@TRTLSectorCeilingAngleX_R, @TRTLSectorCeilingAngleX_W, 'CeilingAngleX');
  rsector.RegisterPropertyHelper(@TRTLSectorCeilingAngleY_R, @TRTLSectorCeilingAngleY_W, 'CeilingAngleY');
  rsector.RegisterPropertyHelper(@TRTLSectorRippleFloor_R, @TRTLSectorRippleFloor_W, 'RippleFloor');
  rsector.RegisterPropertyHelper(@TRTLSectorRippleCeiling_R, @TRTLSectorRippleCeiling_W, 'RippleCeiling');
  rsector.RegisterPropertyHelper(@TRTLSectorInterpolate_R, @TRTLSectorInterpolate_W, 'Interpolate');
  rsector.RegisterPropertyHelper(@TRTLSectorFog_R, @TRTLSectorFog_W, 'Fog');
  rsector.RegisterPropertyHelper(@TRTLSectorGravity_R, @TRTLSectorGravity_W, 'Gravity');
  rsector.RegisterMethod(@TRTLSector.PlaySound, 'PlaySound');
  rsector.RegisterMethod(@TRTLSector.MoveZ, 'MoveZ');
  rsector.RegisterMethod(@TRTLSector.SetFloorSlope, 'SetFloorSlope');
  rsector.RegisterMethod(@TRTLSector.SetCeilingSlope, 'SetCeilingSlope');

  rsectors.RegisterPropertyHelper(@TRTLSectorsSector_R, nil, 'Sector');
  rsectors.RegisterPropertyHelper(@TRTLSectorsCount_R, nil, 'Count');

  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemName_R, nil, 'Name');
  {$IFDEF STRIFE}
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemName2_R, nil, 'Name2');
  {$ENDIF}
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemInheritsFrom_R, nil, 'InheritsFrom');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemDoomEdNum_R, nil, 'DoomEdNum');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemSpawnState_R, nil, 'SpawnState');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemSpawnHealth_R, nil, 'SpawnHealth');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemSeeState_R, nil, 'SeeState');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemSeeSound_R, nil, 'SeeSound');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemReactionTime_R, nil, 'ReactionTime');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemAttackSound_R, nil, 'AttackSound');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemPainState_R, nil, 'PainState');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemPainChance_R, nil, 'PainChance');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemPainSound_R, nil, 'PainSound');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemMeleeState_R, nil, 'MeleeState');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemMissileState_R, nil, 'MissileState');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemDeathState_R, nil, 'DeathState');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemXdeathState_R, nil, 'XdeathState');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemDeathSound_R, nil, 'DeathSound');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemSpeed_R, nil, 'Speed');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemVSpeed_R, nil, 'VSpeed');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemMinMissileChance_R, nil, 'MinMissileChance');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemPushFactor_R, nil, 'PushFactor');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemScale_R, nil, 'Scale');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemGravity_R, nil, 'Gravity');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemRadius_R, nil, 'Radius');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemHeight_R, nil, 'Height');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemMass_R, nil, 'Mass');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemDamage_R, nil, 'Damage');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemActiveSound_R, nil, 'ActiveSound');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemFlag_R, nil, 'Flag');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemRaiseState_R, nil, 'RaiseState');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemCustomSound1_R, nil, 'CustomSound1');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemCustomSound2_R, nil, 'CustomSound2');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemCustomSound3_R, nil, 'CustomSound3');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemMeleeSound_R, nil, 'MeleeSound');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemDropItem_R, nil, 'DropItem');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemMissileType_R, nil, 'MissileType');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemExplosionDamage_R, nil, 'ExplosionDamage');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemExplosionRadius_R, nil, 'ExplosionRadius');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemMeleeDamage_R, nil, 'MeleeDamage');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemRenderStyle_R, nil, 'RenderStyle');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemAlpha_R, nil, 'Alpha');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemHealState_R, nil, 'HealState');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemCrashState_R, nil, 'CrashState');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemFloatSpeed_R, nil, 'FloatSpeed');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemNormalSpeed_R, nil, 'NormalSpeed');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemFastSpeed_R, nil, 'FastSpeed');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemObituary_R, nil, 'Obituary');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemHitObituary_R, nil, 'HitObituary');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemGender_R, nil, 'Gender');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemMeleeRange_R, nil, 'MeleeRange');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemMaxStepHeight_R, nil, 'MaxStepHeight');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemMaxDropOffHeight_R, nil, 'MaxDropOffHeight');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemGibHealth_R, nil, 'GibHealth');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemMaxTargetRange_R, nil, 'MaxTargetRange');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemWeaveIndexXY_R, nil, 'WeaveIndexXY');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemWeaveIndexZ_R, nil, 'WeaveIndexZ');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemFriction_R, nil, 'Friction');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemSpriteDX_R, nil, 'SpriteDX');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemSpriteDY_R, nil, 'SpriteDY');
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemInteractState_R, nil, 'InteractState');
  {$IFDEF DOOM_OR_STRIFE}
  rmobjinfoitem.RegisterPropertyHelper(@TRTLMobjInfoItemMissileHeight_R, nil, 'MissileHeight');
  {$ENDIF}

  rmobjinfo.RegisterPropertyHelper(@TRTLMobjInfoItem_R, nil, 'Item');
  rmobjinfo.RegisterPropertyHelper(@TRTLMobjInfoCount_R, nil, 'Count');

end;

procedure RIRegisterRTL_Game(Exec: TPSExec);
begin
  SetVariantToClass(Exec.GetVarNo(Exec.GetVar('Actors')), rtlactors);
  SetVariantToClass(Exec.GetVarNo(Exec.GetVar('Vertexes')), rtlvertexes);
  SetVariantToClass(Exec.GetVarNo(Exec.GetVar('Sides')), rtlsides);
  SetVariantToClass(Exec.GetVarNo(Exec.GetVar('Lines')), rtllines);
  SetVariantToClass(Exec.GetVarNo(Exec.GetVar('Sectors')), rtlsectors);
  SetVariantToClass(Exec.GetVarNo(Exec.GetVar('MobjInfo')), rtlmobjinfo);
end;

procedure PS_InitGameImport;
begin
  rtlactors := TRTLActors.Create;
  rtlvertexes := TRTLVertexes.Create;
  rtlsides := TRTLSides.Create;
  rtllines := TRTLLines.Create;
  rtlsectors := TRTLSectors.Create;
  rtlmobjinfo := TRTLMobjInfo.Create;
end;

procedure PS_ShutDownGameImport;
begin
  rtlactors.Free;
  rtlvertexes.Free;
  rtlsides.Free;
  rtllines.Free;
  rtlsectors.Free;
  rtlmobjinfo.Free;
end;

end.

