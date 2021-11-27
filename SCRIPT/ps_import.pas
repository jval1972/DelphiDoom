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
//  Pascal Script import definitions.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit ps_import;

interface

uses
  d_delphi,
  ps_defs,
  ps_compiler,
  uPSPreProcessor,
  ps_runtime,
  ps_utils;

procedure PS_InitProcLists;

procedure PS_ShutDownProcLists;


type
  TDoomCompiler = class(TPSPascalCompiler)
  private
    FPP: TPSPreProcessor;
    funitnames: TDStringList;
  public
    constructor CreateDoomCompiler; virtual;
    function CompileDoomScript(const source: TbtString; var pcode: string): Boolean; virtual;
    destructor Destroy; override;
    property unitnames: TDStringList read funitnames;
  end;

  TDoomExec = class(TPSExec)
  public
    constructor CreateDoomExec(const aImporter: TPSRuntimeClassImporter); virtual;
    function LoadData(const pcode: TbtString): Boolean; override;
  end;

  TPSDoomRuntimeClassImporter = class(TPSRuntimeClassImporter)
  public
    constructor Create; override;
  end;

const
  ID_DDPS = 1397769284; // 'DDPS'

function PS_ImportUnits: TObject;

function PS_ScriptOnUses(Sender: TPSPascalCompiler; const Name: string): Boolean;

implementation

uses
  Classes,
  TypInfo,
  uPSC_dateutils,
  uPSC_dll,
  uPSR_dateutils,
  uPSR_dll,
  ps_proclist,
  psi_game,
  psi_overlay,
  psi_system,
  psi_globals,
  doomdef,
  deh_main,
  d_main,
  g_game,
  info_h,
  i_system,
  m_base,
  m_fixed,
  m_rnd,
  p_gender,
  p_local,
  p_maputl,
  p_slopes,
  {$IFDEF HEXEN}
  p_setup,
  {$ENDIF}
  r_main,
  r_renderstyle,
  sc_states,
  tables,
  w_pak,
  ddc_base,
  ps_keywords;

var
  units: TStringList;

procedure PS_InitProcLists;
var
  baseproclist: TProcedureList;
  basename: string;
begin
  units := TStringList.Create;

  basename := 'SYSTEM';
  baseproclist := TProcedureList.Create(basename);
  baseproclist.Add('procedure OutputDebugString(const parm: string);', @PS_OutputDebugString);
  baseproclist.Add('procedure OutputDebugStringFmt(const Fmt: string; const args: array of const);', @PS_OutputDebugStringFmt);
  baseproclist.Add('procedure BreakPoint(const msg: string);', @PS_BreakPoint);
  baseproclist.Add('procedure ConsoleCommand(Data: string)', @PS_ConsoleCommand);
  baseproclist.Add('function GetConsoleStr(const cvar: string): string;', @PS_GetConsoleStr);
  baseproclist.Add('function GetConsoleInt(const cvar: string): integer;', @PS_GetConsoleInt);
  baseproclist.Add('function GetConsoleBool(const cvar: string): boolean;', @PS_GetConsoleBool);
  baseproclist.Add('procedure Write(Data: string)', @PS_Write);
  baseproclist.Add('procedure WriteFmt(const Fmt: string; const args: array of const);', @PS_WriteFmt);
  baseproclist.Add('procedure Writeln(Data: string)', @PS_Writeln);
  baseproclist.Add('procedure WritelnFmt(const Fmt: string; const args: array of const);', @PS_WritelnFmt);
  baseproclist.Add('function LevelTime: integer', @PS_Leveltime);
  baseproclist.Add('function FineSine(const parm: LongWord): Integer', @PS_FineSine);
  baseproclist.Add('function FineCosine(const parm: LongWord): Integer', @PS_FineCosine);
  baseproclist.Add('function FineTangent(const parm: LongWord): Integer', @PS_FineTangent);
  baseproclist.Add('function GetMapString(const x: string): string;', @PS_GetMapStr);
  baseproclist.Add('procedure SetMapString(const x: string; const v: string);', @PS_SetMapStr);
  baseproclist.Add('function GetMapInteger(const x: string): integer;', @PS_GetMapInt);
  baseproclist.Add('procedure SetMapInteger(const x: string; const v: integer);', @PS_SetMapInt);
  baseproclist.Add('function GetMapFloat(const x: string): single;', @PS_GetMapFloat);
  baseproclist.Add('procedure SetMapFloat(const x: string; const v: single);', @PS_SetMapFloat);
  baseproclist.Add('function GetWorldString(const x: string): string;', @PS_GetWorldStr);
  baseproclist.Add('procedure SetWorldString(const x: string; const v: string);', @PS_SetWorldStr);
  baseproclist.Add('function GetWorldInteger(const x: string): integer;', @PS_GetWorldInt);
  baseproclist.Add('procedure SetWorldInteger(const x: string; const v: integer);', @PS_SetWorldInt);
  baseproclist.Add('function GetWorldFloat(const x: string): single;', @PS_GetWorldFloat);
  baseproclist.Add('procedure SetWorldFloat(const x: string; const v: single);', @PS_SetWorldFloat);
  baseproclist.Add('function Tan(const parm: Extended): Extended;', @PS_Tan);
  baseproclist.Add('function Sin360(const parm: Extended): Extended;', @PS_Sin360);
  baseproclist.Add('function Cos360(const parm: Extended): Extended;', @PS_Cos360);
  baseproclist.Add('function Tan360(const parm: Extended): Extended;', @PS_Tan360);
  baseproclist.Add('function P_Random: integer;', @P_Random);
  baseproclist.Add('function Format(const Fmt: string; const args: array of const): string;', @PS_Format);
  baseproclist.Add('function IsIntegerInRange(const test, i1, i2: integer): boolean;', @IsIntegerInRange);
  baseproclist.Add('function IsLongWordInRange(const test, i1, i2: LongWord): boolean;', @IsLongWordInRange);
  baseproclist.Add('function IsSingleInRange(const test, f1, f2: single): boolean;', @IsFloatInRange);
  baseproclist.Add('function IsFloatInRange(const test, f1, f2: single): boolean;', @IsFloatInRange);
  baseproclist.Add('function IsDoubleInRange(const test, f1, f2: double): boolean;', @IsDoubleInRange);
  baseproclist.Add('function IsExtendedInRange(const test, f1, f2: Extended): boolean;', @IsExtendedInRange);
  baseproclist.Add('function IFI(const condition: boolean; const iftrue, iffalse: Int64): Int64;', @PS_IFI);
  baseproclist.Add('function IFF(const condition: boolean; const iftrue, iffalse: Extended): Extended;', @PS_IFF);
  baseproclist.Add('function IFS(const condition: boolean; const iftrue, iffalse: string): string;', @PS_IFS);
  baseproclist.Add('function Odd(const x: integer): boolean;', @PS_Odd);
  baseproclist.Add('function Even(const x: integer): boolean;', @PS_Even);
  baseproclist.Add('function MergeIntegerArrays(const A1, A2: TIntegerArray): TIntegerArray;', @PS_MergeIntegerArrays);
  baseproclist.Add('function MergeInt64Arrays(const A1, A2: TInt64Array): TInt64Array;', @PS_MergeInt64Arrays);
  baseproclist.Add('function MergeLongWordArrays(const A1, A2: TLongWordArray): TLongWordArray;', @PS_MergeLongWordArrays);
  baseproclist.Add('function MergeSingleArrays(const A1, A2: TSingleArray): TSingleArray;', @PS_MergeSingleArrays);
  baseproclist.Add('function MergeFloatArrays(const A1, A2: TFloatArray): TFloatArray;', @PS_MergeSingleArrays);
  baseproclist.Add('function MergeDoubleArrays(const A1, A2: TDoubleArray): TDoubleArray;', @PS_MergeDoubleArrays);
  baseproclist.Add('function MergeExtendedArrays(const A1, A2: TExtendedArray): TExtendedArray;', @PS_MergeExtendedArrays);
  baseproclist.Add('function IsPrime(const N: Int64): Boolean;', @PS_IsPrime);
  baseproclist.Add('function RandomRange(const x1, x2: integer): integer;', @PS_RandomRange);
  units.AddObject(basename, baseproclist);

  basename := 'PS_GAME';
  baseproclist := TProcedureList.Create(basename);
  // --------------------------- Actors ----------------------------------------
  baseproclist.Add('function Actor: LongWord;', @PS_Actor);
  baseproclist.AddWithCustomResult('procedure TActor(const key: LongWord);', '!TActor', 'function TActor(const key: LongWord): !TActor;', @PS_TActor);
  baseproclist.Add('function GetActorTarget(const key: LongWord): LongWord;', @PS_GetActorTarget);
  baseproclist.Add('procedure SetActorTarget(const key: LongWord; const targ: LongWord);', @PS_SetActorTarget);
  baseproclist.Add('function GetActorTracer(const key: LongWord): LongWord;', @PS_GetActorTracer);
  baseproclist.Add('procedure SetActorTracer(const key: LongWord; const targ: LongWord);', @PS_SetActorTracer);
  baseproclist.Add('function GetActorMaster(const key: LongWord): LongWord;', @PS_GetActorMaster);
  baseproclist.Add('procedure SetActorMaster(const key: LongWord; const targ: LongWord);', @PS_SetActorMaster);
  baseproclist.Add('function GetActorX(const key: LongWord): fixed_t;', @PS_GetActorX);
  baseproclist.Add('procedure SetActorX(const key: LongWord; const x: fixed_t);', @PS_SetActorX);
  baseproclist.Add('function GetActorY(const key: LongWord): fixed_t;', @PS_GetActorY);
  baseproclist.Add('procedure SetActorY(const key: LongWord; const y: fixed_t);', @PS_SetActorY);
  baseproclist.Add('function GetActorZ(const key: LongWord): fixed_t;', @PS_GetActorZ);
  baseproclist.Add('procedure SetActorZ(const key: LongWord; const z: fixed_t);', @PS_SetActorZ);
  baseproclist.Add('procedure SetActorPosition(const key: LongWord; const x, y, z: fixed_t);', @PS_SetActorPosition);
  baseproclist.Add('function GetActorMOMX(const key: LongWord): fixed_t;', @PS_GetActorMOMX);
  baseproclist.Add('procedure SetActorMOMX(const key: LongWord; const x: fixed_t);', @PS_SetActorMOMX);
  baseproclist.Add('function GetActorMOMY(const key: LongWord): fixed_t;', @PS_GetActorMOMY);
  baseproclist.Add('procedure SetActorMOMY(const key: LongWord; const y: fixed_t);', @PS_SetActorMOMY);
  baseproclist.Add('function GetActorMOMZ(const key: LongWord): fixed_t;', @PS_GetActorMOMZ);
  baseproclist.Add('procedure SetActorMOMZ(const key: LongWord; const z: fixed_t);', @PS_SetActorMOMZ);
  baseproclist.Add('function GetActorFloorZ(const key: LongWord): fixed_t;', @PS_GetActorFloorZ);
  baseproclist.Add('procedure SetActorFloorZ(const key: LongWord; const z: fixed_t);', @PS_SetActorFloorZ);
  baseproclist.Add('function GetActorCeilingZ(const key: LongWord): fixed_t;', @PS_GetActorCeilingZ);
  baseproclist.Add('procedure SetActorCeilingZ(const key: LongWord; const z: fixed_t);', @PS_SetActorCeilingZ);
  baseproclist.Add('function GetActorSpeed(const key: LongWord): fixed_t;', @PS_GetActorSpeed);
  baseproclist.Add('procedure SetActorSpeed(const key: LongWord; const speed: fixed_t);', @PS_SetActorSpeed);
  baseproclist.Add('function GetActorAngle(const key: LongWord): angle_t;', @PS_GetActorAngle);
  baseproclist.Add('procedure SetActorAngle(const key: LongWord; const angle: angle_t);', @PS_SetActorAngle);
  baseproclist.Add('function GetActorSector(const key: LongWord): integer;', @PS_GetActorSector);
  baseproclist.Add('function GetActorHealth(const key: LongWord): integer;', @PS_GetActorHealth);
  baseproclist.Add('procedure SetActorHealth(const key: LongWord; const h: integer);', @PS_SetActorHealth);
  baseproclist.Add('function GetActorSpawnHealth(const key: LongWord): integer;', @PS_GetActorSpawnHealth);
  baseproclist.Add('function GetActorMass(const key: LongWord): integer;', @PS_GetActorMass);
  baseproclist.Add('procedure SetActorMass(const key: LongWord; const m: Integer);', @PS_SetActorMass);
  baseproclist.Add('function GetActorHeight(const key: LongWord): fixed_t;', @PS_GetActorHeight);
  baseproclist.Add('procedure SetActorHeight(const key: LongWord; const h: fixed_t);', @PS_SetActorHeight);
  baseproclist.Add('function GetActorCustomDropItem(const key: LongWord): integer;', @PS_GetActorCustomDropItem);
  baseproclist.Add('procedure SetActorCustomDropItem(const key: LongWord; const value: integer);', @PS_SetActorCustomDropItem);
  baseproclist.Add('procedure SetActorDefaultDropItem(const key: LongWord);', @PS_SetActorDefaultDropItem);
  baseproclist.Add('function GetActorPushFactor(const key: LongWord): integer;', @PS_GetActorPushFactor);
  baseproclist.Add('procedure SetActorPushFactor(const key: LongWord; const value: integer);', @PS_SetActorPushFactor);
  baseproclist.Add('function GetActorScale(const key: LongWord): integer;', @PS_GetActorScale);
  baseproclist.Add('procedure SetActorScale(const key: LongWord; const value: integer);', @PS_SetActorScale);
  baseproclist.Add('function GetActorGravity(const key: LongWord): integer;', @PS_GetActorGravity);
  baseproclist.Add('procedure SetActorGravity(const key: LongWord; const value: integer);', @PS_SetActorGravity);
  baseproclist.Add('function GetActorSpecial(const key: LongWord): Integer;', @PS_GetActorSpecial);
  baseproclist.Add('procedure SetActorSpecial(const key: LongWord; const value: Integer);', @PS_SetActorSpecial);
  baseproclist.Add('function GetActorArg1(const key: LongWord): integer;', @PS_GetActorArg1);
  baseproclist.Add('procedure SetActorArg1(const key: LongWord; const value: integer);', @PS_SetActorArg1);
  baseproclist.Add('function GetActorArg2(const key: LongWord): integer;', @PS_GetActorArg2);
  baseproclist.Add('procedure SetActorArg2(const key: LongWord; const value: integer);', @PS_SetActorArg2);
  baseproclist.Add('function GetActorArg3(const key: LongWord): integer;', @PS_GetActorArg3);
  baseproclist.Add('procedure SetActorArg3(const key: LongWord; const value: integer);', @PS_SetActorArg3);
  baseproclist.Add('function GetActorArg4(const key: LongWord): integer;', @PS_GetActorArg4);
  baseproclist.Add('procedure SetActorArg4(const key: LongWord; const value: integer);', @PS_SetActorArg4);
  baseproclist.Add('function GetActorArg5(const key: LongWord): integer;', @PS_GetActorArg5);
  baseproclist.Add('procedure SetActorArg5(const key: LongWord; const value: integer);', @PS_SetActorArg5);
  baseproclist.Add('function GetActorWeaveIndexXY(const key: LongWord): Integer;);', @PS_GetActorWeaveIndexXY);
  baseproclist.Add('procedure SetActorWeaveIndexXY(const key: LongWord; const value: Integer);', @PS_SetActorWeaveIndexXY);
  baseproclist.Add('function GetActorWeaveIndexZ(const key: LongWord): Integer;);', @PS_GetActorWeaveIndexZ);
  baseproclist.Add('procedure SetActorWeaveIndexZ(const key: LongWord; const value: Integer);', @PS_SetActorWeaveIndexZ);
  baseproclist.Add('function GetActorFriction(const key: LongWord): Integer;);', @PS_GetActorFriction);
  baseproclist.Add('procedure SetActorFriction(const key: LongWord; const value: Integer););', @PS_SetActorFriction);
  baseproclist.Add('function GetActorPainChance(const key: LongWord): Integer;);', @PS_GetActorPainChance);
  baseproclist.Add('procedure SetActorPainChance(const key: LongWord; const value: Integer);', @PS_SetActorPainChance);
  baseproclist.Add('function GetActorSpriteDX(const key: LongWord): Integer;);', @PS_GetActorSpriteDX);
  baseproclist.Add('procedure SetActorSpriteDX(const key: LongWord; const value: Integer);', @PS_SetActorSpriteDX);
  baseproclist.Add('function GetActorSpriteDY(const key: LongWord): Integer;);', @PS_GetActorSpriteDY);
  baseproclist.Add('procedure SetActorSpriteDY(const key: LongWord; const value: Integer);', @PS_SetActorSpriteDY);
  baseproclist.Add('function GetActorCustomParam(const key: LongWord; const parm: string): integer;', @PS_GetActorCustomParam);
  baseproclist.Add('procedure SetActorCustomParam(const key: LongWord; const parm: string; const value: integer);', @PS_SetActorCustomParam);
  baseproclist.Add('function CheckActorFlag(const key: LongWord; const flag: LongWord): boolean;', @PS_CheckActorFlag);
  baseproclist.Add('procedure SetActorFlag(const key: LongWord; const flag: LongWord);', @PS_SetActorFlag);
  baseproclist.Add('procedure UnSetActorFlag(const key: LongWord; const flag: LongWord);', @PS_UnSetActorFlag);
  baseproclist.Add('function GetActorName(const key: LongWord): string;', @PS_GetActorName);
  {$IFDEF STRIFE}
  baseproclist.Add('function GetActorName2(const key: LongWord): string;', @PS_GetActorName2);
  {$ENDIF}
  baseproclist.Add('function GetActorDistanceXY(const key1, key2: LongWord): fixed_t;', @PS_GetActorDistanceXY);
  baseproclist.Add('function GetActorDistanceXYZ(const key1, key2: LongWord): fixed_t;', @PS_GetActorDistanceXYZ);
  baseproclist.Add('function GetActorMobjType(const key: LongWord): integer;', @PS_GetActorMobjType);
  baseproclist.Add('function GetActorEditorNumber(const key: LongWord): integer;', @PS_GetActorEditorNumber);
  baseproclist.Add('function GetActorState(const key: LongWord): integer;', @PS_GetActorState);
  baseproclist.Add('procedure SetActorState(const key: LongWord; const value: Integer);', @PS_SetActorState);
  baseproclist.Add('function IsValidActor(const key: LongWord): boolean;', @PS_IsValidActor);
  baseproclist.Add('procedure ActorPlaySound(const key: LongWord; const snd: string);', @PS_ActorPlaySound);
  baseproclist.Add('procedure PlaySound(const snd: string);', @PS_PlaySound);
  baseproclist.Add('function GetActorPlayer(const key: LongWord): integer;', @PS_GetActorPlayer);
  baseproclist.Add('function GetPlayerActor(const plnum: integer): LongWord;', @PS_GetPlayerActor);
  baseproclist.Add('function SpawnActorType(x, y, z: fixed_t; const typ: integer): LongWord;', @PS_SpawnActorType);
  baseproclist.Add('function SpawnActorEditorNumber(x, y, z: fixed_t; const ednum: integer): LongWord;', @PS_SpawnActorEditorNumber);
  baseproclist.Add('function SpawnActorName(x, y, z: fixed_t; const name: string): LongWord;', @PS_SpawnActorName);
  baseproclist.Add('procedure RemoveActor(const key: LongWord);', @PS_RemoveActor);
  baseproclist.Add('function CheckActorSight(const actor1, actor2: LongWord): boolean;', @PS_CheckActorSight);
  baseproclist.Add('function ActorTypeFromEditorNumber(const ednum: integer): integer;', @PS_ActorTypeFromEditorNumber);
  baseproclist.Add('function ActorTypeFromName(const name: string): integer;', @PS_ActorTypeFromName);
  baseproclist.Add('function GetActorSeeSound(const key: LongWord): string;', @PS_GetActorSeeSound);
  baseproclist.Add('function GetActorAttackSound(const key: LongWord): string;', @PS_GetActorAttackSound);
  baseproclist.Add('function GetActorPainSound(const key: LongWord): string;', @PS_GetActorPainSound);
  baseproclist.Add('function GetActorDeathSound(const key: LongWord): string;', @PS_GetActorDeathSound);
  baseproclist.Add('function GetActorActiveSound(const key: LongWord): string;', @PS_GetActorActiveSound);
  baseproclist.Add('function GetActorCustomSound1(const key: LongWord): string;', @PS_GetActorCustomSound1);
  baseproclist.Add('function GetActorCustomSound2(const key: LongWord): string;', @PS_GetActorCustomSound2);
  baseproclist.Add('function GetActorCustomSound3(const key: LongWord): string;', @PS_GetActorCustomSound3);
  baseproclist.Add('function GetActorMeleeSound(const key: LongWord): string;', @PS_GetActorMeleeSound);
  baseproclist.Add('function MergeActorArrays(const A1, A2: TActorArray): TActorArray;', @PS_MergeLongWordArrays);
  // ------------------------- Vertexes ----------------------------------------
  baseproclist.AddWithCustomResult('procedure TVertex(const id: Integer);', '!TVertex', 'function TVertex(const id: Integer): !TVertex;', @PS_TVertex);
  baseproclist.Add('function GetVertexX(const v: integer): fixed_t;', @PS_GetVertexX);
  baseproclist.Add('function GetVertexY(const v: integer): fixed_t;', @PS_GetVertexY);
  baseproclist.Add('function IsValidVertex(const v: Integer): boolean;', @PS_IsValidVertex);
  baseproclist.Add('function GetVertexCount: Integer;', @PS_GetVertexCount);
  // ---------------------------- Sides ----------------------------------------
  baseproclist.AddWithCustomResult('procedure TSide(const id: Integer);', '!TSide', 'function TSide(const id: Integer): !TSide;', @PS_TSide);
  baseproclist.Add('function GetSideTextureOffset(const sd: Integer): fixed_t;', @PS_GetSideTextureOffset);
  baseproclist.Add('procedure SetSideTextureOffset(const sd: Integer; const offs: fixed_t);', @PS_SetSideTextureOffset);
  baseproclist.Add('function GetSideRowOffset(const sd: Integer): fixed_t;', @PS_GetSideRowOffset);
  baseproclist.Add('procedure SetSideRowOffset(const sd: Integer; const offs: fixed_t);', @PS_SetSideRowOffset);
  baseproclist.Add('function GetSideTopTexture(const sd: Integer): string;', @PS_GetSideTopTexture);
  baseproclist.Add('procedure SetSideTopTexture(const sd: Integer; const tex: string);', @PS_SetSideTopTexture);
  baseproclist.Add('function GetSideBottomTexture(const sd: Integer): string;', @PS_GetSideBottomTexture);
  baseproclist.Add('procedure SetSideBottomTexture(const sd: Integer; const tex: string);', @PS_SetSideBottomTexture);
  baseproclist.Add('function GetSideMiddleTexture(const sd: Integer): string;', @PS_GetSideMiddleTexture);
  baseproclist.Add('procedure SetSideMiddleTexture(const sd: Integer; const tex: string);', @PS_SetSideMiddleTexture);
  baseproclist.Add('function GetSideSector(const sd: Integer): integer;', @PS_GetSideSector);
  baseproclist.Add('function IsValidSide(const sd: Integer): boolean;', @PS_IsValidSide);
  baseproclist.Add('function GetSideCount: Integer;', @PS_GetSideCount);
  // ---------------------------- Lines ----------------------------------------
  baseproclist.AddWithCustomResult('procedure TLine(const id: Integer);', '!TLine', 'function TLine(const id: Integer): !TLine;', @PS_TLine);
  baseproclist.Add('function GetLineVertex1(const ld: Integer): integer;', @PS_GetLineVertex1);
  baseproclist.Add('function GetLineVertex2(const ld: Integer): integer;', @PS_GetLineVertex2);
  baseproclist.Add('function GetLineDX(const ld: Integer): fixed_t;', @PS_GetLineDX);
  baseproclist.Add('function GetLineDY(const ld: Integer): fixed_t;', @PS_GetLineDY);
  baseproclist.Add('function GetLineSpecial(const ld: Integer): integer;', @PS_GetLineSpecial);
  baseproclist.Add('procedure SetLineSpecial(const ld: Integer; const spec: integer);', @PS_SetLineSpecial);
  {$IFDEF HEXEN}
  baseproclist.Add('function GetLineArg1(const ld: Integer): byte;', @PS_GetLineArg1);
  baseproclist.Add('procedure SetLineArg1(const ld: Integer; const arg: byte);', @PS_SetLineArg1);
  baseproclist.Add('function GetLineArg2(const ld: Integer): byte;', @PS_GetLineArg2);
  baseproclist.Add('procedure SetLineArg2(const ld: Integer; const arg: byte);', @PS_SetLineArg2);
  baseproclist.Add('function GetLineArg3(const ld: Integer): byte;', @PS_GetLineArg3);
  baseproclist.Add('procedure SetLineArg3(const ld: Integer; const arg: byte);', @PS_SetLineArg3);
  baseproclist.Add('function GetLineArg4(const ld: Integer): byte;', @PS_GetLineArg4);
  baseproclist.Add('procedure SetLineArg4(const ld: Integer; const arg: byte);', @PS_SetLineArg4);
  baseproclist.Add('function GetLineArg5(const ld: Integer): byte;', @PS_GetLineArg5);
  baseproclist.Add('procedure SetLineArg5(const ld: Integer; const arg: byte);', @PS_SetLineArg5);
  {$ELSE}
  baseproclist.Add('function GetLineTag(const ld: Integer): integer;', @PS_GetLineTag);
  baseproclist.Add('procedure SetLineTag(const ld: Integer; const tag: integer);', @PS_SetLineTag);
  baseproclist.Add('function GetLineTransparent(const ld: Integer): Boolean;', @PS_GetLineTransparent);
  baseproclist.Add('procedure SetLineTransparent(const ld: Integer; const x: Boolean);', @PS_SetLineTransparent);
  {$ENDIF}
  baseproclist.Add('function GetLineIsBlocking(const ld: Integer): Boolean;', @PS_GetLineIsBlocking);
  baseproclist.Add('procedure SetLineIsBlocking(const ld: Integer; const x: Boolean);', @PS_SetLineIsBlocking);
  baseproclist.Add('function GetLineIsBlockingMonsters(const ld: Integer): Boolean;', @PS_GetLineIsBlockingMonsters);
  baseproclist.Add('procedure SetLineIsBlockingMonsters(const ld: Integer; const x: Boolean);', @PS_SetLineIsBlockingMonsters);
  baseproclist.Add('function GetLineTriggerScripts(const ld: Integer): Boolean;', @PS_GetLineTriggerScripts);
  baseproclist.Add('procedure SetLineTriggerScripts(const ld: Integer; const x: Boolean);', @PS_SetLineTriggerScripts);
  baseproclist.Add('function GetLineFrontSide(const ld: Integer): integer;', @PS_GetLineFrontSide);
  baseproclist.Add('function GetLineBackSide(const ld: Integer): integer;', @PS_GetLineBackSide);
  baseproclist.Add('function GetLineFrontSector(const ld: Integer): integer;', @PS_GetLineFrontSector);
  baseproclist.Add('function GetLineBackSector(const ld: Integer): integer;', @PS_GetLineBackSector);
  baseproclist.Add('function IsValidLine(const ld: Integer): boolean;', @PS_IsValidLine);
  baseproclist.Add('function GetLineCount: Integer;', @PS_GetLineCount);
  {$IFNDEF HEXEN}
  baseproclist.Add('function FindLinesFromTag(const tag: integer): TIntegerArray;', @PS_FindLinesFromTag);
  {$ENDIF}
// --------------------------- SECTORS -----------------------------------------
  baseproclist.AddWithCustomResult('procedure TSector(const id: Integer);', '!TSector', 'function TSector(const id: Integer): !TSector;', @PS_TSector);
  baseproclist.Add('function GetSectorFloorHeight(const sec: Integer): fixed_t;', @PS_GetSectorFloorHeight);
  baseproclist.Add('procedure SetSectorFloorHeight(const sec: Integer; const x: fixed_t);', @PS_SetSectorFloorHeight);
  baseproclist.Add('function GetSectorCeilingHeight(const sec: Integer): fixed_t;', @PS_GetSectorCeilingHeight);
  baseproclist.Add('procedure SetSectorCeilingHeight(const sec: Integer; const x: fixed_t);', @PS_SetSectorCeilingHeight);
  baseproclist.Add('function GetSectorFloorPicture(const sec: Integer): string;', @PS_GetSectorFloorPicture);
  baseproclist.Add('procedure SetSectorFloorPicture(const sec: Integer; const pic: string);', @PS_SetSectorFloorPicture);
  baseproclist.Add('function GetSectorCeilingPicture(const sec: Integer): string;', @PS_GetSectorCeilingPicture);
  baseproclist.Add('procedure SetSectorCeilingPicture(const sec: Integer; const pic: string);', @PS_SetSectorCeilingPicture);
  baseproclist.Add('function GetSectorLightLevel(const sec: Integer): Integer;', @PS_GetSectorLightLevel);
  baseproclist.Add('procedure SetSectorLightLevel(const sec: Integer; const x: Integer);', @PS_SetSectorLightLevel);
  baseproclist.Add('function GetSectorSpecial(const sec: Integer): Integer;', @PS_GetSectorSpecial);
  baseproclist.Add('function GetSectorTag(const sec: Integer): Integer;', @PS_GetSectorTag);
  baseproclist.Add('procedure SetSectorTag(const sec: Integer; const x: Integer);', @PS_SetSectorTag);
  baseproclist.Add('function GetSectorActors(const sec: Integer): TActorArray;', @PS_GetSectorActors);
  baseproclist.Add('function GetSectorNumActors(const sec: Integer): Integer;', @PS_GetSectorNumActors);
  baseproclist.Add('function GetSectorLines(const sec: Integer): TIntegerArray;', @PS_GetSectorLines);
  baseproclist.Add('function GetSectorNumLines(const sec: Integer): Integer;', @PS_GetSectorNumLines);
  {$IFDEF DOOM_OR_STRIFE}
  baseproclist.Add('function GetSectorFloorXOffset(const sec: Integer): fixed_t;', @PS_GetSectorFloorXOffset);
  baseproclist.Add('procedure SetSectorFloorXOffset(const sec: Integer; const offs: fixed_t);', @PS_SetSectorFloorXOffset);
  baseproclist.Add('function GetSectorFloorYOffset(const sec: Integer): fixed_t;', @PS_GetSectorFloorYOffset);
  baseproclist.Add('procedure SetSectorFloorYOffset(const sec: Integer; const offs: fixed_t);', @PS_SetSectorFloorYOffset);
  baseproclist.Add('function GetSectorCeilingXOffset(const sec: Integer): fixed_t;', @PS_GetSectorCeilingXOffset);
  baseproclist.Add('procedure SetSectorCeilingXOffset(const sec: Integer; const offs: fixed_t);', @PS_SetSectorCeilingXOffset);
  baseproclist.Add('function GetSectorCeilingYOffset(const sec: Integer): fixed_t;', @PS_GetSectorCeilingYOffset);
  baseproclist.Add('procedure SetSectorCeilingYOffset(const sec: Integer; const offs: fixed_t);', @PS_SetSectorCeilingYOffset);
  {$ENDIF}
  baseproclist.Add('function GetSectorFloorAngle(const sec: Integer): angle_t;', @PS_GetSectorFloorAngle);
  baseproclist.Add('procedure SetSectorFloorAngle(const sec: Integer; const ang: angle_t);', @PS_SetSectorFloorAngle);
  baseproclist.Add('function GetSectorFloorAngleX(const sec: Integer): fixed_t;', @PS_GetSectorFloorAngleX);
  baseproclist.Add('procedure SetSectorFloorAngleX(const sec: Integer; const angx: fixed_t);', @PS_SetSectorFloorAngleX);
  baseproclist.Add('function GetSectorFloorAngleY(const sec: Integer): fixed_t;', @PS_GetSectorFloorAngleY);
  baseproclist.Add('procedure SetSectorFloorAngleY(const sec: Integer; const angy: fixed_t);', @PS_SetSectorFloorAngleY);
  baseproclist.Add('function GetSectorCeilingAngle(const sec: Integer): angle_t;', @PS_GetSectorCeilingAngle);
  baseproclist.Add('procedure SetSectorCeilingAngle(const sec: Integer; const ang: angle_t);', @PS_SetSectorCeilingAngle);
  baseproclist.Add('function GetSectorCeilingAngleX(const sec: Integer): fixed_t;', @PS_GetSectorCeilingAngleX);
  baseproclist.Add('procedure SetSectorCeilingAngleX(const sec: Integer; const angx: fixed_t);', @PS_SetSectorCeilingAngleX);
  baseproclist.Add('function GetSectorCeilingAngleY(const sec: Integer): fixed_t;', @PS_GetSectorCeilingAngleY);
  baseproclist.Add('procedure SetSectorCeilingAngleY(const sec: Integer; const angy: fixed_t);', @PS_SetSectorCeilingAngleY);
  baseproclist.Add('function GetSectorRippleFloor(const sec: Integer): Boolean;', @PS_GetSectorRippleFloor);
  baseproclist.Add('procedure SetSectorRippleFloor(const sec: Integer; const rpl: Boolean);', @PS_SetSectorRippleFloor);
  baseproclist.Add('function GetSectorRippleCeiling(const sec: Integer): Boolean;', @PS_GetSectorRippleCeiling);
  baseproclist.Add('procedure SetSectorRippleCeiling(const sec: Integer; const rpl: Boolean);', @PS_SetSectorRippleCeiling);
  baseproclist.Add('function GetSectorInterpolate(const sec: Integer): Boolean;', @PS_GetSectorInterpolate);
  baseproclist.Add('procedure SetSectorInterpolate(const sec: Integer; const intpl: Boolean);', @PS_SetSectorInterpolate);
  baseproclist.Add('function GetSectorFog(const sec: Integer): Boolean;', @PS_GetSectorFog);
  baseproclist.Add('procedure SetSectorFog(const sec: Integer; const fog: Boolean);', @PS_SetSectorFog);
  baseproclist.Add('function GetSectorGravity(const sec: Integer): fixed_t;', @PS_GetSectorGravity);  // JVAL: sector gravity (VERSION 204)
  baseproclist.Add('procedure SetSectorGravity(const sec: Integer; const grav: fixed_t);', @PS_SetSectorGravity); // JVAL: sector gravity (VERSION 204)
  baseproclist.Add('function GetSectorMidSector(const sec: Integer): Integer;', @PS_GetSectorMidSector);
  baseproclist.Add('function GetSectorSlopeSector(const sec: Integer): Integer;', @PS_GetSectorSlopeSector);
  baseproclist.Add('function SkyPicture: string;', @PS_SkyPicture);
  baseproclist.Add('function IsValidSector(const sec: Integer): boolean;', @PS_IsValidSector);
  baseproclist.Add('function GetSectorCount: Integer;', @PS_GetSectorCount);
  baseproclist.Add('function FindSectorsFromTag(const tag: integer): TIntegerArray;', @PS_FindSectorsFromTag);
  baseproclist.Add('procedure SectorPlaySound(const secid: integer; const snd: string);', @PS_SectorPlaySound);
  baseproclist.Add('procedure SectorMoveZ(const secid: integer; const dz: fixed_t);', @PS_SectorMoveZ);
  baseproclist.Add('procedure SetSectorFloorSlope(const secid: integer; const x1, y1, z1: fixed_t; const x2, y2, z2: fixed_t; const x3, y3, z3: fixed_t);', @PS_SetFloorSlope);
  baseproclist.Add('procedure SetSectorCeilingSlope(const secid: integer; const x1, y1, z1: fixed_t; const x2, y2, z2: fixed_t; const x3, y3, z3: fixed_t);', @PS_SetCeilingSlope);
// --------------------------- PLAYERS -----------------------------------------
  baseproclist.Add('function PlayerInGame(const plnum: integer): boolean;', @PS_PlayerInGame);
  {$IFDEF DOOM_OR_STRIFE}
  baseproclist.Add('procedure PlayerFaceMobj(const plnum: integer; const actor: LongWord; const ticks: integer);', @PS_PlayerFaceMobj);
  {$ENDIF}
  {$IFDEF DOOM_OR_STRIFE}
  baseproclist.Add('procedure SetPlayerHasCard(const plnum: integer; const card: integer; const value: boolean);', @PS_SetPlayerHasCard);
  baseproclist.Add('function GetPlayerHasCard(const plnum: integer; const card: integer): boolean;', @PS_GetPlayerHasCard);
  baseproclist.Add('procedure SetPlayerHasKey(const plnum: integer; const key: integer; const value: boolean);', @PS_SetPlayerHasCard);
  baseproclist.Add('function GetPlayerHasKey(const plnum: integer; const key: integer): boolean;', @PS_GetPlayerHasCard);
  {$ENDIF}
  {$IFDEF HERETIC_OR_HEXEN}
  baseproclist.Add('procedure SetPlayerHasCard(const plnum: integer; const card: integer; const value: boolean);', @PS_SetPlayerHasKey);
  baseproclist.Add('function GetPlayerHasCard(const plnum: integer; const card: integer): boolean;', @PS_GetPlayerHasKey);
  baseproclist.Add('procedure SetPlayerHasKey(const plnum: integer; const key: integer; const value: boolean);', @PS_SetPlayerHasKey);
  baseproclist.Add('function GetPlayerHasKey(const plnum: integer; const key: integer): boolean;', @PS_GetPlayerHasKey);
  baseproclist.Add('procedure PlayerUseArtifact(const plnum: Integer; const arti: Integer);', @PS_PlayerUseArtifact);
  baseproclist.Add('function GiveArtifactToPlayer(const plnum: Integer; const arti: Integer): Boolean;', @PS_GiveArtifactToPlayer);
  baseproclist.Add('function CheckPlayerArtifact(const plnum: Integer; const arti: Integer): Integer;', @PS_CheckPlayerArtifact);
  {$ENDIF}
  baseproclist.Add('procedure SetPlayerHasWeapon(const plnum: integer; const weapon: integer; const value: boolean);', @PS_SetPlayerHasWeapon);
  baseproclist.Add('function GetPlayerHasWeapon(const plnum: integer; const weapon: integer): boolean;', @PS_GetPlayerHasWeapon);
  {$IFNDEF HEXEN}
  baseproclist.Add('procedure SetPlayerAmmo(const plnum: integer; const ammotype: integer; const value: integer);', @PS_SetPlayerAmmo);
  baseproclist.Add('function GetPlayerAmmo(const plnum: integer; const ammotype: integer): integer;', @PS_GetPlayerAmmo);
  {$ELSE}
  baseproclist.Add('procedure SetPlayerMana(const plnum: integer; const mana: integer; const value: integer);', @PS_SetPlayerMana);
  baseproclist.Add('function GetPlayerMana(const plnum: integer; const mana: integer): integer;', @PS_GetPlayerMana);
  {$ENDIF}
  {$IFDEF HEXEN}
  baseproclist.Add('function GetPlayerClass(const plnum: Integer): Integer;', @PS_GetPlayerClass);
  {$ENDIF}
  baseproclist.Add('procedure SetPlayerMessage(const plnum: integer; const msg: string);', @PS_SetPlayerMessage);
  baseproclist.Add('function GetPlayerMessage(const plnum: integer): string;', @PS_GetPlayerMessage);
  baseproclist.Add('procedure SetPlayerExtraLight(const plnum: Integer; const l: Integer);', @PS_SetPlayerExtraLight);
  baseproclist.Add('function GetPlayerExtraLight(const plnum: Integer): Integer;', @PS_GetPlayerExtraLight);
  baseproclist.Add('procedure SetPlayerPowerTicks(const plnum: integer; const powertype: integer; const ticks: integer);', @PS_SetPlayerPowerTicks);
  baseproclist.Add('function GetPlayerPowerTicks(const plnum: integer; const powertype: integer): integer;', @PS_GetPlayerPowerTicks);
  baseproclist.Add('function GetPlayerViewZ(const plnum: integer): fixed_t;', @PS_GetPlayerViewZ);
  baseproclist.Add('function GetPlayerViewHeight(const plnum: integer): fixed_t;', @PS_GetPlayerViewHeight);
  baseproclist.Add('function ConsolePlayer: Integer;', @PS_ConsolePlayer);
// -------------------------- TEXTURES -----------------------------------------
  baseproclist.Add('function GetTextureWidth(const tex: string): Integer;', @PS_GetTextureWidth);
  baseproclist.Add('function GetTextureHeight(const tex: string): Integer;', @PS_GetTextureHeight);
  baseproclist.Add('function IsValidTexture(const tex: string): boolean;', @PS_IsValidTexture);
// ----------------------------- MOBJS -----------------------------------------
  baseproclist.Add('function IsValidMobjType(const typ: integer): boolean;', @PS_IsValidMobjType);
  baseproclist.Add('function GetMobjTypeFromEditorNumber(const en: integer): integer;', @PS_GetMobjTypeFromEditorNumber);
  baseproclist.Add('function GetEditorNumberFromMobjType(const typ: integer): integer;', @PS_GetEditorNumberFromMobjType);
  baseproclist.Add('function GetMobjInfoCount: integer;', @PS_GetMobjInfoCount);
  baseproclist.Add('function GetMobjInfoName(const typ: integer): string;', @PS_GetMobjInfoName);
  {$IFDEF STRIFE}
  baseproclist.Add('function GetMobjInfoName2(const typ: integer): string;', @PS_GetMobjInfoName2);
  {$ENDIF}
  baseproclist.Add('function GetMobjInfoInheritsFrom(const typ: integer): integer;', @PS_GetMobjInfoInheritsFrom);
  baseproclist.Add('function GetMobjInfoDoomEdNum(const typ: integer): integer;', @PS_GetMobjInfoDoomEdNum);
  baseproclist.Add('function GetMobjInfoSpawnState(const typ: integer): integer;', @PS_GetMobjInfoSpawnState);
  baseproclist.Add('function GetMobjInfoSpawnHealth(const typ: integer): integer;', @PS_GetMobjInfoSpawnHealth);
  baseproclist.Add('function GetMobjInfoSeeState(const typ: integer): integer;', @PS_GetMobjInfoSeeState);
  baseproclist.Add('function GetMobjInfoSeeSound(const typ: integer): string;', @PS_GetMobjInfoSeeSound);
  baseproclist.Add('function GetMobjInfoReactionTime(const typ: integer): integer;', @PS_GetMobjInfoReactionTime);
  baseproclist.Add('function GetMobjInfoAttackSound(const typ: integer): string;', @PS_GetMobjInfoAttackSound);
  baseproclist.Add('function GetMobjInfoPainState(const typ: integer): integer;', @PS_GetMobjInfoPainState);
  baseproclist.Add('function GetMobjInfoPainChance(const typ: integer): integer;', @PS_GetMobjInfoPainChance);
  baseproclist.Add('function GetMobjInfoPainSound(const typ: integer): string;', @PS_GetMobjInfoPainSound);
  baseproclist.Add('function GetMobjInfoMeleeState(const typ: integer): integer;', @PS_GetMobjInfoMeleeState);
  baseproclist.Add('function GetMobjInfoMissileState(const typ: integer): integer;', @PS_GetMobjInfoMissileState);
  baseproclist.Add('function GetMobjInfoDeathState(const typ: integer): integer;', @PS_GetMobjInfoDeathState);
  baseproclist.Add('function GetMobjInfoXdeathState(const typ: integer): integer;', @PS_GetMobjInfoXdeathState);
  baseproclist.Add('function GetMobjInfoDeathSound(const typ: integer): string;', @PS_GetMobjInfoDeathSound);
  baseproclist.Add('function GetMobjInfoSpeed(const typ: integer): integer;', @PS_GetMobjInfoSpeed);
  baseproclist.Add('function GetMobjInfoRadius(const typ: integer): integer;', @PS_GetMobjInfoRadius);
  baseproclist.Add('function GetMobjInfoHeight(const typ: integer): integer;', @PS_GetMobjInfoHeight);
  baseproclist.Add('function GetMobjInfoMass(const typ: integer): integer;', @PS_GetMobjInfoMass);
  baseproclist.Add('function GetMobjInfoDamage(const typ: integer): integer;', @PS_GetMobjInfoDamage);
  baseproclist.Add('function GetMobjInfoActiveSound(const typ: integer): string;', @PS_GetMobjInfoActiveSound);
  baseproclist.Add('function GetMobjInfoFlag(const typ: Integer; const flg: integer): boolean;', @PS_GetMobjInfoFlag);
  baseproclist.Add('function GetMobjInfoRaiseState(const typ: integer): integer;', @PS_GetMobjInfoRaiseState);
  baseproclist.Add('function GetMobjInfoCustomSound1(const typ: integer): string;', @PS_GetMobjInfoCustomSound1);
  baseproclist.Add('function GetMobjInfoCustomSound2(const typ: integer): string;', @PS_GetMobjInfoCustomSound2);
  baseproclist.Add('function GetMobjInfoCustomSound3(const typ: integer): string;', @PS_GetMobjInfoCustomSound3);
  baseproclist.Add('function GetMobjInfoDropItem(const typ: integer): integer;', @PS_GetMobjInfoDropItem);
  baseproclist.Add('function GetMobjInfoMissiletype(const typ: integer): integer;', @PS_GetMobjInfoMissiletype);
  baseproclist.Add('function GetMobjInfoExplosionDamage(const typ: integer): integer;', @PS_GetMobjInfoExplosionDamage);
  baseproclist.Add('function GetMobjInfoExplosionRadius(const typ: integer): integer;', @PS_GetMobjInfoExplosionRadius);
  baseproclist.Add('function GetMobjInfoMeleeDamage(const typ: integer): integer;', @PS_GetMobjInfoMeleeDamage);
  baseproclist.Add('function GetMobjInfoMeleeSound(const typ: integer): string;', @PS_GetMobjInfoMeleeSound);
  baseproclist.Add('function GetMobjInfoRenderStyle(const typ: integer): integer;', @PS_GetMobjInfoRenderStyle);
  baseproclist.Add('function GetMobjInfoAlpha(const typ: integer): integer;', @PS_GetMobjInfoAlpha);
  baseproclist.Add('function GetMobjInfoHealState(const typ: integer): integer;', @PS_GetMobjInfoHealState);
  baseproclist.Add('function GetMobjInfoCrashState(const typ: integer): integer;', @PS_GetMobjInfoCrashState);
  baseproclist.Add('function GetMobjInfoVSpeed(const typ: integer): integer;', @PS_GetMobjInfoVSpeed);
  baseproclist.Add('function GetMobjInfoMinMissileChance(const typ: integer): integer;', @PS_GetMobjInfoMinMissileChance);
  baseproclist.Add('function GetMobjInfoPushFactor(const typ: integer): integer;', @PS_GetMobjInfoPushFactor);
  baseproclist.Add('function GetMobjInfoScale(const typ: integer): integer;', @PS_GetMobjInfoScale);
  baseproclist.Add('function GetMobjInfoInteractState(const typ: integer): integer;', @PS_GetMobjInfoInteractState);
  {$IFDEF DOOM_OR_STRIFE}
  baseproclist.Add('function GetMobjInfoMissileHeight(const typ: integer): integer;', @PS_GetMobjInfoMissileHeight);
  {$ENDIF}
  baseproclist.Add('function GetMobjInfoFloatSpeed(const typ: integer): integer;', @PS_GetMobjInfoFloatSpeed);
  baseproclist.Add('function GetMobjInfoNormalSpeed(const typ: integer): integer;', @PS_GetMobjInfoNormalSpeed);
  baseproclist.Add('function GetMobjInfoFastSpeed(const typ: integer): integer;', @PS_GetMobjInfoFastSpeed);
  baseproclist.Add('function GetMobjInfoObituary(const typ: integer): string;', @PS_GetMobjInfoObituary);
  baseproclist.Add('function GetMobjInfoHitObituary(const typ: integer): string;', @PS_GetMobjInfoHitObituary);
  baseproclist.Add('function GetMobjInfoGender(const typ: integer): integer;', @PS_GetMobjInfoGender);
  baseproclist.Add('function GetMobjInfoMeleeRange(const typ: integer): integer;', @PS_GetMobjInfoMeleeRange);
  baseproclist.Add('function GetMobjInfoMaxStepHeight(const typ: integer): integer;', @PS_GetMobjInfoMaxStepHeight);
  baseproclist.Add('function GetMobjInfoMaxDropOffHeight(const typ: integer): integer;', @PS_GetMobjInfoMaxDropOffHeight);
  baseproclist.Add('function GetMobjInfoGibHealth(const typ: integer): integer;', @PS_GetMobjInfoGibHealth);
  baseproclist.Add('function GetMobjInfoMaxTargetRange(const typ: integer): integer;', @PS_GetMobjInfoMaxTargetRange);
  baseproclist.Add('function GetMobjInfoWeaveIndexXY(const typ: integer): integer;', @PS_GetMobjInfoWeaveIndexXY);
  baseproclist.Add('function GetMobjInfoWeaveIndexZ(const typ: integer): integer;', @PS_GetMobjInfoWeaveIndexZ);
  baseproclist.Add('function GetMobjInfoFriction(const typ: integer): integer;', @PS_GetMobjInfoFriction);
  baseproclist.Add('function GetMobjInfoSpriteDX(const dx: integer): integer;', @PS_GetMobjInfoSpriteDX);
  baseproclist.Add('function GetMobjInfoSpriteDY(const dy: integer): integer;', @PS_GetMobjInfoSpriteDY);
// ------------------------------ GAME -----------------------------------------
  {$IFDEF HEXEN}
  baseproclist.Add('procedure G_Completed(map, position: integer);', @G_Completed);
  baseproclist.Add('function P_TranslateMap(map: integer): integer;', @P_TranslateMap);
  baseproclist.Add('function P_GetMapNextMap(map: integer): integer;', @P_GetMapNextMap);
  {$ENDIF}
  {$IFDEF DOOM_OR_HERETIC}
  baseproclist.Add('procedure G_ExitLevel;', @G_ExitLevel);
  baseproclist.Add('procedure G_SecretExitLevel;', @G_SecretExitLevel);
  {$ENDIF}
  {$IFDEF STRIFE}
  baseproclist.Add('procedure G_ExitLevel(dest: integer);', @G_ExitLevel);
  {$ENDIF}
  baseproclist.Add('function _GAME: string;', @PS_Game);
  baseproclist.Add('procedure GlobalEarthQuake(const tics: integer);', @PS_GlobalEarthQuake);
  baseproclist.Add('procedure LocalEarthQuake(const x, y, z: fixed_t; const tics: integer; const intensity: fixed_t; const maxdist: fixed_t);', @PS_LocalEarthQuake);
  baseproclist.Add('function GameSkill: integer;', @PS_GameSkill);
  baseproclist.Add('function GameMap: integer;', @PS_GameMap);
  {$IFDEF DOOM_OR_HERETIC}
  baseproclist.Add('function GameEpisode: integer;', @PS_GameEpisode);
  {$ENDIF}
// ------------------ KEYBOARD CONTROL -----------------------------------------
  baseproclist.Add('function gamekeydown(const kkey: integer): boolean;', @PS_gamekeydown);
  baseproclist.Add('function mousebuttons(const mkey: integer): boolean;', @PS_mousebuttons);
  baseproclist.Add('function joybuttons(const jkey: integer): boolean;', @PS_joybuttons);
  baseproclist.Add('function key_right: integer;', @PS_key_right);
  baseproclist.Add('function key_left: integer;', @PS_key_left);
  baseproclist.Add('function key_up: integer;', @PS_key_up);
  baseproclist.Add('function key_down: integer;', @PS_key_down);
  baseproclist.Add('function key_lookup: integer;', @PS_key_lookup);
  baseproclist.Add('function key_lookdown: integer;', @PS_key_lookdown);
  baseproclist.Add('function key_lookcenter: integer;', @PS_key_lookcenter);
  baseproclist.Add('function key_lookright: integer;', @PS_key_lookright);
  baseproclist.Add('function key_lookleft: integer;', @PS_key_lookleft);
  {$IFNDEF STRIFE}
  baseproclist.Add('function key_lookforward: integer;', @PS_key_lookforward);
  {$ENDIF}
  baseproclist.Add('function key_strafeleft: integer;', @PS_key_strafeleft);
  baseproclist.Add('function key_straferight: integer;', @PS_key_straferight);
  baseproclist.Add('function key_fire: integer;', @PS_key_fire);
  baseproclist.Add('function key_use: integer;', @PS_key_use);
  baseproclist.Add('function key_strafe: integer;', @PS_key_strafe);
  baseproclist.Add('function key_speed: integer;', @PS_key_speed);
  baseproclist.Add('function key_jump: integer;', @PS_key_jump);
  // JVAL: 20211101 - Crouch
  baseproclist.Add('function key_crouch: integer;', @PS_key_crouch);
  baseproclist.Add('function key_weapon0: integer;', @PS_key_weapon0);
  baseproclist.Add('function key_weapon1: integer;', @PS_key_weapon1);
  baseproclist.Add('function key_weapon2: integer;', @PS_key_weapon2);
  baseproclist.Add('function key_weapon3: integer;', @PS_key_weapon3);
  {$IFNDEF HEXEN}
  baseproclist.Add('function key_weapon4: integer;', @PS_key_weapon4);
  baseproclist.Add('function key_weapon5: integer;', @PS_key_weapon5);
  baseproclist.Add('function key_weapon6: integer;', @PS_key_weapon6);
  baseproclist.Add('function key_weapon7: integer;', @PS_key_weapon7);
  {$ENDIF}
  {$IFDEF HERETIC_OR_HEXEN}
  baseproclist.Add('function key_flyup: integer;', @PS_key_flyup);
  baseproclist.Add('function key_flydown: integer;', @PS_key_flydown);
  baseproclist.Add('function key_flycenter: integer;', @PS_key_flycenter);
  baseproclist.Add('function key_invleft: integer;', @PS_key_invleft);
  baseproclist.Add('function key_invright: integer;', @PS_key_invright);
  baseproclist.Add('function key_useartifact: integer;', @PS_key_useartifact);
  {$ENDIF}
  {$IFDEF STRIFE}
  baseproclist.Add('function key_invleft: integer;', @PS_key_invleft);
  baseproclist.Add('function key_invright: integer;', @PS_key_invright);
  baseproclist.Add('function key_weapon8: integer;', @PS_key_weapon8);
  baseproclist.Add('function key_weapon9: integer;', @PS_key_weapon9);
  baseproclist.Add('function key_usehealth: integer;', @PS_key_usehealth);
  baseproclist.Add('function key_invquery: integer;', @PS_key_invquery);
  baseproclist.Add('function key_mission: integer;', @PS_key_mission);
  baseproclist.Add('function key_invpop: integer;', @PS_key_invpop);
  baseproclist.Add('function key_invkey: integer;', @PS_key_invkey);
  baseproclist.Add('function key_invhome: integer;', @PS_key_invhome);
  baseproclist.Add('function key_invend: integer;', @PS_key_invend);
  baseproclist.Add('function key_invuse: integer;', @PS_key_invuse);
  baseproclist.Add('function key_invdrop: integer;', @PS_key_invdrop);
  {$ENDIF}
  baseproclist.Add('function mousebfire: integer;', @PS_mousebfire);
  baseproclist.Add('function mousebstrafe: integer;', @PS_mousebstrafe);
  baseproclist.Add('function mousebforward: integer;', @PS_mousebforward);
  baseproclist.Add('function joybfire: integer;', @PS_joybfire);
  baseproclist.Add('function joybstrafe: integer;', @PS_joybstrafe);
  baseproclist.Add('function joybuse: integer;', @PS_joybuse);
  baseproclist.Add('function joybspeed: integer;', @PS_joybspeed);
  baseproclist.Add('function joybjump: integer;', @PS_joybjump);
  // JVAL: 20211101 - Crouch
  baseproclist.Add('function joybcrouch: integer;', @PS_joybcrouch);
  baseproclist.Add('function joyblleft: integer;', @PS_joyblleft);
  baseproclist.Add('function joyblright: integer;', @PS_joyblright);
// ------------------------------- MAP -----------------------------------------
  baseproclist.Add('function R_PointToAngle(x: fixed_t; y: fixed_t): angle_t;', @R_PointToAngle);
  baseproclist.Add('function R_PointToAngle2(const x1: fixed_t; const y1: fixed_t; const x2: fixed_t; const y2: fixed_t): angle_t;', @R_PointToAngle2);
  baseproclist.Add('function R_PointToDist(const x: fixed_t; const y: fixed_t): fixed_t;', @R_PointToDist);
  baseproclist.Add('function P_AproxDistance(dx: fixed_t; dy: fixed_t): fixed_t;', @P_AproxDistance);
  baseproclist.Add('function P_PointOnLineSide(x: fixed_t; y: fixed_t; line: integer): integer;', @PS_P_PointOnLineSide);
  baseproclist.Add('function R_PointInSector(const x: fixed_t; const y: fixed_t): integer;', @PS_R_PointInSector);
  baseproclist.Add('function R_PointInSubSector(const x: fixed_t; const y: fixed_t): integer;', @PS_R_PointInSubSector);
  units.AddObject(basename, baseproclist);

  basename := 'PS_FIXED';
  baseproclist := TProcedureList.Create(basename);
  baseproclist.Add('function FixedMul(const a, b: fixed_t): fixed_t;', @FixedMulEx);
  baseproclist.Add('function FixedDiv(const a, b: fixed_t): fixed_t;', @FixedDivEx);
  baseproclist.Add('function FixedSqrt(const a: fixed_t): fixed_t;', @FixedSqrt);
  baseproclist.Add('function FloatToFixed(const f: single): fixed_t;', @FloatToFixed);
  baseproclist.Add('function DoubleToFixed(const f: double): fixed_t;', @DoubleToFixed);
  baseproclist.Add('function ExtendedToFixed(const f: extended): fixed_t;', @ExtendedToFixed);
  baseproclist.Add('function FixedToFloat(const x: fixed_t): single;', @FixedToFloat);
  baseproclist.Add('function FixedToDouble(const x: fixed_t): double;', @FixedToDouble);
  baseproclist.Add('function FixedToExtended(const x: fixed_t): extended;', @FixedToExtended);
  units.AddObject(basename, baseproclist);

  basename := 'PS_OVERLAY';
  baseproclist := TProcedureList.Create(basename);
  baseproclist.Add('procedure OverlayClear;', @PS_OverlayClear);
  baseproclist.Add('procedure OverlayDrawPatch(const ticks : Integer; const patchname : string; const x, y : Integer);', @PS_OverlayDrawPatch);
  baseproclist.Add('procedure OverlayDrawPatchStretched(const ticks: Integer; const patchname: string; const x1, y1, x2, y2: Integer);', @PS_OverlayDrawPatchStretched);
  baseproclist.Add('procedure OverlayDrawPixel(const ticks : Integer; const red, green, blue : byte; const x, y : Integer);', @PS_OverlayDrawPixel);
  baseproclist.Add('procedure OverlayDrawRect(const ticks: Integer; const red, green, blue: byte; const x1, y1, x2, y2: Integer);', @PS_OverlayDrawRect);
  baseproclist.Add('procedure OverlayDrawLine(const ticks: Integer; const red, green, blue: byte; const x1, y1, x2, y2: Integer);', @PS_OverlayDrawLine);
  baseproclist.Add('procedure OverlayDrawText(const ticks : Integer; const txt : string; const align : Integer; const x, y : Integer);', @PS_OverlayDrawText);
  baseproclist.Add('procedure OverlayDrawLeftText(const ticks : Integer; const txt : string; const x, y : Integer);', @PS_OverlayDrawLeftText);
  baseproclist.Add('procedure OverlayDrawRightText(const ticks : Integer; const txt : string; const x, y : Integer);', @PS_OverlayDrawRightText);
  baseproclist.Add('procedure OverlayDrawCenterText(const ticks : Integer; const txt : string; const x, y : Integer);', @PS_OverlayDrawCenterText);
  units.AddObject(basename, baseproclist);

  units.Sorted := True;
end;

procedure PS_ResetProcLists;
var
  i: integer;
  baseproclist: TProcedureList;
begin
  for i := 0 to units.Count - 1 do
  begin
    baseproclist := units.Objects[i] as TProcedureList;
    baseproclist.Reset;
  end;
end;

procedure PS_ShutDownProcLists;
var
  i: integer;
  baseproclist: TProcedureList;
begin
  for i := 0 to units.Count - 1 do
  begin
    baseproclist := units.Objects[i] as TProcedureList;
    baseproclist.Free;
  end;
  units.Free;
end;

function PS_ScriptOnUses(Sender: TPSPascalCompiler; const Name: string): Boolean;
var
  i: integer;
  lst: TDStringList;
  m: integer;
  uT_integer: TPSType;
  uT_longword: TPSType;
  uT_extended: TPSType;
  idx: integer;
  baseproclist: TProcedureList;
  uUnit: string;
  flgcount: LongWord;
{ the OnUses callback function is called for each "uses" in the script.
  It's always called with the parameter 'SYSTEM' at the top of the script.
  For example: uses ii1, ii2;
  This will call this function 3 times. First with 'SYSTEM' then 'II1' and then 'II2'.
}
begin
  if Name = 'SYSTEM' then
  begin
    // Functions
    with Sender.AddClassN(nil, '!TOBJECT') do
    begin
      RegisterMethod('constructor Create');
      RegisterMethod('procedure Free');
    end;
    Sender.AddTypeS('TActorArray', 'array of LongWord');
    Sender.AddTypeS('TIntegerArray', 'array of integer');
    Sender.AddTypeS('TInt64Array', 'array of int64');
    Sender.AddTypeS('TLongWordArray', 'array of LongWord');
    Sender.AddTypeS('TSingleArray', 'array of single');
    Sender.AddTypeS('TFloatArray', 'array of single');
    Sender.AddTypeS('TDoubleArray', 'array of double');
    Sender.AddTypeS('TExtendedArray', 'array of extended');
    Sender.AddTypeS('float', 'single');
    Sender.AddTypeS('real', 'double');
    Sender.AddTypeS('fixed_t', 'integer');
    Sender.AddTypeS('angle_t', 'longword');
    RegisterDateTimeLibrary_C(Sender);
    SIRegister_GlobalVars(Sender);
    SIRegister_Game(Sender);
    SIRegister_TOverlay(Sender);

    idx := units.IndexOf(Name);
    if idx >= 0 then
    begin
      baseproclist := units.Objects[idx] as TProcedureList;
      baseproclist.RegisterProcsComp(Sender);
    end;

    { This will register the function to the script engine. Now it can be used from within the script. }
    uT_extended := Sender.FindType('extended');
    uT_integer := Sender.FindType('integer');
    uT_longword := Sender.FindType('longword');
    Sender.AddConstant('NaN', uT_extended).Value.textended := 0.0 / 0.0;
    Sender.AddConstant('Infinity', uT_extended).Value.textended := 1.0 / 0.0;
    Sender.AddConstant('NegInfinity', uT_extended).Value.textended := - 1.0 / 0.0;
    Sender.AddConstant('MAXINT', uT_integer).Value.ts32 := MAXINT;

    // Overlay drawing
    Sender.AddConstant('alLeft', uT_integer).Value.ts32 := OVR_ALIGN_LEFT;
    Sender.AddConstant('alRight', uT_integer).Value.ts32 := OVR_ALIGN_RIGHT;
    Sender.AddConstant('alCenter', uT_integer).Value.ts32 := OVR_ALIGN_CENTER;

    // mobj flags
    flgcount := 0;
    for i := 0 to mobj_flags.Count - 1 do
    begin
      Sender.AddConstant(mobj_flags[i], uT_longword).Value.tu32 := flgcount;
      Inc(flgcount);
    end;

    {$IFDEF HERETIC_OR_HEXEN}
    // mobj flags2
    flgcount := 32;
    for i := 0 to mobj_flags2.Count - 1 do
    begin
      Sender.AddConstant(mobj_flags2[i], uT_longword).Value.tu32 := flgcount;
      Inc(flgcount);
    end;
    {$ENDIF}

    // mobj flags_ex
    flgcount := 64;
    for i := 0 to mobj_flags_ex.Count - 1 do
    begin
      Sender.AddConstant(mobj_flags_ex[i], uT_longword).Value.tu32 := flgcount;
      Inc(flgcount);
    end;

    // mobj flags2_ex
    flgcount := 96;
    for i := 0 to mobj_flags2_ex.Count - 1 do
    begin
      Sender.AddConstant(mobj_flags2_ex[i], uT_longword).Value.tu32 := flgcount;
      Inc(flgcount);
    end;

    // mobj flags3_ex
    flgcount := 128;
    for i := 0 to mobj_flags3_ex.Count - 1 do
    begin
      Sender.AddConstant(mobj_flags3_ex[i], uT_longword).Value.tu32 := flgcount;
      Inc(flgcount);
    end;

    // mobj flags4_ex
    flgcount := 160;
    for i := 0 to mobj_flags4_ex.Count - 1 do
    begin
      Sender.AddConstant(mobj_flags4_ex[i], uT_longword).Value.tu32 := flgcount;
      Inc(flgcount);
    end;

    // states
    lst := statenames.AllTokens;
    for i := 0 to lst.Count - 1 do
      Sender.AddConstant(lst[i], uT_integer).Value.ts32 := statenames.IndexOfToken(lst[i]);
    lst.Free;

    // mobj types
    for m := 0 to Ord(DO_NUMMOBJTYPES) - 1 do
      Sender.AddConstant(GetENumName(TypeInfo(mobjtype_t), m), uT_integer).Value.ts32 := m;

    // consts doomdef
    Sender.AddConstant('MAXPLAYERS', uT_integer).Value.ts32 := MAXPLAYERS;

    for m := 0 to Ord(sk_nightmare) do
      Sender.AddConstant(GetENumName(TypeInfo(skill_t), m), uT_integer).Value.ts32 := m;

    {$IFDEF DOOM_OR_STRIFE}
    for m := 0 to Ord(NUMCARDS) do
      Sender.AddConstant(GetENumName(TypeInfo(card_t), m), uT_integer).Value.ts32 := m;
    {$ELSE}
    for m := 0 to Ord(NUMKEYCARDS) do
      Sender.AddConstant(GetENumName(TypeInfo(keytype_t), m), uT_integer).Value.ts32 := m;
    {$ENDIF}

    {$IFDEF HEXEN}
    for m := 0 to Ord(NUMCLASSES) do
      Sender.AddConstant(GetENumName(TypeInfo(pclass_t), m), uT_integer).Value.ts32 := m;
    {$ENDIF}

    for m := 0 to Ord(NUMWEAPONS) do
      Sender.AddConstant(GetENumName(TypeInfo(weapontype_t), m), uT_integer).Value.ts32 := m;

    for m := 0 to Ord(NUMMOBJRENDERSTYLES) do
      Sender.AddConstant(GetENumName(TypeInfo(mobjrenderstyle_t), m), uT_integer).Value.ts32 := m;

    for m := 0 to Ord(NUMGENDERS) do
      Sender.AddConstant(GetENumName(TypeInfo(gender_t), m), uT_integer).Value.ts32 := m;

    {$IFDEF HEXEN}
    for m := 0 to Ord(NUMMANA) do
      Sender.AddConstant(GetENumName(TypeInfo(manatype_t), m), uT_integer).Value.ts32 := m;
    Sender.AddConstant('MAX_MANA', uT_integer).Value.ts32 := MAX_MANA;
    {$ELSE}
    for m := 0 to Ord(NUMAMMO) do
      Sender.AddConstant(GetENumName(TypeInfo(ammotype_t), m), uT_integer).Value.ts32 := m;
    {$ENDIF}

    for m := 0 to Ord(NUMPOWERS) do
      Sender.AddConstant(GetENumName(TypeInfo(powertype_t), m), uT_integer).Value.ts32 := m;

    {$IFDEF HERETIC_OR_HEXEN}
    for m := 0 to Ord(NUMARTIFACTS) do
      Sender.AddConstant(GetENumName(TypeInfo(artitype_t), m), uT_integer).Value.ts32 := m;
    {$ENDIF}

    {$IFDEF HEXEN}
    Sender.AddConstant('arti_firstpuzzitem', uT_integer).Value.ts32 := arti_firstpuzzitem;
    {$ENDIF}

    {$IFDEF STRIFE}
    for m := 0 to Ord(tk_numquests) do
      Sender.AddConstant(GetENumName(TypeInfo(questtype_t), m), uT_integer).Value.ts32 := m;
    {$ENDIF}

    // consts p_local
    Sender.AddConstant('ONFLOORZ', uT_integer).Value.ts32 := ONFLOORZ;
    Sender.AddConstant('FLOORZ', uT_integer).Value.ts32 := ONFLOORZ;
    Sender.AddConstant('ONCEILINGZ', uT_integer).Value.ts32 := ONCEILINGZ;
    Sender.AddConstant('CEILINGZ', uT_integer).Value.ts32 := ONCEILINGZ;
    Sender.AddConstant('ONFLOATZ', uT_integer).Value.ts32 := {$IFDEF HEXEN}FLOATRANDZ{$ELSE}ONFLOATZ{$ENDIF};
    Sender.AddConstant('FLOATZ', uT_integer).Value.ts32 := {$IFDEF HEXEN}FLOATRANDZ{$ELSE}ONFLOATZ{$ENDIF};
    {$IFDEF HEXEN}
    Sender.AddConstant('FLOATRANDZ', uT_integer).Value.ts32 := FLOATRANDZ;
    {$ENDIF}
    Sender.AddConstant('TICRATE', uT_integer).Value.ts32 := TICRATE;
    Sender.AddConstant('FRACUNIT', uT_integer).Value.ts32 := FRACUNIT;
    Sender.AddConstant('ACTOR_INVALID', uT_integer).Value.ts32 := ACTOR_INVALID;
    Sender.AddConstant('PLAYER_INVALID', uT_integer).Value.ts32 := PLAYER_INVALID;
    Sender.AddConstant('MOBJTYPE_INVALID', uT_integer).Value.ts32 := MOBJTYPE_INVALID;
    Sender.AddConstant('STATE_INVALID', uT_integer).Value.ts32 := STATE_INVALID;
    Sender.AddConstant('EDITORNUMBER_INVALID', uT_integer).Value.ts32 := EDITORNUMBER_INVALID;
    Sender.AddConstant('EDITORNUMBER_UNKNOWN', uT_integer).Value.ts32 := EDITORNUMBER_UNKNOWN;
    Sender.AddConstant('TEXTURE_INVALID', uT_integer).Value.ts32 := TEXTURE_INVALID;
    Sender.AddConstant('FLAT_INVALID', uT_integer).Value.ts32 := FLAT_INVALID;
    Sender.AddConstant('SECTOR_INVALID', uT_integer).Value.ts32 := SECTOR_INVALID;
    Sender.AddConstant('LINE_INVALID', uT_integer).Value.ts32 := LINE_INVALID;
    Sender.AddConstant('SIDE_INVALID', uT_integer).Value.ts32 := SIDE_INVALID;
    Sender.AddConstant('VERTEX_INVALID', uT_integer).Value.ts32 := VERTEX_INVALID;
    Sender.AddConstant('LIGHTLEVEL_INVALID', uT_integer).Value.ts32 := LIGHTLEVEL_INVALID;

    // consts tables
    Sender.AddConstant('D_PI', uT_extended).Value.textended := D_PI;
    Sender.AddConstant('ANGLE_T_TO_RAD', uT_extended).Value.textended := ANGLE_T_TO_RAD;
    Sender.AddConstant('ANGLETOFINESHIFT', uT_longword).Value.tu32 := ANGLETOFINESHIFT;
    Sender.AddConstant('ANGLETOFINEUNIT', uT_longword).Value.tu32 := ANGLETOFINEUNIT;
    Sender.AddConstant('FINEANGLES', uT_longword).Value.tu32 := FINEANGLES;
    Sender.AddConstant('FINEMASK', uT_longword).Value.tu32 := FINEMASK;
    Sender.AddConstant('DIR256ANGLES', uT_longword).Value.tu32 := DIR256ANGLES;
    Sender.AddConstant('DIR256TOANGLEUNIT', uT_longword).Value.tu32 := DIR256TOANGLEUNIT;
    Sender.AddConstant('DIR256TOANGLESHIFT', uT_longword).Value.tu32 := DIR256TOANGLESHIFT;
    Sender.AddConstant('ANG1', uT_longword).Value.tu32 := ANG1;
{    for i := 2 to 4 do
      Sender.AddConstant('ANG' + itoa(i), uT_longword).Value.tu32 := ANG1 * i;}
    Sender.AddConstant('ANG5', uT_longword).Value.tu32 := ANG5;
    Sender.AddConstant('ANG30', uT_longword).Value.tu32 := ANG60 div 2;
    Sender.AddConstant('ANG45', uT_longword).Value.tu32 := ANG45;
    Sender.AddConstant('ANG60', uT_longword).Value.tu32 := ANG60;
    Sender.AddConstant('ANG90', uT_longword).Value.tu32 := ANG90;
    Sender.AddConstant('ANG120', uT_longword).Value.tu32 := ANG120;
    Sender.AddConstant('ANG180', uT_longword).Value.tu32 := ANG180;
    Sender.AddConstant('ANG240', uT_longword).Value.tu32 := ANG240;
    Sender.AddConstant('ANG270', uT_longword).Value.tu32 := ANG270;
    Sender.AddConstant('ANG300', uT_longword).Value.tu32 := ANG300;
    Sender.AddConstant('ANG315', uT_longword).Value.tu32 := ANG315;
    Sender.AddConstant('ANG355', uT_longword).Value.tu32 := ANG355;
    Sender.AddConstant('ANGLE_MAX', uT_longword).Value.tu32 := ANGLE_MAX;
    Sender.AddConstant('SLOPEBITS', uT_longword).Value.tu32 := SLOPEBITS;
    Sender.AddConstant('SLOPERANGE', uT_longword).Value.tu32 := SLOPERANGE;
    Sender.AddConstant('DBITS', uT_longword).Value.tu32 := DBITS;
    Sender.AddConstant('DRANGE', uT_longword).Value.tu32 := DRANGE;

    // Generalized constants
    Sender.AddConstant('DAMAGE_MASK', uT_integer).Value.ts32 := $60;
    Sender.AddConstant('DAMAGE_SHIFT', uT_integer).Value.ts32 := 5;
    Sender.AddConstant('SECRET_MASK', uT_integer).Value.ts32 := $80;
    Sender.AddConstant('SECRET_SHIFT', uT_integer).Value.ts32 := 7;
    Sender.AddConstant('FRICTION_MASK', uT_integer).Value.ts32 := $100;
    Sender.AddConstant('FRICTION_SHIFT', uT_integer).Value.ts32 := 8;
    Sender.AddConstant('PUSH_MASK', uT_integer).Value.ts32 := $200;
    Sender.AddConstant('PUSH_SHIFT', uT_integer).Value.ts32 := 9;

    Sender.AddConstant('CGENFLOORBASE', uT_integer).Value.ts32 := $6000;
    Sender.AddConstant('CGENCEILINGBASE', uT_integer).Value.ts32 := $4000;
    Sender.AddConstant('CGENDOORBASE', uT_integer).Value.ts32 := $3C00;
    Sender.AddConstant('CGENLOCKEDBASE', uT_integer).Value.ts32 := $3800;
    Sender.AddConstant('CGENLIFTBASE', uT_integer).Value.ts32 := $3400;
    Sender.AddConstant('CGENSTAIRSBASE', uT_integer).Value.ts32 := $3000;
    Sender.AddConstant('CGENCRUSHERBASE', uT_integer).Value.ts32 := $2F80;

    Sender.AddConstant('TriggerType', uT_integer).Value.ts32 := $0007;
    Sender.AddConstant('TriggerTypeShift', uT_integer).Value.ts32 := 0;

    Sender.AddConstant('gen_FloorCrush', uT_integer).Value.ts32 := $1000;
    Sender.AddConstant('gen_FloorChange', uT_integer).Value.ts32 := $0C00;
    Sender.AddConstant('gen_FloorTarget', uT_integer).Value.ts32 := $0380;
    Sender.AddConstant('gen_FloorDirection', uT_integer).Value.ts32 := $0040;
    Sender.AddConstant('gen_FloorModel', uT_integer).Value.ts32 := $0020;
    Sender.AddConstant('gen_FloorSpeed', uT_integer).Value.ts32 := $0018;

    Sender.AddConstant('FloorCrushShift', uT_integer).Value.ts32 := 12;
    Sender.AddConstant('FloorChangeShift', uT_integer).Value.ts32 := 10;
    Sender.AddConstant('FloorTargetShift', uT_integer).Value.ts32 := 7;
    Sender.AddConstant('FloorDirectionShift', uT_integer).Value.ts32 := 6;
    Sender.AddConstant('FloorModelShift', uT_integer).Value.ts32 := 5;
    Sender.AddConstant('FloorSpeedShift', uT_integer).Value.ts32 := 3;

    Sender.AddConstant('CeilingCrush', uT_integer).Value.ts32 := $1000;
    Sender.AddConstant('CeilingChange', uT_integer).Value.ts32 := $0C00;
    Sender.AddConstant('CeilingTarget', uT_integer).Value.ts32 := $0380;
    Sender.AddConstant('CeilingDirection', uT_integer).Value.ts32 := $0040;
    Sender.AddConstant('CeilingModel', uT_integer).Value.ts32 := $0020;
    Sender.AddConstant('CeilingSpeed', uT_integer).Value.ts32 := $0018;

    Sender.AddConstant('CeilingCrushShift', uT_integer).Value.ts32 := 12;
    Sender.AddConstant('CeilingChangeShift', uT_integer).Value.ts32 := 10;
    Sender.AddConstant('CeilingTargetShift', uT_integer).Value.ts32 := 7;
    Sender.AddConstant('CeilingDirectionShift', uT_integer).Value.ts32 := 6;
    Sender.AddConstant('CeilingModelShift', uT_integer).Value.ts32 := 5;
    Sender.AddConstant('CeilingSpeedShift', uT_integer).Value.ts32 := 3;

    Sender.AddConstant('LiftTarget', uT_integer).Value.ts32 := $0300;
    Sender.AddConstant('LiftDelay', uT_integer).Value.ts32 := $00C0;
    Sender.AddConstant('LiftMonster', uT_integer).Value.ts32 := $0020;
    Sender.AddConstant('LiftSpeed', uT_integer).Value.ts32 := $0018;

    Sender.AddConstant('LiftTargetShift', uT_integer).Value.ts32 := 8;
    Sender.AddConstant('LiftDelayShift', uT_integer).Value.ts32 := 6;
    Sender.AddConstant('LiftMonsterShift', uT_integer).Value.ts32 := 5;
    Sender.AddConstant('LiftSpeedShift', uT_integer).Value.ts32 := 3;

    Sender.AddConstant('StairIgnore', uT_integer).Value.ts32 := $0200;
    Sender.AddConstant('StairDirection', uT_integer).Value.ts32 := $0100;
    Sender.AddConstant('StairStep', uT_integer).Value.ts32 := $00c0;
    Sender.AddConstant('StairMonster', uT_integer).Value.ts32 := $0020;
    Sender.AddConstant('StairSpeed', uT_integer).Value.ts32 := $0018;

    Sender.AddConstant('StairIgnoreShift', uT_integer).Value.ts32 := 9;
    Sender.AddConstant('StairDirectionShift', uT_integer).Value.ts32 := 8;
    Sender.AddConstant('StairStepShift', uT_integer).Value.ts32 := 6;
    Sender.AddConstant('StairMonsterShift', uT_integer).Value.ts32 := 5;
    Sender.AddConstant('StairSpeedShift', uT_integer).Value.ts32 := 3;

    Sender.AddConstant('CrusherSilent', uT_integer).Value.ts32 := $0040;
    Sender.AddConstant('CrusherMonster', uT_integer).Value.ts32 := $0020;
    Sender.AddConstant('CrusherSpeed', uT_integer).Value.ts32 := $0018;

    Sender.AddConstant('CrusherSilentShift', uT_integer).Value.ts32 := 6;
    Sender.AddConstant('CrusherMonsterShift', uT_integer).Value.ts32 := 5;
    Sender.AddConstant('CrusherSpeedShift', uT_integer).Value.ts32 := 3;

    Sender.AddConstant('DoorDelay', uT_integer).Value.ts32 := $0300;
    Sender.AddConstant('DoorMonster', uT_integer).Value.ts32 := $0080;
    Sender.AddConstant('DoorKind', uT_integer).Value.ts32 := $0060;
    Sender.AddConstant('DoorSpeed', uT_integer).Value.ts32 := $0018;

    Sender.AddConstant('DoorDelayShift', uT_integer).Value.ts32 := 8;
    Sender.AddConstant('DoorMonsterShift', uT_integer).Value.ts32 := 7;
    Sender.AddConstant('DoorKindShift', uT_integer).Value.ts32 := 5;
    Sender.AddConstant('DoorSpeedShift', uT_integer).Value.ts32 := 3;

    Sender.AddConstant('LockedNKeys', uT_integer).Value.ts32 := $0200;
    Sender.AddConstant('LockedKey', uT_integer).Value.ts32 := $01c0;
    Sender.AddConstant('LockedKind', uT_integer).Value.ts32 := $0020;
    Sender.AddConstant('LockedSpeed', uT_integer).Value.ts32 := $0018;

    Sender.AddConstant('LockedNKeysShift', uT_integer).Value.ts32 := 9;
    Sender.AddConstant('LockedKeyShift', uT_integer).Value.ts32 := 6;
    Sender.AddConstant('LockedKindShift', uT_integer).Value.ts32 := 5;
    Sender.AddConstant('LockedSpeedShift', uT_integer).Value.ts32 := 3;

    Result := True;
  end
  else if Name = 'ALL' then
  begin
    for i := 0 to units.Count - 1 do
    begin
      uUnit := strupper(units.Strings[i]);
      if uUnit <> 'SYSTEM' then
        PS_ScriptOnUses(Sender, uUnit);
    end;
    Result := True;
  end
  else
  begin
    idx := units.IndexOf(Name);
    if idx >= 0 then
    begin
      baseproclist := units.Objects[idx] as TProcedureList;
      baseproclist.RegisterProcsComp(Sender);
      Result := True;
    end
    else
    begin
      Sender.MakeError('', ecUnknownIdentifier, '');
      Result := False;
    end;
  end;

  if Result then
    (Sender as TDoomCompiler).unitnames.Add(Name);
end;

// **** TDoomCompiler ****
var
  dccnt: integer = 0;

procedure CompTranslateLineInfo(Sender: TPSPascalCompiler; var Pos, Row, Col: Cardinal; var Name: TbtString);
var
  res: TPSLineInfoResults;
begin
  if TDoomCompiler(Sender).FPP.CurrentLineInfo.GetLineInfo(Name, Pos, Res) then
  begin
    Pos := Res.Pos;
    Row := Res.Row;
    Col := Res.Col;
    Name := Res.Name;
  end;
end;

procedure callObjectOnProcessDirective (
  Sender: TPSPreProcessor;
  Parser: TPSPascalPreProcessorParser;
  const Active: Boolean;
  const DirectiveName, DirectiveParam: TbtString;
  var Continue: Boolean);
begin
end;

procedure callObjectOnProcessUnknowDirective (
  Sender: TPSPreProcessor;
  Parser: TPSPascalPreProcessorParser;
  const Active: Boolean;
  const DirectiveName, DirectiveParam: TbtString;
  var Continue: Boolean);
begin
end;


function FPPNeedFile(Sender: TPSPreProcessor; const callingfilename: TbtString; var FileName, Output: TbtString): Boolean;
begin
  Output := PAK_ReadFileAsString(FileName);
  Result := Output <> '';
end;

constructor TDoomCompiler.CreateDoomCompiler;
begin
  if dccnt <> 0 then
    I_Error('TDoomCompiler.CreateDoomCompiler(): Compiler is already active!');

  Inc(dccnt);

  inherited Create(@DefaultLookupTable, DEFAULT_KEYWORD_COUNT);
  OnUses := PS_ScriptOnUses;
  OnExternalProc := DllExternalProc;
  AllowNoEnd := true;

  FPP := TPSPreProcessor.Create;
  FPP.Id := Self;
  FPP.OnNeedFile := FPPNeedFile;

  funitnames := TDStringList.Create;
end;

destructor TDoomCompiler.Destroy;
begin
  Dec(dccnt);
  FPP.Free;
  funitnames.Free;

  inherited;
end;

function TDoomCompiler.CompileDoomScript(const source: TbtString; var pcode: string): Boolean;
var
  ppsrc: string;
  ppout: string;
  unitstr: string;
  len: integer;
begin
  PS_ResetProcLists;

  FPP.Clear;

  FPP.Defines.Add(strupper(_GAME));
  {$IFDEF DEBUG}
  FPP.Defines.Add('DEBUG');
  {$ENDIF}
  if devparm then
    FPP.Defines.Add('DEVPARM');

  {$IFDEF DOOM}
  FPP.Defines.Add('DOOM_OR_HERETIC');
  FPP.Defines.Add('DOOM_OR_HEXEN');
  FPP.Defines.Add('DOOM_OR_STRIFE');
  FPP.Defines.Add('HERETIC_OR_DOOM');
  FPP.Defines.Add('HEXEN_OR_DOOM');
  FPP.Defines.Add('STRIFE_OR_DOOM');
  {$ENDIF}

  {$IFDEF HERETIC}
  FPP.Defines.Add('HERETIC_OR_DOOM');
  FPP.Defines.Add('HERETIC_OR_HEXEN');
  FPP.Defines.Add('HERETIC_OR_STRIFE');
  FPP.Defines.Add('DOOM_OR_HERETIC');
  FPP.Defines.Add('HEXEN_OR_HERETIC');
  FPP.Defines.Add('STRIFE_OR_HERETIC');
  {$ENDIF}

  {$IFDEF HEXEN}
  FPP.Defines.Add('HEXEN_OR_DOOM');
  FPP.Defines.Add('HEXEN_OR_HERETIC');
  FPP.Defines.Add('HEXEN_OR_STRIFE');
  FPP.Defines.Add('DOOM_OR_HEXEN');
  FPP.Defines.Add('HERETIC_OR_HEXEN');
  FPP.Defines.Add('STRIFE_OR_HEXEN');
  {$ENDIF}

  {$IFDEF STRIFE}
  FPP.Defines.Add('STRIFE_OR_DOOM');
  FPP.Defines.Add('STRIFE_OR_HERETIC');
  FPP.Defines.Add('STRIFE_OR_HEXEN');
  FPP.Defines.Add('DOOM_OR_STRIFE');
  FPP.Defines.Add('HERETIC_OR_STRIFE');
  FPP.Defines.Add('HEXEN_OR_STRIFE');
  {$ENDIF}

  {$IFDEF OPENGL}
  FPP.Defines.Add('OPENGL');
  {$ENDIF}

  OnTranslateLineInfo := CompTranslateLineInfo;
  FPP.OnProcessDirective := callObjectOnProcessDirective;
  FPP.OnProcessUnknowDirective := callObjectOnProcessUnknowDirective;
  FPP.MainFile := source;
  FPP.MainFileName := '';
  FPP.PreProcess(FPP.MainFileName, ppsrc);

  funitnames.Clear;

  Result := Compile(ppsrc);

  if Result then
    GetOutput(ppout)
  else
    ppout := '';

  unitstr := _GAME + #13#10 + funitnames.Text;
  len := Length(unitstr);
  pcode := '            ' + unitstr + ppout;
  PLongWord(@pcode[1])^ := ID_DDPS;
  PLongWord(@pcode[5])^ := DDCVERSION;
  PLongWord(@pcode[9])^ := len;

  FPP.AdjustMessages(Self);
end;

// **** TDoomExec ****
constructor TDoomExec.CreateDoomExec(const aImporter: TPSRuntimeClassImporter);
begin
  inherited Create;
  AllowNullClasses := True;
  RegisterDLLRuntime(Self);
  RegisterClassLibraryRuntime(Self, aImporter);
  RegisterDateTimeLibrary_R(Self);
end;

function TDoomExec.LoadData(const pcode: TbtString): Boolean;
var
  dta: string;
  unitstr: string;
  len: integer;
  ver: integer;
  i, x: integer;
  lst: TStringList;

  function RegisterRTLUnit(const s: string): boolean;
  var
    i: integer;
  begin
    i := units.indexOf(s);
    if i >= 0 then
    begin
      (units.Objects[i] as TProcedureList).RegisterProcsExec(Self);
      Result := True;
    end
    else if s = 'ALL' then
    begin
      for i := 0 to units.Count - 1 do
        (units.Objects[i] as TProcedureList).RegisterProcsExec(Self);
      Result := True;
    end
    else
    begin
      I_Warning('RegisterRTLUnit(): Unknown RTL unit %s'#13#10, [s]);
      Result := False;
    end;
  end;

begin
  len := Length(pcode);
  if len < 4 then
  begin
    Result := False;
    Exit;
  end;

  if PLongWord(@pcode[1])^ <> ID_DDPS then
  begin
    RegisterRTLUnit('SYSTEM');
    Result := inherited LoadData(pcode);
    Exit;
  end;

  if len < 16 then
  begin
    Result := False;
    Exit;
  end;

  ver := PLongWord(@pcode[5])^;
  if ver <> DDCVERSION then
  begin
    Result := False;
    Exit;
  end;

  x := PLongWord(@pcode[9])^;
  if len < 12 + x then
  begin
    Result := False;
    Exit;
  end;

  SetLength(unitstr, x);
  for i := 1 to x do
    unitstr[i] := pcode[i + 12];

  lst := TStringList.Create;
  lst.Text := unitstr;

  if lst.Count < 2 then
  begin
    lst.Free;
    Result := False;
    Exit;
  end;

  if lst.Strings[0] <> _GAME then
  begin
    I_Warning('TDoomExec.LoadData(): Can not load "%s" compiled script while playing "%s"'#13#10, [lst.Strings[0], _GAME]);
    lst.Free;
    Result := False;
    Exit;
  end;

  for i := 1 to lst.Count - 1 do
    RegisterRTLUnit(lst.Strings[i]);
  lst.Free;

  dta := Copy(pcode, x + 13, len - x - 12);
  Result := inherited LoadData(dta);

  if Result then
  begin
    // JVAL: Must be called after TPSExec.LoadData()
    RIRegisterRTL_GlobalVars(Self);
    RIRegisterRTL_Game(Self);
    RIRegisterRTL_TOverlay(Self);
  end;
end;

function PS_ImportUnits: TObject;
begin
  Result := units;
end;

type
  TRTLObject = class(TObject)
  public
    procedure FakeFree;
  end;

procedure TRTLObject.FakeFree;
begin
  // JVAL: Do nothing :)
  // Explanation:
  //   Since we can only use imported classes usage of Free procedure inside
  //   script could cause problems. Instead we use a stub method, and we leave
  //   the actual Free of the objects to the calling Application.
end;

constructor TPSDoomRuntimeClassImporter.Create;
begin
  inherited;
  with Add2(TRTLObject, '!TOBJECT') do
  begin
    RegisterConstructor(@TRTLObject.Create, 'Create');
    RegisterMethod(@TRTLObject.FakeFree, 'Free');
  end;
  RIRegister_GlobalVars(Self);
  RIRegister_Game(Self);
  RIRegister_TOverlay(Self);
end;

end.

