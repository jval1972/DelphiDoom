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
//  Pascal Script RTL - Map interaction
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit psi_map;

interface

//==============================================================================
// PS_Ceiling_CrushAndRaise
//
// Action 42
//
//==============================================================================
function PS_Ceiling_CrushAndRaise(const tag: integer; const speed: integer; const crush: integer): boolean;

//==============================================================================
// PS_Ceiling_CrushRaiseAndStay
//
// Action 45
//
//==============================================================================
function PS_Ceiling_CrushRaiseAndStay(const tag: integer; const speed: integer; const crush: integer): boolean;

//==============================================================================
// PS_Ceiling_CrushStop
//
// Action 44
//
//==============================================================================
function PS_Ceiling_CrushStop(const tag: integer): boolean;

//==============================================================================
// PS_Ceiling_LowerAndCrush
//
// Action 43
//
//==============================================================================
function PS_Ceiling_LowerAndCrush(const tag: integer; const speed: integer; const crush: integer): boolean;

//==============================================================================
// PS_Ceiling_LowerByValue
//
// Action 40
//
//==============================================================================
function PS_Ceiling_LowerByValue(const tag: integer; const speed: integer; const height: integer): boolean;

//==============================================================================
// PS_Ceiling_LowerByValueTimes8
//
// Action 199 (ZDoom)
//
//==============================================================================
function PS_Ceiling_LowerByValueTimes8(const tag: integer; const speed: integer; const height: integer): boolean;

//==============================================================================
// PS_Ceiling_LowerToFloor
//
// Action 254 (ZDoom)
//
//==============================================================================
function PS_Ceiling_LowerToFloor(const tag: integer; const speed: integer): boolean;

//==============================================================================
// PS_Ceiling_LowerToHighestFloor
//
// Action 192 (ZDoom)
//
//==============================================================================
function PS_Ceiling_LowerToHighestFloor(const tag: integer; const speed: integer): boolean;

//==============================================================================
// PS_Ceiling_LowerToLowest
//
// Action 253 (ZDoom)
//
//==============================================================================
function PS_Ceiling_LowerToLowest(const tag: integer; const speed: integer): boolean;

//==============================================================================
// PS_Ceiling_MoveToValueAndCrush
//
// Action 280 (ZDoom)
//
//==============================================================================
function PS_Ceiling_MoveToValueAndCrush(const tag: integer; const speed: integer; const height: integer; const crush: integer): boolean;

//==============================================================================
// PS_Ceiling_MoveToValue
//
// Action 47 (ZDoom)
//
//==============================================================================
function PS_Ceiling_MoveToValue(const tag: integer; const speed: integer; const height: integer; const neg: integer): boolean;

//==============================================================================
// PS_Ceiling_MoveToValueTimes8
//
// Action 69
//
//==============================================================================
function PS_Ceiling_MoveToValueTimes8(const tag: integer; const speed: integer; const height: integer; const neg: integer): boolean;

//==============================================================================
// PS_Ceiling_RaiseByValue
//
// Action 41
//
//==============================================================================
function PS_Ceiling_RaiseByValue(const tag: integer; const speed: integer; const height: integer): boolean;

//==============================================================================
// PS_Ceiling_RaiseByValueTimes8
//
// Action 198 (ZDoom)
//
//==============================================================================
function PS_Ceiling_RaiseByValueTimes8(const tag: integer; const speed: integer; const height: integer): boolean;

//==============================================================================
// PS_Ceiling_RaiseToHighestFloor
//
// Action 266 (ZDoom)
//
//==============================================================================
function PS_Ceiling_RaiseToHighestFloor(const tag: integer; const speed: integer): boolean;

//==============================================================================
// PS_Door_Close
//
// Action 10
//
//==============================================================================
function PS_Door_Close(const tag: integer; const speed: integer): boolean;

//==============================================================================
// PS_Door_CloseWaitOpen
//
// Action 249 (ZDoom)
//
//==============================================================================
function PS_Door_CloseWaitOpen(const tag: integer; const speed: integer; const delay: integer): boolean;

//==============================================================================
// PS_Door_Open
//
// Action 11
//
//==============================================================================
function PS_Door_Open(const tag: integer; const speed: integer): boolean;

//==============================================================================
// PS_Door_Raise
//
// Action 12
//
//==============================================================================
function PS_Door_Raise(const tag: integer; const speed: integer; delay: integer): boolean;

//==============================================================================
// PS_FloorAndCeiling_LowerByValue
//
// Action 95
//
//==============================================================================
function PS_FloorAndCeiling_LowerByValue(const tag: integer; const speed: integer; const height: integer): boolean;

//==============================================================================
// PS_FloorAndCeiling_LowerRaise
//
// Action 251 (ZDoom)
//
//==============================================================================
function PS_FloorAndCeiling_LowerRaise(const tag: integer; const fspeed, cspeed: integer): boolean;

//==============================================================================
// PS_FloorAndCeiling_RaiseByValue
//
// Action 96
//
//==============================================================================
function PS_FloorAndCeiling_RaiseByValue(const tag: integer; const speed: integer; const height: integer): boolean;

//==============================================================================
// PS_Floor_CrushStop
//
// Action 46
//
//==============================================================================
function PS_Floor_CrushStop(const tag: integer): boolean;

//==============================================================================
// PS_Floor_LowerByValue
//
// Action 20
//
//==============================================================================
function PS_Floor_LowerByValue(const tag: integer; const speed: integer; const height: integer): boolean;

//==============================================================================
// PS_Floor_LowerByValueTimes8
//
// Action 36
//
//==============================================================================
function PS_Floor_LowerByValueTimes8(const tag: integer; const speed: integer; const height: integer): boolean;

//==============================================================================
// PS_Floor_LowerToHighest
//
// Action 242 (ZDoom)
//
//==============================================================================
function PS_Floor_LowerToHighest(const tag: integer; const speed: integer; const adjust: integer): boolean;

//==============================================================================
// PS_Floor_LowerToLowestCeiling
//
// Action 258 (ZDoom)
//
//==============================================================================
function PS_Floor_LowerToLowestCeiling(const tag: integer; const speed: integer): boolean;

//==============================================================================
// PS_Floor_LowerToLowest
//
// Action 21
//
//==============================================================================
function PS_Floor_LowerToLowest(const tag: integer; const speed: integer): boolean;

//==============================================================================
// PS_Floor_LowerToNearest
//
// Action 22
//
//==============================================================================
function PS_Floor_LowerToNearest(const tag: integer; const speed: integer): boolean;

//==============================================================================
// PS_Floor_MoveToValue
//
// Action 37 (ZDoom)
//
//==============================================================================
function PS_Floor_MoveToValue(const tag: integer; const speed: integer; const height: integer; const neg: integer): boolean;

//==============================================================================
// PS_Floor_MoveToValueTimes8
//
// Action 68
//
//==============================================================================
function PS_Floor_MoveToValueTimes8(const tag: integer; const speed: integer; const height: integer; const neg: integer): boolean;

//==============================================================================
// PS_Floor_RaiseAndCrushDoom
//
// Action 99 (ZDoom)
//
//==============================================================================
function PS_Floor_RaiseAndCrushDoom(const tag: integer; const speed: integer; const crush: integer): boolean;

//==============================================================================
// PS_Floor_RaiseAndCrush
//
// Action 28
//
//==============================================================================
function PS_Floor_RaiseAndCrush(const tag: integer; const speed: integer; const crush: integer): boolean;

//==============================================================================
// PS_Floor_RaiseByTexture
//
// Action 240 (ZDoom)
//
//==============================================================================
function PS_Floor_RaiseByTexture(const tag: integer; const speed: integer): boolean;

//==============================================================================
// PS_Floor_RaiseByValue
//
// Action 23
//
//==============================================================================
function PS_Floor_RaiseByValue(const tag: integer; const speed: integer; const height: integer): boolean;

//==============================================================================
// PS_Floor_RaiseByValueTimes8
//
// Action 35
//
//==============================================================================
function PS_Floor_RaiseByValueTimes8(const tag: integer; const speed: integer; const height: integer): boolean;

//==============================================================================
// PS_Floor_RaiseInstant
//
// Action 67
//
//==============================================================================
function PS_Floor_RaiseInstant(const tag: integer; const speed: integer; const height: integer): boolean;

//==============================================================================
// PS_Floor_LowerInstant
//
// Action 66
//
//==============================================================================
function PS_Floor_LowerInstant(const tag: integer; const speed: integer; const height: integer): boolean;

//==============================================================================
// PS_Floor_RaiseToCeilingNoChange
//
// Action 259 (ZDoom) without the change
//
//==============================================================================
function PS_Floor_RaiseToCeilingNoChange(const tag: integer; const speed: integer; const crush: integer; const gap: integer): boolean;

//==============================================================================
// PS_Floor_RaiseToHighest
//
// Action 24
//
//==============================================================================
function PS_Floor_RaiseToHighest(const tag: integer; const speed: integer): boolean;

//==============================================================================
// PS_Floor_RaiseToLowestCeiling
//
// Action 238 (ZDoom)
//
//==============================================================================
function PS_Floor_RaiseToLowestCeiling(const tag: integer; const speed: integer): boolean;

//==============================================================================
// PS_Floor_RaiseToNearest
//
// Action 25
//
//==============================================================================
function PS_Floor_RaiseToNearest(const tag: integer; const speed: integer): boolean;

//==============================================================================
// PS_Floor_ToCeilingInstantNoChange
//
// Action 260 (ZDoom) without the change
//
//==============================================================================
function PS_Floor_ToCeilingInstantNoChange(const tag: integer; const crush: integer; const gap: integer): boolean;

//==============================================================================
// PS_Floor_Waggle
//
// Action 138
//
//==============================================================================
function PS_Floor_Waggle(const tag: integer; const amp: integer; const freq: integer; const offset: integer; const time: integer): boolean;

//==============================================================================
// PS_Light_ChangeToValue
//
// Action 112
//
//==============================================================================
function PS_Light_ChangeToValue(const tag: integer; const value: integer): boolean;

//==============================================================================
// PS_Light_Fade
//
// Action 113
//
//==============================================================================
function PS_Light_Fade(const tag: integer; const value: integer; const tics: integer): boolean;

//==============================================================================
// PS_Light_Flicker
//
// Action 115
//
//==============================================================================
function PS_Light_Flicker(const tag: integer; const upper: integer; const lower: integer): boolean;

//==============================================================================
// PS_Light_ForceLightning
//
// Action 109
//
//==============================================================================
function PS_Light_ForceLightning: boolean;

//==============================================================================
// PS_Light_ForceLightningTics
//
// Action 109
//
//==============================================================================
function PS_Light_ForceLightningTics(const tics: integer): boolean;

//==============================================================================
// PS_Light_Glow
//
// Action 114
//
//==============================================================================
function PS_Light_Glow(const tag: integer; const upper: integer; const lower: integer; const tics: integer): boolean;

//==============================================================================
// PS_Light_LowerByValue
//
// Action 111
//
//==============================================================================
function PS_Light_LowerByValue(const tag: integer; const value: integer): boolean;

//==============================================================================
// PS_Light_MaxNeighbor
//
// Action 234 (ZDoom)
//
//==============================================================================
function PS_Light_MaxNeighbor(const tag: integer): boolean;

//==============================================================================
// PS_Light_MinNeighbor
//
// Action 233 (ZDoom)
//
//==============================================================================
function PS_Light_MinNeighbor(const tag: integer): boolean;

//==============================================================================
// PS_Light_RaiseByValue
//
// Action 110
//
//==============================================================================
function PS_Light_RaiseByValue(const tag: integer; const value: integer): boolean;

//==============================================================================
// PS_Light_Strobe
//
// Action 116
//
//==============================================================================
function PS_Light_Strobe(const tag: integer; const upper: integer; const lower: integer; const utics: integer; const ltics: integer): boolean;

//==============================================================================
// PS_Light_StrobeDoom
//
// Action 232 (ZDoom)
//
//==============================================================================
function PS_Light_StrobeDoom(const tag: integer; const utics: integer; const ltics: integer): boolean;

//==============================================================================
// PS_Pillar_BuildAndCrush
//
// Action 94
//
//==============================================================================
function PS_Pillar_BuildAndCrush(const tag: integer; const speed: integer; const height: integer): boolean;

//==============================================================================
// PS_Pillar_Build
//
// Action 29
//
//==============================================================================
function PS_Pillar_Build(const tag: integer; const speed: integer; const height: integer): boolean;

//==============================================================================
// PS_Pillar_Open
//
// Action 30
//
//==============================================================================
function PS_Pillar_Open(const tag: integer; const speed: integer; const fdist: integer; const cdist: integer): boolean;

//==============================================================================
// PS_Plat_DownByValue
//
// Action 63
//
//==============================================================================
function PS_Plat_DownByValue(const tag: integer; const speed: integer; const delay: integer; const height: integer): boolean;

//==============================================================================
// PS_Plat_DownWaitUpStay
//
// Action 62
//
//==============================================================================
function PS_Plat_DownWaitUpStay(const tag: integer; const speed: integer; const delay: integer): boolean;

//==============================================================================
// PS_Plat_DownWaitUpStayLip
//
// Action 206 (ZDoom)
//
//==============================================================================
function PS_Plat_DownWaitUpStayLip(const tag: integer; const speed: integer; const delay: integer; const lip: integer): boolean;

//==============================================================================
// PS_Plat_PerpetualRaise
//
// Action 60
//
//==============================================================================
function PS_Plat_PerpetualRaise(const tag: integer; const speed: integer; const delay: integer): boolean;

//==============================================================================
// PS_Plat_PerpetualRaiseLip
//
// Action 207 (ZDoom)
//
//==============================================================================
function PS_Plat_PerpetualRaiseLip(const tag: integer; const speed: integer; const delay: integer; const lip: integer): boolean;

//==============================================================================
// PS_Plat_UpByValue
//
// Action 65
//
//==============================================================================
function PS_Plat_UpByValue(const tag: integer; const speed: integer; const delay: integer; const height: integer): boolean;

//==============================================================================
// PS_Plat_UpNearestWaitDownStay 
//
// Action 172 (ZDoom)
//
//==============================================================================
function PS_Plat_UpNearestWaitDownStay (const tag: integer; const speed: integer; const delay: integer): boolean;

//==============================================================================
// PS_Plat_UpWaitDownStay
//
// Action 64
//
//==============================================================================
function PS_Plat_UpWaitDownStay(const tag: integer; const speed: integer; const delay: integer): boolean;

//==============================================================================
// PS_Polyobj_DoorSlide
//
// Action 8
//
//==============================================================================
function PS_Polyobj_DoorSlide(const po: integer; const speed: integer; const angle: integer; const dist: integer; const delay: integer): boolean;

//==============================================================================
// PS_Polyobj_DoorSwing
//
// Action 7
//
//==============================================================================
function PS_Polyobj_DoorSwing(const po: integer; const speed: integer; const angle: integer; const delay: integer): boolean;

//==============================================================================
// PS_Polyobj_Move
//
// Action 4
//
//==============================================================================
function PS_Polyobj_Move(const po: integer; const speed: integer; const angle: integer; const dist: integer): boolean;

//==============================================================================
// PS_Polyobj_MoveTimes8
//
// Action 6
//
//==============================================================================
function PS_Polyobj_MoveTimes8(const po: integer; const speed: integer; const angle: integer; const dist: integer): boolean;

//==============================================================================
// PS_Polyobj_OR_Move
//
// Action 92
//
//==============================================================================
function PS_Polyobj_OR_Move(const po: integer; const speed: integer; const angle: integer; const dist: integer): boolean;

//==============================================================================
// PS_Polyobj_OR_MoveTimes8
//
// Action 93
//
//==============================================================================
function PS_Polyobj_OR_MoveTimes8(const po: integer; const speed: integer; const angle: integer; const dist: integer): boolean;

//==============================================================================
// PS_Polyobj_OR_RotateLeft
//
// Action 90
//
//==============================================================================
function PS_Polyobj_OR_RotateLeft(const po: integer; const speed: integer; const angle: integer): boolean;

//==============================================================================
// PS_Polyobj_OR_RotateRight
//
// Action 91
//
//==============================================================================
function PS_Polyobj_OR_RotateRight(const po: integer; const speed: integer; const angle: integer): boolean;

//==============================================================================
// PS_Polyobj_RotateLeft
//
// Action 2
//
//==============================================================================
function PS_Polyobj_RotateLeft(const po: integer; const speed: integer; const angle: integer): boolean;

//==============================================================================
// PS_Polyobj_RotateRight
//
// Action 3
//
//==============================================================================
function PS_Polyobj_RotateRight(const po: integer; const speed: integer; const angle: integer): boolean;

implementation

uses
  d_delphi,
  p_spec,
  {$IFDEF HEXEN}
  p_anim,
  {$ELSE}
  udmf_spec,
  {$ENDIF}
  po_man,
  udmf_ceilng,
  udmf_doors,
  udmf_floor,
  udmf_lights,
  udmf_plats;

type
  argsbuffer_t = array[0..7] of byte;
  Pargsbuffer_t = ^argsbuffer_t;

//==============================================================================
//
// ClearArgs
//
//==============================================================================
procedure ClearArgs(const args: Pargsbuffer_t);
begin
  ZeroMemory(args, SizeOf(argsbuffer_t));
end;

//==============================================================================
// PS_Ceiling_CrushAndRaise
//
// Action 42
//
//==============================================================================
function PS_Ceiling_CrushAndRaise(const tag: integer; const speed: integer; const crush: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := crush;
  result := EVH_DoCeiling(nil, @args, CLEV_CRUSHANDRAISE);
end;

//==============================================================================
// PS_Ceiling_CrushRaiseAndStay
//
// Action 45
//
//==============================================================================
function PS_Ceiling_CrushRaiseAndStay(const tag: integer; const speed: integer; const crush: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := crush;
  result := EVH_DoCeiling(nil, @args, CLEV_CRUSHRAISEANDSTAY);
end;

//==============================================================================
// PS_Ceiling_CrushStop
//
// Action 44
//
//==============================================================================
function PS_Ceiling_CrushStop(const tag: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  result := EVH_CeilingCrushStop(nil, @args);
end;

//==============================================================================
// PS_Ceiling_LowerAndCrush
//
// Action 43
//
//==============================================================================
function PS_Ceiling_LowerAndCrush(const tag: integer; const speed: integer; const crush: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := crush;
  result := EVH_DoCeiling(nil, @args, CLEV_LOWERANDCRUSH);
end;

//==============================================================================
// PS_Ceiling_LowerByValue
//
// Action 40
//
//==============================================================================
function PS_Ceiling_LowerByValue(const tag: integer; const speed: integer; const height: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := height;
  result := EVH_DoCeiling(nil, @args, CLEV_LOWERBYVALUE);
end;

//==============================================================================
// PS_Ceiling_LowerByValueTimes8
//
// Action 199 (ZDoom)
//
//==============================================================================
function PS_Ceiling_LowerByValueTimes8(const tag: integer; const speed: integer; const height: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := height;
  result := EVH_DoCeiling(nil, @args, CLEV_LOWERBYVALUETIMES8);
end;

//==============================================================================
// PS_Ceiling_LowerToFloor
//
// Action 254 (ZDoom)
//
//==============================================================================
function PS_Ceiling_LowerToFloor(const tag: integer; const speed: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  result := EVH_DoCeiling(nil, @args, CLEV_LOWERTOFLOOR);
end;

//==============================================================================
// PS_Ceiling_LowerToHighestFloor
//
// Action 192 (ZDoom)
//
//==============================================================================
function PS_Ceiling_LowerToHighestFloor(const tag: integer; const speed: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  result := EVH_DoCeiling(nil, @args, CLEV_LOWERTOMAXFLOOR);
end;

//==============================================================================
// PS_Ceiling_LowerToLowest
//
// Action 253 (ZDoom)
//
//==============================================================================
function PS_Ceiling_LowerToLowest(const tag: integer; const speed: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  result := EVH_DoCeiling(nil, @args, CLEV_LOWERTOLOWEST);
end;

//==============================================================================
// PS_Ceiling_MoveToValueAndCrush
//
// Action 280 (ZDoom)
//
//==============================================================================
function PS_Ceiling_MoveToValueAndCrush(const tag: integer; const speed: integer; const height: integer; const crush: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  PSmallInt(@args[2])^ := height;
  args[4] := crush;
  result := EVH_DoCeiling(nil, @args, CLEV_MOVETOVALUEANDCRUSH);
end;

//==============================================================================
// PS_Ceiling_MoveToValue
//
// Action 47 (ZDoom)
//
//==============================================================================
function PS_Ceiling_MoveToValue(const tag: integer; const speed: integer; const height: integer; const neg: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  PSmallInt(@args[2])^ := height;
  args[4] := neg;
  result := EVH_DoCeiling(nil, @args, CLEV_MOVETOVALUE);
end;

//==============================================================================
// PS_Ceiling_MoveToValueTimes8
//
// Action 69
//
//==============================================================================
function PS_Ceiling_MoveToValueTimes8(const tag: integer; const speed: integer; const height: integer; const neg: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := height;
  args[3] := neg;
  result := EVH_DoCeiling(nil, @args, CLEV_MOVETOVALUETIMES8);
end;

//==============================================================================
// PS_Ceiling_RaiseByValue
//
// Action 41
//
//==============================================================================
function PS_Ceiling_RaiseByValue(const tag: integer; const speed: integer; const height: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := height;
  result := EVH_DoCeiling(nil, @args, CLEV_RAISEBYVALUE);
end;

//==============================================================================
// PS_Ceiling_RaiseByValueTimes8
//
// Action 198 (ZDoom)
//
//==============================================================================
function PS_Ceiling_RaiseByValueTimes8(const tag: integer; const speed: integer; const height: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := height;
  result := EVH_DoCeiling(nil, @args, CLEV_RAISEBYVALUETIMES8);
end;

//==============================================================================
// PS_Ceiling_RaiseToHighestFloor
//
// Action 266 (ZDoom)
//
//==============================================================================
function PS_Ceiling_RaiseToHighestFloor(const tag: integer; const speed: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  result := EVH_DoCeiling(nil, @args, CLEV_RAISETOHIGHESTFLOOR);
end;

//==============================================================================
// PS_Door_Close
//
// Action 10
//
//==============================================================================
function PS_Door_Close(const tag: integer; const speed: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  result := EVH_DoDoor(nil, @args, DREV_CLOSE);
end;

//==============================================================================
// PS_Door_CloseWaitOpen
//
// Action 249 (ZDoom)
//
//==============================================================================
function PS_Door_CloseWaitOpen(const tag: integer; const speed: integer; const delay: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := delay;
  result := EVH_DoDoor(nil, @args, DREV_CLOSEWAITTHENOPEN);
end;

//==============================================================================
// PS_Door_Open
//
// Action 11
//
//==============================================================================
function PS_Door_Open(const tag: integer; const speed: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  result := EVH_DoDoor(nil, @args, DREV_OPEN);
end;

//==============================================================================
// PS_Door_Raise
//
// Action 12
//
//==============================================================================
function PS_Door_Raise(const tag: integer; const speed: integer; delay: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := delay;
  result := EVH_DoDoor(nil, @args, DREV_NORMAL);
end;

//==============================================================================
// PS_FloorAndCeiling_LowerByValue
//
// Action 95
//
//==============================================================================
function PS_FloorAndCeiling_LowerByValue(const tag: integer; const speed: integer; const height: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := height;
  result := EVH_DoFloorAndCeiling(nil, @args, false);
end;

//==============================================================================
// PS_FloorAndCeiling_LowerRaise
//
// Action 251 (ZDoom)
//
//==============================================================================
function PS_FloorAndCeiling_LowerRaise(const tag: integer; const fspeed, cspeed: integer): boolean;
var
  args: argsbuffer_t;
  ret1, ret2: boolean;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := fspeed;
  ret1 := EVH_DoFloor(nil, @args, FLEV_LOWERFLOORTOLOWEST);
  args[1] := cspeed;
  ret2 := EVH_DoCeiling(nil, @args, CLEV_RAISETOHIGHEST);
  result := ret1 or ret2;
end;

//==============================================================================
// PS_FloorAndCeiling_RaiseByValue
//
// Action 96
//
//==============================================================================
function PS_FloorAndCeiling_RaiseByValue(const tag: integer; const speed: integer; const height: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := height;
  result := EVH_DoFloorAndCeiling(nil, @args, true);
end;

//==============================================================================
// PS_Floor_CrushStop
//
// Action 46
//
//==============================================================================
function PS_Floor_CrushStop(const tag: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  result := EVH_FloorCrushStop(nil, @args);
end;

//==============================================================================
// PS_Floor_LowerByValue
//
// Action 20
//
//==============================================================================
function PS_Floor_LowerByValue(const tag: integer; const speed: integer; const height: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := height;
  result := EVH_DoFloor(nil, @args, FLEV_LOWERFLOORBYVALUE);
end;

//==============================================================================
// PS_Floor_LowerByValueTimes8
//
// Action 36
//
//==============================================================================
function PS_Floor_LowerByValueTimes8(const tag: integer; const speed: integer; const height: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := height;
  result := EVH_DoFloor(nil, @args, FLEV_LOWERBYVALUETIMES8);
end;

//==============================================================================
// PS_Floor_LowerToHighest
//
// Action 242 (ZDoom)
//
//==============================================================================
function PS_Floor_LowerToHighest(const tag: integer; const speed: integer; const adjust: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := adjust;
  result := EVH_DoFloor(nil, @args, FLEV_LOWERFLOORTOHIGHEST);
end;

//==============================================================================
// PS_Floor_LowerToLowestCeiling
//
// Action 258 (ZDoom)
//
//==============================================================================
function PS_Floor_LowerToLowestCeiling(const tag: integer; const speed: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  result := EVH_DoFloor(nil, @args, FLEV_LOWERFLOORTOLOWESTCEILING);
end;

//==============================================================================
// PS_Floor_LowerToLowest
//
// Action 21
//
//==============================================================================
function PS_Floor_LowerToLowest(const tag: integer; const speed: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  result := EVH_DoFloor(nil, @args, FLEV_LOWERFLOORTOLOWEST);
end;

//==============================================================================
// PS_Floor_LowerToNearest
//
// Action 22
//
//==============================================================================
function PS_Floor_LowerToNearest(const tag: integer; const speed: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  result := EVH_DoFloor(nil, @args, FLEV_LOWERFLOOR);
end;

//==============================================================================
// PS_Floor_MoveToValue
//
// Action 37 (ZDoom)
//
//==============================================================================
function PS_Floor_MoveToValue(const tag: integer; const speed: integer; const height: integer; const neg: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  PSmallInt(@args[2])^ := height;
  args[4] := neg;
  result := EVH_DoFloor(nil, @args, FLEV_MOVETOVALUE);
end;

//==============================================================================
// PS_Floor_MoveToValueTimes8
//
// Action 68
//
//==============================================================================
function PS_Floor_MoveToValueTimes8(const tag: integer; const speed: integer; const height: integer; const neg: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := height;
  args[3] := neg;
  result := EVH_DoFloor(nil, @args, FLEV_MOVETOVALUETIMES8);
end;

//==============================================================================
// PS_Floor_RaiseAndCrushDoom
//
// Action 99 (ZDoom)
//
//==============================================================================
function PS_Floor_RaiseAndCrushDoom(const tag: integer; const speed: integer; const crush: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := crush;
  result := EVH_DoFloor(nil, @args, FLEV_RAISEFLOORCRUSHDOOM);
end;

//==============================================================================
// PS_Floor_RaiseAndCrush
//
// Action 28
//
//==============================================================================
function PS_Floor_RaiseAndCrush(const tag: integer; const speed: integer; const crush: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := crush;
  result := EVH_DoFloor(nil, @args, FLEV_RAISEFLOORCRUSH);
end;

//==============================================================================
// PS_Floor_RaiseByTexture
//
// Action 240 (ZDoom)
//
//==============================================================================
function PS_Floor_RaiseByTexture(const tag: integer; const speed: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  result := EVH_DoFloor(nil, @args, FLEV_RAISETOTEXTURE);
end;

//==============================================================================
// PS_Floor_RaiseByValue
//
// Action 23
//
//==============================================================================
function PS_Floor_RaiseByValue(const tag: integer; const speed: integer; const height: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := height;
  result := EVH_DoFloor(nil, @args, FLEV_RAISEFLOORBYVALUE);
end;

//==============================================================================
// PS_Floor_RaiseByValueTimes8
//
// Action 35
//
//==============================================================================
function PS_Floor_RaiseByValueTimes8(const tag: integer; const speed: integer; const height: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := height;
  result := EVH_DoFloor(nil, @args, FLEV_RAISEFLOORBYVALUETIMES8);
end;

//==============================================================================
// PS_Floor_RaiseInstant
//
// Action 67
//
//==============================================================================
function PS_Floor_RaiseInstant(const tag: integer; const speed: integer; const height: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := height;
  result := EVH_DoFloor(nil, @args, FLEV_RAISETIMES8INSTANT);
end;

//==============================================================================
// PS_Floor_LowerInstant
//
// Action 66
//
//==============================================================================
function PS_Floor_LowerInstant(const tag: integer; const speed: integer; const height: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := height;
  result := EVH_DoFloor(nil, @args, FLEV_LOWERTIMES8INSTANT);
end;

//==============================================================================
// PS_Floor_RaiseToCeilingNoChange
//
// Action 259 (ZDoom) without the change
//
//==============================================================================
function PS_Floor_RaiseToCeilingNoChange(const tag: integer; const speed: integer; const crush: integer; const gap: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := crush;
  args[3] := gap;
  result := EVH_DoFloor(nil, @args, FLEV_RAISETOCEILING);
end;

//==============================================================================
// PS_Floor_RaiseToHighest
//
// Action 24
//
//==============================================================================
function PS_Floor_RaiseToHighest(const tag: integer; const speed: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  result := EVH_DoFloor(nil, @args, FLEV_RAISEFLOOR);
end;

//==============================================================================
// PS_Floor_RaiseToLowestCeiling
//
// Action 238 (ZDoom)
//
//==============================================================================
function PS_Floor_RaiseToLowestCeiling(const tag: integer; const speed: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  result := EVH_DoFloor(nil, @args, FLEV_RAISETOLOWESTCEILING);
end;

//==============================================================================
// PS_Floor_RaiseToNearest
//
// Action 25
//
//==============================================================================
function PS_Floor_RaiseToNearest(const tag: integer; const speed: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  result := EVH_DoFloor(nil, @args, FLEV_RAISEFLOORTONEAREST);
end;

//==============================================================================
// PS_Floor_ToCeilingInstantNoChange
//
// Action 260 (ZDoom) without the change
//
//==============================================================================
function PS_Floor_ToCeilingInstantNoChange(const tag: integer; const crush: integer; const gap: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := 0;
  args[2] := crush;
  args[3] := gap;
  result := EVH_DoFloor(nil, @args, FLEV_FLOORTOCEILINGINSTANT);
end;

//==============================================================================
// PS_Floor_Waggle
//
// Action 138
//
//==============================================================================
function PS_Floor_Waggle(const tag: integer; const amp: integer; const freq: integer; const offset: integer; const time: integer): boolean;
begin
  result := EVH_StartFloorWaggle(tag, amp, freq, offset, time);
end;

//==============================================================================
// PS_Light_ChangeToValue
//
// Action 112
//
//==============================================================================
function PS_Light_ChangeToValue(const tag: integer; const value: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := value;
  result := EVH_SpawnLight(nil, @args, LITE_CHANGETOVALUE);
end;

//==============================================================================
// PS_Light_Fade
//
// Action 113
//
//==============================================================================
function PS_Light_Fade(const tag: integer; const value: integer; const tics: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := value;
  args[2] := tics;
  result := EVH_SpawnLight(nil, @args, LITE_FADE);
end;

//==============================================================================
// PS_Light_Flicker
//
// Action 115
//
//==============================================================================
function PS_Light_Flicker(const tag: integer; const upper: integer; const lower: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := upper;
  args[2] := lower;
  result := EVH_SpawnLight(nil, @args, LITE_FLICKER);
end;

//==============================================================================
// PS_Light_ForceLightning
//
// Action 109
//
//==============================================================================
function PS_Light_ForceLightning: boolean;
begin
  P_ForceLightning(0);
  result := true;
end;

//==============================================================================
// PS_Light_ForceLightningTics
//
// Action 109
//
//==============================================================================
function PS_Light_ForceLightningTics(const tics: integer): boolean;
begin
  P_ForceLightning(tics);
  result := true;
end;

//==============================================================================
// PS_Light_Glow
//
// Action 114
//
//==============================================================================
function PS_Light_Glow(const tag: integer; const upper: integer; const lower: integer; const tics: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := upper;
  args[2] := lower;
  args[3] := tics;
  result := EVH_SpawnLight(nil, @args, LITE_GLOW);
end;

//==============================================================================
// PS_Light_LowerByValue
//
// Action 111
//
//==============================================================================
function PS_Light_LowerByValue(const tag: integer; const value: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := value;
  result := EVH_SpawnLight(nil, @args, LITE_LOWERBYVALUE);
end;

//==============================================================================
// PS_Light_MaxNeighbor
//
// Action 234 (ZDoom)
//
//==============================================================================
function PS_Light_MaxNeighbor(const tag: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  result := EVH_SpawnLight(nil, @args, LITE_MAXNEIGHTBOR);
end;

//==============================================================================
// PS_Light_MinNeighbor
//
// Action 233 (ZDoom)
//
//==============================================================================
function PS_Light_MinNeighbor(const tag: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  result := EVH_SpawnLight(nil, @args, LITE_MINNEIGHTBOR);
end;

//==============================================================================
// PS_Light_RaiseByValue
//
// Action 110
//
//==============================================================================
function PS_Light_RaiseByValue(const tag: integer; const value: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  result := EVH_SpawnLight(nil, @args, LITE_RAISEBYVALUE);
end;

//==============================================================================
// PS_Light_Strobe
//
// Action 116
//
//==============================================================================
function PS_Light_Strobe(const tag: integer; const upper: integer; const lower: integer; const utics: integer; const ltics: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := upper;
  args[2] := lower;
  args[3] := utics;
  args[4] := ltics;
  result := EVH_SpawnLight(nil, @args, LITE_STROBE);
end;

//==============================================================================
// PS_Light_StrobeDoom
//
// Action 232 (ZDoom)
//
//==============================================================================
function PS_Light_StrobeDoom(const tag: integer; const utics: integer; const ltics: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := utics;
  args[2] := ltics;
  result := EVH_SpawnLight(nil, @args, LITE_STROBEDOOM);
end;

//==============================================================================
// PS_Pillar_BuildAndCrush
//
// Action 94
//
//==============================================================================
function PS_Pillar_BuildAndCrush(const tag: integer; const speed: integer; const height: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := height;
  result := EVH_BuildPillar(nil, @args, true);
end;

//==============================================================================
// PS_Pillar_Build
//
// Action 29
//
//==============================================================================
function PS_Pillar_Build(const tag: integer; const speed: integer; const height: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := height;
  result := EVH_BuildPillar(nil, @args, false);
end;

//==============================================================================
// PS_Pillar_Open
//
// Action 30
//
//==============================================================================
function PS_Pillar_Open(const tag: integer; const speed: integer; const fdist: integer; const cdist: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := fdist;
  args[3] := cdist;
  result := EVH_OpenPillar(nil, @args);
end;

//==============================================================================
// PS_Plat_DownByValue
//
// Action 63
//
//==============================================================================
function PS_Plat_DownByValue(const tag: integer; const speed: integer; const delay: integer; const height: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := delay;
  args[3] := height;
  result := EVH_DoPlat(nil, @args, PLAT_DOWNBYVALUEWAITUPSTAY, 0);
end;

//==============================================================================
// PS_Plat_DownWaitUpStay
//
// Action 62
//
//==============================================================================
function PS_Plat_DownWaitUpStay(const tag: integer; const speed: integer; const delay: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := delay;
  result := EVH_DoPlat(nil, @args, PLAT_DOWNWAITUPSTAY, 0);
end;

//==============================================================================
// PS_Plat_DownWaitUpStayLip
//
// Action 206 (ZDoom)
//
//==============================================================================
function PS_Plat_DownWaitUpStayLip(const tag: integer; const speed: integer; const delay: integer; const lip: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := delay;
  args[3] := lip;
  result := EVH_DoPlat(nil, @args, PLAT_DOWNWAITUPSTAYLIP, 0);
end;

//==============================================================================
// PS_Plat_PerpetualRaise
//
// Action 60
//
//==============================================================================
function PS_Plat_PerpetualRaise(const tag: integer; const speed: integer; const delay: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := delay;
  result := EVH_DoPlat(nil, @args, PLAT_PERPETUALRAISE, 0);
end;

//==============================================================================
// PS_Plat_PerpetualRaiseLip
//
// Action 207 (ZDoom)
//
//==============================================================================
function PS_Plat_PerpetualRaiseLip(const tag: integer; const speed: integer; const delay: integer; const lip: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := delay;
  args[3] := lip;
  result := EVH_DoPlat(nil, @args, PLAT_PERPETUALRAISELIP, 0);
end;

//==============================================================================
// PS_Plat_UpByValue
//
// Action 65
//
//==============================================================================
function PS_Plat_UpByValue(const tag: integer; const speed: integer; const delay: integer; const height: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := delay;
  args[3] := height;
  result := EVH_DoPlat(nil, @args, PLAT_UPBYVALUEWAITDOWNSTAY, 0);
end;

//==============================================================================
// PS_Plat_UpNearestWaitDownStay 
//
// Action 172 (ZDoom)
//
//==============================================================================
function PS_Plat_UpNearestWaitDownStay (const tag: integer; const speed: integer; const delay: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := delay;
  result := EVH_DoPlat(nil, @args, PLAT_UPNEARESTWAITDOWNSTAY, 0);
end;

//==============================================================================
// PS_Plat_UpWaitDownStay
//
// Action 64
//
//==============================================================================
function PS_Plat_UpWaitDownStay(const tag: integer; const speed: integer; const delay: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := tag;
  args[1] := speed;
  args[2] := delay;
  result := EVH_DoPlat(nil, @args, PLAT_UPWAITDOWNSTAY, 0);
end;

//==============================================================================
// PS_Polyobj_DoorSlide
//
// Action 8
//
//==============================================================================
function PS_Polyobj_DoorSlide(const po: integer; const speed: integer; const angle: integer; const dist: integer; const delay: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := po;
  args[1] := speed;
  args[2] := angle;
  args[3] := dist;
  args[4] := delay;
  result := EVH_OpenPolyDoor(nil, @args, PODOOR_SLIDE);
end;

//==============================================================================
// PS_Polyobj_DoorSwing
//
// Action 7
//
//==============================================================================
function PS_Polyobj_DoorSwing(const po: integer; const speed: integer; const angle: integer; const delay: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := po;
  args[1] := Abs(speed);
  args[2] := angle;
  args[3] := delay;
  if speed < 0 then
    args[4] := 1;
  result := EVH_OpenPolyDoor(nil, @args, PODOOR_SWING);
end;

//==============================================================================
// PS_Polyobj_Move
//
// Action 4
//
//==============================================================================
function PS_Polyobj_Move(const po: integer; const speed: integer; const angle: integer; const dist: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := po;
  args[1] := speed;
  args[2] := angle;
  args[3] := dist;
  result := EVH_MovePoly(nil, @args, false, false);
end;

//==============================================================================
// PS_Polyobj_MoveTimes8
//
// Action 6
//
//==============================================================================
function PS_Polyobj_MoveTimes8(const po: integer; const speed: integer; const angle: integer; const dist: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := po;
  args[1] := speed;
  args[2] := angle;
  args[3] := dist;
  result := EVH_MovePoly(nil, @args, true, false);
end;

//==============================================================================
// PS_Polyobj_OR_Move
//
// Action 92
//
//==============================================================================
function PS_Polyobj_OR_Move(const po: integer; const speed: integer; const angle: integer; const dist: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := po;
  args[1] := speed;
  args[2] := angle;
  args[3] := dist;
  result := EVH_MovePoly(nil, @args, false, true);
end;

//==============================================================================
// PS_Polyobj_OR_MoveTimes8
//
// Action 93
//
//==============================================================================
function PS_Polyobj_OR_MoveTimes8(const po: integer; const speed: integer; const angle: integer; const dist: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := po;
  args[1] := speed;
  args[2] := angle;
  args[3] := dist;
  result := EVH_MovePoly(nil, @args, true, true);
end;

//==============================================================================
// PS_Polyobj_OR_RotateLeft
//
// Action 90
//
//==============================================================================
function PS_Polyobj_OR_RotateLeft(const po: integer; const speed: integer; const angle: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := po;
  args[1] := speed;
  args[2] := angle;
  result := EVH_RotatePoly(nil, @args, 1, true);
end;

//==============================================================================
// PS_Polyobj_OR_RotateRight
//
// Action 91
//
//==============================================================================
function PS_Polyobj_OR_RotateRight(const po: integer; const speed: integer; const angle: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := po;
  args[1] := speed;
  args[2] := angle;
  result := EVH_RotatePoly(nil, @args, -1, true);
end;

//==============================================================================
// PS_Polyobj_RotateLeft
//
// Action 2
//
//==============================================================================
function PS_Polyobj_RotateLeft(const po: integer; const speed: integer; const angle: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := po;
  args[1] := speed;
  args[2] := angle;
  result := EVH_RotatePoly(nil, @args, 1, false);
end;

//==============================================================================
// PS_Polyobj_RotateRight
//
// Action 3
//
//==============================================================================
function PS_Polyobj_RotateRight(const po: integer; const speed: integer; const angle: integer): boolean;
var
  args: argsbuffer_t;
begin
  ClearArgs(@args);
  args[0] := po;
  args[1] := speed;
  args[2] := angle;
  result := EVH_RotatePoly(nil, @args, -1, false);
end;

end.

