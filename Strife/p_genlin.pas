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
// DESCRIPTION:
//  Generalized linedef type handlers
//  Floors, Ceilings, Doors, Locked Doors, Lifts, Stairs, Crushers
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_genlin;

interface

uses
  r_defs;

//jff 3/14/98 add bits and shifts for generalized sector types

const
  DAMAGE_MASK    = $60;
  DAMAGE_SHIFT   = 5;
  SECRET_MASK    = $80;
  SECRET_SHIFT   = 7;
  FRICTION_MASK  = $100;
  FRICTION_SHIFT = 8;
  PUSH_MASK      = $200;
  PUSH_SHIFT     = 9;

//jff 02/04/98 Define masks, shifts, for fields in
// generalized linedef types

  CGENFLOORBASE        = $6000;
  CGENCEILINGBASE      = $4000;
  CGENDOORBASE         = $3C00;
  CGENLOCKEDBASE       = $3800;
  CGENLIFTBASE         = $3400;
  CGENSTAIRSBASE       = $3000;
  CGENCRUSHERBASE      = $2F80;

  TriggerType         = $0007;
  TriggerTypeShift    = 0;

// define masks and shifts for the floor type fields

  gen_FloorCrush          = $1000;
  gen_FloorChange         = $0C00;
  gen_FloorTarget         = $0380;
  gen_FloorDirection      = $0040;
  gen_FloorModel          = $0020;
  gen_FloorSpeed          = $0018;

  FloorCrushShift         = 12;
  FloorChangeShift        = 10;
  FloorTargetShift         = 7;
  FloorDirectionShift      = 6;
  FloorModelShift          = 5;
  FloorSpeedShift          = 3;

// define masks and shifts for the ceiling type fields

  CeilingCrush        = $1000;
  CeilingChange       = $0C00;
  CeilingTarget       = $0380;
  CeilingDirection    = $0040;
  CeilingModel        = $0020;
  CeilingSpeed        = $0018;

  CeilingCrushShift       = 12;
  CeilingChangeShift      = 10;
  CeilingTargetShift       = 7;
  CeilingDirectionShift    = 6;
  CeilingModelShift        = 5;
  CeilingSpeedShift        = 3;

// define masks and shifts for the lift type fields

  LiftTarget          = $0300;
  LiftDelay           = $00C0;
  LiftMonster         = $0020;
  LiftSpeed           = $0018;

  LiftTargetShift          = 8;
  LiftDelayShift           = 6;
  LiftMonsterShift         = 5;
  LiftSpeedShift           = 3;

// define masks and shifts for the stairs type fields

  StairIgnore         = $0200;
  StairDirection      = $0100;
  StairStep           = $00c0;
  StairMonster        = $0020;
  StairSpeed          = $0018;

  StairIgnoreShift         = 9;
  StairDirectionShift      = 8;
  StairStepShift           = 6;
  StairMonsterShift        = 5;
  StairSpeedShift          = 3;

// define masks and shifts for the crusher type fields

  CrusherSilent       = $0040;
  CrusherMonster      = $0020;
  CrusherSpeed        = $0018;

  CrusherSilentShift       = 6;
  CrusherMonsterShift      = 5;
  CrusherSpeedShift        = 3;

// define masks and shifts for the door type fields

  DoorDelay           = $0300;
  DoorMonster         = $0080;
  DoorKind            = $0060;
  DoorSpeed           = $0018;

  DoorDelayShift           = 8;
  DoorMonsterShift         = 7;
  DoorKindShift            = 5;
  DoorSpeedShift           = 3;

// define masks and shifts for the locked door type fields

  LockedNKeys         = $0200;
  LockedKey           = $01c0;
  LockedKind          = $0020;
  LockedSpeed         = $0018;

  LockedNKeysShift         = 9;
  LockedKeyShift           = 6;
  LockedKindShift          = 5;
  LockedSpeedShift         = 3;

// define names for the TriggerType field of the general linedefs

type
  triggertype_e = (
    WalkOnce,
    WalkMany,
    SwitchOnce,
    SwitchMany,
    GunOnce,
    GunMany,
    PushOnce,
    PushMany
  );

// define names for the Speed field of the general linedefs

  motionspeed_e = (
    SpeedSlow,
    SpeedNormal,
    SpeedFast,
    SpeedTurbo
  );

// define names for the Target field of the general floor

  floortarget_e = (
    FtoHnF,
    FtoLnF,
    FtoNnF,
    FtoLnC,
    FtoC,
    FbyST,
    Fby24,
    Fby32
  );

// define names for the Changer Type field of the general floor

  floorchange_e = (
    FNoChg,
    FChgZero,
    FChgTxt,
    FChgTyp
  );

// define names for the Change Model field of the general floor

  floormodel_t = (
    FTriggerModel,
    FNumericModel
  );

// define names for the Target field of the general ceiling

  ceilingtarget_e = (
    CtoHnC,
    CtoLnC,
    CtoNnC,
    CtoHnF,
    CtoF,
    CbyST,
    Cby24,
    Cby32
  );

// define names for the Changer Type field of the general ceiling

  ceilingchange_e = (
    CNoChg,
    CChgZero,
    CChgTxt,
    CChgTyp
  );

// define names for the Change Model field of the general ceiling

  ceilingmodel_t = (
    CTriggerModel,
    CNumericModel
  );

// define names for the Target field of the general lift

  lifttarget_e = (
    F2LnF,
    F2NnF,
    F2LnC,
    LnF2HnF
  );

// define names for the door Kind field of the general ceiling

  doorkind_e = (
    OdCDoor,
    ODoor,
    CdODoor,
    CDoor
  );

// define names for the locked door Kind field of the general ceiling

  keykind_e = (
    AnyKey,
    RCard,
    BCard,
    YCard,
    RSkull,
    BSkull,
    YSkull,
    AllKeys
  );

//==============================================================================
//
// EV_DoGenFloor
//
//==============================================================================
function EV_DoGenFloor(line: Pline_t): integer;

//==============================================================================
//
// EV_DoGenCeiling
//
//==============================================================================
function EV_DoGenCeiling(line: Pline_t): integer;

//==============================================================================
//
// EV_DoGenDoor
//
//==============================================================================
function EV_DoGenDoor(line: Pline_t): integer;

//==============================================================================
//
// EV_DoGenLockedDoor
//
//==============================================================================
function EV_DoGenLockedDoor(line: Pline_t): integer;

//==============================================================================
//
// EV_DoGenLift
//
//==============================================================================
function EV_DoGenLift(line: Pline_t): integer;

//==============================================================================
//
// EV_DoGenStairs
//
//==============================================================================
function EV_DoGenStairs(line: Pline_t): integer;

//==============================================================================
//
// EV_DoGenCrusher
//
//==============================================================================
function EV_DoGenCrusher(line: Pline_t): integer;

implementation

uses
  d_delphi,
  doomdef,
  m_fixed,
  m_rnd,
  p_ceilng,
  p_doors,
  p_floor,
  p_plats,
  p_setup,
  p_spec,
  p_tick,
  sounddata,
  s_sound,
  z_zone;

//////////////////////////////////////////////////////////
//
// Generalized Linedef Type handlers
//
//////////////////////////////////////////////////////////
//
// EV_DoGenFloor
//
// Handle generalized floor types
//
// Passed the line activating the generalized floor function
// Returns true if a thinker is created
//
// jff 02/04/98 Added this routine (and file) to handle generalized
// floor movers using bit fields in the line special type.
//
//==============================================================================
function EV_DoGenFloor(line: Pline_t): integer;
var
  secnum: integer;
  manual: boolean;
  sec: Psector_t;
  floor: Pfloormove_t;
  value: integer;
  Crsh: integer;
  ChgT: integer;
  Targ: integer;
  Dirn: integer;
  ChgM: integer;
  Sped: integer;
  Trig: integer;
label
  manual_floor;
begin
  value := word(line.special) - CGENFLOORBASE;

  // parse the bit fields in the line's special type
  Crsh := (value and gen_FloorCrush) shr FloorCrushShift;
  ChgT := (value and gen_FloorChange) shr FloorChangeShift;
  Targ := (value and gen_FloorTarget) shr FloorTargetShift;
  Dirn := (value and gen_FloorDirection) shr FloorDirectionShift;
  ChgM := (value and gen_FloorModel) shr FloorModelShift;
  Sped := (value and gen_FloorSpeed) shr FloorSpeedShift;
  Trig := (value and TriggerType) shr TriggerTypeShift;

  result := 0;

  // check if a manual trigger, if so do just the sector on the backside
  manual := false;
  if (Trig = Ord(PushOnce)) or (Trig = Ord(PushMany)) then
  begin
    sec := line.backsector;
    if sec = nil then
      exit;
    secnum := pDiff(sec, sectors, SizeOf(sector_t));
    manual := true;
    goto manual_floor;
  end;

  secnum := -1;
  // if not manual do all sectors tagged the same as the line
  while P_FindSectorFromLineTag2(line, secnum) >= 0 do
  begin
    sec := @sectors[secnum];

manual_floor:
    // Do not start another function if floor already moving
    if P_SectorActive(floor_special, sec) then
    begin
      if not manual then
        continue
      else
        exit;
    end;

    // new floor thinker
    result := 1;
    floor := Z_Malloc(SizeOf(floormove_t), PU_LEVSPEC, nil);
    P_AddThinker(@floor.thinker);
    sec.floordata := floor;
    floor.thinker._function.acp1 := @T_MoveFloor;
    floor.crush := Crsh <> 0;
    if Dirn <> 0 then
      floor.direction := 1
    else
      floor.direction := -1;
    floor.sector := sec;
    floor.texture := sec.floorpic;
    floor.newspecial := sec.special;
    //jff 3/14/98 transfer old special field too
    floor.oldspecial := sec.oldspecial;
    floor._type := genFloor;

    // set the speed of motion
    case Sped of
      Ord(SpeedSlow):
        floor.speed := FLOORSPEED;
      Ord(SpeedNormal):
        floor.speed := FLOORSPEED * 2;
      Ord(SpeedFast):
        floor.speed := FLOORSPEED * 4;
      Ord(SpeedTurbo):
        floor.speed := FLOORSPEED * 8;
    end;

    // set the destination height
    case Targ of
      Ord(FtoHnF):
        floor.floordestheight := P_FindHighestFloorSurrounding(sec);
      Ord(FtoLnF):
        floor.floordestheight := P_FindLowestFloorSurrounding(sec);
      Ord(FtoNnF):
        begin
          if Dirn <> 0 then
            floor.floordestheight := P_FindNextHighestFloor(sec, sec.floorheight)
          else
            floor.floordestheight := P_FindNextLowestFloor(sec, sec.floorheight);
        end;
      Ord(FtoLnC):
        floor.floordestheight := P_FindLowestCeilingSurrounding(sec);
      Ord(FtoC):
        floor.floordestheight := sec.ceilingheight;
      Ord(FbyST):
        begin
          floor.floordestheight := FixedInt(floor.sector.floorheight) +
              floor.direction * FixedInt(P_FindShortestTextureAround(secnum));
          if floor.floordestheight > 32000 then  //jff 3/13/98 prevent overflow
            floor.floordestheight := 32000;    // wraparound in floor height
          if floor.floordestheight < -32000 then
            floor.floordestheight := -32000;
          floor.floordestheight := floor.floordestheight * FRACUNIT;
        end;
      Ord(Fby24):
        floor.floordestheight := floor.sector.floorheight + floor.direction * 24 * FRACUNIT;
      Ord(Fby32):
        floor.floordestheight := floor.sector.floorheight + floor.direction * 32 * FRACUNIT;
    end;

    // set texture/type change properties
    if ChgT <> 0 then   // if a texture change is indicated
    begin
      if ChgM <> 0 then // if a numeric model change
      begin
        //jff 5/23/98 find model with ceiling at target height if target
        //is a ceiling type
        if (Targ = Ord(FtoLnC)) or (Targ = Ord(FtoC)) then
          sec := P_FindModelCeilingSector(floor.floordestheight, secnum)
        else
          sec := P_FindModelFloorSector(floor.floordestheight, secnum);
        if sec <> nil then
        begin
          floor.texture := sec.floorpic;
          case ChgT of
            Ord(FChgZero):  // zero type
              begin
                floor.newspecial := 0;
                //jff 3/14/98 change old field too
                floor.oldspecial := 0;
                floor._type := genFloorChg0;
              end;
            Ord(FChgTyp):   // copy type
              begin
                floor.newspecial := sec.special;
                //jff 3/14/98 change old field too
                floor.oldspecial := sec.oldspecial;
                floor._type := genFloorChgT;
              end;
            Ord(FChgTxt):   // leave type be
              floor._type := genFloorChg;
          end;
        end;
      end
      else     // else if a trigger model change
      begin
        floor.texture := line.frontsector.floorpic;
        case ChgT of
          Ord(FChgZero):    // zero type
            begin
              floor.newspecial := 0;
              //jff 3/14/98 change old field too
              floor.oldspecial := 0;
              floor._type := genFloorChg0;
            end;
          Ord(FChgTyp):     // copy type
            begin
              floor.newspecial := line.frontsector.special;
              //jff 3/14/98 change old field too
              floor.oldspecial := line.frontsector.oldspecial;
              floor._type := genFloorChgT;
            end;
          Ord(FChgTxt):     // leave type be
            floor._type := genFloorChg;
        end;
      end;
    end;
    if manual then
      exit;
  end;
end;

//==============================================================================
//
// EV_DoGenCeiling
//
// Handle generalized ceiling types
//
// Passed the linedef activating the ceiling function
// Returns true if a thinker created
//
// jff 02/04/98 Added this routine (and file) to handle generalized
// floor movers using bit fields in the line special type.
//
//==============================================================================
function EV_DoGenCeiling(line: Pline_t): integer;
var
  secnum: integer;
  manual: boolean;
  sec: Psector_t;
  targheight: fixed_t;
  ceiling: Pceiling_t;
  value: integer;
  Crsh: integer;
  ChgT: integer;
  Targ: integer;
  Dirn: integer;
  ChgM: integer;
  Sped: integer;
  Trig: integer;
label
  manual_ceiling;
begin
  value := word(line.special) - CGENCEILINGBASE;

  // parse the bit fields in the line's special type
  Crsh := (value and CeilingCrush) shr CeilingCrushShift;
  ChgT := (value and CeilingChange) shr CeilingChangeShift;
  Targ := (value and CeilingTarget) shr CeilingTargetShift;
  Dirn := (value and CeilingDirection) shr CeilingDirectionShift;
  ChgM := (value and CeilingModel) shr CeilingModelShift;
  Sped := (value and CeilingSpeed) shr CeilingSpeedShift;
  Trig := (value and TriggerType) shr TriggerTypeShift;

  result := 0;

  // check if a manual trigger, if so do just the sector on the backside
  manual := false;
  if (Trig = Ord(PushOnce)) or (Trig = Ord(PushMany)) then
  begin
    sec := line.backsector;
    if sec = nil then
      exit;
    secnum := pDiff(sec, sectors, SizeOf(sector_t));
    manual := true;
    goto manual_ceiling;
  end;

  secnum := -1;
  // if not manual do all sectors tagged the same as the line
  while P_FindSectorFromLineTag2(line, secnum) >= 0 do
  begin
    sec := @sectors[secnum];

manual_ceiling:
    // Do not start another function if ceiling already moving
    if P_SectorActive(ceiling_special, sec) then //jff 2/22/98
    begin
      if not manual then
        continue
      else
        exit;
    end;

    // new ceiling thinker
    result := 1;
    ceiling := Z_Malloc(SizeOf(ceiling_t), PU_LEVSPEC, nil);
    P_AddThinker (@ceiling.thinker);
    sec.ceilingdata := ceiling; //jff 2/22/98
    ceiling.thinker._function.acp1 := @T_MoveCeiling;
    ceiling.crush := Crsh <> 0;
    if Dirn <> 0 then
      ceiling.direction := 1
    else
      ceiling.direction := -1;
    ceiling.sector := sec;
    ceiling.texture := sec.ceilingpic;
    ceiling.newspecial := sec.special;
    //jff 3/14/98 change old field too
    ceiling.oldspecial := sec.oldspecial;
    ceiling.tag := sec.tag;
    ceiling._type := genCeiling;

    // set speed of motion
    case Sped of
      Ord(SpeedSlow):
        ceiling.speed := CEILSPEED;
      Ord(SpeedNormal):
        ceiling.speed := CEILSPEED * 2;
      Ord(SpeedFast):
        ceiling.speed := CEILSPEED * 4;
      Ord(SpeedTurbo):
        ceiling.speed := CEILSPEED * 8;
    end;

    // set destination target height
    targheight := sec.ceilingheight;
    case Targ of
      Ord(CtoHnC):
        targheight := P_FindHighestCeilingSurrounding(sec);
      Ord(CtoLnC):
        targheight := P_FindLowestCeilingSurrounding(sec);
      Ord(CtoNnC):
        begin
          if Dirn <> 0 then
            targheight := P_FindNextHighestCeiling(sec,sec.ceilingheight)
          else
            targheight := P_FindNextLowestCeiling(sec,sec.ceilingheight);
        end;
      Ord(CtoHnF):
        targheight := P_FindHighestFloorSurrounding(sec);
      Ord(CtoF):
        targheight := sec.floorheight;
      Ord(CbyST):
        begin
          targheight :=  FixedInt(ceiling.sector.ceilingheight) +
            ceiling.direction * FixedInt(P_FindShortestUpperAround(secnum));
          if targheight > 32000 then  //jff 3/13/98 prevent overflow
            targheight := 32000;    // wraparound in ceiling height
          if targheight < -32000 then
            targheight := -32000;
          targheight := targheight * FRACUNIT;
        end;
      Ord(Cby24):
        targheight := ceiling.sector.ceilingheight + ceiling.direction * 24 * FRACUNIT;
      Ord(Cby32):
        targheight := ceiling.sector.ceilingheight + ceiling.direction * 32 * FRACUNIT;
    end;
    // proff 6/17/98: Visual C doesn't like the other form
    if Dirn <> 0 then
      ceiling.topheight := targheight
    else
      ceiling.bottomheight := targheight;

    // set texture/type change properties
    if ChgT <> 0 then     // if a texture change is indicated
    begin
      if ChgM <> 0 then   // if a numeric model change
      begin
        //jff 5/23/98 find model with floor at target height if target
        //is a floor type
        if (Targ = Ord(CtoHnF)) or (Targ = Ord(CtoF)) then
          sec := P_FindModelFloorSector(targheight, secnum)
        else
          sec := P_FindModelCeilingSector(targheight, secnum);
        if sec <> nil then
        begin
          ceiling.texture := sec.ceilingpic;
          case ChgT of
            Ord(CChgZero):  // type is zeroed
              begin
                ceiling.newspecial := 0;
                //jff 3/14/98 change old field too
                ceiling.oldspecial := 0;
                ceiling._type := genCeilingChg0;
              end;
            Ord(CChgTyp):   // type is copied
              begin
                ceiling.newspecial := sec.special;
                //jff 3/14/98 change old field too
                ceiling.oldspecial := sec.oldspecial;
                ceiling._type := genCeilingChgT;
              end;
            Ord(CChgTxt):   // type is left alone
              ceiling._type := genCeilingChg;
          end;
        end;
      end
      else        // else if a trigger model change
      begin
        ceiling.texture := line.frontsector.ceilingpic;
        case ChgT of
          Ord(CChgZero):    // type is zeroed
            begin
              ceiling.newspecial := 0;
              //jff 3/14/98 change old field too
              ceiling.oldspecial := 0;
              ceiling._type := genCeilingChg0;
            end;
          Ord(CChgTyp):     // type is copied
            begin
              ceiling.newspecial := line.frontsector.special;
              //jff 3/14/98 change old field too
              ceiling.oldspecial := line.frontsector.oldspecial;
              ceiling._type := genCeilingChgT;
            end;
          Ord(CChgTxt):     // type is left alone
            ceiling._type := genCeilingChg;
        end;
      end;
    end;
    P_AddActiveCeiling(ceiling);  // add this ceiling to the active list
    if manual then
      exit;
  end;
end;

//==============================================================================
//
// EV_DoGenLift
//
// Handle generalized lift types
//
// Passed the linedef activating the lift
// Returns true if a thinker is created
//
//==============================================================================
function EV_DoGenLift(line: Pline_t): integer;
var
  secnum: integer;
  manual: boolean;
  sec: Psector_t;
  plat: Pplat_t;
  value: integer;
  Targ: integer;
  Dely: integer;
  Sped: integer;
  Trig: integer;
label
  manual_lift;
begin
  value := word(line.special) - CGENLIFTBASE;

  // parse the bit fields in the line's special type
  Targ := (value and LiftTarget) shr LiftTargetShift;
  Dely := (value and LiftDelay) shr LiftDelayShift;
  Sped := (value and LiftSpeed) shr LiftSpeedShift;
  Trig := (value and TriggerType) shr TriggerTypeShift;

  secnum := -1;
  result := 0;

  // Activate all <type> plats that are in_stasis

  if Targ = Ord(LnF2HnF) then
    P_ActivateInStasis(line.tag);

  // check if a manual trigger, if so do just the sector on the backside
  manual := false;
  if (Trig = Ord(PushOnce)) or (Trig = Ord(PushMany)) then
  begin
    sec := line.backsector;
    if sec = nil then
      exit;
    secnum := pDiff(sec, sectors, SizeOf(sector_t));
    manual := true;
    goto manual_lift;
  end;

  // if not manual do all sectors tagged the same as the line
  while P_FindSectorFromLineTag2(line, secnum) >= 0 do
  begin
    sec := @sectors[secnum];

manual_lift:
    // Do not start another function if floor already moving
    if P_SectorActive(floor_special, sec) then
    begin
      if not manual then
        continue
      else
        exit;
    end;

    // Setup the plat thinker
    result := 1;
    plat := Z_Malloc(SizeOf(plat_t), PU_LEVSPEC, nil);
    P_AddThinker(@plat.thinker);

    plat.sector := sec;
    plat.sector.floordata := plat;
    plat.thinker._function.acp1 := @T_PlatRaise;
    plat.crush := false;
    plat.tag := line.tag;

    plat._type := genLift;
    plat.high := sec.floorheight;
    plat.status := down;

    // setup the target destination height
    case Targ of
      Ord(F2LnF):
        begin
          plat.low := P_FindLowestFloorSurrounding(sec);
          if plat.low > sec.floorheight then
            plat.low := sec.floorheight;
        end;
      Ord(F2NnF):
          plat.low := P_FindNextLowestFloor(sec, sec.floorheight);
      Ord(F2LnC):
        begin
          plat.low := P_FindLowestCeilingSurrounding(sec);
          if plat.low > sec.floorheight then
            plat.low := sec.floorheight;
        end;
      Ord(LnF2HnF):
        begin
          plat._type := genPerpetual;
          plat.low := P_FindLowestFloorSurrounding(sec);
          if plat.low > sec.floorheight then
            plat.low := sec.floorheight;
          plat.high := P_FindHighestFloorSurrounding(sec);
          if plat.high < sec.floorheight then
            plat.high := sec.floorheight;
          plat.status := plat_e(N_Random and 1);
        end;
    end;

    // setup the speed of motion
    case Sped of
      Ord(SpeedSlow):
        plat.speed := PLATSPEED * 2;
      Ord(SpeedNormal):
        plat.speed := PLATSPEED * 4;
      Ord(SpeedFast):
        plat.speed := PLATSPEED * 8;
      Ord(SpeedTurbo):
        plat.speed := PLATSPEED * 16;
    end;

    // setup the delay time before the floor returns
    case Dely of
      0:
        plat.wait := 1 * TICRATE;
      1:
        plat.wait := PLATWAIT * TICRATE;
      2:
        plat.wait := 5 * TICRATE;
      3:
        plat.wait := 10 * TICRATE;
    end;

    S_StartSound(@sec.soundorg, Ord(sfx_pstart));
    P_AddActivePlat(plat); // add this plat to the list of active plats

    if manual then
      exit;
  end;
end;

//==============================================================================
//
// EV_DoGenStairs
//
// Handle generalized stair building
//
// Passed the linedef activating the stairs
// Returns true if a thinker is created
//
//==============================================================================
function EV_DoGenStairs(line: Pline_t): integer;
var
  secnum: integer;
  osecnum: integer; //jff 3/4/98 preserve loop index
  height: integer;
  i: integer;
  newsecnum: integer;
  texture: integer;
  ok: integer;
  manual: boolean;
  sec: Psector_t;
  tsec: Psector_t;
  floor: Pfloormove_t;
  stairsize: fixed_t;
  speed: fixed_t;
  value: integer;
  Igno: integer;
  Dirn: integer;
  Step: integer;
  Sped: integer;
  Trig: integer;
label
  manual_stair;
begin
  value := word(line.special) - CGENSTAIRSBASE;

  // parse the bit fields in the line's special type
  Igno := (value and StairIgnore) shr StairIgnoreShift;
  Dirn := (value and StairDirection) shr StairDirectionShift;
  Step := (value and StairStep) shr StairStepShift;
  Sped := (value and StairSpeed) shr StairSpeedShift;
  Trig := (value and TriggerType) shr TriggerTypeShift;

  result := 0;

  // check if a manual trigger, if so do just the sector on the backside
  manual := false;
  if (Trig = Ord(PushOnce)) or (Trig = Ord(PushMany)) then
  begin
    sec := line.backsector;
    if sec = nil then
      exit;
    secnum := pDiff(sec, sectors, SizeOf(sector_t));
    manual := true;
    goto manual_stair;
  end;

  secnum := -1;
  // if not manual do all sectors tagged the same as the line
  while P_FindSectorFromLineTag2(line, secnum) >= 0 do
  begin
    sec := @sectors[secnum];

manual_stair:
    //Do not start another function if floor already moving
    //jff 2/26/98 add special lockout condition to wait for entire
    //staircase to build before retriggering
    if P_SectorActive(floor_special, sec) or (sec.stairlock <> 0) then
    begin
      if not manual then
        continue
      else
        exit;
    end;

    // new floor thinker
    result := 1;
    floor := Z_Malloc(SizeOf(floormove_t), PU_LEVSPEC, nil);
    P_AddThinker(@floor.thinker);
    sec.floordata := floor;
    floor.thinker._function.acp1 := @T_MoveFloor;
    if Dirn <> 0 then
      floor.direction := 1
    else
      floor.direction := -1;
    floor.sector := sec;

    // setup speed of stair building
    case Sped of
      Ord(SpeedNormal): floor.speed := FLOORSPEED div 2;
      Ord(SpeedFast): floor.speed := FLOORSPEED * 2;
      Ord(SpeedTurbo): floor.speed := FLOORSPEED * 4;
    else
      floor.speed := FLOORSPEED div 4;
    end;

    // setup stepsize for stairs
    case Step of
      1: stairsize := 8 * FRACUNIT;
      2: stairsize := 16 * FRACUNIT;
      3: stairsize := 24 * FRACUNIT;
    else
      stairsize := 4 * FRACUNIT;
    end;

    speed := floor.speed;
    height := sec.floorheight + floor.direction * stairsize;
    floor.floordestheight := height;
    texture := sec.floorpic;
    floor.crush := false;
    floor._type := genBuildStair; // jff 3/31/98 do not leave uninited

    sec.stairlock := -2;         // jff 2/26/98 set up lock on current sector
    sec.nextsec := -1;
    sec.prevsec := -1;

    osecnum := secnum;            //jff 3/4/98 preserve loop index
    // Find next sector to raise
    // 1.     Find 2-sided line with same sector side[0]
    // 2.     Other side is the next sector to raise
    repeat
      ok := 0;
      for i := 0 to sec.linecount - 1 do
      begin
        if sec.lines[i].backsector = nil then
          continue;

        tsec := sec.lines[i].frontsector;
        newsecnum := pDiff(tsec, sectors, SizeOf(sector_t));

        if secnum <> newsecnum then
          continue;

        tsec := sec.lines[i].backsector;
        newsecnum := pDiff(tsec, sectors, SizeOf(sector_t));

        if (Igno = 0) and (tsec.floorpic <> texture) then
          continue;

        //jff 2/26/98 special lockout condition for retriggering
        if P_SectorActive(floor_special, tsec) or (tsec.stairlock <> 0) then
          continue;

        height := height + floor.direction * stairsize;

        // jff 2/26/98
        // link the stair chain in both directions
        // lock the stair sector until building complete
        sec.nextsec := newsecnum; // link step to next
        tsec.prevsec := secnum;   // link next back
        tsec.nextsec := -1;       // set next forward link as end
        tsec.stairlock := -2;     // lock the step

        sec := tsec;
        secnum := newsecnum;
        floor := Z_Malloc(SizeOf(floormove_t), PU_LEVSPEC, nil);

        P_AddThinker(@floor.thinker);

        sec.floordata := floor;
        floor.thinker._function.acp1 := @T_MoveFloor;
        if Dirn <> 0 then
          floor.direction := 1
        else
          floor.direction := -1;
        floor.sector := sec;
        floor.speed := speed;
        floor.floordestheight := height;
        floor.crush := false;
        floor._type := genBuildStair; // jff 3/31/98 do not leave uninited

        ok := 1;
        break;
      end;
    until ok = 0;
    if manual then
      exit;
    secnum := osecnum; //jff 3/4/98 restore old loop index
  end;
  // retriggerable generalized stairs build up or down alternately
  if result <> 0 then
    line.special := line.special xor StairDirection; // alternate dir on succ activations
end;

//==============================================================================
//
// EV_DoGenCrusher
//
// Handle generalized crusher types
//
// Passed the linedef activating the crusher
// Returns true if a thinker created
//
//==============================================================================
function EV_DoGenCrusher(line: Pline_t): integer;
var
  secnum: integer;
  manual: boolean;
  sec: Psector_t;
  ceiling: Pceiling_t;
  value: integer;
  Slnt: integer;
  Sped: integer;
  Trig: integer;
label
  manual_crusher;
begin
  value := word(line.special) - CGENCRUSHERBASE;

  // parse the bit fields in the line's special type
  Slnt := (value and CrusherSilent) shr CrusherSilentShift;
  Sped := (value and CrusherSpeed) shr CrusherSpeedShift;
  Trig := (value and TriggerType) shr TriggerTypeShift;

  //jff 2/22/98  Reactivate in-stasis ceilings...for certain types.
  //jff 4/5/98 return if activated
  result := P_ActivateInStasisCeiling(line);

  // check if a manual trigger, if so do just the sector on the backside
  manual := false;
  if (Trig = Ord(PushOnce)) or (Trig = Ord(PushMany)) then
  begin
    sec := line.backsector;
    if sec = nil then
      exit;
    secnum := pDiff(sec, sectors, SizeOf(sector_t));
    manual := true;
    goto manual_crusher;
  end;

  secnum := -1;
  // if not manual do all sectors tagged the same as the line
  while P_FindSectorFromLineTag2(line, secnum) >= 0 do
  begin
    sec := @sectors[secnum];

manual_crusher:
    // Do not start another function if ceiling already moving
    if P_SectorActive(ceiling_special, sec) then //jff 2/22/98
    begin
      if not manual then
        continue
      else
        exit;
    end;

    // new ceiling thinker
    result := 1;
    ceiling := Z_Malloc(SizeOf(ceiling_t), PU_LEVSPEC, nil);
    P_AddThinker (@ceiling.thinker);
    sec.ceilingdata := ceiling; //jff 2/22/98
    ceiling.thinker._function.acp1 := @T_MoveCeiling;
    ceiling.crush := true;
    ceiling.direction := -1;
    ceiling.sector := sec;
    ceiling.texture := sec.ceilingpic;
    ceiling.newspecial := sec.special;
    ceiling.tag := sec.tag;
    if Slnt <> 0 then
      ceiling._type := genSilentCrusher
    else
      ceiling._type := genCrusher;
    ceiling.topheight := sec.ceilingheight;
    ceiling.bottomheight := sec.floorheight + 8 * FRACUNIT;

    // setup ceiling motion speed
    case Sped of
      Ord(SpeedSlow): ceiling.speed := CEILSPEED;
      Ord(SpeedNormal): ceiling.speed := CEILSPEED * 2;
      Ord(SpeedFast): ceiling.speed := CEILSPEED * 4;
      Ord(SpeedTurbo): ceiling.speed := CEILSPEED * 8;
    end;
    ceiling.oldspeed := ceiling.speed;

    P_AddActiveCeiling(ceiling);  // add to list of active ceilings
    if manual then
      exit;
  end;
end;

//==============================================================================
//
// EV_DoGenLockedDoor
//
// Handle generalized locked door types
//
// Passed the linedef activating the generalized locked door
// Returns true if a thinker created
//
//==============================================================================
function EV_DoGenLockedDoor(line: Pline_t): integer;
var
  secnum: integer;
  sec: Psector_t;
  door: Pvldoor_t;
  manual: boolean;
  value: integer;
  // parse the bit fields in the line's special type
  Kind: integer;
  Sped: integer;
  Trig: integer;
label
  manual_locked;
begin
  value := word(line.special) - CGENLOCKEDBASE;

  // parse the bit fields in the line's special type
  Kind := (value and LockedKind) shr LockedKindShift;
  Sped := (value and LockedSpeed) shr LockedSpeedShift;
  Trig := (value and TriggerType) shr TriggerTypeShift;

  result := 0;

  // check if a manual trigger, if so do just the sector on the backside
  manual := false;
  if (Trig = Ord(PushOnce)) or (Trig = Ord(PushMany)) then
  begin
    sec := line.backsector;
    if sec = nil then
      exit;
    secnum := pDiff(sec, sectors, SizeOf(sector_t));
    manual := true;
    goto manual_locked;
  end;

  secnum := -1;
  result := 0;

  // if not manual do all sectors tagged the same as the line
  while P_FindSectorFromLineTag2(line, secnum) >= 0 do
  begin
    sec := @sectors[secnum];

manual_locked:
    // Do not start another function if ceiling already moving
    if P_SectorActive(ceiling_special, sec) then //jff 2/22/98
    begin
      if not manual then
        continue
      else
        exit;
    end;

    // new door thinker
    result := 1;
    door := Z_Malloc(SizeOf(vldoor_t), PU_LEVSPEC, nil);
    P_AddThinker (@door.thinker);
    sec.ceilingdata := door; //jff 2/22/98
    door.thinker._function.acp1 := @T_VerticalDoor;
    door.sector := sec;
    door.topwait := VDOORWAIT;
    door.line := line;
    door.topheight := P_FindLowestCeilingSurrounding(sec) - 4 * FRACUNIT;
    door.direction := 1;

    // setup speed of door motion
    case Sped of
      Ord(SpeedNormal):
        begin
          if Kind <> 0 then
            door._type := vld_genOpen
          else
            door._type := vld_genRaise;
          door.speed := VDOORSPEED * 2;
        end;
      Ord(SpeedFast):
        begin
          if Kind <> 0 then
            door._type := vld_genBlazeOpen
          else
            door._type := vld_genBlazeRaise;
          door.speed := VDOORSPEED * 4;
        end;
      Ord(SpeedTurbo):
        begin
          if Kind <> 0 then
            door._type := vld_genBlazeOpen
          else
            door._type := vld_genBlazeRaise;
          door.speed := VDOORSPEED * 8;
        end;
    else
      begin
        if Kind <> 0 then
          door._type := vld_genOpen
        else
          door._type := vld_genRaise;
        door.speed := VDOORSPEED;
      end;
    end;

    S_StartSound(@door.sector.soundorg, Ord(sfx_bdopn));

    if manual then
      exit;
  end;
end;

//==============================================================================
//
// EV_DoGenDoor
//
// Handle generalized door types
//
// Passed the linedef activating the generalized door
// Returns true if a thinker created
//
//==============================================================================
function EV_DoGenDoor(line: Pline_t): integer;
var
  secnum: integer;
  sec: Psector_t;
  manual: boolean;
  door: Pvldoor_t;
  value: integer;
  Dely: integer;
  Kind: integer;
  Sped: integer;
  Trig: integer;
label
  manual_door;
begin
  value := word(line.special) - CGENDOORBASE;

  // parse the bit fields in the line's special type
  Dely := (value and DoorDelay) shr DoorDelayShift;
  Kind := (value and DoorKind) shr DoorKindShift;
  Sped := (value and DoorSpeed) shr DoorSpeedShift;
  Trig := (value and TriggerType) shr TriggerTypeShift;

  result := 0;

  // check if a manual trigger, if so do just the sector on the backside
  manual := false;
  if (Trig = Ord(PushOnce)) or (Trig = Ord(PushMany)) then
  begin
    sec := line.backsector;
    if sec = nil then
      exit;
    secnum := pDiff(sec, sectors, SizeOf(sector_t));
    manual := true;
    goto manual_door;
  end;

  secnum := -1;

  // if not manual do all sectors tagged the same as the line
  while P_FindSectorFromLineTag2(line, secnum) >= 0 do
  begin
    sec := @sectors[secnum];

manual_door:
    // Do not start another function if ceiling already moving
    if P_SectorActive(ceiling_special, sec) then //jff 2/22/98
    begin
      if not manual then
        continue
      else
        exit;
    end;

    // new door thinker
    result := 1;
    door := Z_Malloc(SizeOf(vldoor_t), PU_LEVSPEC, nil);
    P_AddThinker(@door.thinker);
    sec.ceilingdata := door; //jff 2/22/98

    door.thinker._function.acp1 := @T_VerticalDoor;
    door.sector := sec;
    // setup delay for door remaining open/closed
    case Dely of
      1: door.topwait := VDOORWAIT;
      2: door.topwait := 2 * VDOORWAIT;
      3: door.topwait := 7 * VDOORWAIT;
    else
      door.topwait := 35;
    end;

    // setup speed of door motion
    case Sped of
      Ord(SpeedNormal): door.speed := VDOORSPEED * 2;
      Ord(SpeedFast): door.speed := VDOORSPEED * 4;
      Ord(SpeedTurbo): door.speed := VDOORSPEED * 8;
    else
      door.speed := VDOORSPEED;
    end;
    door.line := line; // jff 1/31/98 remember line that triggered us

    // set kind of door, whether it opens then close, opens, closes etc.
    // assign target heights accordingly
    case Kind of

      Ord(OdCDoor):
        begin
          door.direction := 1;
          door.topheight := P_FindLowestCeilingSurrounding(sec) - 4 * FRACUNIT;
          if door.topheight <> sec.ceilingheight then
            S_StartSound(@door.sector.soundorg, Ord(sfx_bdopn));
          if Sped >= Ord(SpeedFast) then
            door._type := vld_genBlazeRaise
          else
            door._type := vld_genRaise;
        end;

      Ord(ODoor):
        begin
          door.direction := 1;
          door.topheight := P_FindLowestCeilingSurrounding(sec) - 4 * FRACUNIT;
          if door.topheight <> sec.ceilingheight then
            S_StartSound(@door.sector.soundorg, Ord(sfx_bdopn));
          if Sped >= Ord(SpeedFast) then
            door._type := vld_genBlazeOpen
          else
            door._type := vld_genOpen;
        end;

      ord(CdODoor):
        begin
          door.topheight := sec.ceilingheight;
          door.direction := -1;
          S_StartSound(@door.sector.soundorg, Ord(sfx_bdcls));
          if Sped >= Ord(SpeedFast) then
            door._type := vld_genBlazeCdO
          else
            door._type := vld_genCdO;
        end;

      Ord(CDoor):
        begin
          door.topheight := P_FindLowestCeilingSurrounding(sec) - 4 * FRACUNIT;
          door.direction := -1;
          S_StartSound(@door.sector.soundorg, Ord(sfx_bdcls));
          if Sped >= Ord(SpeedFast) then
            door._type := vld_genBlazeClose
          else
            door._type := vld_genClose;
        end;

    end;

    if manual then
      exit;
  end;
end;

end.

