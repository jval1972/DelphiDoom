//------------------------------------------------------------------------------
//
//  DelphiHexen: A modified and improved Hexen port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit po_man;

interface

uses
  d_delphi,
  d_think,
  m_fixed,
  r_defs,
  tables;

type
  podoortype_t = (
    PODOOR_NONE,
    PODOOR_SLIDE,
    PODOOR_SWING
  );


function PO_MovePolyobj(num: integer; x, y: integer): boolean;

function PO_RotatePolyobj(num: integer; angle: angle_t): boolean;

procedure PO_Init(lump: integer);

function PO_Busy(polyobj: integer): boolean;

function EV_RotatePoly(line: Pline_t; args: PByteArray; direction: integer;
  overriden: boolean): boolean;

function EV_MovePoly(line: Pline_t; args: PByteArray; timesEight: boolean;
  overriden: boolean): boolean;

function EV_OpenPolyDoor(line: Pline_t; args: PByteArray; _type: podoortype_t): boolean;


var
  PolyBlockMap: Ppolyblock_tPArray;
  po_NumPolyobjs: integer;

const
  PO_ANCHOR_TYPE = 3000;
  PO_SPAWN_TYPE = 3001;
  PO_SPAWNCRUSH_TYPE = 3002;

type
  polyevent_t = record
    thinker: thinker_t;
    polyobj: integer;
    speed: integer;
    dist: integer;
    angle: angle_t;
    xSpeed: fixed_t; // for sliding walls
    ySpeed: fixed_t;
  end;
  Ppolyevent_t = ^polyevent_t;


procedure T_RotatePoly(pe: Ppolyevent_t);

procedure T_MovePoly(pe: Ppolyevent_t);

type
  polydoor_t = record
    thinker: thinker_t;
    polyobj: integer;
    speed: integer;
    dist: integer;
    totalDist: integer;
    direction: integer;
    xSpeed, ySpeed: fixed_t;
    tics: integer;
    waitTics: integer;
    _type: podoortype_t;
    close: boolean;
  end;
  Ppolydoor_t = ^polydoor_t;


procedure T_PolyDoor(pd: Ppolydoor_t);


var
  polyobjs: Ppolyobj_tArray; // list of all poly-objects on the level

implementation

uses
  doomdata,
  i_system,
  m_bbox,
  p_mobj_h,
  p_tick,
  p_local,
  p_setup,
  p_acs,
  p_map,
  p_inter,
  p_maputl,
  r_main,
  s_sndseq,
  w_wad,
  z_zone;

const
  PO_LINE_START = 1; // polyobj line start special
  PO_LINE_EXPLICIT = 5;

// MACROS ------------------------------------------------------------------

const
  PO_MAXPOLYSEGS = 64;

// PRIVATE DATA DEFINITIONS ------------------------------------------------
var
  PolySegCount: integer;
  PolyStartX: fixed_t;
  PolyStartY: fixed_t;

// CODE --------------------------------------------------------------------

// ===== Higher Level Poly Interface code =====

//==========================================================================
//
// GetPolyobj
//
//==========================================================================

function GetPolyobj(polyNum: integer): Ppolyobj_t;
var
  i: integer;
begin
  for i := 0 to po_NumPolyobjs - 1 do
  begin
    if polyobjs[i].tag = polyNum then
    begin
      result := @polyobjs[i];
      exit;
    end;
  end;
  result := nil;
end;

//==========================================================================
//
// GetPolyobjMirror
//
//==========================================================================

function GetPolyobjMirror(poly: integer; mir: PInteger): integer;
var
  i: integer;
begin
  for i := 0 to po_NumPolyobjs - 1 do
  begin
    if polyobjs[i].tag = poly then
    begin
      result := polyobjs[i].segs^.linedef.arg2; // JVAL SOS
      mir^ := result;
      exit;
    end;
  end;
  result := 0;
  mir^ := 0;
end;

// ===== Polyobj Event Code =====

//==========================================================================
//
// T_RotatePoly
//
//==========================================================================

procedure T_RotatePoly(pe: Ppolyevent_t);
var
  absSpeed: integer;
  poly: Ppolyobj_t;
begin
  if PO_RotatePolyobj(pe.polyobj, pe.speed) then
  begin
    absSpeed := abs(pe.speed);

    if pe.dist = -1 then
    begin // perpetual polyobj
      exit;
    end;

    pe.dist := pe.dist - absSpeed;
    if pe.dist <= 0 then
    begin
      poly := GetPolyobj(pe.polyobj);
      if poly.specialdata = pe then
      begin
        poly.specialdata := nil;
      end;
      S_StopSequence(Pmobj_t(@poly.startSpot));
      P_PolyobjFinished(poly.tag);
      P_RemoveThinker(@pe.thinker);
    end;
    if pe.dist < absSpeed then
    begin
      if pe.speed < 0 then
        pe.speed := -pe.dist * pe.speed
      else
        pe.speed := pe.dist * pe.speed;
    end;
  end;
end;

//==========================================================================
//
// EV_RotatePoly
//
//==========================================================================

function EV_RotatePoly(line: Pline_t; args: PByteArray; direction: integer;
  overriden: boolean): boolean;
var
  mirror: integer;
  polyNum: integer;
  pe: Ppolyevent_t;
  poly: Ppolyobj_t;
begin
  polyNum := args[0];
  poly := GetPolyobj(polyNum);
  if poly <> nil then
  begin
    if (poly.specialdata <> nil) and not overriden then
    begin // poly is already moving
      result := false;
      exit;
    end;
  end
  else
    I_Error('EV_RotatePoly(): Invalid polyobj num: %d', [polyNum]);

  pe := Z_Malloc(SizeOf(polyevent_t), PU_LEVSPEC, nil);
  P_AddThinker(@pe.thinker);
  pe.thinker._function.acp1 := @T_RotatePoly;
  pe.polyobj := polyNum;
  if args[2] <> 0 then
  begin
    if args[2] = 255 then
      pe.dist := -1
    else
      pe.dist := args[2] * (ANG90 div 64); // Angle
  end
  else
    pe.dist := -2;
  pe.speed := _SHR(args[1] * direction * (ANG90 div 64), 3);
  poly.specialdata := pe;
  S_StartSequence(Pmobj_t(@poly.startSpot), Ord(SEQ_DOOR_STONE) + poly.seqType);

  while GetPolyobjMirror(polyNum, @mirror) <> 0 do
  begin
    poly := GetPolyobj(mirror);
    if (poly <> nil) and (poly.specialdata <> nil) and not overriden then
    begin // mirroring poly is already in motion
      break;
    end;
    pe := Z_Malloc(SizeOf(polyevent_t), PU_LEVSPEC, nil);
    P_AddThinker(@pe.thinker);
    pe.thinker._function.acp1 := @T_RotatePoly;
    poly.specialdata := pe;
    pe.polyobj := mirror;
    if args[2] <> 0 then
    begin
      if args[2] = 255 then
      begin
        pe.dist := -1;
      end
      else
      begin
        pe.dist := args[2] * (ANG90 div 64); // Angle
      end;
    end
    else
    begin
      pe.dist := -2;
    end;
    poly := GetPolyobj(polyNum);
    if poly <> nil then
    begin
      poly.specialdata := pe;
    end
    else
    begin
      I_Error('EV_RotatePoly():  Invalid polyobj num: %d', [polyNum]);
    end;
    direction := -direction;
    pe.speed := _SHR(args[1] * direction * (ANG90 div 64), 3);
    polyNum := mirror;
    S_StartSequence(Pmobj_t(@poly.startSpot), Ord(SEQ_DOOR_STONE) + poly.seqType);
  end;
  result := true;
end;

//==========================================================================
//
// T_MovePoly
//
//==========================================================================

procedure T_MovePoly(pe: Ppolyevent_t);
var
  absSpeed: integer;
  poly: Ppolyobj_t;
begin
  if PO_MovePolyobj(pe.polyobj, pe.xSpeed, pe.ySpeed) then
  begin
    absSpeed := abs(pe.speed);
    pe.dist := pe.dist - absSpeed;
    if pe.dist <= 0 then
    begin
      poly := GetPolyobj(pe.polyobj);
      if poly.specialdata = pe then
      begin
        poly.specialdata := nil;
      end;
      S_StopSequence(Pmobj_t(@poly.startSpot));
      P_PolyobjFinished(poly.tag);
      P_RemoveThinker(@pe.thinker);
    end;
    if pe.dist < absSpeed then
    begin
      if pe.speed < 0 then
        pe.speed := -pe.dist * pe.speed
      else
        pe.speed := pe.dist * pe.speed;
      pe.xSpeed := FixedMul(pe.speed, finecosine[pe.angle]);
      pe.ySpeed := FixedMul(pe.speed, finesine[pe.angle]);
    end;
  end;
end;

//==========================================================================
//
// EV_MovePoly
//
//==========================================================================

function EV_MovePoly(line: Pline_t; args: PByteArray; timesEight: boolean;
  overriden: boolean): boolean;
var
  mirror: integer;
  polyNum: integer;
  pe: Ppolyevent_t;
  poly: Ppolyobj_t;
  an: angle_t;
begin
  polyNum := args[0];
  poly := GetPolyobj(polyNum);
  if poly <> nil then
  begin
    if (poly.specialdata <> nil) and not overriden then
    begin // poly is already moving
      result := false;
      exit;
    end;
  end
  else
    I_Error('EV_MovePoly(): Invalid polyobj num: %d', [polyNum]);
  pe := Z_Malloc(SizeOf(polyevent_t), PU_LEVSPEC, nil);
  P_AddThinker(@pe.thinker);
  pe.thinker._function.acp1 := @T_MovePoly;
  pe.polyobj := polyNum;
  if timesEight then
    pe.dist := args[3] * 8 * FRACUNIT
  else
    pe.dist := args[3] * FRACUNIT; // Distance
  pe.speed := args[1] * (FRACUNIT div 8);
  poly.specialdata := pe;

  an := args[2] * (ANG90 div 64);

  pe.angle := an shr ANGLETOFINESHIFT;
  pe.xSpeed := FixedMul(pe.speed, finecosine[pe.angle]);
  pe.ySpeed := FixedMul(pe.speed, finesine[pe.angle]);
  S_StartSequence(Pmobj_t(@poly.startSpot), Ord(SEQ_DOOR_STONE) + poly.seqType);

  while GetPolyobjMirror(polyNum, @mirror) <> 0 do
  begin
    poly := GetPolyobj(mirror);
    if (poly <> nil) and (poly.specialdata <> nil) and not overriden then
    begin // mirroring poly is already in motion
      break;
    end;
    pe := Z_Malloc(SizeOf(polyevent_t), PU_LEVSPEC, nil);
    P_AddThinker(@pe.thinker);
    pe.thinker._function.acp1 := @T_MovePoly;
    pe.polyobj := mirror;
    poly.specialdata := pe;
    if timesEight then
      pe.dist := args[3] * 8 * FRACUNIT
    else
      pe.dist := args[3] * FRACUNIT; // Distance
    pe.speed := args[1] * (FRACUNIT div 8);
    an := an + ANG180; // reverse the angle
    pe.angle := an shr ANGLETOFINESHIFT;
    pe.xSpeed := FixedMul(pe.speed, finecosine[pe.angle]);
    pe.ySpeed := FixedMul(pe.speed, finesine[pe.angle]);
    polyNum := mirror;
    S_StartSequence(Pmobj_t(@poly.startSpot), Ord(SEQ_DOOR_STONE) + poly.seqType);
  end;
  result := true;
end;

//==========================================================================
//
// T_PolyDoor
//
//==========================================================================

procedure T_PolyDoor(pd: Ppolydoor_t);
var
  absSpeed: integer;
  poly: Ppolyobj_t;
begin
  if pd.tics <> 0 then
  begin
    dec(pd.tics);
    if pd.tics = 0 then
    begin
      poly := GetPolyobj(pd.polyobj);
      S_StartSequence(Pmobj_t(@poly.startSpot), Ord(SEQ_DOOR_STONE) + poly.seqType);
    end;
    exit;
  end;

  case pd._type of
    PODOOR_SLIDE:
      begin
        if PO_MovePolyobj(pd.polyobj, pd.xSpeed, pd.ySpeed) then
        begin
          absSpeed := abs(pd.speed);
          pd.dist := pd.dist - absSpeed;
          if pd.dist <= 0 then
          begin
            poly := GetPolyobj(pd.polyobj);
            S_StopSequence(Pmobj_t(@poly.startSpot));
            if not pd.close then
            begin
              pd.dist := pd.totalDist;
              pd.close := true;
              pd.tics := pd.waitTics;
              pd.direction := (ANGLE_MAX shr ANGLETOFINESHIFT) - pd.direction;
              pd.xSpeed := -pd.xSpeed;
              pd.ySpeed := -pd.ySpeed;
            end
            else
            begin
              if poly.specialdata = pd then
                poly.specialdata := nil;
              P_PolyobjFinished(poly.tag);
              P_RemoveThinker(@pd.thinker);
            end;
          end;
        end
        else
        begin
          poly := GetPolyobj(pd.polyobj);
          if poly.crush or not pd.close then
          begin // continue moving if the poly is a crusher, or is opening
            exit;
          end
          else
          begin // open back up
            pd.dist := pd.totalDist - pd.dist;
            pd.direction := (ANGLE_MAX shr ANGLETOFINESHIFT) - pd.direction;
            pd.xSpeed := -pd.xSpeed;
            pd.ySpeed := -pd.ySpeed;
            pd.close := false;
            S_StartSequence(Pmobj_t(@poly.startSpot), Ord(SEQ_DOOR_STONE) + poly.seqType);
          end;
        end;
      end;
    PODOOR_SWING:
      begin
        if PO_RotatePolyobj(pd.polyobj, pd.speed) then
        begin
          absSpeed := abs(pd.speed);
          if pd.dist = -1 then
          begin // perpetual polyobj
            exit;
          end;
          pd.dist := pd.dist - absSpeed;
          if pd.dist <= 0 then
          begin
            poly := GetPolyobj(pd.polyobj);
            S_StopSequence(Pmobj_t(@poly.startSpot));
            if not pd.close then
            begin
              pd.dist := pd.totalDist;
              pd.close := true;
              pd.tics := pd.waitTics;
              pd.speed := -pd.speed;
            end
            else
            begin
              if poly.specialdata = pd then
                poly.specialdata := nil;
              P_PolyobjFinished(poly.tag);
              P_RemoveThinker(@pd.thinker);
            end;
          end;
        end
        else
        begin
          poly := GetPolyobj(pd.polyobj);
          if poly.crush or not pd.close then
          begin // continue moving if the poly is a crusher, or is opening
            exit;
          end
          else
          begin // open back up and rewait
            pd.dist := pd.totalDist-pd.dist;
            pd.speed := -pd.speed;
            pd.close := false;
            S_StartSequence(Pmobj_t(@poly.startSpot), Ord(SEQ_DOOR_STONE) + poly.seqType);
          end;
        end;
      end;
  end;
end;

//==========================================================================
//
// EV_OpenPolyDoor
//
//==========================================================================

function EV_OpenPolyDoor(line: Pline_t; args: PByteArray; _type: podoortype_t): boolean;
var
  mirror: integer;
  polyNum: integer;
  pd: Ppolydoor_t;
  poly: Ppolyobj_t;
  an: angle_t;
begin
  polyNum := args[0];
  poly := GetPolyobj(polyNum);
  if poly <> nil then
  begin
    if poly.specialdata <> nil then
    begin // poly is already moving
      result := false;
      exit;
    end;
  end
  else
    I_Error('EV_OpenPolyDoor(): Invalid polyobj num: %d', [polyNum]);

  pd := Z_Malloc(SizeOf(polydoor_t), PU_LEVSPEC, nil);
  memset(pd, 0, SizeOf(polydoor_t));
  P_AddThinker(@pd.thinker);
  pd.thinker._function.acp1 := @T_PolyDoor;
  pd._type := _type;
  pd.polyobj := polyNum;
  an := 0; // JVAL: Avoid compiler warning
  if _type = PODOOR_SLIDE then
  begin
    pd.waitTics := args[4];
    pd.speed := args[1] * (FRACUNIT div 8);
    pd.totalDist := args[3] * FRACUNIT; // Distance
    pd.dist := pd.totalDist;
    an := args[2] * (ANG90 div 64);
    pd.direction := an shr ANGLETOFINESHIFT;
    pd.xSpeed := FixedMul(pd.speed, finecosine[pd.direction]);
    pd.ySpeed := FixedMul(pd.speed, finesine[pd.direction]);
    S_StartSequence(Pmobj_t(@poly.startSpot), Ord(SEQ_DOOR_STONE) + poly.seqType);
  end
  else if _type = PODOOR_SWING then
  begin
    pd.waitTics := args[3];
    pd.direction := 1; // ADD:  PODOOR_SWINGL, PODOOR_SWINGR
    pd.speed := _SHR3(args[1] * pd.direction * (ANG90 div 64));
    pd.totalDist := args[2] * (ANG90 div 64);
    pd.dist := pd.totalDist;
    S_StartSequence(Pmobj_t(@poly.startSpot), Ord(SEQ_DOOR_STONE) + poly.seqType);
  end;

  poly.specialdata := pd;

  while GetPolyobjMirror(polyNum, @mirror) <> 0 do
  begin
    poly := GetPolyobj(mirror);
    if (poly <> nil) and (poly.specialdata <> nil) then
    begin // mirroring poly is already in motion
      break;
    end;
    pd := Z_Malloc(SizeOf(polydoor_t), PU_LEVSPEC, nil);
    memset(pd, 0, SizeOf(polydoor_t));
    P_AddThinker(@pd.thinker);
    pd.thinker._function.acp1 := @T_PolyDoor;
    pd.polyobj := mirror;
    pd._type := _type;
    poly.specialdata := pd;
    if _type = PODOOR_SLIDE then
    begin
      pd.waitTics := args[4];
      pd.speed := args[1] * (FRACUNIT div 8);
      pd.totalDist := args[3] * FRACUNIT; // Distance
      pd.dist := pd.totalDist;
      an := an + ANG180; // reverse the angle
      pd.direction := an shr ANGLETOFINESHIFT;
      pd.xSpeed := FixedMul(pd.speed, finecosine[pd.direction]);
      pd.ySpeed := FixedMul(pd.speed, finesine[pd.direction]);
      S_StartSequence(Pmobj_t(@poly.startSpot), Ord(SEQ_DOOR_STONE) + poly.seqType);
    end
    else if _type = PODOOR_SWING then
    begin
      pd.waitTics := args[3];
      pd.direction := -1; // ADD:  same as above
      pd.speed := _SHR(args[1] * pd.direction * (ANG90 div 64), 3);
      pd.totalDist := args[2] * (ANG90 div 64);
      pd.dist := pd.totalDist;
      S_StartSequence(Pmobj_t(@poly.startSpot), Ord(SEQ_DOOR_STONE) + poly.seqType);
    end;
    polyNum := mirror;
  end;
  result := true;
end;

//==========================================================================
//
// ThrustMobj
//
//==========================================================================

procedure ThrustMobj(mobj: Pmobj_t; seg: Pseg_t; po: Ppolyobj_t);
var
  thrustAngle: integer;
  thrustX: integer;
  thrustY: integer;
  pe: Ppolyevent_t;
  force: integer;
begin
  if (mobj.flags and MF_SHOOTABLE = 0) and (mobj.player = nil) then
    exit;

  thrustAngle := (seg.angle - ANG90) shr ANGLETOFINESHIFT;

  pe := po.specialdata;
  if pe <> nil then
  begin
    if @pe.thinker._function.acp1 = @T_RotatePoly then
      force := _SHR(pe.speed, 8)
    else
      force := _SHR(pe.speed, 3);
    if force < FRACUNIT then
      force := FRACUNIT
    else if force > 4 * FRACUNIT then
      force := 4 * FRACUNIT;
  end
  else
    force := FRACUNIT;

  thrustX := FixedMul(force, finecosine[thrustAngle]);
  thrustY := FixedMul(force, finesine[thrustAngle]);
  mobj.momx := mobj.momx + thrustX;
  mobj.momy := mobj.momy + thrustY;
  if po.crush then
  begin
    if not P_CheckPosition(mobj, mobj.x + thrustX, mobj.y + thrustY) then
      P_DamageMobj(mobj, nil, nil, 3);
  end;
end;

//==========================================================================
//
// UpdateSegBBox
//
//==========================================================================

procedure UpdateSegBBox(seg: Pseg_t);
var
  line: Pline_t;
begin
  line := seg.linedef;

  if seg.v1.x < seg.v2.x then
  begin
    line.bbox[BOXLEFT] := seg.v1.x;
    line.bbox[BOXRIGHT] := seg.v2.x;
  end
  else
  begin
    line.bbox[BOXLEFT] := seg.v2.x;
    line.bbox[BOXRIGHT] := seg.v1.x;
  end;
  if seg.v1.y < seg.v2.y then
  begin
    line.bbox[BOXBOTTOM] := seg.v1.y;
    line.bbox[BOXTOP] := seg.v2.y;
  end
  else
  begin
    line.bbox[BOXBOTTOM] := seg.v2.y;
    line.bbox[BOXTOP] := seg.v1.y;
  end;

  // Update the line's slopetype
  line.dx := line.v2.x - line.v1.x;
  line.dy := line.v2.y - line.v1.y;
  if line.dx = 0 then
    line.slopetype := ST_VERTICAL
  else if line.dy = 0 then
    line.slopetype := ST_HORIZONTAL
  else
  begin
    if FixedDiv(line.dy, line.dx) > 0 then
      line.slopetype := ST_POSITIVE
    else
      line.slopetype := ST_NEGATIVE;
  end;
end;

//==========================================================================
//
// UnLinkPolyobj
//
//==========================================================================

procedure UnLinkPolyobj(po: Ppolyobj_t);
var
  link: Ppolyblock_t;
  i, j: integer;
  index: integer;
begin
  // remove the polyobj from each blockmap section
  j := po.bbox[BOXBOTTOM];
  while j <= po.bbox[BOXTOP] do
  begin
    index := j * bmapwidth;
    i := po.bbox[BOXLEFT];
    while i <= po.bbox[BOXRIGHT] do
    begin
      if (i >= 0) and (i < bmapwidth) and (j >= 0) and (j < bmapheight) then
      begin
        link := PolyBlockMap[index + i];
        while (link <> nil) and (link.polyobj <> po) do
          link := link.next;
        if link = nil then
        begin // polyobj not located in the link cell
          inc(i);
          continue;
        end;
        link.polyobj := nil;
      end;
      inc(i);
    end;
    inc(j);
  end;
end;

//==========================================================================
//
// LinkPolyobj
//
//==========================================================================

procedure LinkPolyobj(po: Ppolyobj_t);
var
  leftX, rightX: integer;
  topY, bottomY: integer;
  tempSeg: PPseg_t;
  link: PPpolyblock_t;
  tempLink: Ppolyblock_t;
  i, j: integer;
begin
  // calculate the polyobj bbox
  tempSeg := po.segs;
  rightX := tempSeg^.v1.x;
  leftX := rightX;
  topY := tempSeg^.v1.y;
  bottomY := topY;

  for i := 0 to po.numsegs - 1 do
  begin
    if not tempSeg^.miniseg then
    begin
      if tempSeg^.v1.x > rightX then
        rightX := tempSeg^.v1.x;
      if tempSeg^.v1.x < leftX then
        leftX := tempSeg^.v1.x;
      if tempSeg^.v1.y > topY then
        topY := tempSeg^.v1.y;
      if tempSeg^.v1.y < bottomY then
        bottomY := tempSeg^.v1.y;
    end;
    inc(tempSeg);
  end;
  if internalblockmapformat then
  begin
    po.bbox[BOXRIGHT] := MapBlockIntX(int64(rightX) - int64(bmaporgx));
    po.bbox[BOXLEFT] := MapBlockIntX(int64(leftX) - int64(bmaporgx));
    po.bbox[BOXTOP] := MapBlockIntY(int64(topY) - int64(bmaporgy));
    po.bbox[BOXBOTTOM] := MapBlockIntY(int64(bottomY) - int64(bmaporgy));
  end
  else
  begin
    po.bbox[BOXRIGHT] := MapBlockInt(rightX - bmaporgx);
    po.bbox[BOXLEFT] := MapBlockInt(leftX - bmaporgx);
    po.bbox[BOXTOP] := MapBlockInt(topY - bmaporgy);
    po.bbox[BOXBOTTOM] := MapBlockInt(bottomY - bmaporgy);
  end;
  // add the polyobj to each blockmap section
  j := po.bbox[BOXBOTTOM] * bmapwidth;
  while j <= po.bbox[BOXTOP] * bmapwidth do
  begin
    i := po.bbox[BOXLEFT];
    while i <= po.bbox[BOXRIGHT] do
    begin
      if (i >= 0) and (i < bmapwidth) and (j >= 0) and (j < bmapheight * bmapwidth) then
      begin
        link := @PolyBlockMap[j + i];
        if link^ = nil then
        begin // Create a new link at the current block cell
          link^ := Z_Malloc(SizeOf(polyblock_t), PU_LEVEL, nil);
          link^.next := nil;
          link^.prev := nil;
          link^.polyobj := po;
        end
        else
        begin
          tempLink := link^;
          while (tempLink.next <> nil) and (tempLink.polyobj <> nil) do
            tempLink := tempLink.next;

          if tempLink.polyobj = nil then
          begin
            tempLink.polyobj := po;
          end
          else
          begin
            tempLink.next := Z_Malloc(SizeOf(polyblock_t), PU_LEVEL, nil);
            tempLink.next.next := nil;
            tempLink.next.prev := tempLink;
            tempLink.next.polyobj := po;
          end;
        end;
      end;
      inc(i);
      // else, don't link the polyobj, since it's off the map
    end;
    j := j + bmapwidth;
  end;
end;


//==========================================================================
//
// PO_CheckMobjBlocking
//
//==========================================================================

function PO_CheckMobjBlocking(seg: Pseg_t; po: Ppolyobj_t): boolean;
var
  mobj: Pmobj_t;
  i, j, k: integer;
  left, right, top, bottom: integer;
  tmbbox: array[0..3] of fixed_t;
  ld: Pline_t;
  blocked: boolean;
  link: Pblocklinkitem_t;
  r: fixed_t;
begin

  ld := seg.linedef;

  if internalblockmapformat then
  begin
    top := MapBlockIntY(int64(ld.bbox[BOXTOP]) - int64(bmaporgy) + MAXRADIUS);
    bottom := MapBlockIntY(int64(ld.bbox[BOXBOTTOM]) - int64(bmaporgy) - MAXRADIUS);
    left := MapBlockIntX(int64(ld.bbox[BOXLEFT]) - int64(bmaporgx) - MAXRADIUS);
    right := MapBlockIntX(int64(ld.bbox[BOXRIGHT]) - int64(bmaporgx) + MAXRADIUS);
  end
  else
  begin
    top := MapBlockInt(ld.bbox[BOXTOP] - bmaporgy + MAXRADIUS);
    bottom := MapBlockInt(ld.bbox[BOXBOTTOM] - bmaporgy - MAXRADIUS);
    left := MapBlockInt(ld.bbox[BOXLEFT] - bmaporgx - MAXRADIUS);
    right := MapBlockInt(ld.bbox[BOXRIGHT] - bmaporgx + MAXRADIUS);
  end;

  blocked := false;

  if bottom < 0 then
    bottom := 0;
  if bottom >= bmapheight then
    bottom := bmapheight - 1;
  if top < 0 then
    top := 0;
  if top >= bmapheight then
    top := bmapheight- 1;
  if left < 0 then
    left := 0;
  if left >= bmapwidth then
    left := bmapwidth - 1;
  if right < 0 then
    right := 0;
  if right >= bmapwidth then
    right := bmapwidth - 1;

  j := bottom * bmapwidth;
  while j <= top * bmapwidth do
  begin
    i := left;
    while i <= right do
    begin
      link := @blocklinks[j + i];
      for k := 0 to link.size - 1 do
      begin
        mobj := link.links[k];
        if (mobj.flags and MF_SOLID <> 0) or (mobj.player <> nil) then
        begin
          r := mobj.radius;
          tmbbox[BOXTOP] := mobj.y + r;
          tmbbox[BOXBOTTOM] := mobj.y - r;
          tmbbox[BOXLEFT] := mobj.x - r;
          tmbbox[BOXRIGHT] := mobj.x + r;

          if (tmbbox[BOXRIGHT] <= ld.bbox[BOXLEFT]) or
             (tmbbox[BOXLEFT] >= ld.bbox[BOXRIGHT]) or
             (tmbbox[BOXTOP] <= ld.bbox[BOXBOTTOM]) or
             (tmbbox[BOXBOTTOM] >= ld.bbox[BOXTOP]) then
          begin
            continue;
          end;
          if P_BoxOnLineSide(@tmbbox, ld) <> -1 then
          begin
            continue;
          end;
          ThrustMobj(mobj, seg, po);
          blocked := true;
        end;
      end;
      inc(i);
    end;
    j := j + bmapwidth;
  end;
  result := blocked;
end;

//==========================================================================
//
// PO_MovePolyobj
//
//==========================================================================

function PO_MovePolyobj(num: integer; x, y: integer): boolean;
var
  count: integer;
  segList: PPseg_t;    // JVAL SOS
  veryTempSeg: PPseg_t;// JVAL SOS
  po: Ppolyobj_t;
  prevPts: Pvertex_t;
  blocked: boolean;
  l: Pline_t;
begin
  po := GetPolyobj(num);
  if po = nil then
    I_Error('PO_MovePolyobj(): Invalid polyobj number: %d', [num]);

  UnLinkPolyobj(po);

  segList := po.segs;
  prevPts := @po.prevPts[0];
  blocked := false;

  inc(validcount);
  count := po.numsegs;
  while count > 0 do
  begin
    if not segList^.miniseg then
    begin
      l := segList^.linedef;
      if l.validcount <> validcount then
      begin
        l.bbox[BOXTOP] := l.bbox[BOXTOP] + y;
        l.bbox[BOXBOTTOM] := l.bbox[BOXBOTTOM] + y;
        l.bbox[BOXLEFT] := l.bbox[BOXLEFT] + x;
        l.bbox[BOXRIGHT] := l.bbox[BOXRIGHT] + x;
        l.validcount := validcount;
      end;
      veryTempSeg := po.segs;
      while veryTempSeg <> segList do
      begin
        if veryTempSeg^.v1 = segList^.v1 then
          break;
        inc(veryTempSeg);
      end;
      if veryTempSeg = segList then
      begin
        segList^.v1.x := segList^.v1.x + x;
        segList^.v1.y := segList^.v1.y + y;
      end;
      prevPts^.x := prevPts^.x + x; // previous points are unique for each seg
      prevPts^.y := prevPts^.y + y;
    end;
    inc(segList);
    inc(prevPts);
    dec(count);
  end;
  segList := po.segs;
  count := po.numsegs;
  while count > 0 do
  begin
    if not segList^.miniseg then
    begin
      if PO_CheckMobjBlocking(segList^, po) then
      begin
        blocked := true;
      end;
    end;
    inc(segList);
    dec(count);
  end;
  if blocked then
  begin
    count := po.numsegs;
    segList := po.segs;
    prevPts := @po.prevPts[0];
    inc(validcount);
    while count > 0 do
    begin
      if not segList^.miniseg then
      begin
        l := segList^.linedef;
        if l.validcount <> validcount then
        begin
          l.bbox[BOXTOP] := l.bbox[BOXTOP] - y;
          l.bbox[BOXBOTTOM] := l.bbox[BOXBOTTOM] - y;
          l.bbox[BOXLEFT] := l.bbox[BOXLEFT] - x;
          l.bbox[BOXRIGHT] := l.bbox[BOXRIGHT] - x;
          l.validcount := validcount;
        end;
        veryTempSeg := po.segs;
        while veryTempSeg <> segList do
        begin
          if not veryTempSeg^.miniseg then
            if veryTempSeg^.v1 = segList^.v1 then
              break;
          inc(veryTempSeg);
        end;
        if veryTempSeg = segList then
        begin
          segList^.v1.x := segList^.v1.x - x;
          segList^.v1.y := segList^.v1.y - y;
        end;
        prevPts^.x := prevPts^.x - x;
        prevPts^.y := prevPts^.y - y;
      end;
      inc(segList);
      inc(prevPts);
      dec(count);
    end;
    LinkPolyobj(po);
    result := false;
    exit;
  end;
  po.startSpot.x := po.startSpot.x + x;
  po.startSpot.y := po.startSpot.y + y;
  LinkPolyobj(po);
  result := true;
end;

//==========================================================================
//
// RotatePt
//
//==========================================================================

procedure RotatePt(an: angle_t; x, y: Pfixed_t; startSpotX, startSpotY: fixed_t);
var
  tr_x, tr_y: fixed_t;
  gxt, gyt: fixed_t;
begin
  tr_x := x^;
  tr_y := y^;

  gxt := FixedMul(tr_x, finecosine[an]);
  gyt := FixedMul(tr_y, finesine[an]);
  x^ := (gxt - gyt) + startSpotX;

  gxt := FixedMul(tr_x, finesine[an]);
  gyt := FixedMul(tr_y, finecosine[an]);
  y^ := (gyt + gxt) + startSpotY;
end;

//==========================================================================
//
// PO_RotatePolyobj
//
//==========================================================================

function PO_RotatePolyobj(num: integer; angle: angle_t): boolean;
var
  count: integer;
  segList: PPseg_t;
  originalPts: Pvertex_t;
  prevPts: Pvertex_t;
  an: angle_t;
  po: Ppolyobj_t;
  blocked: boolean;
begin
  po := GetPolyobj(num);
  if po = nil then
    I_Error('PO_RotatePolyobj(): Invalid polyobj number: %d', [num]);

  an := (po.angle + angle) shr ANGLETOFINESHIFT;

  UnLinkPolyobj(po);

  segList := po.segs;
  originalPts := @po.originalPts[0];
  prevPts := @po.prevPts[0];

  count := po.numsegs;
  while count > 0 do
  begin
    if not segList^.miniseg then
    begin
      prevPts.x := segList^.v1.x;
      prevPts.y := segList^.v1.y;
      segList^.v1.x := originalPts.x;
      segList^.v1.y := originalPts.y;
      RotatePt(an, @segList^.v1.x, @segList^.v1.y, po.startSpot.x, po.startSpot.y);
    end;
    inc(segList);
    inc(originalPts);
    inc(prevPts);
    dec(count);
  end;

  segList := po.segs;
  blocked := false;
  inc(validcount);

  count := po.numsegs;
  while count > 0 do
  begin
    if not segList^.miniseg then
    begin
      if PO_CheckMobjBlocking(segList^, po) then
        blocked := true;

      if segList^.linedef.validcount <> validcount then
      begin
        UpdateSegBBox(segList^);
        segList^.linedef.validcount := validcount;
      end;
      segList^.angle := segList^.angle + angle;
    end;
    inc(segList);
    dec(count);
  end;

  if blocked then
  begin
    segList := po.segs;
    prevPts := @po.prevPts[0];
    count := po.numsegs;
    while count > 0 do
    begin
      if not segList^.miniseg then
      begin
        segList^.v1.x := prevPts.x;
        segList^.v1.y := prevPts.y;
      end;
      inc(segList);
      inc(prevPts);
      dec(count);
    end;
    segList := po.segs;
    inc(validcount);
    count := po.numsegs;
    while count > 0 do
    begin
      if not segList^.miniseg then
      begin
        if segList^.linedef.validcount <> validcount then
        begin
          UpdateSegBBox(segList^);
          segList^.linedef.validcount := validcount;
        end;
        segList^.angle := segList^.angle - angle;
      end;
      inc(segList);
      dec(count);
    end;
    LinkPolyobj(po);
    result := false;
    exit;
  end;

  po.angle := po.angle + angle;
  LinkPolyobj(po);
  result := true;
end;

//==========================================================================
//
// InitBlockMap
//
//==========================================================================

procedure InitBlockMap;
var
  i, j: integer;
  segList: PPseg_t;
  leftX, rightX: integer;
  topY, bottomY: integer;
  v1: Pvertex_t;
begin
  PolyBlockMap := Z_Malloc(bmapwidth * bmapheight * SizeOf(Ppolyblock_t), PU_LEVEL, nil);
  memset(PolyBlockMap, 0, bmapwidth * bmapheight * SizeOf(Ppolyblock_t));

  for i := 0 to po_NumPolyobjs - 1 do
  begin
    LinkPolyobj(@polyobjs[i]);

    // calculate a rough area
    // right now, working like shit...gotta fix this...
    segList := polyobjs[i].segs;
    leftX := segList^.v1.x;
    rightX := leftX;
    topY := segList^.v1.y;
    bottomY := topY;
    j := 0;
    while j < polyobjs[i].numsegs do
    begin
      if not segList^.miniseg then
      begin
        v1 := segList^.v1;
        if v1.x < leftX then
          leftX := v1.x;
        if v1.x > rightX then
          rightX := v1.x;
        if v1.y < bottomY then
          bottomY := v1.y;
        if v1.y > topY then
          topY := v1.y;
      end;
      inc(segList);
      inc(j);
    end;
  end;
end;

//==========================================================================
//
// IterFindPolySegs
//
//              Passing NULL for segList will cause IterFindPolySegs to
//      count the number of segs in the polyobj
//==========================================================================

procedure IterFindPolySegs(x, y: integer; segList: PPseg_t);
var
  i: integer;
begin
  if (x = PolyStartX) and (y = PolyStartY) then
    exit;

  for i := 0 to numsegs - 1 do
  begin
    if segs[i].miniseg then
      continue;
    if (segs[i].v1.x = x) and (segs[i].v1.y = y) then
    begin
      if segList = nil then
        inc(PolySegCount)
      else
      begin
        segList^ := @segs[i];
        inc(segList);
      end;
      IterFindPolySegs(segs[i].v2.x, segs[i].v2.y, segList);
      exit;
    end;
  end;
  I_Error('IterFindPolySegs(): Non-closed Polyobj located.');
end;


//==========================================================================
//
// SpawnPolyobj
//
//==========================================================================

procedure SpawnPolyobj(index: integer; tag: integer; crush: boolean);
var
  i, j: integer;
  psIndex, psIndexOld: integer;
  polySegList: array[0..PO_MAXPOLYSEGS - 1] of Pseg_t;
  polsegs: PPseg_t;
begin
  for i := 0 to numsegs - 1 do
  begin
    if not segs[i].miniseg then
      if (segs[i].linedef.special = PO_LINE_START) and
         (segs[i].linedef.arg1 = tag) then
      begin
        if polyobjs[index].segs <> nil then
          I_Error('SpawnPolyobj(): Polyobj %d already spawned.', [tag]);
        segs[i].linedef.special := 0;
        segs[i].linedef.arg1 := 0;
        PolySegCount := 1;
        PolyStartX := segs[i].v1.x;
        PolyStartY := segs[i].v1.y;
        IterFindPolySegs(segs[i].v2.x, segs[i].v2.y, nil);

        polyobjs[index].numsegs := PolySegCount;
        polyobjs[index].segs := Z_Malloc(PolySegCount * SizeOf(Pseg_t), PU_LEVEL, nil);
        polyobjs[index].segs^ := @segs[i]; // insert the first seg
        polsegs := polyobjs[index].segs;
        inc(polsegs);
        IterFindPolySegs(segs[i].v2.x, segs[i].v2.y, polsegs);
        polyobjs[index].crush := crush;
        polyobjs[index].tag := tag;
        polyobjs[index].seqType := segs[i].linedef.arg3;
        if (polyobjs[index].seqType < 0) or
           (polyobjs[index].seqType >= Ord(SEQTYPE_NUMSEQ)) then
          polyobjs[index].seqType := 0;
        break;
      end;
  end;
  if polyobjs[index].segs = nil then
  begin // didn't find a polyobj through PO_LINE_START
    psIndex := 0;
    polyobjs[index].numsegs := 0;
    for j := 1 to PO_MAXPOLYSEGS - 1 do
    begin
      psIndexOld := psIndex;
      for i := 0 to numsegs - 1 do
      begin
        if (segs[i].linedef.special = PO_LINE_EXPLICIT) and
           (segs[i].linedef.arg1 = tag) then
        begin
          if segs[i].linedef.arg2 = 0 then
            I_Error('SpawnPolyobj(): Explicit line missing order number (probably %d) in poly %d.',
              [j + 1, tag]);
          if segs[i].linedef.arg2 = j then
          begin
            polySegList[psIndex] := @segs[i];
            inc(polyobjs[index].numsegs);
            inc(psIndex);
            if psIndex > PO_MAXPOLYSEGS then
              I_Error('SpawnPolyobj(): psIndex > PO_MAXPOLYSEGS');
          end;
        end;
      end;
      // Clear out any specials for these segs...we cannot clear them out
      //   in the above loop, since we aren't guaranteed one seg per
      //    linedef.
      for i := 0 to numsegs - 1 do
      begin
        if (segs[i].linedef.special = PO_LINE_EXPLICIT) and
           (segs[i].linedef.arg1 = tag) and
           (segs[i].linedef.arg2 = j) then
        begin
          segs[i].linedef.special := 0;
          segs[i].linedef.arg1 := 0;
        end;
      end;
      if psIndex = psIndexOld then
      begin // Check if an explicit line order has been skipped
        // A line has been skipped if there are any more explicit
        // lines with the current tag value
        for i := 0 to numsegs - 1 do
        begin
          if (segs[i].linedef.special = PO_LINE_EXPLICIT) and
             (segs[i].linedef.arg1 = tag) then
            I_Error('SpawnPolyobj(): Missing explicit line %d for poly %d',
              [j, tag]);
        end;
      end;
    end;
    if polyobjs[index].numsegs <> 0 then
    begin
      PolySegCount := polyobjs[index].numsegs; // PolySegCount used globally
      polyobjs[index].crush := crush;
      polyobjs[index].tag := tag;
      polyobjs[index].segs := Z_Malloc(polyobjs[index].numsegs * SizeOf(Pseg_t), PU_LEVEL, nil);
      for i := 0 to polyobjs[index].numsegs - 1 do
        Pseg_tPArray(polyobjs[index].segs)[i] := polySegList[i];
      polyobjs[index].seqType := polyobjs[index].segs^.linedef.arg4;
    end;
    // Next, change the polyobjs first line to point to a mirror
    //    if it exists
    polyobjs[index].segs^.linedef.arg2 := polyobjs[index].segs^.linedef.arg3;
  end;
end;

//==========================================================================
//
// TranslateToStartSpot
//
//==========================================================================

procedure TranslateToStartSpot(tag: integer; originX, originY: integer);
var
  tempSeg: PPseg_t;
  veryTempSeg: PPseg_t;
  tempPt: Pvertex_t;
  sub: Psubsector_t;
  po: Ppolyobj_t;
  deltaX, deltaY: integer;
  avg: vertex_t; // used to find a polyobj's center, and hence subsector
  i: integer;
  l: Pline_t;
  v1: Pvertex_t;
begin
  po := nil;
  for i := 0 to po_NumPolyobjs - 1 do
  begin
    if polyobjs[i].tag = tag then
    begin
      po := @polyobjs[i];
      break;
    end;
  end;
  if po = nil then
  begin // didn't match the tag with a polyobj tag
    I_Error('TranslateToStartSpot(): Unable to match polyobj tag: %d', [tag]);
  end;
  if po.segs = nil then
  begin
    I_Error('TranslateToStartSpot(): Anchor point located without a StartSpot point: %d', [tag]);
  end;
  po.originalPts := Z_Malloc(po.numsegs * SizeOf(vertex_t), PU_LEVEL, nil);
  po.prevPts := Z_Malloc(po.numsegs * SizeOf(vertex_t), PU_LEVEL, nil);
  deltaX := originX - po.startSpot.x;
  deltaY := originY - po.startSpot.y;

  tempSeg := po.segs;
  tempPt := @po.originalPts[0];
  avg.x := 0;
  avg.y := 0;

  inc(validcount);
  for i := 0 to po.numsegs - 1 do
  begin
    if not tempSeg^.miniseg then
    begin
      l := tempSeg^.linedef;
      if l.validcount <> validcount then
      begin
        l.bbox[BOXTOP] := l.bbox[BOXTOP] - deltaY;
        l.bbox[BOXBOTTOM] := l.bbox[BOXBOTTOM] - deltaY;
        l.bbox[BOXLEFT] := l.bbox[BOXLEFT] - deltaX;
        l.bbox[BOXRIGHT] := l.bbox[BOXRIGHT] - deltaX;
        l.validcount := validcount;
      end;
      veryTempSeg := po.segs;
      while veryTempSeg <> tempSeg do
      begin
        if not veryTempSeg^.miniseg then
          if veryTempSeg^.v1 = tempSeg^.v1 then
            break;
        inc(veryTempSeg);
      end;
      v1 := tempSeg^.v1;
      if veryTempSeg = tempSeg then
      begin // the point hasn't been translated, yet
        v1.x := v1.x - deltaX;
        v1.y := v1.y - deltaY;
      end;
      avg.x := avg.x + FixedInt(v1.x);
      avg.y := avg.y + FixedInt(v1.y);
      // the original Pts are based off the startSpot Pt, and are
      // unique to each seg, not each linedef
      tempPt.x := v1.x - po.startSpot.x;
      tempPt.y := v1.y - po.startSpot.y;
    end;
    inc(tempSeg);
    inc(tempPt);
  end;
  avg.x := avg.x div po.numsegs;
  avg.y := avg.y div po.numsegs;
  sub := R_PointInSubsector(avg.x * FRACUNIT, avg.y * FRACUNIT);
  if sub.poly <> nil then
    I_Error('PO_TranslateToStartSpot(): Multiple polyobjs in a single subsector.');
  sub.poly := po;
end;

//==========================================================================
//
// PO_Init
//
//==========================================================================

procedure PO_Init(lump: integer);
var
  data: PByteArray;
  i: integer;
  mt: Pmapthing_t;
  numthings: integer;
  polyIndex: integer;
begin
  polyobjs := Z_Malloc(po_NumPolyobjs * SizeOf(polyobj_t), PU_LEVEL, nil);
  memset(polyobjs, 0, po_NumPolyobjs * SizeOf(polyobj_t));

  data := W_CacheLumpNum(lump, PU_STATIC);
  numthings := W_LumpLength(lump) div SizeOf(mapthing_t);
  mt := Pmapthing_t(data);
  polyIndex := 0; // index polyobj number
  // Find the startSpot points, and spawn each polyobj
  for i := 0 to numthings - 1 do
  begin
    // 3001 := no crush, 3002 := crushing
    if (mt._type = PO_SPAWN_TYPE) or (mt._type = PO_SPAWNCRUSH_TYPE) then
    begin // Polyobj StartSpot Pt.
      polyobjs[polyIndex].startSpot.x := mt.x * FRACUNIT;
      polyobjs[polyIndex].startSpot.y := mt.y * FRACUNIT;
      SpawnPolyobj(polyIndex, mt.angle, (mt._type = PO_SPAWNCRUSH_TYPE));
      inc(polyIndex);
    end;
    inc(mt);
  end;
  mt := Pmapthing_t(data);
  for i := 0 to numthings - 1 do
  begin
    if mt._type = PO_ANCHOR_TYPE then // Polyobj Anchor Pt.
      TranslateToStartSpot(mt.angle, mt.x * FRACUNIT, mt.y * FRACUNIT);
    inc(mt);
  end;
  Z_Free (data);
  // check for a startspot without an anchor point
  for i := 0 to po_NumPolyobjs - 1 do
  begin
    if polyobjs[i].originalPts = nil then
      I_Error('PO_Init(): StartSpot located without an Anchor point: %d', [polyobjs[i].tag]);
  end;
  InitBlockMap;
end;

//==========================================================================
//
// PO_Busy
//
//==========================================================================

function PO_Busy(polyobj: integer): boolean;
var
  poly: Ppolyobj_t;
begin
  poly := GetPolyobj(polyobj);
  result := poly.specialdata <> nil;
end;

end.
