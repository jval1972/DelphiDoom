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
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_intrpl;

// JVAL
// Frame interpolation to exceed the 35fps limit
//

interface

uses
  d_delphi,
  m_fixed;

//==============================================================================
//
// R_InitInterpolations
//
//==============================================================================
procedure R_InitInterpolations;

//==============================================================================
//
// R_ResetInterpolationBuffer
//
//==============================================================================
procedure R_ResetInterpolationBuffer;

//==============================================================================
//
// R_StoreInterpolationData
//
//==============================================================================
procedure R_StoreInterpolationData(const tick: integer; const counts: integer);

//==============================================================================
//
// R_RestoreInterpolationData
//
//==============================================================================
procedure R_RestoreInterpolationData;

//==============================================================================
//
// R_RestoreInterpolationPolys
//
//==============================================================================
procedure R_RestoreInterpolationPolys;

//==============================================================================
//
// R_Interpolate
//
//==============================================================================
function R_Interpolate: boolean;

//==============================================================================
//
// R_InterpolateTicker
//
//==============================================================================
procedure R_InterpolateTicker;

//==============================================================================
//
// R_SetInterpolateSkipTicks
//
//==============================================================================
procedure R_SetInterpolateSkipTicks(const ticks: integer);

var
  interpolate: boolean;
  interpolateprecise: boolean = true;
  interpolateoncapped: boolean = false;
  interpolatepolyobjs: boolean = true;
  interpolatereducelag: boolean = false;
  interpolationstarttime: fixed_t = 0;
  didinterpolations: boolean;
  storedpolyinterpolations: boolean;
  ticfrac: fixed_t;
  ticfrac_f: float;

const
  NUM_SECTOR_INTERPOLATE_GROUPS = 4;

implementation

uses
  d_player,
  d_think,
  c_cmds,
  m_misc,
  g_game,
  {$IFDEF HEXEN}
  g_demo,
  {$ENDIF}
  mt_utils,
  i_system,
  p_setup,
  p_tick,
  p_mobj,
  p_mobj_h,
  p_pspr_h,
  po_man,
  r_defs,
  r_main,
  tables;

type
  itype = (
    iinteger,
    iinteger2,
    ismallint,
    ibyte,
    iangle,
    ifloat,
    imobj,
    isector,
    ipolyrotate,
    ipolymove
  );

  // Interpolation item
  //  Holds information about the previous and next values and interpolation type
  iitem_t = record
    lastaddress: pointer;
    address: pointer;
    case _type: itype of
      iinteger: (iprev, inext: integer);
      iinteger2: (iprev2, inext2: integer);
      ismallint: (siprev, sinext: smallint);
      ibyte: (bprev, bnext: byte);
      iangle: (aprev, anext: LongWord);
      ifloat: (fprev, fnext: float);
  end;
  Piitem_t = ^iitem_t;
  iitem_tArray = array[0..$FFFF] of iitem_t;
  Piitem_tArray = ^iitem_tArray;

  // Interpolation structure
  //  Holds the global interpolation items list
  istruct_t = record
    numitems: integer;
    realsize: integer;
    items: Piitem_tArray;
  end;
  Pistruct_t = ^istruct_t;

  sectorinterpolatearray_t = record
    numsectors: integer;
    sectors: array[0..$FFFF] of Psector_t;
  end;
  Psectorinterpolatearray_t = ^sectorinterpolatearray_t;

const
  IGROWSTEP = 256;
  NUMISTRUCTS = 3;
  IDS_PLAYER = 0;
  IDS_LINES = 1;
  IDS_POLYS = 2;

  IDS_SECTOR1 = 0;
  IDS_SECTOR2 = 1;
  IDS_SECTOR3 = 2;
  IDS_SECTOR4 = 3;

var
  istructs: array[0..NUMISTRUCTS - 1] of istruct_t;
  isectors: array[0..NUM_SECTOR_INTERPOLATE_GROUPS - 1] of sectorinterpolatearray_t;
  imobjs: array[0..$FFFF] of Pmobj_t;
  numismobjs: integer;

//==============================================================================
//
// CmdInterpolate
//
//==============================================================================
procedure CmdInterpolate(const parm: string = '');
var
  newval: boolean;
begin
  if parm = '' then
  begin
    printf('Current setting: interpolate = %s.'#13#10, [truefalseStrings[interpolate]]);
    exit;
  end;

  newval := C_BoolEval(parm, interpolate);
  if newval <> interpolate then
  begin
    interpolate := newval;
    R_SetInterpolateSkipTicks(1);
  end;

  CmdInterpolate;
end;

//==============================================================================
//
// CmdInterpolateOncapped
//
//==============================================================================
procedure CmdInterpolateOncapped(const parm: string = '');
var
  newval: boolean;
begin
  if parm = '' then
  begin
    printf('Current setting: interpolateoncapped = %s.'#13#10, [truefalseStrings[interpolateoncapped]]);
    exit;
  end;

  newval := C_BoolEval(parm, interpolateoncapped);
  if newval <> interpolateoncapped then
  begin
    interpolateoncapped := newval;
    R_SetInterpolateSkipTicks(1);
  end;

  CmdInterpolateOncapped;
end;

//==============================================================================
//
// R_InitInterpolations
//
//==============================================================================
procedure R_InitInterpolations;
var
  i: integer;
begin
  for i := 0 to NUMISTRUCTS - 1 do
  begin
    istructs[i].numitems := 0;
    istructs[i].realsize := 0;
    istructs[i].items := nil;
  end;
  MT_ZeroMemory(@imobjs, SizeOf(imobjs));
  MT_ZeroMemory(@isectors, SizeOf(isectors));
  numismobjs := 0;
  C_AddCmd('interpolate, interpolation, setinterpolation, r_interpolate', @CmdInterpolate);
  C_AddCmd('interpolateoncapped, r_interpolateoncapped', @CmdInterpolateOncapped);
end;

//==============================================================================
//
// R_ResetInterpolationBuffer
//
//==============================================================================
procedure R_ResetInterpolationBuffer;
var
  i: integer;
begin
  for i := 0 to NUMISTRUCTS - 1 do
  begin
    memfree(pointer(istructs[i].items), istructs[i].realsize * SizeOf(iitem_t));
    istructs[i].numitems := 0;
    istructs[i].realsize := 0;
  end;
  for i := 0 to NUM_SECTOR_INTERPOLATE_GROUPS - 1 do
    isectors[i].numsectors := 0;
  numismobjs := 0;
end;

//==============================================================================
//
// R_InterpolationCalcIF
//
//==============================================================================
function R_InterpolationCalcIF(const prev, next: fixed_t): fixed_t;
begin
  if next = prev then
    result := prev
  else
    result := prev + round((next - prev) * ticfrac_f);
end;

const
  II2MARGIN = 8 * FRACUNIT;

//==============================================================================
//
// R_InterpolationCalcIF2
//
//==============================================================================
function R_InterpolationCalcIF2(const prev, next: fixed_t): fixed_t;
var
  diff: integer;
begin
  if next = prev then
    result := prev
  else
  begin
    diff := prev - next;
    if (diff > II2MARGIN) or (diff < -II2MARGIN) then
    begin
      if ticfrac < FRACUNIT div 2 then
        result := prev
      else
        result := next
    end
    else
      result := prev + round((next - prev) * ticfrac_f);
  end;
end;

//==============================================================================
//
// R_InterpolationCalcSIF
//
//==============================================================================
function R_InterpolationCalcSIF(const prev, next: smallint): smallint;
begin
  if next = prev then
    result := prev
  else
    result := prev + round((next - prev) * ticfrac_f);
end;

function R_InterpolationCalcFF(const prev, next: float): float;
begin
  if next = prev then
    result := prev
  else
    result := prev + (next - prev) * ticfrac_f;
end;


//==============================================================================
//
// R_InterpolationCalcI
//
//==============================================================================
procedure R_InterpolationCalcI(const pi: Piitem_t);
begin
  if pi.inext = pi.iprev then
    exit;

  PInteger(pi.address)^ := pi.iprev + round((pi.inext - pi.iprev) * ticfrac_f);
end;

//==============================================================================
//
// R_InterpolationCalcI2
//
//==============================================================================
procedure R_InterpolationCalcI2(const pi: Piitem_t);
var
  diff: integer;
begin
  if pi.inext2 = pi.iprev2 then
    exit;

  diff := pi.inext2 - pi.iprev2;
  if (diff > II2MARGIN) or (diff < -II2MARGIN) then
  begin
    if ticfrac < FRACUNIT div 2 then
      PInteger(pi.address)^ := pi.iprev2
    else
      PInteger(pi.address)^ := pi.inext2
  end
  else
    PInteger(pi.address)^ := pi.iprev2 + round((pi.inext2 - pi.iprev2) * ticfrac_f);
end;

//==============================================================================
//
// R_InterpolationCalcSI
//
//==============================================================================
procedure R_InterpolationCalcSI(const pi: Piitem_t);
begin
  if pi.sinext = pi.siprev then
    exit;

  PSmallInt(pi.address)^ := pi.siprev + round((pi.sinext - pi.siprev) * ticfrac_f);
end;

//==============================================================================
//
// R_InterpolationCalcB
//
//==============================================================================
function R_InterpolationCalcB(const prev, next: byte): byte;
begin
  if next = prev then
    result := prev
  else if (next = 0) or (prev = 0) then // Hack for player.lookdir2
    result := next
  else if ((next > 247) and (prev < 8)) or ((next < 8) and (prev > 247)) then // Hack for player.lookdir2
    result := 0
  else
    result := prev + (next - prev) * ticfrac div FRACUNIT;
end;

//==============================================================================
//
// R_InterpolationCalcA
//
//==============================================================================
function R_InterpolationCalcA(const prev, next: angle_t): angle_t;
var
  prev_e, next_e, mid_e: float;
begin
  if prev = next then
    result := prev
  else
  begin
    if ((prev < ANG90) and (next > ANG270)) or
       ((next < ANG90) and (prev > ANG270)) then
    begin
      prev_e := prev / ANGLE_MAX;
      next_e := next / ANGLE_MAX;
      if prev > next then
        next_e := next_e + 1.0
      else
        prev_e := prev_e + 1.0;

      mid_e := prev_e + (next_e - prev_e) * ticfrac_f;
      if mid_e > 1.0 then
        mid_e := mid_e - 1.0;
      result := round(mid_e * ANGLE_MAX);
    end
    else if prev > next then
    begin
      result := prev - round((prev - next) * ticfrac_f);
    end
    else
    begin
      result := prev + round((next - prev) * ticfrac_f);
    end;
  end;
end;

//==============================================================================
//
// R_InterpolationCalcF
//
//==============================================================================
procedure R_InterpolationCalcF(const pi: Piitem_t);
begin
  if pi.fnext = pi.fprev then
    exit;

  Pfloat(pi.address)^ := pi.fprev + (pi.fnext - pi.fprev) * ticfrac_f;
end;

//==============================================================================
//
// R_InterpolationCalcPolyRotation
//
//==============================================================================
procedure R_InterpolationCalcPolyRotation(const pi: Piitem_t);
var
  po: Ppolyobj_t;
begin
  po := pi.address;

  if po.nextangle = po.prevangle then
    exit;

  po.angle := R_InterpolationCalcA(po.prevangle, po.nextangle);
  PO_RotatePolyIntrpl(po);
end;

//==============================================================================
//
// R_InterpolationCalcPolyMove
//
//==============================================================================
procedure R_InterpolationCalcPolyMove(const pi: Piitem_t);
var
  po: Ppolyobj_t;
begin
  po := pi.address;

  if po.nextx = po.prevx then
    if po.nexty = po.prevy then
      exit;

  po.x := R_InterpolationCalcIF(po.prevx, po.nextx);
  po.y := R_InterpolationCalcIF(po.prevy, po.nexty);
  PO_MovePolyIntrpl(po);
end;

//==============================================================================
//
// R_InterpolationCalcSector
//
//==============================================================================
procedure R_InterpolationCalcSector(const sec: Psector_t);
begin
  sec.floorheight := R_InterpolationCalcIF2(sec.prev_floorheight, sec.next_floorheight);
  sec.ceilingheight := R_InterpolationCalcIF2(sec.prev_ceilingheight, sec.next_ceilingheight);
  sec.lightlevel := R_InterpolationCalcSIF(sec.prev_lightlevel, sec.next_lightlevel);
  {$IFDEF DOOM_OR_STRIFE}
  // JVAL: 30/9/2009
  // JVAL: 9/11/2015 Added strife offsets
  sec.floor_xoffs := R_InterpolationCalcIF(sec.prev_floor_xoffs, sec.next_floor_xoffs);
  sec.floor_yoffs := R_InterpolationCalcIF(sec.prev_floor_yoffs, sec.next_floor_yoffs);
  sec.ceiling_xoffs := R_InterpolationCalcIF(sec.prev_ceiling_xoffs, sec.next_ceiling_xoffs);
  sec.ceiling_yoffs := R_InterpolationCalcIF(sec.prev_ceiling_yoffs, sec.next_ceiling_yoffs);
  {$ENDIF}
  if sec.renderflags and SRF_INTERPOLATE_ROTATE <> 0 then
  begin
    sec.floorangle := R_InterpolationCalcA(sec.prev_floorangle, sec.next_floorangle);
    sec.flooranglex := R_InterpolationCalcIF(sec.prev_flooranglex, sec.next_flooranglex);
    sec.floorangley := R_InterpolationCalcIF(sec.prev_floorangley, sec.next_floorangley);
    sec.ceilingangle := R_InterpolationCalcA(sec.prev_ceilingangle, sec.next_ceilingangle);
    sec.ceilinganglex := R_InterpolationCalcIF(sec.prev_ceilinganglex, sec.next_ceilinganglex);
    sec.ceilingangley := R_InterpolationCalcIF(sec.prev_ceilingangley, sec.next_ceilingangley);
  end;
  if sec.renderflags and SRF_INTERPOLATE_FLOORSLOPE <> 0 then
  begin
    sec.fa := R_InterpolationCalcFF(sec.prev_fa, sec.next_fa);
    sec.fb := R_InterpolationCalcFF(sec.prev_fb, sec.next_fb);
    sec.fd := R_InterpolationCalcFF(sec.prev_fd, sec.next_fd);
    sec.fic := R_InterpolationCalcFF(sec.prev_fic, sec.next_fic);
  end;
  if sec.renderflags and SRF_INTERPOLATE_CEILINGSLOPE <> 0 then
  begin
    sec.ca := R_InterpolationCalcFF(sec.prev_ca, sec.next_ca);
    sec.cb := R_InterpolationCalcFF(sec.prev_cb, sec.next_cb);
    sec.cd := R_InterpolationCalcFF(sec.prev_cd, sec.next_cd);
    sec.cic := R_InterpolationCalcFF(sec.prev_cic, sec.next_cic);
  end;
end;

//==============================================================================
//
// R_InterpolationCalcMobj
//
//==============================================================================
procedure R_InterpolationCalcMobj(const mo: Pmobj_t);
begin
  mo.x := R_InterpolationCalcIF(mo.prevx, mo.nextx);
  mo.y := R_InterpolationCalcIF(mo.prevy, mo.nexty);
  mo.z := R_InterpolationCalcIF(mo.prevz, mo.nextz);
  mo.angle := R_InterpolationCalcA(mo.prevangle, mo.nextangle);
end;

//==============================================================================
//
// R_AddInterpolationItem
//
//==============================================================================
procedure R_AddInterpolationItem(const addr: pointer; const typ: itype; const istruct: Pistruct_t);
var
  newrealsize: integer;
  pisector: Psectorinterpolatearray_t;
  sec: Psector_t;
  pi: Piitem_t;
begin
  if typ = imobj then
  begin
    imobjs[numismobjs] := addr;
    if numismobjs < $FFFF then
      inc(numismobjs);
    Pmobj_t(addr).prevx := Pmobj_t(addr).nextx;
    Pmobj_t(addr).prevy := Pmobj_t(addr).nexty;
    Pmobj_t(addr).prevz := Pmobj_t(addr).nextz;
    Pmobj_t(addr).prevangle := Pmobj_t(addr).nextangle;
    Pmobj_t(addr).nextx := Pmobj_t(addr).x;
    Pmobj_t(addr).nexty := Pmobj_t(addr).y;
    Pmobj_t(addr).nextz := Pmobj_t(addr).z;
    Pmobj_t(addr).nextangle := Pmobj_t(addr).angle;
    inc(Pmobj_t(addr).intrplcnt);
    exit;
  end;

  if typ = isector then
  begin
    pisector := @isectors[Psector_t(addr).interpolate_group];
    if pisector.numsectors < $FFFF then
    begin
      sec := Psector_t(addr);
      pisector.sectors[pisector.numsectors] := sec;
      inc(pisector.numsectors);

      sec.prev_floorheight := sec.next_floorheight;
      sec.prev_ceilingheight := sec.next_ceilingheight;
      sec.prev_lightlevel := sec.next_lightlevel;
      sec.next_floorheight := sec.floorheight;
      sec.next_ceilingheight := sec.ceilingheight;
      sec.next_lightlevel := sec.lightlevel;
      {$IFDEF DOOM_OR_STRIFE}
      // JVAL: 30/9/2009
      // JVAL: 9/11/2015 Added strife offsets
      sec.prev_floor_xoffs := sec.next_floor_xoffs;
      sec.prev_floor_yoffs := sec.next_floor_yoffs;
      sec.prev_ceiling_xoffs := sec.next_ceiling_xoffs;
      sec.prev_ceiling_yoffs := sec.next_ceiling_yoffs;
      sec.next_floor_xoffs := sec.floor_xoffs;
      sec.next_floor_yoffs := sec.floor_yoffs;
      sec.next_ceiling_xoffs := sec.ceiling_xoffs;
      sec.next_ceiling_yoffs := sec.ceiling_yoffs;
      {$ENDIF}
      if sec.renderflags and SRF_INTERPOLATE_ROTATE <> 0 then
      begin
        sec.prev_floorangle := sec.next_floorangle;
        sec.prev_flooranglex := sec.next_flooranglex;
        sec.prev_floorangley := sec.next_floorangley;
        sec.prev_ceilingangle := sec.next_ceilingangle;
        sec.prev_ceilinganglex := sec.next_ceilinganglex;
        sec.prev_ceilingangley := sec.next_ceilingangley;
        sec.next_floorangle := sec.floorangle;
        sec.next_flooranglex := sec.flooranglex;
        sec.next_floorangley := sec.floorangley;
        sec.next_ceilingangle := sec.ceilingangle;
        sec.next_ceilinganglex := sec.ceilinganglex;
        sec.next_ceilingangley := sec.ceilingangley;
      end;
      if sec.renderflags and SRF_INTERPOLATE_FLOORSLOPE <> 0 then
      begin
        sec.prev_fa := sec.next_fa;
        sec.prev_fb := sec.next_fb;
        sec.prev_fd := sec.next_fd;
        sec.prev_fic := sec.next_fic;
        sec.next_fa := sec.fa;
        sec.next_fb := sec.fb;
        sec.next_fd := sec.fd;
        sec.next_fic := sec.fic;
      end;
      if sec.renderflags and SRF_INTERPOLATE_CEILINGSLOPE <> 0 then
      begin
        sec.prev_ca := sec.next_ca;
        sec.prev_cb := sec.next_cb;
        sec.prev_cd := sec.next_cd;
        sec.prev_cic := sec.next_cic;
        sec.next_ca := sec.ca;
        sec.next_cb := sec.cb;
        sec.next_cd := sec.cd;
        sec.next_cic := sec.cic;
      end;
      inc(sec.intrplcnt);
    end;
    exit;
  end;

  if istruct.realsize <= istruct.numitems then
  begin
    newrealsize := istruct.realsize + IGROWSTEP;
    realloc(pointer(istruct.items), istruct.realsize * SizeOf(iitem_t), newrealsize * SizeOf(iitem_t));
    ZeroMemory(@istruct.items[istruct.realsize], IGROWSTEP * SizeOf(iitem_t));
    istruct.realsize := newrealsize;
  end;
  pi := @istruct.items[istruct.numitems];
  pi.lastaddress := pi.address;
  pi.address := addr;
  pi._type := typ;
  case typ of
    iinteger:
      begin
        pi.iprev := pi.inext;
        pi.inext := PInteger(addr)^;
      end;
    iinteger2:
      begin
        pi.iprev2 := pi.inext2;
        pi.inext2 := PInteger(addr)^;
      end;
    ismallint:
      begin
        pi.siprev := pi.sinext;
        pi.sinext := PSmallInt(addr)^;
      end;
    ibyte:
      begin
        pi.bprev := pi.bnext;
        pi.bnext := PByte(addr)^;
      end;
    iangle:
      begin
        pi.aprev := pi.anext;
        pi.anext := Pangle_t(addr)^;
      end;
    ifloat:
      begin
        pi.fprev := pi.fnext;
        pi.fnext := Pfloat(addr)^;
      end;
    ipolyrotate:
      begin
        PO_SaveSegsIntrpl(Ppolyobj_t(addr));
        Ppolyobj_t(addr).nextangle := Ppolyobj_t(addr).angle;
        Ppolyobj_t(addr).angle := Ppolyobj_t(addr).prevangle;
        PO_RotatePolyIntrpl(Ppolyobj_t(addr));
      end;
    ipolymove:
      begin
        PO_SaveSegsIntrpl(Ppolyobj_t(addr));
        Ppolyobj_t(addr).x := Ppolyobj_t(addr).prevx;
        Ppolyobj_t(addr).y := Ppolyobj_t(addr).prevy;
        Ppolyobj_t(addr).nextx := Ppolyobj_t(addr).startspot.x;
        Ppolyobj_t(addr).nexty := Ppolyobj_t(addr).startspot.y;
        PO_MovePolyIntrpl(Ppolyobj_t(addr));
      end;
  end;
  inc(istruct.numitems);
end;

var
  interpolationcount: integer = 0;
  prevtic: fixed_t = 0;

// JVAL: Skip interpolation if we have teleport
var
  skipinterpolationticks: integer = -1;

//==============================================================================
//
// R_StoreInterpolationData
//
//==============================================================================
procedure R_StoreInterpolationData(const tick: integer; const counts: integer);
var
  sec: Psector_t;
  li: Pline_t;
  si: PSide_t;
  i, j: integer;
  player: Pplayer_t;
  th: Pthinker_t;
begin
  if prevtic > 0 then
    if gametic = prevtic then
      exit;

  {$IFDEF DEBUG}
  I_DevWarning('R_StoreInterpolationData(): - start - fractime = %5.3f, gametic = %d'#13#10, [I_GetFracTime / FRACUNIT, gametic]);
  I_DevWarning('R_StoreInterpolationData(): - gametic = %d, tick = %d, time = %d'#13#10, [gametic, tick, I_GetTime]);
  {$ENDIF}
  prevtic := gametic;
  interpolationcount := counts;
  for i := 0 to NUMISTRUCTS - 1 do
    istructs[i].numitems := 0;
  for i := 0 to NUM_SECTOR_INTERPOLATE_GROUPS - 1 do
    isectors[i].numsectors := 0;
  numismobjs := 0;

  // Interpolate player
  player := @players[displayplayer];

  R_AddInterpolationItem(@player.lookdir, iinteger, @istructs[IDS_PLAYER]);
  R_AddInterpolationItem(@player.lookdir16, iinteger, @istructs[IDS_PLAYER]); // JVAL Smooth Look Up/Down
  R_AddInterpolationItem(@player.lookdir2, ibyte, @istructs[IDS_PLAYER]);
  R_AddInterpolationItem(@player.teleporttics, iinteger, @istructs[IDS_PLAYER]);
  R_AddInterpolationItem(@player.quaketics, iinteger, @istructs[IDS_PLAYER]);
  for i := 0 to Ord(NUMPSPRITES) - 1 do
  begin
    R_AddInterpolationItem(@player.psprites[i].sx, iinteger, @istructs[IDS_PLAYER]);
    R_AddInterpolationItem(@player.psprites[i].sy, iinteger, @istructs[IDS_PLAYER]);
  end;
  sec := R_PointInSubsector(viewx, viewy).sector;
  if sec.renderflags and SRF_NO_INTERPOLATE = 0 then
    R_AddInterpolationItem(@player.viewz, iinteger, @istructs[IDS_PLAYER]);

  // Interpolate Sectors
  sec := @sectors[0];
  for i := 0 to numsectors - 1 do
  begin
    if sec.rendervalidcount = rendervalidcount then
      if sec.renderflags and SRF_NO_INTERPOLATE = 0 then
      begin
        R_AddInterpolationItem(sec, isector, nil);
        inc(sec);
        Continue;
      end;
    sec.intrplcnt := 0;
    inc(sec);
  end;

  // Interpolate Lines
  li := @lines[0];
  for i := 0 to numlines - 1 do
  begin
    if (li.special <> 0)
      {$IFDEF DOOM_OR_STRIFE} or (li.tag <> 0) {$ENDIF} then // JVAL: For scroll specials 1024, 1025 & 1026
      for j := 0 to 1 do
      begin
        if li.sidenum[j] > -1 then
        begin
          si := @sides[li.sidenum[j]];
          R_AddInterpolationItem(@si.textureoffset, iinteger2, @istructs[IDS_LINES]);
          R_AddInterpolationItem(@si.rowoffset, iinteger2, @istructs[IDS_LINES]);
        end;
      end;
    inc(li);
  end;

  // Poly objects
  if interpolatepolyobjs then
    // JVAL: Polyobj interpolation may cause desync, needs verification
    if not demoplayback and not demorecording then
      for i := 0 to po_NumPolyobjs - 1 do
        if polyobjs[i].specialdata <> nil then
        begin
          if (@Ppolyevent_t(polyobjs[i].specialdata).thinker._function.acp1 = @TH_MovePoly) or
             ((@Ppolydoor_t(polyobjs[i].specialdata).thinker._function.acp1 = @TH_PolyDoor) and (Ppolydoor_t(polyobjs[i].specialdata)._type = PODOOR_SLIDE)) then
            R_AddInterpolationItem(@polyobjs[i], ipolymove, @istructs[IDS_POLYS])
          else
            R_AddInterpolationItem(@polyobjs[i], ipolyrotate, @istructs[IDS_POLYS]);
          storedpolyinterpolations := true;
        end;

  // Map Objects
  th := thinkercap.next;
  while (th <> nil) and (th <> @thinkercap) do
  begin
    if @th._function.acp1 = @P_MobjThinker then
    begin
    {$IFDEF DOOM}
      if Pmobj_t(th).flags4_ex and MF4_EX_JUSTAPPEARED = 0 then
    {$ELSE}
      if Pmobj_t(th).flags2_ex and MF2_EX_JUSTAPPEARED = 0 then
    {$ENDIF}
    // JVAL: 20200105 - Interpolate only mobjs that the renderer touched
      if (Pmobj_t(th).rendervalidcount = rendervalidcount) or (Pmobj_t(th).player <> nil) then
        if Pmobj_t(th).flags3_ex and MF3_EX_NORENDERINTERPOLATION = 0 then
        begin
          R_AddInterpolationItem(th, imobj, nil);
          th := th.next;
          Continue;
        end;
      Pmobj_t(th).intrplcnt := 0;
    end;
    th := th.next;
  end;
  {$IFDEF DEBUG}
  I_DevWarning('R_StoreInterpolationData(): - finished - fractime = %5.3f, gametic = %d'#13#10, [I_GetFracTime / FRACUNIT, gametic]);
  {$ENDIF}
end;

//==============================================================================
//
// R_RestoreInterpolationPolyRotate
//
//==============================================================================
procedure R_RestoreInterpolationPolyRotate(const po: Ppolyobj_t);
begin
  po.angle := po.nextangle;
  po.prevangle := po.nextangle;
  PO_RotatePolyIntrpl(po);
  PO_RestoreSegsIntrpl(po);
end;

//==============================================================================
//
// R_RestoreInterpolationPolyMove
//
//==============================================================================
procedure R_RestoreInterpolationPolyMove(const po: Ppolyobj_t);
begin
  po.x := po.nextx;
  po.y := po.nexty;
  po.prevx := po.startSpot.x;
  po.prevy := po.startSpot.y;
  PO_MovePolyIntrpl(po);
  PO_RestoreSegsIntrpl(po);
end;

//==============================================================================
//
// R_RestoreInterpolationSector
//
//==============================================================================
procedure R_RestoreInterpolationSector(const sec: Psector_t);
begin
  sec.floorheight := sec.next_floorheight;
  sec.ceilingheight := sec.next_ceilingheight;
  sec.lightlevel := sec.next_lightlevel;
  {$IFDEF DOOM_OR_STRIFE}
  // JVAL: 30/9/2009
  // JVAL: 9/11/2015 Added strife offsets
  sec.floor_xoffs := sec.next_floor_xoffs;
  sec.floor_yoffs := sec.next_floor_yoffs;
  sec.ceiling_xoffs := sec.next_ceiling_xoffs;
  sec.ceiling_yoffs := sec.next_ceiling_yoffs;
  {$ENDIF}
  if sec.renderflags and SRF_INTERPOLATE_ROTATE <> 0 then
  begin
    sec.floorangle := sec.next_floorangle;
    sec.flooranglex := sec.next_flooranglex;
    sec.floorangley := sec.next_floorangley;
    sec.ceilingangle := sec.next_ceilingangle;
    sec.ceilinganglex := sec.next_ceilinganglex;
    sec.ceilingangley := sec.next_ceilingangley;
  end;
  if sec.renderflags and SRF_INTERPOLATE_FLOORSLOPE <> 0 then
  begin
    sec.fa := sec.next_fa;
    sec.fb := sec.next_fb;
    sec.fd := sec.next_fd;
    sec.fic := sec.next_fic;
  end;
  if sec.renderflags and SRF_INTERPOLATE_CEILINGSLOPE <> 0 then
  begin
    sec.ca := sec.next_ca;
    sec.cb := sec.next_cb;
    sec.cd := sec.next_cd;
    sec.cic := sec.next_cic;
  end;
end;

//==============================================================================
//
// R_RestoreInterpolationMobj
//
//==============================================================================
procedure R_RestoreInterpolationMobj(mo: Pmobj_t);
begin
  mo.x := mo.nextx;
  mo.y := mo.nexty;
  mo.z := mo.nextz;
  mo.angle := mo.nextangle;
end;

//==============================================================================
//
// R_RestoreInterpolationData_thr1
//
//==============================================================================
function R_RestoreInterpolationData_thr1(p: pointer): integer; stdcall;
var
  i, j: integer;
  istruct: Pistruct_t;
  pi: Piitem_t;
begin
  for j := 0 to NUMISTRUCTS - 1 do
  begin
    istruct := @istructs[j];
    pi := @istruct.items[0];
    for i := 0 to istruct.numitems - 1 do
    begin
      case pi._type of
        iinteger: PInteger(pi.address)^ := pi.inext;
        iinteger2: PInteger(pi.address)^ := pi.inext2;
        ismallint: PSmallInt(pi.address)^ := pi.sinext;
        ibyte: PByte(pi.address)^ := pi.bnext;
        iangle: Pangle_t(pi.address)^ := pi.anext;
        ifloat: Pfloat(pi.address)^ := pi.fnext;
        ipolyrotate: R_RestoreInterpolationPolyRotate(Ppolyobj_t(pi.address));
        ipolymove: R_RestoreInterpolationPolyMove(Ppolyobj_t(pi.address));
      end;
      inc(pi);
    end;
  end;
  result := 0;
end;

//==============================================================================
//
// R_RestoreInterpolationData_thr2
//
//==============================================================================
function R_RestoreInterpolationData_thr2(p: pointer): integer; stdcall;
var
  i, j: integer;
  sec: Psector_t;
  pisector: Psectorinterpolatearray_t;
  r: mt_range_p;
begin
  r := mt_range_p(p);
  for j := r.start to r.finish do
  begin
    pisector := @isectors[j];
    for i := 0 to pisector.numsectors - 1 do
    begin
      sec := pisector.sectors[i];
      if sec.intrplcnt > 1 then
        R_RestoreInterpolationSector(sec);
    end;
  end;
  result := 0;
end;

//==============================================================================
//
// R_RestoreInterpolationData_thr3
//
//==============================================================================
function R_RestoreInterpolationData_thr3(p: pointer): integer; stdcall;
var
  i: integer;
  mo: Pmobj_t;
begin
  for i := 0 to numismobjs - 1 do
  begin
    mo := imobjs[i];
    if mo.intrplcnt > 1 then
      R_RestoreInterpolationMobj(mo);
  end;
  result := 0;
end;

//==============================================================================
//
// R_RestoreInterpolationData_Single_Thread
//
//==============================================================================
procedure R_RestoreInterpolationData_Single_Thread;
var
  i, j: integer;
  istruct: Pistruct_t;
  pisector: Psectorinterpolatearray_t;
  pi: Piitem_t;
  sec: Psector_t;
  mo: Pmobj_t;
begin
  for j := 0 to NUMISTRUCTS - 1 do
  begin
    istruct := @istructs[j];
    pi := @istruct.items[0];
    for i := 0 to istruct.numitems - 1 do
    begin
      case pi._type of
        iinteger: PInteger(pi.address)^ := pi.inext;
        iinteger2: PInteger(pi.address)^ := pi.inext2;
        ismallint: PSmallInt(pi.address)^ := pi.sinext;
        ibyte: PByte(pi.address)^ := pi.bnext;
        iangle: Pangle_t(pi.address)^ := pi.anext;
        ifloat: Pfloat(pi.address)^ := pi.fnext;
        ipolyrotate: R_RestoreInterpolationPolyRotate(Ppolyobj_t(pi.address));
        ipolymove: R_RestoreInterpolationPolyMove(Ppolyobj_t(pi.address));
      end;
      inc(pi);
    end;
  end;

  for j := 0 to NUM_SECTOR_INTERPOLATE_GROUPS - 1 do
  begin
    pisector := @isectors[j];
    for i := 0 to pisector.numsectors - 1 do
    begin
      sec := pisector.sectors[i];
      if sec.intrplcnt > 1 then
        R_RestoreInterpolationSector(sec);
    end;
  end;

  for i := 0 to numismobjs - 1 do
  begin
    mo := imobjs[i];
    if mo.intrplcnt > 1 then
      R_RestoreInterpolationMobj(mo);
  end;
end;

//==============================================================================
//
// R_RestoreInterpolationPolys
//
//==============================================================================
procedure R_RestoreInterpolationPolys;
var
  i: integer;
  pi: Piitem_t;
begin
  if not interpolatepolyobjs then
    exit;

  pi := @istructs[IDS_POLYS].items[0];
  for i := 0 to istructs[IDS_POLYS].numitems - 1 do
  begin
    case pi._type of
      ipolyrotate: R_RestoreInterpolationPolyRotate(Ppolyobj_t(pi.address));
      ipolymove: R_RestoreInterpolationPolyMove(Ppolyobj_t(pi.address));
    end;
    inc(pi);
  end;
end;

//==============================================================================
//
// R_TotalInterpolationItems
//
//==============================================================================
function R_TotalInterpolationItems: integer;
var
  i: integer;
begin
  result := numismobjs;
  for i := 0 to NUMISTRUCTS - 1 do
    result := result + istructs[i].numitems;
  for i := 0 to NUM_SECTOR_INTERPOLATE_GROUPS - 1 do
    result := result + isectors[i].numsectors;
end;

//==============================================================================
//
// R_RestoreInterpolationData
//
//==============================================================================
procedure R_RestoreInterpolationData;
var
  r1, r2, r3, r4: mt_range_t;
  cpus: integer;
begin
  cpus := I_GetNumCPUs;
  if usemultithread and (cpus >= 4) and (R_TotalInterpolationItems > 1024) then
  begin
    if cpus = 4 then
    begin
      r1.start := IDS_SECTOR1;
      r1.finish := IDS_SECTOR2;
      r2.start := IDS_SECTOR3;
      r2.finish := IDS_SECTOR4;
      MT_Execute4(
        @R_RestoreInterpolationData_thr1, nil,
        @R_RestoreInterpolationData_thr2, @r1,
        @R_RestoreInterpolationData_thr2, @r2,
        @R_RestoreInterpolationData_thr3, nil
      );
    end
    else
    begin
      r1.start := IDS_SECTOR1;
      r1.finish := IDS_SECTOR1;
      r2.start := IDS_SECTOR2;
      r2.finish := IDS_SECTOR2;
      r3.start := IDS_SECTOR3;
      r3.finish := IDS_SECTOR3;
      r4.start := IDS_SECTOR4;
      r4.finish := IDS_SECTOR4;
      MT_Execute6(
        @R_RestoreInterpolationData_thr1, nil,
        @R_RestoreInterpolationData_thr2, @r1,
        @R_RestoreInterpolationData_thr2, @r2,
        @R_RestoreInterpolationData_thr2, @r3,
        @R_RestoreInterpolationData_thr2, @r4,
        @R_RestoreInterpolationData_thr3, nil
      );
    end;
  end
  else
    R_RestoreInterpolationData_Single_Thread;
end;

//==============================================================================
//
// R_DoInterpolate_thr1
//
//==============================================================================
function R_DoInterpolate_thr1(p: pointer): integer; stdcall;
var
  i, j: integer;
  istruct: Pistruct_t;
  pi: Piitem_t;
begin
  for j := 0 to NUMISTRUCTS - 1 do
  begin
    istruct := @istructs[j];
    pi := @istruct.items[0];
    for i := 0 to istruct.numitems - 1 do
    begin
      if pi.address = pi.lastaddress then
      begin
        case pi._type of
          iinteger: R_InterpolationCalcI(pi);
          iinteger2: R_InterpolationCalcI2(pi);
          ismallint: R_InterpolationCalcSI(pi);
          ibyte: PByte(pi.address)^ := R_InterpolationCalcB(pi.bprev, pi.bnext);
          iangle: PAngle_t(pi.address)^ := R_InterpolationCalcA(pi.aprev, pi.anext);
          ifloat: R_InterpolationCalcF(pi);
          ipolyrotate: R_InterpolationCalcPolyRotation(pi);
          ipolymove: R_InterpolationCalcPolyMove(pi);
        end;
      end;
      inc(pi);
    end;
  end;
  result := 0;
end;

//==============================================================================
//
// R_DoInterpolate_thr2
//
//==============================================================================
function R_DoInterpolate_thr2(p: pointer): integer; stdcall;
var
  i, j: integer;
  sec: Psector_t;
  pisector: Psectorinterpolatearray_t;
  r: mt_range_p;
begin
  r := mt_range_p(p);
  for j := r.start to r.finish do
  begin
    pisector := @isectors[j];
    for i := 0 to pisector.numsectors - 1 do
    begin
      sec := pisector.sectors[i];
      if sec.intrplcnt > 1 then
        R_InterpolationCalcSector(sec);
    end;
  end;
  result := 0;
end;

//==============================================================================
//
// R_DoInterpolate_thr3
//
//==============================================================================
function R_DoInterpolate_thr3(p: pointer): integer; stdcall;
var
  i: integer;
  mo: Pmobj_t;
begin
  for i := 0 to numismobjs - 1 do
  begin
    mo := imobjs[i];
    if mo.intrplcnt > 1 then
      R_InterpolationCalcMobj(mo);
  end;
  result := 0;
end;

var
  frametime: integer = 0;

//==============================================================================
//
// R_Interpolate
//
//==============================================================================
function R_Interpolate: boolean;
var
  i, j: integer;
  istruct: Pistruct_t;
  pi: Piitem_t;
  fractime: fixed_t;
  sec: Psector_t;
  pisector: Psectorinterpolatearray_t;
  mo: Pmobj_t;
  r1, r2, r3, r4: mt_range_t;
  cpus: integer;
begin
  if skipinterpolationticks >= 0 then
  begin
    result := false;
    exit;
  end;

  fractime := I_GetFracTime;
  ticfrac := fractime - interpolationstarttime;
  if interpolateprecise then
    ticfrac := ticfrac + frametime;
  frametime := fractime;
  {$IFDEF DEBUG}
  I_Warning('R_Interpolate(): fractime = %5.3f, gametic = %d'#13#10, [fractime / FRACUNIT, gametic]);
  {$ENDIF}
  if interpolatereducelag then
    ticfrac := FRACUNIT div 2 + ticfrac div 2;
  if ticfrac > FRACUNIT then
  begin
  // JVAL
  // frac > FRACUNIT should rarelly happen,
  // we don't calc, we just use the Xnext values for interpolation frame
    {$IFDEF DEBUG}
    I_Warning('R_Interpolate(): ticfrac > FRACUNIT (%d), interpolationcount = %d'#13#10, [ticfrac, interpolationcount]);
    {$ENDIF}
    result := false;
  end
  else
  begin
    result := true;

    ticfrac_f := ticfrac / FRACUNIT;

    cpus := I_GetNumCPUs;
    if usemultithread and (cpus >= 4) and (R_TotalInterpolationItems > 1024) then
    begin
      if cpus = 4 then
      begin
        r1.start := IDS_SECTOR1;
        r1.finish := IDS_SECTOR2;
        r2.start := IDS_SECTOR3;
        r2.finish := IDS_SECTOR4;
        MT_Execute4(
          @R_DoInterpolate_thr1, nil,
          @R_DoInterpolate_thr2, @r1,
          @R_DoInterpolate_thr2, @r2,
          @R_DoInterpolate_thr3, nil
        );
      end
      else
      begin
        r1.start := IDS_SECTOR1;
        r1.finish := IDS_SECTOR1;
        r2.start := IDS_SECTOR2;
        r2.finish := IDS_SECTOR2;
        r3.start := IDS_SECTOR3;
        r3.finish := IDS_SECTOR3;
        r4.start := IDS_SECTOR4;
        r4.finish := IDS_SECTOR4;
        MT_Execute6(
          @R_DoInterpolate_thr1, nil,
          @R_DoInterpolate_thr2, @r1,
          @R_DoInterpolate_thr2, @r2,
          @R_DoInterpolate_thr2, @r3,
          @R_DoInterpolate_thr2, @r4,
          @R_DoInterpolate_thr3, nil
        );
      end;
    end
    else
    begin
      for j := 0 to NUMISTRUCTS - 1 do
      begin
        istruct := @istructs[j];
        pi := @istruct.items[0];
        for i := 0 to istruct.numitems - 1 do
        begin
          if pi.address = pi.lastaddress then
          begin
            case pi._type of
              iinteger: R_InterpolationCalcI(pi);
              iinteger2: R_InterpolationCalcI2(pi);
              ismallint: R_InterpolationCalcSI(pi);
              ibyte: PByte(pi.address)^ := R_InterpolationCalcB(pi.bprev, pi.bnext);
              iangle: PAngle_t(pi.address)^ := R_InterpolationCalcA(pi.aprev, pi.anext);
              ifloat: R_InterpolationCalcF(pi);
              ipolyrotate: R_InterpolationCalcPolyRotation(pi);
            end;
          end;
          inc(pi);
        end;
      end;
      for j := 0 to NUM_SECTOR_INTERPOLATE_GROUPS - 1 do
      begin
        pisector := @isectors[j];
        for i := 0 to pisector.numsectors - 1 do
        begin
          sec := pisector.sectors[i];
          if sec.intrplcnt > 1 then
            R_InterpolationCalcSector(sec);
        end;
      end;
      for i := 0 to numismobjs - 1 do
      begin
        mo := imobjs[i];
        if mo.intrplcnt > 1 then
          R_InterpolationCalcMobj(mo);
      end;
    end;

  end;
  frametime := I_GetFracTime - frametime;
end;

//==============================================================================
//
// R_InterpolateTicker
//
//==============================================================================
procedure R_InterpolateTicker;
begin
  if skipinterpolationticks >= 0 then
    dec(skipinterpolationticks);
end;

//==============================================================================
//
// R_SetInterpolateSkipTicks
//
//==============================================================================
procedure R_SetInterpolateSkipTicks(const ticks: integer);
begin
  skipinterpolationticks := ticks;
  frametime := 0;
end;

end.

