//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2018 by Jim Valavanis
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
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_intrpl;

// JVAL
// Frame interpolation to exceed the 35fps limit
//

interface

uses
    m_fixed;
    
procedure R_InitInterpolations;

procedure R_ResetInterpolationBuffer;

procedure R_StoreInterpolationData(const tick: integer; const counts: integer);

procedure R_RestoreInterpolationData;

function R_Interpolate: boolean;

procedure R_InterpolateTicker;

procedure R_SetInterpolateSkipTicks(const ticks: integer);

var
  interpolate: boolean;
  didinterpolations: boolean;
  ticfrac: fixed_t;

implementation

uses
  d_delphi,
  d_player,
  d_think,
  c_cmds,
  m_misc,
  g_game,
  mt_utils,
  i_system,
  p_setup,
  p_tick,
  p_mobj,
  p_mobj_h,
  p_pspr_h,
  r_defs,
  tables;

type
  itype = (iinteger, ismallint, ibyte, iangle, imobj);

  // Interpolation item
  //  Holds information about the previous and next values and interpolation type
  iitem_t = record
    lastaddress: pointer;
    address: pointer;
    case _type: itype of
      iinteger: (iprev, inext: integer);
      ismallint: (siprev, sinext: smallint);
      ibyte: (bprev, bnext: byte);
      iangle: (aprev, anext: LongWord);
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

const
  IGROWSTEP = 256;

var
  istruct: istruct_t;
  imobjs: array[0..$FFFF] of Pmobj_t;
  numismobjs: integer;

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

procedure R_InitInterpolations;
begin
  istruct.numitems := 0;
  istruct.realsize := 0;
  istruct.items := nil;
  MT_ZeroMemory(@imobjs, SizeOf(imobjs));
  numismobjs := 0;
  C_AddCmd('interpolate, interpolation, setinterpolation, r_interpolate', @CmdInterpolate);
end;

procedure R_ResetInterpolationBuffer;
begin
  memfree(pointer(istruct.items), istruct.realsize * SizeOf(iitem_t));
  istruct.numitems := 0;
  istruct.realsize := 0;
  numismobjs := 0;
end;

function R_InterpolationCalcIF(const prev, next: fixed_t; const frac: fixed_t): fixed_t;
begin
  if next = prev then
    result := prev
  else
    result := prev + round((next - prev) / FRACUNIT * frac);
end;

function R_InterpolationCalcSIF(const prev, next: smallint; const frac: fixed_t): smallint;
begin
  if next = prev then
    result := prev
  else
    result := prev + round((next - prev) / FRACUNIT * frac);
end;

procedure R_InterpolationCalcI(const pi: Piitem_t; const frac: fixed_t);
begin
  if pi.inext = pi.iprev then
    exit;

  PInteger(pi.address)^ := pi.iprev + round((pi.inext - pi.iprev) / FRACUNIT * frac);
end;

procedure R_InterpolationCalcSI(const pi: Piitem_t; const frac: fixed_t);
begin
  if pi.sinext = pi.siprev then
    exit;

  PSmallInt(pi.address)^ := pi.siprev + round((pi.sinext - pi.siprev) / FRACUNIT * frac);
end;

function R_InterpolationCalcB(const prev, next: byte; const frac: fixed_t): byte;
begin
  if next = prev then
    result := prev
  else if (next = 0) or (prev = 0) then // Hack for player.lookdir2
    result := next
  else if ((next > 247) and (prev < 8)) or ((next < 8) and (prev > 247)) then // Hack for player.lookdir2
    result := 0
  else
    result := prev + (next - prev) * frac div FRACUNIT;
end;

function R_InterpolationCalcA(const prev, next: angle_t; const frac: fixed_t): angle_t;
var
  prev_e, next_e, mid_e: Extended;
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

      mid_e := prev_e + (next_e - prev_e) / FRACUNIT * frac;
      if mid_e > 1.0 then
        mid_e := mid_e - 1.0;
      result := Round(mid_e * ANGLE_MAX);
    end
    else if prev > next then
    begin
      result := prev - round((prev - next) / FRACUNIT * frac);
    end
    else
    begin
      result := prev + round((next - prev) / FRACUNIT * frac);
    end;
  end;
end;

procedure R_AddInterpolationItem(const addr: pointer; const typ: itype);
var
  newrealsize: integer;
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
  end;
  inc(istruct.numitems);
end;

var
  interpolationstoretime: fixed_t = 0;
  interpolationcount: integer = 0;
  prevtic: fixed_t = 0;

// JVAL: Skip interpolation if we have teleport
var
  skipinterpolationticks: integer = -1;

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
  {$ENDIF}
  prevtic := gametic;
  interpolationstoretime := tick * FRACUNIT;
  interpolationcount := counts;
  istruct.numitems := 0;
  numismobjs := 0;

  // Interpolate player
  player := @players[displayplayer];
  if player <> nil then
  begin
    R_AddInterpolationItem(@player.lookdir, iinteger);
    R_AddInterpolationItem(@player.lookdir16, iinteger); // JVAL Smooth Look Up/Down
    R_AddInterpolationItem(@player.lookdir2, ibyte);
    R_AddInterpolationItem(@player.viewz, iinteger);
    R_AddInterpolationItem(@player.teleporttics, iinteger);
    R_AddInterpolationItem(@player.quaketics, iinteger);
  end;

  // Interpolate Sectors
  sec := @sectors[0];
  for i := 0 to numsectors - 1 do
  begin
    if sec.renderflags and SRF_NO_INTERPOLATE = 0 then
    begin
      R_AddInterpolationItem(@sec.floorheight, iinteger);
      R_AddInterpolationItem(@sec.ceilingheight, iinteger);
      R_AddInterpolationItem(@sec.lightlevel, ismallint);
      {$IFDEF DOOM_OF_STRIFE}
      // JVAL: 30/9/2009
      // JVAL: 9/11/2015 Added strife offsets
      R_AddInterpolationItem(@sec.floor_xoffs, iinteger);
      R_AddInterpolationItem(@sec.floor_yoffs, iinteger);
      R_AddInterpolationItem(@sec.ceiling_xoffs, iinteger);
      R_AddInterpolationItem(@sec.ceiling_yoffs, iinteger);
      {$ENDIF}
    end;
    inc(sec);
  end;

  if player <> nil then
  begin
    for i := 0 to Ord(NUMPSPRITES) - 1 do
    begin
      R_AddInterpolationItem(@player.psprites[i].sx, iinteger);
      R_AddInterpolationItem(@player.psprites[i].sy, iinteger);
    end;
  end;

  // Interpolate Lines
  li := @lines[0];
  for i := 0 to numlines - 1 do
  begin
    if li.special <> 0 then
      for j := 0 to 1 do
      begin
        if li.sidenum[j] > -1 then
        begin
          si := @sides[li.sidenum[j]];
          R_AddInterpolationItem(@si.textureoffset, iinteger);
          R_AddInterpolationItem(@si.rowoffset, iinteger);
        end;
      end;
    inc(li);
  end;

  // Map Objects
  th := thinkercap.next;
  while (th <> nil) and (th <> @thinkercap) do
  begin
    if @th._function.acp1 = @P_MobjThinker then
      R_AddInterpolationItem(th, imobj);
    th := th.next;
  end;
  {$IFDEF DEBUG}
  I_DevWarning('R_StoreInterpolationData(): - finished - fractime = %5.3f, gametic = %d'#13#10, [I_GetFracTime / FRACUNIT, gametic]);
  {$ENDIF}
end;

function R_RestoreInterpolationData_thr1(p: pointer): integer; stdcall;
var
  i: integer;
  pi: Piitem_t;
  r: mt_range_p;
begin
  r := mt_range_p(p);
  pi := @istruct.items[r.start];
  for i := r.start to r.finish do
  begin
    case pi._type of
      iinteger: PInteger(pi.address)^ := pi.inext;
      ismallint: PSmallInt(pi.address)^ := pi.sinext;
      ibyte: PByte(pi.address)^ := pi.bnext;
      iangle: Pangle_t(pi.address)^ := pi.anext;
    end;
    inc(pi);
  end;
  result := 0;
end;

function R_RestoreInterpolationData_thr2(p: pointer): integer; stdcall;
var
  i: integer;
  mo: Pmobj_t;
begin
  for i := 0 to numismobjs - 1 do
  begin
    mo := imobjs[i];
    mo.x := mo.nextx;
    mo.y := mo.nexty;
    mo.z := mo.nextz;
    mo.angle := mo.nextangle;
  end;
  result := 0;
end;

procedure R_RestoreInterpolationData_Single_Thread;
var
  i: integer;
  pi: Piitem_t;
  mo: Pmobj_t;
begin
  pi := @istruct.items[0];
  for i := 0 to istruct.numitems - 1 do
  begin
    case pi._type of
      iinteger: PInteger(pi.address)^ := pi.inext;
      ismallint: PSmallInt(pi.address)^ := pi.sinext;
      ibyte: PByte(pi.address)^ := pi.bnext;
      iangle: Pangle_t(pi.address)^ := pi.anext;
    end;
    inc(pi);
  end;

  for i := 0 to numismobjs - 1 do
  begin
    mo := imobjs[i];
    mo.x := mo.nextx;
    mo.y := mo.nexty;
    mo.z := mo.nextz;
    mo.angle := mo.nextangle;
  end;
end;

procedure R_RestoreInterpolationData;
var
  r1, r2, r3: mt_range_t;
begin
  if usemultithread and (I_GetNumCPUs >=4) and (istruct.numitems + numismobjs >= 4096) then
  begin
    r1.start := 0;
    r1.finish := istruct.numitems div 3;
    r2.start := r1.finish + 1;
    r2.finish := r1.finish * 2;
    r3.start := r2.finish + 1;
    r3.finish := istruct.numitems - 1;
    MT_Execute(
      @R_RestoreInterpolationData_thr1, @r1,
      @R_RestoreInterpolationData_thr1, @r2,
      @R_RestoreInterpolationData_thr1, @r3,
      @R_RestoreInterpolationData_thr2, nil
    );
  end
  else
    R_RestoreInterpolationData_Single_Thread;
end;

function R_DoInterpolate_thr1(p: pointer): integer; stdcall;
var
  pi, pi2: Piitem_t;
begin
  pi := @istruct.items[mt_range_p(p).start];
  pi2 := @istruct.items[mt_range_p(p).finish];
  while integer(pi) <= integer(pi2) do
  begin
    if pi.address = pi.lastaddress then
    begin
      case pi._type of
        iinteger: R_InterpolationCalcI(pi, ticfrac);
        ismallint: R_InterpolationCalcSI(pi, ticfrac);
        ibyte: PByte(pi.address)^ := R_InterpolationCalcB(pi.bprev, pi.bnext, ticfrac);
        iangle: PAngle_t(pi.address)^ := R_InterpolationCalcA(pi.aprev, pi.anext, ticfrac);
      end;
    end;
    inc(pi);
  end;
  result := 0;
end;

function R_DoInterpolate_thr2(p: pointer): integer; stdcall;
var
  i: integer;
  mo: Pmobj_t;
begin
  for i := 0 to numismobjs - 1 do
  begin
    mo := imobjs[i];
    if mo.intrplcnt > 1 then
    begin
      mo.x := R_InterpolationCalcIF(mo.prevx, mo.nextx, ticfrac);
      mo.y := R_InterpolationCalcIF(mo.prevy, mo.nexty, ticfrac);
      mo.z := R_InterpolationCalcIF(mo.prevz, mo.nextz, ticfrac);
      mo.angle := R_InterpolationCalcA(mo.prevangle, mo.nextangle, ticfrac);
    end;
  end;
  result := 0;
end;

function R_Interpolate: boolean;
var
  i: integer;
  pi: Piitem_t;
  fractime: fixed_t;
  mo: Pmobj_t;
  r1, r2, r3: mt_range_t;
begin
  if skipinterpolationticks >= 0 then
  begin
    result := false;
    exit;
  end;

  fractime := I_GetFracTime;
  ticfrac := fractime - interpolationstoretime;
  ticfrac := round(ticfrac / interpolationcount);
  {$IFDEF DEBUG}
  I_DevWarning('R_Interpolate(): fractime = %5.3f, gametic = %d'#13#10, [fractime / FRACUNIT, gametic]);
  {$ENDIF}
  if ticfrac > FRACUNIT then
  begin
  // JVAL
  // frac > FRACUNIT should rarelly happen,
  // we don't calc, we just use the Xnext values for interpolation frame
    {$IFDEF DEBUG}
    I_DevWarning('R_Interpolate(): ticfrac > FRACUNIT (%d), interpolationcount = %d'#13#10, [ticfrac, interpolationcount]);
    {$ENDIF}
    result := false;
  end
  else
  begin
    result := true;
    if usemultithread and (I_GetNumCPUs >=4) and (istruct.numitems + numismobjs >= 4096) then
    begin
      r1.start := 0;
      r1.finish := istruct.numitems div 3;
      r2.start := r1.finish + 1;
      r2.finish := r1.finish * 2;
      r3.start := r2.finish + 1;
      r3.finish := istruct.numitems - 1;
      MT_Execute(
        @R_DoInterpolate_thr1, @r1,
        @R_DoInterpolate_thr1, @r2,
        @R_DoInterpolate_thr1, @r3,
        @R_DoInterpolate_thr2, nil
      );
    end
    else
    begin
      pi := @istruct.items[0];
      for i := 0 to istruct.numitems - 1 do
      begin
        if pi.address = pi.lastaddress then
        begin
          case pi._type of
            iinteger: R_InterpolationCalcI(pi, ticfrac);
            ismallint: R_InterpolationCalcSI(pi, ticfrac);
            ibyte: PByte(pi.address)^ := R_InterpolationCalcB(pi.bprev, pi.bnext, ticfrac);
            iangle: PAngle_t(pi.address)^ := R_InterpolationCalcA(pi.aprev, pi.anext, ticfrac);
          end;
        end;
        inc(pi);
      end;
      for i := 0 to numismobjs - 1 do
      begin
        mo := imobjs[i];
        if mo.intrplcnt > 1 then
        begin
          mo.x := R_InterpolationCalcIF(mo.prevx, mo.nextx, ticfrac);
          mo.y := R_InterpolationCalcIF(mo.prevy, mo.nexty, ticfrac);
          mo.z := R_InterpolationCalcIF(mo.prevz, mo.nextz, ticfrac);
          mo.angle := R_InterpolationCalcA(mo.prevangle, mo.nextangle, ticfrac);
        end;
      end;
    end;

  end;
end;

procedure R_InterpolateTicker;
begin
  if skipinterpolationticks >= 0 then
    dec(skipinterpolationticks);
end;

procedure R_SetInterpolateSkipTicks(const ticks: integer);
begin
  skipinterpolationticks := ticks;
end;

end.

