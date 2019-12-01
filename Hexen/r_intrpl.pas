//------------------------------------------------------------------------------
//
//  DelphiHexen: A modified and improved Hexen port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
//  Copyright (C) 2004-2012 by Jim Valavanis
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
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://delphidoom.sitesled.com/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_intrpl;

interface

uses
  m_fixed;

procedure R_InitInterpolations;

procedure R_ResetInterpolationBuffer;

procedure R_StoreInterpolationData(const tick: integer);

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
  g_game,
  i_system,
  p_setup,
  p_tick,
  p_mobj,
  p_mobj_h,
  p_pspr_h,
  r_defs,
  r_sky,
  tables;

type
  itype = (iinteger, ismallint, ibyte, iangle);

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

procedure R_InitInterpolations;
begin
  istruct.numitems := 0;
  istruct.realsize := 0;
  istruct.items := nil;
end;

procedure R_ResetInterpolationBuffer;
begin
  memfree(pointer(istruct.items), istruct.realsize * SizeOf(iitem_t));
  istruct.numitems := 0;
  istruct.realsize := 0;
end;

function R_InterpolationCalcI(const prev, next: fixed_t; const frac: fixed_t): fixed_t;
begin
  if next = prev then
    result := next
  else
    result := prev + round((next - prev) / FRACUNIT * frac);
end;

function R_InterpolationCalcSI(const prev, next: smallint; const frac: fixed_t): smallint;
begin
  if next = prev then
    result := next
  else
    result := prev + round((next - prev) / FRACUNIT * frac);
end;

function R_InterpolationCalcB(const prev, next: byte; const frac: fixed_t): byte;
begin
  if next = prev then
    result := next
  else if (next = 0) or (prev = 0) then // Hack for player.lookdir2
    result := next
  else if ((next > 247) and (prev < 8)) or ((next < 8) and (prev > 247)) then // Hack for player.lookdir2
    result := 0
  else
    result := prev + (next - prev) * frac div FRACUNIT;
end;

function R_InterpolationCalcA(const prev, next: angle_t; const frac: fixed_t): angle_t;
begin
  if prev = next then
    result := next
  else
  begin
    if ((prev < ANG90) and (next > ANG270)) or
       ((next < ANG90) and (prev > ANG270)) then
    begin
      if frac < FRACUNIT div 4 then
        result := prev
      else if frac > FRACUNIT * 3 div 4 then
        result := next
      else
        result := 0;
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
  if istruct.realsize <= istruct.numitems then
  begin
    newrealsize := istruct.realsize + IGROWSTEP;
    realloc(pointer(istruct.items), istruct.realsize * SizeOf(iitem_t), newrealsize * SizeOf(iitem_t));
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
  interpolationstoretime: fixed_t;

procedure R_StoreInterpolationData(const tick: integer);
var
  sec: Psector_t;
  li: Pline_t;
  si: PSide_t;
  i, j: integer;
  player: Pplayer_t;
  th: Pthinker_t;
begin
  interpolationstoretime := tick * FRACUNIT;
  istruct.numitems := 0;

  // Interpolate player
  player := @players[displayplayer];
  if player <> nil then
  begin
    R_AddInterpolationItem(@player.lookdir, iinteger);
    R_AddInterpolationItem(@player.lookdir2, ibyte);
    R_AddInterpolationItem(@player.viewz, iinteger);
    for i := 0 to Ord(NUMPSPRITES) - 1 do
    begin
      R_AddInterpolationItem(@player.psprites[i].sx, iinteger);
      R_AddInterpolationItem(@player.psprites[i].sy, iinteger);
    end;
  end;

//  R_AddInterpolationItem(@Sky1ColumnOffset, iinteger);
//  R_AddInterpolationItem(@Sky2ColumnOffset, iinteger);
  // Interpolate Sectors
  sec := @sectors[0];
  for i := 0 to numsectors - 1 do
  begin
    R_AddInterpolationItem(@sec.floorheight, iinteger);
    R_AddInterpolationItem(@sec.ceilingheight, iinteger);
    R_AddInterpolationItem(@sec.lightlevel, ismallint);
    inc(sec);
  end;

  // Interpolate Lines
  li := @lines[0];                  
  for i := 0 to numlines - 1 do
  begin
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
    begin
      if Pmobj_t(th).flags and MF_JUSTAPPEARED = 0 then  // JVAL Remove ?
      begin
        R_AddInterpolationItem(@Pmobj_t(th).x, iinteger);
        R_AddInterpolationItem(@Pmobj_t(th).y, iinteger);
        R_AddInterpolationItem(@Pmobj_t(th).z, iinteger);
        R_AddInterpolationItem(@Pmobj_t(th).angle, iangle);
      end
    end;
    th := th.next;
  end;

end;

procedure R_RestoreInterpolationData;
var
  i: integer;
  pi: Piitem_t;
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
end;

// JVAL: Skip interpolation if we have teleport
var
  skipinterpolationticks: integer = -1;

function R_Interpolate: boolean;
var
  i: integer;
  pi: Piitem_t;
  fractime: fixed_t;
begin
  if skipinterpolationticks >= 0 then
  begin
    result := false;
    exit;
  end;

  fractime := I_GetFracTime;
  ticfrac := fractime - interpolationstoretime;
  pi := @istruct.items[0];
  if ticfrac > FRACUNIT then
  begin
  // JVAL
  // frac > FRACUNIT should rarelly happen,
  // we don't calc, we just use the Xnext values for interpolation frame
    result := false;
{    for i := 0 to istruct.numitems - 1 do
    begin
      if pi.address = pi.lastaddress then
      begin
        case pi._type of
          iinteger: PInteger(pi.address)^ := pi.inext;
          ismallint: PSmallInt(pi.address)^ := pi.sinext;
          ibyte: PByte(pi.address)^ := pi.bnext;
          iangle: Pangle_t(pi.address)^ := pi.anext;
        end;
      end;
      inc(pi);
    end;}
  end
  else
  begin
    result := true;
    for i := 0 to istruct.numitems - 1 do
    begin
      if pi.address = pi.lastaddress then
      begin
        case pi._type of
          iinteger: PInteger(pi.address)^ := R_InterpolationCalcI(pi.iprev, pi.inext, ticfrac);
          ismallint: PSmallInt(pi.address)^ := R_InterpolationCalcSI(pi.siprev, pi.sinext, ticfrac);
          ibyte: PByte(pi.address)^ := R_InterpolationCalcB(pi.bprev, pi.bnext, ticfrac);
          iangle: PAngle_t(pi.address)^ := R_InterpolationCalcA(pi.aprev, pi.anext, ticfrac);
        end;
      end;
      inc(pi);
    end;
  end;
end;

procedure R_InterpolateTicker;
begin
  if skipinterpolationticks >= 0 then
    dec(skipinterpolationticks)
end;

procedure R_SetInterpolateSkipTicks(const ticks: integer);
begin
  skipinterpolationticks := ticks;
end;

end.
