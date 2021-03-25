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
//  Pascal Script RTL - Overlay Drawing
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit psi_overlay;

interface

uses
  d_delphi,
  doomdef,
  r_defs,
  p_mobj_h,
  ps_compiler,
  ps_runtime;

const
  OVERLAYWIDTH = 320;
  OVERLAYHEIGHT = 200;
  OVERLAYSIZE = OVERLAYWIDTH * OVERLAYHEIGHT;

type
  overlaycast_t = packed array[0..OVERLAYHEIGHT - 1, 0..OVERLAYWIDTH - 1] of byte;
  overlaycast_p = ^overlaycast_t;

type
  overlayindexes_t = array[0..OVERLAYSIZE - 1] of Integer;
  overlayindexes_p = ^overlayindexes_t;

type
  overlaydrawer_t = record
    proc: Integer;
    tick: Integer;
    sparam: string[255];
    xparam: Integer;
    yparam: Integer;
    iparam1: Integer;
    iparam2: Integer;
    iparam3: Integer;
    xparam2: Integer;
    yparam2: Integer;
  end;
  Poverlaydrawer_t = ^overlaydrawer_t;
  overlaydrawer_tArray = array[0..$FFF] of overlaydrawer_t;
  Poverlaydrawer_tArray = ^overlaydrawer_tArray;

const
  OVR_ALIGN_LEFT = 0;
  OVR_ALIGN_RIGHT = 1;
  OVR_ALIGN_CENTER = 2;

type
  overlaylookup_t = array[0..MAXWIDTH * MAXHEIGHT - 1] of PByte;
  overlaylookup_p = ^overlaylookup_t;

type
  TOverlayDrawer = class(TObject)
  private
    foverlayscreen: PByteArray;
    fbackbuffer: PByteArray;
    foverlaylookup: overlaylookup_p;
    foverlaylookupsize: integer;
    ffirstoverlaylookup: overlayindexes_t;
    flastoverlaylookup: overlayindexes_t;
    fdrawers: Poverlaydrawer_tArray;
    fnumdrawers: Integer;
    frealnumdrawers: Integer;
    fmodified: Boolean;
    lastdrawcnt: Integer;
    fstart, fend: Integer;
  protected
    procedure ClearScreen; virtual;
    procedure Grow; virtual;
    procedure DrawPatch(const x, y: Integer; const patchlump: Integer); overload; virtual;
    procedure DrawPatch(const x, y: Integer; const patch: Ppatch_t); overload; virtual;
    procedure DrawPatchStretched(const x1, y1, x2, y2: Integer; const patchlump: Integer); overload; virtual;
    procedure DrawPatchStretched(const x1, y1, x2, y2: Integer; const patch: Ppatch_t); overload; virtual;
    procedure DrawPixel(const x, y: Integer; const red, green, blue: byte); virtual;
    procedure DrawRect(const x1, y1, x2, y2: Integer; const red, green, blue: byte); virtual;
    procedure DrawLine(const x1, y1, x2, y2: Integer; const red, green, blue: byte); virtual;
    procedure DrawText(const txt: string; const align: Integer;
      const x, y: Integer);
    procedure DrawDrawer(const i: Integer); virtual;
    procedure NotifyDrawSize(const astart, aend: Integer);
    function BufferPosition(const x, y: Integer): Integer;
    procedure CalcOverlayLookUp;
    procedure DrawDrawers; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SaveToBuffer(var buff: pointer); virtual;
    procedure LoadFromBuffer(var buff: pointer); virtual;
    function SaveSize: Integer; virtual;
    procedure Clear; virtual;
    procedure AddPatch(const ticks: Integer; const patchname: string;
      const x, y: Integer);
    procedure AddPatchStretched(const ticks: Integer; const patchname: string;
      const x1, y1, x2, y2: Integer);
    procedure AddPixel(const ticks: Integer; const red, green, blue: byte;
      const x, y: Integer);
    procedure AddRect(const ticks: Integer; const red, green, blue: byte;
      const x1, y1, x2, y2: Integer);
    procedure AddLine(const ticks: Integer; const red, green, blue: byte;
      const x1, y1, x2, y2: Integer);
    procedure AddText(const ticks: Integer; const txt: string; const align: Integer;
      const x, y: Integer);
    procedure AddLeftText(const ticks: Integer; const txt: string;
      const x, y: Integer);
    procedure AddRightText(const ticks: Integer; const txt: string;
      const x, y: Integer);
    procedure AddCenterText(const ticks: Integer; const txt: string;
      const x, y: Integer);
    {$IFNDEF OPENGL}
    procedure FlashToScreen8; virtual;
    {$ENDIF}
    procedure FlashToScreen32; virtual;
    {$IFDEF OPENGL}
    function GetOverlayHeight: Integer;
    {$ENDIF}
    procedure ReCalcOverlayLookUp;
    property Modified: Boolean read fmodified;
    property overlayscreen: PByteArray read foverlayscreen;
    property overlaylookup: overlaylookup_p read foverlaylookup;
    property firstoverlaylookup: overlayindexes_t read ffirstoverlaylookup;
    property lastoverlaylookup: overlayindexes_t read flastoverlaylookup;
    property drawers: Poverlaydrawer_tArray read fdrawers;
    property numdrawers: Integer read fnumdrawers;
  end;

// ----------------- OVERLAY FUNCTIONS -----------------------------------------

procedure PS_OverlayClear;

procedure PS_OverlayDrawPatch(const ticks: Integer; const patchname: string;
  const x, y: Integer);

procedure PS_OverlayDrawPatchStretched(const ticks: Integer; const patchname: string;
  const x1, y1, x2, y2: Integer);

procedure PS_OverlayDrawPixel(const ticks: Integer; const red, green, blue: byte;
  const x, y: Integer);

procedure PS_OverlayDrawRect(const ticks: Integer; const red, green, blue: byte;
  const x1, y1, x2, y2: Integer);

procedure PS_OverlayDrawLine(const ticks: Integer; const red, green, blue: byte;
  const x1, y1, x2, y2: Integer);

procedure PS_OverlayDrawText(const ticks: Integer; const txt: string; const align: Integer;
  const x, y: Integer);

procedure PS_OverlayDrawLeftText(const ticks: Integer; const txt: string;
  const x, y: Integer);

procedure PS_OverlayDrawRightText(const ticks: Integer; const txt: string;
  const x, y: Integer);

procedure PS_OverlayDrawCenterText(const ticks: Integer; const txt: string;
  const x, y: Integer);

// -------------------- INITIALIZATION -----------------------------------------
procedure PS_InitOverlay;

// --------------------- FINALIZATION -----------------------------------------
procedure PS_ShutDownOverlay;

procedure OVR_Drawer;

function OVR_IsModified: Boolean;

{$IFDEF OPENGL}
function OVR_OverlayHeight: Integer;
{$ENDIF}

var
  overlay: TOverlayDrawer;

//------------ Register Overlay to PascalScript --------------------------------
procedure SIRegister_TOverlay(CL: TPSPascalCompiler);

procedure RIRegister_TOverlay(CL: TPSRuntimeClassImporter);

procedure RIRegisterRTL_TOverlay(Exec: TPSExec);

//------------ Mobj Codepointers -----------------------------------------------
procedure A_OverlayClear;

procedure A_OverlayDrawPatch(actor: Pmobj_t);

procedure A_OverlayDrawPatchStretched(actor: Pmobj_t);

procedure A_OverlayDrawPixel(actor: Pmobj_t);

procedure A_OverlayDrawRect(actor: Pmobj_t);

procedure A_OverlayDrawLine(actor: Pmobj_t);

procedure A_OverlayDrawText(actor: Pmobj_t);

procedure A_OverlayDrawLeftText(actor: Pmobj_t);

procedure A_OverlayDrawRightText(actor: Pmobj_t);

procedure A_OverlayDrawCenterText(actor: Pmobj_t);

implementation

uses
  c_cmds,
  d_net,
  hu_stuff,
  m_fixed,
  i_system,
  mt_utils,
  p_common,
  p_tick,
  sc_engine,
  sc_params,
  {$IFNDEF OPENGL}
  r_hires,
  {$ENDIF}
  v_data,
  v_video,
  w_wad,
  z_zone;

const
  OVR_GROWSTEP = 8;

constructor TOverlayDrawer.Create;
begin
  inherited Create;
  foverlayscreen := malloc(OVERLAYSIZE * SizeOf(Byte));
  fbackbuffer := mallocz(OVERLAYSIZE * SizeOf(Byte));
  foverlaylookupsize := V_GetScreenWidth(SCN_FG) * V_GetScreenHeight(SCN_FG) * SizeOf(PByte);
  foverlaylookup := malloc(foverlaylookupsize);
  CalcOverlayLookUp;
  fdrawers := malloc(OVR_GROWSTEP * SizeOf(overlaydrawer_t));
  fnumdrawers := 0;
  frealnumdrawers := OVR_GROWSTEP;
  fstart := 0;
  fend := OVERLAYSIZE - 1;
  ClearScreen;
  fmodified := False;
  lastdrawcnt := 0;
end;

destructor TOverlayDrawer.Destroy;
begin
  memfree(Pointer(fdrawers), frealnumdrawers * SizeOf(overlaydrawer_t));
  memfree(Pointer(foverlaylookup), foverlaylookupsize);
  memfree(Pointer(foverlayscreen), OVERLAYSIZE * SizeOf(Byte));
  memfree(Pointer(fbackbuffer), OVERLAYSIZE * SizeOf(Byte));
  inherited;
end;

procedure TOverlayDrawer.CalcOverlayLookUp;
var
  x, y: Integer;
  idx: Integer;
  w, h: Integer;
  tw, th: Integer;
  apos: Integer;
begin
  MT_memseti(@ffirstoverlaylookup, MAXINT, OVERLAYSIZE);
  MT_memseti(@flastoverlaylookup, 0, OVERLAYSIZE);
  idx := 0;
  w := V_GetScreenWidth(SCN_FG);
  h := V_GetScreenHeight(SCN_FG);
  for y := 0 to h - 1 do
  begin
    th := Trunc(y / h * OVERLAYHEIGHT);
    if th >= OVERLAYHEIGHT then
      th := OVERLAYHEIGHT - 1
    else if th < 0 then
      th := 0;
    for x := 0 to w - 1 do
    begin
      tw := Trunc(x / w * OVERLAYWIDTH);
      if tw >= OVERLAYWIDTH then
        tw := OVERLAYWIDTH - 1
      else if tw < 0 then
        tw := 0;
      apos := BufferPosition(tw, th);
      foverlaylookup[idx] := @foverlayscreen[apos];
      if ffirstoverlaylookup[apos] > idx then
        ffirstoverlaylookup[apos] := idx;
      if flastoverlaylookup[apos] < idx then
        flastoverlaylookup[apos] := idx;
      Inc(idx);
    end;
  end;
end;

procedure TOverlayDrawer.ReCalcOverlayLookUp;
begin
  memfree(Pointer(foverlaylookup), foverlaylookupsize);
  foverlaylookupsize := V_GetScreenWidth(SCN_FG) * V_GetScreenHeight(SCN_FG) * SizeOf(PByte);
  foverlaylookup := malloc(foverlaylookupsize);
  CalcOverlayLookUp;
end;

procedure TOverlayDrawer.SaveToBuffer(var buff: pointer);
var
  sz: Integer;
begin
  PInteger(buff)^ := fnumdrawers;
  incp(buff, SizeOf(Integer));
  sz := fnumdrawers * SizeOf(overlaydrawer_t);
  MT_memcpy(buff, @fdrawers[0], sz);
  incp(buff, sz);
end;

procedure TOverlayDrawer.LoadFromBuffer(var buff: pointer);
var
  sz: Integer;
begin
  fnumdrawers := PInteger(buff)^;
  incp(buff, SizeOf(Integer));
  sz := fnumdrawers * SizeOf(overlaydrawer_t);
  realloc(pointer(fdrawers), frealnumdrawers * SizeOf(overlaydrawer_t), sz);
  frealnumdrawers := fnumdrawers;
  MT_memcpy(@fdrawers[0], buff, sz);
  incp(buff, sz);
  ClearScreen;
end;

function TOverlayDrawer.SaveSize: Integer;
begin
  Result := SizeOf(Integer) + fnumdrawers * SizeOf(overlaydrawer_t);
end;

procedure TOverlayDrawer.Clear;
begin
  fnumdrawers := 0;
  realloc(pointer(fdrawers), frealnumdrawers * SizeOf(overlaydrawer_t), OVR_GROWSTEP * SizeOf(overlaydrawer_t));
  frealnumdrawers := OVR_GROWSTEP;
  ClearScreen;
end;

procedure TOverlayDrawer.ClearScreen;
begin
  if fstart <= fend then
    MT_ZeroMemory(@foverlayscreen[fstart], fend - fstart + 1, 2);
  fstart := OVERLAYSIZE;
  fend := -1;
end;

procedure TOverlayDrawer.Grow;
begin
  if fnumdrawers = frealnumdrawers then
  begin
    realloc(pointer(fdrawers), frealnumdrawers * SizeOf(overlaydrawer_t),
      (frealnumdrawers + OVR_GROWSTEP) * SizeOf(overlaydrawer_t));
    frealnumdrawers := frealnumdrawers + OVR_GROWSTEP;
  end;
end;

procedure TOverlayDrawer.DrawPatch(const x, y: Integer; const patchlump: Integer);
var
  patch: Ppatch_t;
begin
  if patchlump < 0 then
    Exit;

  patch := W_CacheLumpNum(patchlump, PU_STATIC);
  DrawPatch(x, y, patch);
end;

procedure TOverlayDrawer.DrawPatch(const x, y: Integer; const patch: Ppatch_t);
var
  fx, fy: Integer;
  apos: Integer;
  desttop: PByteArray;
  w, col: Integer;
  column: Pcolumn_t;
  dest: PByte;
  source: PByte;
  count: Integer;
  astart, aend, acurr: Integer;
  delta, prevdelta: Integer;
  tallpatch: Boolean;
begin
  fx := x - patch.leftoffset;
  fy := y - patch.topoffset;
  apos := BufferPosition(fx, fy);

  desttop := @foverlayscreen[apos];

  col := 0;
  w := patch.width;
  if w = 0 then
    exit;

  while col < w do
  begin
    column := Pcolumn_t(Integer(patch) + patch.columnofs[col]);
    delta := 0;
    tallpatch := false;
    // step through the posts in a column
    while column.topdelta <> $ff do
    begin
      source := PByte(Integer(column) + 3);
      delta := delta + column.topdelta;
      dest := @desttop[delta * OVERLAYWIDTH];
      count := column.length;

      astart := pDiff(dest, @foverlayscreen[0], 1);
      aend := astart + OVERLAYWIDTH * (count - 1);
      if ((astart >= 0) and (astart < OVERLAYSIZE)) or
         ((aend >= 0) and (aend < OVERLAYSIZE)) then
        if (fx >=0) and (fx < OVERLAYWIDTH) then
        begin
          acurr := astart;
          while count > 0 do
          begin
            if (acurr >= 0) and (acurr < OVERLAYSIZE) then
              dest^ := source^;
            Inc(source);
            Inc(dest, OVERLAYWIDTH);
            Inc(acurr, OVERLAYWIDTH);
            Dec(count);
          end;
          NotifyDrawSize(astart, aend);
        end;
      if not tallpatch then
      begin
        prevdelta := column.topdelta;
        column := Pcolumn_t(Integer(column) + column.length + 4);
        if column.topdelta > prevdelta then
          delta := 0
        else
          tallpatch := true;
      end
      else
        column := Pcolumn_t(Integer(column) + column.length + 4);
    end;
    Inc(col);
    Inc(fx);
    desttop := @desttop[1];
  end;
end;

procedure TOverlayDrawer.DrawPatchStretched(const x1, y1, x2, y2: Integer; const patchlump: Integer);
var
  patch: Ppatch_t;
begin
  if patchlump < 0 then
    Exit;

  patch := W_CacheLumpNum(patchlump, PU_STATIC);
  DrawPatchStretched(x1, y1, x2, y2, patch);
end;

procedure TOverlayDrawer.DrawPatchStretched(const x1, y1, x2, y2: Integer; const patch: Ppatch_t);
var
  desttop: PByteArray;
  w, col: Integer;
  column: Pcolumn_t;
  dest: PByte;
  source: PByte;
  count: Integer;
  astart, aend: Integer;
  delta, prevdelta: Integer;
  tallpatch: Boolean;
  minsize, maxsize: integer;
  fracxstep, fracystep: fixed_t;
  fracx, fracy: fixed_t;
  overlaycastscreen: overlaycast_p;
  overlaycastbuffer: overlaycast_p;
  x, y: integer;
  dxstep, dystep: integer;
  b: byte;
begin
  if (x1 = x2) or (y1 = y2) then
    exit;

  desttop := @fbackbuffer[0];

  col := 0;
  w := patch.width;
  if w = 0 then
    exit;

  maxsize := 0;
  minsize := OVERLAYSIZE;

  while col < w do
  begin
    column := Pcolumn_t(Integer(patch) + patch.columnofs[col]);
    delta := 0;
    tallpatch := false;
    // step through the posts in a column
    while column.topdelta <> $ff do
    begin
      source := PByte(Integer(column) + 3);
      delta := delta + column.topdelta;
      dest := @desttop[delta * OVERLAYWIDTH];
      count := column.length;

      astart := pDiff(dest, @fbackbuffer[0], 1);
      aend := astart + OVERLAYWIDTH * (count - 1);
      if (astart >= 0) and (astart < OVERLAYSIZE) and
         (aend >= 0) and (aend < OVERLAYSIZE) then
      begin
        while count > 0 do
        begin
          dest^ := source^;
          Inc(source);
          Inc(dest, OVERLAYWIDTH);
          Dec(count);
        end;
        if maxsize < aend then
          maxsize := aend;
        if minsize > astart then
          minsize := astart;
      end;
      if not tallpatch then
      begin
        prevdelta := column.topdelta;
        column := Pcolumn_t(Integer(column) + column.length + 4);
        if column.topdelta > prevdelta then
          delta := 0
        else
          tallpatch := true;
      end
      else
        column := Pcolumn_t(Integer(column) + column.length + 4);
    end;
    Inc(col);
    desttop := @desttop[1];
  end;

  if minsize > maxsize then
    exit;

  overlaycastscreen := @foverlayscreen[0];
  overlaycastbuffer := @fbackbuffer[0];

  fracxstep := (patch.width * FRACUNIT) div (x2 - x1);
  fracystep := (patch.height * FRACUNIT) div (y2 - y1);

  if x1 < x2 then
    dxstep := 1
  else
  begin
    dxstep := -1;
    fracxstep := -fracxstep;
  end;
  if y1 < y2 then
    dystep := 1
  else
  begin
    dystep := -1;
    fracystep := -fracystep;
  end;

  fracy := 0;
  y := y1;
  while y <> y2 do
  begin
    if (y >= 0) and (y < OVERLAYHEIGHT) then
    begin
      fracx := 0;
      x := x1;
      while x <> x2 do
      begin
        b := overlaycastbuffer[fracy div FRACUNIT, fracx div FRACUNIT];
        if b <> 0 then
          if (x >= 0) and (x < OVERLAYWIDTH) then
            overlaycastscreen[y, x] := b;
        fracx := fracx + fracxstep;
        x := x + dxstep;
      end;
    end;
    fracy := fracy + fracystep;
    y := y + dystep;
  end;

  NotifyDrawSize(BufferPosition(minI(x1, x2), minI(y1, y2)), BufferPosition(maxI(x1, x2), maxI(y1, y2)));
  ZeroMemory(@fbackbuffer[minsize], maxsize - minsize + 1);
end;

procedure TOverlayDrawer.DrawPixel(const x, y: Integer; const red, green, blue: byte);
var
  apos: Integer;
  pb: PByte;
begin
  apos := BufferPosition(x, y);
  if (apos >= 0) and (apos < OVERLAYSIZE) then
  begin
    pb := @foverlayscreen[apos];
    pb^ := V_FindAproxColorIndex(@videopal, blue or (green shl 8) or (red shl 16));
    NotifyDrawSize(apos, apos);
  end;
end;

procedure TOverlayDrawer.DrawRect(const x1, y1, x2, y2: Integer; const red, green, blue: byte);
var
  pos1: Integer;
  pos2: Integer;
  pb: PByte;
  c: byte;
  x, y: integer;
  xx1, xx2, yy1, yy2: integer;
begin
  c := V_FindAproxColorIndex(@videopal, blue or (green shl 8) or (red shl 16));

  xx1 := GetIntegerInRange(x1, 0, OVERLAYWIDTH - 1);
  xx2 := GetIntegerInRange(x2, 0, OVERLAYWIDTH - 1);
  yy1 := GetIntegerInRange(y1, 0, OVERLAYWIDTH - 1);
  yy2 := GetIntegerInRange(y2, 0, OVERLAYWIDTH - 1);
  for y := yy1 to yy2 do
  begin
    pos1 := BufferPosition(xx1, y);
    pb := @foverlayscreen[pos1];
    for x := xx1 to xx2 do
    begin
      pb^ := c;
      inc(pb);
    end;
  end;
  pos1 := BufferPosition(xx1, yy1);
  pos2 := BufferPosition(xx2, yy2);
  NotifyDrawSize(pos1, pos2);
end;

procedure TOverlayDrawer.DrawLine(const x1, y1, x2, y2: Integer; const red, green, blue: byte);
// Bresenham's Line Algorithm.  Byte, March 1988, pp. 249-253.
// Modified from http://www.efg2.com/Lab/Library/Delphi/Graphics/Bresenham.txt and tested.
var
  c: byte;
  pos1: Integer;
  pos2: Integer;
  pb: PByte;
  xx1, xx2, yy1, yy2: integer;
  a, b: integer;        // displacements in x and y
  d: integer;           // decision variable
  diag_inc: integer;    // d's increment for diagonal steps
  dx_diag: integer;     // diagonal x step for next pixel
  dx_nondiag: integer;  // nondiagonal x step for next pixel
  dy_diag: integer;     // diagonal y step for next pixel
  dy_nondiag: integer;  // nondiagonal y step for next pixel
  i: integer;           // loop index
  nondiag_inc: integer; // d's increment for nondiagonal steps
  swap: integer;        // temporary variable for swap
  x, y: integer;        // current x and y coordinates
begin
  c := V_FindAproxColorIndex(@videopal, blue or (green shl 8) or (red shl 16));
  pos1 := BufferPosition(x1, y1);
  pos2 := BufferPosition(x2, y2);
  NotifyDrawSize(pos1, pos2); // JVAL: Inaccurate, since we haven't check clipping yet

  // Special case - Horizontal line
  if y1 = y2 then
  begin
    if IsIntegerInRange(y1, 0, OVERLAYHEIGHT - 1) then
    begin
      xx1 := GetIntegerInRange(x1, 0, OVERLAYWIDTH - 1);
      xx2 := GetIntegerInRange(x2, 0, OVERLAYWIDTH - 1);
      pos1 := BufferPosition(xx1, y1);
      pb := @foverlayscreen[pos1];
      for x := xx1 to xx2 do
      begin
        pb^ := c;
        inc(pb);
      end;
    end;
    Exit;
  end;

  // Special case - Verical line
  if x1 = x2 then
  begin
    if IsIntegerInRange(x1, 0, OVERLAYWIDTH - 1) then
    begin
      yy1 := GetIntegerInRange(y1, 0, OVERLAYHEIGHT - 1);
      yy2 := GetIntegerInRange(y2, 0, OVERLAYHEIGHT - 1);
      pos1 := BufferPosition(x1, yy1);
      pb := @foverlayscreen[pos1];
      for y := yy1 to yy2 do
      begin
        pb^ := c;
        inc(pb, OVERLAYWIDTH);
      end;
    end;
    Exit;
  end;

  x := x1;              // line starting point
  y := y1;
  // Determine drawing direction and step to the next pixel.
  a := x2 - x1;         // difference in x dimension
  b := y2 - y1;         // difference in y dimension
  // Determine whether end point lies to right or left of start point.
  if a < 0 then         // drawing towards smaller x values?
  begin
    a := -a;            // make 'a' positive
    dx_diag := -1
  end
  else
    dx_diag := 1;

  // Determine whether end point lies above or below start point.
  if b < 0 then         // drawing towards smaller x values?
  begin
    b := -b;            // make 'a' positive
    dy_diag := -1
  end
  else
    dy_diag := 1;

  // Identify octant containing end point.
  if a < b then
  begin
    swap := a;
    a := b;
    b := swap;
    dx_nondiag := 0;
    dy_nondiag := dy_diag
  end
  else
  begin
    dx_nondiag := dx_diag;
    dy_nondiag := 0
  end;

  d := b + b - a;       // initial value for d is 2*b - a
  nondiag_inc := b + b; // set initial d increment values
  diag_inc := b + b - a - a;
  for i := 0 to a do
  begin   /// draw the a+1 pixels
    pos1 := BufferPosition(x, y);
    if (x >= 0) and (y < OVERLAYSIZE) then
      foverlayscreen[pos1] := c;
    if d < 0 then       // is midpoint above the line?
    begin               // step nondiagonally
      x := x + dx_nondiag;
      y := y + dy_nondiag;
      d := d + nondiag_inc  // update decision variable
    end
    else
    begin               // midpoint is above the line; step diagonally}
      x := x + dx_diag;
      y := y + dy_diag;
      d := d + diag_inc
    end;
  end;
end;

procedure TOverlayDrawer.DrawText(const txt: string; const align: Integer;
  const x, y: Integer);
var
  i: Integer;
  len: Integer;
  c: char;
  twidth: Integer;
  patch: Ppatch_t;
  fx, fy: Integer;

  procedure CalcTextWidth;
  begin
    i := 1;
    twidth := 0;
    while i <= len do
    begin
      c := txt[i];
      if (c >= HU_FONTSTART) and (c <= {$IFDEF DOOM_OR_STRIFE}HU_FONTEND{$ELSE}HU_CFONTEND{$ENDIF}) then
      begin
        patch := {$IFDEF DOOM_OR_STRIFE}hu_font{$ELSE}hu_font3{$ENDIF}[Ord(c) - Ord(HU_FONTSTART)];
        twidth := twidth + patch.width + 1;
      end
      else
        twidth := twidth + 4;
      Inc(i);
    end;
  end;

begin
  len := Length(txt);

  if align = OVR_ALIGN_CENTER then
  begin
    CalcTextWidth;
    fx := x - twidth div 2;
  end
  else if align = OVR_ALIGN_RIGHT then
  begin
    CalcTextWidth;
    fx := x - twidth;
  end
  else
    fx := x;

  fy := y;

  i := 1;
  while i <= len do
  begin
    c := txt[i];
    if (c >= HU_FONTSTART) and (c <= {$IFDEF DOOM_OR_STRIFE}HU_FONTEND{$ELSE}HU_CFONTEND{$ENDIF}) then
    begin
      patch := {$IFDEF DOOM_OR_STRIFE}hu_font{$ELSE}hu_font3{$ENDIF}[Ord(c) - Ord(HU_FONTSTART)];
      DrawPatch(fx, fy, patch);
      fx := fx + (patch.width + 1);
    end
    else
      fx := fx + 4;
    Inc(i);
  end;

end;

const
  OVR_DRAWTEXT = 1;
  OVR_DRAWPATCH = 2;
  OVR_DRAWPIXEL = 3;
  OVR_DRAWRECT = 4;
  OVR_DRAWLINE = 5;
  OVR_DRAWPATCHSTRETCHED = 6;

procedure TOverlayDrawer.DrawDrawer(const i: Integer);
var
  dr: Poverlaydrawer_t;
begin
  dr := @fdrawers[i];
  case dr.proc of
    OVR_DRAWTEXT:
      DrawText(dr.sparam, dr.iparam1, dr.xparam, dr.yparam);
    OVR_DRAWPATCH:
      DrawPatch(dr.xparam, dr.yparam, dr.iparam1);
    OVR_DRAWPIXEL:
      DrawPixel(dr.xparam, dr.yparam, dr.iparam1, dr.iparam2, dr.iparam3);
    OVR_DRAWRECT:
      DrawRect(dr.xparam, dr.yparam, dr.xparam2, dr.yparam2, dr.iparam1, dr.iparam2, dr.iparam3);
    OVR_DRAWLINE:
      DrawLine(dr.xparam, dr.yparam, dr.xparam2, dr.yparam2, dr.iparam1, dr.iparam2, dr.iparam3);
    OVR_DRAWPATCHSTRETCHED:
      DrawPatchStretched(dr.xparam, dr.yparam, dr.xparam2, dr.yparam2, dr.iparam1);
  else
    begin
      I_Warning('TOverlayDrawer.DrawDrawer(): Unknown drawer type "%d"'#13#10, [dr.proc]);
      Exit;
    end;
  end;
end;

procedure TOverlayDrawer.NotifyDrawSize(const astart, aend: Integer);
begin
  if astart < fstart then
  begin
    fstart := astart;
    if fstart < 0 then
      fstart := 0
    else if fstart >= OVERLAYSIZE then
      fstart := OVERLAYSIZE - 1;
  end;
  if aend > fend then
  begin
    fend := aend;
    if fend < 0 then
      fend := 0
    else if fend >= OVERLAYSIZE then
      fend := OVERLAYSIZE - 1;
  end;
end;

function TOverlayDrawer.BufferPosition(const x, y: Integer): Integer;
begin
  Result := x + y * OVERLAYWIDTH;
end;

procedure TOverlayDrawer.AddPatch(const ticks: Integer; const patchname: string;
  const x, y: Integer);
var
  pdrawer: Poverlaydrawer_t;
  lump: Integer;
  pname: string;
begin
  if ticks < 0 then
    Exit;

  pname := patchname;
  if Length(pname) > 8 then
  begin
    SetLength(pname, 8);
    I_Warning('TOverlayDrawer.AddPatch(): Patch name "%s" has more than 8 characters, truncated to "%s"'#13#10, [patchname, pname]);
  end;
  lump := W_CheckNumForName(pname, TYPE_PATCH or TYPE_SPRITE);
  if lump < 0 then
  begin
    I_Warning('TOverlayDrawer.AddPatch(): Invalid patch "%s"'#13#10, [pname]);
    Exit;
  end;

  Grow;
  pdrawer := @fdrawers[fnumdrawers];
  Inc(fnumdrawers);

  pdrawer.proc := OVR_DRAWPATCH;
  pdrawer.tick := leveltime + ticks;
  pdrawer.xparam := x;
  pdrawer.yparam := y;
  pdrawer.iparam1 := lump;
  pdrawer.sparam := patchname;
end;

procedure TOverlayDrawer.AddPatchStretched(const ticks: Integer; const patchname: string;
  const x1, y1, x2, y2: Integer);
var
  pdrawer: Poverlaydrawer_t;
  lump: Integer;
  pname: string;
begin
  if ticks < 0 then
    Exit;

  pname := patchname;
  if Length(pname) > 8 then
  begin
    SetLength(pname, 8);
    I_Warning('TOverlayDrawer.AddPatch(): Patch name "%s" has more than 8 characters, truncated to "%s"'#13#10, [patchname, pname]);
  end;
  lump := W_CheckNumForName(pname, TYPE_PATCH or TYPE_SPRITE);
  if lump < 0 then
  begin
    I_Warning('TOverlayDrawer.AddPatch(): Invalid patch "%s"'#13#10, [pname]);
    Exit;
  end;

  Grow;
  pdrawer := @fdrawers[fnumdrawers];
  Inc(fnumdrawers);

  pdrawer.proc := OVR_DRAWPATCHSTRETCHED;
  pdrawer.tick := leveltime + ticks;
  pdrawer.xparam := x1;
  pdrawer.yparam := y1;
  pdrawer.xparam2 := x2;
  pdrawer.yparam2 := y2;
  pdrawer.iparam1 := lump;
  pdrawer.sparam := patchname;
end;

procedure TOverlayDrawer.AddPixel(const ticks: Integer; const red, green, blue: byte;
  const x, y: Integer);
var
  pdrawer: Poverlaydrawer_t;
begin
  if ticks < 0 then
    Exit;

  Grow;
  pdrawer := @fdrawers[fnumdrawers];
  Inc(fnumdrawers);

  pdrawer.proc := OVR_DRAWPIXEL;
  pdrawer.tick := leveltime + ticks;
  pdrawer.xparam := x;
  pdrawer.yparam := y;
  pdrawer.iparam1 := red;
  pdrawer.iparam2 := green;
  pdrawer.iparam3 := blue;
end;

procedure TOverlayDrawer.AddRect(const ticks: Integer; const red, green, blue: byte;
  const x1, y1, x2, y2: Integer);
var
  pdrawer: Poverlaydrawer_t;
begin
  if ticks < 0 then
    Exit;

  Grow;
  pdrawer := @fdrawers[fnumdrawers];
  Inc(fnumdrawers);

  pdrawer.proc := OVR_DRAWRECT;
  pdrawer.tick := leveltime + ticks;
  pdrawer.xparam := x1;
  pdrawer.yparam := y1;
  pdrawer.xparam2 := x2;
  pdrawer.yparam2 := y2;
  pdrawer.iparam1 := red;
  pdrawer.iparam2 := green;
  pdrawer.iparam3 := blue;
end;

procedure TOverlayDrawer.AddLine(const ticks: Integer; const red, green, blue: byte;
  const x1, y1, x2, y2: Integer);
var
  pdrawer: Poverlaydrawer_t;
begin
  if ticks < 0 then
    Exit;

  Grow;
  pdrawer := @fdrawers[fnumdrawers];
  Inc(fnumdrawers);

  pdrawer.proc := OVR_DRAWLINE;
  pdrawer.tick := leveltime + ticks;
  pdrawer.xparam := x1;
  pdrawer.yparam := y1;
  pdrawer.xparam2 := x2;
  pdrawer.yparam2 := y2;
  pdrawer.iparam1 := red;
  pdrawer.iparam2 := green;
  pdrawer.iparam3 := blue;
end;

procedure TOverlayDrawer.AddText(const ticks: Integer; const txt: string; const align: Integer;
  const x, y: Integer);
var
  pdrawer: Poverlaydrawer_t;
begin
  if ticks < 0 then
    Exit;

  Grow;
  pdrawer := @fdrawers[fnumdrawers];
  Inc(fnumdrawers);

  pdrawer.proc := OVR_DRAWTEXT;
  pdrawer.tick := leveltime + ticks;
  pdrawer.xparam := x;
  pdrawer.yparam := y;
  pdrawer.sparam := strupper(txt);
  pdrawer.iparam1 := align;
end;

procedure TOverlayDrawer.AddLeftText(const ticks: Integer; const txt: string;
  const x, y: Integer);
begin
  AddText(ticks, txt, OVR_ALIGN_LEFT, x, y);
end;

procedure TOverlayDrawer.AddRightText(const ticks: Integer; const txt: string;
  const x, y: Integer);
begin
  AddText(ticks, txt, OVR_ALIGN_RIGHT, x, y);
end;

procedure TOverlayDrawer.AddCenterText(const ticks: Integer; const txt: string;
  const x, y: Integer);
begin
  AddText(ticks, txt, OVR_ALIGN_CENTER, x, y);
end;

procedure TOverlayDrawer.DrawDrawers;
var
  i, j: Integer;
  cnt, drawcnt: Integer;
begin
  ClearScreen;
  cnt := 0;
  drawcnt := 0;
  for i := 0 to fnumdrawers - 1 do
    if leveltime <= fdrawers[i].tick then
    begin
      DrawDrawer(i);
      Inc(drawcnt);
    end
    else
    begin
      Inc(cnt);
      fdrawers[i].proc := 0;
    end;

  if cnt > 0 then
  begin
    j := 0;
    for i := 0 to fnumdrawers - 1 do
    begin
      if j < i then
      begin
        if fdrawers[i].proc <> 0 then
        begin
          fdrawers[j] := fdrawers[i];
          inc(j);
        end;
      end
      else if fdrawers[i].proc <> 0 then
        inc(j);
    end;
    fnumdrawers := j;
  end;
  fmodified := lastdrawcnt <> drawcnt;
  lastdrawcnt := drawcnt;
  if fmodified then
    needsbackscreen := True;
end;

{$IFDEF OPENGL}
function TOverlayDrawer.GetOverlayHeight: Integer;
var
  dend: Integer;
  swidht: Integer;
  sheight: Integer;
begin
  dend := lastoverlaylookup[fend];
  swidht := V_GetScreenWidth(SCN_FG);
  Result := (dend + swidht) div swidht;
  sheight := V_GetScreenHeight(SCN_FG);
  if Result > sheight then
    Result := sheight;
end;
{$ENDIF}

type
  ovrflash_t = record
    ovr: TOverlayDrawer;
    astart, afinish: integer;
  end;
  ovrflash_p = ^ovrflash_t;

{$IFNDEF OPENGL}
function _thr_ovr_flash8(p: ovrflash_p): integer; stdcall;
var
  destb: PByte;
  src: PByte;
  dstart, dend: Integer;
  b: byte;
  ovr: TOverlayDrawer;
begin
  Result := 0;

  ovr := p.ovr;
  dstart := ovr.firstoverlaylookup[p.astart];
  destb := @screens[SCN_FG][dstart];

  dend := ovr.lastoverlaylookup[p.afinish];

  repeat
    src := ovr.overlaylookup[dstart];
    if PLongWord(src)^ = 0 then
    begin
      Inc(dstart, 4);
      Inc(destb, 4);
    end
    else
    begin
      b := src^;
      if b <> 0 then
        destb^ := b;
      Inc(dstart);
      Inc(destb);
    end;
  until dstart > dend;
end;

procedure TOverlayDrawer.FlashToScreen8;
var
  parms: array[0..16] of ovrflash_t;
  destb: PByte;
  src: PByte;
  dstart, dend: Integer;
  b: byte;
  i: integer;
  nthreads: integer;
  sz: integer;
begin
  if fnumdrawers = 0 then
    Exit;

  if firstinterpolation then
    DrawDrawers;

  sz := fend - fstart;
  if sz < 0 then
    Exit;

  if usemultithread and (sz > 8 * 320) then
  begin
    nthreads := I_GetNumCPUs;
    if nthreads < 2 then
      nthreads := 2
    else if nthreads > 17 then
      nthreads := 17;
    sz := sz div nthreads;
    parms[0].ovr := self;
    parms[0].astart := fstart;
    for i := 1 to nthreads - 1 do
    begin
      parms[i].ovr := self;
      parms[i - 1].afinish := fstart + sz * i;
      parms[i].astart := parms[i - 1].afinish + 1;
    end;
    parms[nthreads - 1].afinish := fend;
    case nthreads of
      2:
        MT_Execute(
          @_thr_ovr_flash8, @parms[0],
          @_thr_ovr_flash8, @parms[1]
        );
      3:
        MT_Execute(
          @_thr_ovr_flash8, @parms[0],
          @_thr_ovr_flash8, @parms[1],
          @_thr_ovr_flash8, @parms[2]
        );
      4:
        MT_Execute4(
          @_thr_ovr_flash8, @parms[0],
          @_thr_ovr_flash8, @parms[1],
          @_thr_ovr_flash8, @parms[2],
          @_thr_ovr_flash8, @parms[3]
        );
      5:
        MT_Execute(
          @_thr_ovr_flash8, @parms[0],
          @_thr_ovr_flash8, @parms[1],
          @_thr_ovr_flash8, @parms[2],
          @_thr_ovr_flash8, @parms[3],
          @_thr_ovr_flash8, @parms[4]
        );
      6:
        MT_Execute6(
         @_thr_ovr_flash8, @parms[0],
         @_thr_ovr_flash8, @parms[1],
         @_thr_ovr_flash8, @parms[2],
         @_thr_ovr_flash8, @parms[3],
         @_thr_ovr_flash8, @parms[4],
         @_thr_ovr_flash8, @parms[5]
        );
      7:
        MT_Execute(
         @_thr_ovr_flash8, @parms[0],
         @_thr_ovr_flash8, @parms[1],
         @_thr_ovr_flash8, @parms[2],
         @_thr_ovr_flash8, @parms[3],
         @_thr_ovr_flash8, @parms[4],
         @_thr_ovr_flash8, @parms[5],
         @_thr_ovr_flash8, @parms[6]
        );
      8:
        MT_Execute8(
         @_thr_ovr_flash8, @parms[0],
         @_thr_ovr_flash8, @parms[1],
         @_thr_ovr_flash8, @parms[2],
         @_thr_ovr_flash8, @parms[3],
         @_thr_ovr_flash8, @parms[4],
         @_thr_ovr_flash8, @parms[5],
         @_thr_ovr_flash8, @parms[6],
         @_thr_ovr_flash8, @parms[7]
        );
      9:
        MT_Execute(
         @_thr_ovr_flash8, @parms[0],
         @_thr_ovr_flash8, @parms[1],
         @_thr_ovr_flash8, @parms[2],
         @_thr_ovr_flash8, @parms[3],
         @_thr_ovr_flash8, @parms[4],
         @_thr_ovr_flash8, @parms[5],
         @_thr_ovr_flash8, @parms[6],
         @_thr_ovr_flash8, @parms[7],
         @_thr_ovr_flash8, @parms[8]
        );
     10:
        MT_Execute(
         @_thr_ovr_flash8, @parms[0],
         @_thr_ovr_flash8, @parms[1],
         @_thr_ovr_flash8, @parms[2],
         @_thr_ovr_flash8, @parms[3],
         @_thr_ovr_flash8, @parms[4],
         @_thr_ovr_flash8, @parms[5],
         @_thr_ovr_flash8, @parms[6],
         @_thr_ovr_flash8, @parms[7],
         @_thr_ovr_flash8, @parms[8],
         @_thr_ovr_flash8, @parms[9]
        );
     11:
        MT_Execute(
         @_thr_ovr_flash8, @parms[0],
         @_thr_ovr_flash8, @parms[1],
         @_thr_ovr_flash8, @parms[2],
         @_thr_ovr_flash8, @parms[3],
         @_thr_ovr_flash8, @parms[4],
         @_thr_ovr_flash8, @parms[5],
         @_thr_ovr_flash8, @parms[6],
         @_thr_ovr_flash8, @parms[7],
         @_thr_ovr_flash8, @parms[8],
         @_thr_ovr_flash8, @parms[9],
         @_thr_ovr_flash8, @parms[10]
        );
     12:
        MT_Execute12(
         @_thr_ovr_flash8, @parms[0],
         @_thr_ovr_flash8, @parms[1],
         @_thr_ovr_flash8, @parms[2],
         @_thr_ovr_flash8, @parms[3],
         @_thr_ovr_flash8, @parms[4],
         @_thr_ovr_flash8, @parms[5],
         @_thr_ovr_flash8, @parms[6],
         @_thr_ovr_flash8, @parms[7],
         @_thr_ovr_flash8, @parms[8],
         @_thr_ovr_flash8, @parms[9],
         @_thr_ovr_flash8, @parms[10],
         @_thr_ovr_flash8, @parms[11]
        );
     13:
        MT_Execute(
         @_thr_ovr_flash8, @parms[0],
         @_thr_ovr_flash8, @parms[1],
         @_thr_ovr_flash8, @parms[2],
         @_thr_ovr_flash8, @parms[3],
         @_thr_ovr_flash8, @parms[4],
         @_thr_ovr_flash8, @parms[5],
         @_thr_ovr_flash8, @parms[6],
         @_thr_ovr_flash8, @parms[7],
         @_thr_ovr_flash8, @parms[8],
         @_thr_ovr_flash8, @parms[9],
         @_thr_ovr_flash8, @parms[10],
         @_thr_ovr_flash8, @parms[11],
         @_thr_ovr_flash8, @parms[12]
        );
     14:
        MT_Execute(
         @_thr_ovr_flash8, @parms[0],
         @_thr_ovr_flash8, @parms[1],
         @_thr_ovr_flash8, @parms[2],
         @_thr_ovr_flash8, @parms[3],
         @_thr_ovr_flash8, @parms[4],
         @_thr_ovr_flash8, @parms[5],
         @_thr_ovr_flash8, @parms[6],
         @_thr_ovr_flash8, @parms[7],
         @_thr_ovr_flash8, @parms[8],
         @_thr_ovr_flash8, @parms[9],
         @_thr_ovr_flash8, @parms[10],
         @_thr_ovr_flash8, @parms[11],
         @_thr_ovr_flash8, @parms[12],
         @_thr_ovr_flash8, @parms[13]
        );
     15:
        MT_Execute(
         @_thr_ovr_flash8, @parms[0],
         @_thr_ovr_flash8, @parms[1],
         @_thr_ovr_flash8, @parms[2],
         @_thr_ovr_flash8, @parms[3],
         @_thr_ovr_flash8, @parms[4],
         @_thr_ovr_flash8, @parms[5],
         @_thr_ovr_flash8, @parms[6],
         @_thr_ovr_flash8, @parms[7],
         @_thr_ovr_flash8, @parms[8],
         @_thr_ovr_flash8, @parms[9],
         @_thr_ovr_flash8, @parms[10],
         @_thr_ovr_flash8, @parms[11],
         @_thr_ovr_flash8, @parms[12],
         @_thr_ovr_flash8, @parms[13],
         @_thr_ovr_flash8, @parms[14]
        );
     16:
        MT_Execute16(
         @_thr_ovr_flash8, @parms[0],
         @_thr_ovr_flash8, @parms[1],
         @_thr_ovr_flash8, @parms[2],
         @_thr_ovr_flash8, @parms[3],
         @_thr_ovr_flash8, @parms[4],
         @_thr_ovr_flash8, @parms[5],
         @_thr_ovr_flash8, @parms[6],
         @_thr_ovr_flash8, @parms[7],
         @_thr_ovr_flash8, @parms[8],
         @_thr_ovr_flash8, @parms[9],
         @_thr_ovr_flash8, @parms[10],
         @_thr_ovr_flash8, @parms[11],
         @_thr_ovr_flash8, @parms[12],
         @_thr_ovr_flash8, @parms[13],
         @_thr_ovr_flash8, @parms[14],
         @_thr_ovr_flash8, @parms[15]
        );
      else
        MT_Execute(
         @_thr_ovr_flash8, @parms[0],
         @_thr_ovr_flash8, @parms[1],
         @_thr_ovr_flash8, @parms[2],
         @_thr_ovr_flash8, @parms[3],
         @_thr_ovr_flash8, @parms[4],
         @_thr_ovr_flash8, @parms[5],
         @_thr_ovr_flash8, @parms[6],
         @_thr_ovr_flash8, @parms[7],
         @_thr_ovr_flash8, @parms[8],
         @_thr_ovr_flash8, @parms[9],
         @_thr_ovr_flash8, @parms[10],
         @_thr_ovr_flash8, @parms[11],
         @_thr_ovr_flash8, @parms[12],
         @_thr_ovr_flash8, @parms[13],
         @_thr_ovr_flash8, @parms[14],
         @_thr_ovr_flash8, @parms[15],
         @_thr_ovr_flash8, @parms[16]
        );
    end;
  end
  else
  begin
    dstart := ffirstoverlaylookup[fstart];
    destb := @screens[SCN_FG][fstart];

    dend := flastoverlaylookup[fend];

    repeat
      src := foverlaylookup[dstart];
      if PLongWord(src)^ = 0 then
      begin
        Inc(dstart, 4);
        Inc(destb, 4);
      end
      else
      begin
        b := src^;
        if b <> 0 then
          destb^ := b;
        Inc(dstart);
        Inc(destb);
      end;
    until dstart > dend;
  end;
end;
{$ENDIF}

function _thr_ovr_flash32(p: ovrflash_p): integer; stdcall;
var
  destl: PLongWord;
  src: PByte;
  dstart, dend: Integer;
  b: byte;
  ovr: TOverlayDrawer;
begin
  Result := 0;

  ovr := p.ovr;
  dstart := ovr.firstoverlaylookup[p.astart];
  destl := @screen32[dstart];

  dend := ovr.lastoverlaylookup[p.afinish];

  repeat
    src := ovr.overlaylookup[dstart];
    if PLongWord(src)^ = 0 then
    begin
      Inc(dstart, 4);
      Inc(destl, 4);
    end
    else
    begin
      b := src^;
      if b <> 0 then
        destl^ := videopal[b];
      Inc(dstart);
      Inc(destl);
    end;
  until dstart > dend;
end;

procedure TOverlayDrawer.FlashToScreen32;
var
  parms: array[0..16] of ovrflash_t;
  destl: PLongWord;
  src: PByte;
  dstart, dend: Integer;
  b: byte;
  i: integer;
  nthreads: integer;
  sz: integer;
begin
  if fnumdrawers = 0 then
    Exit;

  if firstinterpolation then
    DrawDrawers;

  sz := fend - fstart;
  if sz < 0 then
    Exit;

  if usemultithread and (sz > 8 * 320) then
  begin
    nthreads := I_GetNumCPUs;
    if nthreads < 2 then
      nthreads := 2
    else if nthreads > 17 then
      nthreads := 17;
    sz := sz div nthreads;
    parms[0].ovr := self;
    parms[0].astart := fstart;
    for i := 1 to nthreads - 1 do
    begin
      parms[i].ovr := self;
      parms[i - 1].afinish := fstart + sz * i;
      parms[i].astart := parms[i - 1].afinish + 1;
    end;
    parms[nthreads - 1].afinish := fend;
    case nthreads of
      2:
        MT_Execute(
          @_thr_ovr_flash32, @parms[0],
          @_thr_ovr_flash32, @parms[1]
        );
      3:
        MT_Execute(
          @_thr_ovr_flash32, @parms[0],
          @_thr_ovr_flash32, @parms[1],
          @_thr_ovr_flash32, @parms[2]
        );
      4:
        MT_Execute4(
          @_thr_ovr_flash32, @parms[0],
          @_thr_ovr_flash32, @parms[1],
          @_thr_ovr_flash32, @parms[2],
          @_thr_ovr_flash32, @parms[3]
        );
      5:
        MT_Execute(
          @_thr_ovr_flash32, @parms[0],
          @_thr_ovr_flash32, @parms[1],
          @_thr_ovr_flash32, @parms[2],
          @_thr_ovr_flash32, @parms[3],
          @_thr_ovr_flash32, @parms[4]
        );
      6:
        MT_Execute6(
         @_thr_ovr_flash32, @parms[0],
         @_thr_ovr_flash32, @parms[1],
         @_thr_ovr_flash32, @parms[2],
         @_thr_ovr_flash32, @parms[3],
         @_thr_ovr_flash32, @parms[4],
         @_thr_ovr_flash32, @parms[5]
        );
      7:
        MT_Execute(
         @_thr_ovr_flash32, @parms[0],
         @_thr_ovr_flash32, @parms[1],
         @_thr_ovr_flash32, @parms[2],
         @_thr_ovr_flash32, @parms[3],
         @_thr_ovr_flash32, @parms[4],
         @_thr_ovr_flash32, @parms[5],
         @_thr_ovr_flash32, @parms[6]
        );
      8:
        MT_Execute8(
         @_thr_ovr_flash32, @parms[0],
         @_thr_ovr_flash32, @parms[1],
         @_thr_ovr_flash32, @parms[2],
         @_thr_ovr_flash32, @parms[3],
         @_thr_ovr_flash32, @parms[4],
         @_thr_ovr_flash32, @parms[5],
         @_thr_ovr_flash32, @parms[6],
         @_thr_ovr_flash32, @parms[7]
        );
      9:
        MT_Execute(
         @_thr_ovr_flash32, @parms[0],
         @_thr_ovr_flash32, @parms[1],
         @_thr_ovr_flash32, @parms[2],
         @_thr_ovr_flash32, @parms[3],
         @_thr_ovr_flash32, @parms[4],
         @_thr_ovr_flash32, @parms[5],
         @_thr_ovr_flash32, @parms[6],
         @_thr_ovr_flash32, @parms[7],
         @_thr_ovr_flash32, @parms[8]
        );
     10:
        MT_Execute(
         @_thr_ovr_flash32, @parms[0],
         @_thr_ovr_flash32, @parms[1],
         @_thr_ovr_flash32, @parms[2],
         @_thr_ovr_flash32, @parms[3],
         @_thr_ovr_flash32, @parms[4],
         @_thr_ovr_flash32, @parms[5],
         @_thr_ovr_flash32, @parms[6],
         @_thr_ovr_flash32, @parms[7],
         @_thr_ovr_flash32, @parms[8],
         @_thr_ovr_flash32, @parms[9]
        );
     11:
        MT_Execute(
         @_thr_ovr_flash32, @parms[0],
         @_thr_ovr_flash32, @parms[1],
         @_thr_ovr_flash32, @parms[2],
         @_thr_ovr_flash32, @parms[3],
         @_thr_ovr_flash32, @parms[4],
         @_thr_ovr_flash32, @parms[5],
         @_thr_ovr_flash32, @parms[6],
         @_thr_ovr_flash32, @parms[7],
         @_thr_ovr_flash32, @parms[8],
         @_thr_ovr_flash32, @parms[9],
         @_thr_ovr_flash32, @parms[10]
        );
     12:
        MT_Execute12(
         @_thr_ovr_flash32, @parms[0],
         @_thr_ovr_flash32, @parms[1],
         @_thr_ovr_flash32, @parms[2],
         @_thr_ovr_flash32, @parms[3],
         @_thr_ovr_flash32, @parms[4],
         @_thr_ovr_flash32, @parms[5],
         @_thr_ovr_flash32, @parms[6],
         @_thr_ovr_flash32, @parms[7],
         @_thr_ovr_flash32, @parms[8],
         @_thr_ovr_flash32, @parms[9],
         @_thr_ovr_flash32, @parms[10],
         @_thr_ovr_flash32, @parms[11]
        );
     13:
        MT_Execute(
         @_thr_ovr_flash32, @parms[0],
         @_thr_ovr_flash32, @parms[1],
         @_thr_ovr_flash32, @parms[2],
         @_thr_ovr_flash32, @parms[3],
         @_thr_ovr_flash32, @parms[4],
         @_thr_ovr_flash32, @parms[5],
         @_thr_ovr_flash32, @parms[6],
         @_thr_ovr_flash32, @parms[7],
         @_thr_ovr_flash32, @parms[8],
         @_thr_ovr_flash32, @parms[9],
         @_thr_ovr_flash32, @parms[10],
         @_thr_ovr_flash32, @parms[11],
         @_thr_ovr_flash32, @parms[12]
        );
     14:
        MT_Execute(
         @_thr_ovr_flash32, @parms[0],
         @_thr_ovr_flash32, @parms[1],
         @_thr_ovr_flash32, @parms[2],
         @_thr_ovr_flash32, @parms[3],
         @_thr_ovr_flash32, @parms[4],
         @_thr_ovr_flash32, @parms[5],
         @_thr_ovr_flash32, @parms[6],
         @_thr_ovr_flash32, @parms[7],
         @_thr_ovr_flash32, @parms[8],
         @_thr_ovr_flash32, @parms[9],
         @_thr_ovr_flash32, @parms[10],
         @_thr_ovr_flash32, @parms[11],
         @_thr_ovr_flash32, @parms[12],
         @_thr_ovr_flash32, @parms[13]
        );
     15:
        MT_Execute(
         @_thr_ovr_flash32, @parms[0],
         @_thr_ovr_flash32, @parms[1],
         @_thr_ovr_flash32, @parms[2],
         @_thr_ovr_flash32, @parms[3],
         @_thr_ovr_flash32, @parms[4],
         @_thr_ovr_flash32, @parms[5],
         @_thr_ovr_flash32, @parms[6],
         @_thr_ovr_flash32, @parms[7],
         @_thr_ovr_flash32, @parms[8],
         @_thr_ovr_flash32, @parms[9],
         @_thr_ovr_flash32, @parms[10],
         @_thr_ovr_flash32, @parms[11],
         @_thr_ovr_flash32, @parms[12],
         @_thr_ovr_flash32, @parms[13],
         @_thr_ovr_flash32, @parms[14]
        );
     16:
        MT_Execute16(
         @_thr_ovr_flash32, @parms[0],
         @_thr_ovr_flash32, @parms[1],
         @_thr_ovr_flash32, @parms[2],
         @_thr_ovr_flash32, @parms[3],
         @_thr_ovr_flash32, @parms[4],
         @_thr_ovr_flash32, @parms[5],
         @_thr_ovr_flash32, @parms[6],
         @_thr_ovr_flash32, @parms[7],
         @_thr_ovr_flash32, @parms[8],
         @_thr_ovr_flash32, @parms[9],
         @_thr_ovr_flash32, @parms[10],
         @_thr_ovr_flash32, @parms[11],
         @_thr_ovr_flash32, @parms[12],
         @_thr_ovr_flash32, @parms[13],
         @_thr_ovr_flash32, @parms[14],
         @_thr_ovr_flash32, @parms[15]
        );
      else
        MT_Execute(
         @_thr_ovr_flash32, @parms[0],
         @_thr_ovr_flash32, @parms[1],
         @_thr_ovr_flash32, @parms[2],
         @_thr_ovr_flash32, @parms[3],
         @_thr_ovr_flash32, @parms[4],
         @_thr_ovr_flash32, @parms[5],
         @_thr_ovr_flash32, @parms[6],
         @_thr_ovr_flash32, @parms[7],
         @_thr_ovr_flash32, @parms[8],
         @_thr_ovr_flash32, @parms[9],
         @_thr_ovr_flash32, @parms[10],
         @_thr_ovr_flash32, @parms[11],
         @_thr_ovr_flash32, @parms[12],
         @_thr_ovr_flash32, @parms[13],
         @_thr_ovr_flash32, @parms[14],
         @_thr_ovr_flash32, @parms[15],
         @_thr_ovr_flash32, @parms[16]
        );
    end;
  end
  else
  begin
    dstart := ffirstoverlaylookup[fstart];
    destl := @screen32[dstart];

    dend := flastoverlaylookup[fend];

    repeat
      src := foverlaylookup[dstart];
      if PLongWord(src)^ = 0 then
      begin
        Inc(dstart, 4);
        Inc(destl, 4);
      end
      else
      begin
        b := src^;
        if b <> 0 then
          destl^ := videopal[b];
        Inc(dstart);
        Inc(destl);
      end;
    until dstart > dend;
  end;
end;

procedure CmdOverlayDrawText(const s1, s2: string; const align: Integer);
var
  ticks: Integer;
  x, y: Integer;
  sx, sy: string;
  msg: string;
  tmp1: string;
begin
  if gamestate <> GS_LEVEL then
  begin
    printf('Overlay drawer is available only when playing the game'#13#10);
    Exit;
  end;

  if s1 = '' then
  begin
    case align of
      OVR_ALIGN_LEFT:
        printf('overlaydrawtextleft [ticks] [x] [y] [message]'#13#10);
      OVR_ALIGN_RIGHT:
        printf('overlaydrawtextright [ticks] [x] [y] [message]'#13#10);
      else
        printf('overlaydrawtextcenter [ticks] [x] [y] [message]'#13#10);
    end;
    Exit;
  end;

  ticks := atoi(s1, -1);
  if ticks < 0 then
  begin
    printf('Ticks must be a positive number'#13#10);
    Exit;
  end;

  splitstring(s2, sx, tmp1, ' ');
  splitstring(tmp1, sy, msg, ' ');
  x := atoi(sx, -1);
  y := atoi(sy, -1);

  overlay.AddText(ticks, msg, align, x, y);
end;

procedure CmdOverlayPutPixel(const s1, s2: string);
var
  ticks: Integer;
  x, y: Integer;
  sx, sy: string;
  red, green, blue: byte;
  sred, sgreen, sblue: string;
  tmp1: string;
  tmp2: string;
begin
  if gamestate <> GS_LEVEL then
  begin
    printf('Overlay drawer is available only when playing the game'#13#10);
    Exit;
  end;

  if s1 = '' then
  begin
    printf('overlayputpixel [ticks] [x] [y] [red] [green] [blue]'#13#10);
    Exit;
  end;

  ticks := atoi(s1, -1);
  if ticks < 0 then
  begin
    printf('Ticks must be a positive number'#13#10);
    Exit;
  end;

  splitstring(s2, sx, tmp1, ' ');
  splitstring(tmp1, sy, tmp2, ' ');
  x := atoi(sx, -1);
  y := atoi(sy, -1);
  splitstring(tmp2, sred, tmp1, ' ');
  splitstring(tmp1, sgreen, sblue, ' ');
  red := atoi(sred, 0);
  green := atoi(sgreen, 0);
  blue := atoi(sblue, 0);

  overlay.AddPixel(ticks, red, green, blue, x, y);
end;

procedure CmdOverlayDrawTextLeft(const s1, s2: string);
begin
  CmdOverlayDrawText(s1, s2, OVR_ALIGN_LEFT);
end;

procedure CmdOverlayDrawTextRight(const s1, s2: string);
begin
  CmdOverlayDrawText(s1, s2, OVR_ALIGN_RIGHT);
end;

procedure CmdOverlayDrawTextCenter(const s1, s2: string);
begin
  CmdOverlayDrawText(s1, s2, OVR_ALIGN_CENTER);
end;

procedure CmdOverlayDrawRect(const s1, s2: string);
var
  ticks: Integer;
  x1, y1, x2, y2: Integer;
  red, green, blue: byte;
  sc: TScriptEngine;
begin
  if gamestate <> GS_LEVEL then
  begin
    printf('Overlay drawer is available only when playing the game'#13#10);
    Exit;
  end;

  if (s1 = '') or (s2 = '') then
  begin
    printf('overlaydrawrect [ticks] [x1] [y1] [x2] [y2] [red] [green] [blue]'#13#10);
    Exit;
  end;

  ticks := atoi(s1, -1);
  if ticks < 0 then
  begin
    printf('Ticks must be a positive number'#13#10);
    Exit;
  end;

  sc := TScriptEngine.Create(s2);

  sc.MustGetInteger;
  x1 := sc._Integer;
  sc.MustGetInteger;
  y1 := sc._Integer;
  sc.MustGetInteger;
  x2 := sc._Integer;
  sc.MustGetInteger;
  y2 := sc._Integer;
  sc.MustGetInteger;
  red := sc._Integer;
  sc.MustGetInteger;
  green := sc._Integer;
  sc.MustGetInteger;
  blue := sc._Integer;
  sc.Free;

  overlay.AddRect(ticks, red, green, blue, x1, y1, x2, y2);
end;

procedure CmdOverlayDrawLine(const s1, s2: string);
var
  ticks: Integer;
  x1, y1, x2, y2: Integer;
  red, green, blue: byte;
  sc: TScriptEngine;
begin
  if gamestate <> GS_LEVEL then
  begin
    printf('Overlay drawer is available only when playing the game'#13#10);
    Exit;
  end;

  if (s1 = '') or (s2 = '') then
  begin
    printf('overlaydrawline [ticks] [x1] [y1] [x2] [y2] [red] [green] [blue]'#13#10);
    Exit;
  end;

  ticks := atoi(s1, -1);
  if ticks < 0 then
  begin
    printf('Ticks must be a positive number'#13#10);
    Exit;
  end;

  sc := TScriptEngine.Create(s2);

  sc.MustGetInteger;
  x1 := sc._Integer;
  sc.MustGetInteger;
  y1 := sc._Integer;
  sc.MustGetInteger;
  x2 := sc._Integer;
  sc.MustGetInteger;
  y2 := sc._Integer;
  sc.MustGetInteger;
  red := sc._Integer;
  sc.MustGetInteger;
  green := sc._Integer;
  sc.MustGetInteger;
  blue := sc._Integer;
  sc.Free;

  overlay.AddLine(ticks, red, green, blue, x1, y1, x2, y2);
end;

procedure CmdOverlayDrawPatch(const s1, s2: string);
var
  ticks: Integer;
  x, y: Integer;
  patchname: string;
  sc: TScriptEngine;
begin
  if gamestate <> GS_LEVEL then
  begin
    printf('Overlay drawer is available only when playing the game'#13#10);
    Exit;
  end;

  if (s1 = '') or (s2 = '') then
  begin
    printf('overlaydrawpatch [ticks] [x] [y] [patch]'#13#10);
    Exit;
  end;

  ticks := atoi(s1, -1);
  if ticks < 0 then
  begin
    printf('Ticks must be a positive number'#13#10);
    Exit;
  end;

  sc := TScriptEngine.Create(s2);

  sc.MustGetInteger;
  x := sc._Integer;
  sc.MustGetInteger;
  y := sc._Integer;
  sc.MustGetString;
  patchname := sc._String;
  sc.Free;

  overlay.AddPatch(ticks, patchname, x, y);
end;

procedure CmdOverlayDrawPatchStretched(const s1, s2: string);
var
  ticks: Integer;
  x1, y1, x2, y2: Integer;
  patchname: string;
  sc: TScriptEngine;
begin
  if gamestate <> GS_LEVEL then
  begin
    printf('Overlay drawer is available only when playing the game'#13#10);
    Exit;
  end;

  if (s1 = '') or (s2 = '') then
  begin
    printf('overlaydrawpatchstretched [ticks] [x1] [y1] [x2] [y2] [patch]'#13#10);
    Exit;
  end;

  ticks := atoi(s1, -1);
  if ticks < 0 then
  begin
    printf('Ticks must be a positive number'#13#10);
    Exit;
  end;

  sc := TScriptEngine.Create(s2);

  sc.MustGetInteger;
  x1 := sc._Integer;
  sc.MustGetInteger;
  y1 := sc._Integer;
  sc.MustGetInteger;
  x2 := sc._Integer;
  sc.MustGetInteger;
  y2 := sc._Integer;
  sc.MustGetString;
  patchname := sc._String;
  sc.Free;

  overlay.AddPatchStretched(ticks, patchname, x1, y1, x2, y2);
end;

procedure PS_InitOverlay;
begin
  overlay := TOverlayDrawer.Create;

  C_AddCmd('overlaydrawtextleft', @CmdOverlayDrawTextLeft);
  C_AddCmd('overlaydrawtextright', @CmdOverlayDrawTextRight);
  C_AddCmd('overlaydrawtextcenter', @CmdOverlayDrawTextCenter);
  C_AddCmd('overlaydrawpixel, overlayputpixel', @CmdOverlayPutPixel);
  C_AddCmd('overlaydrawrect', @CmdOverlayDrawRect);
  C_AddCmd('overlaydrawline', @CmdOverlayDrawLine);
  C_AddCmd('overlaydrawpatch', @CmdOverlayDrawPatch);
  C_AddCmd('overlaydrawpatchstretched', @CmdOverlayDrawPatchStretched);
end;

procedure PS_ShutDownOverlay;
begin
  FreeAndNil(overlay);
end;

procedure OVR_Drawer;
begin
  if gamestate = GS_LEVEL then
  {$IFDEF OPENGL}
    overlay.FlashToScreen32;
  {$ELSE}
    if videomode = vm32bit then
      overlay.FlashToScreen32
    else
      overlay.FlashToScreen8;
  {$ENDIF}
end;

function OVR_IsModified: Boolean;
begin
  Result := overlay.Modified;
end;

{$IFDEF OPENGL}
function OVR_OverlayHeight: Integer;
begin
  if gamestate = GS_LEVEL then
    Result := overlay.GetOverlayHeight
  else
    Result := 0;
end;
{$ENDIF}

procedure PS_OverlayClear;
begin
  if overlay <> nil then
    overlay.Clear;
end;

procedure PS_OverlayDrawPatch(const ticks: Integer; const patchname: string;
  const x, y: Integer);
begin
  if overlay <> nil then
    overlay.AddPatch(ticks, patchname, x, y);
end;

procedure PS_OverlayDrawPatchStretched(const ticks: Integer; const patchname: string;
  const x1, y1, x2, y2: Integer);
begin
  if overlay <> nil then
    overlay.AddPatchStretched(ticks, patchname, x1, y1, x2, y2);
end;

procedure PS_OverlayDrawPixel(const ticks: Integer; const red, green, blue: byte;
  const x, y: Integer);
begin
  if overlay <> nil then
    overlay.AddPixel(ticks, red, green, blue, x, y);
end;

procedure PS_OverlayDrawRect(const ticks: Integer; const red, green, blue: byte;
  const x1, y1, x2, y2: Integer);
begin
  if overlay <> nil then
    overlay.AddRect(ticks, red, green, blue, x1, y1, x2, y2);
end;

procedure PS_OverlayDrawLine(const ticks: Integer; const red, green, blue: byte;
  const x1, y1, x2, y2: Integer);
begin
  if overlay <> nil then
    overlay.AddLine(ticks, red, green, blue, x1, y1, x2, y2);
end;

procedure PS_OverlayDrawText(const ticks: Integer; const txt: string; const align: Integer;
  const x, y: Integer);
begin
  if overlay <> nil then
    overlay.AddText(ticks, txt, align, x, y);
end;

procedure PS_OverlayDrawLeftText(const ticks: Integer; const txt: string;
  const x, y: Integer);
begin
  if overlay <> nil then
    overlay.AddLeftText(ticks, txt, x, y);
end;

procedure PS_OverlayDrawRightText(const ticks: Integer; const txt: string;
  const x, y: Integer);
begin
  if overlay <> nil then
    overlay.AddRightText(ticks, txt, x, y);
end;

procedure PS_OverlayDrawCenterText(const ticks: Integer; const txt: string;
  const x, y: Integer);
begin
  if overlay <> nil then
    overlay.AddCenterText(ticks, txt, x, y);
end;

// ---------------------- REGISTRATION ----------------------------------------

procedure SIRegister_TOverlay(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('!TOBJECT'),'!TOverlay') do
  begin
    RegisterMethod('Constructor Create');
    RegisterMethod('Procedure Clear');
    RegisterMethod('Procedure DrawPatch( const ticks : Integer; const patchname : string; const x, y : Integer)');
    RegisterMethod('Procedure DrawPatchStretched( const ticks : Integer; const patchname : string; const x1, y1, x2, y2 : Integer)');
    RegisterMethod('Procedure DrawPixel( const ticks : Integer; const red, green, blue : byte; const x, y : Integer)');
    RegisterMethod('Procedure DrawRect( const ticks : Integer; const red, green, blue : byte; const x1, y1, x2, y2 : Integer)');
    RegisterMethod('Procedure DrawLine( const ticks : Integer; const red, green, blue : byte; const x1, y1, x2, y2 : Integer)');
    RegisterMethod('Procedure DrawText( const ticks : Integer; const txt : string; const align : Integer; const x, y : Integer)');
    RegisterMethod('Procedure DrawLeftText( const ticks : Integer; const txt : string; const x, y : Integer)');
    RegisterMethod('Procedure DrawRightText( const ticks : Integer; const txt : string; const x, y : Integer)');
    RegisterMethod('Procedure DrawCenterText( const ticks : Integer; const txt : string; const x, y : Integer)');
  end;
  AddImportedClassVariable(CL, 'Overlay', '!TOverlay');
end;

procedure RIRegister_TOverlay(CL: TPSRuntimeClassImporter);
begin
  with CL.Add2(TOverlayDrawer, '!TOVERLAY') do
  begin
    RegisterVirtualConstructor(@TOverlayDrawer.Create, 'Create');
    RegisterVirtualMethod(@TOverlayDrawer.Clear, 'Clear');
    RegisterMethod(@TOverlayDrawer.AddPatch, 'DrawPatch');
    RegisterMethod(@TOverlayDrawer.AddPatchStretched, 'DrawPatchStretched');
    RegisterMethod(@TOverlayDrawer.AddPixel, 'DrawPixel');
    RegisterMethod(@TOverlayDrawer.AddRect, 'DrawRect');
    RegisterMethod(@TOverlayDrawer.AddLine, 'DrawLine');
    RegisterMethod(@TOverlayDrawer.AddText, 'DrawText');
    RegisterMethod(@TOverlayDrawer.AddLeftText, 'DrawLeftText');
    RegisterMethod(@TOverlayDrawer.AddRightText, 'DrawRightText');
    RegisterMethod(@TOverlayDrawer.AddCenterText, 'DrawCenterText');
  end;
end;

// JVAL: Must be called after Loading data.
procedure RIRegisterRTL_TOverlay(Exec: TPSExec);
begin
  SetVariantToClass(Exec.GetVarNo(Exec.GetVar('Overlay')), overlay);
end;

//------------ Mobj Codepointers -----------------------------------------------
procedure A_OverlayClear;
begin
  PS_OverlayClear;
end;

procedure A_OverlayDrawPatch(actor: Pmobj_t);
var
  ticks: Integer;
  patchname: string;
  x, y: Integer;
begin
  if not P_CheckStateParams(actor, 4) then
    Exit;

  ticks := actor.state.params.IntVal[0];
  patchname := actor.state.params.strval[1];
  x := actor.state.params.IntVal[2];
  y := actor.state.params.IntVal[3];

  PS_OverlayDrawPatch(ticks, patchname, x, y);
end;

procedure A_OverlayDrawPatchStretched(actor: Pmobj_t);
var
  ticks: Integer;
  patchname: string;
  x1, y1, x2, y2: Integer;
begin
  if not P_CheckStateParams(actor, 6) then
    Exit;

  ticks := actor.state.params.IntVal[0];
  patchname := actor.state.params.strval[1];
  x1 := actor.state.params.IntVal[2];
  y1 := actor.state.params.IntVal[3];
  x2 := actor.state.params.IntVal[4];
  y2 := actor.state.params.IntVal[5];

  PS_OverlayDrawPatchStretched(ticks, patchname, x1, y1, x2, y2);
end;

procedure A_OverlayDrawPixel(actor: Pmobj_t);
var
  ticks: Integer;
  red, green, blue: byte;
  x, y: Integer;
begin
  if not P_CheckStateParams(actor, 6) then
    Exit;

  ticks := actor.state.params.IntVal[0];
  red := actor.state.params.IntVal[1];
  green := actor.state.params.IntVal[2];
  blue := actor.state.params.IntVal[3];
  x := actor.state.params.IntVal[4];
  y := actor.state.params.IntVal[5];

  PS_OverlayDrawPixel(ticks, red, green, blue, x, y);
end;

procedure A_OverlayDrawRect(actor: Pmobj_t);
var
  ticks: Integer;
  red, green, blue: byte;
  x1, y1, x2, y2: Integer;
begin
  if not P_CheckStateParams(actor, 8) then
    Exit;

  ticks := actor.state.params.IntVal[0];
  red := actor.state.params.IntVal[1];
  green := actor.state.params.IntVal[2];
  blue := actor.state.params.IntVal[3];
  x1 := actor.state.params.IntVal[4];
  y1 := actor.state.params.IntVal[5];
  x2 := actor.state.params.IntVal[6];
  y2 := actor.state.params.IntVal[7];

  PS_OverlayDrawRect(ticks, red, green, blue, x1, y1, x2, y2);
end;

procedure A_OverlayDrawLine(actor: Pmobj_t);
var
  ticks: Integer;
  red, green, blue: byte;
  x1, y1, x2, y2: Integer;
begin
  if not P_CheckStateParams(actor, 8) then
    Exit;

  ticks := actor.state.params.IntVal[0];
  red := actor.state.params.IntVal[1];
  green := actor.state.params.IntVal[2];
  blue := actor.state.params.IntVal[3];
  x1 := actor.state.params.IntVal[4];
  y1 := actor.state.params.IntVal[5];
  x2 := actor.state.params.IntVal[6];
  y2 := actor.state.params.IntVal[7];

  PS_OverlayDrawLine(ticks, red, green, blue, x1, y1, x2, y2);
end;

procedure A_OverlayDrawText(actor: Pmobj_t);
var
  ticks: Integer;
  txt: string;
  align: Integer;
  x, y: Integer;
begin
  if not P_CheckStateParams(actor, 5) then
    Exit;

  ticks := actor.state.params.IntVal[0];
  txt := actor.state.params.StrVal[1];
  align := actor.state.params.IntVal[2];
  x := actor.state.params.IntVal[3];
  y := actor.state.params.IntVal[4];

  PS_OverlayDrawText(ticks, txt, align, x, y);
end;

procedure A_OverlayDrawLeftText(actor: Pmobj_t);
var
  ticks: Integer;
  txt: string;
  x, y: Integer;
begin
  if not P_CheckStateParams(actor, 4) then
    Exit;

  ticks := actor.state.params.IntVal[0];
  txt := actor.state.params.StrVal[1];
  x := actor.state.params.IntVal[2];
  y := actor.state.params.IntVal[3];

  PS_OverlayDrawLeftText(ticks, txt, x, y);
end;

procedure A_OverlayDrawRightText(actor: Pmobj_t);
var
  ticks: Integer;
  txt: string;
  x, y: Integer;
begin
  if not P_CheckStateParams(actor, 4) then
    Exit;

  ticks := actor.state.params.IntVal[0];
  txt := actor.state.params.StrVal[1];
  x := actor.state.params.IntVal[2];
  y := actor.state.params.IntVal[3];

  PS_OverlayDrawRightText(ticks, txt, x, y);
end;

procedure A_OverlayDrawCenterText(actor: Pmobj_t);
var
  ticks: Integer;
  txt: string;
  x, y: Integer;
begin
  if not P_CheckStateParams(actor, 4) then
    Exit;

  ticks := actor.state.params.IntVal[0];
  txt := actor.state.params.StrVal[1];
  x := actor.state.params.IntVal[2];
  y := actor.state.params.IntVal[3];

  PS_OverlayDrawCenterText(ticks, txt, x, y);
end;

end.

