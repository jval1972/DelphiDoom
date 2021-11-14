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
//  Voxel stuff (software rendering)
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_voxels;

interface

uses
  d_delphi,
  m_fixed,
  r_defs,
  p_mobj_h;

var
  r_drawvoxels: boolean = true;

const
  MAXVOXELSIZE = 256;

type
  TVoxelModel = class
  private
    fnumframes: integer;
    frames: TDNumberList;
  protected
    foffset: fixed_t;
    fscale: fixed_t;
    fflags: LongWord;
    fradius: fixed_t;
  public
    constructor Create(const name: string; const offset, scale: fixed_t; flags: LongWord);
    destructor Destroy; override;
    property radius: fixed_t read fradius;
  end;

procedure R_InitVoxels;

procedure R_VoxelsDone;

procedure R_DrawVoxel(const vis: Pvissprite_t);

implementation

uses
  Math,
  doomdef,
  g_game,
  mt_utils,
  r_renderstyle,
  r_softgl,
  r_palette,
  r_utils,
  r_main,
  r_draw,
  r_things,
  r_data,
  r_bsp,
  r_segs,
  r_segs2,
  r_column,
  r_batchcolumn,
  r_colorcolumn,
  r_hires,
  r_trans8,
  r_fake3d,
  r_intrpl,
  r_3dfloors, // JVAL: 3d Floors
  r_depthbuffer, // JVAL: 3d Floors
  p_setup,
  p_pspr,
  p_tick,
  tables,
  info_h,
  vx_base,
  i_system,
  i_threads,
  sc_engine,
  v_data,
  v_video,
  w_folders,
  w_pak,
  w_wad,
  z_zone;

type
  voxelcolumn_p = ^voxelcolumn_t;
  voxelcolumn_t = record
    x, y: fixed_t;
    ix, iy: integer;
    scale: fixed_t;
    topdelta: word;
    fixeddelta: fixed_t;
    length: word;
    fixedlength: fixed_t;
    fixedheight: fixed_t;
    fixedoffset: fixed_t;
    angle: angle_t;
    dc_color: byte;
    dc_color32: LongWord;
    drawtop, drawbottom: boolean;
    next: voxelcolumn_p;
    maxlistsize: integer;
  end;

  voxelcolumn_a = array[0..$1FFF] of voxelcolumn_p;
  voxelcolumn_pa = ^voxelcolumn_a;
  voxelmip_t = record
    numcolumns: integer;
    columns: voxelcolumn_pa;
    anglesortorder: array[0..7] of PWordArray;
    columnssortorder: array[0..7] of voxelcolumn_pa;
    mipscale: integer;
  end;
  voxelmip_p = ^voxelmip_t;
  voxelcolumns_t = record
    mips: array[0..2] of voxelmip_t;
    range: fixed_t;
  end;
  voxelcolumns_p = ^voxelcolumns_t;

var
  vx_membuffer: PByteArray = nil;

type
  voxelitem_t = packed record
    color: LongWord;
    skip: boolean;
  end;
  voxelitem_p = ^voxelitem_t;
  voxelbuffer_t = array[0..MAXVOXELSIZE - 1, -1..MAXVOXELSIZE - 1, 0..MAXVOXELSIZE - 1] of voxelitem_t;
  voxelbuffer_p = ^voxelbuffer_t;
  voxelbuffer2D_t = array[0..MAXVOXELSIZE - 1] of voxelitem_t;
  voxelbuffer2D_p = ^voxelbuffer2D_t;

type
  rgb_t = record
    r, g, b: Byte;
  end;

//
// JVAL
// R_VoxelColumnFromBuffer()
// Create a vertical column from a voxelbuffer
//
function R_VoxelColumnFromBuffer(const buf: voxelbuffer2D_p; const size: integer; const mip: integer): voxelcolumn_p;
var
  i, j: integer;
  rover: voxelcolumn_p;
  r1, parent: voxelcolumn_p;
  pal: palette_p;
  source: array[0..MAXVOXELSIZE] of byte;
  source32: array[0..MAXVOXELSIZE] of LongWord;
  lastc: byte;
  col, last: voxelcolumn_p;
  buf2: voxelbuffer2D_p;
  size2: integer;
  rgb: array[0..15] of rgb_t;
  clrs: integer;
  _lo, _hi: integer;
  _r, _g, _b: LongWord;
  c: LongWord;
  maxlistsize: byte;
begin
  pal := R_DefaultPalette;

  if mip <> 1 then
  begin
    size2 := size div mip;
    buf2 := malloc(size2 * SizeOf(voxelitem_t));
    i := 0;
    while i < size2 do
    begin
      clrs := 0;
      _lo := i * mip;
      if i = size2 - 1 then
        _hi := size - 1
      else
        _hi := _lo + mip - 1;
      for j := _lo to _hi do
      begin
        c := buf[j].color;
        if c <> 0 then
        begin
          rgb[clrs].r := c shr 16;
          rgb[clrs].g := c shr 8;
          rgb[clrs].b := c;
          inc(clrs);
        end;
      end;
      if clrs > 0 then
      begin
        _r := 0;
        _g := 0;
        _b := 0;
        for j := 0 to clrs - 1 do
        begin
          _r := _r + rgb[j].r;
          _g := _g + rgb[j].g;
          _b := _b + rgb[j].b;
        end;
        _r := Round(_r / clrs);
        _g := Round(_g / clrs);
        _b := Round(_b / clrs);
        if _r > 255 then
          _r := 255;
        if _g > 255 then
          _g := 255;
        if _b > 255 then
          _b := 255;
        buf2[i].color := _r shl 16 + _g shl 8 + _b;
      end
      else
        buf2[i].color := 0;

      Inc(i);
    end;
  end
  else
  begin
    size2 := size;
    buf2 := buf;
  end;

  i := 0;
  result := nil;
  while i < size2 do
  begin
    if buf2[i].color <> 0 then
    begin
      rover := Z_Malloc(SizeOf(voxelcolumn_t), PU_STATIC, nil);
      rover.topdelta := i * mip;
      rover.fixeddelta := rover.topdelta * FRACUNIT;
      rover.next := nil;
      if result = nil then
        result := rover
      else
      begin
        r1 := result;
        parent := nil;
        while r1 <> nil do
        begin
          parent := r1;
          r1 := r1.next;
        end;
        parent.next := rover;
      end;
      j := 0;
      lastc := 0;
      while (buf2[i].color <> 0) and (i < size2) do
      begin
        source32[j] := buf2[i].color;
        source[j] := V_FindAproxColorIndex(PLongWordArray(pal), source32[j], 1, 255);
        if source[j] <> lastc then
          if j <> 0 then
            Break;
        lastc := source[j];
        inc(i);
        inc(j);
      end;
      if j > 128 then
        j := 128;
      rover.length := j * mip;
      rover.fixedlength := rover.length * FRACUNIT;
      rover.fixedheight := size * FRACUNIT;
      rover.fixedoffset := rover.fixedheight - rover.fixeddelta;
      rover.dc_color := source[0];
      rover.dc_color32 := source32[0];
    end
    else
      inc(i);
  end;

  if mip <> 1 then
    memfree(Pointer(buf2), size2 * SizeOf(voxelitem_t));

  col := result;
  last := nil;
  while col <> nil do
  begin
    col.drawtop := false;
    col.drawbottom := false;
    if last = nil then
      col.drawtop := true
    else if last.topdelta + last.length < col.topdelta then
    begin
      col.drawtop := true;
      last.drawbottom := true;
    end;
    last := col;
    col := col.next;
  end;

  if last <> nil then
    last.drawbottom := true;

  maxlistsize := 0;
  rover := result;
  while rover <> nil do
  begin
    if rover.length > maxlistsize then
      maxlistsize := rover.length;
    rover := rover.next;
  end;

  rover := result;
  while rover <> nil do
  begin
    rover.maxlistsize := maxlistsize;
    rover := rover.next;
  end;

end;

procedure R_ClearVoxelBuffer(const voxelbuffer: voxelbuffer_p; const voxelsize: integer);
var
  xx, yy, zz: integer;
  vp: voxelitem_p;
begin
  if voxelsize = MAXVOXELSIZE then
  begin
    MT_ZeroMemory(voxelbuffer, SizeOf(voxelbuffer_t))
  end
  else
  begin
    for xx := 0 to voxelsize - 1 do
      for yy := -1 to voxelsize - 1 do
      begin
        vp := @voxelbuffer[xx, yy, 0];
        for zz := 0 to voxelsize - 1 do
        begin
          vp.color := 0;
          vp.skip := false;
          Inc(vp);
        end;
      end;
  end;
end;

type
  mt_struct1_t = record
    voxelbuffer: voxelbuffer_p;
    voxelsize: integer;
    start, finish: integer;
  end;
  mt_struct1_p = ^mt_struct1_t;

function _mt_prepare_optimize_voxel_buffer(r: mt_struct1_p): integer; stdcall;
var
  xx, yy, zz: integer;
  voxelsize: integer;
  voxelbuffer: voxelbuffer_p;
begin
  voxelsize := r.voxelsize;
  voxelbuffer := r.voxelbuffer;
  for xx := 1 to voxelsize - 2 do
    for yy := 1 to voxelsize - 2 do
      for zz := r.start to r.finish do
      begin
        if voxelbuffer[xx, yy, zz].color <> 0 then
          if (voxelbuffer[xx - 1, yy, zz].color <> 0) and
             (voxelbuffer[xx + 1, yy, zz].color <> 0) and
             (voxelbuffer[xx, yy - 1, zz].color <> 0) and
             (voxelbuffer[xx, yy + 1, zz].color <> 0) and
             (voxelbuffer[xx, yy, zz - 1].color <> 0) and
             (voxelbuffer[xx, yy, zz + 1].color <> 0) then
          voxelbuffer[xx, yy, zz].skip := true;
      end;
  result := 0;
end;

procedure R_OptimizeVoxelBuffer(const voxelbuffer: voxelbuffer_p; const voxelsize: integer);
var
  xx, yy, zz: integer;
  r1, r2, r3, r4: mt_struct1_t;
begin
  if not usemultithread or (voxelsize < 32) then
  begin
    for xx := 1 to voxelsize - 2 do
      for yy := 1 to voxelsize - 2 do
        for zz := 1 to voxelsize - 2 do
        begin
          if voxelbuffer[xx, yy, zz].color <> 0 then
            if (voxelbuffer[xx - 1, yy, zz].color <> 0) and
               (voxelbuffer[xx + 1, yy, zz].color <> 0) and
               (voxelbuffer[xx, yy - 1, zz].color <> 0) and
               (voxelbuffer[xx, yy + 1, zz].color <> 0) and
               (voxelbuffer[xx, yy, zz - 1].color <> 0) and
               (voxelbuffer[xx, yy, zz + 1].color <> 0) then
            voxelbuffer[xx, yy, zz].skip := true;
        end;
  end
  else
  begin
    r1.voxelbuffer := voxelbuffer;
    r1.voxelsize := voxelsize;
    r1.start := 1;
    r1.finish := voxelsize div 4;
    r2.voxelbuffer := voxelbuffer;
    r2.voxelsize := voxelsize;
    r2.start := r1.finish + 1;
    r2.finish := voxelsize div 2;
    r3.voxelbuffer := voxelbuffer;
    r3.voxelsize := voxelsize;
    r3.start := r2.finish + 1;
    r3.finish := voxelsize div 2 + voxelsize div 4;
    r4.voxelbuffer := voxelbuffer;
    r4.voxelsize := voxelsize;
    r4.start := r3.finish + 1;
    r4.finish := voxelsize - 2;
    MT_Execute4(
      @_mt_prepare_optimize_voxel_buffer, @r1,
      @_mt_prepare_optimize_voxel_buffer, @r2,
      @_mt_prepare_optimize_voxel_buffer, @r3,
      @_mt_prepare_optimize_voxel_buffer, @r4);
  end;

  for xx := 1 to voxelsize - 2 do
    for yy := 1 to voxelsize - 2 do
      for zz := 1 to voxelsize - 2 do
        if voxelbuffer[xx, yy, zz].skip then
          voxelbuffer[xx, yy, zz].color := 0;

end;

function _mt_prepare_voxel_columns(r: mt_struct1_p): integer; stdcall;
var
  xx, yy, zz: integer;
  skip: boolean;
  voxelsize: integer;
  voxelbuffer: voxelbuffer_p;
begin
  voxelsize := r.voxelsize;
  voxelbuffer := r.voxelbuffer;
  for xx := r.start to r.finish - 1 do
    for zz := 0 to voxelsize - 1 do
    begin
      skip := true;
      for yy := 0 to voxelsize - 1 do
        if voxelbuffer[xx, yy, zz].color <> 0 then
        begin
          skip := False;
          break;
        end;
      voxelbuffer[xx, -1, zz].skip := skip;
    end;
  result := 0;
end;

procedure R_PrepareVoxelColumns(const voxelbuffer: voxelbuffer_p; const voxelsize: integer);
var
  xx, yy, zz: integer;
  skip: boolean;
  r1, r2, r3, r4: mt_struct1_t;
begin
  if not usemultithread or (voxelsize < 32) then
  begin
    for xx := 0 to voxelsize - 1 do
      for zz := 0 to voxelsize - 1 do
      begin
        skip := true;
        for yy := 0 to voxelsize - 1 do
          if voxelbuffer[xx, yy, zz].color <> 0 then
          begin
            skip := False;
            break;
          end;
        voxelbuffer[xx, -1, zz].skip := skip;
      end;
  end
  else
  begin
    r1.voxelbuffer := voxelbuffer;
    r1.voxelsize := voxelsize;
    r1.start := 0;
    r1.finish := voxelsize div 4;
    r2.voxelbuffer := voxelbuffer;
    r2.voxelsize := voxelsize;
    r2.start := r1.finish + 1;
    r2.finish := voxelsize div 2;
    r3.voxelbuffer := voxelbuffer;
    r3.voxelsize := voxelsize;
    r3.start := r2.finish + 1;
    r3.finish := voxelsize div 2 + voxelsize div 4;
    r4.voxelbuffer := voxelbuffer;
    r4.voxelsize := voxelsize;
    r4.start := r3.finish + 1;
    r4.finish := voxelsize - 1;
    MT_Execute4(
      @_mt_prepare_voxel_columns, @r1,
      @_mt_prepare_voxel_columns, @r2,
      @_mt_prepare_voxel_columns, @r3,
      @_mt_prepare_voxel_columns, @r4);
  end;
end;

type
  distancetableitem_t = record
    dist: LongWord;
    idx: integer;
  end;

  distancetable_t = array[0..$1FFF] of distancetableitem_t;
  distancetable_p = ^distancetable_t;

procedure R_QSortDistanceTable(const A: distancetable_p; const len: Integer);

  procedure qsorttbl(l, r: Integer);
  var
    i, j: Integer;
    t: distancetableitem_t;
    dist: LongWord;
  begin
    repeat
      i := l;
      j := r;
      dist := A[(l + r) shr 1].dist;
      repeat
        while A[i].dist < dist do
          inc(i);
        while A[j].dist > dist do
          dec(j);
        if i <= j then
        begin
          t := A[i];
          A[i] := A[j];
          A[j] := t;
          inc(i);
          dec(j);
        end;
      until i > j;
      if l < j then
        qsorttbl(l, j);
      l := i;
    until i >= r;
  end;

begin
  if len > 1 then
    qsorttbl(0, len - 1);
end;

function R_CreateVoxelMipSortOrderAngle(const mip: voxelmip_p; const an: integer): boolean;
var
  i: integer;
  distancetable: distancetable_p;  // Holds square distance
  rx, ry: integer;
  dx, dy: integer;
begin
  case an of
    0:
      begin
        rx := 0;
        ry := MAXVOXELSIZE;
      end;
    1:
      begin
        rx := -MAXVOXELSIZE;
        ry := MAXVOXELSIZE;
      end;
    2:
      begin
        rx := -MAXVOXELSIZE;
        ry := 0;
      end;
    3:
      begin
        rx := -MAXVOXELSIZE;
        ry := -MAXVOXELSIZE;
      end;
    4:
      begin
        rx := 0;
        ry := -MAXVOXELSIZE;
      end;
    5:
      begin
        rx := MAXVOXELSIZE;
        ry := -MAXVOXELSIZE;
      end;
    6:
      begin
        rx := MAXVOXELSIZE;
        ry := 0;
      end;
    7:
      begin
        rx := MAXVOXELSIZE;
        ry := MAXVOXELSIZE;
      end;
    else
      begin
        result := false;
        exit;
      end;
  end;

  mip.anglesortorder[an] := Z_Malloc(mip.numcolumns * SizeOf(word), PU_STATIC, nil);
  distancetable := malloc(mip.numcolumns * SizeOf(distancetableitem_t));

  for i := 0 to mip.numcolumns - 1 do
  begin
    dx := rx - mip.columns[i].ix;
    dy := ry - mip.columns[i].iy;
    distancetable[i].dist := dx * dx + dy * dy;
    distancetable[i].idx := i;
  end;

  R_QSortDistanceTable(distancetable, mip.numcolumns);

  for i := mip.numcolumns - 1 downto 0 do
    mip.anglesortorder[an][i] := distancetable[i].idx;

  mip.columnssortorder[an] := Z_Malloc(mip.numcolumns * SizeOf(voxelcolumn_p), PU_STATIC, nil);
  for i := 0 to mip.numcolumns - 1 do
    mip.columnssortorder[an][i] := mip.columns[mip.anglesortorder[an][i]];

  memfree(pointer(distancetable), mip.numcolumns * SizeOf(distancetableitem_t));

  result := true;
end;

procedure R_CreateVoxelMipSortOrder(const mip: voxelmip_p);
var
  i: integer;
begin
  if mip.numcolumns = 0 then
    Exit;

  for i := 0 to 7 do
    R_CreateVoxelMipSortOrderAngle(mip, i);
end;

function R_VoxelColumnsFromBuffer(const voxelbuffer: voxelbuffer_p; const voxelsize: integer; const offset, scale: fixed_t): voxelcolumns_p;
var
  xx, yy, zz: integer;
  i, j: integer;
  vbuf: voxelbuffer2D_p;
  mid: integer;
  col: voxelcolumn_p;
  buf: TDNumberList;
  rgb: array[0..9] of rgb_t;
  c: LongWord;
  blacks: integer;
  dist, maxdist: extended;
  skip: boolean;
  sz: integer;
begin
  result := Z_Malloc(SizeOf(voxelcolumns_t), PU_STATIC, nil);
  vbuf := malloc(SizeOf(voxelbuffer2D_t));
  buf := TDNumberList.Create;
  mid := voxelsize div 2;

  maxdist := 0;
  for xx := 0 to voxelsize - 1 do
    for zz := 0 to voxelsize - 1 do
      if not voxelbuffer[xx, -1, zz].skip then
      begin
        for yy := 0 to voxelsize - 1 do
          vbuf[yy] := voxelbuffer[xx, yy, zz];
        col := R_VoxelColumnFromBuffer(vbuf, voxelsize, 1);
        if col <> nil then
        begin
          col.ix := xx - mid;
          col.iy := zz - mid;
          col.x := (xx - mid) * scale;
          col.y := (zz - mid) * scale;
          dist := Sqr(col.x / FRACUNIT) + Sqr(col.y / FRACUNIT);
          if dist > maxdist then
            maxdist := dist;
          col.angle := Round(arctan2(col.y, col.x) * (ANG180 / D_PI));
          col.scale := scale;
          buf.Add(integer(col))
        end;
      end;
  result.mips[0].numcolumns := buf.Count;
  result.mips[0].mipscale := 1;
  result.range := Round(Sqrt(maxdist) + 1) * FRACUNIT;
  if buf.Count > 0 then
  begin
    sz := result.mips[0].numcolumns * SizeOf(voxelcolumn_p);
    result.mips[0].columns := Z_Malloc(sz, PU_STATIC, nil);
    memcpy(@result.mips[0].columns[0], buf.List, sz);
{    for i := 0 to buf.Count - 1 do
      result.mips[0].columns[i] := voxelcolumn_p(buf.Numbers[i]);}
  end
  else
    result.mips[0].columns := nil;

  buf.Clear;

  for xx := 0 to voxelsize - 1 do
    for zz := 0 to voxelsize - 1 do
    begin
      if Odd(xx) and Odd(zz) then
      begin
        skip := voxelbuffer[xx - 1, -1, zz - 1].skip and
                voxelbuffer[xx - 1, -1, zz].skip and
                voxelbuffer[xx, -1, zz - 1].skip and
                voxelbuffer[xx, -1, zz].skip;
        if not skip then
        begin
          for yy := 0 to voxelsize - 1 do
          begin
            blacks := 0;
            c := voxelbuffer[xx - 1, yy, zz - 1].color;
            if c = 0 then
            begin
              inc(blacks);
              rgb[0].r := 0;
              rgb[0].g := 0;
              rgb[0].b := 0;
            end
            else
            begin
              rgb[0].r := c shr 16;
              rgb[0].g := c shr 8;
              rgb[0].b := c;
            end;
            c := voxelbuffer[xx - 1, yy, zz].color;
            if c = 0 then
            begin
              inc(blacks);
              rgb[1].r := 0;
              rgb[1].g := 0;
              rgb[1].b := 0;
            end
            else
            begin
              rgb[1].r := c shr 16;
              rgb[1].g := c shr 8;
              rgb[1].b := c;
            end;
            c := voxelbuffer[xx, yy, zz - 1].color;
            if c = 0 then
            begin
              inc(blacks);
              rgb[2].r := 0;
              rgb[2].g := 0;
              rgb[2].b := 0;
            end
            else
            begin
              rgb[2].r := c shr 16;
              rgb[2].g := c shr 8;
              rgb[2].b := c;
            end;
            c := voxelbuffer[xx, yy, zz].color;
            if c = 0 then
            begin
              inc(blacks);
              rgb[3].r := 0;
              rgb[3].g := 0;
              rgb[3].b := 0;
            end
            else
            begin
              rgb[3].r := c shr 16;
              rgb[3].g := c shr 8;
              rgb[3].b := c;
            end;
            if blacks < 4 then
            begin
              c := Round((rgb[0].r + rgb[1].r + rgb[2].r + rgb[3].r) / (4 - blacks));
              if c > 255 then
                c := 255;
              rgb[4].r := c;
              c := Round((rgb[0].g + rgb[1].g + rgb[2].g + rgb[3].g) / (4 - blacks));
              if c > 255 then
                c := 255;
              rgb[4].g := c;
              c := Round((rgb[0].b + rgb[1].b + rgb[2].b + rgb[3].b) / (4 - blacks));
              if c > 255 then
                c := 255;
              rgb[4].b := c;
              vbuf[yy].color := rgb[4].r shl 16 + rgb[4].g shl 8 + rgb[4].b;
            end
            else
            begin
              vbuf[yy].color := 0;
            end;
          end;

          col := R_VoxelColumnFromBuffer(vbuf, voxelsize, 2);
          if col <> nil then
          begin
            col.ix := xx - mid;
            col.iy := zz - mid;
            col.x := (xx - mid) * scale;
            col.y := (zz - mid) * scale;
            col.angle := Round(arctan2(col.y, col.x) * (ANG180 / D_PI));
            col.scale := scale;
            buf.Add(integer(col));
          end;
        end;
      end;
    end;
  result.mips[1].numcolumns := buf.Count;
  result.mips[1].mipscale := 2;
  if buf.Count > 0 then
  begin
    sz := result.mips[1].numcolumns * SizeOf(voxelcolumn_p);
    result.mips[1].columns := Z_Malloc(sz, PU_STATIC, nil);
    memcpy(@result.mips[1].columns[0], buf.List, sz);
{    for i := 0 to buf.Count - 1 do
      result.mips[1].columns[i] := voxelcolumn_p(buf.Numbers[i]);}
  end
  else
    result.mips[1].columns := nil;

  buf.Clear;

  for xx := 0 to voxelsize - 1 do
    for zz := 0 to voxelsize - 1 do
    begin
      if ((xx mod 3) = 2) and ((zz mod 3) = 2) then
      begin

        skip := true;

        for j := 0 to 8 do
        begin
          skip := skip and voxelbuffer[xx - (j div 3), -1, zz - (j mod 3)].skip;
          if not skip then
            break;
        end;

        if not skip then
        begin
          for yy := 0 to voxelsize - 1 do
          begin
            blacks := 0;
            for j := 0 to 8 do
            begin
              c := voxelbuffer[xx - (j div 3), yy, zz - (j mod 3)].color;
              if c = 0 then
              begin
                inc(blacks);
                rgb[j].r := 0;
                rgb[j].g := 0;
                rgb[j].b := 0;
              end
              else
              begin
                rgb[j].r := c shr 16;
                rgb[j].g := c shr 8;
                rgb[j].b := c;
              end;
            end;
            if blacks < 9 then
            begin
              c := 0;
              for j := 0 to 8 do
                c := c + rgb[j].r;
              c := Round(c / (9 - blacks));
              if c > 255 then
                c := 255;
              rgb[9].r := c;
              c := 0;
              for j := 0 to 8 do
                c := c + rgb[j].g;
              c := Round(c / (9 - blacks));
              if c > 255 then
                c := 255;
              rgb[9].g := c;
              c := 0;
              for j := 0 to 8 do
                c := c + rgb[j].b;
              c := Round(c / (9 - blacks));
              if c > 255 then
                c := 255;
              rgb[9].b := c;
              vbuf[yy].color := rgb[9].r shl 16 + rgb[9].g shl 8 + rgb[9].b;
            end
            else
            begin
              vbuf[yy].color := 0;
            end;
          end;

          col := R_VoxelColumnFromBuffer(vbuf, voxelsize, 3);
          if col <> nil then
          begin
            col.ix := xx - mid;
            col.iy := zz - mid;
            col.x := (xx - mid) * scale;
            col.y := (zz - mid) * scale;
            col.angle := Round(arctan2(col.y, col.x) * (ANG180 / D_PI));
            col.scale := scale;
            buf.Add(integer(col));
          end;
        end;
      end;
    end;
  result.mips[2].numcolumns := buf.Count;
  result.mips[2].mipscale := 3;
  if buf.Count > 0 then
  begin
    sz := result.mips[2].numcolumns * SizeOf(voxelcolumn_p);
    result.mips[2].columns := Z_Malloc(sz, PU_STATIC, nil);
    memcpy(@result.mips[2].columns[0], buf.List, sz);
{    result.mips[2].columns := Z_Malloc(result.mips[2].numcolumns * SizeOf(voxelcolumn_p), PU_STATIC, nil);
    for i := 0 to buf.Count - 1 do
      result.mips[2].columns[i] := voxelcolumn_p(buf.Numbers[i]);}
  end
  else
    result.mips[2].columns := nil;

  for i := 0 to 2 do
    R_CreateVoxelMipSortOrder(@result.mips[i]);

  buf.Free;
  memfree(Pointer(vbuf), SizeOf(voxelbuffer2D_t));
end;

function SwapRGB(const c: LongWord): LongWord;
var
  r, g, b: byte;
begin
  r := c shr 16;
  g := c shr 8;
  b := c;
  result := b shl 16 + g shl 8 + r;
end;

function R_LoadDDVOX(const fname: string; const offset, scale: fixed_t): voxelcolumns_p;
var
  buf: TDStringList;
  sc: TScriptEngine;
  xx, yy, zz: integer;
  strm: TPakStream;
  voxelbuffer: voxelbuffer_p;
  voxelsize: integer;
begin
  strm := TPakStream.Create(fname, pm_prefered, gamedirectories, FOLDER_VOXELS);
  if strm.IOResult <> 0 then
  begin
    strm.Free;
    result := nil;
    Exit;
  end;

  buf := TDStringList.Create;
  buf.LoadFromStream(strm);
  strm.Free;
  sc := TScriptEngine.Create(buf.Text);
  buf.free;

  sc.MustGetInteger;
  voxelsize := sc._Integer;

  voxelbuffer := @vx_membuffer[0];
  R_ClearVoxelBuffer(voxelbuffer, voxelsize);

  xx := 0;
  yy := 0;
  zz := 0;
  while sc.GetString do
  begin
    if sc.MatchString('skip') then
    begin
      sc.MustGetInteger;
      inc(zz, sc._Integer);
    end
    else
    begin
      sc.UnGet;
      sc.MustGetInteger;
      voxelbuffer[xx, yy, zz].color := SwapRGB(sc._Integer);
      inc(zz);
    end;
    if zz = voxelsize then
    begin
      zz := 0;
      inc(yy);
      if yy = voxelsize then
      begin
        yy := 0;
        inc(xx);
        if xx = voxelsize then
          Break;
      end;
    end;
  end;

  sc.Free;

  R_OptimizeVoxelBuffer(voxelbuffer, voxelsize);
  R_PrepareVoxelColumns(voxelbuffer, voxelsize);
  result := R_VoxelColumnsFromBuffer(voxelbuffer, voxelsize, offset, scale);
end;

type
  ddmeshitem_t = packed record
    x, y, z: byte;
    color: LongWord;
  end;
  ddmeshitem_p = ^ddmeshitem_t;
  ddmeshitem_a = array[0..$FFF] of ddmeshitem_t;
  ddmeshitem_pa = ^ddmeshitem_a;

function R_LoadDDMESH(const fname: string; const offset, scale: fixed_t): voxelcolumns_p;
var
  strm: TPakStream;
  voxelbuffer: voxelbuffer_p;
  voxelsize: integer;
  i: integer;
  HDR: LongWord;
  version: integer;
  numquads: integer;
  fnumvoxels: Integer;
  buf: ddmeshitem_pa;
  item: ddmeshitem_p;
begin
  strm := TPakStream.Create(fname, pm_prefered, gamedirectories, FOLDER_VOXELS);
  if strm.IOResult <> 0 then
  begin
    strm.Free;
    result := nil;
    Exit;
  end;

  strm.Read(HDR, SizeOf(LongWord));
  if HDR <> Ord('D') + Ord('D') shl 8 + Ord('M') shl 16 + Ord('S') shl 24 then
  begin
    I_Warning('R_LoadDDMESH(): File %s does not have DDMESH magic header!', [fname]);
    strm.Free;
    result := nil;
    Exit;
  end;

  strm.Read(version, SizeOf(integer));
  if version <> 1 then
  begin
    I_Warning('R_LoadDDMESH(): File %s is from unsupported version = %d!', [fname, version]);
    strm.Free;
    result := nil;
    Exit;
  end;

  strm.Read(voxelsize, SizeOf(integer));

  strm.Read(numquads, SizeOf(integer));

  voxelbuffer := @vx_membuffer[0];
  R_ClearVoxelBuffer(voxelbuffer, voxelsize);

  // Skip OpenGL data
  strm.Seek(16 + numquads * (4 * 3 * SizeOf(Integer) + SizeOf(LongWord)), sFromBeginning);

  strm.Read(fnumvoxels, SizeOf(integer));

  buf := malloc(fnumvoxels * SizeOf(ddmeshitem_t));

  strm.Read(buf^, fnumvoxels * SizeOf(ddmeshitem_t));
  strm.Free;

  item := @buf[0];
  for i := 0 to fnumvoxels - 1 do
  begin
    voxelbuffer[item.x, item.y, item.z].color := SwapRGB(item.color);
    Inc(item);
  end;

  memfree(pointer(buf), fnumvoxels * SizeOf(ddmeshitem_t));

  // We don't call R_OptimizeVoxelBuffer(), mesh is already optimized
  R_PrepareVoxelColumns(voxelbuffer, voxelsize);
  result := R_VoxelColumnsFromBuffer(voxelbuffer, voxelsize, offset, scale);
end;


const
  MAXKVXSIZE = 256;

type
  kvxbuffer_t = array[0..MAXKVXSIZE - 1, 0..MAXKVXSIZE - 1, 0..MAXKVXSIZE - 1] of word;
  kvxbuffer_p = ^kvxbuffer_t;

type
  kvxslab_t = record
    ztop: byte;     // starting z coordinate of top of slab
    zleng: byte;    // # of bytes in the color array - slab height
    backfacecull: byte;  // low 6 bits tell which of 6 faces are exposed
    col: array[0..255] of byte;// color data from top to bottom
  end;
  kvxslab_p = ^kvxslab_t;

function R_LoadKVX(const fn: string; const offset, scale: fixed_t): voxelcolumns_p;
var
  strm: TDStream;
  pal: array[0..255] of LongWord;
  i: integer;
  x1, y1, z1: integer;
  r, g, b: byte;
  buf: PByteArray;
  numbytes: integer;
  xsiz, ysiz, zsiz, xpivot, ypivot, zpivot: integer;
  xoffset: PIntegerArray;
  xyoffset: PSmallIntPArray;
  voxdata: PByteArray;
  xx, yy, zz: integer;
  slab: kvxslab_p;
  kvxbuffer: kvxbuffer_p;
  voxdatasize: integer;
  offs: integer;
  endptr: PByte;
  voxelbuffer: voxelbuffer_p;
  voxelsize: integer;
  idx: integer;
  maxpal: integer;
  cc: integer;
  palfactor: double;
  lump: integer;
  len: integer;
  s1, s2, s3: string;
begin
  strm := TPakStream.Create(fn, pm_prefered, gamedirectories, FOLDER_VOXELS);
  if strm.IOResult <> 0 then
  begin
    strm.Free;
    strm := TPakStream.Create(fn, pm_directory, '', FOLDER_VOXELS);
  end;
  if strm.IOResult <> 0 then
  begin
    strm.Free;
    s1 := fname(fn);
    splitstring(s1, s2, s3, '.');
    lump := W_CheckNumForName(s2, TYPE_VOXEL);
    if lump < 0 then
    begin
      result := nil;
      Exit;
    end;
    len := W_LumpLength(lump);
    buf := malloc(len);
    W_ReadLump(lump, buf);
    strm := TDMemoryStream.Create;
    strm.Write(buf^, len);
    strm.Seek(0, sFromBeginning);
    memfree(pointer(buf), len);
  end;

  if strm.Size < 768 + 28 then
  begin
    strm.Free;
    result := nil;
    Exit;
  end;

  strm.Seek(768, sFromEnd);
  maxpal := 0;
  for i := 0 to 255 do
  begin
    strm.Read(r, SizeOf(Byte));
    if r > maxpal then
      maxpal := r;
    strm.Read(g, SizeOf(Byte));
    if g > maxpal then
      maxpal := g;
    strm.Read(b, SizeOf(Byte));
    if b > maxpal then
      maxpal := b;
    pal[i] := r shl 16 + g shl 8 + b;
    if pal[i] = 0 then
      pal[i] := $01;
  end;
  if (maxpal < 255) and (maxpal > 0) then
  begin
    palfactor := 255 / maxpal;
    if palfactor > 4.0 then
      palfactor := 4.0;
    for i := 0 to 255 do
    begin
      r := pal[i] shr 16;
      g := pal[i] shr 8;
      b := pal[i];
      cc := round(palfactor * r);
      if cc < 0 then
        cc := 0
      else if cc > 255 then
        cc := 255;
      r := cc;
      cc := round(palfactor * g);
      if cc < 0 then
        cc := 0
      else if cc > 255 then
        cc := 255;
      g := cc;
      cc := round(palfactor * b);
      if cc < 0 then
        cc := 0
      else if cc > 255 then
        cc := 255;
      b := cc;
      pal[i] := r shl 16 + g shl 8 + b;
    end;
  end;

  strm.Seek(0, sFromBeginning);
  strm.Read(numbytes, SizeOf(Integer));
  strm.Read(xsiz, SizeOf(Integer));
  strm.Read(ysiz, SizeOf(Integer));
  strm.Read(zsiz, SizeOf(Integer));
  strm.Read(xpivot, SizeOf(Integer));
  strm.Read(ypivot, SizeOf(Integer));
  strm.Read(zpivot, SizeOf(Integer));
  xoffset := malloc((xsiz + 1) * SizeOf(Integer));
  xyoffset := malloc(xsiz * SizeOf(PSmallIntArray));
  for i := 0 to xsiz - 1 do
    xyoffset[i] := malloc((ysiz + 1) * SizeOf(SmallInt));
  strm.Read(xoffset^, (xsiz + 1) * SizeOf(Integer));
  for i := 0 to xsiz - 1 do
    strm.Read(xyoffset[i]^, (ysiz + 1) * SizeOf(SmallInt));
  offs := xoffset[0];
  voxdatasize := numbytes - 24 - (xsiz + 1) * 4 - xsiz * (ysiz + 1) * 2;
  voxdata := malloc(voxdatasize);
  strm.Read(voxdata^, voxdatasize);
  strm.Free;

  kvxbuffer := @vx_membuffer[0];

  for xx := 0 to xsiz - 1 do
    for yy := 0 to ysiz - 1 do
      for zz := 0 to zsiz - 1 do
        kvxbuffer[xx, yy, zz] := $FFFF;

  for xx := 0 to xsiz - 1 do
  begin
    for yy := 0 to ysiz - 1 do
    begin
      endptr := @voxdata[xoffset[xx] + xyoffset[xx][yy + 1] - offs];
      slab := @voxdata[xoffset[xx] + xyoffset[xx][yy] - offs];
      while Integer(slab) < integer(endptr) do
      begin
        for zz := slab.ztop to slab.zleng + slab.ztop - 1 do
          kvxbuffer[xx, yy, zz] := slab.col[zz - slab.ztop];
        slab := kvxslab_p(integer(slab) + slab.zleng + 3);
      end;
    end;
  end;

  voxelsize := xsiz;
  if voxelsize < ysiz then
    voxelsize := ysiz;
  if voxelsize < zsiz then
    voxelsize := zsiz;
  if voxelsize < 256 then
    inc(voxelsize);
{  if voxelsize < 256 then
  begin
    inc(voxelsize, 2);
    voxelsize := voxelsize and not 1;
  end;}

  idx := SizeOf(kvxbuffer_t);
  voxelbuffer := @vx_membuffer[idx];
  R_ClearVoxelBuffer(voxelbuffer, voxelsize);

  x1 := voxelsize div 2 - xpivot div 256;
  y1 := voxelsize div 2 - ypivot div 256;
  z1 := voxelsize - zpivot div 256; // JVAL: Align down
  if x1 < 0 then
    x1 := 0;
  if y1 < 0 then
    y1 := 0;
  if z1 < 0 then
    z1 := 0;
  while x1 + xsiz >= voxelsize do
    dec(x1);
  while y1 + ysiz >= voxelsize do
    dec(y1);
  while z1 + zsiz >= voxelsize do
    dec(z1);

  for xx := x1 to x1 + xsiz - 1 do
    for yy := y1 to y1 + ysiz - 1 do
      for zz := z1 to z1 + zsiz - 1 do
        if kvxbuffer[xx - x1, yy - y1, zz - z1] <> $FFFF then
          voxelbuffer[xx, zz, voxelsize - yy - 1].color := pal[kvxbuffer[xx - x1, yy - y1, zz - z1]];


  R_OptimizeVoxelBuffer(voxelbuffer, voxelsize);
  R_PrepareVoxelColumns(voxelbuffer, voxelsize);
  result := R_VoxelColumnsFromBuffer(voxelbuffer, voxelsize, offset, scale);

  for i := 0 to xsiz - 1 do
    memfree(pointer(xyoffset[i]), (ysiz + 1) * SizeOf(SmallInt));
  memfree(pointer(xoffset), (xsiz + 1) * SizeOf(Integer));
  memfree(pointer(xyoffset), xsiz * SizeOf(PSmallIntArray));
  memfree(pointer(voxdata), voxdatasize);
end;

//
// R_LoadSlab6VOX
// JVAL 20191004 Support for slab6 VOX files
//
function R_LoadSlab6VOX(const fn: string; const offset, scale: fixed_t): voxelcolumns_p;
var
  strm: TDStream;
  pal: array[0..255] of LongWord;
  i: integer;
  r, g, b: byte;
  xsiz, ysiz, zsiz: integer;
  voxdatasize: integer;
  voxdata: PByteArray;
  voxelbuffer: voxelbuffer_p;
  voxelsize: integer;
  xx, yy, zz: integer;
  x1, y1, z1: integer;
  maxpal: integer;
  cc: integer;
  palfactor: double;
begin
  strm := TPakStream.Create(fn, pm_prefered, gamedirectories, FOLDER_VOXELS);
  if strm.IOResult <> 0 then
  begin
    strm.Free;
    result := nil;
    Exit;
  end;

  if strm.Size < 768 + 12 then
  begin
    strm.Free;
    result := nil;
    Exit;
  end;

  strm.Seek(768, sFromEnd);
  maxpal := 0;
  for i := 0 to 255 do
  begin
    strm.Read(r, SizeOf(Byte));
    if r > maxpal then
      maxpal := r;
    strm.Read(g, SizeOf(Byte));
    if g > maxpal then
      maxpal := g;
    strm.Read(b, SizeOf(Byte));
    if b > maxpal then
      maxpal := b;
    pal[i] := r shl 16 + g shl 8 + b;
    if pal[i] = 0 then
      pal[i] := $01;
  end;
  if (maxpal < 255) and (maxpal > 0) then
  begin
    palfactor := 255 / maxpal;
    if palfactor > 4.0 then
      palfactor := 4.0;
    for i := 0 to 255 do
    begin
      r := pal[i] shr 16;
      g := pal[i] shr 8;
      b := pal[i];
      cc := round(palfactor * r);
      if cc < 0 then
        cc := 0
      else if cc > 255 then
        cc := 255;
      r := cc;
      cc := round(palfactor * g);
      if cc < 0 then
        cc := 0
      else if cc > 255 then
        cc := 255;
      g := cc;
      cc := round(palfactor * b);
      if cc < 0 then
        cc := 0
      else if cc > 255 then
        cc := 255;
      b := cc;
      pal[i] := r shl 16 + g shl 8 + b;
    end;
  end;

  strm.Seek(0, sFromBeginning);
  strm.Read(xsiz, SizeOf(Integer));
  strm.Read(ysiz, SizeOf(Integer));
  strm.Read(zsiz, SizeOf(Integer));

  if (xsiz <= 0) or (xsiz > 256) or
     (ysiz <= 0) or (ysiz > 256) or
     (zsiz <= 0) or (zsiz > 256) then
  begin
    strm.Free;
    result := nil;
    Exit;
  end;

  voxelsize := xsiz;
  if voxelsize < ysiz then
    voxelsize := ysiz;
  if voxelsize < zsiz then
    voxelsize := zsiz;

  voxdatasize := xsiz * ysiz * zsiz;
  GetMem(voxdata, voxdatasize);
  strm.Read(voxdata^, voxdatasize);
  strm.Free;

  x1 := (voxelsize - xsiz) div 2;
  y1 := (voxelsize - ysiz) div 2;
  z1 := (voxelsize - zsiz) div 2;

  voxelbuffer := @vx_membuffer[0];
  R_ClearVoxelBuffer(voxelbuffer, voxelsize);

  i := 0;
  for xx := x1 to x1 + xsiz - 1 do
    for yy := y1 to y1 + ysiz - 1 do
      for zz := z1 to z1 + zsiz - 1 do
      begin
        if voxdata[i] <> 255 then
          voxelbuffer[xx, zz, voxelsize - yy - 1].color := pal[voxdata[i]];
        inc(i);
      end;
  memfree(pointer(voxdata), voxdatasize);

  R_OptimizeVoxelBuffer(voxelbuffer, voxelsize);
  R_PrepareVoxelColumns(voxelbuffer, voxelsize);
  result := R_VoxelColumnsFromBuffer(voxelbuffer, voxelsize, offset, scale);
end;

constructor TVoxelModel.Create(const name: string; const offset, scale: fixed_t; flags: LongWord);
var
  ext: string;
  i: integer;
begin
  printf('  Found external voxel %s'#13#10, [name]);

  ext := strupper(fext(name));
  frames := TDNumberList.Create;
  foffset := offset;
  fscale := scale;
  fflags := flags;

  if ext = '.DDMESH' then
  begin
    fnumframes := 1;
    frames.Add(integer(R_LoadDDMESH(name, offset, scale)));
  end
  else if ext = '.DDVOX' then
  begin
    fnumframes := 1;
    frames.Add(integer(R_LoadDDVOX(name, offset, scale)));
  end
  else if (ext = '.KVX') or (ext = '') then
  begin
    fnumframes := 1;
    frames.Add(integer(R_LoadKVX(name, offset, scale)));
  end
  else if ext = '.VOX' then
  begin
    fnumframes := 1;
    frames.Add(integer(R_LoadSlab6VOX(name, offset, scale)));
  end
  else
    I_Error('TVoxelModel.Create(): Can not identify voxel type "%s"', [name]);

  fradius := 0;
  for i := 0 to frames.Count - 1 do
    if voxelcolumns_p(frames.Numbers[i]).range > fradius then
      fradius := voxelcolumns_p(frames.Numbers[i]).range;
end;

destructor TVoxelModel.Destroy;
begin
  frames.Free;
end;

procedure VX_RotatePoint(x: Pfixed_t; y: Pfixed_t; a: angle_t);
var
  tmpx: fixed_t;
  asin, acos: fixed_t;
begin
  acos := finecosine[a];
  asin := finesine[a];
  tmpx :=
    FixedMul(x^, acos) -
    FixedMul(y^, asin);

  y^ :=
    FixedMul(x^, asin) +
    FixedMul(y^, acos);

  x^ := tmpx;
end;

var
  vx_simpleclip: boolean;
  vx_ceilingclip: fixed_t;
  vx_floorclip: fixed_t;

procedure R_InitVoxels;
var
  size: integer;
begin
  size := SizeOf(voxelbuffer_t) + SizeOf(kvxbuffer_t);
  vx_membuffer := malloc(size);
  VX_InitVoxels;
  memfree(pointer(vx_membuffer), size);
end;

procedure R_VoxelsDone;
begin
  VX_VoxelsDone;
end;

procedure R_DrawThingVoxel(const thing: Pmobj_t; const vidx: integer; const depth: LongWord;
  const renderflags: LongWord);
var
  info: Pvoxelstate_t;
  voxelinf: Pvoxelmanageritem_t;
  clms: voxelcolumns_p;
  i: integer;
  tr_x: fixed_t;
  tr_y: fixed_t;
  tz: fixed_t;
  vscale: fixed_t;
  rot: LongWord;
  mip: voxelmip_p;
  mscale: integer;
  footclip: fixed_t;
  mipcols: voxelcolumn_pa;
  angleadd: angle_t;
  spinang: angle_t;
  left, right, top, bottom: integer;
  Xup: integer;
  a_x, a_y: array[0..3] of fixed_t;
  tx: fixed_t;
  col: voxelcolumn_p;
  scale: fixed_t;
  scaley0: fixed_t;
  scaley1: fixed_t;
  scaley2: fixed_t;
  scaley3: fixed_t;
  mipscale: fixed_t;
  mipscale2: fixed_t;
  tmpx: fixed_t;
  asin, acos: fixed_t;
  c_x_asin,
  c_x_acos: fixed_t;
  c_y_asin,
  c_y_acos: fixed_t;
  c_x_asin2,
  c_x_acos2: fixed_t;
  c_y_asin2,
  c_y_acos2: fixed_t;
  x, y1, y2: integer;
  ang: angle_t;
  c_x, c_y: fixed_t;
  c_x2, c_y2: fixed_t;
  dist: fixed_t;
  dx, dy: fixed_t;
  floorz: fixed_t;
  ceilz: fixed_t;
  diffz: fixed_t;
  last_dc_x: integer;
  last_top: smallint;
  last_bot: smallint;
  cur_top: smallint;
  cur_bot: smallint;
  save_dc_x: integer;
  topz, bottomz: fixed_t;
  tr_topz, tr_bottomz: fixed_t;
  gxt: fixed_t;
  gyt: fixed_t;
  xscale: fixed_t;
  t_x, t_y, t_z: fixed_t;
  t_ang: angle_t;
  tmp_top, tmp_bottom: fixed_t;
  vprojection, vprojectiony: fixed_t;
  voxelinfscale: fixed_t;
begin
  info := @voxelstates[vidx];
  voxelinf := @voxelmanager.items[info.voxelidx];

  if voxelinf.voxel = nil then
  begin
    voxelinf.voxel := TVoxelModel.Create(voxelinf.name, voxelinf.offset, voxelinf.scale, voxelinf.flags);
    if voxelinf.voxel = nil then
    begin
      I_Warning('R_DrawThingVoxel(): Can not load voxel %s'#13#10, [voxelinf.name]);
      exit;
    end;
  end;

  clms := voxelcolumns_p(voxelinf.voxel.frames.Numbers[info.frame]);
  if clms <> nil then
  begin
    angleadd := voxelinf.angleoffset * ANG1;
    {$IFDEF HEXEN}
    if thing.flags2 and MF2_DROPPED <> 0 then
    {$ELSE}
    if thing.flags and MF_DROPPED <> 0 then
    {$ENDIF}
      spinang := voxelinf.droppedspin
    else
      spinang := voxelinf.placedspin;
    if spinang <> 0 then
    begin
      if not isgamesuspended then
        spinang := Round(((leveltime + ticfrac / FRACUNIT) / TICRATE * spinang) * (ANGLE_MAX / 360))
      else
        spinang := Round((leveltime / TICRATE * spinang) * (ANGLE_MAX / 360));
    end;
    angleadd := spinang - angleadd;

    rot := (R_PointToAngleEx(thing.x, thing.y) - thing.angle + angleadd + LongWord(ANG45 div 2) * 9) shr 29;

    tr_x := thing.x - viewx;
    tr_y := thing.y - viewy;

    tz := FixedMul(tr_x, viewcos) + FixedMul(tr_y, viewsin);

    // thing is behind view plane?
    if tz < MINZ{ - 2 * clms.range} then
      exit;

    // scale y
    voxelinfscale := voxelinf.voxel.fscale;
    vprojection := projection;
    vprojectiony := projectiony;

    // scale: small at large distances
    vscale := FixedDiv(vprojection, tz);
    if detailLevel <= DL_LOW then
    begin
      if vscale >= FRACUNIT then
        mip := @clms.mips[1]
      else
        mip := @clms.mips[2];
    end
    else
    begin
      if vscale >= FRACUNIT then
        mip := @clms.mips[0]
      else if vscale >= FRACUNIT div 2 then
        mip := @clms.mips[1]
      else
        mip := @clms.mips[2];
    end;


    {$IFDEF HERETIC}
    if (thing.flags2 and MF2_FEETARECLIPPED <> 0) and (thing.z <= Psubsector_t(thing.subsector).sector.floorheight) then
      footclip := 10 * FRACUNIT
    else
      footclip := 0;
    {$ELSE}
      footclip := thing.floorclip;
    {$ENDIF}

    mscale := mip.mipscale;
    // Obtain sorted back to front column list
    mipcols := mip.columnssortorder[rot];

    floorz := thing.floorz;
    ceilz := thing.ceilingz;
    t_z := thing.z - footclip;
    t_ang := (thing.angle + ANG90 - angleadd) shr ANGLETOFINESHIFT;
    acos := finecosine[t_ang];
    asin := finesine[t_ang];

    mipscale := mscale * voxelinf.voxel.fscale;
    mipscale2 := mipscale div 2;

    // For each voxel column
    for i := 0 to mip.numcolumns - 1 do
    begin
      col := mipcols[i];
      mipscale := mscale * col.scale;
      mipscale2 := mipscale div 2;
      c_x := col.x - mipscale2;
      c_y := col.y - mipscale2;

      t_x := thing.x;
      t_y := thing.y;

      // Precalc some variables
      c_x_asin := FixedMul(c_x, asin);
      c_x_acos := FixedMul(c_x, acos);
      c_y_asin := FixedMul(c_y, asin);
      c_y_acos := FixedMul(c_y, acos);

      a_x[0] := c_x_acos - c_y_asin + t_x;
      a_y[0] := c_x_asin + c_y_acos + t_y;

      // Too far away? Draw a single pixel!
      if col.maxlistsize * vscale < FRACUNIT then
      begin
        while col <> nil do
        begin
          if R_PointToScreenBuffer(a_x[0], a_y[0], t_z + col.fixedoffset, left, top) then
            if top > mceilingclip[left] then
              if top < mfloorclip[left] then
              begin
                dc_color := col.dc_color;
                if depthbufferactive then
                begin
                  if depth >= R_DepthBufferAt(left, top) then
                    putpixelfunc(left, top);
                end
                else
                  putpixelfunc(left, top);
              end;

          col := col.next;
        end;
        Continue;
      end;

      a_x[0] := a_x[0] - viewx;
      a_y[0] := a_y[0] - viewy;
      t_x := t_x - viewx;
      t_y := t_y - viewy;

      c_y2 := c_y + mipscale;
      c_y_asin2 := FixedMul(c_y2, asin);
      c_y_acos2 := FixedMul(c_y2, acos);

      a_x[1] := c_x_acos - c_y_asin2 + t_x;
      a_y[1] := c_x_asin + c_y_acos2 + t_y;

      c_x2 := c_x + mipscale;
      c_x_asin2 := FixedMul(c_x2, asin);
      c_x_acos2 := FixedMul(c_x2, acos);

      a_x[2] := c_x_acos2 - c_y_asin + t_x;
      a_y[2] := c_x_asin2 + c_y_acos + t_y;

      a_x[3] := c_x_acos2 - c_y_asin2 + t_x;
      a_y[3] := c_x_asin2 + c_y_acos2 + t_y;


      tz := FixedMul(a_x[0], viewcos) + FixedMul(a_y[0], viewsin);
      if tz < MINZ then
        Continue;
      scale := FixedDiv2(vprojection, tz);
      tx := FixedMul(a_x[0], viewsin) - FixedMul(a_y[0], viewcos);
      left := FixedInt_FixedMul(tx, scale);
      right := left;
      scaley0 := FixedDiv2(vprojectiony, tz);

      tz := FixedMul(a_x[1], viewcos) + FixedMul(a_y[1], viewsin);
      if tz < MINZ then
        Continue;
      scale := FixedDiv2(vprojection, tz);
      tx := FixedMul(a_x[1], viewsin) - FixedMul(a_y[1], viewcos);
      Xup := FixedInt_FixedMul(tx, scale);
      if left > Xup then
        left := Xup
      else if right < Xup then
        right := Xup;
      scaley1 := FixedDiv2(vprojectiony, tz);

      tz := FixedMul(a_x[2], viewcos) + FixedMul(a_y[2], viewsin);
      if tz < MINZ then
        Continue;
      scale := FixedDiv2(vprojection, tz);
      tx := FixedMul(a_x[2], viewsin) - FixedMul(a_y[2], viewcos);
      Xup := FixedInt_FixedMul(tx, scale);
      if left > Xup then
        left := Xup
      else if right < Xup then
        right := Xup;
      scaley2 := FixedDiv2(vprojectiony, tz);

      tz := FixedMul(a_x[3], viewcos) + FixedMul(a_y[3], viewsin);
      if tz < MINZ then
        Continue;
      scale := FixedDiv2(vprojection, tz);
      tx := FixedMul(a_x[3], viewsin) - FixedMul(a_y[3], viewcos);
      Xup := FixedInt_FixedMul(tx, scale);
      if left > Xup then
        left := Xup
      else if right < Xup then
        right := Xup;
      scaley3 := FixedDiv2(vprojectiony, tz);

//--------------------------------------------------------
{      left := left + centerx;
      if left >= arightx then
        Continue;

      right := right + centerx;
      if right < aleftx then
        Continue;

      if left < aleftx then
        left := aleftx
      else if left >= arightx then
        left := arightx - 1;
      if right >= arightx then
        right := arightx - 1
      else if right < aleftx then
        right := aleftx;}
//------------------------------------------------------

      left := left + centerx;
      right := right + centerx;
      if left < 0 then
        left := 0
      else if left >= viewwidth then
        left := viewwidth - 1;
      if right >= viewwidth then
        right := viewwidth - 1
      else if right < 0 then
        right := 0;

      if vx_simpleclip then
      begin
        num_batch_columns := right - left;
        dc_x := left;
      end;

      // Proccess all fractions of the column
      while col <> nil do
      begin
      // Any optimization inside here will give good fps boost
        topz := t_z + FixedMul(col.fixedoffset, voxelinfscale);
        bottomz := topz - FixedMul(col.fixedlength, voxelinfscale);
        if topz > ceilz then
          topz := ceilz;
        if bottomz < floorz then
          bottomz := floorz;
        tr_topz := topz - viewz;
        tr_bottomz := bottomz - viewz;

        top := FixedInt_FixedMul(tr_topz, scaley0);
        bottom := FixedInt_FixedMul(tr_bottomz, scaley0);

        tmp_top := FixedInt_FixedMul(tr_topz, scaley1);
        if top < tmp_top then
          top := tmp_top
        else
        begin
          tmp_bottom := FixedInt_FixedMul(tr_bottomz, scaley1);
          if bottom > tmp_bottom then
            bottom := tmp_bottom;
        end;

        tmp_top := FixedInt_FixedMul(tr_topz, scaley2);
        if top < tmp_top then
          top := tmp_top
        else
        begin
          tmp_bottom := FixedInt_FixedMul(tr_bottomz, scaley2);
          if bottom > tmp_bottom then
            bottom := tmp_bottom;
        end;

        tmp_top := FixedInt_FixedMul(tr_topz, scaley3);
        if top < tmp_top then
          top := tmp_top
        else
        begin
          tmp_bottom := FixedInt_FixedMul(tr_bottomz, scaley3);
          if bottom > tmp_bottom then
            bottom := tmp_bottom;
        end;

        top := centery - top;
        bottom := centery - bottom;

        if top < 0 then
          top := 0
        else if top >= viewheight then
        begin
          col := nil; // All columns ahead are below viewheight, set col = nil to advance
          Continue;
        end;

        if bottom >= viewheight then
          bottom := viewheight - 1
        else if bottom < 0 then
        begin
          col := col.next;
          Continue;
        end;

        if (bottom < fake3dtopclip) or (top > fake3dbottomclip) then
        begin
          col := col.next;
          Continue;
        end;

        dc_color := col.dc_color;
        dc_source := @dc_color;

        if vx_simpleclip then
        begin
          if top <= vx_ceilingclip then
            dc_yl := vx_ceilingclip + 1
          else
            dc_yl := top;
          if bottom >= vx_floorclip then
            dc_yh := vx_floorclip - 1
          else
            dc_yh := bottom;

          if depthbufferactive then
          begin
            if renderflags and VSF_TRANSPARENCY <> 0 then
              R_DrawBatchColumnWithDepthBufferCheckOnly(batchcolfunc, depth)
            else
              R_DrawBatchColumnWithDepthBufferCheckWrite(batchcolfunc, depth)
          end
          else
            batchcolfunc;

          col := col.next;

          Continue;
        end;

        dc_x := left;
        last_dc_x := left;
        last_top := top;
        if last_top <= mceilingclip[left] then
          last_top := mceilingclip[left] + 1;
        last_bot := bottom;
        if last_bot >= mfloorclip[left] then
          last_bot := mfloorclip[left] - 1;
        while dc_x <= right do
        begin
          cur_top := top;
          if cur_top <= mceilingclip[dc_x] then
            cur_top := mceilingclip[dc_x] + 1;
          cur_bot := bottom;
          if cur_bot >= mfloorclip[dc_x] then
            cur_bot := mfloorclip[dc_x] - 1;

          if (last_top <> cur_top) or
             (last_bot <> cur_bot) then
          begin
            num_batch_columns := dc_x - last_dc_x;
            save_dc_x := last_dc_x;
            last_dc_x := dc_x;
            dc_x := save_dc_x;
            dc_yl := last_top;
            dc_yh := last_bot;

            if depthbufferactive then
            begin
              if renderflags and VSF_TRANSPARENCY <> 0 then
                R_DrawBatchColumnWithDepthBufferCheckOnly(batchcolfunc, depth)
              else
                R_DrawBatchColumnWithDepthBufferCheckWrite(batchcolfunc, depth)
            end
            else
              batchcolfunc;

            last_top := cur_top;
            last_bot := cur_bot;
            dc_x := last_dc_x;
          end;
          inc(dc_x);
        end;

        num_batch_columns := dc_x - last_dc_x - 1;
        if num_batch_columns > 0 then
        begin
          dc_x := last_dc_x;
          dc_yl := last_top;
          dc_yh := last_bot;

          if depthbufferactive then
          begin
            if renderflags and VSF_TRANSPARENCY <> 0 then
              R_DrawBatchColumnWithDepthBufferCheckOnly(batchcolfunc, depth)
            else
              R_DrawBatchColumnWithDepthBufferCheckWrite(batchcolfunc, depth)
          end
          else
            batchcolfunc;
        end;

        col := col.next;
      end;

    end;

  end;
end;

procedure R_DrawThingVoxels(const thing: Pmobj_t; const depth: LongWord;
  const renderflags: LongWord);
var
  i: integer;
begin
  for i := 0 to thing.state.voxels.Count - 1 do
    R_DrawThingVoxel(thing, thing.state.voxels.Numbers[i], depth, renderflags);
end;

procedure R_DrawVoxel(const vis: Pvissprite_t);
var
  i: integer;
  ds: Pdrawseg_t;
  x: integer;
  r1: integer;
  r2: integer;
  scale: fixed_t;
  lowscale: fixed_t;
  silhouette: integer;
{$IFDEF DOOM_OR_STRIFE}
  h, mh: fixed_t;
  plheightsec: integer;
{$ENDIF}
  flag: boolean;
  depth: LongWord;
  size: integer;
  fds_p: integer;
  fdrawsegs: Pdrawsegsbuffer_t;
begin
  size := vis.vx2 - vis.vx1 + 1;
  if size <= 0 then
    exit;
  memsetsi(@clipbot[vis.vx1], -2, size);
  memsetsi(@cliptop[vis.vx1], -2, size);

  R_GetDrawsegsForRange(vis.vx1, vis.vx2, fdrawsegs, fds_p);
  // Scan drawsegs from end to start for obscuring segs.
  // The first drawseg that has a greater scale
  //  is the clip seg.
  for i := fds_p - 1 downto 0 do
  begin
    ds := fdrawsegs[i];
    // determine if the drawseg obscures the sprite
    if (ds.x1 > vis.vx2) or
       (ds.x2 < vis.vx1) or
       ((ds.silhouette = 0) and (ds.maskedtexturecol = nil) and (ds.thicksidecol = nil)) then
    begin
      // does not cover sprite
      continue;
    end;

    if ds.x1 < vis.vx1 then
      r1 := vis.vx1
    else
      r1 := ds.x1;
    if ds.x2 > vis.vx2 then
      r2 := vis.vx2
    else
      r2 := ds.x2;

    if ds.scale1 > ds.scale2 then
    begin
      lowscale := ds.scale2;
      scale := ds.scale1;
    end
    else
    begin
      lowscale := ds.scale1;
      scale := ds.scale2;
    end;

    if (scale < vis.scale) or
       ((lowscale < vis.scale) and (not R_PointOnSegSide(vis.gx, vis.gy, ds.curline))) then
    begin
      // masked mid texture?
      if ds.thicksidecol <> nil then        // JVAL: 3d Floors
        R_RenderThickSideRange(ds, r1, r2); // JVAL: 3d Floors
      if ds.maskedtexturecol <> nil then
        R_RenderMaskedSegRange(ds, r1, r2);
      // seg is behind sprite
      continue;
    end;

    // clip this piece of the sprite
    silhouette := ds.silhouette;

    if vis.gz >= ds.bsilheight then
      silhouette := silhouette and not SIL_BOTTOM;

    if vis.gzt <= ds.tsilheight then
      silhouette := silhouette and not SIL_TOP;

    if silhouette = 1 then
    begin
      // bottom sil
      for x := r1 to r2 do
        if clipbot[x] = -2 then
          clipbot[x] := ds.sprbottomclip[x];
    end
    else if silhouette = 2 then
    begin
      // top sil
      for x := r1 to r2 do
        if cliptop[x] = -2 then
          cliptop[x] := ds.sprtopclip[x];
    end
    else if silhouette = 3 then
    begin
      // both
      for x := r1 to r2 do
      begin
        if clipbot[x] = -2 then
          clipbot[x] := ds.sprbottomclip[x];
        if cliptop[x] = -2 then
          cliptop[x] := ds.sprtopclip[x];
      end;
    end;
  end;


  {$IFDEF DOOM_OR_STRIFE}
  // killough 3/27/98:
  // Clip the sprite against deep water and/or fake ceilings.
  // killough 4/9/98: optimize by adding mh
  // killough 4/11/98: improve sprite clipping for underwater/fake ceilings

  if vis.heightsec <> -1 then  // only things in specially marked sectors
  begin
    plheightsec := Psubsector_t(viewplayer.mo.subsector).sector.heightsec;
    mh := sectors[vis.heightsec].floorheight;
    if mh > vis.gz then
    begin
      mh := mh - viewz;
      h := centeryfrac - FixedMul(mh, vis.scale);
      if h >= 0 then
      begin
        h := h div FRACUNIT;
        if h < viewheight then
        begin
          if (mh <= 0) or ((plheightsec <> -1) and (viewz > sectors[plheightsec].floorheight)) then
          begin
            for x := vis.vx1 to vis.vx2 do
              if (clipbot[x] = -2) or (h < clipbot[x]) then
                clipbot[x] := h;
          end
          else if (plheightsec <> -1) and (viewz <= sectors[plheightsec].floorheight) then
          begin
            for x := vis.vx1 to vis.vx2 do
              if (cliptop[x] = -2) or (h > cliptop[x]) then
                cliptop[x] := h;
          end;
        end;
      end;
    end;

    mh := sectors[vis.heightsec].ceilingheight;
    if mh < vis.gzt then
    begin
      h := centeryfrac - FixedMul(mh - viewz, vis.scale);
      if h >= 0 then
      begin
        h := h div FRACUNIT;
        if h < viewheight then
        begin
          if (plheightsec <> -1) and (viewz >= sectors[plheightsec].ceilingheight) then
          begin
            for x := vis.vx1 to vis.vx2 do
              if (clipbot[x] = -2) or (h < clipbot[x]) then
                clipbot[x] := h;
          end
          else
          begin
            for x := vis.vx1 to vis.vx2 do
              if (cliptop[x] = -2) or (h > cliptop[x]) then
                cliptop[x] := h;
          end;
        end;
      end;
    end;
  end;

  // killough 3/27/98: end special clipping for deep water / fake ceilings
  {$ENDIF}

  // all clipping has been performed, so draw the sprite

  // check for unclipped columns
  for x := vis.vx1 to vis.vx2 do
  begin
    if clipbot[x] = -2 then
      clipbot[x] := fake3dbottomclip
    else if clipbot[x] > fake3dbottomclip then
      clipbot[x] := fake3dbottomclip;

    if cliptop[x] = -2 then
      cliptop[x] := fake3dtopclip
    else if cliptop[x] < fake3dtopclip then
      cliptop[x] := fake3dtopclip;
  end;

  mfloorclip := @clipbot;
  mceilingclip := @cliptop;

  dc_colormap := vis.colormap;

  if videomode = vm32bit then
  begin
    dc_colormap32 := R_GetColormap32(dc_colormap);
    if fixedcolormapnum = INVERSECOLORMAP then
      dc_lightlevel := -1
    else
      dc_lightlevel := R_GetColormapLightLevel(dc_colormap);
    putpixelfunc := @R_PutPixel32
  end
  else
    putpixelfunc := @R_PutPixel8;

  flag := false;
  if dc_colormap = nil then
  begin
    // NULL colormap = shadow draw
    batchcolfunc := {$IFDEF HEXEN}batchtaveragecolfunc{$ELSE}{$IFDEF STRIFE}batchfuzzcolfunc1{$ELSE}batchfuzzcolfunc{$ENDIF}{$ENDIF};
  end
  {$IFDEF STRIFE}
  else if vis.mobjflags and MF_SHADOW <> 0 then
  begin
    if vis.mobjflags and MF_TRANSLATION = 0 then
    begin
      if vis.mobjflags and MF_MVIS <> 0 then
      begin
        batchcolfunc := batchfuzzcolfunc2;
      end
      else
      begin
        batchcolfunc := batchfuzzcolfunc1;
      end;
    end
    else
    begin
      dc_translation := PByteArray(integer(translationtables) - 256 +
        (_SHR((vis.mobjflags and MF_TRANSLATION), (MF_TRANSSHIFT - 8))));
      batchcolfunc := batchfuzztranscolfunc;
    end;
  end
  else if vis.mobjflags and MF_MVIS <> 0 then
  begin
    batchcolfunc := batchfuzzcolfunc2;
  end
  {$ENDIF}
  else if vis.mobjflags and MF_TRANSLATION <> 0 then
  begin
    batchcolfunc := batchtranscolfunc;
    dc_translation := PByteArray(integer(translationtables) - 256 +
      (_SHR((vis.mobjflags and MF_TRANSLATION), (MF_TRANSSHIFT - 8))));
  end
  else if usetransparentsprites and (vis.mo <> nil) and (vis.mo.renderstyle = mrs_translucent) then
  begin
    dc_alpha := vis.mo.alpha;
    curtrans8table := R_GetTransparency8table(dc_alpha);
    batchcolfunc := batchtalphacolfunc;
  end
  else if usetransparentsprites and (vis.mo <> nil) and (vis.mo.renderstyle = mrs_add) then
  begin
    dc_alpha := vis.mo.alpha;
    curadd8table := R_GetAdditive8table(dc_alpha);
    batchcolfunc := batchaddcolfunc;
  end
  else if usetransparentsprites and (vis.mo <> nil) and (vis.mo.renderstyle = mrs_subtract) then
  begin
    dc_alpha := vis.mo.alpha;
    cursubtract8table := R_GetSubtractive8table(dc_alpha);
    batchcolfunc := batchsubtractcolfunc;
  end
  else if usetransparentsprites and (vis.mobjflags_ex and MF_EX_TRANSPARENT <> 0) then
  begin
    batchcolfunc := batchtaveragecolfunc;
  end
  else
  begin
    if videomode = vm32bit then
      batchcolfunc := @R_DrawColorColumnHi
    else
      batchcolfunc := @R_DrawColorColumnMedium;
    flag := true;
  end;

  if not Assigned(batchcolfunc) then
  begin
    if videomode = vm32bit then
      batchcolfunc := @R_DrawColorColumnHi
    else
      batchcolfunc := @R_DrawColorColumnMedium;
    flag := true;
  end;

  depth := vis.scale;
  if not flag then
  begin
    dc_texturemid := 0;
    dc_iscale := 1;
  end;

  vx_simpleclip := true;
  vx_ceilingclip := mceilingclip[vis.vx1];
  vx_floorclip := mfloorclip[vis.vx1];
  for x := vis.vx1 + 1 to vis.vx2 do
  begin
    if mceilingclip[x] <> vx_ceilingclip then
    begin
      vx_simpleclip := False;
      break;
    end;
    if mfloorclip[x] <> vx_floorclip then
    begin
      vx_simpleclip := False;
      break;
    end;
  end;

  R_DrawThingVoxels(vis.mo, depth, vis.renderflags);
end;

end.

