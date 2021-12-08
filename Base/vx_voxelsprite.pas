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
//  Use Voxels as sprites without patch inside WAD
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit vx_voxelsprite;

interface

procedure VX_VoxelToSprite;

var
  r_generatespritesfromvoxels: boolean = true;

implementation

uses
  d_delphi,
  doomdef,
  i_tmp,
  m_argv,
  m_sha1,
  {$IFDEF OPENGL}
  gl_voxels,
  {$ELSE}
  r_voxels,
  {$ENDIF}
  r_defs,
  sc_engine,
  v_video,
  w_folders,
  w_pak,
  w_wad,
  w_wadwriter;

////////////////////////////////////////////////////////////////////////////////
// TVoxelImageLoader
const
  MAXVOXELSIZE3D = 256;

type
  voxelbuffer3d_t = array[0..MAXVOXELSIZE3D - 1, 0..MAXVOXELSIZE3D - 1, 0..MAXVOXELSIZE3D - 1] of LongWord;
  voxelbuffer3d_p = ^voxelbuffer3d_t;

  voxelbuffer2d_t = array[0..MAXVOXELSIZE3D - 1, 0..MAXVOXELSIZE3D - 1] of LongWord;
  voxelbuffer2d_p = ^voxelbuffer2d_t;

type
  TVoxelImageLoader = class
  private
    fvoxelbuffer: voxelbuffer3d_p;
    fvoxelsize: integer;
  protected
    procedure Clear;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function LoadDDVOX(const vname: string): boolean;
    function LoadKVX(const vname: string): boolean;
    function LoadVOX(const vname: string): boolean;
    function LoadDDMESH(const vname: string): boolean;
    procedure CreateDoomPatch(out p: pointer; out size: integer);
  end;

constructor TVoxelImageLoader.Create;
begin
  fvoxelbuffer := mallocz(SizeOf(voxelbuffer3d_t));
  fvoxelsize := 0;
end;

destructor TVoxelImageLoader.Destroy;
begin
  memfree(pointer(fvoxelbuffer), SizeOf(voxelbuffer3d_t));
end;

procedure TVoxelImageLoader.Clear;
begin
  ZeroMemory(fvoxelbuffer, SizeOf(voxelbuffer3d_t));
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

function TVoxelImageLoader.LoadDDVOX(const vname: string): boolean;
var
  buf: TDStringList;
  sc: TScriptEngine;
  xx, yy, zz: integer;
  strm: TPakStream;
begin
  strm := TPakStream.Create(vname, pm_prefered, gamedirectories, FOLDER_VOXELS);
  if strm.IOResult <> 0 then
  begin
    strm.Free;
    result := false;
    Exit;
  end;

  Clear;

  buf := TDStringList.Create;
  buf.LoadFromStream(strm);
  strm.Free;
  sc := TScriptEngine.Create(buf.Text);
  buf.free;

  sc.MustGetInteger;
  fvoxelsize := sc._Integer;

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
      fvoxelbuffer[xx, yy, zz] := SwapRGB(sc._Integer);
      inc(zz);
    end;
    if zz = fvoxelsize then
    begin
      zz := 0;
      inc(yy);
      if yy = fvoxelsize then
      begin
        yy := 0;
        inc(xx);
        if xx = fvoxelsize then
          Break;
      end;
    end;
  end;

  sc.Free;

  result := true;
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

function TVoxelImageLoader.LoadKVX(const vname: string): boolean;
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
  maxpal: integer;
  cc: integer;
  palfactor: double;
  lump: integer;
  len: integer;
  s1, s2, s3: string;
begin
  strm := TPakStream.Create(vname, pm_prefered, gamedirectories, FOLDER_VOXELS);
  if strm.IOResult <> 0 then
  begin
    strm.Free;
    strm := TPakStream.Create(vname, pm_directory, '', FOLDER_VOXELS);
  end;
  if strm.IOResult <> 0 then
  begin
    strm.Free;
    s1 := fname(vname);
    splitstring(s1, s2, s3, '.');
    lump := W_CheckNumForName(s2, TYPE_VOXEL);
    if lump < 0 then
    begin
      result := false;
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
    result := false;
    Exit;
  end;

  Clear;

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

  kvxbuffer := malloc(SizeOf(kvxbuffer_t));

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
      while integer(slab) < integer(endptr) do
      begin
        for zz := slab.ztop to slab.zleng + slab.ztop - 1 do
          kvxbuffer[xx, yy, zz] := slab.col[zz - slab.ztop];
        slab := kvxslab_p(integer(slab) + slab.zleng + 3);
      end;
    end;
  end;

  fvoxelsize := xsiz;
  if fvoxelsize < ysiz then
    fvoxelsize := ysiz;
  if fvoxelsize < zsiz then
    fvoxelsize := zsiz;
  if fvoxelsize < 256 then
  begin
    inc(fvoxelsize, 2);
    fvoxelsize := fvoxelsize and not 1;
  end;

  x1 := fvoxelsize div 2 - xpivot div 256;
  y1 := fvoxelsize div 2 - ypivot div 256;
  z1 := fvoxelsize - zpivot div 256; // JVAL: Align down
  if x1 < 0 then
    x1 := 0;
  if y1 < 0 then
    y1 := 0;
  if z1 < 0 then
    z1 := 0;
  while x1 + xsiz >= fvoxelsize do
      dec(x1);
  while y1 + ysiz >= fvoxelsize do
      dec(y1);
  while z1 + zsiz >= fvoxelsize do
      dec(z1);

  x1 := fvoxelsize div 2 - xpivot div 256;
  y1 := fvoxelsize div 2 - ypivot div 256;
  z1 := fvoxelsize - zpivot div 256; // JVAL: Align down

  for xx := x1 to x1 + xsiz - 1 do
    for yy := y1 to y1 + ysiz - 1 do
      for zz := z1 to z1 + zsiz - 1 do
        if kvxbuffer[xx - x1, yy - y1, zz - z1] <> $FFFF then
          fvoxelbuffer[xx, zz, fvoxelsize - yy - 1] := pal[kvxbuffer[xx - x1, yy - y1, zz - z1]];

  memfree(pointer(kvxbuffer), SizeOf(kvxbuffer_t));

  for i := 0 to xsiz - 1 do
    memfree(pointer(xyoffset[i]), (ysiz + 1) * SizeOf(SmallInt));
  memfree(pointer(xoffset), (xsiz + 1) * SizeOf(Integer));
  memfree(pointer(xyoffset), xsiz * SizeOf(PSmallIntArray));
  memfree(pointer(voxdata), voxdatasize);

  result := true;
end;

function TVoxelImageLoader.LoadVOX(const vname: string): boolean;
var
  strm: TDStream;
  pal: array[0..255] of LongWord;
  i: integer;
  r, g, b: byte;
  xsiz, ysiz, zsiz: integer;
  voxdatasize: integer;
  voxdata: PByteArray;
  xx, yy, zz: integer;
  x1, y1, z1: integer;
  s: string;
  maxpal: integer;
  cc: integer;
  palfactor: double;
begin
  strm := TPakStream.Create(vname, pm_prefered, gamedirectories, FOLDER_VOXELS);
  if strm.IOResult <> 0 then
  begin
    strm.Free;
    result := false;
    Exit;
  end;

  if strm.Size < 768 + 12 then
  begin
    strm.Free;
    result := false;
    Exit;
  end;

  Clear;

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
    result := false;
    Exit;
  end;

  fvoxelsize := xsiz;
  if fvoxelsize < ysiz then
    fvoxelsize := ysiz;
  if fvoxelsize < zsiz then
    fvoxelsize := zsiz;

  voxdatasize := xsiz * ysiz * zsiz;
  GetMem(voxdata, voxdatasize);
  strm.Read(voxdata^, voxdatasize);
  strm.Free;

  x1 := (fvoxelsize - xsiz) div 2;
  y1 := (fvoxelsize - ysiz) div 2;
  z1 := (fvoxelsize - zsiz) div 2;

  i := 0;
  for xx := x1 to x1 + xsiz - 1 do
    for yy := y1 to y1 + ysiz - 1 do
      for zz := z1 to z1 + zsiz - 1 do
      begin
        if voxdata[i] <> 255 then
          fvoxelbuffer[xx, zz, fvoxelsize - yy - 1] := pal[voxdata[i]];
        inc(i);
      end;

  memfree(pointer(voxdata), voxdatasize);

  result := true;
end;

type
  ddmeshitem_t = packed record
    x, y, z: byte;
    color: LongWord;
  end;
  ddmeshitem_p = ^ddmeshitem_t;
  ddmeshitem_a = array[0..$FFF] of ddmeshitem_t;
  ddmeshitem_pa = ^ddmeshitem_a;

function TVoxelImageLoader.LoadDDMESH(const vname: string): boolean;
var
  strm: TPakStream;
  i: integer;
  HDR: LongWord;
  version: integer;
  numquads: integer;
  fnumvoxels: Integer;
  buf: ddmeshitem_pa;
  item: ddmeshitem_p;
begin
  strm := TPakStream.Create(vname, pm_prefered, gamedirectories, FOLDER_VOXELS);
  if strm.IOResult <> 0 then
  begin
    strm.Free;
    result := false;
    Exit;
  end;

  strm.Read(HDR, SizeOf(LongWord));
  if HDR <> Ord('D') + Ord('D') shl 8 + Ord('M') shl 16 + Ord('S') shl 24 then
  begin
    strm.Free;
    result := false;
    Exit;
  end;

  strm.Read(version, SizeOf(integer));
  if version <> 1 then
  begin
    strm.Free;
    result := false;
    Exit;
  end;

  Clear;

  strm.Read(fvoxelsize, SizeOf(integer));

  strm.Read(numquads, SizeOf(integer));

  // Skip OpenGL data
  strm.Seek(16 + numquads * (4 * 3 * SizeOf(Integer) + SizeOf(LongWord)), sFromBeginning);

  strm.Read(fnumvoxels, SizeOf(integer));

  buf := malloc(fnumvoxels * SizeOf(ddmeshitem_t));

  strm.Read(buf^, fnumvoxels * SizeOf(ddmeshitem_t));
  strm.Free;

  item := @buf[0];
  for i := 0 to fnumvoxels - 1 do
  begin
    fvoxelbuffer[item.x, item.y, item.z] := SwapRGB(item.color);
    Inc(item);
  end;

  memfree(pointer(buf), fnumvoxels * SizeOf(ddmeshitem_t));

  result := true;
end;

type
  patchheader_t = packed record
    width: smallint; // bounding box size
    height: smallint;
    leftoffset: smallint; // pixels to the left of origin
    topoffset: smallint;  // pixels below the origin
  end;

procedure TVoxelImageLoader.CreateDoomPatch(out p: pointer; out size: integer);
var
  x, y, z: integer;
  c: LongWord;
  img: voxelbuffer2d_p;
  m, fs: TDMemoryStream;
  patch: patchheader_t;
  column: column_t;
  columnofs: TDNumberList;
  columndata: TDByteList;
  i: integer;

  procedure flashcolumnend;
  begin
    column.topdelta := 255;
    column.length := 0;
    m.Write(column, SizeOf(column_t));
  end;

  procedure flashcolumndata;
  var
    bb: byte;
  begin
    if columndata.Count > 0 then
    begin
      column.topdelta := y - columndata.Count;
      column.length := columndata.Count;
      m.Write(column, SizeOf(column_t));
      bb := 0;
      m.Write(bb, SizeOf(bb));
      m.Write(columndata.List^, columndata.Count);
      m.Write(bb, SizeOf(bb));
      columndata.FastClear;
    end;
  end;

begin
  img := mallocz(SizeOf(voxelbuffer2d_t));

  for x := 0 to fvoxelsize - 1 do
    for y := 0 to fvoxelsize - 1 do
    begin
      c := 0;
      for z := 0 to fvoxelsize - 1 do
        if fvoxelbuffer[x, y, z] <> 0 then
        begin
          c := fvoxelbuffer[x, y, z];
          Break;
        end;
      img[x, y] := c;
    end;

  if Odd(fvoxelsize) then
    inc(fvoxelsize);

  m := TDMemoryStream.Create;
  fs := TDMemoryStream.Create;
  columnofs := TDNumberList.Create;
  columndata := TDByteList.Create;
  try
    patch.width := fvoxelsize;
    patch.height := fvoxelsize;
    patch.leftoffset := fvoxelsize div 2;
    patch.topoffset := fvoxelsize;
    fs.Write(patch, SizeOf(patchheader_t));

    for x := 0 to fvoxelsize - 1 do
    begin
      columnofs.Add(m.Position + SizeOf(patchheader_t) + fvoxelsize * SizeOf(integer));
      columndata.FastClear;
      for y := 0 to fvoxelsize - 1 do
      begin
        c := img[x, y];
        if c and $FFFFFF = 0 then
        begin
          flashcolumndata;
          continue;
        end;
        columndata.Add(V_FindAproxColorIndex(default_palette, c))
      end;
      flashcolumndata;
      flashcolumnend;
    end;

    for i := 0 to columnofs.Count - 1 do
    begin
      x := columnofs.Numbers[i];
      fs.Write(x, SizeOf(integer));
    end;

    size := fs.Size + m.Size;
    p := malloc(size);

    memcpy(p, fs.Memory, fs.Size);
    memcpy(pointer(integer(p) + fs.Size), m.Memory, m.Size);

  finally
    m.Free;
    columnofs.Free;
    columndata.Free;
    fs.Free;
    memfree(pointer(img), SizeOf(voxelbuffer2d_t));
  end;

end;

////////////////////////////////////////////////////////////////////////////////

var
  vx_names: TDStringList = nil;

function VX_SpriteExistsInWAD(const filename: string): boolean;
var
  check, lumpname: string;
  i: integer;
  in_loop: boolean;
begin
  check := firstword(filename, '.'); // Input is uppercase
  SetLength(check, 5); // SPRITE & FRAME
  in_loop := false;
  for i := 0 to W_NumLumps - 1 do
  begin
    lumpname := char8tostring(W_GetNameForNum(i));
    if (lumpname = 'SS_START') or (lumpname = 'S_START') then
      in_loop := true
    else if (lumpname = 'SS_END') or (lumpname = 'S_END') then
      in_loop := false
    else if in_loop then
    begin
      if Pos(check, lumpname) = 1 then
      begin
        result := true;
        exit;
      end;
    end;
  end;
  result := false;
end;

procedure VX_AddFileName(const filename: string);
var
  check: string;
  name: string;
  ext: string;
begin
  check := strupper(filename);
  ext := fext(check);
  if (ext = '.DDVOX') or (ext = '.DDMESH') or (ext = '.KVX') or (ext = '.VOX') or
     ((ext = '') and (Pos(FOLDER_VOXELS + '\', fixpathname(check)) > 0)) then
  begin
    check := fname(check);
    name := firstword(check, '.');
    if (Length(name) = 5) or ((Length(name) = 6) and (name[6] = '0')) then
      if vx_names.IndexOf(name) < 0 then
        if not VX_SpriteExistsInWAD(name) then
          vx_names.Add(name + ext)
  end;
end;

const
  LUMP0: array[0..434] of Byte = (
    $89, $50, $4E, $47, $0D, $0A, $1A, $0A, $00, $00, $00, $0D, $49, $48, $44,
    $52, $00, $00, $00, $80, $00, $00, $00, $80, $08, $02, $00, $00, $00, $4C,
    $5C, $F6, $9C, $00, $00, $00, $08, $67, $72, $41, $62, $00, $00, $00, $40,
    $00, $00, $00, $80, $37, $F7, $B6, $0F, $00, $00, $00, $09, $70, $48, $59,
    $73, $00, $00, $0B, $13, $00, $00, $0B, $13, $01, $00, $9A, $9C, $18, $00,
    $00, $01, $51, $49, $44, $41, $54, $78, $9C, $ED, $D9, $B1, $09, $C0, $30,
    $10, $04, $41, $0B, $5C, $98, $FA, $6F, $4A, $2E, $C1, $3C, $08, $36, $99,
    $89, $DF, $D1, $82, $02, $DF, $3A, $67, $3F, $84, $46, $01, $A6, $B5, $DC,
    $FF, $DE, $BF, $A3, $0F, $B8, $4E, $80, $98, $00, $31, $01, $62, $02, $C4,
    $04, $88, $09, $10, $13, $20, $26, $40, $4C, $80, $98, $00, $31, $01, $62,
    $02, $C4, $96, $DF, $D1, $31, $BF, $A3, $DB, $7B, $4F, $50, $4C, $80, $98,
    $00, $31, $01, $62, $02, $C4, $04, $88, $09, $10, $13, $20, $26, $40, $4C,
    $80, $98, $00, $31, $01, $62, $02, $C4, $04, $88, $D9, $03, $6A, $F6, $80,
    $F6, $DE, $13, $14, $13, $20, $26, $40, $4C, $80, $98, $00, $31, $01, $62,
    $02, $C4, $04, $88, $09, $10, $13, $20, $26, $40, $4C, $80, $98, $00, $31,
    $01, $62, $F6, $80, $9A, $3D, $A0, $BD, $F7, $04, $C5, $04, $88, $09, $10,
    $13, $20, $26, $40, $4C, $80, $98, $00, $31, $01, $62, $02, $C4, $04, $88,
    $09, $10, $13, $20, $26, $40, $4C, $80, $98, $3D, $A0, $66, $0F, $68, $EF,
    $3D, $41, $31, $01, $62, $02, $C4, $04, $88, $09, $10, $13, $20, $26, $40,
    $4C, $80, $98, $00, $31, $01, $62, $02, $C4, $04, $88, $09, $10, $13, $20,
    $66, $0F, $A8, $D9, $03, $DA, $7B, $4F, $50, $4C, $80, $98, $00, $31, $01,
    $62, $02, $C4, $04, $88, $09, $10, $13, $20, $26, $40, $4C, $80, $98, $00,
    $31, $01, $62, $02, $C4, $04, $88, $D9, $03, $6A, $F6, $80, $F6, $DE, $13,
    $14, $13, $20, $26, $40, $4C, $80, $98, $00, $31, $01, $62, $02, $C4, $04,
    $88, $09, $10, $13, $20, $26, $40, $4C, $80, $98, $00, $31, $01, $62, $F6,
    $80, $9A, $3D, $A0, $BD, $F7, $04, $C5, $04, $88, $09, $10, $13, $20, $26,
    $40, $4C, $80, $98, $00, $31, $01, $62, $02, $C4, $04, $88, $09, $10, $13,
    $20, $26, $40, $4C, $80, $98, $3D, $20, $F6, $01, $68, $2F, $B7, $64, $73,
    $C2, $B3, $80, $00, $00, $00, $00, $49, $45, $4E, $44, $AE, $42, $60, $82
  );

procedure VX_VoxelToSprite;
var
  wad: TWADWriter;
  wadfilename: string;
  i: integer;
  mem: TDMemoryStream;
  vil: TVoxelImageLoader;
  ext: string;
  ret: boolean;
  p: pointer;
  size: integer;

  function _vxsprname(const vxn: string): string;
  begin
    result := firstword(vx_names.Strings[i], '.');
    if Length(result) = 5 then
      result := result + '0';
  end;

begin
  vx_names := TDStringList.Create;
  PAK_FileNameIterator(@VX_AddFileName);

  if r_generatespritesfromvoxels then
    vil := TVoxelImageLoader.Create
  else
    vil := nil;

  if vx_names.Count > 0 then
  begin
    wad := TWADWriter.Create;

    wad.AddSeparator('SS_START');

    if vil = nil then
    begin
      for i := 0 to vx_names.Count - 1 do
        wad.AddData(_vxsprname(vx_names.Strings[i]), @LUMP0, SizeOf(LUMP0));
    end
    else
    begin
      for i := 0 to vx_names.Count - 1 do
      begin
        ext := strupper(fext(vx_names.Strings[i]));
        if ext = '.DDVOX' then
          ret := vil.LoadDDVOX(vx_names.Strings[i])
        else if ext = '.DDMESH' then
          ret := vil.LoadDDMESH(vx_names.Strings[i])
        else if ext = '.KVX' then
          ret := vil.LoadKVX(vx_names.Strings[i])
        else if ext = '.VOX' then
          ret := vil.LoadVOX(vx_names.Strings[i])
        else
          ret := vil.LoadKVX(vx_names.Strings[i]);
        if ret then
        begin
          vil.CreateDoomPatch(p, size);
          wad.AddData(_vxsprname(vx_names.Strings[i]), p, size);
          memfree(p, size);
        end
        else
          wad.AddData(_vxsprname(vx_names.Strings[i]), @LUMP0, SizeOf(LUMP0));
      end;
    end;

    wad.AddSeparator('SS_END');

    mem := TDMemoryStream.Create;

    wad.SaveToStream(mem);

    wadfilename := M_SaveFileName('DATA\');
    MkDir(wadfilename);
    wadfilename := wadfilename + 'TMP\';
    MkDir(wadfilename);
    wadfilename := wadfilename + 'voxelsprite_' + readablestring(SHA1_CalcSHA1Buf(mem.memory^, mem.Size)) + '.wad';
    mem.Free;

    wad.SaveToFile(wadfilename);

    wad.Free;

    W_RuntimeLoad(wadfilename, F_ORIGIN_WAD);

    I_DeclareTempFile(wadfilename);
  end;

  if vil <> nil then
    vil.Free;

  vx_names.Free;
end;

end.
