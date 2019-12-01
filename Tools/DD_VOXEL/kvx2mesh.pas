unit kvx2mesh;

interface

function ConvertKVF2DDMESH(const sf, st: string): boolean;

implementation

uses
  vxe_kvx,
  vxe_mesh,
  voxels,
  SysUtils,
  Classes;

function ConvertKVF2DDMESH(const sf, st: string): boolean;
var
  strm: TFileStream;
  pal: array[0..255] of LongWord;
  i: integer;
  r, g, b: byte;
  numbytes: integer;
  xsiz, ysiz, zsiz, xpivot, ypivot, zpivot: integer;
	xoffset: PIntegerArray;
	xyoffset: PSmallIntPArray;
  offs: integer;
  voxdatasize: integer;
  voxdata: PByteArray;
  xx, yy, zz: integer;
  x1, y1, z1: integer;
  endptr: PByte;
  slab: kvxslab_p;
  s: string;
  kvxbuffer: kvxbuffer_p;
  vmo: TVoxelMeshOptimizer;
  fvoxelsize: integer;
  voxelbuffer: voxelbuffer_p;
  maxpal: integer;
  cc: integer;
  palfactor: double;
begin
  if not FileExists(sf) then
  begin
    Result := false;
    exit;
  end;

  GetMem(voxelbuffer, SizeOf(voxelbuffer_t));
  FillChar(voxelbuffer^, SizeOf(voxelbuffer_t), Chr(0));

  strm := TFileStream.Create(sf, fmOpenRead or fmShareDenyWrite);

  strm.Seek(strm.size - 768, soFromBeginning);
  maxpal := 0;
  for i := 0 to 255 do
  begin
    strm.Read(b, SizeOf(Byte));
    if b > maxpal then
      maxpal := b;
    strm.Read(g, SizeOf(Byte));
    if g > maxpal then
      maxpal := g;
    strm.Read(r, SizeOf(Byte));
    if r > maxpal then
      maxpal := r;
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


  strm.Seek(0, soFromBeginning);
  strm.Read(numbytes, SizeOf(Integer));
  strm.Read(xsiz, SizeOf(Integer));
  strm.Read(ysiz, SizeOf(Integer));
  strm.Read(zsiz, SizeOf(Integer));

  fvoxelsize := 1;

  while xsiz > fvoxelsize do
    fvoxelsize := fvoxelsize * 2;
  while ysiz > fvoxelsize do
    fvoxelsize := fvoxelsize * 2;
  while zsiz > fvoxelsize do
    fvoxelsize := fvoxelsize * 2;

  if fvoxelsize < 256 then
    fvoxelsize := fvoxelsize * 2;

  strm.Read(xpivot, SizeOf(Integer));
  strm.Read(ypivot, SizeOf(Integer));
  strm.Read(zpivot, SizeOf(Integer));

  GetMem(xoffset, (xsiz + 1) * SizeOf(Integer));
  GetMem(xyoffset, xsiz * SizeOf(PSmallIntArray));
  for i := 0 to xsiz - 1 do
    GetMem(xyoffset[i], (ysiz + 1) * SizeOf(SmallInt));

  strm.Read(xoffset^, (xsiz + 1) * SizeOf(Integer));

  for i := 0 to xsiz - 1 do
    strm.Read(xyoffset[i]^, (ysiz + 1) * SizeOf(SmallInt));

  offs := xoffset[0];

  voxdatasize := numbytes - 24 - (xsiz + 1) * 4 - xsiz * (ysiz + 1) * 2;
  GetMem(voxdata, voxdatasize);
  strm.Read(voxdata^, voxdatasize);
  strm.Free;

  GetMem(kvxbuffer, SizeOf(kvxbuffer_t));
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

  x1 := fvoxelsize div 2 - xpivot div 256;
  y1 := fvoxelsize div 2 - ypivot div 256;
  z1 := fvoxelsize{ div 2} - zpivot div 256;
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

  for xx := x1 to x1 + xsiz - 1 do
    for yy := y1 to y1 + ysiz - 1 do
      for zz := z1 to z1 + zsiz - 1 do
        if kvxbuffer[xx - x1, yy - y1, zz - z1] <> $FFFF then
          voxelbuffer[xx, zz, fvoxelsize - yy - 1] := pal[kvxbuffer[xx - x1, yy - y1, zz - z1]];

  FreeMem(xoffset, (xsiz + 1) * SizeOf(Integer));
  for i := 0 to xsiz - 1 do
    FreeMem(xyoffset[i], (ysiz + 1) * SizeOf(SmallInt));
  FreeMem(xyoffset, xsiz * SizeOf(PSmallIntArray));
  FreeMem(voxdata, voxdatasize);
  FreeMem(kvxbuffer, SizeOf(kvxbuffer_t));

  vmo := TVoxelMeshOptimizer.Create;
  vmo.LoadVoxel(fvoxelsize, voxelbuffer);
  vmo.Optimize;
  vmo.SaveToFile(st);
  vmo.Free;

  FreeMem(voxelbuffer, SizeOf(voxelbuffer_t));

end;

end.
