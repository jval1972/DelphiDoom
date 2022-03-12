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
//  Create sprite from texture images inside S_START/S_END namespace
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit w_sprite;

interface

uses
  d_delphi;

//==============================================================================
//
// W_InitSprites
//
//==============================================================================
procedure W_InitSprites;

//==============================================================================
//
// W_ShutDownSprites
//
//==============================================================================
procedure W_ShutDownSprites;

//==============================================================================
//
// W_CacheSpriteNum
//
//==============================================================================
function W_CacheSpriteNum(const lump: integer; const ftranslation: pointer; const tag: integer): pointer;

//==============================================================================
//
// W_CacheSpriteName
//
//==============================================================================
function W_CacheSpriteName(const name: string; const ftranslation: pointer; const tag: integer): pointer;

implementation

uses
  i_system,
  r_defs,
  t_main,
  t_png,
  v_video,
  w_wad,
  z_zone;

const
  MAXSPRITETRANSLATIONS = 16;

type
  translationspriteitem_t = record
    translation: PByteArray;
    data: Pointer;
  end;

  translationsprite_t = record
    numsprites: integer;
    overwrite: integer;
    sprites: array[0..MAXSPRITETRANSLATIONS - 1] of translationspriteitem_t;
  end;
  Ptranslationsprite_t = ^translationsprite_t;
  translationsprite_tArray = array[0..$FFFF] of translationsprite_t;
  Ptranslationsprite_tArray = ^translationsprite_tArray;

type
  TSpriteLumpCache = class(TObject)
  private
    fsize: integer;
    fitems: PPointerArray;
    ftrasnlatedsprites: Ptranslationsprite_tArray;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure NotifySize(const lastsprite: integer);
    property Size: integer read fSize;
    property Items: PPointerArray read fitems;
    property Trasnlatedsprites: Ptranslationsprite_tArray read ftrasnlatedsprites;
  end;

//==============================================================================
//
// TSpriteLumpCache.Create
//
//==============================================================================
constructor TSpriteLumpCache.Create;
begin
  fsize := 0;
  fitems := nil;
  ftrasnlatedsprites := nil;
  Inherited;
end;

//==============================================================================
//
// TSpriteLumpCache.Destroy
//
//==============================================================================
destructor TSpriteLumpCache.Destroy;
begin
  memfree(pointer(fitems), fsize * SizeOf(pointer));
  memfree(pointer(ftrasnlatedsprites), fsize * SizeOf(translationsprite_t));
  fsize := 0;
  Inherited;
end;

//==============================================================================
//
// TSpriteLumpCache.NotifySize
//
//==============================================================================
procedure TSpriteLumpCache.NotifySize(const lastsprite: integer);
begin
  if fsize <> 0 then
    I_Error('TSpriteLumpCache.NotifySize(): Internal Error - Can not notify sprite cache size twice!');

  fitems := mallocz(lastsprite * SizeOf(pointer));
  ftrasnlatedsprites := mallocz(lastsprite * SizeOf(translationsprite_t));
  fsize := lastsprite;
end;

var
  spritecache: TSpriteLumpCache;

//==============================================================================
//
// W_InitSprites
//
//==============================================================================
procedure W_InitSprites;
begin
  spritecache := TSpriteLumpCache.Create;
  spritecache.NotifySize(W_NumLumps);
end;

//==============================================================================
//
// W_ShutDownSprites
//
//==============================================================================
procedure W_ShutDownSprites;
begin
  spritecache.Free;
end;

type
  patchheader_t = packed record
    width: smallint; // bounding box size
    height: smallint;
    leftoffset: smallint; // pixels to the left of origin
    topoffset: smallint;  // pixels below the origin
  end;

//==============================================================================
//
// W_TextureAsPatch
//
//==============================================================================
function W_TextureAsPatch(const tex: PTexture; const tag: integer; const user: pointer): pointer;
var
  m, fs: TDMemoryStream;
  patch: patchheader_t;
  column: column_t;
  columnofs: TDNumberList;
  columndata: TDByteList;
  x, y: integer;
  c: LongWord;
  i: integer;
  col: PLongWordArray;
  w, h: integer;
  lastdelta: integer;

  procedure flashcolumnend;
  begin
    column.topdelta := 255;
    column.length := 0;
    m.Write(column, SizeOf(column_t));
  end;

  procedure flashcolumndata;
  var
    bb: byte;
    realdelta: integer;
    restdelta: integer;
  begin
    if columndata.Count > 0 then
    begin
      realdelta := y - columndata.Count;
      if realdelta >= 254 then
      begin
        if lastdelta < 254 then
        begin
          restdelta := realdelta - 254;
          column.topdelta := 254;
          column.length := 0;
          m.Write(column, SizeOf(column_t));
          bb := 0;
          m.Write(bb, SizeOf(bb));
          m.Write(bb, SizeOf(bb));
        end
        else
          restdelta := realdelta - lastdelta;
        while restdelta >= 255 do
        begin
          restdelta := restdelta - 254;
          column.topdelta := 254;
          column.length := 0;
          m.Write(column, SizeOf(column_t));
          bb := 0;
          m.Write(bb, SizeOf(bb));
          m.Write(bb, SizeOf(bb));
        end;
        column.topdelta := restdelta;
      end
      else
        column.topdelta := y - columndata.Count;
      lastdelta := realdelta;
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
  tex.SetDefaultAlphaChannel;

  m := TDMemoryStream.Create;
  fs := TDMemoryStream.Create;
  columnofs := TDNumberList.Create;
  columndata := TDByteList.Create;
  try
    w := tex.GetWidth;
    h := tex.GetHeight;
    patch.width := w;
    patch.height := h;
    patch.leftoffset := tex.LeftOffset;
    patch.topoffset := tex.TopOffset;
    fs.Write(patch, SizeOf(patchheader_t));

    col := malloc(h * SizeOf(LongWord));
    for x := 0 to w - 1 do
    begin
      columnofs.Add(m.Position + SizeOf(patchheader_t) + w * SizeOf(integer));
      columndata.FastClear;
      tex.GetColumn32(x, h, col);
      lastdelta := -1;
      for y := 0 to h - 1 do
      begin
        c := col[y];
        if c and $FFFFFF = 0 then
        begin
          flashcolumndata;
          continue;
        end;
        if columndata.Count = 128 then
          flashcolumndata;
        columndata.Add(V_FindAproxColorIndex(default_palette, c))
      end;
      flashcolumndata;
      flashcolumnend;
    end;
    memfree(pointer(col), h * SizeOf(LongWord));

    for i := 0 to columnofs.Count - 1 do
    begin
      x := columnofs.Numbers[i];
      fs.Write(x, SizeOf(integer));
    end;

    result := Z_Malloc(fs.Size + m.Size, tag, user);

    memcpy(result, fs.Memory, fs.Size);
    memcpy(pointer(integer(result) + fs.Size), m.Memory, m.Size);

  finally
    m.Free;
    columnofs.Free;
    columndata.Free;
    fs.Free;
  end;
end;

//==============================================================================
//
// W_CacheSpriteNumUntranslated
//
//==============================================================================
function W_CacheSpriteNumUntranslated(const lump: integer; const tag: integer): pointer;
var
  ext: string;
  tm: PTextureManager;
  strm: TAttachableMemoryStream;
  buf: array[0..3] of byte;
  tex: PTexture;
  data: pointer;
  N: integer;
begin
  data := spritecache.Items[lump];
  if integer(data) > 1 then
  begin
    result := data;
    Exit;
  end
  else
    result := W_CacheLumpNum(lump, tag);

  if integer(data) = 1 then
    Exit;

  spritecache.Items[lump] := Pointer($1);

  strm := TAttachableMemoryStream.Create;
  strm.Attach(result, W_LumpLength(lump));

  ZeroMemory(@buf, SizeOf(buf));

  N := strm.Read(buf, 4);
  if N <> 4 then
  begin
    strm.Free;
    Exit;
  end;

  if (buf[1] = $50) and (buf[2] = $4E) and (buf[3] = $47) then // PNG
    ext := PNGSPRITEEXT
  else if (buf[0] = $42) and (buf[1] = $4D) then // BMP
    ext := '.BMP'
  else
  begin
    strm.Free;
    Exit;
  end;

  strm.Seek(0, sFromBeginning);

  tm := GetImageFormat(ext);
  if tm = nil then
    I_Error('W_CacheSpriteNum(): Internal Error - texture manager is null');

  tex := new(PTexture, Create);
  tm^.SetBitmap(tex);
  if tm^.LoadFromStream(strm) then
    result := W_TextureAsPatch(tex, tag, @spritecache.Items[lump])
  else
    result := nil;
  Dispose(tex, Destroy);
  strm.Free;
end;

//==============================================================================
//
// W_CacheSpriteNum
//
//==============================================================================
function W_CacheSpriteNum(const lump: integer; const ftranslation: pointer; const tag: integer): pointer;
var
  i: integer;
  trn: Ptranslationsprite_t;
  patch, newpatch: Ppatch_t;
  freespot: integer;
  sz: integer;
  col: integer;
  column: Pcolumn_t;
  checkcols: TDNumberList;
  offs, count: integer;
  source: PByte;
  ftrans: PByteArray;
begin
  if ftranslation = nil then
  begin
    result := W_CacheSpriteNumUntranslated(lump, tag);
    exit;
  end;

  trn := @spritecache.Trasnlatedsprites[lump];
  for i := 0 to trn.numsprites - 1 do
  begin
    if trn.sprites[i].translation = ftranslation then
    begin
      result := trn.sprites[i].data;
      exit;
    end;
  end;

  if trn.numsprites < MAXSPRITETRANSLATIONS then
  begin
    freespot := trn.numsprites;
    inc(trn.numsprites);
  end
  else
  begin
    freespot := -1;
    for i := 0 to MAXSPRITETRANSLATIONS - 1 do
      if trn.sprites[i].data = nil then
      begin
        freespot := i;
        break;
      end;

    if freespot < 0 then
    begin
      freespot := trn.overwrite;
      Inc(trn.overwrite);
      if trn.overwrite = MAXSPRITETRANSLATIONS then
        trn.overwrite := 0;
    end;
  end;

  patch := W_CacheSpriteNumUntranslated(lump, tag);
  if patch = nil then
  begin
    result := nil;
    exit;
  end;

  if trn.sprites[freespot].data <> nil then
    Z_Free(trn.sprites[freespot].data);

  sz := Z_Size(patch);
  newpatch := Z_Malloc(sz, tag, @trn.sprites[freespot].data);
  memcpy(newpatch, patch, sz);

  ftrans := ftranslation;
  trn.sprites[freespot].translation := ftranslation;

  checkcols := TDNumberList.Create;

  for col := 0 to newpatch.width - 1 do
  begin
    offs := newpatch.columnofs[col];
    // JVAL: Avoid compressed columns
    if checkcols.IndexOf(offs) < 0 then
      checkcols.Add(offs)
    else
      Continue;

    column := Pcolumn_t(integer(newpatch) + offs);

    // step through the posts in a column
    while column.topdelta <> $ff do
    begin
      source := PByte(integer(column) + 3);
      count := column.length;

      while count > 0 do
      begin
        source^ := ftrans[source^];
        inc(source);
        dec(count);
      end;
      column := Pcolumn_t(integer(column) + column.length + 4);
    end;

  end;

  checkcols.Free;

  result := newpatch;
end;

//==============================================================================
//
// W_CacheSpriteName
//
//==============================================================================
function W_CacheSpriteName(const name: string; const ftranslation: pointer; const tag: integer): pointer;
begin
  result := W_CacheSpriteNum(W_GetNumForName(name, TYPE_SPRITE), ftranslation, tag);
end;

end.
