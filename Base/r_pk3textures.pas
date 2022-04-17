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
//  Textrues in PK3 without TEXXTUREx equivalent
//  Flats in PK3 without F_START/F_END equivalent
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_pk3textures;

interface

//==============================================================================
//
// R_InitPK3Textures
//
//==============================================================================
function R_InitPK3Textures: boolean;

implementation

uses
  d_delphi,
  m_argv,
  m_sha1,
  mt_utils,
  r_data,
  r_defs,
  t_main,
  v_video,
  w_pak,
  w_wadreader,
  w_wadwriter,
  w_wad;

var
  t_names: TDStringList;
  f_names: TDStringList;

//==============================================================================
//
// R_AddTextureFileName
//
//==============================================================================
procedure R_AddTextureFileName(const filename: string);
var
  check: string;
  name: string;
  ext: string;
begin
  check := strupper(filename);
  ext := fext(check);
  if TextureExtensions.IndexOf(ext) >= 0 then
    if Pos(s_TEX_PATH, fixpathname(check)) > 0 then
    begin
      check := fname(check);
      name := firstword_ch(check, '.');
      if name + '.' + ext = check then  // Only one '.'
        if Length(name) <= 8 then
          if W_CheckNumForName(name) < 0 then
          begin
            if t_names.IndexOf(name) < 0 then
              if R_CheckTextureNumForName(name) <= 0 then
              begin
                t_names.Add(name);
                f_names.Add(name);
                Exit;
              end;
            if f_names.IndexOf(name) < 0 then
              f_names.Add(name);
          end;
    end;
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
// R_TextureAsPatch
//
//==============================================================================
procedure R_TextureAsPatch(const tex: PTexture; var p: pointer; var size: integer);
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

    size := fs.Size + m.Size;
    p := malloc(size);

    memcpy(p, fs.Memory, fs.Size);
    memcpy(pointer(integer(p) + fs.Size), m.Memory, m.Size);
  finally
    m.Free;
    columnofs.Free;
    columndata.Free;
    fs.Free;
  end;
end;

type
  copyflat_t = record
    src: PLongWordArray;
    dest: PByteArray;
    size: integer;
  end;
  Pcopyflat_t = ^copyflat_t;

//==============================================================================
//
// R_CopyFlat_thr
//
//==============================================================================
function R_CopyFlat_thr(it: iterator_p): integer; stdcall;
var
  parms: Pcopyflat_t;
  i: integer;
begin
  parms := it.data;
  i := it.idx;
  while i < parms.size do
  begin
    parms.dest[i] := V_FindAproxColorIndex(default_palette, parms.src[i]);
    Inc(i, it.numidxs);
  end;
  result := 0;
end;

//==============================================================================
//
// R_TextureAsFlat
//
//==============================================================================
function R_TextureAsFlat(const tex: PTexture; var p: pointer; var size: integer): boolean;
var
  w, h: integer;
  sz: integer;
  cache: PLongWordPArray;
  i: integer;
  parms: copyflat_t;
begin
  Result := False;

  w := tex.GetWidth;
  h := tex.GetHeight;

  if (w = 0) or (h = 0) then
    Exit;

  if (w mod h <> 0) and (h mod w <> 0) then
    Exit;

  Result := True;

  tex.ConvertTo32bit;
  tex.RemoveTransparency;

  if w > h then
  begin
    sz := w;
    cache := malloc(h * SizeOf(PLongWordArray));
    for i := 0 to h - 1 do
    begin
      cache[i] := malloc(w * SizeOf(LongWord));
      tex.GetRow32(i, w, cache[i]);
    end;
    tex.SetHeight(w);
    for i := 0 to w - 1 do
      tex.SetRow32(i, cache[i mod h]);
    for i := 0 to h - 1 do
      memfree(Pointer(cache[i]), w * SizeOf(LongWord));
    memfree(Pointer(cache), h * SizeOf(PLongWordArray));
  end
  else if h > w then
  begin
    sz := h;
    cache := malloc(w * SizeOf(PLongWordArray));
    for i := 0 to w - 1 do
    begin
      cache[i] := malloc(h * SizeOf(LongWord));
      tex.GetColumn32(i, h, cache[i]);
    end;
    tex.SetWidth(h);
    for i := 0 to h - 1 do
      tex.SetColumn32(i, cache[i mod w]);
    for i := 0 to w - 1 do
      memfree(Pointer(cache[i]), h * SizeOf(LongWord));
    memfree(Pointer(cache), w * SizeOf(PLongWordArray));
  end
  else
    sz := h;

  if (sz <> 64) and (sz <> 128) and (sz <> 256) and (sz <> 512) and
    (sz <> 1024) and (sz <> 2048) and (sz <> 4096) then
  begin
    if sz < 64 then
      sz := 64
    else if sz < 128 then
      sz := 128
    else if sz < 256 then
      sz := 256
    else if sz < 512 then
      sz := 512
    else if sz < 1024 then
      sz := 1024
    else if sz < 2048 then
      sz := 2048
    else
      sz := 4096;
    tex.ScaleTo(sz, sz);
  end;

  size := sz * sz;
  p := malloc(size);
  parms.src := tex.GetImage;
  parms.dest := p;
  parms.size := size;
  MT_Iterate(@R_CopyFlat_thr, @parms);
end;

//==============================================================================
//
// Hash
//
//==============================================================================
function Hash(const name: string): integer;
var
  b: Byte;
  i: integer;
  len: integer;
begin
  len := Length(name);
  if len = 0 then
  begin
    Result := 0;
    Exit;
  end;

  b := Ord(name[1]);

  Result := 5381 * 33 + b;

  for i := 2 to len do
  begin
    b := Ord(name[i]);
    Result := Result * 33 + b;
  end;
end;

//==============================================================================
//
// R_InitPK3Textures
//
//==============================================================================
function R_InitPK3Textures: boolean;
var
  wad1: TWADWriter;
  wad1filename: string;
  wad2: TWADWriter;
  wad2filename: string;
  reader: TWadReader;
  t_ok, f_ok: Boolean;
  i: integer;
  p: pointer;
  size: integer;
  c8: char8_t;
  mp, mt: TDMemoryStream;
  tex: maptexture_t;
  psize: integer;
  t: PTexture;
begin
  Result := False;

  t_names := TDStringList.Create;
  f_names := TDStringList.Create;

  PAK_FileNameIterator(@R_AddTextureFileName);

  if f_names.Count > 0 then
  begin
    t_names.Sort;
    f_names.Sort;

    wad1filename := M_SaveFileName('DATA\');
    MkDir(wad1filename);
    wad1filename := wad1filename + 'WADS\';
    MkDir(wad1filename);
    wad1filename := wad1filename + 'pk3tex_' + uitoa(PAK_GetGlobalHash) + '_' + itoa(Hash(t_names.Text)) + '.wad';
    // Check if the texture cache exists
    t_ok := False;
    if fexists(wad1filename) then
    begin
      reader := TWadReader.Create;
      reader.OpenWadFile(wad1filename);
      if reader.NumEntries > 4 then
        if reader.EntryName(0) = 'P_START' then
          t_ok := True;
      reader.Free;
    end;

    wad2filename := M_SaveFileName('DATA\');
    MkDir(wad2filename);
    wad2filename := wad2filename + 'WADS\';
    MkDir(wad2filename);
    wad2filename := wad2filename + 'pk3flat_' + uitoa(PAK_GetGlobalHash) + '_' + itoa(Hash(f_names.Text)) + '.wad';
    // Check if the flat cache exists
    f_ok := False;
    if fexists(wad2filename) then
    begin
      reader := TWadReader.Create;
      reader.OpenWadFile(wad2filename);
      if reader.NumEntries > 2 then
        if reader.EntryName(0) = 'F_START' then
          f_ok := True;
      reader.Free;
    end;

    if t_ok and f_ok then
    begin
      W_RuntimeLoad(wad1filename, F_ORIGIN_WAD);
      W_RuntimeLoad(wad2filename, F_ORIGIN_WAD);
      Result := True;
    end
    else
    begin
      printf('R_InitPK3Textures(): Create textures and flats from PK3, please wait'#13#10);
      printf('---------------------------------------------------------------------'#13#10);
      printf('  This procedure is performed only once and it will not be repeated at next run'#13#10);
      printf('---------------------------------------------------------------------'#13#10);
      wad1 := TWadWriter.Create;
      wad2 := TWadWriter.Create;

      mp := TDMemoryStream.Create;  // PNAMES0
      mt := TDMemoryStream.Create;  // TEXTURE0

      psize := 0; // Will be set after

      // PNAMES header
      mp.Write(psize, SizeOf(integer));

      // TEXTURE1 header
      mt.Write(psize, SizeOf(integer));

      wad1.AddSeparator('P_START');
      wad2.AddSeparator('F_START');
      for i := 0 to f_names.Count - 1 do
      begin
        t := T_LoadHiResTexture(f_names.Strings[i]);
        if t <> nil then
        begin
          if t_names.IndexOf(f_names.Strings[i]) >= 0 then
          begin
            R_TextureAsPatch(t, p, size);
            wad1.AddData(f_names.Strings[i], p, size);
            memfree(p, size);

            // Save PNAMES0 entry
            c8 := stringtochar8(t_names.Strings[i]);
            mp.Write(c8, 8);

            // Save TEXTURE0 entry
            ZeroMemory(@tex, SizeOf(maptexture_t));
            tex.name := c8;
            tex.width := t.GetWidth;
            tex.height := t.GetHeight;
            tex.patchcount := 1;
            tex.patches[0].patch := psize;
            mt.Write(tex, SizeOf(maptexture_t));

            Inc(psize);
          end;

          if R_TextureAsFlat(t, p, size) then
          begin
            wad2.AddData(f_names.Strings[i], p, size);
            memfree(p, size);
          end;

          Dispose(t, Destroy);
        end;
      end;
      wad1.AddSeparator('P_END');
      wad2.AddSeparator('F_END');

      if psize > 0 then
      begin
        mp.Seek(0, sFromBeginning);
        mp.Write(psize, SizeOf(integer));
        wad1.AddData('PNAMES0', mp.Memory, mp.Size); // Unused
        mp.Free;

        mt.Seek(0, sFromBeginning);
        mt.Write(psize, SizeOf(integer));
        wad1.AddData('TEXTURE0', mt.Memory, mt.Size);
        mt.Free;

        wad1.SaveToFile(wad1filename);

        W_RuntimeLoad(wad1filename, F_ORIGIN_WAD);
        Result := True;
      end
      else
      begin
        mp.Free;
        mt.Free;
      end;

      if wad2.NumEntries > 2 then
      begin
        wad2.SaveToFile(wad2filename);

        W_RuntimeLoad(wad2filename, F_ORIGIN_WAD);
      end;

      wad1.Free;
      wad2.Free;
    end;
  end;

  t_names.Free;
  f_names.Free;
end;

end.
