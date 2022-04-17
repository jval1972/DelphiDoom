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
  i_tmp,
  m_argv,
  m_sha1,
  r_data,
  r_defs,
  t_main,
  v_video,
  w_pak,
  w_wadwriter,
  w_wad;

var
  t_names: TDStringList;

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
      if Length(name) <= 8 then
        if t_names.IndexOf(name) < 0 then
          if W_CheckNumForName(name) < 0 then
            if R_CheckTextureNumForName(name) <= 0 then
              t_names.Add(name);
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

//==============================================================================
//
// R_InitPK3Textures
//
//==============================================================================
function R_InitPK3Textures: boolean;
var
  wad: TWADWriter;
  wadfilename: string;
  i: integer;
  p: pointer;
  size: integer;
  c8: char8_t;
  mp, mt: TDMemoryStream;
  tex: maptexture_t;
  psize: integer;
  mem: TDMemoryStream;
  t: PTexture;
begin
  Result := False;

  t_names := TDStringList.Create;

  PAK_FileNameIterator(@R_AddTextureFileName);

  if t_names.Count > 0 then
  begin
    t_names.Sort;

    wad := TWadWriter.Create;

    mp := TDMemoryStream.Create;  // PNAMES0
    mt := TDMemoryStream.Create;  // TEXTURE0

    psize := 0; // Will be set after

    // PNAMES header
    mp.Write(psize, SizeOf(integer));

    // TEXTURE1 header
    mt.Write(psize, SizeOf(integer));

    wad.AddSeparator('P_START');
    for i := 0 to t_names.Count - 1 do
    begin
      t := T_LoadHiResTexture(t_names.Strings[i]);
      if t <> nil then
      begin
        R_TextureAsPatch(t, p, size);
        wad.AddData(t_names.Strings[i], p, size);
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

        Dispose(t, Destroy);
      end;
    end;
    wad.AddSeparator('P_END');

    if psize > 0 then
    begin
      mp.Seek(0, sFromBeginning);
      mp.Write(psize, SizeOf(integer));
      wad.AddData('PNAMES0', mp.Memory, mp.Size); // Unused
      mp.Free;

      mt.Seek(0, sFromBeginning);
      mt.Write(psize, SizeOf(integer));
      wad.AddData('TEXTURE0', mt.Memory, mt.Size);
      mt.Free;

      mem := TDMemoryStream.Create;

      wad.SaveToStream(mem);

      wadfilename := M_SaveFileName('DATA\');
      MkDir(wadfilename);
      wadfilename := wadfilename + 'TMP\';
      MkDir(wadfilename);
      wadfilename := wadfilename + 'pk3tex_' + readablestring(SHA1_CalcSHA1Buf(mem.memory^, mem.Size)) + '.wad';
      mem.Free;

      wad.SaveToFile(wadfilename);

      W_RuntimeLoad(wadfilename, F_ORIGIN_WAD);
      Result := True;

      I_DeclareTempFile(wadfilename);
    end
    else
    begin
      mp.Free;
      mt.Free;
    end;

    wad.Free;
  end;

  t_names.Free;
end;

end.
