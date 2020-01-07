//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2020 by Jim Valavanis
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
// DESCRIPTION:
//  PATCH custom image format. (Load patches inside HI_START/HI_END namespace)
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit t_patch;

interface

uses
  d_delphi,
  r_defs,
  t_main;

type
  TPatchTextureManager = object(TTextureManager)
  private
    patch: Ppatch_t;
    patchsize: integer;
  public
    constructor Create;
    function LoadHeader(stream: TDStream): boolean; virtual;
    function LoadImage(stream: TDStream): boolean; virtual;
    destructor Destroy; virtual;
  end;

function T_IsValidPatchImage(var f: file; const start, size: integer): boolean;

implementation

uses
  mt_utils,
  v_video;

constructor TPatchTextureManager.Create;
begin
  inherited Create;
  SetFileExt('.PATCH');
  patch := nil;
  patchsize := 0;
end;

function TPatchTextureManager.LoadHeader(stream: TDStream): boolean;
var
  w, h: integer;
begin
  patchsize := stream.Size;
  patch := malloc(patchsize);
  stream.seek(0, sFromBeginning);
  stream.Read(patch^, patchsize);
  w := patch.width;
  h := patch.height;
  if IsIntegerInRange(w, 0, 8192) and IsIntegerInRange(h, 0, 1024) then
  begin
    FBitmap^.SetBytesPerPixel(4);
    FBitmap^.SetWidth(w);
    FBitmap^.SetHeight(h);
    MT_ZeroMemory(FBitmap^.GetImage, w * h * 4);
    result := true;
  end
  else
  begin
    memfree(pointer(patch), patchsize);
    patchsize := 0;
    result := false;
  end;
end;

function TPatchTextureManager.LoadImage(stream: TDStream): boolean;
var
  count: integer;
  col: integer;
  column: Pcolumn_t;
  desttop: PLongWordArray;
  dest: PLongWord;
  source: PByte;
  w: integer;
  delta, prevdelta: integer;
begin
  if patch = nil then
  begin
    result := false;
    exit;
  end;

  col := 0;

  desttop := FBitmap.GetImage;

  // JVAL: Support for offsets
  FBitmap.LeftOffset := patch.leftoffset;
  FBitmap.TopOffset := patch.topoffset;

  w := patch.width;

  while col < w do
  begin
    column := Pcolumn_t(integer(patch) + patch.columnofs[col]);
    delta := 0;
    // step through the posts in a column
    while column.topdelta <> $ff do
    begin
      source := PByte(integer(column) + 3);
      delta := delta + column.topdelta;
      dest := @desttop[delta * w];
      count := column.length;

      while count > 0 do
      begin
        dest^ := default_palette[source^] or $FF000000;
        inc(source);
        inc(dest, w);
        dec(count);
      end;
      prevdelta := column.topdelta;
      column := Pcolumn_t(integer(column) + column.length + 4);
      if column.topdelta > prevdelta then
        delta := 0;
    end;
    inc(col);
    desttop := @desttop[1];
  end;

  FBitmap^.SwapRGB;

  memfree(pointer(patch), patchsize);
  result := true;
end;

destructor TPatchTextureManager.Destroy;
begin
  if patch <> nil then
    memfree(pointer(patch), patchsize);
  Inherited destroy;
end;

function T_IsValidPatchImage(var f: file; const start, size: integer): boolean;
var
  N, pos: integer;
  patch: Ppatch_t;
  col: integer;
  column: Pcolumn_t;
  desttop: integer;
  dest: integer;
  w, h: integer;
  mx: integer;
  cnt: integer;
  delta, prevdelta: integer;
begin
  result := true;

  pos := FilePos(f);

  seek(f, start);

  patch := malloc(size);

  BlockRead(f, patch^, size, N);

  w := patch.width;
  h := patch.height;

  if IsIntegerInRange(w, 0, 8192) and IsIntegerInRange(h, 0, 1024)  and (N = size) then
  begin
    col := 0;
    desttop := 0;
    mx := w * h;

    while col < w do
    begin
      if not result then
        break;

      column := Pcolumn_t(integer(patch) + patch.columnofs[col]);
      if not IsIntegerInRange(integer(column), integer(patch), integer(patch) + N - 3) then
      begin
        result := false;
        break;
      end;

      delta := 0;

      // step through the posts in a column
      cnt := 0;
      while column.topdelta <> $ff do
      begin
        if not result then
          break;

        delta := delta + column.topdelta;
        dest := desttop + (delta + column.length - 1) * w;
        if dest >= mx then
        begin
          result := false;
          break;
        end;

        prevdelta := column.topdelta;
        column := Pcolumn_t(integer(column) + column.length + 4);
        if column.topdelta > prevdelta then
          delta := 0;

        if not IsIntegerInRange(integer(column), integer(patch), integer(patch) + N - 3) then
          if col < w - 1 then
          begin
            result := false;
            break;
          end;

        inc(cnt);
        if cnt >= h then
        begin
          result := false;
          break;
        end;
      end;
      inc(col);
      inc(desttop);
    end;

  end
  else
    result := false;

  memfree(pointer(patch), size);
  seek(f, pos);
end;

end.

