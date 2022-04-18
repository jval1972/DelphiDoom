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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  FLAT custom image format. (Load flats inside TX_START/TX_END namespace)
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit t_flat;

interface

uses
  d_delphi,
  t_main;

type
  TFlatTextureManager = object(TTextureManager)
  private
    flat: PByteArray;
    flatsize: integer;
  public
    constructor Create;
    function LoadHeader(stream: TDStream): boolean; virtual;
    function LoadImage(stream: TDStream): boolean; virtual;
    destructor Destroy; virtual;
  end;

//==============================================================================
//
// T_IsValidFlatImage
//
//==============================================================================
function T_IsValidFlatImage(var f: file; const start, size: integer): boolean; overload;

implementation

uses
  mt_utils,
  v_video;

//==============================================================================
//
// TFlatTextureManager.Create
//
//==============================================================================
constructor TFlatTextureManager.Create;
begin
  inherited Create;
  SetFileExt('.FLAT');
  flat := nil;
  flatsize := 0;
end;

//==============================================================================
//
// TFlatTextureManager.LoadHeader
//
//==============================================================================
function TFlatTextureManager.LoadHeader(stream: TDStream): boolean;
var
  w, h: integer;
begin
  flatsize := stream.Size;
  if (flatsize = 64 * 64) or (flatsize = 64 * 128) then
  begin
    w := 64;
    h := 64;
    flatsize := w * h;
  end
  else if flatsize = 128 * 128 then
  begin
    w := 128;
    h := 128;
  end
  else if flatsize = 256 * 256 then
  begin
    w := 256;
    h := 256;
  end
  else if flatsize = 512 * 512 then
  begin
    w := 512;
    h := 512;
  end
  else if flatsize = 1024 * 1024 then
  begin
    w := 1024;
    h := 1024;
  end
  else if flatsize = 2048 * 2048 then
  begin
    w := 2048;
    h := 2048;
  end
  else if flatsize = 4096 * 4096 then
  begin
    w := 4096;
    h := 4096;
  end
  else
  begin
    result := false;
    exit;
  end;

  flat := malloc(flatsize);
  stream.seek(0, sFromBeginning);
  stream.Read(flat^, flatsize);

  FBitmap^.SetBytesPerPixel(4);
  FBitmap^.SetWidth(w);
  FBitmap^.SetHeight(h);
  MT_ZeroMemory(FBitmap^.GetImage, w * h * 4);
  result := true;
end;

//==============================================================================
//
// TFlatTextureManager.LoadImage
//
//==============================================================================
function TFlatTextureManager.LoadImage(stream: TDStream): boolean;
var
  i: integer;
  desttop: PLongWordArray;
begin
  if flat = nil then
  begin
    result := false;
    exit;
  end;

  desttop := FBitmap.GetImage;
  for i := 0 to flatsize - 1 do
    desttop[i] := default_palette[flat[i]] or $FF000000;

  FBitmap^.SwapRGB;

  memfree(pointer(flat), flatsize);
  result := true;
end;

//==============================================================================
//
// TFlatTextureManager.Destroy
//
//==============================================================================
destructor TFlatTextureManager.Destroy;
begin
  if flat <> nil then
    memfree(pointer(flat), flatsize);
  Inherited destroy;
end;

//==============================================================================
//
// T_IsValidFlatImage
//
//==============================================================================
function T_IsValidFlatImage(var f: file; const start, size: integer): boolean;
begin
  Result :=
    (size = 64 * 64) or
    (size = 64 * 128) or
    (size = 128 * 128) or
    (size = 256 * 256) or
    (size = 512 * 512) or
    (size = 1024 * 1024) or
    (size = 2048 * 2048) or
    (size = 4096 * 4096);
end;

end.

