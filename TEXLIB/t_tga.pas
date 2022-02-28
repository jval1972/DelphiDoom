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
//  TGA image format.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

{
   *********************************************************************
   Version: 1998.06.09
   Copyright (C) 1997, 1998 Gertjan Schouten

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   *********************************************************************

   Modified by Carl Eric Codere <ccodere@ieee.org>:
   Fixes for FPC 1.0.x compilation (2nd Februrary 2003)
}

unit t_tga;

interface

uses
  d_delphi,
  t_main;

type
  TTGAHeader = packed record
    idlen: byte;
    cmtype: byte;
    imgtype: byte;
    cmorg: word;
    cmlen: word;
    cmes: byte;
    xorg: word;
    yorg: word;
    width: word;
    height: word;
    pixsize: byte;
    desc: byte;
  end;

  TTGATextureManager = object(TTextureManager)
    hdr: TTGAHeader;
    id: pointer;
    function LoadPalette(stream: TDStream): boolean;
    function Load32bittga(stream: TDStream): boolean;
    function load24bittga(stream: TDStream): boolean;
    function load16bittga(stream: TDStream): boolean;
    function load8bitgraytga(stream: TDStream): boolean;
    function Load8bitpaltga(stream: TDStream): boolean;
  public
    constructor Create;
    function LoadHeader(stream: TDStream): boolean; virtual;
    function LoadImage(stream: TDStream): boolean; virtual;
    destructor Destroy; virtual;
  end;

implementation

//==============================================================================
//
// TTGATextureManager.LoadPalette
//
//==============================================================================
function TTGATextureManager.LoadPalette(stream: TDStream): boolean;
var
  palet: pointer;
begin
  if hdr.cmLen > 0 then
  begin
    Palet := malloc(hdr.cmLen * 4);
    case hdr.cmes of
      15, 16:
        begin
          stream.read(Palet^, hdr.cmLen * 2);
          FBitmap^.setPalette(Palet, hdr.cmLen, ptBGR5, 2);
        end;
      24:
        begin
          stream.read(Palet^, hdr.cmlen * 3);
          FBitmap^.setPalette(Palet, hdr.cmLen, ptBGR8 ,3);
         end;
      32:
        begin
          stream.read(Palet^, hdr.cmLen * 4);
          FBitmap^.setPalette(Palet, hdr.cmLen, ptBGR8, 4);
        end;
    end;
    memfree(Palet, hdr.cmLen * 4);
  end;
  LoadPalette := true;
end;

//==============================================================================
//
// TTGATextureManager.Load8bitpaltga
//
//==============================================================================
function TTGATextureManager.Load8bitpaltga(stream: TDStream): boolean;
var
  pixel, rle: byte;
  ix, x, y: integer;
  buffer: pointer;
begin
  if not (hdr.cmes in [15, 16, 24, 32]) then
  begin
    result := false;
    exit;
  end;
  buffer := malloc(hdr.width);
  Load8bitpaltga := true;
  if hdr.imgtype = 1 then
  begin
    for y := hdr.height - 1 downto 0 do
    begin
      stream.read(buffer^, hdr.width);
      FBitmap^.PutPixels(0, y, hdr.width, buffer, 8);
    end;
  end
  else if hdr.imgtype = 9 then
  begin
    x := 0;
    y := hdr.height - 1;
    while not (y < 0) do
    begin
      stream.read(rle, 1);
      if rle > 127 then
      begin
        rle := rle - 128;
        stream.read(pixel, 1);
        for ix := 0 to rle do
        begin
          byte(pointer(integer(buffer) + x)^) := pixel;
          x := x + 1;
          if x >= hdr.width then
          begin
            FBitmap^.PutPixels(0, y, hdr.width, buffer, 8);
            dec(y);
            x := 0;
            if y < 0 then
              break;
          end;
        end;
      end
      else
      begin
        for ix := 0 to rle do
        begin
          stream.read(pixel, 1);
          byte(pointer(integer(buffer) + x)^) := pixel;
          x := x + 1;
          if (x >= hdr.width) then
          begin
            FBitmap^.PutPixels(0, y, hdr.width, buffer, 8);
            dec(y);
            x := 0;
            if y < 0 then
              break;
            end;
          end;
        end;
      end;
   end
  else
    Load8bitpaltga := false;
  memfree(buffer, hdr.width);
end;

//==============================================================================
//
// TTGATextureManager.load8bitgraytga
//
//==============================================================================
function TTGATextureManager.load8bitgraytga(stream: TDStream): boolean;
begin
  load8bitgraytga := false;
end;

//==============================================================================
//
// TTGATextureManager.load16bittga
//
//==============================================================================
function TTGATextureManager.load16bittga(stream: TDStream): boolean;
var
  rle: byte;
  pixel: word;
  ix, x, y: integer;
  buffer: pointer;
begin
  load16bittga := true;
  buffer := malloc(hdr.width * 2);
  if hdr.imgtype = 2 then
  begin
    for y := hdr.height - 1 downto 0 do
    begin
      stream.read(buffer^, hdr.width * 2);
      FBitmap^.PutPixels(0, y, hdr.width, buffer, 16);
    end;
  end
  else if hdr.imgtype = 10 then
  begin
    x := 0;
    y := hdr.height - 1;
    while y >= 0 do
    begin
      stream.read(rle, 1);
      if rle > 127 then
      begin
         rle := rle - 128;
         stream.read(pixel, 2);
         for ix := 0 to rle do
         begin
           word(pointer(integer(buffer) + x * 2)^) := pixel;
           x := x + 1;
           if x >= hdr.width then
           begin
             FBitmap^.PutPixels(0, y, hdr.width, buffer, 16);
             dec(y);
             x := 0;
             if y < 0 then
              break;
           end;
         end;
      end
      else
      begin
        for ix := 0 to rle do
        begin
          stream.read(pixel, 2);
          word(pointer(integer(buffer) + x * 2)^) := pixel;
          x := x + 1;
          if x >= hdr.width then
          begin
            FBitmap^.PutPixels(0, y, hdr.width, buffer, 16);
            dec(y);
            x := 0;
            if y < 0 then
              break;
          end;
        end;
      end;
    end;
  end
  else
    load16bittga := false;
  memfree(buffer, hdr.width * 2);
end;

//==============================================================================
//
// TTGATextureManager.load24bittga
//
//==============================================================================
function TTGATextureManager.load24bittga(stream: TDStream):boolean;
var
  rle: byte;
  pixel: integer;
  ix, x, y: integer;
  buffer: pointer;
begin
  load24bittga := true;
  buffer := malloc(hdr.width * 3);
  if hdr.imgtype = 2 then
  begin
    for y := hdr.height - 1 downto 0 do
    begin
      stream.read(buffer^, hdr.width * 3);
      FBitmap^.PutPixels(0, y, hdr.width, buffer, 24);
    end;
  end
  else if hdr.imgtype = 10 then
  begin
    x := 0;
    y := hdr.height - 1;
    while not (y < 0) do
    begin
      stream.read(rle, 1);
      if rle > 127 then
      begin
        rle := rle - 128;
        stream.read(pixel, 3);
        for ix := 0 to rle do
        begin
          integer(pointer(integer(buffer) + x * 3)^) := pixel;
          x := x + 1;
          if x >= hdr.width then
          begin
            FBitmap^.PutPixels(0, y, hdr.width, buffer, 24);
            dec(y);
            x := 0;
            if y < 0 then
              break;
          end;
        end;
      end
      else
      begin
        for ix := 0 to rle do
        begin
          stream.read(pixel, 3);
          integer(pointer(integer(buffer) + x * 3)^) := pixel;
          x := x + 1;
          if x >= hdr.width then
          begin
            FBitmap^.PutPixels(0, y, hdr.width, buffer, 24);
            dec(y);
            x := 0;
            if y < 0 then
              break;
          end;
        end;
      end;
    end;
  end
  else
    load24bittga := false;
  memfree(buffer, hdr.width * 3);
end;

//==============================================================================
//
// TTGATextureManager.Load32bittga
//
//==============================================================================
function TTGATextureManager.Load32bittga(stream: TDStream):boolean;
var
  rle: byte;
  pixel: integer;
  ix, x, y: integer;
  buffer: pointer;
begin
  Load32bittga := true;
  buffer := malloc(hdr.width * 4);
  if hdr.imgtype = 2 then
  begin
    for y := hdr.height - 1 downto 0 do
    begin
      stream.read(buffer^, hdr.width * 4);
      FBitmap^.PutPixels(0, y, hdr.width, buffer, 32);
    end;
  end
  else if hdr.imgtype = 10 then
  begin
    x := 0;
    y := hdr.height - 1;
    while not (y < 0) do
    begin
      stream.read(rle, 1);
      if rle > 127 then
      begin
         rle := rle - 128;
         stream.read(pixel, 4);
         for ix := 0 to rle do
         begin
           integer(pointer(integer(buffer) + x * 4)^) := pixel;
           x := x + 1;
           if x >= hdr.width then
           begin
             FBitmap^.PutPixels(0, y, hdr.width, buffer, 32);
             dec(y);
             x := 0;
             if y < 0 then
              break;
           end;
         end;
      end
      else
      begin
        for ix := 0 to rle do
        begin
          stream.read(pixel, 4);
          integer(pointer(integer(buffer) + x * 4)^) := pixel;
          x := x + 1;
          if (x >= hdr.width) then
          begin
            FBitmap^.PutPixels(0, y, hdr.width, buffer, 32);
            dec(y);
            x := 0;
            if y < 0 then
              break;
          end;
        end;
      end;
    end;
  end
  else
    Load32bittga := false;
  memfree(buffer, hdr.width * 4);
end;

//==============================================================================
//
// TTGATextureManager.Create
//
//==============================================================================
constructor TTGATextureManager.Create;
begin
  inherited Create;
  SetFileExt('.TGA');
  FFrameCount := 10;
end;

//==============================================================================
//
// TTGATextureManager.LoadHeader
//
//==============================================================================
function TTGATextureManager.LoadHeader(stream: TDStream):boolean;
begin
  if id <> nil then
    memfree(id, hdr.idlen);
  id := nil;
  stream.seek(0, sFromBeginning);
  stream.read(hdr, SizeOf(TTGAHeader));
  if FBitmap^.getBytesPerPixel = 0 then
  begin
    case hdr.PixSize of
       8:
        FBitmap^.setBytesPerPixel(1);
      16:
        FBitmap^.setBytesPerPixel(2);
      24, 32:
        FBitmap^.setBytesPerPixel(4);
    end;
  end;
  FBitmap^.SetWidth(hdr.Width);
  FBitmap^.SetHeight(hdr.Height);
  if hdr.idlen <> 0 then
  begin
    id := malloc(hdr.idlen);
    stream.read(id^, hdr.idlen);
  end;
  LoadPalette(stream);
  LoadHeader := true;
end;

//==============================================================================
//
// TTGATextureManager.LoadImage
//
//==============================================================================
function TTGATextureManager.LoadImage(stream: TDStream):boolean;
begin
  LoadImage := true;
  case hdr.pixsize of
     8:
      begin
        case hdr.imgtype of
           1, 9:
            load8bitpaltga(stream);
           3, 11:
            load8bitgraytga(stream);
        end;
      end;
    16:
      load16bittga(stream);
    24:
      load24bittga(stream);
    32:
      load32bittga(stream);
  else
    LoadImage := false;
  end;
end;

//==============================================================================
//
// TTGATextureManager.Destroy
//
//==============================================================================
destructor TTGATextureManager.Destroy;
begin
  if id <> nil then
    memfree(id, hdr.idlen);
  Inherited destroy;
end;

end.

