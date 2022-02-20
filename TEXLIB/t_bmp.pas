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
//  BMP image format.
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

   Modified by Matthias K"oppe <mkoeppe@cs.uni-magdeburg.de>:
   Added support for 1-bpp and 4-bpp bitmaps; fixed a bug with 8-bpp bitmaps.

   Modified by Carl Eric Codere <ccodere@ieee.org>:
   Fixes for FPC 1.0.x compilation (2nd Februrary 2003)

}

unit t_bmp;

interface

uses
  d_delphi,
  t_main;

type
  TBitMapFileHeader = packed record
    bfType: Word;       // is always 19778 : 'BM'
    bfSize: LongWord;   // Filesize
    bfReserved1: Word;
    bfReserved2: Word;
    bfOffset: LongWord; // Offset of image data
  end;

  TBitMapInfoHeader = packed record
    Size: integer;
    Width: integer;
    Height: integer;
    Planes: word;
    BitCount: word;
    Compression: integer;
    SizeImage: integer;
    XPelsPerMeter: integer;
    YPelsPerMeter: integer;
    ClrUsed: integer;
    ClrImportant: integer;
  end;

  TBitmapCoreHeader = packed record
    bcSize: LongWord;
    bcWidth: word;
    bcHeight: word;
    bcPlanes: word;
    bcBitCount: word;
  end;

  TBMPTextureManager = object(TTextureManager)
  private
    BFH: TBitMapFileHeader;
    BFI: TBitMapInfoHeader;
    pal: array[0..255] of integer;
  public
    constructor Create;
    function LoadHeader(stream: TDStream): boolean; virtual;
    procedure LoadRLE8(stream: TDStream);
    function LoadImage(stream: TDStream): boolean; virtual;
  end;

implementation

const
   biRGB = 0;
   biRLE8 = 1;

//==============================================================================
//
// TBMPTextureManager.Create
//
//==============================================================================
constructor TBMPTextureManager.Create;
begin
  inherited Create;
  SetFileExt('.BMP');
end;

//==============================================================================
//
// TBMPTextureManager.LoadHeader
//
//==============================================================================
function TBMPTextureManager.LoadHeader(stream: TDStream): boolean;
begin
  stream.seek(0, sFromBeginning);
  stream.read(bfh, SizeOf(TBitMapFileHeader));
  stream.read(bfi, SizeOf(TBitMapInfoHeader));
  if FBitmap^.GetBytesPerPixel = 0 then
  begin
    case bfi.bitcount of
       1, 4, 8:
        FBitmap^.SetBytesPerPixel(1);
    else
      FBitmap^.SetBytesPerPixel(4);
    end;
  end;
  FBitmap^.SetWidth(bfi.Width);
  FBitmap^.SetHeight(bfi.Height);
  if bfi.bitCount = 8 then
  begin
    stream.read(pal, bfh.bfOffset - 54);
    FBitmap^.SetPalette(@pal, 256, ptBGR8, 4);
  end;
  LoadHeader := true;
end;

//==============================================================================
//
// TBMPTextureManager.LoadRLE8
//
//==============================================================================
procedure TBMPTextureManager.LoadRLE8(Stream: TDStream);
var
  x, y: integer;
  n: word;
  count: byte;
  buffer: array[0..255] of byte;
  done: boolean;
begin
  y := bfi.Height - 1;
  x := 0;
  done := false;
  while not done do
  begin
    stream.read(n, 2);
    count := n shr 8;
    if n and 255 <> 0 then
    begin
      count := n and 255;
      Fillchar(Buffer, Count, n shr 8);
      FBitmap^.PutPixels(x, y, Count, @Buffer, 8);
      x := x + count;
    end
    else
    begin
      case count of
         0:
          begin
            y := y - 1;
            x := 0;
          end;
         1:
          begin
            done := true;
          end;
         2:
          begin
            stream.read(n, 2);
            inc(x, n and 255);
            dec(y, n shr 8);
          end;
          else
          begin
            stream.read(Buffer, (Count + 1) and $FFFE); { word align }
            FBitmap^.PutPixels(x, y, count, @Buffer, 8);
            x := x + count;
          end;
      end;
    end;
  end;
end;

//==============================================================================
//
// TBMPTextureManager.LoadImage
//
//==============================================================================
function TBMPTextureManager.LoadImage(stream: TDStream): boolean;
var
  y: integer;
  buffer: pointer;
  bufsize: integer;
begin
  bufsize := ((bfi.Width * bfi.BitCount div 8) + 3) and $FFFFFFFC;
  buffer := malloc(bufsize);
  LoadImage := true;
  if bfi.bitcount = 24 then
  begin
    for y := bfi.Height - 1 downto 0 do
    begin
      stream.read(buffer^, bufsize);
      FBitmap^.PutPixels(0, y, bfi.Width, buffer, 24); // is now 24
    end;
  end
  else if (bfi.Bitcount = 8) and (bfi.Compression = biRGB) then
  begin
    for y := bfi.Height - 1 downto 0 do
    begin
      stream.read(buffer^, bufsize);
      FBitmap^.PutPixels(0, y, bfi.Width, buffer, 8); // This is ok now.
                                                      // Since PutPixels takes
                                                      // the number of bits per
                                                      // pixel instead of bytes.
    end;
  end
  else if (bfi.Bitcount = 8) and (bfi.Compression = biRLE8) then
    LoadRLE8(Stream)
// By Matthias K"oppe <mkoeppe@cs.uni-magdeburg.de>:
  else if (bfi.Bitcount = 4) and (bfi.Compression = biRGB) then
  begin
    for y := bfi.Height - 1 downto 0 do
    begin
      stream.read(buffer^, bufsize);
      FBitmap^.PutPixels(0, y, bfi.Width, buffer, 4); // is now 4
   end;
  end
  // By Matthias K"oppe <mkoeppe@cs.uni-magdeburg.de>:
  else if bfi.Bitcount = 1 then
  begin
    for y := bfi.Height - 1 downto 0 do
    begin
      stream.read(buffer^, bufsize);
      FBitmap^.PutPixels(0, y, bfi.Width, buffer, 1); // is now 1
   end;
  end
  else
    LoadImage := false;
  memfree(buffer, bufsize);
end;

end.

