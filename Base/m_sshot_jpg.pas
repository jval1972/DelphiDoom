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
//  JPG Screenshot.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit m_sshot_jpg;

{$P+,S-,W-,R-,T-,X+,H+}
{$C PRELOAD}

interface

uses
  Windows, Forms, SysUtils, Classes, Graphics;

type
  TTGABitmap = class(TBitmap)
  private
    procedure WriteTGAStreamData(Stream: TStream);
    procedure ReadTGAStreamData(Stream: TStream);
  protected
    procedure WriteData(Stream: TStream); override;
    procedure ReadData(Stream: TStream); override;
  public
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
  end;

function TGAtoJPG(const TGApic, JPGpic: string; const Quality: integer = 100): boolean;

const
  rsErrUnsupported1 = 'Couldn''t load TGA Image. Only 24 and 32bit TGA Images supported.';
  rsErrUnsupported2 = 'Couldn''t load TGA Image. Colormapped TGA images not supported.';
  rsErrUnsupported3 = 'Couldn''t load TGA Image. Only standard 24, 32 bit TGA Images supported.';

var
  mirrorjpgsshot: Boolean = True;

implementation

uses
  jpeg;

{ TTGABitmap }

type
  TTGAHeader = packed record   // Header type for TGA images
    FileType     : Byte;
    ColorMapType : Byte;
    ImageType    : Byte;
    ColorMapSpec : array[0..4] of Byte;
    OrigX  : array [0..1] of Byte;
    OrigY  : array [0..1] of Byte;
    Width  : array [0..1] of Byte;
    Height : array [0..1] of Byte;
    BPP    : Byte;
    ImageInfo : Byte;
  end;

procedure TTGABitmap.WriteData(Stream: TStream);
begin
  WriteTGAStreamData(Stream);
end;

procedure TTGABitmap.SaveToStream(Stream: TStream);
begin
  WriteTGAStreamData(Stream);
end;

procedure TTGABitmap.LoadFromStream(Stream: TStream);
begin
  ReadTGAStreamData(Stream);
end;

procedure TTGABitmap.ReadData(Stream: TStream);
begin
  ReadTGAStreamData(Stream);
end;

procedure TTGABitmap.ReadTGAStreamData(Stream: TStream);
var
  aBitmap: TBitmap;
  TGAHeader: TTGAHeader;
  i: integer;
  P1: PByteArray;
begin
  Stream.Read(TGAHeader, SizeOf(TGAHeader));

  // Only support 24, 32 bit images
  if (TGAHeader.ImageType <> 2) and    { TGA_RGB }
     (TGAHeader.ImageType <> 10) then  { Compressed RGB }
  begin
    raise Exception.Create(rsErrUnsupported1);
    exit;
  end;

  if TGAHeader.ColorMapType <> 0 then
  begin
    raise Exception.Create(rsErrUnsupported2);
    exit;
  end;

  if not (TGAHeader.BPP in [24, 32]) then
  begin
    raise Exception.Create(rsErrUnsupported1);
    exit;
  end;

  if TGAHeader.ImageType <> 2 then   // Standard 24, 32 bit TGA file supported
  begin
    raise Exception.Create(rsErrUnsupported3);
    exit;
  end;

  aBitmap := TBitmap.Create;
  try
    aBitmap.Width  := TGAHeader.Width[0]  + TGAHeader.Width[1]  * 256;
    aBitmap.Height := TGAHeader.Height[0] + TGAHeader.Height[1] * 256;
    if TGAHeader.BPP = 24 then
      aBitmap.PixelFormat := pf24bit
    else
      aBitmap.PixelFormat := pf32bit;

    if TGAHeader.ImageInfo = 32 then
    begin
      for i := aBitmap.Height - 1 downto 0 do
      begin
        P1 := aBitmap.Scanline[aBitmap.Height - i - 1];

        if TGAHeader.BPP = 24 then
          Stream.Read(P1^, aBitmap.width * 3)
        else if TGAHeader.BPP = 32 then
          Stream.Read(P1^, aBitmap.width * 4)
      end;
    end
    else
    begin
      for i := aBitmap.Height - 1 downto 0 do
      begin
        P1 := aBitmap.Scanline[i];

        if TGAHeader.BPP = 24 then
          Stream.Read(P1^, aBitmap.width * 3)
        else if TGAHeader.BPP = 32 then
          Stream.Read(P1^, aBitmap.width * 4)
      end;
    end;
    Assign(aBitmap);
  finally
    aBitmap.Free;
  end;
end;

procedure TTGABitmap.WriteTGAStreamData(Stream: TStream);
var
  aBitmap: TBitmap;
  TGAHeader: TTGAHeader;
  i: integer;
  P1: PByteArray;
begin
  FillChar(TGAHeader, SizeOf(TGAHeader), Chr(0));
  TGAHeader.ImageType := 2;

  aBitmap := TBitmap.Create;
  try
    aBitmap.Assign(self);
    if not (aBitmap.PixelFormat in [pf24bit, pf32bit]) then
      aBitmap.PixelFormat := pf24bit;
    if aBitmap.PixelFormat = pf24bit then
      TGAHeader.BPP := 24
    else
      TGAHeader.BPP := 32;
    TGAHeader.Width[0] := byte(aBitmap.Width);
    TGAHeader.Width[1] := aBitmap.Width shr 8;
    TGAHeader.Height[0] := byte(aBitmap.Height);
    TGAHeader.Height[1] := aBitmap.Height shr 8;
    Stream.Write(TGAHeader, SizeOf(TGAHeader));

    for i := aBitmap.Height - 1 downto 0 do
    begin
      P1 := aBitmap.ScanLine[i];
      if TGAHeader.BPP = 24 then
        Stream.Write(P1^, aBitmap.Width * 3)
      else if TGAHeader.BPP = 32 then
        Stream.Write(P1^, aBitmap.Width * 4);
    end;

  finally
    aBitmap.Free;
  end;
end;

function TGAtoJPG(const TGApic, JPGpic: string; const Quality: integer = 100): boolean;
var
  Bitmap: TTGABitmap;
  JpegImg: TJpegImage;
  fs: TFileStream;
begin
  Bitmap := TTGABitmap.Create;
  try
    fs := TFileStream.Create(TGApic, fmOpenRead or fmShareDenyWrite);
    try
      Bitmap.LoadFromStream(fs);
    finally
      fs.Free;
    end;
    JpegImg := TJpegImage.Create;
    try
      JpegImg.Assign(Bitmap);
      JpegImg.CompressionQuality := Quality;
      JpegImg.SaveToFile(JPGpic);
      Result := True;
    finally
      JpegImg.Free
    end;
  finally
    Bitmap.Free
  end;
end;

initialization
  { Register the TTGABitmap as a new graphic file format
    now all the TPicture storage stuff can access our new
    TGA graphic format !
  }
  TPicture.RegisterFileFormat('TGA','Ttruevision Targa', TTGABitmap);

finalization
  TPicture.UnregisterGraphicClass(TTGABitmap);

end.

