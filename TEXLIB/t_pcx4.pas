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
//  PCX additional loader.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit t_pcx4;

interface

uses
   Windows,
   Classes,
   Graphics;

//==============================================================================
//
// PCXLoadFromFile
//
//==============================================================================
procedure PCXLoadFromFile(FileName: string; const bitmap: TBitmap);

implementation

type
  DataLineArray = array[0..65535] of Byte;
  DataWordArray = array[0..65535] of SmallInt;
  FakePalette = packed record
    LPal: TLogPalette;
    Dummy: array[1..255] of TPaletteEntry;
  end;

  TypeEgaPalette = array[0..16] of Byte;
  TypePalette = array[0..255,1..3] of Byte;

const
   Global_HiColor = 3;
   Global_KeepTrueFormat: Word = 2;

   Global_PaletteDef:array[0..15,1..3] of Byte = (
{Black}                                       (0  ,0  ,0 ),
{Blue}                                        (0  ,0  ,32),
{Green}                                       (0  ,32 ,0 ),
{Cyan}                                        (0  ,32 ,32),
{Red}                                         (32 ,0  ,0 ),
{Magenta}                                     (32 ,0  ,32),
{Brown}                                       (32 ,32 ,0 ),
{Light Gray}                                  (42 ,42 ,42),
{Dark Gray}                                   (21 ,21 ,21),
{Light Blue}                                  (0  ,0  ,63),
{Light Green}                                 (0  ,63 ,0 ),
{Light Cyan}                                  (0  ,63 ,63),
{Light Red}                                   (63 ,0  ,0 ),
{Light Magenta}                               (63 ,0  ,63),
{Yellow}                                      (63 ,63 ,0 ),
{Bright White}                                (63 ,63 ,63)
                                              );

var
  PictureFile: file;
  PaletteVGA: TypePalette;
  SysPal: FakePalette;
  TempArrayDBIg, TempArrayDBig16: ^DataLineArray;
  ErrorString: ShortString;
  Width: Word;
  Height: Word;
  BitsPerPixel: SmallInt;
  MyKeepTrueFormat: Boolean;
  MyKeepTrueBits: Word;

var
  PcxVersion: Word;
  PcxColorPlanes: Byte;
  PcxEncoding: Word;
  PcxBytesPerLine: Word;
  PcxPaletteType: Word;

var
  Index1: Word = 0;
  Index2: Word = 0;

const
  PCXBUFFERSIZE = 8 * 1024;

var
  IndexData: array[0..PCXBUFFERSIZE - 1] of Byte;

//==============================================================================
//
// FileGetMore
//
//==============================================================================
procedure FileGetMore;
var
  NumRead: Integer;
begin
  FillChar(IndexData, PCXBUFFERSIZE, 0);
  BlockRead(PictureFile, IndexData, PCXBUFFERSIZE, NumRead);
  Index1 := PCXBUFFERSIZE;
  Index2 := 0;
end;

//==============================================================================
//
// FastGetByte
//
//==============================================================================
function FastGetByte: Byte;
begin
  if Index1 = 0 then
    FileGetMore;
  FastGetByte := IndexData[Index2];
  Inc(Index2);
  Dec(Index1);
end;

//==============================================================================
//
// FastGetWord
//
//==============================================================================
function FastGetWord: Word;
begin
  FastGetWord := Word(FastGetByte) + Word(FastGetByte) * 256;
end;

//==============================================================================
//
// FileIoReset
//
//==============================================================================
procedure FileIoReset;
begin
  Index1 := 0;
  Index2 := 0;
end;

//==============================================================================
//
// OpenFile
//
//==============================================================================
procedure OpenFile(var FileName: string; var FileOk: Boolean);
var
  OldFileMode: Word;
begin
  FileIoReset;
  OldFileMode := FileMode;
  FileMode := 0;
  AssignFile(PictureFile, FileName);
  ReSet(PictureFile, 1);
  if IoResult <> 0 then
    FileOk := False;
  FileMode := OldFileMode;
end;

//==============================================================================
//
// FillerUp
//
//==============================================================================
procedure FillerUp(var TempArrayD; Size: Word; B1: Byte);
begin
  FillChar(TempArrayD, Size, B1);
end;

//==============================================================================
//
// SetUpMaskGrayPalette
//
//==============================================================================
procedure SetUpMaskGrayPalette;
var
  I, J: Integer;
begin
  for J := 0 to 255 do
  begin
    for I := 1 to 3 do
    begin
      PaletteVga[J, I] := J * 63 div 255;
    end;
  end;
end;

//==============================================================================
//
// PCXGrayValue
//
//==============================================================================
function PCXGrayValue(R, G, B: Word): Word;
begin
  Result := ((R shl 5) + (G shl 6) + (B * 12)) div 108;
end;

//==============================================================================
//
// MakePalBW
//
//==============================================================================
procedure MakePalBW(const bitmap: TBitmap);
begin
  SysPal.LPal.palVersion := $300;
  SysPal.LPal.palNumEntries := 2;
  Syspal.LPal.PalPalEntry[0].peRed := 0;
  Syspal.LPal.PalPalEntry[0].peGreen := 0;
  Syspal.LPal.PalPalEntry[0].peBlue := 0;
  Syspal.LPal.PalPalEntry[0].peFlags := 0;
  Syspal.Dummy[1].peRed := 255;
  Syspal.Dummy[1].peGreen := 255;
  Syspal.Dummy[1].peBlue := 255;
  Syspal.Dummy[1].peFlags := 0;
  Bitmap.Palette := CreatePalette(Syspal.LPal);
end;

//==============================================================================
//
// MakePalPalette
//
//==============================================================================
procedure MakePalPalette(const bitmap: TBitmap);
var
  I: Integer;
begin
  SysPal.LPal.palVersion := $300;
  SysPal.LPal.palNumEntries := 256;
  for I := 0 to 255 do
  begin
    Syspal.LPal.PalPalEntry[I].peRed := (PaletteVga[I, 1]) * 4;
    Syspal.LPal.PalPalEntry[I].peGreen := (PaletteVga[I, 2]) * 4;
    Syspal.LPal.PalPalEntry[I].peBlue := (PaletteVga[I, 3]) * 4;
    Syspal.LPal.PalPalEntry[I].peFlags := 0;
  end;
  Bitmap.Palette := CreatePalette(Syspal.LPal);
end;

//==============================================================================
//
// MakeGenPalette
//
//==============================================================================
procedure MakeGenPalette;
var
  X: Word;
  R, G, B: Word;
begin
  X := 0;
  for R := 0 to 7 do
  begin
    for G := 0 to 7 do
    begin
      for B := 0 to 3 do
      begin
        PaletteVga[X, 1] := (R + 1) * 8 - 1;
        PaletteVga[X, 2] := (G + 1) * 8 - 1;
        PaletteVga[X, 3] := (B + 1) * 16 - 1;
        Inc(X);
      end;
    end;
  end;
end;

//==============================================================================
//
// ShouldIKeepTrueFormat
//
//==============================================================================
function ShouldIKeepTrueFormat(var BPP: Word): Boolean;
begin
{
{ Choices
{    Use File Colors
{    Force 256 Colors
{    Force 16M Colors
}
  if Global_KeepTrueFormat = 0 then
    ShouldIKeepTrueFormat := True
  else
    ShouldIKeepTrueFormat := False;
  if Global_KeepTrueFormat = 1 then
    BPP := 8;
  if Global_KeepTrueFormat = 2 then
    BPP := 24;
end;

//==============================================================================
//
// DetColorVGA 
//
//==============================================================================
procedure DetColorVGA (var PValue: Byte; MapValue: Byte);
begin
  PValue := MapValue div 4;
end;

//==============================================================================
//
// PaletteDefaults
//
//==============================================================================
procedure PaletteDefaults;
var
  i, j: integer;
begin
  for j := 0 to 15 do
  begin
    for i := 1 to 3 do
      PaletteVGA[j, i] := Global_PaletteDef[j, i];
  end;
end;

//==============================================================================
//
// SetUpMaskAndColorMap
//
//==============================================================================
procedure SetUpMaskAndColorMap;
var
  R, G, B, PalBlue, PalGreen, PalRed: Byte;
  i: Integer;
  ColorMapSize: Integer;
begin
{
{ Handle black and white images
}
  ColorMapSize := 1 shl BitsPerPixel;
  if BitsPerPixel = 24 then
    SetUpMaskGrayPalette
  else
  begin
    for I := 0 to ColorMapSize - 1 do
    begin
      PalRed := FastGetbyte;
      PalGreen := FastGetbyte;
      PalBlue := FastGetbyte;
      if PcxVersion = 2 then
      begin
        if PalRed < 4 then
          PalRed := PalRed * $55;
        if PalGreen < 4 then
          PalGreen := PalGreen * $55;
        if PalBlue < 4 then
          PalBlue := PalBlue * $55;
      end;

      DetColorVGA(R, PalRed);
      DetColorVGA(G, PalGreen);
      DetColorVGA(B, PalBlue);

      PaletteVGA[I, 1] := R;
      PaletteVGA[I, 2] := G;
      PaletteVGA[I, 3] := B;
    end;
  end;
end;

{
============================================
}

//==============================================================================
//
// ReadPCXLine
//
//==============================================================================
procedure ReadPCXLine;
var
  N, MaximumN, Z: Word;
  I: SmallInt;
  TmpB1, B1, C, CurrentPlane: Byte;
  CX: SmallInt;
  RealWidth: Integer;
begin
  N := 0;
  Z := 0;
  CurrentPlane := 0;
  MaximumN := PCXBytesPerLine * PcxColorPlanes;
  RealWidth := PcxBytesPerLine * 8;
  repeat
    B1 := FastGetByte;
    if B1 and $C0 = $C0 then
    begin
      I := B1 and $3F;
      C := FastGetByte;
      while I > 0 do
      begin
        case BitsPerPixel of
          1:
            begin
              if MyKeepTrueFormat and (PcxColorPlanes = 1) then
              begin
                TempArrayDBIG[Z] := TempArrayDBIG[Z] + C;
                Inc(Z);
              end
              else
              begin
                {
                { 16 Color 4 planes or KEEP FORMAT=FALSE
                }
                for CX := 7 downto 0 do
                begin
                  if C and (1 shl CX) <> 0 then
                    TmpB1 := 1
                  else
                    TmpB1 := 0;
                  TmpB1 := TmpB1 shl CurrentPlane;
                  TempArrayDBIG[Z] := TempArrayDBIG[Z] + TmpB1;
                  Inc(Z);
                  if Z >= RealWidth then
                  begin
                    Z := 0;
                    Inc(CurrentPlane);
                  end;
                end;
              end;
            end;
          8:
            begin
              TempArrayDBIG[Z] := C;
              Inc(Z);
            end;
        end;
        Dec(I);
        Inc(N);
      end;
    end
    else
    begin
      case BitsPerPixel of
        1:
          begin
            if MyKeepTrueFormat and (PcxColorPlanes = 1) then
            begin
              TempArrayDBIG[Z] := TempArrayDBIG[Z] + B1;
              Inc(Z);
            end
            else
            begin
              for CX := 7 downto 0 do
              begin
                if B1 and (1 shl CX) <>0 then
                  TmpB1 := 1
                else
                  TmpB1 := 0;
                TmpB1 := TmpB1 shl CurrentPlane;
                TempArrayDBIG[Z] := TempArrayDBIG[Z] + TmpB1;
                Inc(Z);
                if Z >= RealWidth then
                begin
                  Z := 0;
                  Inc(CurrentPlane);
                end;
              end;
            end;
          end;
        8:
          begin
            TempArrayDBIG[Z] := B1;
            Inc(Z);
          end;
      end;
      Inc(N);
    end;
  until N >= MaximumN;
end;

//==============================================================================
//
// ReadPcxHeader
//
//==============================================================================
procedure ReadPcxHeader(var FileOk: Boolean; var ErrorString: ShortString);
var
  B1: Byte;
  B2, X: Word;
  TopOfs, LeftOfs: Word;
begin
  B1 := FastGetByte;
  if B1 <> 10 then
  begin
    ErrorString := 'Not a PCX file, or header read error.';
    FileOk := False;
    exit;
  end;
  PcxVersion := FastGetByte;
  PcxEncoding := FastGetByte;
  BitsPerPixel := FastGetByte;
  LeftOfs := FastGetWord;
  TopOfs := FastGetWord;
  Width := FastGetWord;
  Height := FastGetWord;
  Width := Width - LeftOfs + 1;
  Height := Height - TopOfs + 1;
  FastGetWord;
  FastGetWord;
  B2 := BitsPerPixel;
  BitsPerPixel := 4;
  SetupMaskAndColorMap;
  BitsPerPixel := B2;
  FastGetByte;
  PcxColorPlanes := FastGetByte;
  PcxBytesPerLine := FastGetWord;
  PcxPaletteType := FastGetWord;
  for X := 1 to 58 do
    FastGetByte;
  if not (BitsPerPixel in [1, 4, 8, 16, 24, 32]) then
  begin
    FileOk := False;
    ErrorString := 'Not a valid PCX file!';
  end;
end;

//==============================================================================
//
// PCXLoadFromFile
//
//==============================================================================
procedure PCXLoadFromFile;
var
  B1: Byte;
  I: SmallInt;
  NewWidth: Word;
  L1, L2: LongInt;
  PaletteOk: Boolean;
  FileOk: Boolean;
  Ptr1: Pointer;

  procedure UpDatePalette;
  var
    I: Integer;
  begin
    for I := 0 to 255 do
      Syspal.LPal.PalPalEntry[I].peflags := 0;
    case BitsPerPixel of
      1:
        begin
          if PcxColorPlanes=1 then
          begin
            if MyKeepTrueFormat then
              bitmap.PixelFormat := pf1bit
            else
            begin
              case MyKeepTrueBits of
                8:
                  bitmap.PixelFormat := pf8bit;
                24:
                  bitmap.PixelFormat := pf24bit;
              end;
            end;
            MakePalBW(bitmap);
          end
          else
          begin
            bitmap.IgnorePalette := False;
            SysPal.LPal.palVersion := $300;
            SysPal.LPal.palNumEntries := 17;
            for I := 0 to 16 do
            begin
              Syspal.LPal.PalPalEntry[I].peRed := (PaletteVga[I, 1] + 1) * 4 - 1;
              Syspal.LPal.PalPalEntry[I].peGreen := (PaletteVga[I, 2] + 1) * 4 - 1;
              Syspal.LPal.PalPalEntry[I].peBlue := (PaletteVga[I, 3] + 1) * 4 - 1;
            end;
            if MyKeepTrueFormat then
              bitmap.PixelFormat := pf8bit
            else
            begin
              case MyKeepTrueBits of
                8:
                  bitmap.PixelFormat := pf8bit;
                24:
                  bitmap.PixelFormat := pf24bit;
              end;
            end;
            Bitmap.Palette := CreatePalette(Syspal.LPal);
          end;
        end;
      8:
        begin
          if PcxColorPlanes = 1 then
          begin
            if MyKeepTrueFormat then
              bitmap.PixelFormat := pf8bit
            else
            begin
              case MyKeepTrueBits of
                8:
                  bitmap.PixelFormat := pf8bit;
                24:
                  bitmap.PixelFormat := pf24bit;
              end;
            end;
            MakePalPalette(bitmap);
          end
          else
          begin
            if MyKeepTrueFormat then
            begin
              bitmap.PixelFormat := pf24bit;
              MakeGenPalette;
            end
            else
            begin
              case MyKeepTrueBits of
                8:
                  begin
                    bitmap.PixelFormat := pf8bit;
                    SetUpMaskGrayPalette
                  end;
                24:
                  begin
                    bitmap.PixelFormat := pf24bit;
                    MakeGenPalette;
                  end;
              end;
              bitmap.IgnorePalette := True;
            end;
            MakePalPalette(bitmap);
          end;
        end;
    end;
  end;

  procedure Do8;
  var
    J: Integer;
  begin
    for J := 0 to Width - 1 do
    begin
      TempArrayDBIG^[J] := PCXGrayValue(
                            TempArrayDBIG^[J],
                            TempArrayDBIG^[PcxBytesPerLine + J],
                            TempArrayDBIG^[(PcxBytesPerLine shl 1) + J]);
    end;
  end;

  procedure Do24;
  var
    J, Z0, Z1, Z2, Z3: Integer;
  begin
    Z0 := 0;
    Z1 := 0;
    Z2 := PcxBytesPerLine;
    Z3 := Z2 + Z2;
    for J := 0 to Width - 1 do
    begin
      TempArrayDBIG16^[Z0 + 0] := TempArrayDBIG^[Z3];
      TempArrayDBIG16^[Z0 + 1] := TempArrayDBIG^[Z2];
      TempArrayDBIG16^[Z0 + 2] := TempArrayDBIG^[Z1];
      Z0 := Z0 + Global_HiColor;
      Inc(Z1);
      Inc(Z2);
      Inc(Z3);
    end;
    Move(TempArrayDBIG16^, TempArrayDBIG^, NewWidth);
  end;

  procedure Do8Adjust;
  begin
    Move(TempArrayDBIG^, Ptr1^, Width);
  end;

  procedure Do24Adjust;
  var
    X, Z: Integer;
    B1: Byte;
  begin
    Z := 0;
    for X := 0 to Width - 1 do
    begin
      B1 := TempArrayDBIG^[X];
      DataLineArray(Ptr1^)[Z + 0] := PaletteVGA[B1, 3] * 4 + 3;
      DataLineArray(Ptr1^)[Z + 1] := PaletteVGA[B1, 2] * 4 + 3;
      DataLineArray(Ptr1^)[Z + 2] := PaletteVGA[B1, 1] * 4 + 3;
      Z := Z + Global_HiColor;
    end;
  end;

begin
  MyKeepTrueFormat := ShouldIKeepTrueFormat(MyKeepTrueBits);
  ErrorString := '';
  FileOk := True;
  OpenFile(FileName, FileOk);
  ReadPcxHeader(FileOK, ErrorString);
  if FileOk then
  begin
    bitmap.Height := 1;
    bitmap.Width := 1;
    bitmap.Height := Height;
    bitmap.Width := Width;
    UpdatePalette;
   {
   { Check version number for FAKE palette!
   }
    NewWidth := Width * Global_HiColor;
    TempArrayDBIG := nil;
    TempArrayDBIG16 := nil;
    GetMem(TempArrayDBig, Width * 4 + 20{Slack Bytes});
    GetMem(TempArrayDBig16,NewWidth + 20);
    PaletteOk := True;
    if PcxVersion = 3 then
      PaletteDefaults;
    if (BitsPerPixel = 1) and (PcxColorPlanes = 1) then
    begin
      PaletteVGA[0, 1] := 0;
      PaletteVGA[0, 2] := 0;
      PaletteVGA[0, 3] := 0;
      PaletteVGA[1, 1] := 63;
      PaletteVGA[1, 2] := 63;
      PaletteVGA[1, 3] := 63;
    end;
    if (BitsPerPixel = 8) and (PcxColorPlanes = 1) then
    begin
      {
      { Fast PALETTE Read On Picture (Could be wrong!)
      }
      L1 := FilePos(PictureFile);
      if SizeOf(IndexData) > L1 then
         L1 := SizeOf(IndexData);
      L2 := L1 - Index1;
      Seek(PictureFile, FileSize(PictureFile));
      L1 := FilePos(PictureFile);
      L1 := L1 - (3 * 256 + 1);
      Seek(PictureFile, L1);
      FileIoReset;
   {
   { Reset GetByte Stuff!
   }
      B1 := FastGetByte;
      if B1 <> $0C then
         PaletteOk := False;
      SetupMaskAndColorMap;
      Seek(PictureFile, L2);
      FileIoReset;
    end;
    if (BitsPerPixel = 8) and (PcxColorPlanes = 3) then
      SetupMaskGrayPalette;
    I := 0;
    UpDatePalette;
    repeat
      if BitsPerPixel <> 8 then
      begin
        FillerUp(TempArrayDBIG^[0], Width * PcxColorPlanes + 20{Slack Bytes}, 0);
      end;
      ReadPCXLine;
      if (PCXColorPlanes = 3) and (BitsPerPixel = 8) then
      begin
       {
       { 24 Bit Image!
       }
        if MyKeepTrueFormat then
          Do24
        else
        begin
          case MyKeepTrueBits of
            8:
              Do8;
            24:
              Do24;
          end;
        end;
      end
      else
      begin
       {
       { 1,4 or 8 Bit file!
       }
      end;
    {
    { Put line into memory!
    }
      Ptr1 := bitmap.ScanLine[I];
      case BitsPerPixel of
        1:
          begin
            if MyKeepTrueFormat and (PcxColorPlanes = 1) then
            begin
              {
              { B&W Keep It
              }
              Move(TempArrayDBIG^, Ptr1^, PcxBytesPerLine * PcxColorPlanes)
            end
            else
            begin
              {
              { No KEEP or 16 Color
              }
              if MyKeepTrueFormat then
                Do8Adjust
              else
              begin
                case MyKeepTrueBits of
                  8:
                    Do8Adjust;
                  24:
                    Do24Adjust;
                end;
              end;
            end;
          end;
        8:
          begin
            if PcxColorPlanes = 1 then
            begin
              if MyKeepTrueFormat then
                Move(TempArrayDBIG^, Ptr1^, PcxBytesPerLine * PcxColorPlanes)
              else
              begin
                case MyKeepTrueBits of
                  8:
                    Move(TempArrayDBIG^, Ptr1^, PcxBytesPerLine);
                  24:
                    Do24Adjust;
                end;
              end;
            end
            else
            begin
              {
              { 24 bit file
              }
              if MyKeepTrueFormat then
                 Move(TempArrayDBIG^, Ptr1^, PcxBytesPerLine * PcxColorPlanes)
              else
              begin
                case MyKeepTrueBits of
                  8:
                    Move(TempArrayDBIG^, Ptr1^, Width);
                  24:
                    Move(TempArrayDBIG^, Ptr1^, PcxBytesPerLine * PcxColorPlanes);
                end;
              end;
            end;
          end;
      end;
      Inc(I);
    until I >= Height;
   {
   { Now read in REAL Palette!
   }
    if (BitsPerPixel = 8) and (PcxColorPlanes = 1) then
    begin
      if not PaletteOk then
      begin
        FastGetByte;
        SetupMaskAndColorMap;
      end;
    end;
    if (BitsPerPixel = 8) and (PcxColorPlanes = 3) then
      BitsPerPixel := 24;
    if (BitsPerPixel = 1) and (PcxColorPlanes = 4) then
      BitsPerPixel := 4;
    FreeMem(TempArrayDBig16, NewWidth + 20);
    FreeMem(TempArrayDBig, Width * 4 + 20);
    if IoResult <> 0 then ;
    CloseFile(PictureFile);
  end;
  if IoResult <> 0 then ;
end;

end.

