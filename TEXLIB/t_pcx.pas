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
//  PCX image format.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit t_pcx;

interface

uses
  d_delphi,
  t_main;

type
  TPCXTextureManager = object(TTextureManager)
  private
    ferrorstring: string;
  public
    constructor Create;
    function LoadHeader(stream: TDStream): boolean; virtual;
    function LoadImage(stream: TDStream): boolean; virtual;
    destructor Destroy; virtual;
  end;

//==============================================================================
//
// T_IsValidPCXHeader
//
//==============================================================================
function T_IsValidPCXHeader(const p: Pointer): boolean;

implementation

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  t_pcx4,
  m_misc,
  i_system,
  i_tmp;

///////////////////////////////////////////////////////////////////////
//                                                                   //
//                           TPCXImage                               //
//                           =========                               //
//                                                                   //
// Completed: The 10th of August 2001                                //
// Author:    M. de Haan                                             //
// Email:     M.deHaan@inn.nl                                        //
// Tested:    under W95 SP1, NT4 SP6, WIN2000                        //
// Version:   1.0                                                    //
//-------------------------------------------------------------------//
// Update:    The 14th of August 2001 to version 1.1.                //
// Reason:    Added version check.                                   //
//            Added comment info on version.                         //
//            Changed PCX header ID check.                           //
//-------------------------------------------------------------------//
// Update:    The 19th of August 2001 to version 2.0.                //
// Reason:    Warning from Delphi about using abstract methods,      //
//            caused by not implementing ALL TGraphic methods.       //
//            (Thanks goes to R.P. Sterkenburg for his diagnostic.)  //
//-------------------------------------------------------------------//
// Update:    The 13th of October 2001 to version 2.1.               //
// Reason:    strange errors, read errors, EExternalException, IDE   //
//            hanging, Delphi hanging, Debugger hanging, windows     //
//            hanging, keyboard locked, and so on.                   //
// Changed:   Assign procedure.                                      //
//-------------------------------------------------------------------//
// Update:    The 5th of April 2002 to version 2.2.                  //
// Changed:   RLE compressor routine.                                //
// Reason:    Incompatibility problems with other programs caused    //
//            by the RLE compressor.                                 //
//            Other programs encode: $C0 as: $C1 $C0.                //
//            ($C0 means: repeat the following byte 0 times          //
//            $C1 means: repeat the following byte 1 time.)          //
// Changed:   File read routine.                                     //
// Reason:    Now detects unsupported PCX data formats.              //
// Added:     'Unsupported data format' in exception handler.        //
// Added:     1 bit PCX support in reading.                          //
// Added:     Procedure Convert1BitPCXDataToImage.                   //
// Renamed:   Procedure ConvertPCXDataToImage to                     //
//            Convert24BitPCXDataToImage.                            //
//-------------------------------------------------------------------//
// Update:    The 14th of April 2002 to version 2.3.                 //
//            Now capable of reading and writing 1 and 24 bit PCX    //
//            images.                                                //
// Added:     1 bit PCX support in writing.                          //
// Added:     Procedure ConvertImageTo1bitPCXData.                   //
// Changed:   Procedure CreatePCXHeader.                             //
// Changed:   Procedure TPCXImage.SaveToFile.                        //
//-------------------------------------------------------------------//
// Update:    The 19th of April 2002 to version 2.4.                 //
//            Now capable of reading and writing: 1, 8 and 24 bit    //
//            PCX images.                                            //
// Added:     8 bit PCX support in reading and writing.              //
// Renamed:   Procedure ConvertImageTo1And8bitPCXData.               //
// Renamed:   Procedure Convert1And8bitPCXDataToImage.               //
// Changed:   Procedure fSetPalette, fGetPalette.                    //
//-------------------------------------------------------------------//
// Update:    The 7th of May 2002 to version 2.5.                    //
// Reason:    The palette of 8-bit PCX images couldn't be read in    //
//            the calling program.                                   //
// Changed:   Procedures Assign, AssignTo, fSetPalette, fGetPalette. //
// Tested:    All formats were tested with the following programs:   //
//            - import in Word 97,                                   //
//            * (Word ignores the palette of 1 bit PCX images!)      //
//            - import and export in MigroGrafX.                     //
//            * (MicroGrafX also ignores the palette of 1 bit PCX    //
//              images.)                                             //
//            No problems were detected.                             //
//                                                                   //
//===================================================================//
//                                                                   //
//         The PCX image file format is copyrighted by:              //
//           ZSoft, PC Paintbrush, PC Paintbrush plus                //
//                        Trademarks: N/A                            //
//                       Royalty fees: NONE                          //
//                                                                   //
//===================================================================//
//                                                                   //
// The author can not be held responsable for using this software    //
// in anyway.                                                        //
//                                                                   //
// The features and restrictions of this component are:              //
// ----------------------------------------------------              //
//                                                                   //
// The reading and writing (import / export) of files / images:      //
//     - PCX version 5 definition, PC Paintbrush 3 and higher,       //
//     - RLE-compressed,                                             //
//     - 1 and 8 bit PCX images WITH palette and                     //
//     - 24 bit PCX images without palette,                          //
//     are supported by this component.                              //
//                                                                   //
// Known issues                                                      //
// ------------                                                      //
//                                                                   //
// 1) GetEmpty is NOT tested.                                        //
//                                                                   //
// 2) 4 bit PCX images (with palette) are NOT (yet) implemented.     //
//    (I have no 4-bit PCX images to test it on...)                  //
//                                                                   //
///////////////////////////////////////////////////////////////////////

const
  WIDTH_OUT_OF_RANGE = 'Illegal width entry in PCX file header';
  HEIGHT_OUT_OF_RANGE = 'Illegal height entry in PCX file header';
  FILE_FORMAT_ERROR = 'Invalid file format';
  VERSION_ERROR = 'Only PC Paintbrush (plus) V3.0 and ' +
    'higher are supported';
  FORMAT_ERROR = 'Illegal identification byte in PCX file' +
    ' header';
  PALETTE_ERROR = 'Invalid palette signature found';
  ASSIGN_ERROR = 'Can only Assign a TBitmap or a TPicture';
  ASSIGNTO_ERROR = 'Can only AssignTo a TBitmap';
  PCXIMAGE_EMPTY = 'The PCX image is empty';
  BITMAP_EMPTY = 'The bitmap is empty';
  INPUT_FILE_TOO_LARGE = 'The input file is too large to be read';
  IMAGE_WIDTH_TOO_LARGE = 'Width of PCX image is too large to handle';
  // added 19/08/2001
  CLIPBOARD_LOAD_ERROR = 'Loading from clipboard failed';
  // added 19/08/2001
  CLIPBOARD_SAVE_ERROR = 'Saving to clipboard failed';
  // added 14/10/2001
  PCX_WIDTH_ERROR = 'Unexpected line length in PCX data';
  PCX_HEIGHT_ERROR = 'More PCX data found than expected';
  PCXIMAGE_TOO_LARGE = 'PCX image is too large';
  // added 5/4/2002
  ERROR_UNSUPPORTED = 'Unsupported PCX format';

const
  sPCXImageFile = 'PCX V3.0+ image';

  // added 19/08/2001
var
  CF_PCX: WORD;

  ///////////////////////////////////////////////////////////////////////
  //                                                                   //
  //                            PCXHeader                              //
  //                                                                   //
  ///////////////////////////////////////////////////////////////////////

type
  QWORD = Cardinal; // Seems more logical to me...

type
  fColorEntry = packed record
    ceRed: BYTE;
    ceGreen: BYTE;
    ceBlue: BYTE;
  end; // of packed record fColorEntry

type
  TPCXImageHeader = packed record
    fID: BYTE;
    fVersion: BYTE;
    fCompressed: BYTE;
    fBitsPerPixel: BYTE;
    fWindow: packed record
      wLeft,
      wTop,
      wRight,
      wBottom: WORD;
    end; // of packed record fWindow
    fHorzResolution: WORD;
    fVertResolution: WORD;
    fColorMap: array[0..15] of fColorEntry;
    fReserved: BYTE;
    fPlanes: BYTE;
    fBytesPerLine: WORD;
    fPaletteInfo: WORD;
    fFiller: array[0..57] of BYTE;
  end; // of packed record TPCXImageHeader
  PPCXImageHeader = ^TPCXImageHeader;

  ///////////////////////////////////////////////////////////////////////
  //                                                                   //
  //                             PCXData                               //
  //                                                                   //
  ///////////////////////////////////////////////////////////////////////

type
  TPCXData = object
    fData: array of BYTE;
  end; // of Type TPCXData

  ///////////////////////////////////////////////////////////////////////
  //                                                                   //
  //                             ScanLine                              //
  //                                                                   //
  ///////////////////////////////////////////////////////////////////////

const
  fMaxScanLineLength = $FFF; // Max image width: 4096 pixels

type
  mByteArray = array[0..fMaxScanLineLength] of BYTE;
  pmByteArray = ^mByteArray;

  // The "standard" pByteArray from Delphi allocates 32768 bytes,
  // which is a little bit overdone here, I think...

const
  fMaxImageWidth = $FFF; // Max image width: 4096 pixels

type
  xByteArray = array[0..fMaxImageWidth] of BYTE;

  ///////////////////////////////////////////////////////////////////////
  //                                                                   //
  //                          PCXPalette                               //
  //                                                                   //
  ///////////////////////////////////////////////////////////////////////

type
  TPCXPalette = packed record
    fSignature: BYTE;
    fPalette: array[0..255] of fColorEntry;
  end; // of packed record TPCXPalette

  ///////////////////////////////////////////////////////////////////////
  //                                                                   //
  //                             Classes                               //
  //                                                                   //
  ///////////////////////////////////////////////////////////////////////

type
  TPCXImage = class;
  TPCXFile = class;

  ///////////////////////////////////////////////////////////////////////
  //                                                                   //
  //                           PCXFile                                 //
  //                                                                   //
  //                         File handler                              //
  //                                                                   //
  ///////////////////////////////////////////////////////////////////////

  TPCXFile = class(TPersistent)
  private
    fHeight: Integer;
    fWidth: Integer;
    fPCXHeader: TPCXImageHeader;
    fPCXData: TPCXData;
    fPCXPalette: TPCXPalette;
    fColorDepth: QWORD;
    fPixelFormat: BYTE; // added 5/4/2002
    fCurrentPos: QWORD;
    fHasPalette: Boolean; // added 7/5/2002
  protected
    // Protected declarations
  public
    // Public declarations
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const Filename: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const Filename: string);
    procedure SaveToStream(Stream: TStream);
  published
    // Published declarations
    // The publishing is done in the TPCXImage section
  end;

  ///////////////////////////////////////////////////////////////////////
  //                                                                   //
  //                         TPCXImage                                 //
  //                                                                   //
  //                       Image handler                               //
  //                                                                   //
  ///////////////////////////////////////////////////////////////////////

  TPCXImage = class(TGraphic)
  private
    // Private declarations
    fBitmap: TBitmap;
    fPCXFile: TPCXFile;
    fRLine: xByteArray;
    fGLine: xByteArray;
    fBLine: xByteArray;
    fP: pmByteArray;
    fhPAL: HPALETTE;
    procedure fConvert24BitPCXDataToImage;
    procedure fConvert1And8BitPCXDataToImage;
    procedure fConvertImageTo24BitPCXData;
    procedure fConvertImageTo1And8BitPCXData(ImageWidthInBytes:
      QWORD);
    procedure fFillDataLines(const fLine: array of BYTE);
    procedure fCreatePCXHeader(const byBitsPerPixel: BYTE;
      const byPlanes: BYTE; const wBytesPerLine: DWORD);
    procedure fSetPalette(const wNumColors: WORD);
    procedure fGetPalette(const wNumColors: WORD);
    function fGetPixelFormat: TPixelFormat; // Added 07/05/2002
    function fGetBitmap: TBitmap; // Added 07/05/2002
  protected
    // Protected declarations
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
    function GetEmpty: Boolean; override;
  public
    // Public declarations
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure LoadFromFile(const Filename: string); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToFile(const Filename: string); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromClipboardFormat(AFormat: WORD;
      AData: THandle; APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: WORD;
      var AData: THandle; var APalette: HPALETTE); override;
  published
    // Published declarations
    property Height: Integer
      read GetHeight write SetHeight;
    property Width: Integer
      read GetWidth write SetWidth;
    property PixelFormat: TPixelFormat
      read fGetPixelFormat;
    property Bitmap: TBitmap
      read fGetBitmap; // Added 7/5/2002
  end;

//==============================================================================
// TPCXImage.Create
//
///////////////////////////////////////////////////////////////////////
//                                                                   //
//                           TPCXImage                               //
//                                                                   //
//                         Image handler                             //
//                                                                   //
///////////////////////////////////////////////////////////////////////
//
//==============================================================================
constructor TPCXImage.Create;
begin
  inherited Create;
  // Init HPALETTE
  fhPAL := 0;

  // Create a private bitmap to hold the image
  if not Assigned(fBitmap) then
    fBitmap := TBitmap.Create;

  // Create the PCXFile
  if not Assigned(fPCXFile) then
    fPCXFile := TPCXFile.Create;
end;

//==============================================================================
// TPCXImage.Destroy
//
//---------------------------------------------------------------------
//
//==============================================================================
destructor TPCXImage.Destroy;
begin
  // Reversed order of create
  // Free fPCXFile
  fPCXFile.Free;
  // Free private bitmap
  fBitmap.Free;
  // Delete palette
  if fhPAL <> 0 then
    DeleteObject(fhPAL);
  // Distroy all the other things
  inherited Destroy;
end;

//==============================================================================
// TPCXImage.SetHeight
//
//---------------------------------------------------------------------
//
//==============================================================================
procedure TPCXImage.SetHeight(Value: Integer);
begin
  if Value >= 0 then
    fBitmap.Height := Value;
end;

//==============================================================================
// TPCXImage.SetWidth
//
//---------------------------------------------------------------------
//
//==============================================================================
procedure TPCXImage.SetWidth(Value: Integer);
begin
  if Value >= 0 then
    fBitmap.Width := Value;
end;

//==============================================================================
// TPCXImage.GetHeight
//
//---------------------------------------------------------------------
//
//==============================================================================
function TPCXImage.GetHeight: Integer;
begin
  Result := fPCXFile.fHeight;
end;

//==============================================================================
// TPCXImage.GetWidth
//
//---------------------------------------------------------------------
//
//==============================================================================
function TPCXImage.GetWidth: Integer;

begin
  Result := fPCXFile.fWidth;
end;

//==============================================================================
// TPCXImage.fGetBitmap
//
//---------------------------------------------------------------------
//
//==============================================================================
function TPCXImage.fGetBitmap: TBitmap;
begin
  Result := fBitmap;
end;

//==============================================================================
// TPCXImage.LoadFromClipboardFormat
//
//-------------------------------------------------------------------//
// The credits for this procedure go to his work of TGIFImage by     //
// Reinier P. Sterkenburg                                            //
// Added 19/08/2001                                                  //
//-------------------------------------------------------------------//
// NOT TESTED!
//
//==============================================================================
procedure TPCXImage.LoadFromClipboardFormat(AFormat: WORD;
  ADAta: THandle; APalette: HPALETTE);
var
  Size: QWORD;
  Buf: Pointer;
  Stream: TMemoryStream;
  BMP: TBitmap;
begin
  if (AData = 0) then
    AData := GetClipBoardData(AFormat);
  if (AData <> 0) and (AFormat = CF_PCX) then
  begin
    Size := GlobalSize(AData);
    Buf := GlobalLock(AData);
    try
      Stream := TMemoryStream.Create;
      try
        Stream.SetSize(Size);
        Move(Buf^, Stream.Memory^, Size);
        Self.LoadFromStream(Stream);
      finally
        Stream.Free;
      end;
    finally

      GlobalUnlock(AData);
    end;
  end
  else if (AData <> 0) and (AFormat = CF_BITMAP) then
  begin
    BMP := TBitmap.Create;
    try
      BMP.LoadFromClipboardFormat(AFormat, AData, APalette);
      Self.Assign(BMP);
    finally
      BMP.Free;
    end;
  end
  else
    raise Exception.Create(CLIPBOARD_LOAD_ERROR);
end;

//==============================================================================
// TPCXImage.SaveToClipboardFormat
//
//-------------------------------------------------------------------//
// The credits for this procedure go to his work of TGIFImage by     //
// Reinier P. Sterkenburg                                            //
// Added 19/08/2001                                                  //
//-------------------------------------------------------------------//
// NOT TESTED!
//
//==============================================================================
procedure TPCXImage.SaveToClipboardFormat(var AFormat: WORD;
  var AData: THandle; var APalette: HPALETTE);
var
  Stream: TMemoryStream;
  Data: THandle;
  Buf: Pointer;
begin
  if Empty then
    Exit;
  // First store the bitmap to the clipboard
  fBitmap.SaveToClipboardFormat(AFormat, AData, APalette);
  // Then try to save the PCX
  Stream := TMemoryStream.Create;
  try
    SaveToStream(Stream);
    Stream.Position := 0;
    Data := GlobalAlloc(2, Stream.Size);
    try
      if Data <> 0 then
      begin
        Buf := GlobalLock(Data);
        try
          Move(Stream.Memory^, Buf^, Stream.Size);
        finally
          GlobalUnlock(Data);
        end;
        if SetClipBoardData(CF_PCX, Data) = 0 then
          raise Exception.Create(CLIPBOARD_SAVE_ERROR);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    Stream.Free;
  end;
end;

//==============================================================================
// TPCXImage.GetEmpty
//
//-------------------------------------------------------------------//
// NOT TESTED!
//
//==============================================================================
function TPCXImage.GetEmpty: Boolean; // Added 19/08/2002
begin
  if Assigned(fBitmap) then
    Result := fBitmap.Empty
  else
    Result := (fPCXFile.fHeight = 0) or (fPCXFile.fWidth = 0);
end;

//==============================================================================
// TPCXImage.SaveToFile
//
//---------------------------------------------------------------------
//
//==============================================================================
procedure TPCXImage.SaveToFile(const Filename: string);
var
  fPCX: TFileStream;
  W, WW: QWORD;
begin
  if (fBitmap.Width = 0) or (fBitmap.Height = 0) then
    raise Exception.Create(BITMAP_EMPTY);
  W := fBitmap.Width;
  WW := W div 8;
  if (W mod 8) > 0 then
    Inc(WW);
  case fBitmap.PixelFormat of
    pf1bit:
      begin
        // Fully supported by PCX and by this component
        fCreatePCXHeader(1, 1, WW);
        fConvertImageTo1And8BitPCXData(WW);
        fGetPalette(2);
      end;
    pf4bit:
      begin
        // I don't have 4-bit PCX images to test with
        // It will be treated as a 24 bit image
        fCreatePCXHeader(8, 3, W);
        fConvertImageTo24BitPCXData;
      end;
    pf8bit:
      begin
        // Fully supported by PCX and by this component
        fCreatePCXHeader(8, 1, W);
        fConvertImageTo1And8BitPCXData(W);
        fGetPalette(256);
      end;
    pf15bit:
      begin
        // Is this supported in PCX?
        // It will be treated as a 24 bit image
        fCreatePCXHeader(8, 3, W);
        fConvertImageTo24BitPCXData;
      end;
    pf16bit:
      begin
        // Is this supported in PCX?
        // It will be treated as a 24 bit image
        fCreatePCXHeader(8, 3, W);
        fConvertImageTo24BitPCXData;
      end;
    pf24bit:
      begin
        // Fully supported by PCX and by this component
        fCreatePCXHeader(8, 3, W);
        fConvertImageTo24BitPCXData;
      end;
    pf32bit:
      begin
        // Not supported by PCX
        fCreatePCXHeader(8, 3, W);
        fConvertImageTo24BitPCXData;
      end;
  else
    begin
      fCreatePCXHeader(8, 3, W);
      fConvertImageTo24BitPCXData;
    end; // of else
  end; // of Case
  fPCX := TFileStream.Create(Filename, fmCreate);
  try
    fPCX.Position := 0;
    SaveToStream(fPCX);
  finally
    fPCX.Free;
  end; // of finally
  SetLength(fPCXFile.fPCXData.fData, 0);
end; // of Procedure SaveToFile

//==============================================================================
// TPCXImage.AssignTo
//
//-------------------------------------------------------------------//
//
//==============================================================================
procedure TPCXImage.AssignTo(Dest: TPersistent);
var
  bAssignToError: Boolean;
begin
  bAssignToError := True;

  if Dest is TBitmap then
  begin
    // The old AssignTo procedure was like this.
    // But then the palette was couldn't be accessed in the calling
    // program for some reason.
    // --------------------------
    // (Dest as TBitmap).Assign(fBitmap);
    // If fBitmap.Palette <> 0 then
    //    (Dest as TBitmap).Palette := CopyPalette(fBitmap.Palette);
    // --------------------------

    // Do the assigning
    (Dest as TBitmap).Assign(fBitmap);

    if fPCXFile.fHasPalette then
      (Dest as TBitmap).Palette := CopyPalette(fhPAL);
    // Now the calling program can access the palette
    // (if it has one)!
    bAssignToError := False;
  end;

  if Dest is TPicture then
  begin
    (Dest as TPicture).Graphic.Assign(fBitmap);
    bAssignToError := False;
  end;

  if bAssignToError then
    raise Exception.Create(ASSIGNTO_ERROR);

  // You can write other assignments here, if you want...
end;

//==============================================================================
// TPCXImage.Assign
//
//-------------------------------------------------------------------//
//
//==============================================================================
procedure TPCXImage.Assign(Source: TPersistent);
var
  iX, iY: DWORD;
  bAssignError: Boolean;
begin
  bAssignError := True;

  if (Source is TBitmap) then
  begin
    fBitmap.Assign(Source as TBitmap);
    if (Source as TBitmap).Palette <> 0 then
    begin
      fhPAL := CopyPalette((Source as TBitmap).Palette);
      fBitmap.Palette := fhPAL;
    end;
    bAssignError := False;
  end;

  if (Source is TPicture) then
  begin
    iX := (Source as TPicture).Width;
    iY := (Source as TPicture).Height;
    fBitmap.Width := iX;
    fBitmap.Height := iY;
    fBitmap.Canvas.Draw(0, 0, (Source as TPicture).Graphic);
    bAssignError := False;
  end;

  // You can write other assignments here, if you want...

  if bAssignError then
    raise Exception.Create(ASSIGN_ERROR);
end;

//==============================================================================
// TPCXImage.Draw
//
//---------------------------------------------------------------------
//
//==============================================================================
procedure TPCXImage.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  // Faster
  // ACanvas.Draw(0,0,fBitmap);

  // Slower
  ACanvas.StretchDraw(Rect, fBitmap);
end;

//==============================================================================
// TPCXImage.LoadFromFile
//
//---------------------------------------------------------------------
//
//==============================================================================
procedure TPCXImage.LoadFromFile(const Filename: string);
begin
  fPCXFile.LoadFromFile(Filename);
  // added 5/4/2002
  case fPCXFile.fPixelFormat of
    1: fConvert1And8BitPCXDataToImage;
    8: fConvert1And8BitPCXDataToImage;
    24: fConvert24BitPCXDataToImage;
  end;
end;

//==============================================================================
// TPCXImage.SaveToStream
//
//---------------------------------------------------------------------
//
//==============================================================================
procedure TPCXImage.SaveToStream(Stream: TStream);

begin
  fPCXFile.SaveToStream(Stream);
end;

//==============================================================================
// TPCXImage.LoadFromStream
//
//---------------------------------------------------------------------
//
//==============================================================================
procedure TPCXImage.LoadFromStream(Stream: TStream);
begin
  fPCXFile.LoadFromStream(Stream);
end;

//==============================================================================
// TPCXImage.fFillDataLines
//
///////////////////////////////////////////////////////////////////////
//                                                                   //
//                       Called by RLE compressor                    //
//                                                                   //
///////////////////////////////////////////////////////////////////////
//
//==============================================================================
procedure TPCXImage.fFillDataLines(const fLine: array of BYTE);
var
  By: BYTE;
  Cnt: WORD;
  I: QWORD;
  W: QWORD;
begin
  I := 0;
  By := fLine[0];
  Cnt := $C1;
  W := fBitmap.Width;

  repeat

    Inc(I);

    if By = fLine[I] then
    begin
      Inc(Cnt);
      if Cnt = $100 then
      begin
        fPCXFile.fPCXData.fData[fPCXFile.fCurrentPos] :=
          BYTE(Pred(Cnt));
        Inc(fPCXFile.fCurrentPos);
        fPCXFile.fPCXData.fData[fPCXFile.fCurrentPos] := By;
        Inc(fPCXFile.fCurrentPos);
        Cnt := $C1;
        By := fLine[I];
      end;
    end;

    if (By <> fLine[I]) then
    begin
      if (Cnt = $C1) then
      begin
        // If (By < $C1) then
        if (By < $C0) then // changed 5/4/2002
        begin
          fPCXFile.fPCXData.fData[fPCXFile.fCurrentPos] := By;
          Inc(fPCXFile.fCurrentPos);
        end
        else
        begin
          fPCXFile.fPCXData.fData[fPCXFile.fCurrentPos] := BYTE(Cnt);
          Inc(fPCXFile.fCurrentPos);
          fPCXFile.fPCXData.fData[fPCXFile.fCurrentPos] := By;
          Inc(fPCXFile.fCurrentPos);
        end;
      end
      else
      begin
        fPCXFile.fPCXData.fData[fPCXFile.fCurrentPos] := BYTE(Cnt);
        Inc(fPCXFile.fCurrentPos);
        fPCXFile.fPCXData.fData[fPCXFile.fCurrentPos] := By;
        Inc(fPCXFile.fCurrentPos);
      end;

      Cnt := $C1;
      By := fLine[I];
    end;

  until I = W - 1;

  // Write the last byte(s)
  if (Cnt > $C1) then
  begin
    fPCXFile.fPCXData.fData[fPCXFile.fCurrentPos] := BYTE(Cnt);
    Inc(fPCXFile.fCurrentPos);
  end;

  if (Cnt = $C1) and (By > $C0) then
  begin
    fPCXFile.fPCXData.fData[fPCXFile.fCurrentPos] := BYTE(Cnt);
    Inc(fPCXFile.fCurrentPos);
  end;

  fPCXFile.fPCXData.fData[fPCXFile.fCurrentPos] := By;
  Inc(fPCXFile.fCurrentPos);
end;

//==============================================================================
// TPCXImage.fConvertImageTo24BitPCXData
//
//-------------------------------------------------------------------//
//                  RLE Compression algorithm                        //
//-------------------------------------------------------------------//
//
//==============================================================================
procedure TPCXImage.fConvertImageTo24BitPCXData; // Renamed 5/4/2002
var
  H, W: QWORD;
  X, Y: QWORD;
  I: QWORD;
begin
  H := fBitmap.Height;
  W := fBitmap.Width;
  fPCXFile.fCurrentPos := 0;
  SetLength(fPCXFile.fPCXData.fData, 6 * H * W); // To be sure...
  fBitmap.PixelFormat := pf24bit; // Always do this if you're using
  // ScanLine!

  for Y := 0 to H - 1 do
  begin
    fP := fBitmap.ScanLine[Y];
    I := 0;
    for X := 0 to W - 1 do
    begin
      fRLine[X] := fP[I];
      Inc(I); // Extract a red line
      fGLine[X] := fP[I];
      Inc(I); // Extract a green line
      fBLine[X] := fP[I];
      Inc(I); // Extract a blue line
    end;

    fFillDataLines(fBLine); // Compress the blue line
    fFillDataLines(fGLine); // Compress the green line
    fFillDataLines(fRLine); // Compress the red line

  end;

  // Correct the length of fPCXData.fData
  SetLength(fPCXFile.fPCXData.fData, fPCXFile.fCurrentPos);
end;
//-------------------------------------------------------------------//

procedure TPCXImage.fConvertImageTo1And8BitPCXData(ImageWidthInBytes:
  QWORD);
var
  H, W, X, Y: QWORD;
  oldByte, newByte: BYTE;
  Cnt: BYTE;
begin
  H := fBitmap.Height;
  W := ImageWidthInBytes;
  fPCXFile.fCurrentPos := 0;
  SetLength(fPCXFile.fPCXData.fData, 2 * H * W); // To be sure...
  oldByte := 0; // Otherwise the compiler issues a warning about
  // oldByte not being initialized...
  Cnt := $C1;
  for Y := 0 to H - 1 do
  begin
    fP := fBitmap.ScanLine[Y];
    for X := 0 to W - 1 do
    begin

      newByte := fP[X];

      if X > 0 then
      begin
        if (Cnt = $FF) then
        begin
          fPCXFile.fPCXData.fData[fPCXFile.fCurrentPos] := Cnt;
          Inc(fPCXFile.fCurrentPos);
          fPCXFile.fPCXData.fData[fPCXFile.fCurrentPos] := oldByte;
          Inc(fPCXFile.fCurrentPos);
          Cnt := $C1;
        end
        else if newByte = oldByte then
          Inc(Cnt);

        if newByte <> oldByte then
        begin
          if (Cnt > $C1) or (oldByte >= $C0) then
          begin
            fPCXFile.fPCXData.fData[fPCXFile.fCurrentPos] := Cnt;
            Inc(fPCXFile.fCurrentPos);
            Cnt := $C1;
          end;
          fPCXFile.fPCXData.fData[fPCXFile.fCurrentPos] := oldByte;
          Inc(fPCXFile.fCurrentPos);
        end;

      end;
      oldByte := newByte;
    end;
    // Write last byte of line
    if (Cnt > $C1) or (oldByte >= $C0) then
    begin
      fPCXFile.fPCXData.fData[fPCXFile.fCurrentPos] := Cnt;
      Inc(fPCXFile.fCurrentPos);
      Cnt := $C1;
    end;

    fPCXFile.fPCXData.fData[fPCXFile.fCurrentPos] := oldByte;
    Inc(fPCXFile.fCurrentPos);
  end;

  // Write last byte of image
  if (Cnt > $C1) or (oldByte >= $C0) then
  begin
    fPCXFile.fPCXData.fData[fPCXFile.fCurrentPos] := Cnt;
    Inc(fPCXFile.fCurrentPos);
    // Cnt := 1;
  end;
  fPCXFile.fPCXData.fData[fPCXFile.fCurrentPos] := oldByte;
  Inc(fPCXFile.fCurrentPos);

  // Correct the length of fPCXData.fData
  SetLength(fPCXFile.fPCXData.fData, fPCXFile.fCurrentPos);
end;

//==============================================================================
// TPCXImage.fConvert24BitPCXDataToImage
//
//-------------------------------------------------------------------//
//                  RLE Decompression algorithm                      //
//-------------------------------------------------------------------//
//
//==============================================================================
procedure TPCXImage.fConvert24BitPCXDataToImage; // Renamed 5/4/2002
var
  I: QWORD;
  By: BYTE;
  Cnt: BYTE;
  H, W: QWORD;
  X, Y: QWORD;
  K, L: QWORD;
begin
  H := fPCXFile.fPCXHeader.fWindow.wBottom -
    fPCXFile.fPCXHeader.fWindow.wTop + 1;
  W := fPCXFile.fPCXHeader.fWindow.wRight -
    fPCXFile.fPCXHeader.fWindow.wLeft + 1;
  Y := 0; // First line of image
  fBitmap.Width := W; // Set bitmap width
  fBitmap.Height := H; // Set bitmap height
  fBitmap.PixelFormat := pf24bit; // Always do this if you're using
  // ScanLine!
  I := 0; // Pointer to data byte of fPXCFile
  repeat

    // Process the red line
    // ProcessLine(fRLine,W);

    X := 0; // Pointer to position in Red / Green / Blue line
    repeat
      By := fPCXFile.fPCXData.fData[I];
      Inc(I);

      // one byte
      if By < $C1 then
        if X <= W then // added 5/4/2002
        begin
          fRLine[X] := By;
          Inc(X);
        end;

      // multiple bytes (RLE)
      if By > $C0 then
      begin
        Cnt := By and $3F;

        By := fPCXFile.fPCXData.fData[I];
        Inc(I);

        for K := 1 to Cnt do
          if X <= W then // added 5/4/2002
          begin
            fRLine[X] := By;
            Inc(X);
          end;

      end;

    until X >= W;

    // Process the green line
    // ProcessLine(fGLine,W);

    X := 0;
    repeat
      By := fPCXFile.fPCXData.fData[I];
      Inc(I);

      // one byte
      if By < $C1 then
        if X <= W then // added 5/4/2002
        begin
          fGLine[X] := By;
          Inc(X);
        end;

      // multiple bytes (RLE)
      if By > $C0 then
      begin
        Cnt := By and $3F;

        By := fPCXFile.fPCXData.fData[I];
        Inc(I);

        for K := 1 to Cnt do
          if X <= W then // added 5/4/2002
          begin
            fGLine[X] := By;
            Inc(X);
          end;

      end;

    until X >= W;

    // Process the blue line
    // ProcessLine(fBLine,W);

    X := 0;
    repeat
      By := fPCXFile.fPCXData.fData[I];
      Inc(I);

      // one byte
      if By < $C1 then
        if X <= W then // added 5/4/2002
        begin
          fBLine[X] := By;
          Inc(X);
        end;

      // multiple bytes (RLE)
      if By > $C0 then
      begin
        Cnt := By and $3F;

        By := fPCXFile.fPCXData.fData[I];
        Inc(I);

        for K := 1 to Cnt do
          if X <= W then // added 5/4/2002
          begin
            fBLine[X] := By;
            Inc(X);
          end;

      end;

    until X >= W;

    // Write the just processed data RGB lines to the bitmap
    fP := fBitmap.ScanLine[Y];
    L := 0;
    for X := 0 to W - 1 do
    begin
      fP[L] := fBLine[X];
      Inc(L);
      fP[L] := fGLine[X];
      Inc(L);
      fP[L] := fRLine[X];
      Inc(L);
    end;

    Inc(Y); // Process the next RGB line

  until Y >= H;

  SetLength(fPCXFile.fPCXData.fData, 0);
end;

//==============================================================================
// TPCXImage.fConvert1And8BitPCXDataToImage
//
//-------------------------------------------------------------------//
//
//==============================================================================
procedure TPCXImage.fConvert1And8BitPCXDataToImage; // added 5/4/2002
var
  I, J: QWORD;
  By: BYTE;
  Cnt: BYTE;
  H, W, WW: QWORD;
  X, Y: QWORD;
begin
  H := fPCXFile.fPCXHeader.fWindow.wBottom -
    fPCXFile.fPCXHeader.fWindow.wTop + 1;
  W := fPCXFile.fPCXHeader.fWindow.wRight -
    fPCXFile.fPCXHeader.fWindow.wLeft + 1;
  fBitmap.Width := W; // Set bitmap width
  fBitmap.Height := H; // Set bitmap height
  WW := W;

  // 1 bit PCX
  if fPCXFile.fPixelFormat = 1 then
  begin
    // All 1 bit images have a palette
    fBitmap.PixelFormat := pf1bit; // Always do this if you're using
    // ScanLine!
    WW := W div 8; // Correct width for pf1bit
    if W mod 8 > 0 then
    begin
      Inc(WW);
      fBitMap.Width := WW * 8;
    end;
    fSetPalette(2);
  end;

  // 8 bit PCX
  if fPCXFile.fPixelFormat = 8 then
  begin
    // All 8 bit images have a palette!
    // This is how to set the palette of a bitmap
    // 1. First set the bitmap to pf8bit;
    // 2. then set the palette of the bitmap;
    // 3. then set the pixels with ScanLine or with Draw.
    // If you do it with StretchDraw, it won't work. Don't ask me why.
    // If you don't do it in this order, it won't work either! You'll
    // get strange colors.
    fBitmap.PixelFormat := pf8bit; // Always do this if you're using
    // ScanLine!
    fSetPalette(256);
  end;

  I := 0;
  Y := 0;
  repeat
    fP := fBitmap.ScanLine[Y];
    X := 0; // Pointer to position in line
    repeat
      By := fPCXFile.fPCXData.fData[I];
      Inc(I);

      // one byte
      if By < $C1 then
        if X <= WW then
        begin
          fP[X] := By;
          Inc(X);
        end;

      // multiple bytes (RLE)
      if By > $C0 then
      begin
        Cnt := By and $3F;

        By := fPCXFile.fPCXData.fData[I];
        Inc(I);

        for J := 1 to Cnt do
          if X <= WW then
          begin
            fP[X] := By;
            Inc(X);
          end;

      end;

    until X >= WW;

    Inc(Y); // Next line

  until Y >= H;
end;

//==============================================================================
// TPCXImage.fCreatePCXHeader
//
//---------------------------------------------------------------------
//
//==============================================================================
procedure TPCXImage.fCreatePCXHeader(const byBitsPerPixel: BYTE;
  const byPlanes: BYTE; const wBytesPerLine: DWORD);
var
  H, W: WORD;
begin
  W := fBitmap.Width;
  H := fBitmap.Height;

  // PCX header
  fPCXFile.fPCXHeader.fID := BYTE($0A); // BYTE (1)
  fPCXFile.fPCXHeader.fVersion := BYTE(5); // BYTE (2)
  fPCXFile.fPCXHeader.fCompressed := BYTE(1); // BYTE (3)
  // 0 = uncompressed, 1 = compressed
  // Only RLE compressed files are supported by this component
  fPCXFile.fPCXHeader.fBitsPerPixel := BYTE(byBitsPerPixel);
  // BYTE (4)
  fPCXFile.fPCXHeader.fWindow.wLeft := WORD(0); // WORD (5,6)
  fPCXFile.fPCXHeader.fWindow.wTop := WORD(0); // WORD (7,8)
  fPCXFile.fPCXHeader.fWindow.wRight := WORD(W - 1); // WORD (9,10)
  fPCXFile.fPCXHeader.fWindow.wBottom := WORD(H - 1); // WORD (11,12)
  fPCXFile.fPCXHeader.fHorzResolution := WORD(72); // WORD (13,14)
  fPCXFile.fPCXHeader.fVertResolution := WORD(72); // WORD (15,16)

  FillChar(fPCXFile.fPCXHeader.fColorMap, 48, 0); // Array of Byte
  // (17..64)

  fPCXFile.fPCXHeader.fReserved := BYTE(0); // BYTE (65)
  fPCXFile.fPCXHeader.fPlanes := BYTE(byPlanes);
  // BYTE (66)
  fPCXFile.fPCXHeader.fBytesPerLine := WORD(wBytesPerLine);
  // WORD (67,68)
  // must be even
  // rounded above
  fPCXFile.fPCXHeader.fPaletteInfo := WORD(1); // WORD (69,70)

  FillChar(fPCXFile.fPCXHeader.fFiller, 58, 0); // Array of Byte
  // (71..128)

  fPCXFile.fPixelFormat := fPCXFile.fPCXHeader.fPlanes *
    fPCXFile.fPCXHeader.fBitsPerPixel;
  fPCXFile.fColorDepth := 1 shl fPCXFile.fPixelFormat;
end;

//==============================================================================
// TPCXImage.fSetPalette
//
//---------------------------------------------------------------------
//
//==============================================================================
procedure TPCXImage.fSetPalette(const wNumColors: WORD);
var
  pal: TMaxLogPalette;
  W: WORD;
begin
  pal.palVersion := $300; // The "Magic" number
  pal.palNumEntries := wNumColors;
  for W := 0 to 255 do
  begin
    pal.palPalEntry[W].peRed :=
      fPCXFile.fPCXPalette.fPalette[W].ceRed;
    pal.palPalEntry[W].peGreen :=
      fPCXFile.fPCXPalette.fPalette[W].ceGreen;
    pal.palPalEntry[W].peBlue :=
      fPCXFile.fPCXPalette.fPalette[W].ceBlue;
    pal.palPalEntry[W].peFlags := 0;
  end;

  (* Must we delete the old palette first here? I don't know.
  If fhPAL <> 0 then
     DeleteObject(fhPAL);
  *)

  fhPAL := CreatePalette(PLogPalette(@pal)^);
  if fhPAL <> 0 then
    fBitmap.Palette := fhPAL;
end;

//==============================================================================
// TPCXImage.fGetPixelFormat
//
//---------------------------------------------------------------------
//
//==============================================================================
function TPCXImage.fGetPixelFormat: TPixelFormat;

// Only pf1bit, pf4bit and pf8bit images have a palette.
// pf15bit, pf16bit, pf24bit and pf32bit images have no palette.
// You can change the palette of pf1bit images in windows.
// The foreground color and the background color of pf1bit images
// do not have to be black and white. You can choose any tow colors.
// The palette of pf4bit images is fixed.
// The palette entries 0..9 and 240..255 of pf8bit images are reserved
// in windows.
begin
  Result := pfDevice;
  case fPCXFile.fPixelFormat of
    01: Result := pf1bit; // Implemented WITH palette.
    // 04 : Result :=  pf4bit; // Not yet implemented in this component,
                               // is however implemented in PCX format.
    08: Result := pf8bit; // Implemented WITH palette.
    // 15 : Result := pf15bit; // Not implemented in PCX format?
    // 16 : Result := pf16bit; // Not implemented in PCX format?
    24: Result := pf24bit; // Implemented, has no palette.
    // 32 : Result := pf32bit; // Not implemented in PCX format.
  end;
end;

//==============================================================================
// TPCXImage.fGetPalette
//
//---------------------------------------------------------------------
//
//==============================================================================
procedure TPCXImage.fGetPalette(const wNumColors: WORD);

var
  pal: TMaxLogPalette;
  W: WORD;

begin
  fPCXFile.fPCXPalette.fSignature := $0C;

  pal.palVersion := $300; // The "Magic" number
  pal.palNumEntries := wNumColors;
  GetPaletteEntries(CopyPalette(fBitmap.Palette), 0, wNumColors,
    pal.palPalEntry);
  for W := 0 to 255 do
    if W < wNumColors then
    begin
      fPCXFile.fPCXPalette.fPalette[W].ceRed :=
        pal.palPalEntry[W].peRed;
      fPCXFile.fPCXPalette.fPalette[W].ceGreen :=
        pal.palPalEntry[W].peGreen;
      fPCXFile.fPCXPalette.fPalette[W].ceBlue :=
        pal.palPalEntry[W].peBlue;
    end
    else
    begin
      fPCXFile.fPCXPalette.fPalette[W].ceRed := 0;
      fPCXFile.fPCXPalette.fPalette[W].ceGreen := 0;
      fPCXFile.fPCXPalette.fPalette[W].ceBlue := 0;
    end;
end;
//=====================================================================

//==============================================================================
// TPCXFile.Create
//
///////////////////////////////////////////////////////////////////////
//                                                                   //
//                         TPCXFile                                  //
//                                                                   //
///////////////////////////////////////////////////////////////////////
//
//==============================================================================
constructor TPCXFile.Create;
begin
  inherited Create;
  fHeight := 0;
  fWidth := 0;
  fCurrentPos := 0;
end;

//==============================================================================
// TPCXFile.Destroy
//
//---------------------------------------------------------------------
//
//==============================================================================
destructor TPCXFile.Destroy;
begin
  SetLength(fPCXData.fData, 0);
  inherited Destroy;
end;

//==============================================================================
// TPCXFile.LoadFromFile
//
//---------------------------------------------------------------------
//
//==============================================================================
procedure TPCXFile.LoadFromFile(const Filename: string);
var
  fPCXStream: TFileStream;
begin
  fPCXStream := TFileStream.Create(Filename, fmOpenRead);
  try
    fPCXStream.Position := 0;
    LoadFromStream(fPCXStream);
  finally
    fPCXStream.Free;
  end;
end;

//==============================================================================
// TPCXFile.SaveToFile
//
//---------------------------------------------------------------------
//
//==============================================================================
procedure TPCXFile.SaveToFile(const Filename: string);
var
  fPCXStream: TFileStream;
begin
  fPCXStream := TFileStream.Create(Filename, fmCreate);
  try
    fPCXStream.Position := 0;
    SaveToStream(fPCXStream);
  finally
    fPCXStream.Free;
  end;
end;

//==============================================================================
// TPCXFile.LoadFromStream
//
//---------------------------------------------------------------------
//
//==============================================================================
procedure TPCXFile.LoadFromStream(Stream: TStream);
var
  fFileLength: Cardinal;
begin
  // Read the PCX header
  Stream.Read(fPCXHeader, SizeOf(fPCXHeader));

  // Check the ID byte
  if fPCXHeader.fID <> $0A then
    raise Exception.Create(FORMAT_ERROR);

  (*
  Check PCX version byte
  ======================
  Versionbyte = 0 => PC PaintBrush V2.5
  Versionbyte = 2 => PC Paintbrush V2.8 with palette information
  Versionbyte = 3 => PC Paintbrush V2.8 without palette information
  Versionbyte = 4 => PC Paintbrush for Windows
  Versionbyte = 5 => PC Paintbrush V3 and up, and PC Paintbrush Plus
                     with 24 bit image support
  *)
  // Check the PCX version
  if fPCXHeader.fVersion <> 5 then
    raise Exception.Create(VERSION_ERROR);

  // Calculate width
  fWidth := fPCXHeader.fWindow.wRight - fPCXHeader.fWindow.wLeft + 1;
  if fWidth < 0 then
    raise Exception.Create(WIDTH_OUT_OF_RANGE);

  // Calculate height
  fHeight := fPCXHeader.fWindow.wBottom - fPCXHeader.fWindow.wTop + 1;
  if fHeight < 0 then
    raise Exception.Create(HEIGHT_OUT_OF_RANGE);

  // Is it too large?
  if fWidth > fMaxImageWidth then
    raise Exception.Create(IMAGE_WIDTH_TOO_LARGE);

  // Calculate pixelformat
  fPixelFormat := fPCXHeader.fPlanes * fPCXHeader.fBitsPerPixel;

  // Calculate number of colors
  fColorDepth := 1 shl fPixelFormat;

  // Is this image supported?
  if not (fPixelFormat in [1, 8, 24]) then
    raise Exception.Create(ERROR_UNSUPPORTED);

  // The lines following are NOT tested!!!
  (*
  If fColorDepth <= 16 then
     For I := 0 to fColorDepth - 1 do
        Begin
        If fPCXHeader.fVersion = 3 then
           Begin
           fPCXPalette.fPalette[I].R := fPCXHeader.fColorMap[I].R shl 2;
           fPCXPalette.fPalette[I].G := fPCXHeader.fColorMap[I].G shl 2;
           fPCXPalette.fPalette[I].B := fPCXHeader.fColorMap[I].B shl 2;
           End
        else
           Begin
           fPCXPalette.fPalette[I].R := fPCXHeader.fColorMap[I].R;
           fPCXPalette.fPalette[I].G := fPCXHeader.fColorMap[I].G;
           fPCXPalette.fPalette[I].B := fPCXHeader.fColorMap[I].B;
           End;
        End;
  *)

  // Calculate number of data bytes

  // If fFileLength > fMaxDataFileLength then
  //    Raise Exception.Create(INPUT_FILE_TOO_LARGE);

  if fPixelFormat = 24 then
  begin
    fFileLength := Stream.Size - Stream.Position;
    SetLength(fPCXData.fData, fFileLength);
    // Read the data
    Stream.Read(fPCXData.fData[0], fFileLength);
    fHasPalette := False;
  end;

  if fPixelFormat in [1, 8] then
  begin
    fFileLength := Stream.Size - Stream.Position - 769;
    SetLength(fPCXData.fData, fFileLength);
    // Correct number of data bytes
    Stream.Read(fPCXData.fData[0], fFilelength);
    // Read the palette
    Stream.Read(fPCXPalette, SizeOf(fPCXPalette));
    fHasPalette := True;
    // Check palette signature byte
    if fPCXPalette.fSignature <> $0C then
      raise Exception.Create(PALETTE_ERROR);
  end;

end;

//==============================================================================
// TPCXFile.SaveToStream
//
//---------------------------------------------------------------------
//
//==============================================================================
procedure TPCXFile.SaveToStream(Stream: TStream);
begin
  fHasPalette := False;
  Stream.Write(fPCXHeader, SizeOf(fPCXHeader));
  Stream.Write(fPCXData.fData[0], fCurrentPos);
  if fPixelFormat in [1, 8] then
  begin
    Stream.Write(fPCXPalette, SizeOf(fPCXPalette));
    fHasPalette := True;
  end;
end;
//---------------------------------------------------------------------

var
  cntref: integer = 0;

//==============================================================================
//
// TPCXTextureManager.Create
//
//==============================================================================
constructor TPCXTextureManager.Create;
begin
  inherited Create;
  SetFileExt('.PCX');
  ferrorstring := '';
end;

//==============================================================================
//
// TPCXTextureManager.LoadHeader
//
//==============================================================================
function TPCXTextureManager.LoadHeader(stream: TDStream): boolean;
var
  buf: PByteArray;
  ftmpfilename: string;
  line: PLongWordArray;
  img: TPCXImage;
  x, y: integer;
  buffer: LongWord;
  trcolor: LongWord;
  trcolor2: LongWord;
  trcolor3: LongWord;
  errorstr: string;
  bmp: TBitmap;
begin
  ftmpfilename := I_NewTempFile('PCX' + itoa(cntref));
  Inc(cntref);
  buf := malloc(stream.size);
  stream.Seek(0, sFromBeginning);
  stream.Read(buf^, stream.Size);
  result := M_WriteFile(ftmpfilename, buf, stream.Size);
  memfree(Pointer(buf), stream.size);

  errorstr := '';
  if result then
  begin
    img := TPCXImage.Create;
    try
      try
        img.LoadFromFile(ftmpfilename);
        FBitmap^.SetTransparentColor(0);
        FBitmap^.SetBytesPerPixel(4);
        FBitmap^.ScaleTo(img.Bitmap.Width, img.Bitmap.Height);
        img.Bitmap.PixelFormat := pf32bit;
        trcolor := FBitmap.GetTransparentColor;
        trcolor2 := FBitmap.GetTransparentColor2;
        trcolor3 := FBitmap.GetTransparentColor3;
        for y := 0 to img.Bitmap.Height - 1 do
        begin
          line := img.Bitmap.ScanLine[y];
          for x := 0 to img.Bitmap.Width - 1 do
          begin
            buffer := line[x];
            if (buffer = trcolor) or (buffer = trcolor2) or (buffer = trcolor3) then
              buffer := 0;
            FBitmap^.PutPixels(x, y, 1, @buffer, 32);
          end;
        end;
        FBitmap^.SetTransparentColor(0);
        FBitmap^.SetTransparentColor2(0);
        FBitmap^.SetTransparentColor3(0);
      except
        on e: Exception do
        begin
          errorstr := e.Message;
          Result := False;
        end;
      end;
    finally
      img.Free;
    end;

    if errorstr <> '' then
    begin
      bmp := TBitmap.Create;
      try
        bmp.Width := 0;
        bmp.Height := 0;
        PCXLoadFromFile(ftmpfilename, bmp);
        if (bmp.Width > 0) and (bmp.Height > 0) then
        begin
          errorstr := '';

          FBitmap^.SetTransparentColor(0);
          FBitmap^.SetBytesPerPixel(4);
          FBitmap^.ScaleTo(bmp.Width, bmp.Height);
          bmp.PixelFormat := pf32bit;
          trcolor := FBitmap.GetTransparentColor;
          trcolor2 := FBitmap.GetTransparentColor2;
          trcolor3 := FBitmap.GetTransparentColor3;
          for y := 0 to bmp.Height - 1 do
          begin
            line := bmp.ScanLine[y];
            for x := 0 to bmp.Width - 1 do
            begin
              buffer := line[x];
              if (buffer = trcolor) or (buffer = trcolor2) or (buffer = trcolor3) then
                buffer := 0;
              FBitmap^.PutPixels(x, y, 1, @buffer, 32);
            end;
          end;
          FBitmap^.SetTransparentColor(0);
          FBitmap^.SetTransparentColor2(0);
          FBitmap^.SetTransparentColor3(0);
        end;
      finally
        bmp.Free;
      end;
    end;

    if errorstr <> '' then
      I_Warning('TPCXTextureManager(): ' + errorstr + #13#10);
  end;

  ferrorstring := errorstr;

  fdelete(ftmpfilename);
end;

//==============================================================================
//
// TPCXTextureManager.LoadImage
//
//==============================================================================
function TPCXTextureManager.LoadImage(stream: TDStream): boolean;
begin
  Result := ferrorstring = '';
end;

//==============================================================================
//
// TPCXTextureManager.Destroy
//
//==============================================================================
destructor TPCXTextureManager.Destroy;
begin
  inherited;
end;

//==============================================================================
//
// T_IsValidPCXHeader
//
//==============================================================================
function T_IsValidPCXHeader(const p: Pointer): boolean;
var
  h: PPCXImageHeader;
  fHeight: Integer;
  fWidth: Integer;
  fPixelFormat: BYTE;
begin
  h := p;

  Result := False;

  if h.fID <> $0A then
    Exit;

  (*
  Check PCX version byte
  ======================
  Versionbyte = 0 => PC PaintBrush V2.5
  Versionbyte = 2 => PC Paintbrush V2.8 with palette information
  Versionbyte = 3 => PC Paintbrush V2.8 without palette information
  Versionbyte = 4 => PC Paintbrush for Windows
  Versionbyte = 5 => PC Paintbrush V3 and up, and PC Paintbrush Plus
                     with 24 bit image support
  *)
  // Check the PCX version
  if not (h.fVersion in [0, 2, 3, 4, 5]) then
    Exit;

  if not (h.fCompressed in [0, 1]) then
    Exit;

  // Calculate width
  fWidth := h.fWindow.wRight - h.fWindow.wLeft + 1;
  if fWidth <= 1 then // JVAL: Do not allow width = 1
    Exit;
  if fWidth > fMaxImageWidth then
    Exit;

  // Calculate height
  fHeight := h.fWindow.wBottom - h.fWindow.wTop + 1;
  if fHeight <= 1 then // JVAL: Do not allow height = 1
    Exit;
  if fHeight > 8192 then
    Exit;

  // Calculate pixelformat
  fPixelFormat := h.fPlanes * h.fBitsPerPixel;

  // Is this image supported?
  if not (fPixelFormat in [1, 4, 8, 16, 24, 32]) then
    Exit;

  Result := True;
end;

end.

