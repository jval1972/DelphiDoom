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
//  PNG image format.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

{Portable Network Graphics Delphi 1.564      (31 July 2006)   }

{This is a full, open sourced implementation of png in Delphi }
{It has native support for most of png features including the }
{partial transparency, gamma and more.                        }
{for the latest version, please be sure to check my website   }
{http://pngdelphi.sourceforge.net                             }
{Gustavo Huffenbacher Daud (gustavo.daud@terra.com.br)        }

{
  Version 1.564
  2006-07-25   BUG 1     - There was one GDI Palette object leak
                           when assigning from other PNG (fixed)
               BUG 2     - Loosing color information when assigning png
                           to bmp on lower screen depth system
               BUG 3     - There was a bug in TStream.GetSize
                           (fixed thanks to Vladimir Panteleev)
               IMPROVE 1 - When assigning png to bmp now alpha information
                           is drawn (simulated into a white background)

  Version 1.563
  2006-07-25   BUG 1     - There was a memory bug in the main component
                           destructor (fixed thanks to Steven L Brenner)
               BUG 2     - The packages name contained spaces which was
                           causing some strange bugs in Delphi
                           (fixed thanks to Martijn Saly)
               BUG 3     - Lots of fixes when handling palettes
                           (bugs implemented in the last version)
                           Fixed thanks to Gabriel Corneanu!!!
               BUG 4     - CreateAlpha was raising an error because it did
                           not resized the palette chunk it created;
                           Fixed thanks to Miha Sokolov
               IMPROVE 1 - Renamed the pngzlib.pas unit to zlibpas.pas
                           as a tentative to all libraries use the same
                           shared zlib implementation and to avoid including
                           two or three times the same P-Code.
                           (Gabriel Corneanu idea)

  Version 1.561
  2006-05-17   BUG 1     - There was a bug in the method that draws semi
                           transparent images (a memory leak). fixed.

  Version 1.56
  2006-05-09 - IMPROVE 1 - Delphi standard TCanvas support is now implemented
               IMPROVE 2 - The PNG files may now be resized and created from
                           scratch using CreateBlank, Resize, Width and Height
               BUG 1     - Fixed some bugs on handling tRNS transparencies
               BUG 2     - Fixed bugs related to palette handling

  Version 1.535
  2006-04-21 - IMPROVE 1 - Now the library uses the latest ZLIB release (1.2.3)
                           (thanks to: Roberto Della Pasqua
                           http://www.dellapasqua.com/delphizlib/)

  Version 1.53
  2006-04-14 -
               BUG 1 - Remove transparency was not working for
                       RGB Alpha and Grayscale alpha. fixed
               BUG 2 - There was a bug were compressed text chunks no keyword
                       name could not be read
               IMPROVE 1 - Add classes and methods to work with the pHYs chunk
                           (including TPNGObject.DrawUsingPixelInformation)
               IMPROVE 3 - Included a property Version to return the library
                           version
               IMPROVE 4 - New polish translation (thanks to Piotr Domanski)
               IMPROVE 5 - Now packages for delphi 5, 6, 7, 2005 and 2006

               Also Martijn Saly (thany) made some improvements in the library:
               IMPROVE 1 - SetPixel now works with grayscale
               IMPROVE 2 - Palette property now can be written using a
                           windows handle
               Thanks !!

  Version 1.5
  2005-06-29 - Fixed a lot of bugs using tips from mails that I´ve
               being receiving for some time
                 BUG 1 - Loosing palette when assigning to TBitmap. fixed
                 BUG 2 - SetPixels and GetPixels worked only with
                         parameters in range 0..255. fixed
                 BUG 3 - Force type address off using directive
                 BUG 4 - TChunkzTXt contained an error
                 BUG 5 - MaxIdatSize was not working correctly (fixed thanks
                 to Gabriel Corneanu
                 BUG 6 - Corrected german translation (thanks to Mael Horz)
               And the following improvements:
                 IMPROVE 1 - Create ImageHandleValue properties as public in
                             TChunkIHDR to get access to this handle
                 IMPROVE 2 - Using SetStretchBltMode to improve stretch quality
                 IMPROVE 3 - Scale is now working for alpha transparent images
                 IMPROVE 4 - GammaTable propery is now public to support an
                             article in the help file

  Version 1.4361
  2003-03-04 - Fixed important bug for simple transparency when using
               RGB, Grayscale color modes

  Version 1.436
  2003-03-04 - * NEW * Property Pixels for direct access to pixels
               * IMPROVED * Palette property (TPngObject) (read only)
               Slovenian traslation for the component (Miha Petelin)
               Help file update (scanline article/png->jpg example)

  Version 1.435
  2003-11-03 - * NEW * New chunk implementation zTXt (method AddzTXt)
               * NEW * New compiler flags to store the extra 8 bits
               from 16 bits samples (when saving it is ignored), the
               extra data may be acessed using ExtraScanline property
               * Fixed * a bug on tIMe chunk
               French translation included (Thanks to IBE Software)
               Bugs fixed

  Version 1.432
  2002-08-24 - * NEW *  A new method, CreateAlpha will transform the
               current image into partial transparency.
               Help file updated with a new article on how to handle
               partial transparency.

  Version 1.431
  2002-08-14 - Fixed and tested to work on:
               C++ Builder 3
               C++ Builder 5
               Delphi 3
               There was an error when setting TransparentColor, fixed
               New method, RemoveTransparency to remove image
               BIT TRANSPARENCY

  Version 1.43
  2002-08-01 - * NEW * Support for Delphi 3 and C++ Builder 3
               Implements mostly some things that were missing,
               a few tweaks and fixes.

  Version 1.428
  2002-07-24 - More minor fixes (thanks to Ian Boyd)
               Bit transparency fixes
               * NEW * Finally support to bit transparency
               (palette / rgb / grayscale -> all)

  Version 1.427
  2002-07-19 - Lots of bugs and leaks fixed
               * NEW * method to easy adding text comments, AddtEXt
               * NEW * property for setting bit transparency,
                       TransparentColor

  Version 1.426
  2002-07-18 - Clipboard finally fixed and working
               Changed UseDelphi trigger to UseDelphi
               * NEW * Support for bit transparency bitmaps
                       when assigning from/to TBitmap objects
               Altough it does not support drawing transparent
               parts of bit transparency pngs (only partial)
               it is closer than ever

  Version 1.425
  2002-07-01 - Clipboard methods implemented
               Lots of bugs fixed

  Version 1.424
  2002-05-16 - Scanline and AlphaScanline are now working correctly.
               New methods for handling the clipboard

  Version 1.423
  2002-05-16 - * NEW * Partial transparency for 1, 2, 4 and 8 bits is
               also supported using the tRNS chunk (for palette and
               grayscaling).
               New bug fixes (Peter Haas).

  Version 1.422
  2002-05-14 - Fixed some critical leaks, thanks to Peter Haas tips.
               New translation for German (Peter Haas).

  Version 1.421
  2002-05-06 - Now uses new ZLIB version, 1.1.4 with some security
               fixes.
               LoadFromResourceID and LoadFromResourceName added and
               help file updated for that.
               The resources strings are now located in pnglang.pas.
               New translation for Brazilian Portuguese.
               Bugs fixed.

 IMPORTANT: As always I´m looking for bugs on the library. If
            anyone has found one, please send me an email and
            I will fix asap. Thanks for all the help and ideas
            I'm receiving so far.}

{My email is    : gustavo.daud@terra.com.br}
{Website link   : http://pngdelphi.sourceforge.net}
{Gustavo Huffenbacher Daud}

unit t_png;

interface

{Triggers avaliable (edit the fields bellow)}
{$TYPEDADDRESS OFF}

{$DEFINE ErrorOnUnknownCritical} //Error when finds an unknown critical chunk
{$DEFINE CheckCRC}               //Enables CRC checking
{$DEFINE RegisterGraphic}        //Registers TPNGObject to use with TPicture
{$DEFINE PartialTransparentDraw} //Draws partial transparent images
{$DEFINE Store16bits}            //Stores the extra 8 bits from 16bits/sample
{$RANGECHECKS OFF} {$J+}

uses
  windows,
  t_main,
  d_delphi,
  z_files;

const
  pngtransparentcolor: integer = $FF00FF;
  pngtransparentcolor2: integer = $FFFF;
  assumecommontranspantcolors: boolean = True;

const
  PNGLibraryVersion = '1.564b'; // JVAL: Change from 1.564 to 1.564b

  EPngInvalidCRCText = 'This "Portable Network Graphics" image is not valid ' +
      'because it contains invalid pieces of data (crc error)';
  EPNGInvalidIHDRText = 'The "Portable Network Graphics" image could not be ' +
      'loaded because one of its main piece of data (ihdr) might be corrupted';
  EPNGMissingMultipleIDATText = 'This "Portable Network Graphics" image is ' +
    'invalid because it has missing image parts.';
  EPNGZLIBErrorText = 'Could not decompress the image because it contains ' +
    'invalid compressed data.'#13#10 + ' Description: ';
  EPNGInvalidPaletteText = 'The "Portable Network Graphics" image contains ' +
    'an invalid palette.';
  EPNGInvalidFileHeaderText = 'The file being readed is not a valid '+
    '"Portable Network Graphics" image because it contains an invalid header.' +
    ' This file may be corruped, try obtaining it again.';
  EPNGIHDRNotFirstText = 'This "Portable Network Graphics" image is not ' +
    'supported or it might be invalid.'#13#10 + '(IHDR chunk is not the first)';
  EPNGNotExistsText = 'The png file could not be loaded because it does not ' +
    'exists.';
  EPNGSizeExceedsText = 'This "Portable Network Graphics" image is not ' +
    'supported because either it''s width or height exceeds the maximum ' +
    'size, which is 65535 pixels length.';
  EPNGUnknownPalEntryText = 'There is no such palette entry.';
  EPNGMissingPaletteText = 'This "Portable Network Graphics" could not be ' +
    'loaded because it uses a color table which is missing.';
  EPNGUnknownCriticalChunkText = 'This "Portable Network Graphics" image ' +
    'contains an unknown critical part which could not be decoded.';
  EPNGUnknownCompressionText = 'This "Portable Network Graphics" image is ' +
    'encoded with an unknown compression scheme which could not be decoded.';
  EPNGUnknownInterlaceText = 'This "Portable Network Graphics" image uses ' +
    'an unknown interlace scheme which could not be decoded.';
  EPNGCannotAssignChunkText = 'The chunks must be compatible to be assigned.';
  EPNGUnexpectedEndText = 'This "Portable Network Graphics" image is invalid ' +
    'because the decoder found an unexpected end of the file.';
  EPNGNoImageDataText = 'This "Portable Network Graphics" image contains no ' +
    'data.';
  EPNGCannotAddChunkText = 'The program tried to add a existent critical ' +
    'chunk to the current image which is not allowed.';
  EPNGCannotAddInvalidImageText = 'It''s not allowed to add a new chunk ' +
    'because the current image is invalid.';
  EPNGCouldNotLoadResourceText = 'The png image could not be loaded from the ' +
    'resource ID.';
  EPNGOutMemoryText = 'Some operation could not be performed because the ' +
    'system is out of resources. Close some windows and try again.';
  EPNGCannotChangeTransparentText = 'Setting bit transparency color is not ' +
    'allowed for png images containing alpha value for each pixel ' +
    '(COLOR_RGBALPHA and COLOR_GRAYSCALEALPHA)';
  EPNGHeaderNotPresentText = 'This operation is not valid because the ' +
    'current image contains no valid header.';
  EInvalidNewSize = 'The new size provided for image resizing is invalid.';
  EInvalidSpec = 'The "Portable Network Graphics" could not be created ' +
    'because invalid image type parameters have being provided.';

const
  {ZLIB constants}
  ZLIBErrors: array[-6..2] of string = ('incompatible version (-6)',
    'buffer error (-5)', 'insufficient memory (-4)', 'data error (-3)',
    'stream error (-2)', 'file error (-1)', '(0)', 'stream end (1)',
    'need dictionary (2)');
  Z_NO_FLUSH      = 0;
  Z_FINISH        = 4;
  Z_STREAM_END    = 1;

  {Avaliable PNG filters for mode 0}
  FILTER_NONE    = 0;
  FILTER_SUB     = 1;
  FILTER_UP      = 2;
  FILTER_AVERAGE = 3;
  FILTER_PAETH   = 4;

  {Avaliable color modes for PNG}
  COLOR_GRAYSCALE      = 0;
  COLOR_RGB            = 2;
  COLOR_PALETTE        = 3;
  COLOR_GRAYSCALEALPHA = 4;
  COLOR_RGBALPHA       = 6;

type
  {Direct access to pixels using R,G,B}
  TRGBTriple = packed record
    rgbtBlue: Byte;
    rgbtGreen: Byte;
    rgbtRed: Byte;
  end;

  TRGBLine = array[word] of TRGBTriple;
  pRGBLine = ^TRGBLine;

  {Same as TBitmapInfo but with allocated space for}
  {palette entries}
  TRGBQuad = packed record
    rgbBlue: Byte;
    rgbGreen: Byte;
    rgbRed: Byte;
    rgbReserved: Byte;
  end;
  PRGBQuad = ^TRGBQuad;

  TMAXBITMAPINFO = packed record
    bmiHeader: TBitmapInfoHeader;
    bmiColors: packed array[0..255] of TRGBQuad;
  end;

  {Transparency mode for pngs}
  TPNGTransparencyMode = (ptmNone, ptmBit, ptmPartial);
  {Pointer to a cardinal type}
  pCardinal = ^Cardinal;
  {Access to a rgb pixel}
  pRGBPixel = ^TRGBPixel;
  TRGBPixel = packed record
    B, G, R: Byte;
  end;

  {Pointer to an array of bytes type}
  TByteArray = array[Word] of Byte;
  PByteArray = ^TByteArray;

  {Forward}
  TPNGObject = class;
  pPointerArray = ^TPointerArray;
  TPointerArray = array[Word] of Pointer;

  {Contains a list of objects}
  TPNGPointerList = class
  private
    fOwner: TPNGObject;
    fCount: Cardinal;
    fMemory: pPointerArray;
    function GetItem(Index: Cardinal): Pointer;
    procedure SetItem(Index: Cardinal; const Value: Pointer);
  protected
    {Removes an item}
    function Remove(Value: Pointer): Pointer; virtual;
    {Inserts an item}
    procedure Insert(Value: Pointer; Position: Cardinal);
    {Add a new item}
    procedure Add(Value: Pointer);
    {Returns an item}
    property Item[Index: Cardinal]: Pointer read GetItem write SetItem;
    {Set the size of the list}
    procedure SetSize(const Size: Cardinal);
    {Returns owner}
    property Owner: TPNGObject read fOwner;
  public
    {Returns number of items}
    property Count: Cardinal read fCount write SetSize;
    {Object being either created or destroyed}
    constructor Create(AOwner: TPNGObject);
    destructor Destroy; override;
  end;

  {Forward declaration}
  TChunk = class;
  TChunkClass = class of TChunk;

  {Same as TPNGPointerList but providing typecasted values}
  TPNGList = class(TPNGPointerList)
  private
    {Used with property Item}
    function GetItem(Index: Cardinal): TChunk;
  public
    {Finds the first item with this class}
    function FindChunk(ChunkClass: TChunkClass): TChunk;
    {Removes an item}
    procedure RemoveChunk(Chunk: TChunk); overload;
    {Add a new chunk using the class from the parameter}
    function Add(ChunkClass: TChunkClass): TChunk;
    {Returns pointer to the first chunk of class}
    function ItemFromClass(ChunkClass: TChunkClass): TChunk;
    {Returns a chunk item from the list}
    property Item[Index: Cardinal]: TChunk read GetItem;
  end;

  {Forward}
  TChunkIHDR = class;
  TChunkpHYs = class;
  {Interlace method}
  TInterlaceMethod = (imNone, imAdam7);
  {Compression level type}
  TCompressionLevel = 0..9;
  {Filters type}
  TFilter = (pfNone, pfSub, pfUp, pfAverage, pfPaeth);
  TFilters = set of TFilter;

  {Png implementation object}
  TPngObject = class
  protected
    {Inverse gamma table values}
    InverseGamma: array[Byte] of Byte;
    procedure InitializeGamma;
  private
    {Filters to test to encode}
    fFilters: TFilters;
    {Compression level for ZLIB}
    fCompressionLevel: TCompressionLevel;
    {Maximum size for IDAT chunks}
    fMaxIdatSize: Integer;
    {Returns if image is interlaced}
    fInterlaceMethod: TInterlaceMethod;
    {Chunks object}
    fChunkList: TPngList;
    {Patch offsets (from grAb chunk)}
    fLeftOffset: integer;
    fTopOffset: integer;
    // tRNS for 256 palette image
    fHastRNS256: boolean;
    ftRNSArray256: packed array[0..255] of byte;
    {Clear all chunks in the list}
    procedure ClearChunks;
    {Returns if header is present}
    function HeaderPresent: Boolean;
    procedure GetPixelInfo(var LineSize, Offset: Cardinal);
    {Returns linesize and byte offset for pixels}
    procedure SetMaxIdatSize(const Value: Integer);
    function GetAlphaScanline(const LineIndex: Integer): PByteArray;
    function GetScanline(const LineIndex: Integer): Pointer;
    {$IFDEF Store16bits}
    function GetExtraScanline(const LineIndex: Integer): Pointer;
    {$ENDIF}
    function GetPixelInformation: TChunkpHYs;
    function GetTransparencyMode: TPNGTransparencyMode;
    function GetTransparentColor: LongWord;
    procedure SetTransparentColor(const Value: LongWord);
    {Returns the version}
    function GetLibraryVersion: String;
  protected
    fError: string;
    {Being created}
    BeingCreated: Boolean;
    {Returns / set the image palette}
    function GetPalette: HPalette;
    procedure SetPalette(palEntries: pLogPalette);
    procedure SetHPalette(value: HPALETTE);
    procedure DoSetPalette(Value: HPalette; const UpdateColors: Boolean);
    {Returns/sets image width and height}
    function GetWidth: Integer;
    function GetHeight: Integer;
    procedure SetWidth(Value: Integer);
    procedure SetHeight(Value: Integer);
    {Assigns from another TPNGObject}
    procedure AssignPNG(Source: TPNGObject);
    {Returns if the image is empty}
    function GetEmpty: Boolean;
    {Used with property Header}
    function GetHeader: TChunkIHDR;
    {Draws using partial transparency}
    procedure DrawPartialTrans(DC: HDC; Rect: TRect);
    {Returns if the image is transparent}
    function GetTransparent: Boolean;
    {Returns a pixel}
    function GetPixels(const X, Y: Integer): LongWord; virtual;
    procedure SetPixels(const X, Y: Integer; const Value: LongWord); virtual;
    function GettRNSArray256: PByteArray;
  public
    PaletteTable: TPalette;
    {Gamma table array}
    GammaTable: array[Byte] of Byte;
    {Resizes the PNG image}
    procedure Resize(const CX, CY: Integer);
    {Generates alpha information}
    procedure CreateAlpha;
    {Removes the image transparency}
    procedure RemoveTransparency;
    {Transparent color}
    property TransparentColor: LongWord read GetTransparentColor write
      SetTransparentColor;
    {Add text chunk, TChunkTEXT, TChunkzTXT}
    procedure AddtEXt(const Keyword, Text: String);
    procedure AddzTXt(const Keyword, Text: String);
    {Calling errors}
    procedure RaiseError(Text: String);
    function IOresult: string;
    {Returns a scanline from png}
    property Scanline[const Index: Integer]: Pointer read GetScanline;
    {$IFDEF Store16bits}
    property ExtraScanline[const Index: Integer]: Pointer read GetExtraScanline;
    {$ENDIF}
    {Used to return pixel information}
    function HasPixelInformation: Boolean;
    property PixelInformation: TChunkpHYs read GetPixelInformation;
    property AlphaScanline[const Index: Integer]: PByteArray read
      GetAlphaScanline;

    {Returns pointer to the header}
    property Header: TChunkIHDR read GetHeader;
    {Returns the transparency mode used by this png}
    property TransparencyMode: TPNGTransparencyMode read GetTransparencyMode;
    {Assigns from another object}
    procedure AssignHandle(Handle: HBitmap; Transparent: Boolean;
      TransparentColor: ColorRef);
    {Width and height properties}
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    {Returns if the image is interlaced}
    property InterlaceMethod: TInterlaceMethod read fInterlaceMethod
      write fInterlaceMethod;
    {Filters to test to encode}
    property Filters: TFilters read fFilters write fFilters;
    {Maximum size for IDAT chunks, default and minimum is 65536}
    property MaxIdatSize: Integer read fMaxIdatSize write SetMaxIdatSize;
    {Property to return if the image is empty or not}
    property Empty: Boolean read GetEmpty;
    {Compression level}
    property CompressionLevel: TCompressionLevel read fCompressionLevel
      write fCompressionLevel;
    {Access to the chunk list}
    property Chunks: TPngList read fChunkList;
    {Object being created and destroyed}
    constructor Create;
    constructor CreateBlank(ColorType, Bitdepth: Cardinal; cx, cy: Integer);
    destructor Destroy; override;
    procedure LoadFromFile(const Filename: String);
    procedure SaveToFile(const Filename: String);
    procedure LoadFromStream(Stream: TDStream);
    procedure SaveToStream(Stream: TDStream);
    {Access to the png pixels}
    property Pixels[const X, Y: Integer]: LongWord read GetPixels write SetPixels;
    {Palette property}
    property Palette: HPalette read GetPalette write
      SetHPalette;
    {Returns the version}
    property Version: String read GetLibraryVersion;
    property ErrorString: string read fError;
    property LeftOffset: integer read fLeftOffset;
    property TopOffset: integer read fTopOffset;
    property HastRNS256: boolean read fHastRNS256;
    property tRNSArray256: PByteArray read GettRNSArray256;
  end;

  {Chunk name object}
  TChunkName = array[0..3] of Char;

  {Global chunk object}
  TChunk = class
  private
    {Contains data}
    fData: Pointer;
    fDataSize: Cardinal;
    {Stores owner}
    fOwner: TPngObject;
    {Stores the chunk name}
    fName: TChunkName;
    {Returns pointer to the TChunkIHDR}
    function GetHeader: TChunkIHDR;
    {Used with property index}
    function GetIndex: Integer;
    {Should return chunk class/name}
    class function GetName: String; virtual;
    {Returns the chunk name}
    function GetChunkName: String;
  public
    {Returns index from list}
    property Index: Integer read GetIndex;
    {Returns pointer to the TChunkIHDR}
    property Header: TChunkIHDR read GetHeader;
    {Resize the data}
    procedure ResizeData(const NewSize: Cardinal);
    {Returns data and size}
    property Data: Pointer read fData;
    property DataSize: Cardinal read fDataSize;
    {Assigns from another TChunk}
    procedure Assign(Source: TChunk); virtual;
    {Returns owner}
    property Owner: TPngObject read fOwner;
    {Being destroyed/created}
    constructor Create(Owner: TPngObject); virtual;
    destructor Destroy; override;
    {Returns chunk class/name}
    property Name: String read GetChunkName;
    {Loads the chunk from a stream}
    function LoadFromStream(Stream: TDStream; const ChunkName: TChunkName;
      Size: Integer): Boolean; virtual;
    {Saves the chunk to a stream}
    function SaveData(Stream: TDStream): Boolean;
    function SaveToStream(Stream: TDStream): Boolean; virtual;
  end;

  {Chunk classes}
  TChunkIEND = class(TChunk);     {End chunk}

  {IHDR data}
  pIHDRData = ^TIHDRData;
  TIHDRData = packed record
    Width, Height: Cardinal;
    BitDepth,
    ColorType,
    CompressionMethod,
    FilterMethod,
    InterlaceMethod: Byte;
  end;

  {Information header chunk}
  TChunkIHDR = class(TChunk)
  private
    {Current image}
    ImageHandle: HBitmap;
    ImageDC: HDC;
    ImagePalette: HPalette;
    {Output windows bitmap}
    HasPalette: Boolean;
    BitmapInfo: TMaxBitmapInfo;
    {Stores the image bytes}
    {$IFDEF Store16bits}ExtraImageData: Pointer;{$ENDIF}
    ImageData: pointer;
    ImageAlpha: Pointer;

    {Contains all the ihdr data}
    IHDRData: TIHDRData;
  protected
    BytesPerRow: Integer;
    {Creates a grayscale palette}
    function CreateGrayscalePalette(Bitdepth: Integer): HPalette;
    {Copies the palette to the Device Independent bitmap header}
    procedure PaletteToDIB(Palette: HPalette);
    {Resizes the image data to fill the color type, bit depth, }
    {width and height parameters}
    procedure PrepareImageData;
    {Release allocated ImageData memory}
    procedure FreeImageData;
  public
    {Access to ImageHandle}
    property ImageHandleValue: HBitmap read ImageHandle;
    {Properties}
    property Width: Cardinal read IHDRData.Width write IHDRData.Width;
    property Height: Cardinal read IHDRData.Height write IHDRData.Height;
    property BitDepth: Byte read IHDRData.BitDepth write IHDRData.BitDepth;
    property ColorType: Byte read IHDRData.ColorType write IHDRData.ColorType;
    property CompressionMethod: Byte read IHDRData.CompressionMethod
      write IHDRData.CompressionMethod;
    property FilterMethod: Byte read IHDRData.FilterMethod
      write IHDRData.FilterMethod;
    property InterlaceMethod: Byte read IHDRData.InterlaceMethod
      write IHDRData.InterlaceMethod;
    {Loads the chunk from a stream}
    function LoadFromStream(Stream: TDStream; const ChunkName: TChunkName;
      Size: Integer): Boolean; override;
    {Saves the chunk to a stream}
    function SaveToStream(Stream: TDStream): Boolean; override;
    {Destructor/constructor}
    constructor Create(Owner: TPngObject); override;
    destructor Destroy; override;
    {Assigns from another TChunk}
    procedure Assign(Source: TChunk); override;
  end;

  {pHYs chunk}
  pUnitType = ^TUnitType;
  TUnitType = (utUnknown, utMeter);
  TChunkpHYs = class(TChunk)
  private
    fPPUnitX, fPPUnitY: Cardinal;
    fUnit: TUnitType;
  public
    {Returns the properties}
    property PPUnitX: Cardinal read fPPUnitX write fPPUnitX;
    property PPUnitY: Cardinal read fPPUnitY write fPPUnitY;
    property UnitType: TUnitType read fUnit write fUnit;
    {Loads the chunk from a stream}
    function LoadFromStream(Stream: TDStream; const ChunkName: TChunkName;
      Size: Integer): Boolean; override;
    {Saves the chunk to a stream}
    function SaveToStream(Stream: TDStream): Boolean; override;
    {Assigns from another TChunk}
    procedure Assign(Source: TChunk); override;
  end;

  {Gamma chunk}
  TChunkgAMA = class(TChunk)
  private
    {Returns/sets the value for the gamma chunk}
    function GetValue: Cardinal;
    procedure SetValue(const Value: Cardinal);
  public
    {Returns/sets gamma value}
    property Gamma: Cardinal read GetValue write SetValue;
    {Loading the chunk from a stream}
    function LoadFromStream(Stream: TDStream; const ChunkName: TChunkName;
      Size: Integer): Boolean; override;
    {Being created}
    constructor Create(Owner: TPngObject); override;
    {Assigns from another TChunk}
    procedure Assign(Source: TChunk); override;
  end;

  {ZLIB Decompression extra information}
  TZStreamRec2 = packed record
    {From ZLIB}
    ZLIB: TZStreamRec;
    {Additional info}
    Data: Pointer;
    fStream: TDStream;
  end;

  {Palette chunk}
  TChunkPLTE = class(TChunk)
  protected
    {Number of items in the palette}
    fCount: Integer;
  private
    {Contains the palette handle}
    function GetPaletteItem(Index: Byte): TRGBQuad;
  public
    {Returns the color for each item in the palette}
    property Item[Index: Byte]: TRGBQuad read GetPaletteItem;
    {Returns the number of items in the palette}
    property Count: Integer read fCount;
    {Loads the chunk from a stream}
    function LoadFromStream(Stream: TDStream; const ChunkName: TChunkName;
      Size: Integer): Boolean; override;
    {Saves the chunk to a stream}
    function SaveToStream(Stream: TDStream): Boolean; override;
    {Assigns from another TChunk}
    procedure Assign(Source: TChunk); override;
  end;

  {Transparency information}
  TChunktRNS = class(TChunk)
  private
    fBitTransparency: Boolean;
    function GetTransparentColor: ColorRef;
    {Returns the transparent color}
    procedure SetTransparentColor(const Value: ColorRef);
  public
    {Palette values for transparency}
    PaletteValues: array[Byte] of Byte;
    {Returns if it uses bit transparency}
    property BitTransparency: Boolean read fBitTransparency;
    {Returns the transparent color}
    property TransparentColor: ColorRef read GetTransparentColor write
      SetTransparentColor;
    {Loads/saves the chunk from/to a stream}
    function LoadFromStream(Stream: TDStream; const ChunkName: TChunkName;
      Size: Integer): Boolean; override;
    function SaveToStream(Stream: TDStream): Boolean; override;
    {Assigns from another TChunk}
    procedure Assign(Source: TChunk); override;
  end;

  {Actual image information}
  TChunkIDAT = class(TChunk)
  private
    {Holds another pointer to the TChunkIHDR}
    Header: TChunkIHDR;
    {Stores temporary image width and height}
    ImageWidth, ImageHeight: Integer;
    {Size in bytes of each line and offset}
    Row_Bytes, Offset: Cardinal;
    {Contains data for the lines}
    Encode_Buffer: array[0..5] of PByteArray;
    Row_Buffer: array[Boolean] of PByteArray;
    {Variable to invert the Row_Buffer used}
    RowUsed: Boolean;
    {Ending position for the current IDAT chunk}
    EndPos: Integer;
    {Filter the current line}
    procedure FilterRow;
    {Filter to encode and returns the best filter}
    function FilterToEncode: Byte;
    {Reads ZLIB compressed data}
    function IDATZlibRead(var ZLIBStream: TZStreamRec2; Buffer: Pointer;
      Count: Integer; var EndPos: Integer; var crcfile: Cardinal): Integer;
    {Compress and writes IDAT data}
    procedure IDATZlibWrite(var ZLIBStream: TZStreamRec2; Buffer: Pointer;
      const Length: Cardinal);
    procedure FinishIDATZlib(var ZLIBStream: TZStreamRec2);
    {Prepares the palette}
    procedure PreparePalette;
  protected
    {Decode interlaced image}
    procedure DecodeInterlacedAdam7(Stream: TDStream;
      var ZLIBStream: TZStreamRec2; const Size: Integer; var crcfile: Cardinal);
    {Decode non interlaced imaged}
    procedure DecodeNonInterlaced(Stream: TDStream;
      var ZLIBStream: TZStreamRec2; const Size: Integer;
      var crcfile: Cardinal);
  protected
    {Encode non interlaced images}
    procedure EncodeNonInterlaced(Stream: TDStream;
      var ZLIBStream: TZStreamRec2);
    {Encode interlaced images}
    procedure EncodeInterlacedAdam7(Stream: TDStream;
      var ZLIBStream: TZStreamRec2);
  protected
    {Memory copy methods to decode}
    procedure CopyNonInterlacedRGB8(
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
    procedure CopyNonInterlacedRGB16(
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
    procedure CopyNonInterlacedPalette148(
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
    procedure CopyNonInterlacedPalette2(
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
    procedure CopyNonInterlacedGray2(
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
    procedure CopyNonInterlacedGrayscale16(
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
    procedure CopyNonInterlacedRGBAlpha8(
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
    procedure CopyNonInterlacedRGBAlpha16(
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
    procedure CopyNonInterlacedGrayscaleAlpha8(
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
    procedure CopyNonInterlacedGrayscaleAlpha16(
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
    procedure CopyInterlacedRGB8(const Pass: Byte;
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
    procedure CopyInterlacedRGB16(const Pass: Byte;
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
    procedure CopyInterlacedPalette148(const Pass: Byte;
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
    procedure CopyInterlacedPalette2(const Pass: Byte;
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
    procedure CopyInterlacedGray2(const Pass: Byte;
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
    procedure CopyInterlacedGrayscale16(const Pass: Byte;
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
    procedure CopyInterlacedRGBAlpha8(const Pass: Byte;
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
    procedure CopyInterlacedRGBAlpha16(const Pass: Byte;
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
    procedure CopyInterlacedGrayscaleAlpha8(const Pass: Byte;
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
    procedure CopyInterlacedGrayscaleAlpha16(const Pass: Byte;
      Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
  protected
    {Memory copy methods to encode}
    procedure EncodeNonInterlacedRGB8(Src, Dest, Trans: PChar);
    procedure EncodeNonInterlacedRGB16(Src, Dest, Trans: PChar);
    procedure EncodeNonInterlacedGrayscale16(Src, Dest, Trans: PChar);
    procedure EncodeNonInterlacedPalette148(Src, Dest, Trans: PChar);
    procedure EncodeNonInterlacedRGBAlpha8(Src, Dest, Trans: PChar);
    procedure EncodeNonInterlacedRGBAlpha16(Src, Dest, Trans: PChar);
    procedure EncodeNonInterlacedGrayscaleAlpha8(Src, Dest, Trans: PChar);
    procedure EncodeNonInterlacedGrayscaleAlpha16(Src, Dest, Trans: PChar);
    procedure EncodeInterlacedRGB8(const Pass: Byte; Src, Dest, Trans: PChar);
    procedure EncodeInterlacedRGB16(const Pass: Byte; Src, Dest, Trans: PChar);
    procedure EncodeInterlacedPalette148(const Pass: Byte;
      Src, Dest, Trans: PChar);
    procedure EncodeInterlacedGrayscale16(const Pass: Byte;
      Src, Dest, Trans: PChar);
    procedure EncodeInterlacedRGBAlpha8(const Pass: Byte;
      Src, Dest, Trans: PChar);
    procedure EncodeInterlacedRGBAlpha16(const Pass: Byte;
      Src, Dest, Trans: PChar);
    procedure EncodeInterlacedGrayscaleAlpha8(const Pass: Byte;
      Src, Dest, Trans: PChar);
    procedure EncodeInterlacedGrayscaleAlpha16(const Pass: Byte;
      Src, Dest, Trans: PChar);
  public
    {Loads the chunk from a stream}
    function LoadFromStream(Stream: TDStream; const ChunkName: TChunkName;
      Size: Integer): Boolean; override;
    {Saves the chunk to a stream}
    function SaveToStream(Stream: TDStream): Boolean; override;
  end;

  {Image last modification chunk}
  TChunktIME = class(TChunk)
  private
    {Holds the variables}
    fYear: Word;
    fMonth, fDay, fHour, fMinute, fSecond: Byte;
  public
    {Returns/sets variables}
    property Year: Word read fYear write fYear;
    property Month: Byte read fMonth write fMonth;
    property Day: Byte read fDay write fDay;
    property Hour: Byte read fHour write fHour;
    property Minute: Byte read fMinute write fMinute;
    property Second: Byte read fSecond write fSecond;
    {Loads the chunk from a stream}
    function LoadFromStream(Stream: TDStream; const ChunkName: TChunkName;
      Size: Integer): Boolean; override;
    {Saves the chunk to a stream}
    function SaveToStream(Stream: TDStream): Boolean; override;
    {Assigns from another TChunk}
    procedure Assign(Source: TChunk); override;
  end;

  {Textual data}
  TChunktEXt = class(TChunk)
  private
    fKeyword, fText: String;
  public
    {Keyword and text}
    property Keyword: String read fKeyword write fKeyword;
    property Text: String read fText write fText;
    {Loads the chunk from a stream}
    function LoadFromStream(Stream: TDStream; const ChunkName: TChunkName;
      Size: Integer): Boolean; override;
    {Saves the chunk to a stream}
    function SaveToStream(Stream: TDStream): Boolean; override;
    {Assigns from another TChunk}
    procedure Assign(Source: TChunk); override;
  end;

  {zTXT chunk}
  TChunkzTXt = class(TChunktEXt)
    {Loads the chunk from a stream}
    function LoadFromStream(Stream: TDStream; const ChunkName: TChunkName;
      Size: Integer): Boolean; override;
    {Saves the chunk to a stream}
    function SaveToStream(Stream: TDStream): Boolean; override;
  end;

{Here we test if it's c++ builder or delphi version 3 or less}
{$IFDEF VER110}{$DEFINE DelphiBuilder3Less}{$ENDIF}
{$IFDEF VER100}{$DEFINE DelphiBuilder3Less}{$ENDIF}
{$IFDEF VER93}{$DEFINE DelphiBuilder3Less}{$ENDIF}
{$IFDEF VER90}{$DEFINE DelphiBuilder3Less}{$ENDIF}
{$IFDEF VER80}{$DEFINE DelphiBuilder3Less}{$ENDIF}

{Registers a new chunk class}

//==============================================================================
//
// RegisterChunk
//
//==============================================================================
procedure RegisterChunk(ChunkClass: TChunkClass);
{Calculates crc}
function update_crc(crc: {$IFNDEF DelphiBuilder3Less}Cardinal{$ELSE}Integer
  {$ENDIF}; buf: PByteArray; len: Integer): Cardinal;
{Invert bytes using assembly}

//==============================================================================
//
// ByteSwap
//
//==============================================================================
function ByteSwap(const a: integer): integer;

type
  TPNGBaseTextureManager = object(TTextureManager)
  private
    png: TPngObject;
  protected
    function RGBSwap(buffer: LongWord): LongWord;
  public
    constructor Create(ext: string);
    function CheckPNGError: boolean;
    destructor Destroy; virtual;
  end;

  TPNGTextureManager = object(TPNGBaseTextureManager)
  public
    constructor Create;
    function LoadHeader(stream: TDStream): boolean; virtual;
    function LoadImage(stream: TDStream): boolean; virtual;
  end;

  TPNGSpriteTextureManager = object(TPNGBaseTextureManager)
  private
    ftransparentcolor: Cardinal;
  public
    constructor Create;
    function LoadHeader(stream: TDStream): boolean; virtual;
    function LoadImage(stream: TDStream): boolean; virtual;
  end;

const
  PNGEXT = '.PNG';
  PNGSPRITEEXT = '.PNGSPRITE';

//==============================================================================
//
// PNG_RegisterCommonChunks
//
//==============================================================================
procedure PNG_RegisterCommonChunks(const onlyimportant: boolean);

//==============================================================================
//
// PNG_FreeChunkClassList
//
//==============================================================================
procedure PNG_FreeChunkClassList;

implementation

uses
  i_system;

var
  ChunkClasses: TPngPointerList = nil;
  {Table of CRCs of all 8-bit messages}
  crc_table: array[0..255] of Cardinal;
  {Flag: has the table been computed? Initially false}
  crc_table_computed: Boolean = False;

{Make the table for a fast CRC.}

//==============================================================================
//
// make_crc_table
//
//==============================================================================
procedure make_crc_table;
var
  c: Cardinal;
  n, k: Integer;
begin
  {fill the crc table}
  for n := 0 to 255 do
  begin
    c := Cardinal(n);
    for k := 0 to 7 do
    begin
      if Boolean(c and 1) then
        c := $edb88320 xor (c shr 1)
      else
        c := c shr 1;
    end;
    crc_table[n] := c;
  end;

  {The table has already being computated}
  crc_table_computed := true;
end;

{Update a running CRC with the bytes buf[0..len-1]--the CRC
 should be initialized to all 1's, and the transmitted value
 is the 1's complement of the final running CRC (see the
 crc() routine below)).}
function update_crc(crc: {$IFNDEF DelphiBuilder3Less}Cardinal{$ELSE}Integer
  {$ENDIF}; buf: PByteArray; len: Integer): Cardinal;
var
  c: Cardinal;
  n: Integer;
begin
  c := crc;

  {Create the crc table in case it has not being computed yet}
  if not crc_table_computed then make_crc_table;

  {Update}
  for n := 0 to len - 1 do
    c := crc_table[(c XOR buf^[n]) and $FF] XOR (c shr 8);

  {Returns}
  Result := c;
end;

{Calculates the paeth predictor}

//==============================================================================
//
// PaethPredictor
//
//==============================================================================
function PaethPredictor(a, b, c: Byte): Byte;
var
  pa, pb, pc: Integer;
begin
  { a = left, b = above, c = upper left }
  pa := abs(b - c);      { distances to a, b, c }
  pb := abs(a - c);
  pc := abs(a + b - c * 2);

  { return nearest of a, b, c, breaking ties in order a, b, c }
  if (pa <= pb) and (pa <= pc) then
    Result := a
  else
    if pb <= pc then
      Result := b
    else
      Result := c;
end;

{Invert bytes using assembly}

//==============================================================================
//
// ByteSwap
//
//==============================================================================
function ByteSwap(const a: integer): integer;
asm
  bswap eax
end;

//==============================================================================
//
// ByteSwap16
//
//==============================================================================
function ByteSwap16(inp: word): word;
asm
  bswap eax
  shr   eax, 16
end;

{Calculates number of bytes for the number of pixels using the}
{color mode in the paramenter}

//==============================================================================
//
// BytesForPixels
//
//==============================================================================
function BytesForPixels(const Pixels: Integer; const ColorType,
  BitDepth: Byte): Integer;
begin
  case ColorType of
    {Palette and grayscale contains a single value, for palette}
    {an value of size 2^bitdepth pointing to the palette index}
    {and grayscale the value from 0 to 2^bitdepth with color intesity}
    COLOR_GRAYSCALE, COLOR_PALETTE:
      Result := (Pixels * BitDepth + 7) div 8;
    {RGB contains 3 values R, G, B with size 2^bitdepth each}
    COLOR_RGB:
      Result := (Pixels * BitDepth * 3) div 8;
    {Contains one value followed by alpha value booth size 2^bitdepth}
    COLOR_GRAYSCALEALPHA:
      Result := (Pixels * BitDepth * 2) div 8;
    {Contains four values size 2^bitdepth, Red, Green, Blue and alpha}
    COLOR_RGBALPHA:
      Result := (Pixels * BitDepth * 4) div 8;
    else
      Result := 0;
  end {case ColorType}
end;

type
  PChunkClassInfo = ^TChunkClassInfo;
  TChunkClassInfo = record
    ClassName: TChunkClass;
  end;

{Register a chunk type}

//==============================================================================
//
// RegisterChunk
//
//==============================================================================
procedure RegisterChunk(ChunkClass: TChunkClass);
var
  NewClass: PChunkClassInfo;
begin
  {In case the list object has not being created yet}
  if ChunkClasses = nil then
    ChunkClasses := TPngPointerList.Create(nil);

  {Add this new class}
  new(NewClass);
  NewClass^.ClassName := ChunkClass;
  ChunkClasses.Add(NewClass);
end;

{Free chunk class list}

//==============================================================================
//
// PNG_FreeChunkClassList
//
//==============================================================================
procedure PNG_FreeChunkClassList;
var
  i: Integer;
begin
  if (ChunkClasses <> nil) then
  begin
    for i := 0 to ChunkClasses.Count - 1 do
      Dispose(PChunkClassInfo(ChunkClasses.Item[i]));
    ChunkClasses.Free;
    ChunkClasses := nil;
  end;
end;

{Registering of common chunk classes}

//==============================================================================
//
// PNG_RegisterCommonChunks
//
//==============================================================================
procedure PNG_RegisterCommonChunks(const onlyimportant: boolean);
begin
  {Important chunks}
  RegisterChunk(TChunkIEND);
  RegisterChunk(TChunkIHDR);
  RegisterChunk(TChunkIDAT);
  RegisterChunk(TChunkPLTE);
  RegisterChunk(TChunkgAMA);
  RegisterChunk(TChunktRNS);

  if not onlyimportant then
  begin
    {Not so important chunks}
    RegisterChunk(TChunkpHYs);
    RegisterChunk(TChunktIME);
    RegisterChunk(TChunktEXt);
    RegisterChunk(TChunkzTXt);
  end;
end;

{Creates a new chunk of this class}

//==============================================================================
//
// CreateClassChunk
//
//==============================================================================
function CreateClassChunk(Owner: TPngObject; Name: TChunkName): TChunk;
var
  i: Integer;
  NewChunk: TChunkClass;
begin
  {Looks for this chunk}
  NewChunk := TChunk;  {In case there is no registered class for this}

  {Looks for this class in all registered chunks}
  if Assigned(ChunkClasses) then
    for i := 0 to ChunkClasses.Count - 1 do
    begin
      if PChunkClassInfo(ChunkClasses.Item[i])^.ClassName.GetName = Name then
      begin
        NewChunk := PChunkClassInfo(ChunkClasses.Item[i])^.ClassName;
        break;
      end;
    end;

  {Returns chunk class}
  Result := NewChunk.Create(Owner);
  Result.fName := Name;
end;

{ZLIB support}

const
  ZLIBAllocate = High(Word);

{Initializes ZLIB for decompression}

//==============================================================================
//
// ZLIBInitInflate
//
//==============================================================================
function ZLIBInitInflate(Stream: TDStream): TZStreamRec2;
begin
  {Fill record}
  Fillchar(Result, SizeOf(TZStreamRec2), #0);

  {Set internal record information}
  with Result do
  begin
    GetMem(Data, ZLIBAllocate);
    fStream := Stream;
  end;

  {Init decompression}
  InflateInit_(Result.zlib, zlib_version, SizeOf(TZStreamRec));
end;

{Initializes ZLIB for compression}

//==============================================================================
//
// ZLIBInitDeflate
//
//==============================================================================
function ZLIBInitDeflate(Stream: TDStream;
  Level: TCompressionlevel; Size: Cardinal): TZStreamRec2;
begin
  {Fill record}
  FillChar(Result, SizeOf(TZStreamRec2), #0);

  {Set internal record information}
  with Result, ZLIB do
  begin
    GetMem(Data, Size);
    fStream := Stream;
    next_out := Data;
    avail_out := Size;
  end;

  {Inits compression}
  deflateInit_(Result.zlib, Level, zlib_version, SizeOf(TZStreamRec));
end;

{Terminates ZLIB for compression}

//==============================================================================
//
// ZLIBTerminateDeflate
//
//==============================================================================
procedure ZLIBTerminateDeflate(var ZLIBStream: TZStreamRec2);
begin
  {Terminates decompression}
  DeflateEnd(ZLIBStream.zlib);
  {Free internal record}
  FreeMem(ZLIBStream.Data, ZLIBAllocate);
end;

{Terminates ZLIB for decompression}

//==============================================================================
//
// ZLIBTerminateInflate
//
//==============================================================================
procedure ZLIBTerminateInflate(var ZLIBStream: TZStreamRec2);
begin
  {Terminates decompression}
  InflateEnd(ZLIBStream.zlib);
  {Free internal record}
  FreeMem(ZLIBStream.Data, ZLIBAllocate);
end;

{Decompresses ZLIB into a memory address}

//==============================================================================
//
// DecompressZLIB
//
//==============================================================================
function DecompressZLIB(const Input: Pointer; InputSize: Integer;
  var Output: Pointer; var OutputSize: Integer;
  var ErrorOutput: String): Boolean;
var
  StreamRec: TZStreamRec;
  Buffer: array[Byte] of Byte;
  InflateRet: Integer;
begin
  with StreamRec do
  begin
    {Initializes}
    Result := true;
    OutputSize := 0;

    {Prepares the data to decompress}
    FillChar(StreamRec, SizeOf(TZStreamRec), #0);
    InflateInit_(StreamRec, zlib_version, SizeOf(TZStreamRec));
    next_in := Input;
    avail_in := InputSize;

    {Decodes data}
    repeat
      {In case it needs an output buffer}
      if (avail_out = 0) then
      begin
        next_out := @Buffer;
        avail_out := SizeOf(Buffer);
      end {if (avail_out = 0)};

      {Decompress and put in output}
      InflateRet := inflate(StreamRec, 0);
      if (InflateRet = Z_STREAM_END) or (InflateRet = 0) then
      begin
        {Reallocates output buffer}
        inc(OutputSize, total_out);
        if Output = nil then
          GetMem(Output, OutputSize) else ReallocMem(Output, OutputSize);
        {Copies the new data}
        memcpy({$IFDEF FPC}pointer{$ELSE}Ptr{$ENDIF}(Longint(Output) + OutputSize - total_out),
          @Buffer, total_out);
      end {if (InflateRet = Z_STREAM_END) or (InflateRet = 0)}
      {Now tests for errors}
      else if InflateRet < 0 then
      begin
        Result := false;
        ErrorOutput := StreamRec.msg;
        InflateEnd(StreamRec);
        Exit;
      end {if InflateRet < 0}
    until InflateRet = Z_STREAM_END;

    {Terminates decompression}
    InflateEnd(StreamRec);
  end {with StreamRec}

end;

{Compresses ZLIB into a memory address}

//==============================================================================
//
// CompressZLIB
//
//==============================================================================
function CompressZLIB(Input: Pointer; InputSize, CompressionLevel: Integer;
  var Output: Pointer; var OutputSize: Integer;
  var ErrorOutput: String): Boolean;
var
  StreamRec: TZStreamRec;
  Buffer: array[Byte] of Byte;
  DeflateRet: Integer;
begin
  with StreamRec do
  begin
    Result := true; {By default returns TRUE as everything might have gone ok}
    OutputSize := 0; {Initialize}
    {Prepares the data to compress}
    FillChar(StreamRec, SizeOf(TZStreamRec), #0);
    DeflateInit_(StreamRec, CompressionLevel,zlib_version, SizeOf(TZStreamRec));

    next_in := Input;
    avail_in := InputSize;

    while avail_in > 0 do
    begin
      {When it needs new buffer to stores the compressed data}
      if avail_out = 0 then
      begin
        {Restore buffer}
        next_out := @Buffer;
        avail_out := SizeOf(Buffer);
      end {if avail_out = 0};

      {Compresses}
      DeflateRet := deflate(StreamRec, Z_FINISH);

      if (DeflateRet = Z_STREAM_END) or (DeflateRet = 0) then
      begin
        {Updates the output memory}
        inc(OutputSize, total_out);
        if Output = nil then
          GetMem(Output, OutputSize)
        else
          ReallocMem(Output, OutputSize);

        {Copies the new data}
        memcpy({$IFDEF FPC}pointer{$ELSE}Ptr{$ENDIF}(Longint(Output) + OutputSize - total_out),
          @Buffer, total_out);
      end {if (InflateRet = Z_STREAM_END) or (InflateRet = 0)}
      {Now tests for errors}
      else if DeflateRet < 0 then
      begin
        Result := false;
        ErrorOutput := StreamRec.msg;
        DeflateEnd(StreamRec);
        Exit;
      end {if InflateRet < 0}

    end {while avail_in > 0};

    {Finishes compressing}
    DeflateEnd(StreamRec);
  end {with StreamRec}

end;

{TPngPointerList implementation}

{Object being created}

//==============================================================================
//
// TPngPointerList.Create
//
//==============================================================================
constructor TPngPointerList.Create(AOwner: TPNGObject);
begin
  inherited Create; {Let ancestor work}
  {Holds owner}
  fOwner := AOwner;
  {Memory pointer not being used yet}
  fMemory := nil;
  {No items yet}
  fCount := 0;
end;

{Removes value from the list}

//==============================================================================
//
// TPngPointerList.Remove
//
//==============================================================================
function TPngPointerList.Remove(Value: Pointer): Pointer;
var
  I, Position: Integer;
begin
  {Gets item position}
  Position := -1;
  for I := 0 to Count - 1 do
    if Value = Item[I] then Position := I;
  {In case a match was found}
  if Position >= 0 then
  begin
    Result := Item[Position]; {Returns pointer}
    {Remove item and move memory}
    dec(fCount);
    if Position < Integer(FCount) then
      System.Move(fMemory^[Position + 1], fMemory^[Position],
      (Integer(fCount) - Position) * SizeOf(Pointer));
  end {if Position >= 0}
  else
    Result := nil
end;

{Add a new value in the list}

//==============================================================================
//
// TPngPointerList.Add
//
//==============================================================================
procedure TPngPointerList.Add(Value: Pointer);
begin
  Count := Count + 1;
  Item[Count - 1] := Value;
end;

{Object being destroyed}

//==============================================================================
//
// TPngPointerList.Destroy
//
//==============================================================================
destructor TPngPointerList.Destroy;
begin
  {Release memory if needed}
  if fMemory <> nil then
    FreeMem(fMemory, fCount * SizeOf(Pointer));

  {Free things}
  inherited Destroy;
end;

{Returns one item from the list}

//==============================================================================
//
// TPngPointerList.GetItem
//
//==============================================================================
function TPngPointerList.GetItem(Index: Cardinal): Pointer;
begin
  if Index <= Count - 1 then
    Result := fMemory[Index]
  else
    {In case it's out of bounds}
    Result := nil;
end;

{Inserts a new item in the list}

//==============================================================================
//
// TPngPointerList.Insert
//
//==============================================================================
procedure TPngPointerList.Insert(Value: Pointer; Position: Cardinal);
begin
  if (Position < Count) or (Count = 0) then
  begin
    {Increase item count}
    SetSize(Count + 1);
    {Move other pointers}
    if Position < Count then
      System.Move(fMemory^[Position], fMemory^[Position + 1],
        (Count - Position - 1) * SizeOf(Pointer));
    {Sets item}
    Item[Position] := Value;
  end;
end;

{Sets one item from the list}

//==============================================================================
//
// TPngPointerList.SetItem
//
//==============================================================================
procedure TPngPointerList.SetItem(Index: Cardinal; const Value: Pointer);
begin
  {If index is in bounds, set value}
  if Index <= Count - 1 then
    fMemory[Index] := Value
end;

{This method resizes the list}

//==============================================================================
//
// TPngPointerList.SetSize
//
//==============================================================================
procedure TPngPointerList.SetSize(const Size: Cardinal);
begin
  {Sets the size}
  if (fMemory = nil) and (Size > 0) then
    GetMem(fMemory, Size * SizeOf(Pointer))
  else
    if Size > 0 then  {Only realloc if the new size is greater than 0}
      ReallocMem(fMemory, Size * SizeOf(Pointer))
    else
    {In case user is resize to 0 items}
    begin
      FreeMem(fMemory);
      fMemory := nil;
    end;
  {Update count}
  fCount := Size;
end;

{TPNGList implementation}

{Finds the first chunk of this class}

//==============================================================================
//
// TPNGList.FindChunk
//
//==============================================================================
function TPNGList.FindChunk(ChunkClass: TChunkClass): TChunk;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Item[i] is ChunkClass then
    begin
      Result := Item[i];
      Break
    end
end;

{Removes an item}

//==============================================================================
//
// TPNGList.RemoveChunk
//
//==============================================================================
procedure TPNGList.RemoveChunk(Chunk: TChunk);
begin
  Remove(Chunk);
  Chunk.Free
end;

{Add a new item}

//==============================================================================
//
// TPNGList.Add
//
//==============================================================================
function TPNGList.Add(ChunkClass: TChunkClass): TChunk;
var
  IHDR: TChunkIHDR;
  IEND: TChunkIEND;

  IDAT: TChunkIDAT;
  PLTE: TChunkPLTE;
begin
  Result := nil; {Default Result}
  {Adding these is not allowed}
  if ((ChunkClass = TChunkIHDR) or (ChunkClass = TChunkIDAT) or
    (ChunkClass = TChunkPLTE) or (ChunkClass = TChunkIEND)) and not
    (Owner.BeingCreated) then
    fOwner.RaiseError(EPNGCannotAddChunkText)
  {Two of these is not allowed}
  else if ((ChunkClass = TChunkgAMA) and (ItemFromClass(TChunkgAMA) <> nil)) or
     ((ChunkClass = TChunktRNS) and (ItemFromClass(TChunktRNS) <> nil)) or
     ((ChunkClass = TChunkpHYs) and (ItemFromClass(TChunkpHYs) <> nil)) then
    fOwner.RaiseError(EPNGCannotAddChunkText)
  {There must have an IEND and IHDR chunk}
  else if ((ItemFromClass(TChunkIEND) = nil) or
    (ItemFromClass(TChunkIHDR) = nil)) and not Owner.BeingCreated then
    fOwner.RaiseError(EPNGCannotAddInvalidImageText)
  else
  begin
    {Get common chunks}
    IHDR := ItemFromClass(TChunkIHDR) as TChunkIHDR;
    IEND := ItemFromClass(TChunkIEND) as TChunkIEND;
    {Create new chunk}
    Result := ChunkClass.Create(Owner);
    {Add to the list}
    if (ChunkClass = TChunkgAMA) or (ChunkClass = TChunkpHYs) or
      (ChunkClass = TChunkPLTE) then
      Insert(Result, IHDR.Index + 1)
    {Header and end}
    else if (ChunkClass = TChunkIEND) then
      Insert(Result, Count)
    else if (ChunkClass = TChunkIHDR) then
      Insert(Result, 0)
    {Transparency chunk (fix by Ian Boyd)}
    else if (ChunkClass = TChunktRNS) then
    begin
      {Transparecy chunk must be after PLTE; before IDAT}
      IDAT := ItemFromClass(TChunkIDAT) as TChunkIDAT;
      PLTE := ItemFromClass(TChunkPLTE) as TChunkPLTE;

      if Assigned(PLTE) then
        Insert(Result, PLTE.Index + 1)
      else if Assigned(IDAT) then
        Insert(Result, IDAT.Index)
      else
        Insert(Result, IHDR.Index + 1)
    end
    else {All other chunks}
      Insert(Result, IEND.Index);
  end {if}
end;

{Returns item from the list}

//==============================================================================
//
// TPNGList.GetItem
//
//==============================================================================
function TPNGList.GetItem(Index: Cardinal): TChunk;
begin
  Result := inherited GetItem(Index);
end;

{Returns first item from the list using the class from parameter}

//==============================================================================
//
// TPNGList.ItemFromClass
//
//==============================================================================
function TPNGList.ItemFromClass(ChunkClass: TChunkClass): TChunk;
var
  i: Integer;
begin
  Result := nil; {Initial Result}
  for i := 0 to Count - 1 do
    {Test if this item has the same class}
    if Item[i] is ChunkClass then
    begin
      {Returns this item and Exit}
      Result := Item[i];
      break;
    end {if}
end;

{TChunk implementation}

{Resizes the data}

//==============================================================================
//
// TChunk.ResizeData
//
//==============================================================================
procedure TChunk.ResizeData(const NewSize: Cardinal);
begin
  fDataSize := NewSize;
  ReallocMem(fData, NewSize + 1);
end;

{Returns index from list}

//==============================================================================
//
// TChunk.GetIndex
//
//==============================================================================
function TChunk.GetIndex: Integer;
var
  i: Integer;
begin
  Result := -1; {Avoiding warnings}
  {Searches in the list}
  for i := 0 to Owner.Chunks.Count - 1 do
    if Owner.Chunks.Item[i] = Self then
    begin
      {Found match}
      Result := i;
      Exit;
    end {for i}
end;

{Returns pointer to the TChunkIHDR}

//==============================================================================
//
// TChunk.GetHeader
//
//==============================================================================
function TChunk.GetHeader: TChunkIHDR;
begin
  Result := Owner.Chunks.Item[0] as TChunkIHDR;
end;

{Assigns from another TChunk}

//==============================================================================
//
// TChunk.Assign
//
//==============================================================================
procedure TChunk.Assign(Source: TChunk);
begin
  {Copy properties}
  fName := Source.fName;
  {Set data size and realloc}
  ResizeData(Source.fDataSize);

  {Copy data (if there's any)}
  if fDataSize > 0 then
    memcpy(fData, Source.fData, fDataSize);
end;

{Chunk being created}

//==============================================================================
//
// TChunk.Create
//
//==============================================================================
constructor TChunk.Create(Owner: TPngObject);
var
  ChunkName: String;
begin
  {Ancestor create}
  inherited Create;

  {If it's a registered class, set the chunk name based on the class}
  {name. for instance, if the class name is TChunkgAMA, the GAMA part}
  {will become the chunk name}
  ChunkName := Copy(ClassName, Length('TChunk') + 1, Length(ClassName));
  if Length(ChunkName) = 4 then
    memcpy(@fName[0], @ChunkName[1], 4);

  {Initialize data holder}
  GetMem(fData, 1);
  fDataSize := 0;
  {Record owner}
  fOwner := Owner;
end;

{Chunk being destroyed}

//==============================================================================
//
// TChunk.Destroy
//
//==============================================================================
destructor TChunk.Destroy;
begin
  {Free data holder}
  FreeMem(fData, fDataSize + 1);
  {Let ancestor destroy}
  inherited Destroy;
end;

{Returns the chunk name 1}

//==============================================================================
//
// TChunk.GetChunkName
//
//==============================================================================
function TChunk.GetChunkName: String;
begin
  Result := fName
end;

{Returns the chunk name 2}
class function TChunk.GetName: String;
begin
  {for avoid writing GetName for each TChunk descendent, by default for}
  {classes which don't declare GetName, it will look for the class name}
  {to extract the chunk kind. Example, if the class name is TChunkIEND }
  {this method extracts and returns IEND}
  Result := Copy(ClassName, Length('TChunk') + 1, Length(ClassName));
end;

{Saves the data to the stream}

//==============================================================================
//
// TChunk.SaveData
//
//==============================================================================
function TChunk.SaveData(Stream: TDStream): Boolean;
var
  ChunkSize, ChunkCRC: Cardinal;
begin
  {First, write the size for the following data in the chunk}
  ChunkSize := ByteSwap(DataSize);
  Stream.Write(ChunkSize, 4);
  {The chunk name}
  Stream.Write(fName, 4);
  {If there is data for the chunk, write it}
  if DataSize > 0 then
    Stream.Write(Data^, DataSize);
  {Calculates and write CRC}
  ChunkCRC := update_crc($ffffffff, @fName[0], 4);
  ChunkCRC := Byteswap(update_crc(ChunkCRC, Data, DataSize) xor $ffffffff);
  Stream.Write(ChunkCRC, 4);

  {Returns that everything went ok}
  Result := true;
end;

{Saves the chunk to the stream}

//==============================================================================
//
// TChunk.SaveToStream
//
//==============================================================================
function TChunk.SaveToStream(Stream: TDStream): Boolean;
begin
  Result := SaveData(Stream)
end;

{Loads the chunk from a stream}

//==============================================================================
//
// TChunk.LoadFromStream
//
//==============================================================================
function TChunk.LoadFromStream(Stream: TDStream; const ChunkName: TChunkName;
  Size: Integer): Boolean;
var
  CheckCRC: Cardinal;
  {$IFDEF CheckCRC}RightCRC: Cardinal;{$ENDIF}
begin
  {Copies data from source}
  ResizeData(Size);
  if Size > 0 then
    Stream.Read(fData^, Size);
  {Reads CRC}
  Stream.Read(CheckCRC, 4);
  CheckCrc := ByteSwap(CheckCRC);

  if ChunkName = 'grAb' then
  begin
    Result := true;
    exit;
  end;

  {Check if crc readed is valid}
  {$IFDEF CheckCRC}
    RightCRC := update_crc($ffffffff, @ChunkName[0], 4);
    RightCRC := update_crc(RightCRC, fData, Size) xor $ffffffff;
    Result := RightCRC = CheckCrc;

    {Handle CRC error}
    if not Result then
    begin
      {In case it coult not load chunk}
      Owner.RaiseError(EPngInvalidCRCText);
      Exit;
    end
  {$ELSE}Result := true; {$ENDIF}

end;

{TChunktIME implementation}

{Chunk being loaded from a stream}

//==============================================================================
//
// TChunktIME.LoadFromStream
//
//==============================================================================
function TChunktIME.LoadFromStream(Stream: TDStream;
  const ChunkName: TChunkName; Size: Integer): Boolean;
begin
  {Let ancestor load the data}
  Result := inherited LoadFromStream(Stream, ChunkName, Size);
  if not Result or (Size <> 7) then
    Exit; {Size must be 7}

  {Reads data}
  fYear := ((PByte(Longint(Data) )^) * 256) + (PByte(Longint(Data) + 1)^);
  fMonth := PByte(Longint(Data) + 2)^;
  fDay := PByte(Longint(Data) + 3)^;
  fHour := PByte(Longint(Data) + 4)^;
  fMinute := PByte(Longint(Data) + 5)^;
  fSecond := PByte(Longint(Data) + 6)^;
end;

{Assigns from another TChunk}

//==============================================================================
//
// TChunktIME.Assign
//
//==============================================================================
procedure TChunktIME.Assign(Source: TChunk);
begin
  fYear := TChunktIME(Source).fYear;
  fMonth := TChunktIME(Source).fMonth;
  fDay := TChunktIME(Source).fDay;
  fHour := TChunktIME(Source).fHour;
  fMinute := TChunktIME(Source).fMinute;
  fSecond := TChunktIME(Source).fSecond;
end;

{Saving the chunk to a stream}

//==============================================================================
//
// TChunktIME.SaveToStream
//
//==============================================================================
function TChunktIME.SaveToStream(Stream: TDStream): Boolean;
begin
  {Update data}
  ResizeData(7);  {Make sure the size is 7}
  PWord(Data)^ := ByteSwap16(Year);
  PByte(Longint(Data) + 2)^ := Month;
  PByte(Longint(Data) + 3)^ := Day;
  PByte(Longint(Data) + 4)^ := Hour;
  PByte(Longint(Data) + 5)^ := Minute;
  PByte(Longint(Data) + 6)^ := Second;

  {Let inherited save data}
  Result := inherited SaveToStream(Stream);
end;

{TChunkztXt implementation}

//==============================================================================
//
// ischarbuffer
//
//==============================================================================
function ischarbuffer(const p: PChar; const len: integer): boolean;
var
  i: integer;
  p1: PChar;
  c: char;
begin
  p1 := p;
  for i := 0 to len - 1 do
  begin
    c := p1^;
    if Pos(c, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
              'abcdefghijklmnopqrstuvwxyz' +
              '0123456789' +
              '~`!@#$%^&*()_-+=''"\|;:?/.>,<* []{}' + #13#10#7#10) = 0 then
    begin
      Result := False;
      Exit;
    end;
    inc(p1);
  end;
  Result := True;
end;

{Loading the chunk from a stream}

//==============================================================================
//
// TChunkzTXt.LoadFromStream
//
//==============================================================================
function TChunkzTXt.LoadFromStream(Stream: TDStream;
  const ChunkName: TChunkName; Size: Integer): Boolean;
var
  ErrorOutput: String;
  CompressionMethod: Byte;
  Output: Pointer;
  OutputSize: Integer;
  i, len: integer;
  p1: PChar;
  c: char;
begin
  {Load data from stream and validate}
  Result := inherited LoadFromStream(Stream, ChunkName, Size);
  if not Result or (Size < 4) then
    Exit;
  fKeyword := PChar(Data);  {Get keyword and compression method bellow}
  if Longint(fKeyword) = 0 then
    CompressionMethod := PByte(Data)^
  else
    CompressionMethod := PByte(Longint(fKeyword) + Length(fKeyword))^;
  fText := '';

  {In case the compression is 0 (only one accepted by specs), reads it}
  if CompressionMethod = 0 then
  begin
    // 20190907 JVAL Avoid problems with png
    p1 := PChar(Longint(Data) + Length(fKeyword) + 2);
    len := Size - Length(fKeyword) - 2;
    if ischarbuffer(p1, len) then
    begin
      for i := 0 to len - 1 do
      begin
        c := p1^;
        fText := fText + c;
        inc(p1);
      end;
    end
    else
    begin
      Output := nil;

      if DecompressZLIB(p1, len, Output, OutputSize, ErrorOutput) then
      begin
        SetLength(fText, OutputSize);
        memcpy(@fText[1], Output, OutputSize);
      end {if DecompressZLIB(...};
      FreeMem(Output);
    end;
  end {if CompressionMethod = 0}

end;

{Saving the chunk to a stream}

//==============================================================================
//
// TChunkztXt.SaveToStream
//
//==============================================================================
function TChunkztXt.SaveToStream(Stream: TDStream): Boolean;
var
  Output: Pointer;
  OutputSize: Integer;
  ErrorOutput: String;
begin
  Output := nil; {Initializes output}
  if fText = '' then
    fText := ' ';

  {Compresses the data}
  if CompressZLIB(@fText[1], Length(fText), Owner.CompressionLevel, Output,
    OutputSize, ErrorOutput) then
  begin
    {Size is length from keyword, plus a null character to divide}
    {plus the compression method, plus the length of the text (zlib compressed)}
    ResizeData(Length(fKeyword) + 2 + OutputSize);

    FillChar(Data^, DataSize, #0);
    {Copies the keyword data}
    if Keyword <> '' then
      memcpy(Data, @fKeyword[1], Length(Keyword));
    {Compression method 0 (inflate/deflate)}
    PByte({$IFDEF FPC}integer{$ELSE}Ptr{$ENDIF}(Longint(Data) + Length(Keyword) + 1))^ := 0;
    if OutputSize > 0 then
      memcpy({$IFDEF FPC}pointer{$ELSE}Ptr{$ENDIF}(Longint(Data) + Length(Keyword) + 2), Output, OutputSize);

    {Let ancestor calculate crc and save}
    Result := SaveData(Stream);
  end {if CompressZLIB(...} else
    Result := false;

  {Frees output}
  if Output <> nil then
    FreeMem(Output)
end;

{TChunktEXt implementation}

{Assigns from another text chunk}

//==============================================================================
//
// TChunktEXt.Assign
//
//==============================================================================
procedure TChunktEXt.Assign(Source: TChunk);
begin
  fKeyword := TChunktEXt(Source).fKeyword;
  fText := TChunktEXt(Source).fText;
end;

{Loading the chunk from a stream}

//==============================================================================
//
// TChunktEXt.LoadFromStream
//
//==============================================================================
function TChunktEXt.LoadFromStream(Stream: TDStream;
  const ChunkName: TChunkName; Size: Integer): Boolean;
begin
  {Load data from stream and validate}
  Result := inherited LoadFromStream(Stream, ChunkName, Size);
  if not Result or (Size < 3) then
    Exit;
  {Get text}
  fKeyword := PChar(Data);
  SetLength(fText, Size - Length(fKeyword) - 1);
  memcpy(@fText[1], {$IFDEF FPC}pointer{$ELSE}Ptr{$ENDIF}(Longint(Data) + Length(fKeyword) + 1),
    Length(fText));
end;

{Saving the chunk to a stream}

//==============================================================================
//
// TChunktEXt.SaveToStream
//
//==============================================================================
function TChunktEXt.SaveToStream(Stream: TDStream): Boolean;
begin
  {Size is length from keyword, plus a null character to divide}
  {plus the length of the text}
  ResizeData(Length(fKeyword) + 1 + Length(fText));
  FillChar(Data^, DataSize, #0);
  {Copy data}
  if Keyword <> '' then
    memcpy(Data, @fKeyword[1], Length(Keyword));
  if Text <> '' then
    memcpy({$IFDEF FPC}pointer{$ELSE}Ptr{$ENDIF}(Longint(Data) + Length(Keyword) + 1), @fText[1],
      Length(Text));
  {Let ancestor calculate crc and save}
  Result := inherited SaveToStream(Stream);
end;

{TChunkIHDR implementation}

{Chunk being created}

//==============================================================================
//
// TChunkIHDR.Create
//
//==============================================================================
constructor TChunkIHDR.Create(Owner: TPngObject);
begin
  {Prepare pointers}
  ImageHandle := 0;
  ImagePalette := 0;
  ImageDC := 0;

  {Call inherited}
  inherited Create(Owner);
end;

{Chunk being destroyed}

//==============================================================================
//
// TChunkIHDR.Destroy
//
//==============================================================================
destructor TChunkIHDR.Destroy;
begin
  {Free memory}
  FreeImageData();

  {Calls TChunk destroy}
  inherited Destroy;
end;

{Copies the palette}

//==============================================================================
//
// CopyPalette
//
//==============================================================================
procedure CopyPalette(Source: HPALETTE; Destination: HPALETTE);
var
  PaletteSize: Integer;
  Entries: array[Byte] of TPaletteEntry;
begin
  PaletteSize := 0;
  if GetObject(Source, SizeOf(PaletteSize), @PaletteSize) = 0 then
    Exit;
  if PaletteSize = 0 then
    Exit;
  ResizePalette(Destination, PaletteSize);
  GetPaletteEntries(Source, 0, PaletteSize, Entries);
  SetPaletteEntries(Destination, 0, PaletteSize, Entries);
end;

{Assigns from another IHDR chunk}

//==============================================================================
//
// TChunkIHDR.Assign
//
//==============================================================================
procedure TChunkIHDR.Assign(Source: TChunk);
begin
  {Copy the IHDR data}
  if Source is TChunkIHDR then
  begin
    {Copy IHDR values}
    IHDRData := TChunkIHDR(Source).IHDRData;

    {Prepare to hold data by filling BitmapInfo structure and}
    {resizing ImageData and ImageAlpha memory allocations}
    PrepareImageData();

    {Copy image data}
    memcpy(ImageData, TChunkIHDR(Source).ImageData,
      BytesPerRow * Integer(Height));
    memcpy(ImageAlpha, TChunkIHDR(Source).ImageAlpha,
      Integer(Width) * Integer(Height));

    {Copy palette colors}
    BitmapInfo.bmiColors := TChunkIHDR(Source).BitmapInfo.bmiColors;
    {Copy palette also}
    CopyPalette(TChunkIHDR(Source).ImagePalette, ImagePalette);
  end
  else
    Owner.RaiseError(EPNGCannotAssignChunkText);
end;

{Release allocated image data}

//==============================================================================
//
// TChunkIHDR.FreeImageData
//
//==============================================================================
procedure TChunkIHDR.FreeImageData;
begin
  {Free old image data}
  if ImageHandle <> 0 then
    DeleteObject(ImageHandle);
  if ImageDC <> 0 then
    DeleteDC(ImageDC);
  if ImageAlpha <> nil then
    FreeMem(ImageAlpha);
  if ImagePalette <> 0 then
    DeleteObject(ImagePalette);
  {$IFDEF Store16bits}
  if ExtraImageData <> nil then
    FreeMem(ExtraImageData);
  {$ENDIF}
  ImageHandle := 0;
  ImageDC := 0;
  ImageAlpha := nil;
  ImageData := nil;
  ImagePalette := 0;
  ExtraImageData := nil;
end;

{Chunk being loaded from a stream}

//==============================================================================
//
// TChunkIHDR.LoadFromStream
//
//==============================================================================
function TChunkIHDR.LoadFromStream(Stream: TDStream; const ChunkName: TChunkName;
  Size: Integer): Boolean;
begin
  {Let TChunk load it}
  Result := inherited LoadFromStream(Stream, ChunkName, Size);
  if not Result then
    Exit;

  {Now check values}
  {Note: It's recommended by png specification to make sure that the size}
  {must be 13 bytes to be valid, but some images with 14 bytes were found}
  {which could be loaded by internet explorer and other tools}
  if fDataSize < SizeOf(TIHdrData) then
  begin
    {Ihdr must always have at least 13 bytes}
    Result := false;
    Owner.RaiseError(EPNGInvalidIHDRText);
    Exit;
  end;

  {Everything ok, reads IHDR}
  IHDRData := pIHDRData(fData)^;
  IHDRData.Width := ByteSwap(IHDRData.Width);
  IHDRData.Height := ByteSwap(IHDRData.Height);

  {The width and height must not be larger than 65535 pixels}
  if (IHDRData.Width > High(Word)) or (IHDRData.Height > High(Word)) then
  begin
    Result := false;
    Owner.RaiseError(EPNGSizeExceedsText);
    Exit;
  end {if IHDRData.Width > High(Word)};
  {Compression method must be 0 (inflate/deflate)}
  if IHDRData.CompressionMethod <> 0 then
  begin
    Result := false;
    Owner.RaiseError(EPNGUnknownCompressionText);
    Exit;
  end;
  {Interlace must be either 0 (none) or 7 (adam7)}
  if (IHDRData.InterlaceMethod <> 0) and (IHDRData.InterlaceMethod <> 1) then
  begin
    Result := false;
    Owner.RaiseError(EPNGUnknownInterlaceText);
    Exit;
  end;

  {Updates owner properties}
  Owner.InterlaceMethod := TInterlaceMethod(IHDRData.InterlaceMethod);

  {Prepares data to hold image}
  PrepareImageData();
end;

{Saving the IHDR chunk to a stream}

//==============================================================================
//
// TChunkIHDR.SaveToStream
//
//==============================================================================
function TChunkIHDR.SaveToStream(Stream: TDStream): Boolean;
begin
  {Ignore 2 bits images}
  if BitDepth = 2 then BitDepth := 4;

  {It needs to do is update the data with the IHDR data}
  {structure containing the write values}
  ResizeData(SizeOf(TIHDRData));
  pIHDRData(fData)^ := IHDRData;
  {..byteswap 4 byte types}
  pIHDRData(fData)^.Width := ByteSwap(pIHDRData(fData)^.Width);
  pIHDRData(fData)^.Height := ByteSwap(pIHDRData(fData)^.Height);
  {..update interlace method}
  pIHDRData(fData)^.InterlaceMethod := Byte(Owner.InterlaceMethod);
  {..and then let the ancestor SaveToStream do the hard work}
  Result := inherited SaveToStream(Stream);
end;

{Creates a grayscale palette}

//==============================================================================
//
// TChunkIHDR.CreateGrayscalePalette
//
//==============================================================================
function TChunkIHDR.CreateGrayscalePalette(Bitdepth: Integer): HPalette;
var
  j: Integer;
  palEntries: TMaxLogPalette;
begin
  {Prepares and fills the strucutre}
  if Bitdepth = 16 then Bitdepth := 8;
  ZeroMemory(@palEntries, SizeOf(palEntries));
  palEntries.palVersion := $300;
  palEntries.palNumEntries := 1 shl Bitdepth;
  {Fill it with grayscale colors}
  for j := 0 to palEntries.palNumEntries - 1 do
  begin
    palEntries.palPalEntry[j].peRed :=
      fOwner.GammaTable[MulDiv(j, 255, palEntries.palNumEntries - 1)];
    palEntries.palPalEntry[j].peGreen := palEntries.palPalEntry[j].peRed;
    palEntries.palPalEntry[j].peBlue := palEntries.palPalEntry[j].peRed;
  end;
  {Creates and returns the palette}
  Result := CreatePalette(pLogPalette(@palEntries)^);
end;

{Copies the palette to the Device Independent bitmap header}

//==============================================================================
//
// TChunkIHDR.PaletteToDIB
//
//==============================================================================
procedure TChunkIHDR.PaletteToDIB(Palette: HPalette);
var
  j: Integer;
  palEntries: TMaxLogPalette;
begin
  {Copy colors}
  ZeroMemory(@palEntries, SizeOf(palEntries));
  BitmapInfo.bmiHeader.biClrUsed := GetPaletteEntries(Palette, 0, 256, palEntries.palPalEntry[0]);
  for j := 0 to BitmapInfo.bmiHeader.biClrUsed - 1 do
  begin
    BitmapInfo.bmiColors[j].rgbBlue := palEntries.palPalEntry[j].peBlue;
    BitmapInfo.bmiColors[j].rgbRed := palEntries.palPalEntry[j].peRed;
    BitmapInfo.bmiColors[j].rgbGreen := palEntries.palPalEntry[j].peGreen;
  end;
end;

{Resizes the image data to fill the color type, bit depth, }
{width and height parameters}

//==============================================================================
//
// TChunkIHDR.PrepareImageData
//
//==============================================================================
procedure TChunkIHDR.PrepareImageData();
  {Set the bitmap info}
  procedure SetInfo(const Bitdepth: Integer; const Palette: Boolean);
  begin

    {Copy if the bitmap contain palette entries}
    HasPalette := Palette;
    {Fill the strucutre}
    with BitmapInfo.bmiHeader do
    begin
      biSize := SizeOf(TBitmapInfoHeader);
      biHeight := Height;
      biWidth := Width;
      biPlanes := 1;
      biBitCount := BitDepth;
      biCompression := BI_RGB;
    end {with BitmapInfo.bmiHeader}
  end;
begin
  {Prepare bitmap info header}
  FillChar(BitmapInfo, SizeOf(TMaxBitmapInfo), #0);
  {Release old image data}
  FreeImageData();

  {Obtain number of bits for each pixel}
  case ColorType of
    COLOR_GRAYSCALE, COLOR_PALETTE, COLOR_GRAYSCALEALPHA:
      case BitDepth of
        {These are supported by windows}
        1, 4, 8: SetInfo(BitDepth, true);
        {2 bits for each pixel is not supported by windows bitmap}
        2: SetInfo(4, true);
        {Also 16 bits (2 bytes) for each pixel is not supported}
        {and should be transormed into a 8 bit grayscale}
        16: SetInfo(8, true);
      end;
    {Only 1 byte (8 bits) is supported}
    COLOR_RGB, COLOR_RGBALPHA:  SetInfo(24, false);
  end {case ColorType};
  {Number of bytes for each scanline}
  BytesPerRow := (((BitmapInfo.bmiHeader.biBitCount * Width) + 31)
    and not 31) div 8;

  {Build array for alpha information, if necessary}
  if (ColorType = COLOR_RGBALPHA) or (ColorType = COLOR_GRAYSCALEALPHA) then
  begin
    GetMem(ImageAlpha, Integer(Width) * Integer(Height));
    ZeroMemory(ImageAlpha, Integer(Width) * Integer(Height));
  end;

  {Build array for extra byte information}
  {$IFDEF Store16bits}
  if (BitDepth = 16) then
  begin
    GetMem(ExtraImageData, BytesPerRow * Integer(Height));
    ZeroMemory(ExtraImageData, BytesPerRow * Integer(Height));
  end;
  {$ENDIF}

  {Creates the image to hold the data, CreateDIBSection does a better}
  {work in allocating necessary memory}
  ImageDC := CreateCompatibleDC(0);

  {In case it is a palette image, create the palette}
  if HasPalette then
  begin
    {Create a standard palette}
    if ColorType = COLOR_PALETTE then
      ImagePalette := CreateHalfTonePalette(ImageDC)
    else
      ImagePalette := CreateGrayscalePalette(Bitdepth);
    ResizePalette(ImagePalette, 1 shl BitmapInfo.bmiHeader.biBitCount);
    BitmapInfo.bmiHeader.biClrUsed := 1 shl BitmapInfo.bmiHeader.biBitCount;
    SelectPalette(ImageDC, ImagePalette, false);
    RealizePalette(ImageDC);
    PaletteTODIB(ImagePalette);
  end;

  {Create the device independent bitmap}
  ImageHandle := CreateDIBSection(ImageDC, pBitmapInfo(@BitmapInfo)^,
    DIB_RGB_COLORS, ImageData, 0, 0);
  SelectObject(ImageDC, ImageHandle);

  {Build array and allocate bytes for each row}
  ZeroMemory(ImageData, BytesPerRow * Integer(Height));
end;

{TChunktRNS implementation}

//==============================================================================
//
// CompareMem
//
//==============================================================================
function CompareMem(P1, P2: PByte; const Size: Integer): Boolean;
var i: Integer;
begin
  Result := true;
  for i := 1 to Size do
  begin
    if P1^ <> P2^ then
    begin
      Result := False;
      Exit;
    end;
    inc(P1);
    inc(P2);
  end {for i}
end;

{Sets the transpararent color}

//==============================================================================
//
// TChunktRNS.SetTransparentColor
//
//==============================================================================
procedure TChunktRNS.SetTransparentColor(const Value: ColorRef);
var
  i: Byte;
  LookColor: TRGBQuad;
begin
  {Clears the palette values}
  ZeroMemory(@PaletteValues, SizeOf(PaletteValues));
  {Sets that it uses bit transparency}
  fBitTransparency := true;

  {Depends on the color type}
  with Header do
    case ColorType of
      COLOR_GRAYSCALE:
        begin
          Self.ResizeData(2);
          PWord(@PaletteValues[0])^ := ByteSwap16(GetRValue(Value));
        end;
      COLOR_RGB:
        begin
          Self.ResizeData(6);
          PWord(@PaletteValues[0])^ := ByteSwap16(GetRValue(Value));
          PWord(@PaletteValues[2])^ := ByteSwap16(GetGValue(Value));
          PWord(@PaletteValues[4])^ := ByteSwap16(GetBValue(Value));
        end;
      COLOR_PALETTE:
        begin
          {Creates a RGBQuad to search for the color}
          LookColor.rgbRed := GetRValue(Value);
          LookColor.rgbGreen := GetGValue(Value);
          LookColor.rgbBlue := GetBValue(Value);
          {Look in the table for the entry}
          for i := 0 to BitmapInfo.bmiHeader.biClrUsed - 1 do
            if CompareMem(@BitmapInfo.bmiColors[i], @LookColor, 3) then
              Break;
          {Fill the transparency table}
          FillChar(PaletteValues, i, 255);
          Self.ResizeData(i + 1)

        end
    end {case / with};

end;

{Returns the transparent color for the image}

//==============================================================================
//
// TChunktRNS.GetTransparentColor
//
//==============================================================================
function TChunktRNS.GetTransparentColor: ColorRef;
var
  PaletteChunk: TChunkPLTE;
  i: Integer;
  Value: Byte;
begin
  Result := 0; {Default: Unknown transparent color}

  {Depends on the color type}
  with Header do
    case ColorType of
      COLOR_GRAYSCALE:
        begin
          Value := BitmapInfo.bmiColors[PaletteValues[1]].rgbRed;
          Result := RGB(Value, Value, Value);
        end;
      COLOR_RGB:
        Result := RGB(fOwner.GammaTable[PaletteValues[1]],
          fOwner.GammaTable[PaletteValues[3]],
          fOwner.GammaTable[PaletteValues[5]]);
      COLOR_PALETTE:
        begin
          {Obtains the palette chunk}
          PaletteChunk := Owner.Chunks.ItemFromClass(TChunkPLTE) as TChunkPLTE;

          {Looks for an entry with 0 transparency meaning that it is the}
          {full transparent entry}
          for i := 0 to Self.DataSize - 1 do
            if PaletteValues[i] = 0 then
              with PaletteChunk.GetPaletteItem(i) do
              begin
                Result := RGB(rgbRed, rgbGreen, rgbBlue);
                break
              end
        end {COLOR_PALETTE}
    end {case Header.ColorType};
end;

{Saving the chunk to a stream}

//==============================================================================
//
// TChunktRNS.SaveToStream
//
//==============================================================================
function TChunktRNS.SaveToStream(Stream: TDStream): Boolean;
begin
  {Copy palette into data buffer}
  if DataSize <= 256 then
    memcpy(fData, @PaletteValues[0], DataSize);

  Result := inherited SaveToStream(Stream);
end;

{Assigns from another chunk}

//==============================================================================
//
// TChunktRNS.Assign
//
//==============================================================================
procedure TChunktRNS.Assign(Source: TChunk);
begin
  memcpy(@PaletteValues[0], @TChunkTrns(Source).PaletteValues[0], 256);
  fBitTransparency := TChunkTrns(Source).fBitTransparency;
  inherited Assign(Source);
end;

{Loads the chunk from a stream}

//==============================================================================
//
// TChunktRNS.LoadFromStream
//
//==============================================================================
function TChunktRNS.LoadFromStream(Stream: TDStream; const ChunkName: TChunkName;
  Size: Integer): Boolean;
var
  i, Differ255: Integer;
begin
  {Let inherited load}
  Result := inherited LoadFromStream(Stream, ChunkName, Size);

  if not Result then
    Exit;

  {Make sure size is correct}
  if Size > 256 then Owner.RaiseError(
    EPNGInvalidPaletteText);

  {The unset items should have value 255}
  FillChar(PaletteValues[0], 256, 255);
  {Copy the other values}
  memcpy(@PaletteValues[0], fData, Size);

  {Create the mask if needed}
  case Header.ColorType of
    {Mask for grayscale and RGB}
    COLOR_RGB, COLOR_GRAYSCALE: fBitTransparency := true;
    COLOR_PALETTE:
    begin
      Differ255 := 0; {Count the entries with a value different from 255}
      {Tests if it uses bit transparency}
      for i := 0 to Size - 1 do
        if PaletteValues[i] <> 255 then
          inc(Differ255);

      {If it has one value different from 255 it is a bit transparency}
      fBitTransparency := (Differ255 = 1);
    end {COLOR_PALETTE}
  end {case Header.ColorType};

end;

{Prepares the image palette}

//==============================================================================
//
// TChunkIDAT.PreparePalette
//
//==============================================================================
procedure TChunkIDAT.PreparePalette;
var
  Entries: Word;
  j: Integer;
  palEntries: TMaxLogPalette;
begin
  {In case the image uses grayscale, build a grayscale palette}
  with Header do
    if (ColorType = COLOR_GRAYSCALE) or (ColorType = COLOR_GRAYSCALEALPHA) then
    begin
      {Calculate total number of palette entries}
      Entries := (1 shl Byte(BitmapInfo.bmiHeader.biBitCount));
      ZeroMemory(@palEntries, SizeOf(palEntries));
      palEntries.palVersion := $300;
      palEntries.palNumEntries := Entries;

      for j := 0 to Entries - 1 do
        with palEntries.palPalEntry[j] do
        begin

          {Calculate each palette entry}
          peRed := fOwner.GammaTable[MulDiv(j, 255, Entries - 1)];
          peGreen := peRed;
          peBlue := peRed;
        end {with BitmapInfo.bmiColors[j]};
        Owner.SetPalette(@palEntries);
    end {if ColorType = COLOR_GRAYSCALE..., with Header}
end;

{Reads from ZLIB}

//==============================================================================
//
// TChunkIDAT.IDATZlibRead
//
//==============================================================================
function TChunkIDAT.IDATZlibRead(var ZLIBStream: TZStreamRec2;
  Buffer: Pointer; Count: Integer; var EndPos: Integer;
  var crcfile: Cardinal): Integer;
var
  Procresult: Integer;
  IDATHeader: array[0..3] of char;
  IDATCRC: Cardinal;
begin
  {Uses internal record pointed by ZLIBStream to gather information}
  with ZLIBStream, ZLIBStream.zlib do
  begin
    {Set the buffer the zlib will read into}
    next_out := Buffer;
    avail_out := Count;

    {Decode until it reach the Count variable}
    while avail_out > 0 do
    begin
      {In case it needs more data and it's in the end of a IDAT chunk,}
      {it means that there are more IDAT chunks}
      if (fStream.Position = EndPos) and (avail_out > 0) and
        (avail_in = 0) then
      begin
        {End this chunk by reading and testing the crc value}
        fStream.Read(IDATCRC, 4);

        {$IFDEF CheckCRC}
          if crcfile xor $ffffffff <> Cardinal(ByteSwap(IDATCRC)) then
          begin
            Result := -1;
            Owner.RaiseError(EPNGInvalidCRCText);
            Exit;
          end;
        {$ENDIF}

        {Start reading the next chunk}
        fStream.Read(EndPos, 4);        {Reads next chunk size}
        fStream.Read(IDATHeader[0], 4); {Next chunk header}
        {It must be a IDAT chunk since image data is required and PNG}
        {specification says that multiple IDAT chunks must be consecutive}
        if IDATHeader <> 'IDAT' then
        begin
          Owner.RaiseError(EPNGMissingMultipleIDATText);
          Result := -1;
          Exit;
        end;

        {Calculate chunk name part of the crc}
        {$IFDEF CheckCRC}
          crcfile := update_crc($ffffffff, @IDATHeader[0], 4);
        {$ENDIF}
        EndPos := fStream.Position + ByteSwap(EndPos);
      end;

      {In case it needs compressed data to read from}
      if avail_in = 0 then
      begin
        {In case it's trying to read more than it is avaliable}
        if fStream.Position + ZLIBAllocate > EndPos then
          avail_in := fStream.Read(Data^, EndPos - fStream.Position)
         else
          avail_in := fStream.Read(Data^, ZLIBAllocate);
        {Update crc}
        {$IFDEF CheckCRC}
          crcfile := update_crc(crcfile, Data, avail_in);
        {$ENDIF}

        {In case there is no more compressed data to read from}
        if avail_in = 0 then
        begin
          Result := Count - avail_out;
          Exit;
        end;

        {Set next buffer to read and record current position}
        next_in := Data;

      end {if avail_in = 0};

      Procresult := inflate(zlib, 0);

      {In case the Result was not sucessfull}
      if (Procresult < 0) then
      begin
        Result := -1;
        Owner.RaiseError(
          EPNGZLIBErrorText + zliberrors[procresult]);
        Exit;
      end;

    end {while avail_out > 0};

  end {with};

  {If everything gone ok, it returns the count bytes}
  Result := Count;
end;

{TChunkIDAT implementation}

const
  {Adam 7 interlacing values}
  RowStart: array[0..6] of Integer = (0, 0, 4, 0, 2, 0, 1);
  ColumnStart: array[0..6] of Integer = (0, 4, 0, 2, 0, 1, 0);
  RowIncrement: array[0..6] of Integer = (8, 8, 8, 4, 4, 2, 2);
  ColumnIncrement: array[0..6] of Integer = (8, 8, 4, 4, 2, 2, 1);

{Copy interlaced images with 1 byte for R, G, B}

//==============================================================================
//
// TChunkIDAT.CopyInterlacedRGB8
//
//==============================================================================
procedure TChunkIDAT.CopyInterlacedRGB8(const Pass: Byte;
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Dest := PChar(Longint(Dest) + Col * 3);
  repeat
    {Copy this row}
    Byte(Dest^) := fOwner.GammaTable[PByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^) := fOwner.GammaTable[PByte(Longint(Src) + 1)^]; inc(Dest);
    Byte(Dest^) := fOwner.GammaTable[PByte(Longint(Src)    )^]; inc(Dest);

    {Move to next column}
    inc(Src, 3);
    inc(Dest, ColumnIncrement[Pass] * 3 - 3);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Copy interlaced images with 2 bytes for R, G, B}

//==============================================================================
//
// TChunkIDAT.CopyInterlacedRGB16
//
//==============================================================================
procedure TChunkIDAT.CopyInterlacedRGB16(const Pass: Byte;
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Dest := PChar(Longint(Dest) + Col * 3);
  repeat
    {Copy this row}
    Byte(Dest^) := Owner.GammaTable[PByte(Longint(Src) + 4)^]; inc(Dest);
    Byte(Dest^) := Owner.GammaTable[PByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^) := Owner.GammaTable[PByte(Longint(Src)    )^]; inc(Dest);
    {$IFDEF Store16bits}
    {Copy extra pixel values}
    Byte(Extra^) := fOwner.GammaTable[PByte(Longint(Src) + 5)^]; inc(Extra);
    Byte(Extra^) := fOwner.GammaTable[PByte(Longint(Src) + 3)^]; inc(Extra);
    Byte(Extra^) := fOwner.GammaTable[PByte(Longint(Src) + 1)^]; inc(Extra);
    {$ENDIF}

    {Move to next column}
    inc(Src, 6);
    inc(Dest, ColumnIncrement[Pass] * 3 - 3);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Copy ímages with palette using bit depths 1, 4 or 8}

//==============================================================================
//
// TChunkIDAT.CopyInterlacedPalette148
//
//==============================================================================
procedure TChunkIDAT.CopyInterlacedPalette148(const Pass: Byte;
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
const
  BitTable: array[1..8] of Integer = ($1, $3, 0, $F, 0, 0, 0, $FF);
  StartBit: array[1..8] of Integer = (7 , 0 , 0, 4,  0, 0, 0, 0);
var
  CurBit, Col: Integer;
  Dest2: PChar;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  repeat
    {Copy data}
    CurBit := StartBit[Header.BitDepth];
    repeat
      {Adjust pointer to pixel byte bounds}
      Dest2 := PChar(Longint(Dest) + (Header.BitDepth * Col) div 8);
      {Copy data}
      Byte(Dest2^) := Byte(Dest2^) or
        ( ((Byte(Src^) shr CurBit) and BitTable[Header.BitDepth])
          shl (StartBit[Header.BitDepth] - (Col * Header.BitDepth mod 8)));

      {Move to next column}
      inc(Col, ColumnIncrement[Pass]);
      {Will read next bits}
      dec(CurBit, Header.BitDepth);
    until CurBit < 0;

    {Move to next byte in source}
    inc(Src);
  until Col >= ImageWidth;
end;

{Copy ímages with palette using bit depth 2}

//==============================================================================
//
// TChunkIDAT.CopyInterlacedPalette2
//
//==============================================================================
procedure TChunkIDAT.CopyInterlacedPalette2(const Pass: Byte; Src, Dest,
  Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
var
  CurBit, Col: Integer;
  Dest2: PChar;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  repeat
    {Copy data}
    CurBit := 6;
    repeat
      {Adjust pointer to pixel byte bounds}
      Dest2 := PChar(Longint(Dest) + Col div 2);
      {Copy data}
      Byte(Dest2^) := Byte(Dest2^) or (((Byte(Src^) shr CurBit) and $3)
         shl (4 - (4 * Col) mod 8));
      {Move to next column}
      inc(Col, ColumnIncrement[Pass]);
      {Will read next bits}
      dec(CurBit, 2);
    until CurBit < 0;

    {Move to next byte in source}
    inc(Src);
  until Col >= ImageWidth;
end;

{Copy ímages with grayscale using bit depth 2}

//==============================================================================
//
// TChunkIDAT.CopyInterlacedGray2
//
//==============================================================================
procedure TChunkIDAT.CopyInterlacedGray2(const Pass: Byte;
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
var
  CurBit, Col: Integer;
  Dest2: PChar;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  repeat
    {Copy data}
    CurBit := 6;
    repeat
      {Adjust pointer to pixel byte bounds}
      Dest2 := PChar(Longint(Dest) + Col div 2);
      {Copy data}
      Byte(Dest2^) := Byte(Dest2^) or ((((Byte(Src^) shr CurBit) shl 2) and $F)
         shl (4 - (Col*4) mod 8));
      {Move to next column}
      inc(Col, ColumnIncrement[Pass]);
      {Will read next bits}
      dec(CurBit, 2);
    until CurBit < 0;

    {Move to next byte in source}
    inc(Src);
  until Col >= ImageWidth;
end;

{Copy ímages with palette using 2 bytes for each pixel}

//==============================================================================
//
// TChunkIDAT.CopyInterlacedGrayscale16
//
//==============================================================================
procedure TChunkIDAT.CopyInterlacedGrayscale16(const Pass: Byte;
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Dest := PChar(Longint(Dest) + Col);
  repeat
    {Copy this row}
    Dest^ := Src^; inc(Dest);
    {$IFDEF Store16bits}
    Extra^ := PChar(Longint(Src) + 1)^; inc(Extra);
    {$ENDIF}

    {Move to next column}
    inc(Src, 2);
    inc(Dest, ColumnIncrement[Pass] - 1);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Decodes interlaced RGB alpha with 1 byte for each sample}

//==============================================================================
//
// TChunkIDAT.CopyInterlacedRGBAlpha8
//
//==============================================================================
procedure TChunkIDAT.CopyInterlacedRGBAlpha8(const Pass: Byte;
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Dest := PChar(Longint(Dest) + Col * 3);
  Trans := PChar(Longint(Trans) + Col);
  repeat
    {Copy this row and alpha value}
    Trans^ := PChar(Longint(Src) + 3)^;
    Byte(Dest^) := fOwner.GammaTable[PByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^) := fOwner.GammaTable[PByte(Longint(Src) + 1)^]; inc(Dest);
    Byte(Dest^) := fOwner.GammaTable[PByte(Longint(Src)    )^]; inc(Dest);

    {Move to next column}
    inc(Src, 4);
    inc(Dest, ColumnIncrement[Pass] * 3 - 3);
    inc(Trans, ColumnIncrement[Pass]);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Decodes interlaced RGB alpha with 2 bytes for each sample}

//==============================================================================
//
// TChunkIDAT.CopyInterlacedRGBAlpha16
//
//==============================================================================
procedure TChunkIDAT.CopyInterlacedRGBAlpha16(const Pass: Byte;
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Dest := PChar(Longint(Dest) + Col * 3);
  Trans := PChar(Longint(Trans) + Col);
  repeat
    {Copy this row and alpha value}
    Trans^ := PChar(Longint(Src) + 6)^;
    Byte(Dest^) := fOwner.GammaTable[PByte(Longint(Src) + 4)^]; inc(Dest);
    Byte(Dest^) := fOwner.GammaTable[PByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^) := fOwner.GammaTable[PByte(Longint(Src)    )^]; inc(Dest);
    {$IFDEF Store16bits}
    {Copy extra pixel values}
    Byte(Extra^) := fOwner.GammaTable[PByte(Longint(Src) + 5)^]; inc(Extra);
    Byte(Extra^) := fOwner.GammaTable[PByte(Longint(Src) + 3)^]; inc(Extra);
    Byte(Extra^) := fOwner.GammaTable[PByte(Longint(Src) + 1)^]; inc(Extra);
    {$ENDIF}

    {Move to next column}
    inc(Src, 8);
    inc(Dest, ColumnIncrement[Pass] * 3 - 3);
    inc(Trans, ColumnIncrement[Pass]);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Decodes 8 bit grayscale image followed by an alpha sample}

//==============================================================================
//
// TChunkIDAT.CopyInterlacedGrayscaleAlpha8
//
//==============================================================================
procedure TChunkIDAT.CopyInterlacedGrayscaleAlpha8(const Pass: Byte;
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
var
  Col: Integer;
begin
  {Get first column, pointers to the data and enter in loop}
  Col := ColumnStart[Pass];
  Dest := PChar(Longint(Dest) + Col);
  Trans := PChar(Longint(Trans) + Col);
  repeat
    {Copy this grayscale value and alpha}
    Dest^ := Src^;  inc(Src);
    Trans^ := Src^; inc(Src);

    {Move to next column}
    inc(Dest, ColumnIncrement[Pass]);
    inc(Trans, ColumnIncrement[Pass]);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Decodes 16 bit grayscale image followed by an alpha sample}

//==============================================================================
//
// TChunkIDAT.CopyInterlacedGrayscaleAlpha16
//
//==============================================================================
procedure TChunkIDAT.CopyInterlacedGrayscaleAlpha16(const Pass: Byte;
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
var
  Col: Integer;
begin
  {Get first column, pointers to the data and enter in loop}
  Col := ColumnStart[Pass];
  Dest := PChar(Longint(Dest) + Col);
  Trans := PChar(Longint(Trans) + Col);
  repeat
    {$IFDEF Store16bits}
    Extra^ := PChar(Longint(Src) + 1)^; inc(Extra);
    {$ENDIF}
    {Copy this grayscale value and alpha, transforming 16 bits into 8}
    Dest^ := Src^;  inc(Src, 2);
    Trans^ := Src^; inc(Src, 2);

    {Move to next column}
    inc(Dest, ColumnIncrement[Pass]);
    inc(Trans, ColumnIncrement[Pass]);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Decodes an interlaced image}

//==============================================================================
//
// TChunkIDAT.DecodeInterlacedAdam7
//
//==============================================================================
procedure TChunkIDAT.DecodeInterlacedAdam7(Stream: TDStream;
  var ZLIBStream: TZStreamRec2; const Size: Integer; var crcfile: Cardinal);
var
  CurrentPass: Byte;
  PixelsThisRow: Integer;
  CurrentRow: Integer;
  Trans, Data{$IFDEF Store16bits}, Extra{$ENDIF}: PChar;
  CopyProc: procedure(const Pass: Byte; Src, Dest,
    Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar) of object;
begin

  CopyProc := nil; {Initialize}
  {Determine method to copy the image data}
  case Header.ColorType of
    {R, G, B values for each pixel}
    COLOR_RGB:
      case Header.BitDepth of
        8: CopyProc := CopyInterlacedRGB8;
       16: CopyProc := CopyInterlacedRGB16;
      end {case Header.BitDepth};
    {Palette}
    COLOR_PALETTE, COLOR_GRAYSCALE:
      case Header.BitDepth of
        1, 4, 8:
          CopyProc := CopyInterlacedPalette148;
        2:
          if Header.ColorType = COLOR_PALETTE then
            CopyProc := CopyInterlacedPalette2
          else
            CopyProc := CopyInterlacedGray2;
        16:
          CopyProc := CopyInterlacedGrayscale16;
      end;
    {RGB followed by alpha}
    COLOR_RGBALPHA:
      case Header.BitDepth of
        8: CopyProc := CopyInterlacedRGBAlpha8;
       16: CopyProc := CopyInterlacedRGBAlpha16;
      end;
    {Grayscale followed by alpha}
    COLOR_GRAYSCALEALPHA:
      case Header.BitDepth of
        8: CopyProc := CopyInterlacedGrayscaleAlpha8;
       16: CopyProc := CopyInterlacedGrayscaleAlpha16;
      end;
  end {case Header.ColorType};

  {Adam7 method has 7 passes to make the final image}
  for CurrentPass := 0 to 6 do
  begin
    {Calculates the number of pixels and bytes for this pass row}
    PixelsThisRow := (ImageWidth - ColumnStart[CurrentPass] +
      ColumnIncrement[CurrentPass] - 1) div ColumnIncrement[CurrentPass];
    Row_Bytes := BytesForPixels(PixelsThisRow, Header.ColorType,
      Header.BitDepth);
    {Clear buffer for this pass}
    ZeroMemory(Row_Buffer[not RowUsed], Row_Bytes);

    {Get current row index}
    CurrentRow := RowStart[CurrentPass];
    {Get a pointer to the current row image data}
    Data := {$IFDEF FPC}pointer{$ELSE}Ptr{$ENDIF}(Longint(Header.ImageData) + Header.BytesPerRow *
      (ImageHeight - 1 - CurrentRow));
    Trans := {$IFDEF FPC}pointer{$ELSE}Ptr{$ENDIF}(Longint(Header.ImageAlpha) + ImageWidth * CurrentRow);
    {$IFDEF Store16bits}
    Extra := {$IFDEF FPC}pointer{$ELSE}Ptr{$ENDIF}(Longint(Header.ExtraImageData) + Header.BytesPerRow *
      (ImageHeight - 1 - CurrentRow));
    {$ENDIF}

    if Row_Bytes > 0 then {There must have bytes for this interlaced pass}
      while CurrentRow < ImageHeight do
      begin
        {Reads this line and filter}
        if IDATZlibRead(ZLIBStream, @Row_Buffer[RowUsed][0], Row_Bytes + 1,
          EndPos, CRCFile) = 0 then break;

        FilterRow;
        {Copy image data}

        CopyProc(CurrentPass, @Row_Buffer[RowUsed][1], Data, Trans
          {$IFDEF Store16bits}, Extra{$ENDIF});

        {Use the other RowBuffer item}
        RowUsed := not RowUsed;

        {Move to the next row}
        inc(CurrentRow, RowIncrement[CurrentPass]);
        {Move pointer to the next line}
        dec(Data, RowIncrement[CurrentPass] * Header.BytesPerRow);
        inc(Trans, RowIncrement[CurrentPass] * ImageWidth);
        {$IFDEF Store16bits}
        dec(Extra, RowIncrement[CurrentPass] * Header.BytesPerRow);
        {$ENDIF}
      end {while CurrentRow < ImageHeight};

  end {for CurrentPass};

end;

{Copy 8 bits RGB image}
procedure TChunkIDAT.CopyNonInterlacedRGB8(
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
var
  I: Integer;
begin
  for I := 1 to ImageWidth do
  begin
    {Copy pixel values}
    Byte(Dest^) := fOwner.GammaTable[PByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^) := fOwner.GammaTable[PByte(Longint(Src) + 1)^]; inc(Dest);
    Byte(Dest^) := fOwner.GammaTable[PByte(Longint(Src)    )^]; inc(Dest);
    {Move to next pixel}
    inc(Src, 3);
  end {for I}
end;

{Copy 16 bits RGB image}
procedure TChunkIDAT.CopyNonInterlacedRGB16(
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
var
  I: Integer;
begin
  for I := 1 to ImageWidth do
  begin
    //Since windows does not supports 2 bytes for
    //each R, G, B value, the method will read only 1 byte from it
    {Copy pixel values}
    Byte(Dest^) := fOwner.GammaTable[PByte(Longint(Src) + 4)^]; inc(Dest);
    Byte(Dest^) := fOwner.GammaTable[PByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^) := fOwner.GammaTable[PByte(Longint(Src)    )^]; inc(Dest);
    {$IFDEF Store16bits}
    {Copy extra pixel values}
    Byte(Extra^) := fOwner.GammaTable[PByte(Longint(Src) + 5)^]; inc(Extra);
    Byte(Extra^) := fOwner.GammaTable[PByte(Longint(Src) + 3)^]; inc(Extra);
    Byte(Extra^) := fOwner.GammaTable[PByte(Longint(Src) + 1)^]; inc(Extra);
    {$ENDIF}

    {Move to next pixel}
    inc(Src, 6);
  end {for I}
end;

{Copy types using palettes (1, 4 or 8 bits per pixel)}
procedure TChunkIDAT.CopyNonInterlacedPalette148(
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
begin
  {It's simple as copying the data}
  memcpy(Dest, Src, Row_Bytes);
end;

{Copy grayscale types using 2 bits for each pixel}
procedure TChunkIDAT.CopyNonInterlacedGray2(
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
var
  i: Integer;
begin
  {2 bits is not supported, this routine will converted into 4 bits}
  for i := 1 to Row_Bytes do
  begin
    Byte(Dest^) := ((Byte(Src^) shr 2) and $F) or ((Byte(Src^)) and $F0);
      inc(Dest);
    Byte(Dest^) := ((Byte(Src^) shl 2) and $F) or ((Byte(Src^) shl 4) and $F0);
      inc(Dest);
    inc(Src);
  end {for i}
end;

{Copy types using palette with 2 bits for each pixel}
procedure TChunkIDAT.CopyNonInterlacedPalette2(
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
var
  i: Integer;
begin
  {2 bits is not supported, this routine will converted into 4 bits}
  for i := 1 to Row_Bytes do
  begin
    Byte(Dest^) := ((Byte(Src^) shr 4) and $3) or ((Byte(Src^) shr 2) and $30);
      inc(Dest);
    Byte(Dest^) := (Byte(Src^) and $3) or ((Byte(Src^) shl 2) and $30);
      inc(Dest);
    inc(Src);
  end {for i}
end;

{Copy grayscale images with 16 bits}
procedure TChunkIDAT.CopyNonInterlacedGrayscale16(
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
var
  I: Integer;
begin
  for I := 1 to ImageWidth do
  begin
    {Windows does not supports 16 bits for each pixel in grayscale}
    {mode, so reduce to 8}
    Dest^ := Src^; inc(Dest);
    {$IFDEF Store16bits}
    Extra^ := PChar(Longint(Src) + 1)^; inc(Extra);
    {$ENDIF}

    {Move to next pixel}
    inc(Src, 2);
  end {for I}
end;

{Copy 8 bits per sample RGB images followed by an alpha byte}
procedure TChunkIDAT.CopyNonInterlacedRGBAlpha8(
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
var
  i: Integer;
begin
  for I := 1 to ImageWidth do
  begin
    {Copy pixel values and transparency}
    Trans^ := PChar(Longint(Src) + 3)^;
    Byte(Dest^) := fOwner.GammaTable[PByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^) := fOwner.GammaTable[PByte(Longint(Src) + 1)^]; inc(Dest);
    Byte(Dest^) := fOwner.GammaTable[PByte(Longint(Src)    )^]; inc(Dest);
    {Move to next pixel}
    inc(Src, 4); inc(Trans);
  end {for I}
end;

{Copy 16 bits RGB image with alpha using 2 bytes for each sample}
procedure TChunkIDAT.CopyNonInterlacedRGBAlpha16(
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
var
  I: Integer;
begin
  for I := 1 to ImageWidth do
  begin
    //Copy rgb and alpha values (transforming from 16 bits to 8 bits)
    {Copy pixel values}
    Trans^ := PChar(Longint(Src) + 6)^;
    Byte(Dest^) := fOwner.GammaTable[PByte(Longint(Src) + 4)^]; inc(Dest);
    Byte(Dest^) := fOwner.GammaTable[PByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^) := fOwner.GammaTable[PByte(Longint(Src)    )^]; inc(Dest);
    {$IFDEF Store16bits}
    {Copy extra pixel values}
    Byte(Extra^) := fOwner.GammaTable[PByte(Longint(Src) + 5)^]; inc(Extra);
    Byte(Extra^) := fOwner.GammaTable[PByte(Longint(Src) + 3)^]; inc(Extra);
    Byte(Extra^) := fOwner.GammaTable[PByte(Longint(Src) + 1)^]; inc(Extra);
    {$ENDIF}
    {Move to next pixel}
    inc(Src, 8); inc(Trans);
  end {for I}
end;

{Copy 8 bits per sample grayscale followed by alpha}
procedure TChunkIDAT.CopyNonInterlacedGrayscaleAlpha8(
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
var
  I: Integer;
begin
  for I := 1 to ImageWidth do
  begin
    {Copy alpha value and then gray value}
    Dest^ := Src^;
    inc(Src);
    Trans^ := Src^;
    inc(Src);

    inc(Dest);
    inc(Trans);
  end;
end;

{Copy 16 bits per sample grayscale followed by alpha}
procedure TChunkIDAT.CopyNonInterlacedGrayscaleAlpha16(
  Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar);
var
  I: Integer;
begin
  for I := 1 to ImageWidth do
  begin
    {Copy alpha value and then gray value}
    {$IFDEF Store16bits}
    Extra^ := PChar(Longint(Src) + 1)^;
    inc(Extra);
    {$ENDIF}
    Dest^ := Src^;
    inc(Src, 2);
    Trans^ := Src^;
    inc(Src, 2);
    inc(Dest);
    inc(Trans);
  end;
end;

{Decode non interlaced image}

//==============================================================================
//
// TChunkIDAT.DecodeNonInterlaced
//
//==============================================================================
procedure TChunkIDAT.DecodeNonInterlaced(Stream: TDStream;
  var ZLIBStream: TZStreamRec2; const Size: Integer; var crcfile: Cardinal);
var
  j: Cardinal;
  Trans, Data{$IFDEF Store16bits}, Extra{$ENDIF}: PChar;
  CopyProc: procedure(
    Src, Dest, Trans{$IFDEF Store16bits}, Extra{$ENDIF}: PChar) of object;
begin
  CopyProc := nil; {Initialize}
  {Determines the method to copy the image data}
  case Header.ColorType of
    {R, G, B values}
    COLOR_RGB:
      case Header.BitDepth of
        8: CopyProc := CopyNonInterlacedRGB8;
       16: CopyProc := CopyNonInterlacedRGB16;
      end;
    {Types using palettes}
    COLOR_PALETTE, COLOR_GRAYSCALE:
      case Header.BitDepth of
        1, 4, 8:
          CopyProc := CopyNonInterlacedPalette148;
        2:
          if Header.ColorType = COLOR_PALETTE then
            CopyProc := CopyNonInterlacedPalette2
          else
            CopyProc := CopyNonInterlacedGray2;
        16:
          CopyProc := CopyNonInterlacedGrayscale16;
      end;
    {R, G, B followed by alpha}
    COLOR_RGBALPHA:
      case Header.BitDepth of
        8: CopyProc := CopyNonInterlacedRGBAlpha8;
       16: CopyProc := CopyNonInterlacedRGBAlpha16;
      end;
    {Grayscale followed by alpha}
    COLOR_GRAYSCALEALPHA:
      case Header.BitDepth of
        8: CopyProc := CopyNonInterlacedGrayscaleAlpha8;
       16: CopyProc := CopyNonInterlacedGrayscaleAlpha16;
      end;
  end;

  {Get the image data pointer}
  Longint(Data) := Longint(Header.ImageData) +
    Header.BytesPerRow * (ImageHeight - 1);
  Trans := Header.ImageAlpha;
  {$IFDEF Store16bits}
  Longint(Extra) := Longint(Header.ExtraImageData) +
    Header.BytesPerRow * (ImageHeight - 1);
  {$ENDIF}
  {Reads each line}
  for j := 0 to ImageHeight - 1 do
  begin
    {Read this line Row_Buffer[RowUsed][0] if the filter type for this line}
    if IDATZlibRead(ZLIBStream, @Row_Buffer[RowUsed][0], Row_Bytes + 1, EndPos,
      CRCFile) = 0 then break;

    {Filter the current row}
    FilterRow;
    {Copies non interlaced row to image}
    CopyProc(@Row_Buffer[RowUsed][1], Data, Trans{$IFDEF Store16bits}, Extra
      {$ENDIF});

    {Invert line used}
    RowUsed := not RowUsed;
    dec(Data, Header.BytesPerRow);
    {$IFDEF Store16bits}dec(Extra, Header.BytesPerRow);{$ENDIF}
    inc(Trans, ImageWidth);
  end {for I};

end;

{Filter the current line}

//==============================================================================
//
// TChunkIDAT.FilterRow
//
//==============================================================================
procedure TChunkIDAT.FilterRow;
var
  pp: Byte;
  vv, left, above, aboveleft: Integer;
  Col: Cardinal;
begin
  {Test the filter}
  case Row_Buffer[RowUsed]^[0] of
    {No filtering for this line}
    FILTER_NONE: begin end;
    {AND 255 serves only to never let the Result be larger than one byte}
    {Sub filter}
    FILTER_SUB:
      for Col := Offset + 1 to Row_Bytes do
        Row_Buffer[RowUsed][Col] := (Row_Buffer[RowUsed][Col] +
          Row_Buffer[RowUsed][Col - Offset]) and 255;
    {Up filter}
    FILTER_UP:
      for Col := 1 to Row_Bytes do
        Row_Buffer[RowUsed][Col] := (Row_Buffer[RowUsed][Col] +
          Row_Buffer[not RowUsed][Col]) and 255;
    {Average filter}
    FILTER_AVERAGE:
      for Col := 1 to Row_Bytes do
      begin
        {Obtains up and left pixels}
        above := Row_Buffer[not RowUsed][Col];
        if col - 1 < Offset then
          left := 0
        else
          Left := Row_Buffer[RowUsed][Col - Offset];

        {Calculates}
        Row_Buffer[RowUsed][Col] := (Row_Buffer[RowUsed][Col] +
          (left + above) div 2) and 255;
      end;
    {Paeth filter}
    FILTER_PAETH:
    begin
      {Initialize}
      left := 0;
      aboveleft := 0;
      {Test each byte}
      for Col := 1 to Row_Bytes do
      begin
        {Obtains above pixel}
        above := Row_Buffer[not RowUsed][Col];
        {Obtains left and top-left pixels}
        if col - 1 >= offset then
        begin
          left := row_buffer[RowUsed][col - offset];
          aboveleft := row_buffer[not RowUsed][col - offset];
        end;

        {Obtains current pixel and paeth predictor}
        vv := row_buffer[RowUsed][Col];
        pp := PaethPredictor(left, above, aboveleft);

        {Calculates}
        Row_Buffer[RowUsed][Col] := (pp + vv) and $FF;
      end {for};
    end;

  end {case};
end;

{Reads the image data from the stream}

//==============================================================================
//
// TChunkIDAT.LoadFromStream
//
//==============================================================================
function TChunkIDAT.LoadFromStream(Stream: TDStream; const ChunkName: TChunkName;
  Size: Integer): Boolean;
var
  ZLIBStream: TZStreamRec2;
  CRCCheck, CRCFile: Cardinal;
begin
  {Get pointer to the header chunk}
  Header := Owner.Chunks.Item[0] as TChunkIHDR;
  {Build palette if necessary}
  if Header.HasPalette then PreparePalette();

  {Copy image width and height}
  ImageWidth := Header.Width;
  ImageHeight := Header.Height;

  {Initialize to calculate CRC}
  {$IFDEF CheckCRC}
    CRCFile := update_crc($ffffffff, @ChunkName[0], 4);
  {$ENDIF}

  Owner.GetPixelInfo(Row_Bytes, Offset); {Obtain line information}
  ZLIBStream := ZLIBInitInflate(Stream);  {Initializes decompression}

  {Calculate ending position for the current IDAT chunk}
  EndPos := Stream.Position + Size;

  {Allocate memory}
  GetMem(Row_Buffer[false], Row_Bytes + 1);
  GetMem(Row_Buffer[true], Row_Bytes + 1);
  ZeroMemory(Row_Buffer[false], Row_bytes + 1);
  {Set the variable to alternate the Row_Buffer item to use}
  RowUsed := true;

  {Call special methods for the different interlace methods}
  case Owner.InterlaceMethod of
    imNone:
      DecodeNonInterlaced(stream, ZLIBStream, Size, crcfile);
    imAdam7:
      DecodeInterlacedAdam7(stream, ZLIBStream, size, crcfile);
  end;

  {Free memory}
  ZLIBTerminateInflate(ZLIBStream); {Terminates decompression}
  FreeMem(Row_Buffer[false], Row_Bytes + 1);
  FreeMem(Row_Buffer[true], Row_Bytes + 1);

  {Now checks CRC}
  Stream.Read(CRCCheck, 4);
  {$IFDEF CheckCRC}
    CRCFile := CRCFile xor $ffffffff;
    CRCCheck := ByteSwap(CRCCheck);
    Result := CRCCheck = CRCFile;

    {Handle CRC error}
    if not Result then
    begin
      {In case it coult not load chunk}
      Owner.RaiseError(EPngInvalidCRCText);
      Exit;
    end;
  {$ELSE}Result := true; {$ENDIF}
end;

const
  IDATHeader: array[0..3] of char = ('I', 'D', 'A', 'T');
  BUFFER = 5;

{Saves the IDAT chunk to a stream}

//==============================================================================
//
// TChunkIDAT.SaveToStream
//
//==============================================================================
function TChunkIDAT.SaveToStream(Stream: TDStream): Boolean;
var
  ZLIBStream: TZStreamRec2;
begin
  {Get pointer to the header chunk}
  Header := Owner.Chunks.Item[0] as TChunkIHDR;
  {Copy image width and height}
  ImageWidth := Header.Width;
  ImageHeight := Header.Height;
  Owner.GetPixelInfo(Row_Bytes, Offset); {Obtain line information}

  {Allocate memory}
  GetMem(Encode_Buffer[BUFFER], Row_Bytes);
  ZeroMemory(Encode_Buffer[BUFFER], Row_Bytes);
  {Allocate buffers for the filters selected}
  {Filter none will always be calculated to the other filters to work}
  GetMem(Encode_Buffer[FILTER_NONE], Row_Bytes);
  ZeroMemory(Encode_Buffer[FILTER_NONE], Row_Bytes);
  if pfSub in Owner.Filters then
    GetMem(Encode_Buffer[FILTER_SUB], Row_Bytes);
  if pfUp in Owner.Filters then
    GetMem(Encode_Buffer[FILTER_UP], Row_Bytes);
  if pfAverage in Owner.Filters then
    GetMem(Encode_Buffer[FILTER_AVERAGE], Row_Bytes);
  if pfPaeth in Owner.Filters then
    GetMem(Encode_Buffer[FILTER_PAETH], Row_Bytes);

  {Initialize ZLIB}
  ZLIBStream := ZLIBInitDeflate(Stream, Owner.fCompressionLevel,
    Owner.MaxIdatSize);
  {Write data depending on the interlace method}
  case Owner.InterlaceMethod of
    imNone: EncodeNonInterlaced(stream, ZLIBStream);
    imAdam7: EncodeInterlacedAdam7(stream, ZLIBStream);
  end;
  {Terminates ZLIB}
  ZLIBTerminateDeflate(ZLIBStream);

  {Release allocated memory}
  FreeMem(Encode_Buffer[BUFFER], Row_Bytes);
  FreeMem(Encode_Buffer[FILTER_NONE], Row_Bytes);
  if pfSub in Owner.Filters then
    FreeMem(Encode_Buffer[FILTER_SUB], Row_Bytes);
  if pfUp in Owner.Filters then
    FreeMem(Encode_Buffer[FILTER_UP], Row_Bytes);
  if pfAverage in Owner.Filters then
    FreeMem(Encode_Buffer[FILTER_AVERAGE], Row_Bytes);
  if pfPaeth in Owner.Filters then
    FreeMem(Encode_Buffer[FILTER_PAETH], Row_Bytes);

  {Everything went ok}
  Result := true;
end;

{Writes the IDAT using the settings}

//==============================================================================
//
// WriteIDAT
//
//==============================================================================
procedure WriteIDAT(Stream: TDStream; Data: Pointer; const Length: Cardinal);
var
  ChunkLen, CRC: Cardinal;
begin
  {Writes IDAT header}
  ChunkLen := ByteSwap(Length);
  Stream.Write(ChunkLen, 4);                      {Chunk length}
  Stream.Write(IDATHeader[0], 4);                 {Idat header}
  CRC := update_crc($ffffffff, @IDATHeader[0], 4); {Crc part for header}

  {Writes IDAT data and calculates CRC for data}
  Stream.Write(Data^, Length);
  CRC := Byteswap(update_crc(CRC, Data, Length) xor $ffffffff);
  {Writes final CRC}
  Stream.Write(CRC, 4);
end;

{Compress and writes IDAT chunk data}

//==============================================================================
//
// TChunkIDAT.IDATZlibWrite
//
//==============================================================================
procedure TChunkIDAT.IDATZlibWrite(var ZLIBStream: TZStreamRec2;
  Buffer: Pointer; const Length: Cardinal);
begin
  with ZLIBStream, ZLIBStream.ZLIB do
  begin
    {Set data to be compressed}
    next_in := Buffer;
    avail_in := Length;

    {Compress all the data avaliable to compress}
    while avail_in > 0 do
    begin
      deflate(ZLIB, Z_NO_FLUSH);

      {The whole buffer was used, save data to stream and restore buffer}
      if avail_out = 0 then
      begin
        {Writes this IDAT chunk}
        WriteIDAT(fStream, Data, Owner.MaxIdatSize);

        {Restore buffer}
        next_out := Data;
        avail_out := Owner.MaxIdatSize;
      end {if avail_out = 0};

    end {while avail_in};

  end {with ZLIBStream, ZLIBStream.ZLIB}
end;

{Finishes compressing data to write IDAT chunk}

//==============================================================================
//
// TChunkIDAT.FinishIDATZlib
//
//==============================================================================
procedure TChunkIDAT.FinishIDATZlib(var ZLIBStream: TZStreamRec2);
begin
  with ZLIBStream, ZLIBStream.ZLIB do
  begin
    {Set data to be compressed}
    next_in := nil;
    avail_in := 0;

    while deflate(ZLIB,Z_FINISH) <> Z_STREAM_END do
    begin
      {Writes this IDAT chunk}
      WriteIDAT(fStream, Data, Owner.MaxIdatSize - avail_out);
      {Re-update buffer}
      next_out := Data;
      avail_out := Owner.MaxIdatSize;
    end;

    if avail_out < Owner.MaxIdatSize then
      {Writes final IDAT}
      WriteIDAT(fStream, Data, Owner.MaxIdatSize - avail_out);

  end {with ZLIBStream, ZLIBStream.ZLIB};
end;

{Copy memory to encode RGB image with 1 byte for each color sample}

//==============================================================================
//
// TChunkIDAT.EncodeNonInterlacedRGB8
//
//==============================================================================
procedure TChunkIDAT.EncodeNonInterlacedRGB8(Src, Dest, Trans: PChar);
var
  I: Integer;
begin
  for I := 1 to ImageWidth do
  begin
    {Copy pixel values}
    Byte(Dest^) := fOwner.InverseGamma[PByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^) := fOwner.InverseGamma[PByte(Longint(Src) + 1)^]; inc(Dest);
    Byte(Dest^) := fOwner.InverseGamma[PByte(Longint(Src)    )^]; inc(Dest);
    {Move to next pixel}
    inc(Src, 3);
  end {for I}
end;

{Copy memory to encode RGB images with 16 bits for each color sample}

//==============================================================================
//
// TChunkIDAT.EncodeNonInterlacedRGB16
//
//==============================================================================
procedure TChunkIDAT.EncodeNonInterlacedRGB16(Src, Dest, Trans: PChar);
var
  I: Integer;
begin
  for I := 1 to ImageWidth do
  begin
    //Now we copy from 1 byte for each sample stored to a 2 bytes (or 1 word)
    //for sample
    {Copy pixel values}
    pWORD(Dest)^ := fOwner.InverseGamma[PByte(Longint(Src) + 2)^]; inc(Dest, 2);
    pWORD(Dest)^ := fOwner.InverseGamma[PByte(Longint(Src) + 1)^]; inc(Dest, 2);
    pWORD(Dest)^ := fOwner.InverseGamma[PByte(Longint(Src)    )^]; inc(Dest, 2);
    {Move to next pixel}
    inc(Src, 3);
  end {for I}

end;

{Copy memory to encode types using palettes (1, 4 or 8 bits per pixel)}

//==============================================================================
//
// TChunkIDAT.EncodeNonInterlacedPalette148
//
//==============================================================================
procedure TChunkIDAT.EncodeNonInterlacedPalette148(Src, Dest, Trans: PChar);
begin
  {It's simple as copying the data}
  memcpy(Dest, Src, Row_Bytes);
end;

{Copy memory to encode grayscale images with 2 bytes for each sample}

//==============================================================================
//
// TChunkIDAT.EncodeNonInterlacedGrayscale16
//
//==============================================================================
procedure TChunkIDAT.EncodeNonInterlacedGrayscale16(Src, Dest, Trans: PChar);
var
  I: Integer;
begin
  for I := 1 to ImageWidth do
  begin
    //Now we copy from 1 byte for each sample stored to a 2 bytes (or 1 word)
    //for sample
    pWORD(Dest)^ := PByte(Longint(Src))^; inc(Dest, 2);
    {Move to next pixel}
    inc(Src);
  end {for I}
end;

{Encode images using RGB followed by an alpha value using 1 byte for each}

//==============================================================================
//
// TChunkIDAT.EncodeNonInterlacedRGBAlpha8
//
//==============================================================================
procedure TChunkIDAT.EncodeNonInterlacedRGBAlpha8(Src, Dest, Trans: PChar);
var
  i: Integer;
begin
  {Copy the data to the destination, including data from Trans pointer}
  for i := 1 to ImageWidth do
  begin
    Byte(Dest^) := Owner.InverseGamma[PByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^) := Owner.InverseGamma[PByte(Longint(Src) + 1)^]; inc(Dest);
    Byte(Dest^) := Owner.InverseGamma[PByte(Longint(Src)    )^]; inc(Dest);
    Dest^ := Trans^; inc(Dest);
    inc(Src, 3); inc(Trans);
  end {for i};
end;

{Encode images using RGB followed by an alpha value using 2 byte for each}

//==============================================================================
//
// TChunkIDAT.EncodeNonInterlacedRGBAlpha16
//
//==============================================================================
procedure TChunkIDAT.EncodeNonInterlacedRGBAlpha16(Src, Dest, Trans: PChar);
var
  i: Integer;
begin
  {Copy the data to the destination, including data from Trans pointer}
  for i := 1 to ImageWidth do
  begin
    PWord(Dest)^ := Owner.InverseGamma[PByte(Longint(Src) + 2)^]; inc(Dest, 2);
    PWord(Dest)^ := Owner.InverseGamma[PByte(Longint(Src) + 1)^]; inc(Dest, 2);
    PWord(Dest)^ := Owner.InverseGamma[PByte(Longint(Src)    )^]; inc(Dest, 2);
    PWord(Dest)^ := PByte(Longint(Trans)  )^; inc(Dest, 2);
    inc(Src, 3); inc(Trans);
  end {for i};
end;

{Encode grayscale images followed by an alpha value using 1 byte for each}
procedure TChunkIDAT.EncodeNonInterlacedGrayscaleAlpha8(
  Src, Dest, Trans: PChar);
var
  i: Integer;
begin
  {Copy the data to the destination, including data from Trans pointer}
  for i := 1 to ImageWidth do
  begin
    Dest^ := Src^; inc(Dest);
    Dest^ := Trans^; inc(Dest);
    inc(Src); inc(Trans);
  end {for i};
end;

{Encode grayscale images followed by an alpha value using 2 byte for each}
procedure TChunkIDAT.EncodeNonInterlacedGrayscaleAlpha16(
  Src, Dest, Trans: PChar);
var
  i: Integer;
begin
  {Copy the data to the destination, including data from Trans pointer}
  for i := 1 to ImageWidth do
  begin
    PWord(Dest)^ := PByte(Src)^;    inc(Dest, 2);
    PWord(Dest)^ := PByte(Trans)^;  inc(Dest, 2);
    inc(Src); inc(Trans);
  end {for i};
end;

{Encode non interlaced images}

//==============================================================================
//
// TChunkIDAT.EncodeNonInterlaced
//
//==============================================================================
procedure TChunkIDAT.EncodeNonInterlaced(Stream: TDStream;
  var ZLIBStream: TZStreamRec2);
var
  {Current line}
  j: Cardinal;
  {Pointers to image data}
  Data, Trans: PChar;
  {Filter used for this line}
  Filter: Byte;
  {Method which will copy the data into the buffer}
  CopyProc: procedure(Src, Dest, Trans: PChar) of object;
begin
  CopyProc := nil;  {Initialize to avoid warnings}
  {Defines the method to copy the data to the buffer depending on}
  {the image parameters}
  case Header.ColorType of
    {R, G, B values}
    COLOR_RGB:
      case Header.BitDepth of
        8: CopyProc := EncodeNonInterlacedRGB8;
       16: CopyProc := EncodeNonInterlacedRGB16;
      end;
    {Palette and grayscale values}
    COLOR_GRAYSCALE, COLOR_PALETTE:
      case Header.BitDepth of
        1, 4, 8: CopyProc := EncodeNonInterlacedPalette148;
             16: CopyProc := EncodeNonInterlacedGrayscale16;
      end;
    {RGB with a following alpha value}
    COLOR_RGBALPHA:
      case Header.BitDepth of
          8: CopyProc := EncodeNonInterlacedRGBAlpha8;
         16: CopyProc := EncodeNonInterlacedRGBAlpha16;
      end;
    {Grayscale images followed by an alpha}
    COLOR_GRAYSCALEALPHA:
      case Header.BitDepth of
        8: CopyProc := EncodeNonInterlacedGrayscaleAlpha8;
       16: CopyProc := EncodeNonInterlacedGrayscaleAlpha16;
      end;
  end {case Header.ColorType};

  {Get the image data pointer}
  Longint(Data) := Longint(Header.ImageData) +
    Header.BytesPerRow * (ImageHeight - 1);
  Trans := Header.ImageAlpha;

  {Writes each line}
  for j := 0 to ImageHeight - 1 do
  begin
    {Copy data into buffer}
    CopyProc(Data, @Encode_Buffer[BUFFER][0], Trans);
    {Filter data}
    Filter := FilterToEncode;

    {Compress data}
    IDATZlibWrite(ZLIBStream, @Filter, 1);
    IDATZlibWrite(ZLIBStream, @Encode_Buffer[Filter][0], Row_Bytes);

    {Adjust pointers to the actual image data}
    dec(Data, Header.BytesPerRow);
    inc(Trans, ImageWidth);
  end;

  {Compress and finishes copying the remaining data}
  FinishIDATZlib(ZLIBStream);
end;

{Copy memory to encode interlaced images using RGB value with 1 byte for}
{each color sample}

//==============================================================================
//
// TChunkIDAT.EncodeInterlacedRGB8
//
//==============================================================================
procedure TChunkIDAT.EncodeInterlacedRGB8(const Pass: Byte;
  Src, Dest, Trans: PChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Src := PChar(Longint(Src) + Col * 3);
  repeat
    {Copy this row}
    Byte(Dest^) := fOwner.InverseGamma[PByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^) := fOwner.InverseGamma[PByte(Longint(Src) + 1)^]; inc(Dest);
    Byte(Dest^) := fOwner.InverseGamma[PByte(Longint(Src)    )^]; inc(Dest);

    {Move to next column}
    inc(Src, ColumnIncrement[Pass] * 3);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Copy memory to encode interlaced RGB images with 2 bytes each color sample}

//==============================================================================
//
// TChunkIDAT.EncodeInterlacedRGB16
//
//==============================================================================
procedure TChunkIDAT.EncodeInterlacedRGB16(const Pass: Byte;
  Src, Dest, Trans: PChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Src := PChar(Longint(Src) + Col * 3);
  repeat
    {Copy this row}
    PWord(Dest)^ := Owner.InverseGamma[PByte(Longint(Src) + 2)^]; inc(Dest, 2);
    PWord(Dest)^ := Owner.InverseGamma[PByte(Longint(Src) + 1)^]; inc(Dest, 2);
    PWord(Dest)^ := Owner.InverseGamma[PByte(Longint(Src)    )^]; inc(Dest, 2);

    {Move to next column}
    inc(Src, ColumnIncrement[Pass] * 3);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Copy memory to encode interlaced images using palettes using bit depths}
{1, 4, 8 (each pixel in the image)}

//==============================================================================
//
// TChunkIDAT.EncodeInterlacedPalette148
//
//==============================================================================
procedure TChunkIDAT.EncodeInterlacedPalette148(const Pass: Byte;
  Src, Dest, Trans: PChar);
const
  BitTable: array[1..8] of Integer = ($1, $3, 0, $F, 0, 0, 0, $FF);
  StartBit: array[1..8] of Integer = (7 , 0 , 0, 4,  0, 0, 0, 0);
var
  CurBit, Col: Integer;
  Src2: PChar;
begin
  {Clean the line}
  ZeroMemory(Dest, Row_Bytes);
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  with Header.BitmapInfo.bmiHeader do
    repeat
      {Copy data}
      CurBit := StartBit[biBitCount];
      repeat
        {Adjust pointer to pixel byte bounds}
        Src2 := PChar(Longint(Src) + (biBitCount * Col) div 8);
        {Copy data}
        Byte(Dest^) := Byte(Dest^) or
          (((Byte(Src2^) shr (StartBit[Header.BitDepth] - (biBitCount * Col)
            mod 8))) and (BitTable[biBitCount])) shl CurBit;

        {Move to next column}
        inc(Col, ColumnIncrement[Pass]);
        {Will read next bits}
        dec(CurBit, biBitCount);
      until CurBit < 0;

      {Move to next byte in source}
      inc(Dest);
    until Col >= ImageWidth;
end;

{Copy to encode interlaced grayscale images using 16 bits for each sample}

//==============================================================================
//
// TChunkIDAT.EncodeInterlacedGrayscale16
//
//==============================================================================
procedure TChunkIDAT.EncodeInterlacedGrayscale16(const Pass: Byte;
  Src, Dest, Trans: PChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Src := PChar(Longint(Src) + Col);
  repeat
    {Copy this row}
    PWord(Dest)^ := Byte(Src^); inc(Dest, 2);

    {Move to next column}
    inc(Src, ColumnIncrement[Pass]);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Copy to encode interlaced rgb images followed by an alpha value, all using}
{one byte for each sample}

//==============================================================================
//
// TChunkIDAT.EncodeInterlacedRGBAlpha8
//
//==============================================================================
procedure TChunkIDAT.EncodeInterlacedRGBAlpha8(const Pass: Byte;
  Src, Dest, Trans: PChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Src := PChar(Longint(Src) + Col * 3);
  Trans := PChar(Longint(Trans) + Col);
  repeat
    {Copy this row}
    Byte(Dest^) := Owner.InverseGamma[PByte(Longint(Src) + 2)^]; inc(Dest);
    Byte(Dest^) := Owner.InverseGamma[PByte(Longint(Src) + 1)^]; inc(Dest);
    Byte(Dest^) := Owner.InverseGamma[PByte(Longint(Src)    )^]; inc(Dest);
    Dest^ := Trans^; inc(Dest);

    {Move to next column}
    inc(Src, ColumnIncrement[Pass] * 3);
    inc(Trans, ColumnIncrement[Pass]);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Copy to encode interlaced rgb images followed by an alpha value, all using}
{two byte for each sample}

//==============================================================================
//
// TChunkIDAT.EncodeInterlacedRGBAlpha16
//
//==============================================================================
procedure TChunkIDAT.EncodeInterlacedRGBAlpha16(const Pass: Byte;
  Src, Dest, Trans: PChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Src := PChar(Longint(Src) + Col * 3);
  Trans := PChar(Longint(Trans) + Col);
  repeat
    {Copy this row}
    PWord(Dest)^ := PByte(Longint(Src) + 2)^; inc(Dest, 2);
    PWord(Dest)^ := PByte(Longint(Src) + 1)^; inc(Dest, 2);
    PWord(Dest)^ := PByte(Longint(Src)    )^; inc(Dest, 2);
    PWord(Dest)^ := PByte(Trans)^; inc(Dest, 2);

    {Move to next column}
    inc(Src, ColumnIncrement[Pass] * 3);
    inc(Trans, ColumnIncrement[Pass]);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Copy to encode grayscale interlaced images followed by an alpha value, all}
{using 1 byte for each sample}

//==============================================================================
//
// TChunkIDAT.EncodeInterlacedGrayscaleAlpha8
//
//==============================================================================
procedure TChunkIDAT.EncodeInterlacedGrayscaleAlpha8(const Pass: Byte;
  Src, Dest, Trans: PChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Src := PChar(Longint(Src) + Col);
  Trans := PChar(Longint(Trans) + Col);
  repeat
    {Copy this row}
    Dest^ := Src^;   inc(Dest);
    Dest^ := Trans^; inc(Dest);

    {Move to next column}
    inc(Src, ColumnIncrement[Pass]);
    inc(Trans, ColumnIncrement[Pass]);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Copy to encode grayscale interlaced images followed by an alpha value, all}
{using 2 bytes for each sample}

//==============================================================================
//
// TChunkIDAT.EncodeInterlacedGrayscaleAlpha16
//
//==============================================================================
procedure TChunkIDAT.EncodeInterlacedGrayscaleAlpha16(const Pass: Byte;
  Src, Dest, Trans: PChar);
var
  Col: Integer;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Src := PChar(Longint(Src) + Col);
  Trans := PChar(Longint(Trans) + Col);
  repeat
    {Copy this row}
    PWord(Dest)^ := PByte(Src)^; inc(Dest, 2);
    PWord(Dest)^ := PByte(Trans)^; inc(Dest, 2);

    {Move to next column}
    inc(Src, ColumnIncrement[Pass]);
    inc(Trans, ColumnIncrement[Pass]);
    inc(Col, ColumnIncrement[Pass]);
  until Col >= ImageWidth;
end;

{Encode interlaced images}

//==============================================================================
//
// TChunkIDAT.EncodeInterlacedAdam7
//
//==============================================================================
procedure TChunkIDAT.EncodeInterlacedAdam7(Stream: TDStream;
  var ZLIBStream: TZStreamRec2);
var
  CurrentPass, Filter: Byte;
  PixelsThisRow: Integer;
  CurrentRow: Integer;
  Trans, Data: PChar;
  CopyProc: procedure(const Pass: Byte;
    Src, Dest, Trans: PChar) of object;
begin
  CopyProc := nil;  {Initialize to avoid warnings}
  {Defines the method to copy the data to the buffer depending on}
  {the image parameters}
  case Header.ColorType of
    {R, G, B values}
    COLOR_RGB:
      case Header.BitDepth of
        8: CopyProc := EncodeInterlacedRGB8;
       16: CopyProc := EncodeInterlacedRGB16;
      end;
    {Grayscale and palette}
    COLOR_PALETTE, COLOR_GRAYSCALE:
      case Header.BitDepth of
        1, 4, 8: CopyProc := EncodeInterlacedPalette148;
             16: CopyProc := EncodeInterlacedGrayscale16;
      end;
    {RGB followed by alpha}
    COLOR_RGBALPHA:
      case Header.BitDepth of
          8: CopyProc := EncodeInterlacedRGBAlpha8;
         16: CopyProc := EncodeInterlacedRGBAlpha16;
      end;
    COLOR_GRAYSCALEALPHA:
    {Grayscale followed by alpha}
      case Header.BitDepth of
          8: CopyProc := EncodeInterlacedGrayscaleAlpha8;
         16: CopyProc := EncodeInterlacedGrayscaleAlpha16;
      end;
  end {case Header.ColorType};

  {Compress the image using the seven passes for ADAM 7}
  for CurrentPass := 0 to 6 do
  begin
    {Calculates the number of pixels and bytes for this pass row}
    PixelsThisRow := (ImageWidth - ColumnStart[CurrentPass] +
      ColumnIncrement[CurrentPass] - 1) div ColumnIncrement[CurrentPass];
    Row_Bytes := BytesForPixels(PixelsThisRow, Header.ColorType,
      Header.BitDepth);
    ZeroMemory(Encode_Buffer[FILTER_NONE], Row_Bytes);

    {Get current row index}
    CurrentRow := RowStart[CurrentPass];
    {Get a pointer to the current row image data}
    Data := {$IFDEF FPC}pointer{$ELSE}Ptr{$ENDIF}(Longint(Header.ImageData) + Header.BytesPerRow *
      (ImageHeight - 1 - CurrentRow));
    Trans := {$IFDEF FPC}pointer{$ELSE}Ptr{$ENDIF}(Longint(Header.ImageAlpha) + ImageWidth * CurrentRow);

    {Process all the image rows}
    if Row_Bytes > 0 then
      while CurrentRow < ImageHeight do
      begin
        {Copy data into buffer}
        CopyProc(CurrentPass, Data, @Encode_Buffer[BUFFER][0], Trans);
        {Filter data}
        Filter := FilterToEncode;

        {Compress data}
        IDATZlibWrite(ZLIBStream, @Filter, 1);
        IDATZlibWrite(ZLIBStream, @Encode_Buffer[Filter][0], Row_Bytes);

        {Move to the next row}
        inc(CurrentRow, RowIncrement[CurrentPass]);
        {Move pointer to the next line}
        dec(Data, RowIncrement[CurrentPass] * Header.BytesPerRow);
        inc(Trans, RowIncrement[CurrentPass] * ImageWidth);
      end {while CurrentRow < ImageHeight}

  end {CurrentPass};

  {Compress and finishes copying the remaining data}
  FinishIDATZlib(ZLIBStream);
end;

{Filters the row to be encoded and returns the best filter}

//==============================================================================
//
// TChunkIDAT.FilterToEncode
//
//==============================================================================
function TChunkIDAT.FilterToEncode: Byte;
var
  Run, LongestRun, ii, jj: Cardinal;
  Last, Above, LastAbove: Byte;
begin
  {Selecting more filters using the Filters property from TPngObject}
  {increases the chances to the file be much smaller, but decreases}
  {the performace}

  {This method will creates the same line data using the different}
  {filter methods and select the best}

  {Sub-filter}
  if pfSub in Owner.Filters then
    for ii := 0 to Row_Bytes - 1 do
    begin
      {There is no previous pixel when it's on the first pixel, so}
      {set last as zero when in the first}
      if (ii >= Offset) then
        last := Encode_Buffer[BUFFER]^[ii - Offset]
      else
        last := 0;
      Encode_Buffer[FILTER_SUB]^[ii] := Encode_Buffer[BUFFER]^[ii] - last;
    end;

  {Up filter}
  if pfUp in Owner.Filters then
    for ii := 0 to Row_Bytes - 1 do
      Encode_Buffer[FILTER_UP]^[ii] := Encode_Buffer[BUFFER]^[ii] -
        Encode_Buffer[FILTER_NONE]^[ii];

  {Average filter}
  if pfAverage in Owner.Filters then
    for ii := 0 to Row_Bytes - 1 do
    begin
      {Get the previous pixel, if the current pixel is the first, the}
      {previous is considered to be 0}
      if (ii >= Offset) then
        last := Encode_Buffer[BUFFER]^[ii - Offset]
      else
        last := 0;
      {Get the pixel above}
      above := Encode_Buffer[FILTER_NONE]^[ii];

      {Calculates formula to the average pixel}
      Encode_Buffer[FILTER_AVERAGE]^[ii] := Encode_Buffer[BUFFER]^[ii] -
        (above + last) div 2 ;
    end;

  {Paeth filter (the slower)}
  if pfPaeth in Owner.Filters then
  begin
    {Initialize}
    last := 0;
    lastabove := 0;
    for ii := 0 to Row_Bytes - 1 do
    begin
      {In case this pixel is not the first in the line obtains the}
      {previous one and the one above the previous}
      if (ii >= Offset) then
      begin
        last := Encode_Buffer[BUFFER]^[ii - Offset];
        lastabove := Encode_Buffer[FILTER_NONE]^[ii - Offset];
      end;
      {Obtains the pixel above}
      above := Encode_Buffer[FILTER_NONE]^[ii];
      {Calculate paeth filter for this byte}
      Encode_Buffer[FILTER_PAETH]^[ii] := Encode_Buffer[BUFFER]^[ii] -
        PaethPredictor(last, above, lastabove);
    end;
  end;

  {Now calculates the same line using no filter, which is necessary}
  {in order to have data to the filters when the next line comes}
  memcpy(@Encode_Buffer[FILTER_NONE]^[0],
    @Encode_Buffer[BUFFER]^[0], Row_Bytes);

  {If only filter none is selected in the filter list, we don't need}
  {to proceed and further}
  if (Owner.Filters = [pfNone]) or (Owner.Filters = []) then
  begin
    Result := FILTER_NONE;
    Exit;
  end {if (Owner.Filters = [pfNone...};

  {Check which filter is the best by checking which has the larger}
  {sequence of the same byte, since they are best compressed}
  LongestRun := 0; Result := FILTER_NONE;
  for ii := FILTER_NONE to FILTER_PAETH do
    {Check if this filter was selected}
    if TFilter(ii) in Owner.Filters then
    begin
      Run := 0;
      {Check if it's the only filter}
      if Owner.Filters = [TFilter(ii)] then
      begin
        Result := ii;
        Exit;
      end;

      {Check using a sequence of four bytes}
      for jj := 2 to Row_Bytes - 1 do
        if (Encode_Buffer[ii]^[jj] = Encode_Buffer [ii]^[jj-1]) or
            (Encode_Buffer[ii]^[jj] = Encode_Buffer [ii]^[jj-2]) then
          inc(Run);  {Count the number of sequences}

      {Check if this one is the best so far}
      if (Run > LongestRun) then
      begin
        Result := ii;
        LongestRun := Run;
      end {if (Run > LongestRun)};

    end {if TFilter(ii) in Owner.Filters};
end;

{TChunkPLTE implementation}

{Returns an item in the palette}

//==============================================================================
//
// TChunkPLTE.GetPaletteItem
//
//==============================================================================
function TChunkPLTE.GetPaletteItem(Index: Byte): TRGBQuad;
begin
  {Test if item is valid, if not raise error}
  if Index > Count - 1 then
    Owner.RaiseError(EPNGUnknownPalEntryText)
  else
    {Returns the item}
    Result := Header.BitmapInfo.bmiColors[Index];
end;

{Loads the palette chunk from a stream}

//==============================================================================
//
// TChunkPLTE.LoadFromStream
//
//==============================================================================
function TChunkPLTE.LoadFromStream(Stream: TDStream;
  const ChunkName: TChunkName; Size: Integer): Boolean;
type
  pPalEntry = ^PalEntry;
  PalEntry = record
    r, g, b: Byte;
  end;
var
  j: Integer;          {for the for}
  PalColor: pPalEntry;
  palEntries: TMaxLogPalette;
begin
  {Let ancestor load data and check CRC}
  Result := inherited LoadFromStream(Stream, ChunkName, Size);
  if not Result then
    Exit;

  {This chunk must be divisible by 3 in order to be valid}
  if (Size mod 3 <> 0) or (Size div 3 > 256) then
  begin
    {Raise error}
    Result := false;
    Owner.RaiseError(EPNGInvalidPaletteText);
    Exit;
  end {if Size mod 3 <> 0};

  {Fill array with the palette entries}
  fCount := Size div 3;
  ZeroMemory(@palEntries, SizeOf(palEntries));
  palEntries.palVersion := $300;
  palEntries.palNumEntries := fCount;
  PalColor := Data;
  for j := 0 to fCount - 1 do
    with palEntries.palPalEntry[j] do
    begin
      peRed := Owner.GammaTable[PalColor.r];
      peGreen := Owner.GammaTable[PalColor.g];
      peBlue := Owner.GammaTable[PalColor.b];
      peFlags := 0;
      {Move to next palette entry}
      inc(PalColor);
    end;
  Owner.SetPalette(@palEntries);
end;

{Saves the PLTE chunk to a stream}

//==============================================================================
//
// TChunkPLTE.SaveToStream
//
//==============================================================================
function TChunkPLTE.SaveToStream(Stream: TDStream): Boolean;
var
  J: Integer;
  DataPtr: PByte;
  BitmapInfo: TMAXBITMAPINFO;
  palEntries: TMaxLogPalette;
begin
  {Adjust size to hold all the palette items}
  if fCount = 0 then
    fCount := Header.BitmapInfo.bmiHeader.biClrUsed;
  ResizeData(fCount * 3);
  {Get all the palette entries}
  ZeroMemory(@palEntries, SizeOf(palEntries));
  GetPaletteEntries(Header.ImagePalette, 0, 256, palEntries.palPalEntry[0]);
  {Copy pointer to data}
  DataPtr := fData;

  {Copy palette items}
  BitmapInfo := Header.BitmapInfo;
  for j := 0 to fCount - 1 do
    with palEntries.palPalEntry[j] do
    begin
      DataPtr^ := Owner.InverseGamma[peRed]; inc(DataPtr);
      DataPtr^ := Owner.InverseGamma[peGreen]; inc(DataPtr);
      DataPtr^ := Owner.InverseGamma[peBlue]; inc(DataPtr);
    end {with BitmapInfo};

  {Let ancestor do the rest of the work}
  Result := inherited SaveToStream(Stream);
end;

{Assigns from another PLTE chunk}

//==============================================================================
//
// TChunkPLTE.Assign
//
//==============================================================================
procedure TChunkPLTE.Assign(Source: TChunk);
begin
  {Copy the number of palette items}
  if Source is TChunkPLTE then
    fCount := TChunkPLTE(Source).fCount
  else
    Owner.RaiseError(EPNGCannotAssignChunkText);
end;

{TChunkgAMA implementation}

{Assigns from another chunk}

//==============================================================================
//
// TChunkgAMA.Assign
//
//==============================================================================
procedure TChunkgAMA.Assign(Source: TChunk);
begin
  {Copy the gamma value}
  if Source is TChunkgAMA then
    Gamma := TChunkgAMA(Source).Gamma
  else
    Owner.RaiseError(EPNGCannotAssignChunkText);
end;

{Gamma chunk being created}

//==============================================================================
//
// TChunkgAMA.Create
//
//==============================================================================
constructor TChunkgAMA.Create(Owner: TPngObject);
begin
  {Call ancestor}
  inherited Create(Owner);
  Gamma := 1;  {Initial value}
end;

{Returns gamma value}

//==============================================================================
//
// TChunkgAMA.GetValue
//
//==============================================================================
function TChunkgAMA.GetValue: Cardinal;
begin
  {Make sure that the size is four bytes}
  if DataSize <> 4 then
  begin
    {Adjust size and returns 1}
    ResizeData(4);
    Result := 1;
  end
  {If it's right, read the value}
  else
    Result := Cardinal(ByteSwap(pCardinal(Data)^))
end;

//==============================================================================
//
// Power
//
//==============================================================================
function Power(Base, Exponent: Extended): Extended;
begin
  if Exponent = 0.0 then
    Result := 1.0 {Math rule}
  else if (Base = 0) or (Exponent = 0) then
    Result := 0
  else
    Result := Exp(Exponent * Ln(Base));
end;

{Loading the chunk from a stream}

//==============================================================================
//
// TChunkgAMA.LoadFromStream
//
//==============================================================================
function TChunkgAMA.LoadFromStream(Stream: TDStream;
  const ChunkName: TChunkName; Size: Integer): Boolean;
var
  i: Integer;
  Value: Cardinal;
begin
  {Call ancestor and test if it went ok}
  Result := inherited LoadFromStream(Stream, ChunkName, Size);
  if not Result then
    Exit;
  Value := Gamma;
  {Build gamma table and inverse table for saving}
  if Value <> 0 then
    with Owner do
      for i := 0 to 255 do
      begin
        GammaTable[I] := Round(Power((I / 255), 1 /
          (Value / 100000 * 2.2)) * 255);
        InverseGamma[Round(Power((I / 255), 1 /
          (Value / 100000 * 2.2)) * 255)] := I;
      end
end;

{Sets the gamma value}

//==============================================================================
//
// TChunkgAMA.SetValue
//
//==============================================================================
procedure TChunkgAMA.SetValue(const Value: Cardinal);
begin
  {Make sure that the size is four bytes}
  if DataSize <> 4 then ResizeData(4);
  {If it's right, set the value}
  pCardinal(Data)^ := ByteSwap(Value);
end;

{TPngObject implementation}

{Clear all the chunks in the list}

//==============================================================================
//
// TPngObject.ClearChunks
//
//==============================================================================
procedure TPngObject.ClearChunks;
var
  i: Integer;
begin
  {Initialize gamma}
  InitializeGamma();
  {Free all the objects and memory (0 chunks Bug fixed by Noel Sharpe)}
  for i := 0 to Integer(Chunks.Count) - 1 do
    TChunk(Chunks.Item[i]).Free;
  Chunks.Count := 0;
end;

{Portable Network Graphics object being created as a blank image}

//==============================================================================
//
// TPNGObject.CreateBlank
//
//==============================================================================
constructor TPNGObject.CreateBlank(ColorType, BitDepth: Cardinal;
  cx, cy: Integer);
var
  NewIHDR: TChunkIHDR;
begin
  {Calls creator}
  Create;
  {Checks if the parameters are ok}
  if not (ColorType in [COLOR_GRAYSCALE, COLOR_RGB, COLOR_PALETTE,
    COLOR_GRAYSCALEALPHA, COLOR_RGBALPHA]) or not (BitDepth in
    [1,2,4,8, 16]) or ((ColorType = COLOR_PALETTE) and (BitDepth = 16)) or
    ((ColorType = COLOR_RGB) and (BitDepth < 8)) then
  begin
    RaiseError(EInvalidSpec);
    Exit;
  end;
  if Bitdepth = 2 then Bitdepth := 4;

  {Add the basis chunks}
  InitializeGamma;
  BeingCreated := true;
  Chunks.Add(TChunkIEND);
  NewIHDR := Chunks.Add(TChunkIHDR) as TChunkIHDR;
  NewIHDR.IHDRData.ColorType := ColorType;
  NewIHDR.IHDRData.BitDepth := BitDepth;
  NewIHDR.IHDRData.Width := cx;
  NewIHDR.IHDRData.Height := cy;
  NewIHDR.PrepareImageData;
  if NewIHDR.HasPalette then
    TChunkPLTE(Chunks.Add(TChunkPLTE)).fCount := 1 shl BitDepth;
  Chunks.Add(TChunkIDAT);
  BeingCreated := false;
end;

{Portable Network Graphics object being created}

//==============================================================================
//
// TPngObject.Create
//
//==============================================================================
constructor TPngObject.Create;
begin
  {Let it be created}
  inherited Create;

  fLeftOffset := 0;
  fTopOffset := 0;
  {Initial properties}
  fFilters := [pfSub];
  fCompressionLevel := 7;
  fInterlaceMethod := imNone;
  fMaxIdatSize := High(Word);
  {Create chunklist object}
  fChunkList := TPngList.Create(Self);
end;

{Portable Network Graphics object being destroyed}

//==============================================================================
//
// TPngObject.Destroy
//
//==============================================================================
destructor TPngObject.Destroy;
begin
  {Free object list}
  ClearChunks;
  fChunkList.Free;

  {Call ancestor destroy}
  inherited Destroy;
end;

{Returns linesize and byte offset for pixels}

//==============================================================================
//
// TPngObject.GetPixelInfo
//
//==============================================================================
procedure TPngObject.GetPixelInfo(var LineSize, Offset: Cardinal);
begin
  {There must be an Header chunk to calculate size}
  if HeaderPresent then
  begin
    {Calculate number of bytes for each line}
    LineSize := BytesForPixels(Header.Width, Header.ColorType, Header.BitDepth);

    {Calculates byte offset}
    case Header.ColorType of
      {Grayscale}
      COLOR_GRAYSCALE:
        if Header.BitDepth = 16 then
          Offset := 2
        else
          Offset := 1 ;
      {It always smaller or equal one byte, so it occupes one byte}
      COLOR_PALETTE:
        offset := 1;
      {It might be 3 or 6 bytes}
      COLOR_RGB:
        offset := 3 * Header.BitDepth div 8;
      {It might be 2 or 4 bytes}
      COLOR_GRAYSCALEALPHA:
        offset := 2 * Header.BitDepth div 8;
      {4 or 8 bytes}
      COLOR_RGBALPHA:
        offset := 4 * Header.BitDepth div 8;
      else
        Offset := 0;
    end;

  end
  else
  begin
    {In case if there isn't any Header chunk}
    Offset := 0;
    LineSize := 0;
  end;

end;

{Returns image height}

//==============================================================================
//
// TPngObject.GetHeight
//
//==============================================================================
function TPngObject.GetHeight: Integer;
begin
  {There must be a Header chunk to get the size, otherwise returns 0}
  if HeaderPresent then
    Result := TChunkIHDR(Chunks.Item[0]).Height
  else
    Result := 0;
end;

{Returns image width}

//==============================================================================
//
// TPngObject.GetWidth
//
//==============================================================================
function TPngObject.GetWidth: Integer;
begin
  {There must be a Header chunk to get the size, otherwise returns 0}
  if HeaderPresent then
    Result := Header.Width
  else
    Result := 0;
end;

{Returns if the image is empty}

//==============================================================================
//
// TPngObject.GetEmpty
//
//==============================================================================
function TPngObject.GetEmpty: Boolean;
begin
  Result := (Chunks.Count = 0);
end;

{Raises an error}

//==============================================================================
//
// TPngObject.RaiseError
//
//==============================================================================
procedure TPngObject.RaiseError(Text: String);
begin
  fError := Text;
end;

//==============================================================================
//
// TPngObject.IOresult
//
//==============================================================================
function TPngObject.IOresult: string;
begin
  Result := fError;
  fError := '';
end;

{Set the maximum size for IDAT chunk}

//==============================================================================
//
// TPngObject.SetMaxIdatSize
//
//==============================================================================
procedure TPngObject.SetMaxIdatSize(const Value: Integer);
begin
  {Make sure the size is at least 65535}
  if Value < High(Word) then
    fMaxIdatSize := High(Word) else fMaxIdatSize := Value;
end;

  {Creates a file stream reading from the filename in the parameter and load}
  procedure TPngObject.LoadFromFile(const Filename: String);
  var
    FileStream: TFile;
  begin
    {Test if the file exists}
    if not fexists(Filename) then
    begin
      {In case it does not exists, raise error}
      RaiseError(EPNGNotExistsText);
      Exit;
    end;

    {Creates the file stream to read}
    FileStream := TFile.Create(Filename, fOpenReadOnly);
    LoadFromStream(FileStream);  {Loads the data}
    FileStream.Free;             {Free file stream}
  end;

  {Saves the current png image to a file}
  procedure TPngObject.SaveToFile(const Filename: String);
  var
    FileStream: TFile;
  begin
    {Creates the file stream to write}
    FileStream := TFile.Create(Filename, fCreate);
    SaveToStream(FileStream);    {Saves the data}
    FileStream.Free;             {Free file stream}
  end;

{Returns if it has the pixel information chunk}

//==============================================================================
//
// TPngObject.HasPixelInformation
//
//==============================================================================
function TPngObject.HasPixelInformation: Boolean;
begin
  Result := (Chunks.ItemFromClass(TChunkpHYs) as tChunkpHYs) <> nil;
end;

{Returns the pixel information chunk}

//==============================================================================
//
// TPngObject.GetPixelInformation
//
//==============================================================================
function TPngObject.GetPixelInformation: TChunkpHYs;
begin
  Result := Chunks.ItemFromClass(TChunkpHYs) as tChunkpHYs;
  if not Assigned(Result) then
  begin
    Result := Chunks.Add(tChunkpHYs) as tChunkpHYs;
    Result.fUnit := utMeter;
  end;
end;

{Returns pointer to the chunk TChunkIHDR which should be the first}

//==============================================================================
//
// TPngObject.GetHeader
//
//==============================================================================
function TPngObject.GetHeader: TChunkIHDR;
begin
  {If there is a TChunkIHDR returns it, otherwise returns nil}
  if (Chunks.Count <> 0) and (Chunks.Item[0] is TChunkIHDR) then
    Result := Chunks.Item[0] as TChunkIHDR
  else
  begin
    {No header, throw error message}
    RaiseError(EPNGHeaderNotPresentText);
    Result := nil
  end
end;

{Draws using partial transparency}

//==============================================================================
//
// TPngObject.DrawPartialTrans
//
//==============================================================================
procedure TPngObject.DrawPartialTrans(DC: HDC; Rect: TRect);
  {Adjust the rectangle structure}
  procedure AdjustRect(var Rect: TRect);
  var
    t: Integer;
  begin
    if Rect.Right < Rect.Left then
    begin
      t := Rect.Right;
      Rect.Right := Rect.Left;
      Rect.Left := t;
    end;
    if Rect.Bottom < Rect.Top then
    begin
      t := Rect.Bottom;
      Rect.Bottom := Rect.Top;
      Rect.Top := t;
    end
  end;

type
  {Access to pixels}
  TPixelLine = array[Word] of TRGBQuad;
  pPixelLine = ^TPixelLine;

const
  {Structure used to create the bitmap}
  BitmapInfoHeader: TBitmapInfoHeader =
    (biSize: SizeOf(TBitmapInfoHeader);
     biWidth: 100;
     biHeight: 100;
     biPlanes: 1;
     biBitCount: 32;
     biCompression: BI_RGB;
     biSizeImage: 0;
     biXPelsPerMeter: 0;
     biYPelsPerMeter: 0;
     biClrUsed: 0;
     biClrImportant: 0);
var
  {Buffer bitmap creation}
  BitmapInfo: TBitmapInfo;
  BufferDC: HDC;
  BufferBits: Pointer;
  OldBitmap,
  BufferBitmap: HBitmap;
  Header: TChunkIHDR;

  {Transparency/palette chunks}
  TransparencyChunk: TChunktRNS;
  PaletteChunk: TChunkPLTE;
  TransValue, PaletteIndex: Byte;
  CurBit: Integer;
  Data: PByte;

  {Buffer bitmap modification}
  BytesPerRowDest,
  BytesPerRowSrc,
  BytesPerRowAlpha: Integer;
  ImageSource, ImageSourceOrg,
  AlphaSource: PByteArray;
  ImageData: pPixelLine;
  i, j, i2, j2: Integer;

  {for bitmap stretching}
  W, H: Cardinal;
  Stretch: Boolean;
  FactorX, FactorY: Double;
begin
  {Prepares the rectangle structure to stretch draw}
  if (Rect.Right = Rect.Left) or (Rect.Bottom = Rect.Top) then
    Exit;
  AdjustRect(Rect);
  {Gets the width and height}
  W := Rect.Right - Rect.Left;
  H := Rect.Bottom - Rect.Top;
  Header := Self.Header; {Fast access to header}
  Stretch := (W <> Header.Width) or (H <> Header.Height);
  if Stretch then
    FactorX := W / Header.Width
  else
    FactorX := 1;
  if Stretch then
    FactorY := H / Header.Height
  else
    FactorY := 1;

  {Prepare to create the bitmap}
  ZeroMemory(@BitmapInfo, SizeOf(BitmapInfo));
  BitmapInfoHeader.biWidth := W;
  BitmapInfoHeader.biHeight := -Integer(H);
  BitmapInfo.bmiHeader := BitmapInfoHeader;

  {Create the bitmap which will receive the background, the applied}
  {alpha blending and then will be painted on the background}
  BufferDC := CreateCompatibleDC(0);
  {In case BufferDC could not be created}
  if BufferDC = 0 then
    RaiseError(EPNGOutMemoryText);
  BufferBitmap := CreateDIBSection(BufferDC, BitmapInfo, DIB_RGB_COLORS,
    BufferBits, 0, 0);
  {In case buffer bitmap could not be created}
  if (BufferBitmap = 0) or (BufferBits = Nil) then
  begin
    if BufferBitmap <> 0 then DeleteObject(BufferBitmap);
    DeleteDC(BufferDC);
    RaiseError(EPNGOutMemoryText);
  end;

  {Selects new bitmap and release old bitmap}
  OldBitmap := SelectObject(BufferDC, BufferBitmap);

  {Draws the background on the buffer image}
  BitBlt(BufferDC, 0, 0, W, H, DC, Rect.Left, Rect.Top, SRCCOPY);

  {Obtain number of bytes for each row}
  BytesPerRowAlpha := Header.Width;
  BytesPerRowDest := (((BitmapInfo.bmiHeader.biBitCount * W) + 31)
    and not 31) div 8; {Number of bytes for each image row in destination}
  BytesPerRowSrc := (((Header.BitmapInfo.bmiHeader.biBitCount * Header.Width) +
    31) and not 31) div 8; {Number of bytes for each image row in source}

  {Obtains image pointers}
  ImageData := BufferBits;
  AlphaSource := Header.ImageAlpha;
  Longint(ImageSource) := Longint(Header.ImageData) +
    Header.BytesPerRow * Longint(Header.Height - 1);
  ImageSourceOrg := ImageSource;

  case Header.BitmapInfo.bmiHeader.biBitCount of
    {R, G, B images}
    24:
      for j := 1 to H do
      begin
        {Process all the pixels in this line}
        for i := 0 to W - 1 do
        begin
          if Stretch then
            i2 := trunc(i / FactorX)
          else
            i2 := i;
          {Optmize when we don´t have transparency}
          if (AlphaSource[i2] <> 0) then
            if (AlphaSource[i2] = 255) then
              ImageData[i] := PRGBQuad(@ImageSource[i2 * 3])^
            else
              with ImageData[i] do
              begin
                rgbRed := (255 + ImageSource[2 + i2 * 3] * AlphaSource[i2] + rgbRed *
                  (not AlphaSource[i2])) shr 8;
                rgbGreen := (255 + ImageSource[1 + i2 * 3] * AlphaSource[i2] +
                  rgbGreen * (not AlphaSource[i2])) shr 8;
                rgbBlue := (255 + ImageSource[i2 * 3] * AlphaSource[i2] + rgbBlue *
                 (not AlphaSource[i2])) shr 8;
            end;
          end;

        {Move pointers}
        inc(Longint(ImageData), BytesPerRowDest);
        if Stretch then
          j2 := trunc(j / FactorY)
        else
          j2 := j;
        Longint(ImageSource) := Longint(ImageSourceOrg) - BytesPerRowSrc * j2;
        Longint(AlphaSource) := Longint(Header.ImageAlpha) +
          BytesPerRowAlpha * j2;
      end;
    {Palette images with 1 byte for each pixel}
    1,4,8: if Header.ColorType = COLOR_GRAYSCALEALPHA then
      for j := 1 to H do
      begin
        {Process all the pixels in this line}
        for i := 0 to W - 1 do
          with ImageData[i], Header.BitmapInfo do
          begin
            if Stretch then
              i2 := trunc(i / FactorX)
            else
              i2 := i;
            rgbRed := (255 + ImageSource[i2] * AlphaSource[i2] +
              rgbRed * (255 - AlphaSource[i2])) shr 8;
            rgbGreen := (255 + ImageSource[i2] * AlphaSource[i2] +
              rgbGreen * (255 - AlphaSource[i2])) shr 8;
            rgbBlue := (255 + ImageSource[i2] * AlphaSource[i2] +
              rgbBlue * (255 - AlphaSource[i2])) shr 8;
          end;

        {Move pointers}
        Longint(ImageData) := Longint(ImageData) + BytesPerRowDest;
        if Stretch then
          j2 := trunc(j / FactorY)
        else
          j2 := j;
        Longint(ImageSource) := Longint(ImageSourceOrg) - BytesPerRowSrc * j2;
        Longint(AlphaSource) := Longint(Header.ImageAlpha) +
          BytesPerRowAlpha * j2;
      end
    else {Palette images}
    begin
      {Obtain pointer to the transparency chunk}
      TransparencyChunk := TChunktRNS(Chunks.ItemFromClass(TChunktRNS));
      PaletteChunk := TChunkPLTE(Chunks.ItemFromClass(TChunkPLTE));

      for j := 1 to H do
      begin
        {Process all the pixels in this line}
        i := 0;
        repeat
          CurBit := 0;
          if Stretch then
            i2 := trunc(i / FactorX)
          else
            i2 := i;
          Data := @ImageSource[i2];

          repeat
            {Obtains the palette index}
            case Header.BitDepth of
              1: PaletteIndex := (Data^ shr (7 - (I mod 8))) and 1;
            2,4: PaletteIndex := (Data^ shr ((1 - (I mod 2)) * 4)) and $0F;
             else PaletteIndex := Data^;
            end;

            {Updates the image with the new pixel}
            with ImageData[i] do
            begin
              TransValue := TransparencyChunk.PaletteValues[PaletteIndex];
              rgbRed := (255 + PaletteChunk.Item[PaletteIndex].rgbRed *
                 TransValue + rgbRed * (255 - TransValue)) shr 8;
              rgbGreen := (255 + PaletteChunk.Item[PaletteIndex].rgbGreen *
                 TransValue + rgbGreen * (255 - TransValue)) shr 8;
              rgbBlue := (255 + PaletteChunk.Item[PaletteIndex].rgbBlue *
                 TransValue + rgbBlue * (255 - TransValue)) shr 8;
            end;

            {Move to next data}
            inc(i); inc(CurBit, Header.BitmapInfo.bmiHeader.biBitCount);
          until CurBit >= 8;
          {Move to next source data}
          //inc(Data);
        until i >= Integer(W);

        {Move pointers}
        Longint(ImageData) := Longint(ImageData) + BytesPerRowDest;
        if Stretch then j2 := trunc(j / FactorY) else j2 := j;
        Longint(ImageSource) := Longint(ImageSourceOrg) - BytesPerRowSrc * j2;
      end
    end {Palette images}
  end {case Header.BitmapInfo.bmiHeader.biBitCount};

  {Draws the new bitmap on the foreground}
  BitBlt(DC, Rect.Left, Rect.Top, W, H, BufferDC, 0, 0, SRCCOPY);

  {Free bitmap}
  SelectObject(BufferDC, OldBitmap);
  DeleteObject(BufferBitmap);
  DeleteDC(BufferDC);
end;

{Characters for the header}
const
  PngHeader: array[0..7] of Char = (#137, #80, #78, #71, #13, #10, #26, #10);

{Loads the image from a stream of data}

//==============================================================================
//
// TPngObject.LoadFromStream
//
//==============================================================================
procedure TPngObject.LoadFromStream(Stream: TDStream);
var
  Header: array[0..7] of Char;
  HasIDAT: Boolean;

  {Chunks reading}
  ChunkCount: Cardinal;
  ChunkLength: Cardinal;
  ChunkName: TChunkName;
  pi: PInteger;
  i: integer;
  pb: PByteArray;
begin
  fHastRNS256 := false;
  for i := 0 to 255 do
    ftRNSArray256[i] := 255;
  {Initialize before start loading chunks}
  ChunkCount := 0;
  ClearChunks();
  {Reads the header}
  Stream.Read(Header[0], 8);

  {Test if the header matches}
  if Header <> PngHeader then
  begin
    RaiseError(EPNGInvalidFileHeaderText);
    Exit;
  end;

  HasIDAT := false;
  Chunks.Count := 10;
  fLeftOffset := 0;
  fTopOffset := 0;

  {Load chunks}
  repeat
    inc(ChunkCount);  {Increment number of chunks}
    if Chunks.Count < ChunkCount then  {Resize the chunks list if needed}
      Chunks.Count := Chunks.Count + 10;

    {Reads chunk length and invert since it is in network order}
    {also checks the Read method return, if it returns 0, it}
    {means that no bytes was readed, probably because it reached}
    {the end of the file}
    if Stream.Read(ChunkLength, 4) = 0 then
    begin
      {In case it found the end of the file here}
      Chunks.Count := ChunkCount - 1;
      RaiseError(EPNGUnexpectedEndText);
    end;

    ChunkLength := ByteSwap(ChunkLength);
    {Reads chunk name}
    Stream.Read(Chunkname, 4);

    {Here we check if the first chunk is the Header which is necessary}
    {to the file in order to be a valid Portable Network Graphics image}
    if (ChunkCount = 1) and (ChunkName <> 'IHDR') then
    begin
      Chunks.Count := ChunkCount - 1;
      RaiseError(EPNGIHDRNotFirstText);
      Exit;
    end;

    {Has a previous IDAT}
    if (HasIDAT and (ChunkName = 'IDAT')) or (ChunkName = 'cHRM') then
    begin
      dec(ChunkCount);
      Stream.Seek(ChunkLength + 4, sFromCurrent);
      Continue;
    end;
    {Tell it has an IDAT chunk}
    if ChunkName = 'IDAT' then HasIDAT := true;

    {Creates object for this chunk}
    Chunks.SetItem(ChunkCount - 1, CreateClassChunk(Self, ChunkName));

    {Check if the chunk is critical and unknown}
    {$IFDEF ErrorOnUnknownCritical}
      if (TChunk(Chunks.Item[ChunkCount - 1]).ClassType = TChunk) and
        ((Byte(ChunkName[0]) and $20) = 0) and (ChunkName <> '') then
      begin
        Chunks.Count := ChunkCount;
        RaiseError(EPNGUnknownCriticalChunkText);
      end;
    {$ENDIF}

    {Loads it}
    try if not TChunk(Chunks.Item[ChunkCount - 1]).LoadFromStream(Stream,
       ChunkName, ChunkLength) then break;
    except
      Chunks.Count := ChunkCount;
      raise;
    end;

    // JVAL: Support for 'grAb' chunk (zdoom offsets)
    if ChunkName = 'grAb' then
    begin
      if TChunk(Chunks.Item[ChunkCount - 1]).DataSize >= 8 then
      begin
        pi := TChunk(Chunks.Item[ChunkCount - 1]).Data;
        fLeftOffset := ByteSwap(pi^);
        inc(pi);
        fTopOffset := ByteSwap(pi^);
      end;
    end
    else if (ChunkName = 'tRNS') then // JVAL: tRNS for 256 color palette
    begin
      if TChunk(Chunks.Item[ChunkCount - 1]).DataSize = 256 then
      begin
        fHastRNS256 := true;
        pb := TChunk(Chunks.Item[ChunkCount - 1]).Data;
        for i := 0 to 255 do
          ftRNSArray256[i] := pb[i];
      end;
    end;

  {Terminates when it reaches the IEND chunk}
  until (ChunkName = 'IEND');

  {Resize the list to the appropriate size}
  Chunks.Count := ChunkCount;

  {Check if there is data}
  if not HasIDAT then
    RaiseError(EPNGNoImageDataText);
end;

{Changing height is not supported}

//==============================================================================
//
// TPngObject.SetHeight
//
//==============================================================================
procedure TPngObject.SetHeight(Value: Integer);
begin
  Resize(Width, Value)
end;

{Changing width is not supported}

//==============================================================================
//
// TPngObject.SetWidth
//
//==============================================================================
procedure TPngObject.SetWidth(Value: Integer);
begin
  Resize(Value, Height)
end;

{Returns if the image is transparent}

//==============================================================================
//
// TPngObject.GetTransparent
//
//==============================================================================
function TPngObject.GetTransparent: Boolean;
begin
  Result := (TransparencyMode <> ptmNone);
end;

{Saving the PNG image to a stream of data}

//==============================================================================
//
// TPngObject.SaveToStream
//
//==============================================================================
procedure TPngObject.SaveToStream(Stream: TDStream);
var
  j: Integer;
begin
  {Reads the header}
  Stream.Write(PNGHeader[0], 8);
  {Write each chunk}
  for j := 0 to Chunks.Count - 1 do
    Chunks.Item[j].SaveToStream(Stream)
end;

{Prepares the Header chunk}

//==============================================================================
//
// BuildHeader
//
//==============================================================================
procedure BuildHeader(Header: TChunkIHDR; Handle: HBitmap; Info: pBitmap);
var
  DC: HDC;
begin
  {Set width and height}
  Header.Width := Info.bmWidth;
  Header.Height := abs(Info.bmHeight);
  {Set bit depth}
  if Info.bmBitsPixel >= 16 then
    Header.BitDepth := 8 else Header.BitDepth := Info.bmBitsPixel;
  {Set color type}
  if Info.bmBitsPixel >= 16 then
    Header.ColorType := COLOR_RGB else Header.ColorType := COLOR_PALETTE;
  {Set other info}
  Header.CompressionMethod := 0;  {deflate/inflate}
  Header.InterlaceMethod := 0;    {no interlace}

  {Prepares bitmap headers to hold data}
  Header.PrepareImageData();
  {Copy image data}
  DC := CreateCompatibleDC(0);
  GetDIBits(DC, Handle, 0, Header.Height, Header.ImageData,
    pBitmapInfo(@Header.BitmapInfo)^, DIB_RGB_COLORS);

  DeleteDC(DC);
end;

{Assigns from a bitmap object}

//==============================================================================
//
// TPngObject.AssignHandle
//
//==============================================================================
procedure TPngObject.AssignHandle(Handle: HBitmap; Transparent: Boolean;
  TransparentColor: ColorRef);
var
  BitmapInfo: Windows.TBitmap;
  {Chunks}
  Header: TChunkIHDR;
  PLTE: TChunkPLTE;
  IDAT: TChunkIDAT;
  IEND: TChunkIEND;
  TRNS: TChunkTRNS;
  i: Integer;
  palEntries: TMaxLogPalette;
begin
  {Obtain bitmap info}
  GetObject(Handle, SizeOf(BitmapInfo), @BitmapInfo);

  {Clear old chunks and prepare}
  ClearChunks();

  {Create the chunks}
  Header := TChunkIHDR.Create(Self);

  {This method will fill the Header chunk with bitmap information}
  {and copy the image data}
  BuildHeader(Header, Handle, @BitmapInfo);

  if Header.HasPalette then PLTE := TChunkPLTE.Create(Self) else PLTE := nil;
  if Transparent then TRNS := TChunkTRNS.Create(Self) else TRNS := nil;
  IDAT := TChunkIDAT.Create(Self);
  IEND := TChunkIEND.Create(Self);

  {Add chunks}
  TPNGPointerList(Chunks).Add(Header);
  if Header.HasPalette then
    TPNGPointerList(Chunks).Add(PLTE);
  if Transparent then
    TPNGPointerList(Chunks).Add(TRNS);
  TPNGPointerList(Chunks).Add(IDAT);
  TPNGPointerList(Chunks).Add(IEND);

  {In case there is a image data, set the PLTE chunk fCount variable}
  {to the actual number of palette colors which is 2^(Bits for each pixel)}
  if Header.HasPalette then
  begin
    PLTE.fCount := 1 shl BitmapInfo.bmBitsPixel;

    {Create and set palette}
    ZeroMemory(@palEntries, SizeOf(palEntries));
    palEntries.palVersion := $300;
    palEntries.palNumEntries := 1 shl BitmapInfo.bmBitsPixel;
    for i := 0 to palEntries.palNumEntries - 1 do
    begin
      palEntries.palPalEntry[i].peRed := Header.BitmapInfo.bmiColors[i].rgbRed;
      palEntries.palPalEntry[i].peGreen := Header.BitmapInfo.bmiColors[i].rgbGreen;
      palEntries.palPalEntry[i].peBlue := Header.BitmapInfo.bmiColors[i].rgbBlue;
    end;
    DoSetPalette(CreatePalette(pLogPalette(@palEntries)^), false);
  end;

  {In case it is a transparent bitmap, prepares it}
  if Transparent then TRNS.TransparentColor := TransparentColor;
end;

{Assigns from another PNG}

//==============================================================================
//
// TPngObject.AssignPNG
//
//==============================================================================
procedure TPngObject.AssignPNG(Source: TPNGObject);
var
  J: Integer;
begin
  {Copy properties}
  InterlaceMethod := Source.InterlaceMethod;
  MaxIdatSize := Source.MaxIdatSize;
  CompressionLevel := Source.CompressionLevel;
  Filters := Source.Filters;

  {Clear old chunks and prepare}
  ClearChunks();
  Chunks.Count := Source.Chunks.Count;
  {Create chunks and makes a copy from the source}
  for J := 0 to Chunks.Count - 1 do
    with Source.Chunks do
    begin
      Chunks.SetItem(J, TChunkClass(TChunk(Item[J]).ClassType).Create(Self));
      TChunk(Chunks.Item[J]).Assign(TChunk(Item[J]));
    end {with};
end;

{Returns a alpha data scanline}

//==============================================================================
//
// TPngObject.GetAlphaScanline
//
//==============================================================================
function TPngObject.GetAlphaScanline(const LineIndex: Integer): PByteArray;
begin
  with Header do
    if (ColorType = COLOR_RGBALPHA) or (ColorType = COLOR_GRAYSCALEALPHA) then
      Longint(Result) := Longint(ImageAlpha) + (LineIndex * Longint(Width))
    else
      Result := nil;  {In case the image does not use alpha information}
end;

{$IFDEF Store16bits}
{Returns a png data extra scanline}

//==============================================================================
//
// TPngObject.GetExtraScanline
//
//==============================================================================
function TPngObject.GetExtraScanline(const LineIndex: Integer): Pointer;
begin
  with Header do
    Longint(Result) := (Longint(ExtraImageData) + ((Longint(Height) - 1) *
      BytesPerRow)) - (LineIndex * BytesPerRow);
end;
{$ENDIF}

{Returns a png data scanline}

//==============================================================================
//
// TPngObject.GetScanline
//
//==============================================================================
function TPngObject.GetScanline(const LineIndex: Integer): Pointer;
begin
  with Header do
    Longint(Result) := (Longint(ImageData) + ((Longint(Height) - 1) *
      BytesPerRow)) - (LineIndex * BytesPerRow);
end;

{Initialize gamma table}

//==============================================================================
//
// TPngObject.InitializeGamma
//
//==============================================================================
procedure TPngObject.InitializeGamma;
var
  i: Integer;
begin
  {Build gamma table as if there was no gamma}
  for i := 0 to 255 do
  begin
    GammaTable[i] := i;
    InverseGamma[i] := i;
  end {for i}
end;

{Returns the transparency mode used by this png}

//==============================================================================
//
// TPngObject.GetTransparencyMode
//
//==============================================================================
function TPngObject.GetTransparencyMode: TPNGTransparencyMode;
var
  TRNS: TChunkTRNS;
begin
  with Header do
  begin
    Result := ptmNone; {Default Result}
    {Gets the TRNS chunk pointer}
    TRNS := Chunks.ItemFromClass(TChunkTRNS) as TChunkTRNS;

    {Test depending on the color type}
    case ColorType of
      {This modes are always partial}
      COLOR_RGBALPHA, COLOR_GRAYSCALEALPHA:
        Result := ptmPartial;
      {This modes support bit transparency}
      COLOR_RGB, COLOR_GRAYSCALE:
        if TRNS <> nil then
          Result := ptmBit;
      {Supports booth translucid and bit}
      COLOR_PALETTE:
        {A TRNS chunk must be present, otherwise it won't support transparency}
        if TRNS <> nil then
          if TRNS.BitTransparency then
            Result := ptmBit
          else
            Result := ptmPartial
    end {case}

  end {with Header}
end;

{Add a text chunk}

//==============================================================================
//
// TPngObject.AddtEXt
//
//==============================================================================
procedure TPngObject.AddtEXt(const Keyword, Text: String);
var
  TextChunk: TChunkTEXT;
begin
  TextChunk := Chunks.Add(TChunkText) as TChunkTEXT;
  TextChunk.Keyword := Keyword;
  TextChunk.Text := Text;
end;

{Add a text chunk}

//==============================================================================
//
// TPngObject.AddzTXt
//
//==============================================================================
procedure TPngObject.AddzTXt(const Keyword, Text: String);
var
  TextChunk: TChunkzTXt;
begin
  TextChunk := Chunks.Add(TChunkzTXt) as TChunkzTXt;
  TextChunk.Keyword := Keyword;
  TextChunk.Text := Text;
end;

{Removes the image transparency}

//==============================================================================
//
// TPngObject.RemoveTransparency
//
//==============================================================================
procedure TPngObject.RemoveTransparency;
var
  TRNS: TChunkTRNS;
begin
  {Removes depending on the color type}
  with Header do
    case ColorType of
      {Palette uses the TChunktRNS to store alpha}
      COLOR_PALETTE:
      begin
        TRNS := Chunks.ItemFromClass(TChunkTRNS) as TChunkTRNS;
        if TRNS <> nil then
          Chunks.RemoveChunk(TRNS)
      end;
      {Png allocates different memory space to hold alpha information}
      {for these types}
      COLOR_GRAYSCALEALPHA, COLOR_RGBALPHA:
      begin
        {Transform into the appropriate color type}
        if ColorType = COLOR_GRAYSCALEALPHA then
          ColorType := COLOR_GRAYSCALE
        else
          ColorType := COLOR_RGB;
        {Free the pointer data}
        if ImageAlpha <> nil then
          FreeMem(ImageAlpha);
        ImageAlpha := nil
      end
    end
end;

{Generates alpha information}

//==============================================================================
//
// TPngObject.CreateAlpha
//
//==============================================================================
procedure TPngObject.CreateAlpha;
var
  TRNS: TChunkTRNS;
begin
  {Generates depending on the color type}
  with Header do
    case ColorType of
      {Png allocates different memory space to hold alpha information}
      {for these types}
      COLOR_GRAYSCALE, COLOR_RGB:
      begin
        {Transform into the appropriate color type}
        if ColorType = COLOR_GRAYSCALE then
          ColorType := COLOR_GRAYSCALEALPHA
        else
          ColorType := COLOR_RGBALPHA;
        {Allocates memory to hold alpha information}
        GetMem(ImageAlpha, Integer(Width) * Integer(Height));
        FillChar(ImageAlpha^, Integer(Width) * Integer(Height), #255);
      end;
      {Palette uses the TChunktRNS to store alpha}
      COLOR_PALETTE:
      begin
        {Gets/creates TRNS chunk}
        if Chunks.ItemFromClass(TChunkTRNS) = nil then
          TRNS := Chunks.Add(TChunkTRNS) as TChunkTRNS
        else
          TRNS := Chunks.ItemFromClass(TChunkTRNS) as TChunkTRNS;

          {Prepares the TRNS chunk}
          with TRNS do
          begin
            ResizeData(256);
            FillChar(PaletteValues[0], 256, 255);
            fDataSize := 1 shl Header.BitDepth;
            fBitTransparency := false
          end {with Chunks.Add};
        end;
    end {case Header.ColorType}

end;

{Returns transparent color}

//==============================================================================
//
// TPngObject.GetTransparentColor
//
//==============================================================================
function TPngObject.GetTransparentColor: LongWord;
var
  TRNS: TChunkTRNS;
begin
  TRNS := Chunks.ItemFromClass(TChunkTRNS) as TChunkTRNS;
  {Reads the transparency chunk to get this info}
  if Assigned(TRNS) then
    Result := TRNS.TransparentColor
  else
    Result := 0
end;

//==============================================================================
// TPngObject.SetTransparentColor
//
//{$OPTIMIZATION OFF}
//
//==============================================================================
procedure TPngObject.SetTransparentColor(const Value: LongWord);
var
  TRNS: TChunkTRNS;
begin
  if HeaderPresent then
    {Tests the ColorType}
    case Header.ColorType of
    {Not allowed for this modes}
    COLOR_RGBALPHA, COLOR_GRAYSCALEALPHA: Self.RaiseError(
      EPNGCannotChangeTransparentText);
    {Allowed}
    COLOR_PALETTE, COLOR_RGB, COLOR_GRAYSCALE:
      begin
        TRNS := Chunks.ItemFromClass(TChunkTRNS) as TChunkTRNS;
        if not Assigned(TRNS) then
          TRNS := Chunks.Add(TChunkTRNS) as TChunkTRNS;

        {Sets the transparency value from TRNS chunk}
        TRNS.TransparentColor := Value;
      end {COLOR_PALETTE, COLOR_RGB, COLOR_GRAYSCALE)}
    end {case}
end;

{Returns if header is present}

//==============================================================================
//
// TPngObject.HeaderPresent
//
//==============================================================================
function TPngObject.HeaderPresent: Boolean;
begin
  Result := ((Chunks.Count <> 0) and (Chunks.Item[0] is TChunkIHDR))
end;

//==============================================================================
//
// TPngObject.GettRNSArray256
//
//==============================================================================
function TPngObject.GettRNSArray256: PByteArray;
begin
  Result := @ftRNSArray256;
end;

{Returns pixel for png using palette and grayscale}

//==============================================================================
//
// GetByteArrayPixel
//
//==============================================================================
function GetByteArrayPixel(const png: TPngObject; const X, Y: Integer): LongWord;
var
  ByteData: Byte;
  DataDepth: Byte;
begin
  with png, Header do
  begin
    {Make sure the bitdepth is not greater than 8}
    DataDepth := BitDepth;
    if DataDepth > 8 then DataDepth := 8;
    {Obtains the byte containing this pixel}
    ByteData := PByteArray(png.Scanline[Y])^[X div (8 div DataDepth)];
    {Moves the bits we need to the right}
    ByteData := (ByteData shr ((8 - DataDepth) -
      (X mod (8 div DataDepth)) * DataDepth));
    {Discard the unwanted pixels}
    ByteData:= ByteData and ($FF shr (8 - DataDepth));

    {for palette mode map the palette entry and for grayscale convert and
    returns the intensity}
    case ColorType of
      COLOR_PALETTE:
        with TChunkPLTE(png.Chunks.ItemFromClass(TChunkPLTE)).Item[ByteData] do
          Result := rgb(GammaTable[rgbRed], GammaTable[rgbGreen],
            GammaTable[rgbBlue]);
      COLOR_GRAYSCALE:
      begin
        if BitDepth = 1 then
          ByteData := GammaTable[Byte(ByteData * 255)]
        else
          ByteData := GammaTable[Byte(ByteData * ((1 shl DataDepth) + 1))];
        Result := rgb(ByteData, ByteData, ByteData);
      end;
      else
        Result := 0;
    end {case};
  end {with}
end;

{Sets a pixel for grayscale and palette pngs}

//==============================================================================
//
// SetByteArrayPixel
//
//==============================================================================
procedure SetByteArrayPixel(const png: TPngObject; const X, Y: Integer;
  const Value: LongWord);
const
  ClearFlag: array[1..8] of Integer = (1, 3, 0, 15, 0, 0, 0, $FF);
var
  ByteData: PByte;
  DataDepth: Byte;
  ValEntry: Byte;
begin
  with png.Header do
  begin
    {Map into a palette entry}
    ValEntry := GetNearestPaletteIndex(Png.Palette, Value);

    {16 bits grayscale extra bits are discarted}
    DataDepth := BitDepth;
    if DataDepth > 8 then DataDepth := 8;
    {Gets a pointer to the byte we intend to change}
    ByteData := @PByteArray(png.Scanline[Y])^[X div (8 div DataDepth)];
    {Clears the old pixel data}
    ByteData^ := ByteData^ and not (ClearFlag[DataDepth] shl ((8 - DataDepth) -
      (X mod (8 div DataDepth)) * DataDepth));

    {Setting the new pixel}
    ByteData^ := ByteData^ or (ValEntry shl ((8 - DataDepth) -
      (X mod (8 div DataDepth)) * DataDepth));
  end {with png.Header}
end;

{Returns pixel when png uses RGB}

//==============================================================================
//
// GetRGBLinePixel
//
//==============================================================================
function GetRGBLinePixel(const png: TPngObject;
  const X, Y: Integer): LongWord;
begin
  with pRGBLine(png.Scanline[Y])^[X] do
    Result := RGB(rgbtRed, rgbtGreen, rgbtBlue)
end;

{Sets pixel when png uses RGB}

//==============================================================================
//
// SetRGBLinePixel
//
//==============================================================================
procedure SetRGBLinePixel(const png: TPngObject;
 const X, Y: Integer; Value: LongWord);
begin
  with pRGBLine(png.Scanline[Y])^[X] do
  begin
    rgbtRed := GetRValue(Value);
    rgbtGreen := GetGValue(Value);
    rgbtBlue := GetBValue(Value)
  end
end;

{Returns pixel when png uses grayscale}

//==============================================================================
//
// GetGrayLinePixel
//
//==============================================================================
function GetGrayLinePixel(const png: TPngObject;
  const X, Y: Integer): LongWord;
var
  B: Byte;
begin
  B := PByteArray(png.Scanline[Y])^[X];
  Result := RGB(B, B, B);
end;

{Sets pixel when png uses grayscale}

//==============================================================================
//
// SetGrayLinePixel
//
//==============================================================================
procedure SetGrayLinePixel(const png: TPngObject;
 const X, Y: Integer; Value: LongWord);
begin
  PByteArray(png.Scanline[Y])^[X] := GetRValue(Value);
end;

{Resizes the PNG image}

//==============================================================================
//
// TPngObject.Resize
//
//==============================================================================
procedure TPngObject.Resize(const CX, CY: Integer);
  function Min(const A, B: Integer): Integer;
  begin
    if A < B then
      Result := A
    else
      Result := B;
  end;
var
  Header: TChunkIHDR;
  Line, NewBytesPerRow: Integer;
  NewHandle: HBitmap;
  NewDC: HDC;
  NewImageData: Pointer;
  NewImageAlpha: Pointer;
  NewImageExtra: Pointer;
begin
  if (CX > 0) and (CY > 0) then
  begin
    {Gets some actual information}
    Header := Self.Header;

    {Creates the new image}
    NewDC := CreateCompatibleDC(Header.ImageDC);
    Header.BitmapInfo.bmiHeader.biWidth := cx;
    Header.BitmapInfo.bmiHeader.biHeight := cy;
    NewHandle := CreateDIBSection(NewDC, pBitmapInfo(@Header.BitmapInfo)^,
      DIB_RGB_COLORS, NewImageData, 0, 0);
    SelectObject(NewDC, NewHandle);
    NewBytesPerRow := (((Header.BitmapInfo.bmiHeader.biBitCount * cx) + 31)
      and not 31) div 8;

    {Copies the image data}
    for Line := 0 to Min(CY - 1, Height - 1) do
      memcpy({$IFDEF FPC}pointer{$ELSE}Ptr{$ENDIF}(Longint(NewImageData) + (Longint(CY) - 1) *
      NewBytesPerRow - (Line * NewBytesPerRow)), Scanline[Line],
      Min(NewBytesPerRow, Header.BytesPerRow));

    {Build array for alpha information, if necessary}
    if (Header.ColorType = COLOR_RGBALPHA) or
      (Header.ColorType = COLOR_GRAYSCALEALPHA) then
    begin
      GetMem(NewImageAlpha, CX * CY);
      FillChar(NewImageAlpha^, CX * CY, 255);
      for Line := 0 to Min(CY - 1, Height - 1) do
        memcpy({$IFDEF FPC}pointer{$ELSE}Ptr{$ENDIF}(Longint(NewImageAlpha) + (Line * CX)),
        AlphaScanline[Line], Min(CX, Width));
      FreeMem(Header.ImageAlpha);
      Header.ImageAlpha := NewImageAlpha;
    end;

    {$IFDEF Store16bits}
    if Header.BitDepth = 16 then
    begin
      GetMem(NewImageExtra, CX * CY);
      ZeroMemory(@NewImageExtra, CX * CY);
      for Line := 0 to Min(CY - 1, Height - 1) do
        memcpy({$IFDEF FPC}pointer{$ELSE}Ptr{$ENDIF}(Longint(NewImageExtra) + (Line * CX)),
        ExtraScanline[Line], Min(CX, Width));
      FreeMem(Header.ExtraImageData);
      Header.ExtraImageData := NewImageExtra;
    end;
    {$ENDIF}

    {Deletes the old image}
    DeleteObject(Header.ImageHandle);
    DeleteDC(Header.ImageDC);

    {Prepares the header to get the new image}
    Header.BytesPerRow := NewBytesPerRow;
    Header.IHDRData.Width := CX;
    Header.IHDRData.Height := CY;
    Header.ImageData := NewImageData;

    {Replaces with the new image}
    Header.ImageHandle := NewHandle;
    Header.ImageDC := NewDC;
  end
  else
    {The new size provided is invalid}
    RaiseError(EInvalidNewSize)
end;

{Sets a pixel}

//==============================================================================
//
// TPngObject.SetPixels
//
//==============================================================================
procedure TPngObject.SetPixels(const X, Y: Integer; const Value: LongWord);
begin
  if ((X >= 0) and (X <= Width - 1)) and
        ((Y >= 0) and (Y <= Height - 1)) then
    with Header do
    begin
      if ColorType in [COLOR_GRAYSCALE, COLOR_PALETTE] then
        SetByteArrayPixel(Self, X, Y, Value)
      else if ColorType in [COLOR_GRAYSCALEALPHA] then
        SetGrayLinePixel(Self, X, Y, Value)
      else
        SetRGBLinePixel(Self, X, Y, Value)
    end {with}
end;

{Returns a pixel}

//==============================================================================
//
// TPngObject.GetPixels
//
//==============================================================================
function TPngObject.GetPixels(const X, Y: Integer): LongWord;
begin
  if ((X >= 0) and (X <= Width - 1)) and
        ((Y >= 0) and (Y <= Height - 1)) then
    with Header do
    begin
      if ColorType in [COLOR_GRAYSCALE, COLOR_PALETTE] then
        Result := GetByteArrayPixel(Self, X, Y)
      else if ColorType in [COLOR_GRAYSCALEALPHA] then
        Result := GetGrayLinePixel(Self, X, Y)
      else
        Result := GetRGBLinePixel(Self, X, Y)
    end {with}
  else
    Result := 0
end;

{Returns the image palette}

//==============================================================================
//
// TPngObject.GetPalette
//
//==============================================================================
function TPngObject.GetPalette: HPalette;
begin
  Result := Header.ImagePalette;
end;

{Assigns from another TChunk}

//==============================================================================
//
// TChunkpHYs.Assign
//
//==============================================================================
procedure TChunkpHYs.Assign(Source: TChunk);
begin
  fPPUnitY := TChunkpHYs(Source).fPPUnitY;
  fPPUnitX := TChunkpHYs(Source).fPPUnitX;
  fUnit := TChunkpHYs(Source).fUnit;
end;

{Loads the chunk from a stream}

//==============================================================================
//
// TChunkpHYs.LoadFromStream
//
//==============================================================================
function TChunkpHYs.LoadFromStream(Stream: TDStream; const ChunkName: TChunkName;
  Size: Integer): Boolean;
begin
  {Let ancestor load the data}
  Result := inherited LoadFromStream(Stream, ChunkName, Size);
  if not Result or (Size <> 9) then
    Exit; {Size must be 9}

  {Reads data}
  fPPUnitX := ByteSwap(pCardinal(Longint(Data))^);
  fPPUnitY := ByteSwap(pCardinal(Longint(Data) + 4)^);
  fUnit := pUnitType(Longint(Data) + 8)^;
end;

{Saves the chunk to a stream}

//==============================================================================
//
// TChunkpHYs.SaveToStream
//
//==============================================================================
function TChunkpHYs.SaveToStream(Stream: TDStream): Boolean;
begin
  {Update data}
  ResizeData(9);  {Make sure the size is 9}
  pCardinal(Data)^ := ByteSwap(fPPUnitX);
  pCardinal(Longint(Data) + 4)^ := ByteSwap(fPPUnitY);
  pUnitType(Longint(Data) + 8)^ := fUnit;

  {Let inherited save data}
  Result := inherited SaveToStream(Stream);
end;

//==============================================================================
//
// TPngObject.DoSetPalette
//
//==============================================================================
procedure TPngObject.DoSetPalette(Value: HPALETTE; const UpdateColors: boolean);
begin
  if Header.HasPalette then
  begin
    {Update the palette entries}
    if UpdateColors then
      Header.PaletteToDIB(Value);

    {Resize the new palette}
    SelectPalette(Header.ImageDC, Value, false);
    RealizePalette(Header.ImageDC);

    {Replaces}
    DeleteObject(Header.ImagePalette);
    Header.ImagePalette := Value;
  end
end;

{Set palette based on a windows palette handle}

//==============================================================================
//
// TPngObject.SetPalette
//
//==============================================================================
procedure TPngObject.SetPalette(palEntries: pLogPalette);
var
  i: integer;
  Value: HPALETTE;
begin
  for i := 0 to palEntries.palNumEntries - 1 do
    PaletteTable[i] := PInteger(@palEntries.palPalEntry[i])^;
  Value := CreatePalette(palEntries^);
  DoSetPalette(Value, true);
end;

//==============================================================================
//
// TPngObject.SetHPalette
//
//==============================================================================
procedure TPngObject.SetHPalette(value: HPALETTE);
begin
  DoSetPalette(Value, true);
end;

{Returns the library version}

//==============================================================================
//
// TPNGObject.GetLibraryVersion
//
//==============================================================================
function TPNGObject.GetLibraryVersion: String;
begin
  Result := PNGLibraryVersion
end;

//==============================================================================
//
// T_IsCommonTransparentColor
//
//==============================================================================
function T_IsCommonTransparentColor(c: LongWord): Boolean;
var
  ca: array[0..2] of byte;
  x00, xFF: integer;
  i: integer;
begin
  for i := 0 to 2 do
  begin
    ca[i] := c;
    c := c shr 8;
  end;

  x00 := 0;
  xFF := 0;

  for i := 0 to 2 do
  begin
    if ca[i] = 0 then
      inc(x00)
    else if ca[i] = $FF then
      inc(xFF)
  end;
  Result := ((x00 + xFF) = 3) and (xFF > 1);
end;

////////////////////////////////////////////////////////////////////////////////
// TPNGBaseTextureManager
////////////////////////////////////////////////////////////////////////////////

//==============================================================================
//
// TPNGBaseTextureManager.Create
//
//==============================================================================
constructor TPNGBaseTextureManager.Create(ext: string);
begin
  inherited Create;
  SetFileExt(ext);
  png := TPngObject.Create;
end;

//==============================================================================
//
// TPNGBaseTextureManager.RGBSwap
//
//==============================================================================
function TPNGBaseTextureManager.RGBSwap(buffer: LongWord): LongWord;
var
  r, g, b: LongWord;
begin
  Result := buffer;
  b := Result and $FF;
  Result := Result shr 8;
  g := Result and $FF;
  Result := Result shr 8;
  r := Result and $FF;
  Result := r + g shl 8 + b shl 16;
end;

//==============================================================================
//
// TPNGBaseTextureManager.CheckPNGError
//
//==============================================================================
function TPNGBaseTextureManager.CheckPNGError: boolean;
var
  pngerr: string;
begin
  pngerr := png.IOresult;
  Result := pngerr = '';
  if not Result then
    I_Warning('TPNGTextureManager(): ' + pngerr + #13#10);
end;

//==============================================================================
//
// TPNGBaseTextureManager.Destroy
//
//==============================================================================
destructor TPNGBaseTextureManager.Destroy;
begin
  png.Free;
  inherited;
end;

////////////////////////////////////////////////////////////////////////////////
// TPNGTextureManager
////////////////////////////////////////////////////////////////////////////////

//==============================================================================
//
// TPNGTextureManager.Create
//
//==============================================================================
constructor TPNGTextureManager.Create;
begin
  inherited Create(PNGEXT);
end;

//==============================================================================
//
// TPNGTextureManager.LoadHeader
//
//==============================================================================
function TPNGTextureManager.LoadHeader(stream: TDStream): boolean;
begin
  png.LoadFromStream(stream);
  FBitmap.SetTransparentColor(RGBSwap(png.GetTransparentColor));
  FBitmap.SetTransparentColor2(RGBSwap(pngtransparentcolor));
  FBitmap.SetTransparentColor3(RGBSwap(pngtransparentcolor2));
  FBitmap.ScaleTo(png.Width, png.Height);
  // JVAL: Support for offsets
  FBitmap.LeftOffset := png.LeftOffset;
  FBitmap.TopOffset := png.TopOffset;

  Result := CheckPNGError;
end;

//==============================================================================
//
// TPNGTextureManager.LoadImage
//
//==============================================================================
function TPNGTextureManager.LoadImage(stream: TDStream): boolean;
var
  x, y: integer;
  buffer: LongWord;
  trcolor: LongWord;
  trcolor2: LongWord;
  trcolor3: LongWord;
  row: pointer;
  i: integer;
  pal: TPalette;
begin
  trcolor := FBitmap.GetTransparentColor;
  trcolor2 := FBitmap.GetTransparentColor2;
  trcolor3 := FBitmap.GetTransparentColor3;
  if (png.header.ColorType = COLOR_PALETTE) and (png.Header.BitDepth = 8) and (png.Header.HasPalette) then
  begin
    FBitmap^.SetBytesPerPixel(1);
    for i := 0 to 255 do
    begin
      buffer := RGBSwap(png.PaletteTable[i]);
      if (buffer = trcolor) or (buffer = trcolor2) or (buffer = trcolor3) or (assumecommontranspantcolors and T_IsCommonTransparentColor(buffer)) then
        pal[i] := 0
      else
        pal[i] := buffer;
    end;
    FBitmap.SetPalette(@pal, 256, 0, 0);
    row := FBitmap.GetImage;
    for i := 0 to png.Height - 1 do
    begin
      memcpy(row, png.Scanline[i], png.Width);
      row := pointer(integer(row) + png.Width);
    end;
  end
  else
  begin
    FBitmap^.SetBytesPerPixel(4);
    for x := 0 to png.Width - 1 do
      for y := 0 to png.Height - 1 do
      begin
        buffer := RGBSwap(png.Pixels[x, y]);
        if (buffer = trcolor) or (buffer = trcolor2) or (buffer = trcolor3) or (assumecommontranspantcolors and T_IsCommonTransparentColor(buffer)) then
          buffer := 0;
        FBitmap^.PutPixels(x, y, 1, @buffer, 32);
      end;
    FBitmap.SetTransparentColor(0);
    FBitmap.SetTransparentColor2(0);
    FBitmap.SetTransparentColor3(0);
  end;
  Result := CheckPNGError;
end;

////////////////////////////////////////////////////////////////////////////////
// TPNGSpriteTextureManager
////////////////////////////////////////////////////////////////////////////////

//==============================================================================
//
// TPNGSpriteTextureManager.Create
//
//==============================================================================
constructor TPNGSpriteTextureManager.Create;
begin
  ftransparentcolor := 0;
  inherited Create(PNGSPRITEEXT);
end;

//==============================================================================
//
// TPNGSpriteTextureManager.LoadHeader
//
//==============================================================================
function TPNGSpriteTextureManager.LoadHeader(stream: TDStream): boolean;
begin
  png.LoadFromStream(stream);
  ftransparentcolor := RGBSwap(png.GetTransparentColor);
  FBitmap.SetTransparentColor(0);
  FBitmap.SetTransparentColor2(0);
  FBitmap.SetTransparentColor3(0);
  FBitmap.ScaleTo(png.Width, png.Height);
  // JVAL: Support for offsets
  FBitmap.LeftOffset := png.LeftOffset;
  FBitmap.TopOffset := png.TopOffset;

  Result := CheckPNGError;
end;

//==============================================================================
//
// TPNGSpriteTextureManager.LoadImage
//
//==============================================================================
function TPNGSpriteTextureManager.LoadImage(stream: TDStream): boolean;
var
  x, y: integer;
  buffer: LongWord;
  row: pointer;
  pb: PByteArray;
  pl: PLongWordArray;
  palpha: PByteArray;
  i: integer;
  pal: TPalette;
  nearblack: Cardinal;
begin
  if ftransparentcolor = $010100 then
    nearblack := $000100
  else
    nearblack := $010100;

  if (png.header.ColorType = COLOR_PALETTE) and (png.Header.BitDepth = 8) and (png.Header.HasPalette) then
  begin
    // JVAL: tRNS palette resolve in 8 bit color images
    if png.HastRNS256 then
    begin
      for i := 0 to 255 do
      begin
        buffer := RGBSwap(png.PaletteTable[i]);
        if png.tRNSArray256[i] = 0 then
          pal[i] := 0
        else if (buffer = ftransparentcolor) or (buffer = 0) then
          pal[i] := 0
        else
          pal[i] := buffer;
      end;

      FBitmap^.SetBytesPerPixel(4);
      pl := FBitmap.GetImage;
      for y := 0 to png.Height - 1 do
      begin
        pb := png.Scanline[y];
        for i := 0 to png.Width - 1 do
        begin
          if png.tRNSArray256[pb[i]] = 0 then
            pl[i] := 0
          else
          begin
            pl[i] := pal[pb[i]];
            if (pl[i] = ftransparentcolor) or (pl[i] and $FFFFFF = 0) then
              pl[i] := nearblack;
          end;
        end;
        pl := @pl[png.Width];
      end;
    end
    else
    begin
      FBitmap^.SetBytesPerPixel(1);
      for i := 0 to 255 do
      begin
        buffer := RGBSwap(png.PaletteTable[i]);
        if buffer = ftransparentcolor then
          pal[i] := 0
        else if buffer = 0 then
          pal[i] := nearblack
        else
          pal[i] := buffer;
      end;
      FBitmap.SetPalette(@pal, 256, 0, 0);
      row := FBitmap.GetImage;
      for i := 0 to png.Height - 1 do
      begin
        memcpy(row, png.Scanline[i], png.Width);
        row := pointer(integer(row) + png.Width);
      end;
    end;
  end
  else
  begin
    FBitmap^.SetBytesPerPixel(4);
    for y := 0 to png.Height - 1 do
    begin
      palpha := png.AlphaScanline[y];
      if palpha <> nil then
      begin
        for x := 0 to png.Width - 1 do
        begin
          if palpha[x] = 0 then
            buffer := 0
          else
          begin
            buffer := RGBSwap(png.Pixels[x, y]);
            if (buffer = ftransparentcolor) or (buffer and $FFFFFF = 0) then
              buffer := nearblack;
          end;
          FBitmap^.PutPixels(x, y, 1, @buffer, 32);
        end;
      end
      else
      begin
        for x := 0 to png.Width - 1 do
        begin
          buffer := RGBSwap(png.Pixels[x, y]);
          if buffer <> ftransparentcolor then
            if buffer and $FFFFFF = 0 then
              buffer := nearblack;
          FBitmap^.PutPixels(x, y, 1, @buffer, 32);
        end;
      end;
    end;
  end;
  Result := CheckPNGError;
end;
////////////////////////////////////////////////////////////////////////////////

end.

