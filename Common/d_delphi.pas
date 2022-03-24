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
//    Delphi specific routines
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit d_delphi;

interface

type
  PPointer = ^Pointer;

  PString = ^string;

  PBoolean = ^Boolean;

  PInteger = ^Integer;

  PLongWord = ^LongWord;

  PShortInt = ^ShortInt;

  TWordArray = packed array[0..$7FFF] of word;
  PWordArray = ^TWordArray;

  TIntegerArray = packed array[0..$7FFF] of integer;
  PIntegerArray = ^TIntegerArray;

  TLongWordArray = packed array[0..$7FFF] of LongWord;
  PLongWordArray = ^TLongWordArray;

  TSmallintArray = packed array[0..$7FFF] of Smallint;
  PSmallintArray = ^TSmallintArray;

  TByteArray = packed array[0..$7FFF] of Byte;
  PByteArray = ^TByteArray;

  TShortIntArray = packed array[0..$7FFF] of ShortInt;
  PShortIntArray = ^TShortIntArray;

  TBooleanArray = packed array[0..$7FFF] of boolean;
  PBooleanArray = ^TBooleanArray;

  PProcedure = procedure;
  PPointerParmProcedure = procedure(const p: pointer);
  PIntFunction = function: integer;

  TStringArray = array[0..$7FFF] of string;
  PStringArray = ^TStringArray;

  TPointerArray = packed array[0..$7FFF] of pointer;
  PPointerArray = ^TPointerArray;

  PSmallInt = ^SmallInt;
  TSmallIntPArray = packed array[0..$7FFF] of PSmallIntArray;
  PSmallIntPArray = ^TSmallIntPArray;

  PWord = ^Word;
  TWordPArray = packed array[0..$7FFF] of PWordArray;
  PWordPArray = ^TWordPArray;

  TLongWordPArray = packed array[0..$7FFF] of PLongWordArray;
  PLongWordPArray = ^TLongWordPArray;

  TIntegerPArray = packed array[0..$7FFF] of PIntegerArray;
  PIntegerPArray = ^TIntegerPArray;

  PByte = ^Byte;
  TBytePArray = packed array[0..$7FFF] of PByteArray;
  PBytePArray = ^TBytePArray;

  float = single;
  Pfloat = ^float;
  TFloatArray = packed array[0..$7FFF] of float;
  PFloatArray = ^TFloatArray;

  TObjectArray = packed array[0..$7FFF] of TObject;
  PObjectArray = ^TObjectArray;

type
  charset_t = set of char;

  twobytes_t = packed record
    byte1, byte2: byte;
  end;
  Ptwobytes_t = ^twobytes_t;

  fourbytes_t = packed record
    byte1, byte2, byte3, byte4: byte;
  end;
  Pfourbytes_t = ^fourbytes_t;

  eightbytes_t = packed record
    byte1, byte2, byte3, byte4: byte;
    byte5, byte6, byte7, byte8: byte;
  end;
  Peightbytes_t = ^eightbytes_t;

  twointegers_t = packed record
    int1, int2: integer;
  end;
  Ptwointegers_t = ^twointegers_t;

  twolongwords_t = packed record
    longword1, longword2: LongWord;
  end;
  Ptwolongwords_t = ^twolongwords_t;

  TOutProc = procedure (const s: string);

var
  outproc: TOutProc = nil;

//==============================================================================
//
// sprintf
//
//==============================================================================
procedure sprintf(var s: string; const Fmt: string; const Args: array of const);

//==============================================================================
//
// sfmt
//
//==============================================================================
function sfmt(const Fmt: string; const Args: array of const): string;

//==============================================================================
//
// printf
//
//==============================================================================
procedure printf(const str: string); overload;

//==============================================================================
//
// printf
//
//==============================================================================
procedure printf(const Fmt: string; const Args: array of const); overload;

//==============================================================================
//
// itoa
//
//==============================================================================
function itoa(i: integer): string;

//==============================================================================
//
// uitoa
//
//==============================================================================
function uitoa(l: LongWord): string;

//==============================================================================
//
// ftoa
//
//==============================================================================
function ftoa(f: single): string;

//==============================================================================
//
// ftoafmt
//
//==============================================================================
function ftoafmt(const fmt: string; f: single): string;

//==============================================================================
//
// atoi
//
//==============================================================================
function atoi(const s: string): integer; overload;

//==============================================================================
//
// atoi
//
//==============================================================================
function atoi(const s: string; const default: integer): integer; overload;

//==============================================================================
//
// atoui
//
//==============================================================================
function atoui(const s: string): LongWord; overload;

//==============================================================================
//
// atoui
//
//==============================================================================
function atoui(const s: string; const default: LongWord): LongWord; overload;

//==============================================================================
//
// atof
//
//==============================================================================
function atof(const s: string): single; overload;

//==============================================================================
//
// atof
//
//==============================================================================
function atof(const s: string; const default: single): single; overload;

//==============================================================================
// memmove
//
// Memory functions
//
//==============================================================================
function memmove(const destination, source: pointer; count: integer): pointer;

//==============================================================================
//
// memcpy
//
//==============================================================================
procedure memcpy(const dest0: pointer; const src0: pointer; count0: integer);

//==============================================================================
//
// memset
//
//==============================================================================
function memset(const dest0: pointer; const val: integer; const count0: integer): pointer;

//==============================================================================
//
// memsetsi
//
//==============================================================================
function memsetsi(const dest0: pointer; const val: smallint; count0: integer): pointer;

//==============================================================================
//
// memseti
//
//==============================================================================
function memseti(const dest0: pointer; const val: integer; const count0: integer): pointer;

//==============================================================================
//
// malloc
//
//==============================================================================
function malloc(const size: integer): Pointer;

//==============================================================================
//
// mallocA
//
//==============================================================================
function mallocA(var Size: integer; const Align: integer; var original: pointer): pointer;

//==============================================================================
//
// mallocz
//
//==============================================================================
function mallocz(const size: integer): Pointer;

//==============================================================================
//
// realloc
//
//==============================================================================
procedure realloc(var p: pointer; const oldsize, newsize: integer);

//==============================================================================
//
// memfree
//
//==============================================================================
procedure memfree(var p: pointer; const size: integer);

var
  memoryusage: integer = 0;

//==============================================================================
//
// IntToStrZfill
//
//==============================================================================
function IntToStrZfill(const z: integer; const x: integer): string;

//==============================================================================
//
// intval
//
//==============================================================================
function intval(const b: boolean): integer;

//==============================================================================
//
// decide
//
//==============================================================================
function decide(const condition: boolean;
  const iftrue: integer; const iffalse: integer): integer; overload;

//==============================================================================
//
// decide
//
//==============================================================================
function decide(const condition: boolean;
  const iftrue: boolean; const iffalse: boolean): boolean; overload;

//==============================================================================
//
// decide
//
//==============================================================================
function decide(const condition: boolean;
  const iftrue: string; const iffalse: string): string; overload;

//==============================================================================
//
// decide
//
//==============================================================================
function decide(const condition: boolean;
  const iftrue: pointer; const iffalse: pointer): pointer; overload;

//==============================================================================
//
// decide
//
//==============================================================================
function decide(const condition: integer;
  const iftrue: integer; const iffalse: integer): integer; overload;

//==============================================================================
//
// decide
//
//==============================================================================
function decide(const condition: integer;
  const iftrue: boolean; const iffalse: boolean): boolean; overload;

//==============================================================================
//
// decide
//
//==============================================================================
function decide(const condition: integer;
  const iftrue: string; const iffalse: string): string; overload;

//==============================================================================
//
// decide
//
//==============================================================================
function decide(const condition: integer;
  const iftrue: pointer; const iffalse: pointer): pointer; overload;

//==============================================================================
//
// decidef
//
//==============================================================================
function decidef(const condition: boolean;
  const iftrue: single; const iffalse: single): single;

//==============================================================================
//
// incp
//
//==============================================================================
function incp(var p: pointer; const size: integer = 1): pointer;

//==============================================================================
//
// pDiff
//
//==============================================================================
function pDiff(const p1, p2: pointer; const size: integer): integer;

//==============================================================================
//
// getenv
//
//==============================================================================
function getenv(const env: string): string;

//==============================================================================
//
// fexists
//
//==============================================================================
function fexists(const filename: string): boolean;

//==============================================================================
//
// direxists
//
//==============================================================================
function direxists(const dirname: string): boolean;

//==============================================================================
//
// fexpand
//
//==============================================================================
function fexpand(const filename: string): string;

//==============================================================================
//
// fpath
//
//==============================================================================
function fpath(const filename: string): string;

//==============================================================================
//
// fdelete
//
//==============================================================================
procedure fdelete(const filename: string);

//==============================================================================
//
// frename
//
//==============================================================================
procedure frename(const src, dest: string);

//==============================================================================
//
// fext
//
//==============================================================================
function fext(const filename: string): string;

//==============================================================================
//
// fname
//
//==============================================================================
function fname(const filename: string): string;

const
  fCreate = 0;
  fOpenReadOnly = 1;
  fOpenReadWrite = 2;

  sFromBeginning = 0;
  sFromCurrent = 1;
  sFromEnd = 2;

type
  TDStream = class
  protected
    FIOResult: integer;
  public
    OnBeginBusy: PProcedure;
    OnEndBusy: PProcedure;
    constructor Create;
    function Read(var Buffer; Count: Longint): Longint; virtual; abstract;
    function Write(const Buffer; Count: Longint): Longint; virtual; abstract;
    function Seek(Offset: Longint; Origin: Word): Longint; virtual; abstract;
    function Size: Longint; virtual; abstract;
    function Position: integer; virtual; abstract;
    function IOResult: integer;
  end;

  TDMemoryStream = class(TDStream)
  private
    FSize: integer;
    FRealSize: integer;
    FPosition: integer;
    FMemory: pointer;
  protected
    procedure Resize(newsize: integer); virtual;
  public
    OnBeginBusy: PProcedure;
    OnEndBusy: PProcedure;
    constructor Create;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Size: Longint; override;
    function Position: integer; override;
    property Memory: pointer read FMemory;
  end;

  TAttachableMemoryStream = class(TDStream)
  protected
    FSize: integer;
    FPosition: integer;
    FMemory: pointer;
  public
    OnBeginBusy: PProcedure;
    OnEndBusy: PProcedure;
    constructor Create;
    destructor Destroy; override;
    procedure Attach(const amemory: pointer; const asize: integer); virtual;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Size: Longint; override;
    function Position: integer; override;
  end;

  TFile = class(TDStream)
  private
    f: file;
  public
    constructor Create(const FileName: string; const mode: integer);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Size: Longint; override;
    function Position: integer; override;
  end;

  TCachedFile = class(TFile)
  private
    fBufSize: integer;
    fBuffer: pointer;
    fPosition: integer;
    fBufferStart: integer;
    fBufferEnd: integer;
    fSize: integer;
    fInitialized: boolean;
  protected
    procedure SetSize(NewSize: Longint); virtual;
    procedure ResetBuffer; virtual;
  public
    constructor Create(const FileName: string; mode: word; ABufSize: integer = $FFFF); virtual;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Position: integer; override;
  end;

type
  TDByteList = class
  private
    fList: PByteArray;
    fNumItems: integer;
    fRealNumItems: integer;
  protected
    function Get(Index: Integer): byte; virtual;
    procedure Put(Index: Integer; const value: byte); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const value: byte): integer; overload; virtual;
    function Delete(const Index: integer): boolean;
    function IndexOf(const value: byte): integer; virtual;
    procedure Clear;
    procedure FastClear;
    property Count: integer read fNumItems;
    property Bytes[Index: Integer]: byte read Get write Put; default;
    property List: PByteArray read fList;
  end;

  TDNumberList = class
  private
    fList: PIntegerArray;
    fNumItems: integer;
    fRealNumItems: integer;
  protected
    function Get(Index: Integer): integer; virtual;
    procedure Put(Index: Integer; const value: integer); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const value: integer): integer; overload; virtual;
    procedure Add(const nlist: TDNumberList); overload; virtual;
    function Delete(const Index: integer): boolean;
    function IndexOf(const value: integer): integer; virtual;
    procedure Clear;
    procedure FastClear;
    procedure Sort; virtual;
    function Sum: integer;
    property Count: integer read fNumItems;
    property Numbers[Index: Integer]: integer read Get write Put; default;
    property List: PIntegerArray read fList;
  end;

type
  twointeger_t = record
    num1, num2: integer;
  end;
  twointeger_tArray = array[0..$FFFF] of twointeger_t;
  Ptwointeger_tArray = ^twointeger_tArray;

  T2DNumberList = class
  private
    fList: Ptwointeger_tArray;
    fNumItems: integer;
    fRealNumItems: integer;
  protected
    function Get(Index: Integer): twointeger_t; virtual;
    procedure Put(Index: Integer; const value: twointeger_t); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const value1, value2: integer): integer; overload; virtual;
    function Add(const value: twointeger_t): integer; overload; virtual;
    procedure Add(const nlist: T2DNumberList); overload; virtual;
    function Delete(const Index: integer): boolean;
    function IndexOf(const value1, value2: integer): integer; virtual;
    procedure Clear;
    procedure FastClear;
    property Count: integer read fNumItems;
    property Numbers[Index: Integer]: twointeger_t read Get write Put; default;
    property List: Ptwointeger_tArray read fList;
  end;

  TDLimitNumberList = class(TDNumberList)
  private
    fLimit: integer;
  public
    constructor Create; override;
    constructor CreateLimited(const v: Integer); virtual;
    function Add(const value: integer): integer; overload; override;
  end;

  TDFloatList = class
  private
    fList: PFloatArray;
    fNumItems: integer;
  protected
    function Get(Index: Integer): float; virtual;
    procedure Put(Index: Integer; const value: float); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const value: float): integer; overload; virtual;
    procedure Add(const nlist: TDFloatList); overload; virtual;
    function Delete(const Index: integer): boolean; virtual;
    function IndexOf(const value: float): integer; virtual;
    procedure Clear; virtual;
    procedure Sort; virtual;
    function Sum: float;
    property Count: integer read fNumItems;
    property Floats[Index: Integer]: float read Get write Put; default;
    property List: PFloatArray read fList;
  end;

  TDPointerList = class
  private
    fList: PPointerArray;
    fNumItems: integer;
    fRealNumItems: integer;
  protected
    function Get(Index: Integer): pointer; virtual;
    procedure Put(Index: Integer; const value: pointer); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const value: pointer): integer; overload; virtual;
    procedure Add(const nlist: TDPointerList); overload; virtual;
    function Delete(const Index: integer): boolean;
    function IndexOf(const value: pointer): integer; virtual;
    procedure Clear;
    procedure FastClear;
    property Count: integer read fNumItems;
    property Pointers[Index: Integer]: pointer read Get write Put; default;
    property List: PPointerArray read fList;
  end;

const
  NLHASHSIZE = 2048;

type
  TDHashNumberList = class(TDNumberList)
  private
    fhash: array[0..NLHASHSIZE - 1] of integer;
    fsorted: boolean;
    procedure CreateHashTable;
  protected
    procedure Put(Index: Integer; const value: integer); override;
  public
    constructor Create; override;
    function IndexOf(const value: integer): integer; override;
    procedure Sort; override;
    property sorted: boolean read fsorted;
  end;

type
  TTextArray = array[0..$FFFF] of string[255];
  PTextArray = ^TTextArray;

type
  TDTextList = class
  private
    fList: PTextArray;
    fNumItems: integer;
  protected
    function Get(Index: Integer): string; virtual;
    procedure Put(Index: Integer; const value: string); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const value: string): integer; overload; virtual;
    procedure Add(const nlist: TDTextList); overload; virtual;
    function Delete(const Index: integer): boolean;
    function IndexOf(const value: string): integer;
    procedure Clear;
    property Count: integer read fNumItems;
    property Strings[Index: Integer]: string read Get write Put; default;
  end;

const
  MaxListSize = MAXINT div 16;

type
{ TDStrings class }

  TDStrings = class
  private
    function GetCommaText: string;
    function GetName(Index: Integer): string;
    function GetValue(const Name: string): string;
    function GetValueIdx(const idx: integer): string;
    procedure SetCommaText(const Value: string);
    procedure SetValue(const Name, Value: string);
  protected
    function Get(Index: Integer): string; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: string; virtual;
    procedure Put(Index: Integer; const S: string); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetTextStr(const Value: string); virtual;
    procedure SetByteStr(const A: PByteArray; const Size: integer); virtual;
  public
    function Add(const S: string): Integer; overload; virtual;
    function Add(const Fmt: string; const Args: array of const): Integer; overload; virtual;
    function AddObject(const S: string; AObject: TObject): Integer; virtual;
    procedure Append(const S: string);
    procedure AddStrings(Strings: TDStrings); virtual;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    function Equals(Strings: TDStrings): Boolean;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetText: PChar; virtual;
    function IndexOf(const S: string): Integer; virtual;
    function IndexOfName(const Name: string): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    procedure Insert(Index: Integer; const S: string); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: string;
      AObject: TObject);
    function LoadFromFile(const FileName: string): boolean; virtual;
    function LoadFromStream(const strm: TDStream): boolean; virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    function SaveToFile(const FileName: string): boolean; virtual;
    procedure SetText(Text: PChar); virtual;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: string read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Names[Index: Integer]: string read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Values[const Name: string]: string read GetValue write SetValue;
    property ValuesIdx[const idx: integer]: string read GetValueIdx;
    property Strings[Index: Integer]: string read Get write Put; default;
    property Text: string read GetTextStr write SetTextStr;
  end;

{ TDStringList class }

  TDStringList = class;

  PStringItem = ^TStringItem;
  TStringItem = record
    FString: string;
    FObject: TObject;
  end;

  PStringItemList = ^TStringItemList;
  TStringItemList = array[0..MaxListSize] of TStringItem;

  TDStringList = class(TDStrings)
  private
    FList: PStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure InsertItem(Index: Integer; const S: string);
  protected
    function Get(Index: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
  public
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;

//==============================================================================
//
// findfile
//
//==============================================================================
function findfile(const mask: string): string;

//==============================================================================
//
// findfiles
//
//==============================================================================
function findfiles(const mask: string): TDStringList;

//==============================================================================
//
// fprintf
//
//==============================================================================
procedure fprintf(var f: file; const str: string); overload;

//==============================================================================
//
// fprintf
//
//==============================================================================
procedure fprintf(var f: file; const Fmt: string; const Args: array of const); overload;

//==============================================================================
//
// fprintf
//
//==============================================================================
procedure fprintf(const f: TFile; const str: string); overload;

//==============================================================================
//
// fprintf
//
//==============================================================================
procedure fprintf(const f: TFile; const Fmt: string; const Args: array of const); overload;

//==============================================================================
//
// tan
//
//==============================================================================
function tan(const x: extended): extended;

//==============================================================================
//
// strcmp
//
//==============================================================================
function strcmp(const s1, s2: string): integer;

//==============================================================================
//
// strupper
//
//==============================================================================
function strupper(const S: string): string;

//==============================================================================
//
// strupperproc
//
//==============================================================================
procedure strupperproc(var S: string);

//==============================================================================
//
// strlower
//
//==============================================================================
function strlower(const S: string): string;

//==============================================================================
//
// strlowerproc
//
//==============================================================================
procedure strlowerproc(var S: string);

//==============================================================================
//
// toupper
//
//==============================================================================
function toupper(ch: Char): Char;

//==============================================================================
//
// tolower
//
//==============================================================================
function tolower(ch: Char): Char;

//==============================================================================
//
// strremovespaces
//
//==============================================================================
function strremovespaces(const s: string): string;

//==============================================================================
//
// _SHL
//
//==============================================================================
function _SHL(const x: integer; const bits: integer): integer;

//==============================================================================
//
// _SHLW
//
//==============================================================================
function _SHLW(const x: LongWord; const bits: LongWord): LongWord;

//==============================================================================
//
// _SHR
//
//==============================================================================
function _SHR(const x: integer; const bits: integer): integer;

//==============================================================================
//
// _SHR1
//
//==============================================================================
function _SHR1(const x: integer): integer;

//==============================================================================
//
// _SHR2
//
//==============================================================================
function _SHR2(const x: integer): integer;

//==============================================================================
//
// _SHR3
//
//==============================================================================
function _SHR3(const x: integer): integer;

//==============================================================================
//
// _SHR4
//
//==============================================================================
function _SHR4(const x: integer): integer;

//==============================================================================
//
// _SHR5
//
//==============================================================================
function _SHR5(const x: integer): integer;

//==============================================================================
//
// _SHR7
//
//==============================================================================
function _SHR7(const x: integer): integer;

//==============================================================================
//
// _SHR8
//
//==============================================================================
function _SHR8(const x: integer): integer;

//==============================================================================
//
// _SHR11
//
//==============================================================================
function _SHR11(const x: integer): integer;

//==============================================================================
//
// _SHR14
//
//==============================================================================
function _SHR14(const x: integer): integer;

//==============================================================================
//
// _SHRW
//
//==============================================================================
function _SHRW(const x: LongWord; const bits: LongWord): LongWord;

//==============================================================================
//
// StringVal
//
//==============================================================================
function StringVal(const Str: PChar): string;

//==============================================================================
//
// ZeroMemory
//
//==============================================================================
procedure ZeroMemory(const dest0: pointer; const count0: integer);

//==============================================================================
//
// fopen
//
//==============================================================================
function fopen(var f: file; const FileName: string; const mode: integer): boolean;

//==============================================================================
//
// fwrite
//
//==============================================================================
function fwrite(const data: pointer; const sz1, sz2: integer; var f: file): boolean;

//==============================================================================
//
// fsize
//
//==============================================================================
function fsize(const FileName: string): integer;

//==============================================================================
//
// fshortname
//
//==============================================================================
function fshortname(const FileName: string): string;

//==============================================================================
//
// fixslashpath
//
//==============================================================================
function fixslashpath(const apath: string): string;

//==============================================================================
//
// strtrim
//
//==============================================================================
function strtrim(const S: string): string;

//==============================================================================
//
// capitalizedstring
//
//==============================================================================
function capitalizedstring(const S: string; const splitter: char = ' '): string;

//==============================================================================
//
// splitstring
//
//==============================================================================
procedure splitstring(const inp: string; var out1, out2: string; const splitter: string = ' '); overload;

//==============================================================================
//
// splitstring
//
//==============================================================================
procedure splitstring(const inp: string; var out1, out2: string; const splitters: charset_t); overload;

//==============================================================================
//
// trimproc
//
//==============================================================================
procedure trimproc(var s: string);

//==============================================================================
//
// trimprocU
//
//==============================================================================
procedure trimprocU(var s: string);

//==============================================================================
//
// splitstring_ch
//
//==============================================================================
procedure splitstring_ch(const inp: string; var out1, out2: string; const splitter: char = ' ');

//==============================================================================
//
// firstword
//
//==============================================================================
function firstword(const inp: string; const splitter: string = ' '): string; overload;

//==============================================================================
//
// firstword_ch
//
//==============================================================================
function firstword_ch(const inp: string; const splitter: char = ' '): string;

//==============================================================================
//
// firstword
//
//==============================================================================
function firstword(const inp: string; const splitters: charset_t): string; overload;

//==============================================================================
//
// parsefirstword
//
//==============================================================================
function parsefirstword(const inp: string): string;

//==============================================================================
//
// secondword
//
//==============================================================================
function secondword(const inp: string; const splitter: string = ' '): string; overload;

//==============================================================================
//
// secondword
//
//==============================================================================
function secondword(const inp: string; const splitters: charset_t): string; overload;

//==============================================================================
//
// lastword
//
//==============================================================================
function lastword(const inp: string; const splitter: string = ' '): string; overload;

//==============================================================================
//
// lastword
//
//==============================================================================
function lastword(const inp: string; const splitters: charset_t): string; overload;

//==============================================================================
//
// FreeAndNil
//
//==============================================================================
procedure FreeAndNil(var Obj);

//==============================================================================
//
// StrLCopy
//
//==============================================================================
function StrLCopy(Dest: PChar; const Source: PChar; MaxLen: Cardinal): PChar;

//==============================================================================
//
// fabs
//
//==============================================================================
function fabs(const f: float): float;

//==============================================================================
//
// MakeDir
//
//==============================================================================
procedure MakeDir(const dir: string);

//==============================================================================
//
// PascalText
//
//==============================================================================
function PascalText(src: PChar): string; overload;

//==============================================================================
//
// PascalText
//
//==============================================================================
function PascalText(src: PChar; const maxsize: integer): string; overload;

//==============================================================================
//
// CopyFile
//
//==============================================================================
procedure CopyFile(const sname, dname: string);

//==============================================================================
//
// CopyFile2
//
//==============================================================================
procedure CopyFile2(const FromN, ToN: string);

//==============================================================================
//
// IsIntegerInRange
//
//==============================================================================
function IsIntegerInRange(const test, f1, f2: integer): boolean;

//==============================================================================
//
// IsLongWordInRange
//
//==============================================================================
function IsLongWordInRange(const test, f1, f2: LongWord): boolean;

//==============================================================================
//
// IsFloatInRange
//
//==============================================================================
function IsFloatInRange(const test, f1, f2: float): boolean;

//==============================================================================
//
// IsDoubleInRange
//
//==============================================================================
function IsDoubleInRange(const test, f1, f2: double): boolean;

//==============================================================================
//
// IsExtendedInRange
//
//==============================================================================
function IsExtendedInRange(const test, f1, f2: Extended): boolean;

//==============================================================================
//
// GetIntegerInRange
//
//==============================================================================
function GetIntegerInRange(const val, f1, f2: integer): integer;

//==============================================================================
//
// GetInt64InRange
//
//==============================================================================
function GetInt64InRange(const val, f1, f2: int64): integer;

var
  mmxMachine: byte = 0;
  AMD3DNowMachine: byte = 0;

type
  union_8b = record
    case integer of
      1: (bytes: array[0..7] of byte);
      2: (words: array[0..3] of word);
      3: (dwords: array[0..1] of LongWord);
      4: (smallints: array[0..3] of word);
  end;

//==============================================================================
//
// GetAllocMemSize
//
//==============================================================================
function GetAllocMemSize: integer;

//==============================================================================
//
// MkDir
//
//==============================================================================
function MkDir(const d: string): boolean;

type
  TString = class
    str: string;
    constructor Create(const astring: string);
  end;

  TInteger = class
    intnum: integer;
    constructor Create(const aint: integer);
  end;

  TFloat = class
    floatnum: float;
    constructor Create(const aflt: float);
  end;

//==============================================================================
//
// MinI
//
//==============================================================================
function MinI(const a, b: integer): integer;

//==============================================================================
//
// MaxI
//
//==============================================================================
function MaxI(const a, b: integer): integer;

//==============================================================================
//
// NowTime
//
//==============================================================================
function NowTime: TDateTime;

//==============================================================================
//
// formatDateTimeAsString
//
//==============================================================================
function formatDateTimeAsString(const Format: string; DateTime: TDateTime): string;

//==============================================================================
//
// QSortIntegers
//
//==============================================================================
procedure QSortIntegers(const A: PIntegerArray; const Len: integer);

//==============================================================================
//
// QSortFloats
//
//==============================================================================
procedure QSortFloats(const A: PFloatArray; const Len: integer);

//==============================================================================
//
// StrIsInteger
//
//==============================================================================
function StrIsInteger(const s: string): Boolean;

//==============================================================================
//
// StrIsLongWord
//
//==============================================================================
function StrIsLongWord(const s: string): Boolean;

//==============================================================================
//
// StrIsFloat
//
//==============================================================================
function StrIsFloat(const s: string): Boolean;

//==============================================================================
//
// SaveStreamToFile
//
//==============================================================================
function SaveStreamToFile(const s: TDStream; const fname: string): boolean;

//==============================================================================
//
// ibetween
//
//==============================================================================
function ibetween(const x: integer; const x1, x2: integer): integer;

//==============================================================================
//
// LeftStr
//
//==============================================================================
function LeftStr(const s: string; const l: integer): string;

//==============================================================================
//
// RightStr
//
//==============================================================================
function RightStr(const s: string; const l: integer): string;

//==============================================================================
//
// fixpathname
//
//==============================================================================
function fixpathname(const s: string): string;

//==============================================================================
//
// logtofile
//
//==============================================================================
procedure logtofile(const fname: string; const str: string);

//==============================================================================
//
// wordstolist
//
//==============================================================================
function wordstolist(const inp: string; const splitters: charset_t): TDStringList;

//==============================================================================
//
// RemoveQuotesFromString
//
//==============================================================================
function RemoveQuotesFromString(const s: string): string;

//==============================================================================
//
// isdigit
//
//==============================================================================
function isdigit(const c: char): boolean;

//==============================================================================
//
// Isign
//
//==============================================================================
function Isign(const x: integer): integer;

//==============================================================================
//
// readablestring
//
//==============================================================================
function readablestring(const s: string): string;

//==============================================================================
// CharPos
//
// Idea from "The Tomes of Delphi™ Algorithms and Data Structures - Pg 18
//
//==============================================================================
function CharPos(const ch: Char; const s: string): integer;

function IsZeroes(const p: Pointer; const size: integer): boolean;

procedure FillDWord(const dest: Pointer; Count: Integer; Value: LongWord); assembler; register;

implementation

uses
  Windows,
  SysUtils
{$IFDEF FPC}
  ,d_fpc
{$ENDIF}
  ;

//==============================================================================
//
// sprintf
//
//==============================================================================
procedure sprintf(var s: string; const Fmt: string; const Args: array of const);
begin
  FmtStr(s, Fmt, Args);
end;

//==============================================================================
//
// sfmt
//
//==============================================================================
function sfmt(const Fmt: string; const Args: array of const): string;
begin
  FmtStr(result, Fmt, Args);
end;

//==============================================================================
//
// printf
//
//==============================================================================
procedure printf(const str: string);
begin
  if Assigned(outproc) then
    outproc(str)
  else if IsConsole then
    write(str);
end;

//==============================================================================
//
// printf
//
//==============================================================================
procedure printf(const Fmt: string; const Args: array of const);
var
  s: string;
begin
  sprintf(s, Fmt, Args);
  printf(s);
end;

//==============================================================================
//
// fprintf
//
//==============================================================================
procedure fprintf(var f: file; const str: string);
begin
  {$I-}
  BlockWrite(f, (@str[1])^, Length(str));
  {$I+}
end;

//==============================================================================
//
// fprintf
//
//==============================================================================
procedure fprintf(var f: file; const Fmt: string; const Args: array of const);
var
  s: string;
begin
  sprintf(s, Fmt, Args);
  fprintf(f, s);
end;

//==============================================================================
//
// fprintf
//
//==============================================================================
procedure fprintf(const f: TFile; const str: string);
begin
  if f <> nil then
    fprintf(f.f, str);
end;

//==============================================================================
//
// fprintf
//
//==============================================================================
procedure fprintf(const f: TFile; const Fmt: string; const Args: array of const);
begin
  if f <> nil then
    fprintf(f.f, Fmt, Args);
end;

//==============================================================================
//
// itoa
//
//==============================================================================
function itoa(i: integer): string;
begin
  sprintf(result, '%d', [i]);
end;

//==============================================================================
//
// uitoa
//
//==============================================================================
function uitoa(l: LongWord): string;
begin
  sprintf(result, '%d', [l]);
end;

//==============================================================================
//
// ftoa
//
//==============================================================================
function ftoa(f: single): string;
begin
  ThousandSeparator := #0;
  DecimalSeparator := '.';

  result := FloatToStr(f);
end;

//==============================================================================
//
// ftoafmt
//
//==============================================================================
function ftoafmt(const fmt: string; f: single): string;
begin
  ThousandSeparator := #0;
  DecimalSeparator := '.';

  sprintf(result, '%' + fmt + 'f', [f]);
end;

//==============================================================================
//
// atoi
//
//==============================================================================
function atoi(const s: string): integer;
var
  code: integer;
  ret2: integer;
begin
  val(s, result, code);
  if code <> 0 then
  begin
    ret2 := 0;
    if Pos('0x', s) = 1 then
      val('$' + Copy(s, 3, Length(s) - 2), ret2, code)
    else if Pos('-0x', s) = 1 then
    begin
      val('$' + Copy(s, 4, Length(s) - 3), ret2, code);
      ret2 := -ret2;
    end
    else if CharPos('#', s) = 1 then
      val(Copy(s, 2, Length(s) - 1), ret2, code);
    if code = 0 then
      result := ret2
    else
      result := 0;
  end;
end;

//==============================================================================
//
// atoi
//
//==============================================================================
function atoi(const s: string; const default: integer): integer; overload;
var
  code: integer;
  ret2: integer;
begin
  val(s, result, code);
  if code <> 0 then
  begin
    ret2 := default;
    if Pos('0x', s) = 1 then
      val('$' + Copy(s, 3, Length(s) - 2), ret2, code)
    else if Pos('-0x', s) = 1 then
    begin
      val('$' + Copy(s, 4, Length(s) - 3), ret2, code);
      ret2 := -ret2;
    end
    else if CharPos('#', s) = 1 then
      val(Copy(s, 2, Length(s) - 1), ret2, code);
    if code = 0 then
      result := ret2
    else
      result := default;
  end;
end;

//==============================================================================
//
// atoui
//
//==============================================================================
function atoui(const s: string): LongWord; overload;
var
  code: integer;
  ret2: LongWord;
begin
  val(s, result, code);
  if code <> 0 then
  begin
    ret2 := 0;
    if Pos('0x', s) = 1 then
      val('$' + Copy(s, 3, Length(s) - 2), ret2, code)
    else if CharPos('#', s) = 1 then
      val(Copy(s, 2, Length(s) - 1), ret2, code);
    if code = 0 then
      result := ret2
    else
      result := 0;
  end;
end;

//==============================================================================
//
// atoui
//
//==============================================================================
function atoui(const s: string; const default: LongWord): LongWord; overload;
var
  code: integer;
  ret2: LongWord;
begin
  val(s, result, code);
  if code <> 0 then
  begin
    ret2 := default;
    if Pos('0x', s) = 1 then
      val('$' + Copy(s, 3, Length(s) - 2), ret2, code)
    else if CharPos('#', s) = 1 then
      val(Copy(s, 2, Length(s) - 1), ret2, code);
    if code = 0 then
      result := ret2
    else
      result := default;
  end;
end;

//==============================================================================
//
// atof
//
//==============================================================================
function atof(const s: string): single;
var
  code: integer;
  i: integer;
  str: string;
begin
  ThousandSeparator := #0;
  DecimalSeparator := '.';

  val(s, result, code);
  if code <> 0 then
  begin
    str := s;
    for i := 1 to Length(str) do
      if str[i] in ['.', ','] then
        str[i] := DecimalSeparator;
    val(str, result, code);
    if code = 0 then
      exit;
    for i := 1 to Length(str) do
      if str[i] in ['.', ','] then
        str[i] := '.';
    val(str, result, code);
    if code = 0 then
      exit;
    for i := 1 to Length(str) do
      if str[i] in ['.', ','] then
        str[i] := ',';
    val(str, result, code);
    if code = 0 then
      exit;
    result := 0.0;
  end;
end;

//==============================================================================
//
// atof
//
//==============================================================================
function atof(const s: string; const default: single): single;
var
  code: integer;
  i: integer;
  str: string;
begin
  ThousandSeparator := #0;
  DecimalSeparator := '.';

  val(s, result, code);
  if code <> 0 then
  begin
    str := s;
    for i := 1 to Length(str) do
      if str[i] in ['.', ','] then
        str[i] := DecimalSeparator;
    val(str, result, code);
    if code = 0 then
      exit;
    for i := 1 to Length(str) do
      if str[i] in ['.', ','] then
        str[i] := '.';
    val(str, result, code);
    if code = 0 then
      exit;
    for i := 1 to Length(str) do
      if str[i] in ['.', ','] then
        str[i] := ',';
    val(str, result, code);
    if code = 0 then
      exit;
    result := default;
  end;
end;

{$IFNDEF FPC}

//==============================================================================
//
// memcpy_MMX8
//
//==============================================================================
procedure memcpy_MMX8(const dst: pointer; const src: pointer; const len: integer); assembler;
asm
  push esi
  push edi

  mov esi, src
  mov edi, dst
  mov ecx, len
  shr ecx, 3  // 8 bytes per iteration

@@loop1:
// Read in source data
  movq  mm1, [esi]
// Non-temporal stores
  movntq [edi], mm1

  add esi, 8
  add edi, 8
  dec ecx
  jnz @@loop1

  emms

  pop edi
  pop esi
end;

//==============================================================================
//
// memcpy_MMX64
//
//==============================================================================
procedure memcpy_MMX64(const dst: pointer; const src: pointer; const len: integer); assembler;
asm
  push esi
  push edi

  mov esi, src
  mov edi, dst
  mov ecx, len
  shr ecx, 6    // 64 bytes per iteration

@@loop1:

// Read in source data
  movq mm1, [esi]
  movq mm2, [esi + 8]
  movq mm3, [esi + 16]
  movq mm4, [esi + 24]
  movq mm5, [esi + 32]
  movq mm6, [esi + 40]
  movq mm7, [esi + 48]
  movq mm0, [esi + 56]

// Non-temporal stores
  movntq [edi], mm1
  movntq [edi + 8], mm2
  movntq [edi + 16], mm3
  movntq [edi + 24], mm4
  movntq [edi + 32], mm5
  movntq [edi + 40], mm6
  movntq [edi + 48], mm7
  movntq [edi + 56], mm0

  add esi, 64
  add edi, 64
  dec ecx
  jnz @@loop1

  emms

  pop edi
  pop esi
end;

//==============================================================================
//
// memcpy_3DNow64
//
//==============================================================================
procedure memcpy_3DNow64(const dst: pointer; const src: pointer; const len: integer); assembler;
asm
  push esi
  push edi

  mov esi, src
  mov edi, dst
  mov ecx, len
  shr ecx, 6    // 64 bytes per iteration

@@loop1:
// Prefetch next loop, non-temporal
  prefetch [esi + 64]
  prefetch [esi + 96]

// Read in source data
  movq mm1, [esi]
  movq mm2, [esi + 8]
  movq mm3, [esi + 16]
  movq mm4, [esi + 24]
  movq mm5, [esi + 32]
  movq mm6, [esi + 40]
  movq mm7, [esi + 48]
  movq mm0, [esi + 56]

// Non-temporal stores
  movntq [edi], mm1
  movntq [edi + 8], mm2
  movntq [edi + 16], mm3
  movntq [edi + 24], mm4
  movntq [edi + 32], mm5
  movntq [edi + 40], mm6
  movntq [edi + 48], mm7
  movntq [edi + 56], mm0

  add esi, 64
  add edi, 64
  dec ecx
  jnz @@loop1

  emms

  pop edi
  pop esi
end;
{$ENDIF}

//==============================================================================
//
// memmove
//
//==============================================================================
function memmove(const destination, source: pointer; count: integer): pointer;
begin
  Move(source^, destination^, count);
  result := destination;
end;

//==============================================================================
//
// memcpy
//
//==============================================================================
procedure memcpy(const dest0: pointer; const src0: pointer; count0: integer);
begin
  Move(src0^, dest0^, count0);
end;

{$IFNDEF FPC}

//==============================================================================
//
// memcpy_MMX
//
//==============================================================================
function memcpy_MMX(const dest0: pointer; const src0: pointer; count0: integer): pointer;
var
  dest: PByte;
  src: PByte;
  count: integer;
begin
  if mmxMachine = 0 then
  begin
    Move(src0^, dest0^, count0);
    result := dest0;
    exit;
  end;

  // if copying more than 16 bytes and we can copy 8 byte aligned
  if (count0 > 16) and (((integer(dest0) xor integer(src0)) and 7) = 0) then
  begin
    dest := PByte(dest0);
    src := PByte(src0);

    // copy up to the first 8 byte aligned boundary
    count := integer(dest) and 7;
    Move(src^, dest^, count);
    inc(dest, count);
    inc(src, count);
    count := count0 - count;

   // if there are blocks of 64 bytes
    if count and not 63 <> 0 then
    begin
      if AMD3DNowMachine <> 0 then
        memcpy_3DNow64(dest, src, count and not 63)
      else
        memcpy_MMX64(dest, src, count and not 63);
      inc(src, count and not 63);
      inc(dest, count and not 63);
      count := count and 63;
    end;

    // if there are blocks of 8 bytes
    if count and not 7 <> 0 then
    begin
      memcpy_MMX8(dest, src, count);
      inc(src, count and not 7);
      inc(dest, count and not 7);
      count := count and 7;
    end;

    // copy any remaining bytes
    Move(src^, dest^, count);
  end
  else
  begin
    // use the regular one if we cannot copy 8 byte aligned
    Move(src0^, dest0^, count0);
  end;
  result := dest0;
end;
{$ENDIF}

//==============================================================================
//
// memset
//
//==============================================================================
function memset(const dest0: pointer; const val: integer; const count0: integer): pointer;
{$IFNDEF FPC}
var
  data: union_8b;
  pdat: pointer;
  dest: PByte;
  count: integer;
{$ENDIF}
begin
  {$IFNDEF FPC}
  if mmxMachine = 0 then
  begin
  {$ENDIF}
    FillChar(dest0^, count0, val);
    result := dest0;
    {$IFNDEF FPC}
    exit;
  end;

  dest := PByte(dest0);
  count := count0;

  while (count > 0) and (integer(dest) and 7 <> 0) do
  begin
    dest^ := val;
    inc(dest);
    dec(count);
  end;

  if count = 0 then
  begin
    result := dest0;
    exit;
  end;

  data.bytes[0] := val;
  data.bytes[1] := val;
  data.words[1] := data.words[0];
  data.dwords[1] := data.dwords[0];
  pdat := @data;

  if count >= 64 then
  begin
    asm
      push esi
      push edi

      mov edi, dest
      mov esi, pdat

      mov ecx, count
      // 64 bytes per iteration
      shr ecx, 6
      // Read in source data
      movq mm1, [esi]
      movq mm2, mm1
      movq mm3, mm1
      movq mm4, mm1
      movq mm5, mm1
      movq mm6, mm1
      movq mm7, mm1
      movq mm0, mm1
@@loop1:
      movntq [edi], mm1
      movntq [edi + 8], mm2
      movntq [edi + 16], mm3
      movntq [edi + 24], mm4
      movntq [edi + 32], mm5
      movntq [edi + 40], mm6
      movntq [edi + 48], mm7
      movntq [edi + 56], mm0

      add edi, 64
      dec ecx
      jnz @@loop1

      pop edi
      pop esi
    end;

    inc(dest, count and not 63);
    count := count and 63;
  end;

  if count >= 8 then
  begin
    asm
      push esi
      push edi

      mov edi, dest
      mov esi, pdat

      mov ecx, count
      // 8 bytes per iteration
      shr ecx, 3
      // Read in source data
      movq mm1, [esi]
@@loop2:
      movntq  [edi], mm1

      add edi, 8
      dec ecx
      jnz @@loop2

      pop edi
      pop esi
    end;
    inc(dest, count and not 7);
    count := count and 7;
  end;

  while count > 0 do
  begin
    dest^ := val;
    inc(dest);
    dec(count);
  end;

  asm
    emms
  end;

  result := dest0;
{$ENDIF}
end;

//==============================================================================
//
// memsetsi
//
//==============================================================================
function memsetsi(const dest0: pointer; const val: smallint; count0: integer): pointer;
{$IFNDEF FPC}
var
  data: union_8b;
  pdat: pointer;
  dest: PByte;
  count: integer;
  i: integer;
{$ENDIF}
begin
  {$IFNDEF FPC}
  if mmxMachine = 0 then
  begin
  {$ENDIF}
    for i := 0 to count0 - 1 do
      PSmallIntArray(dest0)[i] := val;
    result := dest0;
    {$IFNDEF FPC}
    exit;
  end;

  dest := PByte(dest0);
  count := count0 * 2;

  while (count > 0) and (integer(dest) and 7 <> 0) do
  begin
    PSmallInt(dest)^ := val;
    inc(dest, 2);
    dec(count, 2);
  end;

  if count = 0 then
  begin
    result := dest0;
    exit;
  end;

  data.smallints[0] := val;
  data.smallints[1] := data.smallints[0];
  data.dwords[1] := data.dwords[0];
  pdat := @data;

  if count >= 64 then
  begin
    asm
      push esi
      push edi

      mov edi, dest
      mov esi, pdat

      mov ecx, count
      // 64 bytes per iteration
      shr ecx, 6
      // Read in source data
      movq mm1, [esi]
      movq mm2, mm1
      movq mm3, mm1
      movq mm4, mm1
      movq mm5, mm1
      movq mm6, mm1
      movq mm7, mm1
      movq mm0, mm1
@@loop1:
      movntq [edi], mm1
      movntq [edi + 8], mm2
      movntq [edi + 16], mm3
      movntq [edi + 24], mm4
      movntq [edi + 32], mm5
      movntq [edi + 40], mm6
      movntq [edi + 48], mm7
      movntq [edi + 56], mm0

      add edi, 64
      dec ecx
      jnz @@loop1

      pop edi
      pop esi
    end;

    inc(dest, count and not 63);
    count := count and 63;
  end;

  if count >= 8 then
  begin
    asm
      push esi
      push edi

      mov edi, dest
      mov esi, pdat

      mov ecx, count
      // 8 bytes per iteration
      shr ecx, 3
      // Read in source data
      movq mm1, [esi]
@@loop2:
      movntq  [edi], mm1

      add edi, 8
      dec ecx
      jnz @@loop2

      pop edi
      pop esi
    end;
    inc(dest, count and not 7);
    count := count and 7;
  end;

  while count > 0 do
  begin
    PSmallInt(dest)^ := val;
    inc(dest, 2);
    dec(count, 2);
  end;

  asm
    emms
  end;

  result := dest0;
{$ENDIF}
end;

//==============================================================================
//
// memseti
//
//==============================================================================
function memseti(const dest0: pointer; const val: integer; const count0: integer): pointer;
var
  data: union_8b;
  pdat: pointer;
  dest: PByte;
  count: integer;
  i: integer;
begin
  dest := PByte(dest0);
  count := count0;

  if mmxMachine = 0 then
  begin
    for i := 0 to count - 1 do
      PIntegerArray(dest)[i] := val;
    result := dest;
    exit;
  end;

  if integer(dest) and 7 <> 0 then
  begin
    for i := 0 to count - 1 do
      PIntegerArray(dest)[i] := val;
    result := dest;
    exit;
  end;

  if count <= 0 then
  begin
    result := dest0;
    exit;
  end;

  count := count * 4;

  data.dwords[0] := PInteger(@val)^;
  data.dwords[1] := PInteger(@val)^;
  pdat := @data;

  if count >= 64 then
  begin
    asm
      push esi
      push edi

      mov edi, dest
      mov esi, pdat

      mov ecx, count
      // 64 bytes per iteration
      shr ecx, 6
      // Read in source data
      movq mm1, [esi]
      movq mm2, mm1
      movq mm3, mm1
      movq mm4, mm1
      movq mm5, mm1
      movq mm6, mm1
      movq mm7, mm1
      movq mm0, mm1
@@loop1:
      movntq [edi], mm1
      movntq [edi + 8], mm2
      movntq [edi + 16], mm3
      movntq [edi + 24], mm4
      movntq [edi + 32], mm5
      movntq [edi + 40], mm6
      movntq [edi + 48], mm7
      movntq [edi + 56], mm0

      add edi, 64
      dec ecx
      jnz @@loop1

      pop edi
      pop esi
    end;

    inc(dest, count and not 63);
    count := count and 63;
  end;

  if count >= 8 then
  begin
    asm
      push esi
      push edi

      mov edi, dest
      mov esi, pdat

      mov ecx, count
      // 8 bytes per iteration
      shr ecx, 3
      // Read in source data
      movq mm1, [esi]
@@loop2:
      movntq  [edi], mm1

      add edi, 8
      dec ecx
      jnz @@loop2

      pop edi
      pop esi
    end;
    inc(dest, count and not 7);
    count := count and 7;
  end;

  while count > 4 do
  begin
    PInteger(dest)^ := val;
    inc(dest, 4);
    dec(count, 4);
  end;

  asm
    emms
  end;

  result := dest0;
end;

//==============================================================================
//
// malloc
//
//==============================================================================
function malloc(const size: integer): Pointer;
begin
  if size = 0 then
    result := nil
  else
  begin
    GetMem(result, size);
    memoryusage := memoryusage + size;
  end;
end;

//==============================================================================
//
// mallocA
//
//==============================================================================
function mallocA(var Size: integer; const Align: integer; var original: pointer): pointer;
begin
  Size := Size + Align;
  result := malloc(Size);
  original := result;
  if result <> nil then
    result := pointer(integer(result) and (1 - Align) + Align);
end;

//==============================================================================
//
// mallocz
//
//==============================================================================
function mallocz(const size: integer): Pointer;
begin
  result := malloc(size);
  if result <> nil then
    ZeroMemory(result, size);
end;

//==============================================================================
//
// realloc
//
//==============================================================================
procedure realloc(var p: pointer; const oldsize, newsize: integer);
begin
  if newsize = 0 then
    memfree(p, oldsize)
  else if newsize <> oldsize then
  begin
    reallocmem(p, newsize);
    memoryusage := memoryusage - oldsize + newsize;
  end;
end;

//==============================================================================
//
// memfree
//
//==============================================================================
procedure memfree(var p: pointer; const size: integer);
begin
  if p <> nil then
  begin
    FreeMem(p, size);
    p := nil;
    memoryusage := memoryusage - size;
  end;
end;

//==============================================================================
//
// IntToStrZfill
//
//==============================================================================
function IntToStrZfill(const z: integer; const x: integer): string;
var
  i: integer;
  len: integer;
begin
  result := itoa(x);
  len := Length(result);
  for i := len + 1 to z do
    result := '0' + result;
end;

//==============================================================================
//
// intval
//
//==============================================================================
function intval(const b: boolean): integer;
begin
  if b then
    result := 1
  else
    result := 0;
end;

//==============================================================================
//
// decide
//
//==============================================================================
function decide(const condition: boolean;
  const iftrue: integer; const iffalse: integer): integer;
begin
  if condition then
    result := iftrue
  else
    result := iffalse;
end;

//==============================================================================
//
// decide
//
//==============================================================================
function decide(const condition: boolean;
  const iftrue: boolean; const iffalse: boolean): boolean;
begin
  if condition then
    result := iftrue
  else
    result := iffalse;
end;

//==============================================================================
//
// decide
//
//==============================================================================
function decide(const condition: boolean;
  const iftrue: string; const iffalse: string): string;
begin
  if condition then
    result := iftrue
  else
    result := iffalse;
end;

//==============================================================================
//
// decide
//
//==============================================================================
function decide(const condition: boolean;
  const iftrue: pointer; const iffalse: pointer): pointer;
begin
  if condition then
    result := iftrue
  else
    result := iffalse;
end;

//==============================================================================
//
// decide
//
//==============================================================================
function decide(const condition: integer;
  const iftrue: integer; const iffalse: integer): integer;
begin
  if condition <> 0 then
    result := iftrue
  else
    result := iffalse;
end;

//==============================================================================
//
// decide
//
//==============================================================================
function decide(const condition: integer;
  const iftrue: boolean; const iffalse: boolean): boolean;
begin
  if condition <> 0 then
    result := iftrue
  else
    result := iffalse;
end;

//==============================================================================
//
// decide
//
//==============================================================================
function decide(const condition: integer;
  const iftrue: string; const iffalse: string): string;
begin
  if condition <> 0 then
    result := iftrue
  else
    result := iffalse;
end;

//==============================================================================
//
// decide
//
//==============================================================================
function decide(const condition: integer;
  const iftrue: pointer; const iffalse: pointer): pointer;
begin
  if condition <> 0 then
    result := iftrue
  else
    result := iffalse;
end;

//==============================================================================
//
// decidef
//
//==============================================================================
function decidef(const condition: boolean;
  const iftrue: single; const iffalse: single): single;
begin
  if condition then
    result := iftrue
  else
    result := iffalse;
end;

//==============================================================================
//
// incp
//
//==============================================================================
function incp(var p: pointer; const size: integer = 1): pointer;
begin
  result := Pointer(integer(p) + size);
  p := result;
end;

//==============================================================================
//
// pDiff
//
//==============================================================================
function pDiff(const p1, p2: pointer; const size: integer): integer;
begin
  result := (Integer(p1) - Integer(p2)) div size;
end;

//==============================================================================
// TDStream.Create
//
////////////////////////////////////////////////////////////////////////////////
// TStream
//
//==============================================================================
constructor TDStream.Create;
begin
  FIOResult := 0;
end;

//==============================================================================
//
// TDStream.IOResult
//
//==============================================================================
function TDStream.IOResult: integer;
begin
  result := FIOResult;
  FIOResult := 0;
end;

//==============================================================================
// TDMemoryStream.Create
//
////////////////////////////////////////////////////////////////////////////////
// TMemoryStream
//
//==============================================================================
constructor TDMemoryStream.Create;
begin
  Inherited Create;
  FSize := 0;
  FRealSize := 0;
  FPosition := 0;
  FMemory := nil;
end;

//==============================================================================
//
// TDMemoryStream.Destroy
//
//==============================================================================
destructor TDMemoryStream.Destroy;
begin
  Resize(0);
  Inherited Destroy;
end;

//==============================================================================
//
// TDMemoryStream.Resize
//
//==============================================================================
procedure TDMemoryStream.Resize(newsize: integer);
var
  newrealsize: integer;
begin
  if FSize <> newsize then
  begin
    if newsize = 0 then
      newrealsize := 0
    else if newsize < 64 then
      newrealsize := 64
    else
      newrealsize := newsize and not $FF + $100;
    if newrealsize <> FRealSize then
    begin
      realloc(FMemory, FRealSize, newrealsize);
      FRealSize := newrealsize;
    end;
    FSize := newsize;
    if FPosition > FSize then
      FPosition := FSize;
  end;
end;

//==============================================================================
//
// TDMemoryStream.Read
//
//==============================================================================
function TDMemoryStream.Read(var Buffer; Count: Longint): Longint;
begin
  if Count + FPosition > FSize then
    result := FSize - FPosition
  else
    result := Count;

  memcpy(@Buffer, pointer(integer(FMemory) + FPosition), result);
  FPosition := FPosition + result;
end;

//==============================================================================
//
// TDMemoryStream.Write
//
//==============================================================================
function TDMemoryStream.Write(const Buffer; Count: Longint): Longint;
begin
  if Count + FPosition > FSize then
    resize(Count + FPosition);
  memcpy(pointer(integer(FMemory) + FPosition), @Buffer, Count);
  FPosition := FPosition + Count;
  result := Count;
end;

//==============================================================================
//
// TDMemoryStream.Seek
//
//==============================================================================
function TDMemoryStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    sFromBeginning:
      result := Offset;
    sFromCurrent:
      result := FPosition + Offset;
    sFromEnd:
      result := FSize - Offset;
  else
    result := 0;
  end;
  FPosition := result;
end;

//==============================================================================
//
// TDMemoryStream.Size
//
//==============================================================================
function TDMemoryStream.Size: Longint;
begin
  result := FSize;
end;

//==============================================================================
//
// TDMemoryStream.Position
//
//==============================================================================
function TDMemoryStream.Position: integer;
begin
  result := FPosition;
end;

//==============================================================================
// TAttachableMemoryStream.Create
//
////////////////////////////////////////////////////////////////////////////////
// TAttachableMemoryStream
//
//==============================================================================
constructor TAttachableMemoryStream.Create;
begin
  Inherited Create;
  FSize := 0;
  FPosition := 0;
  FMemory := nil;
end;

//==============================================================================
//
// TAttachableMemoryStream.Destroy
//
//==============================================================================
destructor TAttachableMemoryStream.Destroy;
begin
  Inherited Destroy;
end;

//==============================================================================
//
// TAttachableMemoryStream.Attach
//
//==============================================================================
procedure TAttachableMemoryStream.Attach(const amemory: pointer; const asize: integer);
begin
  FMemory := amemory;
  FSize := asize;
  FPosition := 0;
end;

//==============================================================================
//
// TAttachableMemoryStream.Read
//
//==============================================================================
function TAttachableMemoryStream.Read(var Buffer; Count: Longint): Longint;
begin
  if Count + FPosition > FSize then
    result := FSize - FPosition
  else
    result := Count;

  memcpy(@Buffer, pointer(integer(FMemory) + FPosition), result);
  FPosition := FPosition + result;
end;

//==============================================================================
//
// TAttachableMemoryStream.Write
//
//==============================================================================
function TAttachableMemoryStream.Write(const Buffer; Count: Longint): Longint;
begin
  if Count + FPosition > FSize then
    result := FSize - FPosition
  else
    result := Count;

  if result <= 0 then
    Exit;

  memcpy(pointer(integer(FMemory) + FPosition), @Buffer, result);
  FPosition := FPosition + result;
end;

//==============================================================================
//
// TAttachableMemoryStream.Seek
//
//==============================================================================
function TAttachableMemoryStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    sFromBeginning:
      result := Offset;
    sFromCurrent:
      result := FPosition + Offset;
    sFromEnd:
      result := FSize - Offset;
  else
    result := 0;
  end;
  FPosition := result;
end;

//==============================================================================
//
// TAttachableMemoryStream.Size
//
//==============================================================================
function TAttachableMemoryStream.Size: Longint;
begin
  result := FSize;
end;

//==============================================================================
//
// TAttachableMemoryStream.Position
//
//==============================================================================
function TAttachableMemoryStream.Position: integer;
begin
  result := FPosition;
end;

//==============================================================================
// TFile.Create
//
////////////////////////////////////////////////////////////////////////////////
// TFile
// File class
//
//==============================================================================
constructor TFile.Create(const FileName: string; const mode: integer);
begin
  Inherited Create;
  OnBeginBusy := nil;
  OnEndBusy := nil;

  fopen(f, FileName, mode);
end;

//==============================================================================
//
// TFile.Destroy
//
//==============================================================================
destructor TFile.Destroy;
begin
  close(f);
  Inherited;
end;

//==============================================================================
//
// TFile.Read
//
//==============================================================================
function TFile.Read(var Buffer; Count: Longint): Longint;
begin
  if Assigned(OnBeginBusy) then OnBeginBusy;

  {$I-}
  BlockRead(f, Buffer, Count, result);
  {$I+}
  FIOResult := IOResult;

  if Assigned(OnEndBusy) then OnEndBusy;
end;

//==============================================================================
//
// TFile.Write
//
//==============================================================================
function TFile.Write(const Buffer; Count: Longint): Longint;
begin
  if Assigned(OnBeginBusy) then OnBeginBusy;

  {$I-}
  BlockWrite(f, Buffer, Count, result);
  {$I+}
  FIOResult := IOResult;

  if Assigned(OnEndBusy) then OnEndBusy;
end;

//==============================================================================
//
// TFile.Seek
//
//==============================================================================
function TFile.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    sFromBeginning:
      result := Offset;
    sFromCurrent:
      result := FilePos(f) + Offset;
    sFromEnd:
      result := FileSize(f) - Offset;
  else
    result := 0;
  end;
  {$I-}
  system.Seek(f, result);
  {$I+}
  FIOResult := IOResult;
end;

//==============================================================================
//
// TFile.Size
//
//==============================================================================
function TFile.Size: Longint;
begin
  {$I-}
  result := FileSize(f);
  {$I+}
  FIOResult := IOResult;
end;

//==============================================================================
//
// TFile.Position
//
//==============================================================================
function TFile.Position: integer;
begin
  {$I-}
  result := FilePos(f);
  {$I+}
  FIOResult := IOResult;
end;

//==============================================================================
// TCachedFile.Create
//
////////////////////////////////////////////////////////////////////////////////
// TCachedFile
// Cache read file class
//
//==============================================================================
constructor TCachedFile.Create(const FileName: string; mode: word; ABufSize: integer = $FFFF);
begin
  fInitialized := false;
  Inherited Create(FileName, mode);
  if ABufSize > Size then
    fBufSize := Size
  else
    fBufSize := ABufSize;
  fBuffer := malloc(fBufSize);
  fPosition := 0;
  ResetBuffer;
  fSize := Inherited Size;
  fInitialized := true;
end;

//==============================================================================
//
// TCachedFile.ResetBuffer
//
//==============================================================================
procedure TCachedFile.ResetBuffer;
begin
  fBufferStart := -1;
  fBufferEnd := -1;
end;

//==============================================================================
//
// TCachedFile.Destroy
//
//==============================================================================
destructor TCachedFile.Destroy;
begin
  memfree(fBuffer, fBufSize);
  Inherited;
end;

//==============================================================================
//
// TCachedFile.Read
//
//==============================================================================
function TCachedFile.Read(var Buffer; Count: Longint): Longint;
var
  x: Longint;
begin
// Buffer hit
  if (fPosition >= fBufferStart) and (fPosition + Count <= fBufferEnd) then
  begin
    x := LongInt(fBuffer) + fPosition - fBufferStart;
    Move(Pointer(x)^, Buffer, Count);
    fPosition := fPosition + Count;
    result := Count;
  end
// Non Buffer hit, cache buffer
  else if Count <= fBufSize then
  begin
    fPosition := Inherited Seek(fPosition, sFromBeginning);
    x := Inherited Read(fBuffer^, fBufSize);
    if x < Count then
      result := x
    else
      result := Count;
    Move(fBuffer^, Buffer, Count);
    fBufferStart := fPosition;
    fBufferEnd := fPosition + x;
    fPosition := fPosition + result;
  end
// Keep old buffer
  else
  begin
    fPosition := Inherited Seek(fPosition, sFromBeginning);
    result := Inherited Read(Buffer, Count);
    fPosition := fPosition + result;
  end;
end;

//==============================================================================
//
// TCachedFile.Write
//
//==============================================================================
function TCachedFile.Write(const Buffer; Count: Longint): Longint;
begin
  fPosition := Inherited Seek(fPosition, sFromBeginning);
  result := Inherited Write(Buffer, Count);
  fPosition := fPosition + result;
  if fSize < fPosition then
    fSize := fPosition;
end;

//==============================================================================
//
// TCachedFile.Seek
//
//==============================================================================
function TCachedFile.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if fInitialized then
  begin
    case Origin of
      sFromBeginning: fPosition := Offset;
      sFromCurrent: inc(fPosition, Offset);
      sFromEnd: fPosition := fSize - Offset;
    end;
    result := fPosition;
  end
  else
    result := Inherited Seek(Offset, Origin);
end;

//==============================================================================
//
// TCachedFile.SetSize
//
//==============================================================================
procedure TCachedFile.SetSize(NewSize: Longint);
begin
  Inherited;
  fSize := NewSize;
end;

//==============================================================================
//
// TCachedFile.Position
//
//==============================================================================
function TCachedFile.Position: integer;
begin
  result := FPosition;
end;

//==============================================================================
// TDByteList.Create
//
////////////////////////////////////////////////////////////////////////////////
// TDByteList
//
//==============================================================================
constructor TDByteList.Create;
begin
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

//==============================================================================
//
// TDByteList.Destroy
//
//==============================================================================
destructor TDByteList.Destroy;
begin
  Clear;
end;

//==============================================================================
//
// TDByteList.Get
//
//==============================================================================
function TDByteList.Get(Index: Integer): byte;
begin
  if (Index < 0) or (Index >= fNumItems) then
    result := 0
  else
    result := fList[Index];
end;

//==============================================================================
//
// TDByteList.Put
//
//==============================================================================
procedure TDByteList.Put(Index: Integer; const value: byte);
begin
  fList[Index] := value;
end;

//==============================================================================
//
// TDByteList.Add
//
//==============================================================================
function TDByteList.Add(const value: byte): integer;
var
  newrealitems: integer;
begin
  if fNumItems >= fRealNumItems then
  begin
    if fRealNumItems < 8 then
      newrealitems := 8
    else if fRealNumItems < 32 then
      newrealitems := 32
    else if fRealNumItems < 128 then
      newrealitems := fRealNumItems + 32
    else
      newrealitems := fRealNumItems + 64;
    realloc(pointer(fList), fRealNumItems * SizeOf(byte), newrealitems * SizeOf(byte));
    fRealNumItems := newrealitems;
  end;
  Put(fNumItems, value);
  result := fNumItems;
  inc(fNumItems);
end;

//==============================================================================
//
// TDByteList.Delete
//
//==============================================================================
function TDByteList.Delete(const Index: integer): boolean;
var
  i: integer;
begin
  if (Index < 0) or (Index >= fNumItems) then
  begin
    result := false;
    exit;
  end;

  for i := Index + 1 to fNumItems - 1 do
    fList[i - 1] := fList[i];

  dec(fNumItems);

  result := true;
end;

//==============================================================================
//
// TDByteList.IndexOf
//
//==============================================================================
function TDByteList.IndexOf(const value: byte): integer;
var
  i: integer;
begin
  for i := 0 to fNumItems - 1 do
    if fList[i] = value then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

//==============================================================================
//
// TDByteList.Clear
//
//==============================================================================
procedure TDByteList.Clear;
begin
  realloc(pointer(fList), fRealNumItems * SizeOf(byte), 0);
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

//==============================================================================
//
// TDByteList.FastClear
//
//==============================================================================
procedure TDByteList.FastClear;
begin
  fNumItems := 0;
end;

//==============================================================================
// TDNumberList.Create
//
////////////////////////////////////////////////////////////////////////////////
// TDNumberList
//
//==============================================================================
constructor TDNumberList.Create;
begin
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

//==============================================================================
//
// TDNumberList.Destroy
//
//==============================================================================
destructor TDNumberList.Destroy;
begin
  Clear;
end;

//==============================================================================
//
// TDNumberList.Get
//
//==============================================================================
function TDNumberList.Get(Index: Integer): integer;
begin
  if (Index < 0) or (Index >= fNumItems) then
    result := 0
  else
    result := fList[Index];
end;

//==============================================================================
//
// TDNumberList.Put
//
//==============================================================================
procedure TDNumberList.Put(Index: Integer; const value: integer);
begin
  fList[Index] := value;
end;

//==============================================================================
//
// TDNumberList.Add
//
//==============================================================================
function TDNumberList.Add(const value: integer): integer;
var
  newrealitems: integer;
begin
  if fNumItems >= fRealNumItems then
  begin
    if fRealNumItems < 8 then
      newrealitems := 8
    else if fRealNumItems < 32 then
      newrealitems := 32
    else if fRealNumItems < 128 then
      newrealitems := fRealNumItems + 32
    else
      newrealitems := fRealNumItems + 64;
    realloc(pointer(fList), fRealNumItems * SizeOf(integer), newrealitems * SizeOf(integer));
    fRealNumItems := newrealitems;
  end;
  Put(fNumItems, value);
  result := fNumItems;
  inc(fNumItems);
end;

//==============================================================================
//
// TDNumberList.Add
//
//==============================================================================
procedure TDNumberList.Add(const nlist: TDNumberList);
var
  i: integer;
begin
  for i := 0 to nlist.Count - 1 do
    Add(nlist[i]);
end;

//==============================================================================
//
// TDNumberList.Delete
//
//==============================================================================
function TDNumberList.Delete(const Index: integer): boolean;
var
  i: integer;
begin
  if (Index < 0) or (Index >= fNumItems) then
  begin
    result := false;
    exit;
  end;

  for i := Index + 1 to fNumItems - 1 do
    fList[i - 1] := fList[i];

  dec(fNumItems);

  result := true;
end;

//==============================================================================
//
// TDNumberList.IndexOf
//
//==============================================================================
function TDNumberList.IndexOf(const value: integer): integer;
var
  i: integer;
begin
  for i := 0 to fNumItems - 1 do
    if fList[i] = value then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

//==============================================================================
//
// TDNumberList.Clear
//
//==============================================================================
procedure TDNumberList.Clear;
begin
  realloc(pointer(fList), fRealNumItems * SizeOf(integer), 0);
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

//==============================================================================
//
// TDNumberList.FastClear
//
//==============================================================================
procedure TDNumberList.FastClear;
begin
  fNumItems := 0;
end;

//==============================================================================
//
// TDNumberList.Sort
//
//==============================================================================
procedure TDNumberList.Sort;
begin
  QSortIntegers(fList, fNumItems);
end;

//==============================================================================
//
// TDNumberList.Sum
//
//==============================================================================
function TDNumberList.Sum: integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to fNumItems - 1 do
    result := result + fList[i];
end;

//==============================================================================
// T2DNumberList.Create
//
////////////////////////////////////////////////////////////////////////////////
// T2DNumberList
//
//==============================================================================
constructor T2DNumberList.Create;
begin
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

//==============================================================================
//
// T2DNumberList.Destroy
//
//==============================================================================
destructor T2DNumberList.Destroy;
begin
  Clear;
end;

//==============================================================================
//
// T2DNumberList.Get
//
//==============================================================================
function T2DNumberList.Get(Index: Integer): twointeger_t;
begin
  if (Index < 0) or (Index >= fNumItems) then
  begin
    result.num1 := 0;
    result.num2 := 0;
  end
  else
    result := fList[Index];
end;

//==============================================================================
//
// T2DNumberList.Put
//
//==============================================================================
procedure T2DNumberList.Put(Index: Integer; const value: twointeger_t);
begin
  fList[Index] := value;
end;

//==============================================================================
//
// T2DNumberList.Add
//
//==============================================================================
function T2DNumberList.Add(const value1, value2: integer): integer;
var
  newrealitems: integer;
  value: twointeger_t;
begin
  if fNumItems >= fRealNumItems then
  begin
    if fRealNumItems < 4 then
      newrealitems := 4
    else if fRealNumItems < 8 then
      newrealitems := 8
    else if fRealNumItems < 32 then
      newrealitems := 32
    else if fRealNumItems < 128 then
      newrealitems := fRealNumItems + 32
    else
      newrealitems := fRealNumItems + 64;
    realloc(pointer(fList), fRealNumItems * SizeOf(twointeger_t), newrealitems * SizeOf(twointeger_t));
    fRealNumItems := newrealitems;
  end;
  value.num1 := value1;
  value.num2 := value2;
  Put(fNumItems, value);
  result := fNumItems;
  inc(fNumItems);
end;

//==============================================================================
//
// T2DNumberList.Add
//
//==============================================================================
function T2DNumberList.Add(const value: twointeger_t): integer;
begin
  result := Add(value.num1, value.num2);
end;

//==============================================================================
//
// T2DNumberList.Add
//
//==============================================================================
procedure T2DNumberList.Add(const nlist: T2DNumberList);
var
  i: integer;
begin
  for i := 0 to nlist.Count - 1 do
    Add(nlist[i]);
end;

//==============================================================================
//
// T2DNumberList.Delete
//
//==============================================================================
function T2DNumberList.Delete(const Index: integer): boolean;
var
  i: integer;
begin
  if (Index < 0) or (Index >= fNumItems) then
  begin
    result := false;
    exit;
  end;

  for i := Index + 1 to fNumItems - 1 do
    fList[i - 1] := fList[i];

  dec(fNumItems);

  result := true;
end;

//==============================================================================
//
// T2DNumberList.IndexOf
//
//==============================================================================
function T2DNumberList.IndexOf(const value1, value2: integer): integer;
var
  i: integer;
begin
  for i := 0 to fNumItems - 1 do
    if (fList[i].num1 = value1) and (fList[i].num2 = value2) then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

//==============================================================================
//
// T2DNumberList.Clear
//
//==============================================================================
procedure T2DNumberList.Clear;
begin
  realloc(pointer(fList), fRealNumItems * SizeOf(twointeger_t), 0);
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

//==============================================================================
//
// T2DNumberList.FastClear
//
//==============================================================================
procedure T2DNumberList.FastClear;
begin
  fNumItems := 0;
end;

//==============================================================================
// TDLimitNumberList.Create
//
////////////////////////////////////////////////////////////////////////////////
// TDLimitNumberList
//
//==============================================================================
constructor TDLimitNumberList.Create;
begin
  inherited Create;
  fLimit := MAXINT;
end;

//==============================================================================
//
// TDLimitNumberList.CreateLimited
//
//==============================================================================
constructor TDLimitNumberList.CreateLimited(const v: Integer);
begin
  inherited Create;
  if v > 0 then // ?
    fLimit := v;
end;

//==============================================================================
//
// TDLimitNumberList.Add
//
//==============================================================================
function TDLimitNumberList.Add(const value: integer): integer;
var
  i: integer;
begin
  if Count >= fLimit then
  begin
    for i := 1 to Count - 1 do
      Numbers[i - 1] := Numbers[i];
    Numbers[Count - 1] := value;
    Result := Count - 1;
  end
  else
    Result := inherited Add(value);
end;

//==============================================================================
// TDFloatList.Create
//
////////////////////////////////////////////////////////////////////////////////
// TDFloatList
//
//==============================================================================
constructor TDFloatList.Create;
begin
  fList := nil;
  fNumItems := 0;
end;

//==============================================================================
//
// TDFloatList.Destroy
//
//==============================================================================
destructor TDFloatList.Destroy;
begin
  Clear;
end;

//==============================================================================
//
// TDFloatList.Get
//
//==============================================================================
function TDFloatList.Get(Index: Integer): float;
begin
  if (Index < 0) or (Index >= fNumItems) then
    result := 0
  else
    result := fList[Index];
end;

//==============================================================================
//
// TDFloatList.Put
//
//==============================================================================
procedure TDFloatList.Put(Index: Integer; const value: float);
begin
  fList[Index] := value;
end;

//==============================================================================
//
// TDFloatList.Add
//
//==============================================================================
function TDFloatList.Add(const value: float): integer;
begin
  realloc(pointer(fList), fNumItems * SizeOf(float), (fNumItems + 1) * SizeOf(float));
  Put(fNumItems, value);
  result := fNumItems;
  inc(fNumItems);
end;

//==============================================================================
//
// TDFloatList.Add
//
//==============================================================================
procedure TDFloatList.Add(const nlist: TDFloatList);
var
  i: integer;
begin
  for i := 0 to nlist.Count - 1 do
    Add(nlist[i]);
end;

//==============================================================================
//
// TDFloatList.Delete
//
//==============================================================================
function TDFloatList.Delete(const Index: integer): boolean;
var
  i: integer;
begin
  if (Index < 0) or (Index >= fNumItems) then
  begin
    result := false;
    exit;
  end;

  for i := Index + 1 to fNumItems - 1 do
    fList[i - 1] := fList[i];

  realloc(pointer(fList), fNumItems * SizeOf(float), (fNumItems - 1) * SizeOf(float));
  dec(fNumItems);

  result := true;
end;

//==============================================================================
//
// TDFloatList.IndexOf
//
//==============================================================================
function TDFloatList.IndexOf(const value: float): integer;
var
  i: integer;
begin
  for i := 0 to fNumItems - 1 do
    if fList[i] = value then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

//==============================================================================
//
// TDFloatList.Clear
//
//==============================================================================
procedure TDFloatList.Clear;
begin
  realloc(pointer(fList), fNumItems * SizeOf(float), 0);
  fList := nil;
  fNumItems := 0;
end;

//==============================================================================
//
// TDFloatList.Sort
//
//==============================================================================
procedure TDFloatList.Sort;
begin
  QSortFloats(fList, fNumItems);
end;

//==============================================================================
//
// TDFloatList.Sum
//
//==============================================================================
function TDFloatList.Sum: float;
var
  i: integer;
begin
  result := 0.0;
  for i := 0 to fNumItems - 1 do
    result := result + fList[i];
end;

//==============================================================================
// TDPointerList.Create
//
////////////////////////////////////////////////////////////////////////////////
// TDPointerList
//
//==============================================================================
constructor TDPointerList.Create;
begin
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

//==============================================================================
//
// TDPointerList.Destroy
//
//==============================================================================
destructor TDPointerList.Destroy;
begin
  Clear;
end;

//==============================================================================
//
// TDPointerList.Get
//
//==============================================================================
function TDPointerList.Get(Index: Integer): pointer;
begin
  if (Index < 0) or (Index >= fNumItems) then
    result := nil
  else
    result := fList[Index];
end;

//==============================================================================
//
// TDPointerList.Put
//
//==============================================================================
procedure TDPointerList.Put(Index: Integer; const value: pointer);
begin
  fList[Index] := value;
end;

//==============================================================================
//
// TDPointerList.Add
//
//==============================================================================
function TDPointerList.Add(const value: pointer): integer;
var
  newrealitems: integer;
begin
  if fNumItems >= fRealNumItems then
  begin
    if fRealNumItems < 8 then
      newrealitems := 8
    else if fRealNumItems < 32 then
      newrealitems := 32
    else if fRealNumItems < 128 then
      newrealitems := fRealNumItems + 32
    else
      newrealitems := fRealNumItems + 64;
    realloc(pointer(fList), fRealNumItems * SizeOf(pointer), newrealitems * SizeOf(pointer));
    fRealNumItems := newrealitems;
  end;
  Put(fNumItems, value);
  result := fNumItems;
  inc(fNumItems);
end;

//==============================================================================
//
// TDPointerList.Add
//
//==============================================================================
procedure TDPointerList.Add(const nlist: TDPointerList);
var
  i: integer;
begin
  for i := 0 to nlist.Count - 1 do
    Add(nlist[i]);
end;

//==============================================================================
//
// TDPointerList.Delete
//
//==============================================================================
function TDPointerList.Delete(const Index: integer): boolean;
var
  i: integer;
begin
  if (Index < 0) or (Index >= fNumItems) then
  begin
    result := false;
    exit;
  end;

  for i := Index + 1 to fNumItems - 1 do
    fList[i - 1] := fList[i];

  dec(fNumItems);

  result := true;
end;

//==============================================================================
//
// TDPointerList.IndexOf
//
//==============================================================================
function TDPointerList.IndexOf(const value: pointer): integer;
var
  i: integer;
begin
  for i := 0 to fNumItems - 1 do
    if fList[i] = value then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

//==============================================================================
//
// TDPointerList.Clear
//
//==============================================================================
procedure TDPointerList.Clear;
begin
  realloc(pointer(fList), fRealNumItems * SizeOf(pointer), 0);
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

//==============================================================================
//
// TDPointerList.FastClear
//
//==============================================================================
procedure TDPointerList.FastClear;
begin
  fNumItems := 0;
end;

//==============================================================================
// TDHashNumberList.Create
//
////////////////////////////////////////////////////////////////////////////////
// TDHashNumberList
//
//==============================================================================
constructor TDHashNumberList.Create;
begin
  Inherited Create;
  memset(@fhash, SizeOf(fhash), 0);
  fsorted := True;
end;

//==============================================================================
//
// TDHashNumberList.Put
//
//==============================================================================
procedure TDHashNumberList.Put(Index: Integer; const value: integer);
begin
  Inherited Put(Index, value);
  if fsorted then
    if fnumitems > 1 then
      fsorted := fList[fNumItems - 1] >= fList[fNumItems - 2];
  fhash[value and (NLHASHSIZE - 1)] := Index;
end;

//==============================================================================
//
// TDHashNumberList.IndexOf
//
//==============================================================================
function TDHashNumberList.IndexOf(const value: integer): integer;
var
  idx: integer;
  l, h: integer;
begin
  idx := fhash[value and (NLHASHSIZE - 1)];
  if (idx >= 0) and (idx < fNumItems) then
    if fList[idx] = value then
    begin
      result := idx;
      exit;
    end;

  if fsorted then
  begin
    l := 0;
    h := fNumItems - 1;
    while l <= h do
    begin
      idx := (l + h) div 2;
      if fList[idx] > value then
      begin
        h := idx - 1;
      end
      else if fList[idx] < value then
      begin
        l := idx + 1;
      end
      else
      begin
        Result := idx;
        fhash[value and (NLHASHSIZE - 1)] := Result;
        Exit;
      end;
    end;
    Result := -1;
    Exit;
  end;

  result := Inherited IndexOf(value);
  if result >= 0 then
    fhash[value and (NLHASHSIZE - 1)] := result;
end;

//==============================================================================
//
// TDHashNumberList.Sort
//
//==============================================================================
procedure TDHashNumberList.Sort;
begin
  if not fsorted then
  begin
    Inherited Sort;
    fsorted := True;
    CreateHashTable;
  end;
end;

//==============================================================================
//
// TDHashNumberList.CreateHashTable
//
//==============================================================================
procedure TDHashNumberList.CreateHashTable;
var
  i: integer;
begin
  for i := 0 to fNumItems - 1 do
    fhash[fList[i] and (NLHASHSIZE - 1)] := i;
end;

//==============================================================================
// TDTextList.Create
//
////////////////////////////////////////////////////////////////////////////////
// TDTextList
//
//==============================================================================
constructor TDTextList.Create;
begin
  fList := nil;
  fNumItems := 0;
end;

//==============================================================================
//
// TDTextList.Destroy
//
//==============================================================================
destructor TDTextList.Destroy;
begin
  Clear;
end;

//==============================================================================
//
// TDTextList.Get
//
//==============================================================================
function TDTextList.Get(Index: Integer): string;
begin
  if (Index < 0) or (Index >= fNumItems) then
    result := ''
  else
    result := fList[Index];
end;

//==============================================================================
//
// TDTextList.Put
//
//==============================================================================
procedure TDTextList.Put(Index: Integer; const value: string);
begin
  fList[Index] := value;
end;

//==============================================================================
//
// TDTextList.Add
//
//==============================================================================
function TDTextList.Add(const value: string): integer;
begin
  realloc(pointer(fList), fNumItems * 256, (fNumItems + 1) * 256);
  Put(fNumItems, value);
  result := fNumItems;
  inc(fNumItems);
end;

//==============================================================================
//
// TDTextList.Add
//
//==============================================================================
procedure TDTextList.Add(const nlist: TDTextList);
var
  i: integer;
begin
  for i := 0 to nlist.Count - 1 do
    Add(nlist[i]);
end;

//==============================================================================
//
// TDTextList.Delete
//
//==============================================================================
function TDTextList.Delete(const Index: integer): boolean;
var
  i: integer;
begin
  if (Index < 0) or (Index >= fNumItems) then
  begin
    result := false;
    exit;
  end;

  for i := Index + 1 to fNumItems - 1 do
    fList[i - 1] := fList[i];

  realloc(pointer(fList), fNumItems * 256, (fNumItems - 1) * 256);
  dec(fNumItems);

  result := true;
end;

//==============================================================================
//
// TDTextList.IndexOf
//
//==============================================================================
function TDTextList.IndexOf(const value: string): integer;
var
  i: integer;
begin
  for i := 0 to fNumItems - 1 do
    if fList[i] = value then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

//==============================================================================
//
// TDTextList.Clear
//
//==============================================================================
procedure TDTextList.Clear;
begin
  realloc(pointer(fList), fNumItems * 256, 0);
  fList := nil;
  fNumItems := 0;
end;

//==============================================================================
// TDStrings.Add
//
////////////////////////////////////////////////////////////////////////////////
// TDStrings
//
//==============================================================================
function TDStrings.Add(const S: string): Integer;
begin
  result := GetCount;
  Insert(result, S);
end;

//==============================================================================
//
// TDStrings.Add
//
//==============================================================================
function TDStrings.Add(const Fmt: string; const Args: array of const): integer;
var
  str: string;
begin
  sprintf(str, Fmt, Args);
  result := Add(str);
end;

//==============================================================================
//
// TDStrings.AddObject
//
//==============================================================================
function TDStrings.AddObject(const S: string; AObject: TObject): Integer;
begin
  result := Add(S);
  PutObject(result, AObject);
end;

//==============================================================================
//
// TDStrings.Append
//
//==============================================================================
procedure TDStrings.Append(const S: string);
begin
  Add(S);
end;

//==============================================================================
//
// TDStrings.AddStrings
//
//==============================================================================
procedure TDStrings.AddStrings(Strings: TDStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    AddObject(Strings[I], Strings.Objects[I]);
end;

//==============================================================================
//
// TDStrings.Equals
//
//==============================================================================
function TDStrings.Equals(Strings: TDStrings): Boolean;
var
  I, iCount: Integer;
begin
  result := false;
  iCount := GetCount;
  if iCount <> Strings.GetCount then
    Exit;
  for I := 0 to iCount - 1 do
    if Get(I) <> Strings.Get(I) then
      Exit;
  result := true;
end;

//==============================================================================
//
// TDStrings.Exchange
//
//==============================================================================
procedure TDStrings.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: string;
begin
  TempString := Strings[Index1];
  TempObject := Objects[Index1];
  Strings[Index1] := Strings[Index2];
  Objects[Index1] := Objects[Index2];
  Strings[Index2] := TempString;
  Objects[Index2] := TempObject;
end;

//==============================================================================
//
// TDStrings.GetCapacity
//
//==============================================================================
function TDStrings.GetCapacity: Integer;
begin  // descendants may optionally override/replace this default implementation
  result := Count;
end;

//==============================================================================
//
// TDStrings.GetCommaText
//
//==============================================================================
function TDStrings.GetCommaText: string;
var
  S: string;
  P: PChar;
  I, iCount: Integer;
begin
  iCount := GetCount;
  if (iCount = 1) and (Get(0) = '') then
    result := '""'
  else
  begin
    result := '';
    for I := 0 to iCount - 1 do
    begin
      S := Get(I);
      P := PChar(S);
      while not (P^ in [#0..' ','"',',']) do P := CharNext(P);
      if (P^ <> #0) then S := AnsiQuotedStr(S, '"');
      result := result + S + ',';
    end;
    System.Delete(result, Length(result), 1);
  end;
end;

//==============================================================================
//
// TDStrings.GetName
//
//==============================================================================
function TDStrings.GetName(Index: Integer): string;
var
  P: Integer;
begin
  result := Get(Index);
  P := AnsiPos('=', result);
  if P <> 0 then
    SetLength(result, P-1)
  else
    SetLength(result, 0);
end;

//==============================================================================
//
// TDStrings.GetObject
//
//==============================================================================
function TDStrings.GetObject(Index: Integer): TObject;
begin
  result := nil;
end;

//==============================================================================
//
// TDStrings.GetText
//
//==============================================================================
function TDStrings.GetText: PChar;
begin
  result := StrNew(PChar(GetTextStr));
end;

//==============================================================================
//
// TDStrings.GetTextStr
//
//==============================================================================
function TDStrings.GetTextStr: string;
var
  I, L, Size, iCount: Integer;
  P: PChar;
  S: string;
begin
  iCount := GetCount;
  Size := 0;
  for I := 0 to iCount - 1 do inc(Size, Length(Get(I)) + 2);
  SetString(result, nil, Size);
  P := Pointer(result);
  for I := 0 to iCount - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L);
      inc(P, L);
    end;
    P^ := #13;
    inc(P);
    P^ := #10;
    inc(P);
  end;
end;

//==============================================================================
//
// TDStrings.GetValue
//
//==============================================================================
function TDStrings.GetValue(const Name: string): string;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    result := Copy(Get(I), Length(Name) + 2, MaxInt) else
    result := '';
end;

//==============================================================================
//
// TDStrings.GetValueIdx
//
//==============================================================================
function TDStrings.GetValueIdx(const idx: integer): string;
var
  tmp: string;
begin
  splitstring_ch(Get(idx), tmp, result, '=');
end;

//==============================================================================
//
// TDStrings.IndexOf
//
//==============================================================================
function TDStrings.IndexOf(const S: string): Integer;
begin
  for result := 0 to GetCount - 1 do
    if AnsiCompareText(Get(result), S) = 0 then
      Exit;
  result := -1;
end;

//==============================================================================
//
// TDStrings.IndexOfName
//
//==============================================================================
function TDStrings.IndexOfName(const Name: string): Integer;
var
  P: Integer;
  S: string;
begin
  for result := 0 to GetCount - 1 do
  begin
    S := Get(result);
    P := AnsiPos('=', S);
    if (P <> 0) and (AnsiCompareText(Copy(S, 1, P - 1), Name) = 0) then
      Exit;
  end;
  result := -1;
end;

//==============================================================================
//
// TDStrings.IndexOfObject
//
//==============================================================================
function TDStrings.IndexOfObject(AObject: TObject): Integer;
begin
  for result := 0 to GetCount - 1 do
    if GetObject(result) = AObject then
      Exit;
  result := -1;
end;

//==============================================================================
//
// TDStrings.InsertObject
//
//==============================================================================
procedure TDStrings.InsertObject(Index: Integer; const S: string;
  AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

//==============================================================================
//
// BufferUtf16ToAnsi
//
//==============================================================================
procedure BufferUtf16ToAnsi(const inpBuf: PByteArray; const inpSize: integer;
  var outBuf: PByteArray; var outSize: integer);
var
  i: integer;
begin
  outSize := (inpSize - 2) div 2;
  if outSize > 0 then
  begin
    outBuf := malloc(outSize + 1);
    for i := 0 to outSize - 1 do
      outBuf[i] := inpBuf[i * 2 + 2];
    outBuf[outsize] := 0;
  end
  else
  begin
    outSize := 0;
    outBuf := nil;
  end;
end;

//==============================================================================
//
// TDStrings.LoadFromFile
//
//==============================================================================
function TDStrings.LoadFromFile(const FileName: string): boolean;
var
  f: file;
  Size: Integer;
  S, S2: string;
  p: integer;
begin
  if fopen(f, FileName, fOpenReadOnly) then
  begin
    {$I-}
    Size := FileSize(f);
    SetString(S, nil, Size);
    BlockRead(f, Pointer(S)^, Size);
    if Size > 1 then
      if S[1] = Chr($FF) then
        if S[2] = Chr($FE) then
        begin
          S2 := '';
          p := 3;
          while p < Size do
          begin
            S2 := S2 + S[p];
            inc(p, 2);
          end;
          S := S2;
        end;

    SetTextStr(S);
    close(f);
    {$I+}
    result := IOresult = 0;
  end
  else
    result := false;
end;

//==============================================================================
//
// TDStrings.LoadFromStream
//
//==============================================================================
function TDStrings.LoadFromStream(const strm: TDStream): boolean;
var
  SizeA, SizeB: Integer;
  A, B: PByteArray;

  isUTF16: boolean;
begin
  {$I-}
  strm.Seek(0, sFromBeginning);
  SizeA := strm.Size;
  {$I+}
  if SizeA = 0 then
  begin
    Clear;
    result := IOresult = 0;
    Exit;
  end;
  {$I-}
  A := malloc(SizeA + 1);
  A[SizeA] := 0;
  strm.Read(A^, SizeA);
  isUTF16 := False;
  if SizeA > 1 then
    isUTF16 := (A[0] = $FF) and (A[1] = $FE);

  if isUTF16 then
  begin
    BufferUtf16ToAnsi(A, SizeA, B, SizeB);
    memfree(pointer(A), SizeA + 1);
    A := B;
    SizeA := SizeB;
  end;

  SetByteStr(A, SizeA);
  memfree(pointer(A), SizeA + 1);
  {$I+}
  result := IOresult = 0;
end;

//==============================================================================
//
// TDStrings.Move
//
//==============================================================================
procedure TDStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: string;
begin
  if CurIndex <> NewIndex then
  begin
    TempString := Get(CurIndex);
    TempObject := GetObject(CurIndex);
    Delete(CurIndex);
    InsertObject(NewIndex, TempString, TempObject);
  end;
end;

//==============================================================================
//
// TDStrings.Put
//
//==============================================================================
procedure TDStrings.Put(Index: Integer; const S: string);
var
  TempObject: TObject;
begin
  TempObject := GetObject(Index);
  Delete(Index);
  InsertObject(Index, S, TempObject);
end;

//==============================================================================
//
// TDStrings.PutObject
//
//==============================================================================
procedure TDStrings.PutObject(Index: Integer; AObject: TObject);
begin
end;

//==============================================================================
//
// TDStrings.SaveToFile
//
//==============================================================================
function TDStrings.SaveToFile(const FileName: string): boolean;
var
  f: file;
  S: string;
begin
  if fopen(f, FileName, fCreate) then
  begin
    {$I-}
    S := GetTextStr;
    BlockWrite(f, Pointer(S)^, Length(S));
    close(f);
    {$I+}
    result := IOresult = 0;
  end
  else
    result := false;
end;

//==============================================================================
//
// TDStrings.SetCapacity
//
//==============================================================================
procedure TDStrings.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendants may optionally implement this method
end;

//==============================================================================
//
// TDStrings.SetCommaText
//
//==============================================================================
procedure TDStrings.SetCommaText(const Value: string);
var
  P, P1: PChar;
  S: string;
begin
  Clear;
  P := PChar(Value);
  while P^ in [#1..' '] do P := CharNext(P);
  while P^ <> #0 do
  begin
    if P^ = '"' then
      S := AnsiExtractQuotedStr(P, '"')
    else
    begin
      P1 := P;
      while (P^ > ' ') and (P^ <> ',') do P := CharNext(P);
      SetString(S, P1, P - P1);
    end;
    Add(S);
    while P^ in [#1..' '] do P := CharNext(P);
    if P^ = ',' then
      repeat
        P := CharNext(P);
      until not (P^ in [#1..' ']);
  end;
end;

//==============================================================================
//
// TDStrings.SetText
//
//==============================================================================
procedure TDStrings.SetText(Text: PChar);
begin
  SetTextStr(Text);
end;

//==============================================================================
//
// TDStrings.SetTextStr
//
//==============================================================================
procedure TDStrings.SetTextStr(const Value: string);
var
  P, Start: PChar;
  S: string;
begin
  Clear;
  P := Pointer(Value);
  if P <> nil then
    while P^ <> #0 do
    begin
      Start := P;
      while not (P^ in [#0, #10, #13]) do inc(P);
      SetString(S, Start, P - Start);
      Add(S);
      if P^ = #13 then inc(P);
      if P^ = #10 then inc(P);
    end;
end;

//==============================================================================
//
// TDStrings.SetByteStr
//
//==============================================================================
procedure TDStrings.SetByteStr(const A: PByteArray; const Size: integer);
var
  P, Start: PChar;
  S: string;
begin
  Clear;
  P := PChar(@A[0]);
  if P <> nil then
    while (P^ <> #0) and (integer(P) <> integer(@A[Size])) do
    begin
      Start := P;
      while (not (P^ in [#0, #10, #13])) and (integer(P) <> integer(@A[Size])) do inc(P);
      SetString(S, Start, P - Start);
      Add(S);
      if P^ = #13 then inc(P);
      if P^ = #10 then inc(P);
    end;
end;

//==============================================================================
//
// TDStrings.SetValue
//
//==============================================================================
procedure TDStrings.SetValue(const Name, Value: string);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then I := Add('');
    Put(I, Name + '=' + Value);
  end
  else
  begin
    if I >= 0 then Delete(I);
  end;
end;

//==============================================================================
// TDStringList.Destroy
//
////////////////////////////////////////////////////////////////////////////////
// TStringList
//
//==============================================================================
destructor TDStringList.Destroy;
begin
  inherited Destroy;
  if FCount <> 0 then
    Finalize(FList[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

//==============================================================================
//
// TDStringList.Add
//
//==============================================================================
function TDStringList.Add(const S: string): Integer;
begin
  result := FCount;
  InsertItem(result, S);
end;

//==============================================================================
//
// TDStringList.Clear
//
//==============================================================================
procedure TDStringList.Clear;
begin
  if FCount <> 0 then
  begin
    Finalize(FList[0], FCount);
    FCount := 0;
    SetCapacity(0);
  end;
end;

//==============================================================================
//
// TDStringList.Delete
//
//==============================================================================
procedure TDStringList.Delete(Index: Integer);
begin
  if (Index >= 0) and (Index < FCount) then
  begin
    Finalize(FList[Index]);
    dec(FCount);
    if Index < FCount then
      System.Move(FList[Index + 1], FList[Index],
        (FCount - Index) * SizeOf(TStringItem));
  end;
end;

//==============================================================================
//
// TDStringList.Exchange
//
//==============================================================================
procedure TDStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    exit;
  if (Index2 < 0) or (Index2 >= FCount) then
    exit;
  ExchangeItems(Index1, Index2);
end;

//==============================================================================
//
// TDStringList.ExchangeItems
//
//==============================================================================
procedure TDStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Integer;
  Item1, Item2: PStringItem;
begin
  Item1 := @FList[Index1];
  Item2 := @FList[Index2];
  Temp := Integer(Item1.FString);
  Integer(Item1.FString) := Integer(Item2.FString);
  Integer(Item2.FString) := Temp;
  Temp := Integer(Item1.FObject);
  Integer(Item1.FObject) := Integer(Item2.FObject);
  Integer(Item2.FObject) := Temp;
end;

//==============================================================================
//
// TDStringList.Get
//
//==============================================================================
function TDStringList.Get(Index: Integer): string;
begin
  if (Index >= 0) and (Index < FCount) then
    result := FList[Index].FString
  else
    result := '';
end;

//==============================================================================
//
// TDStringList.GetCapacity
//
//==============================================================================
function TDStringList.GetCapacity: Integer;
begin
  result := FCapacity;
end;

//==============================================================================
//
// TDStringList.GetCount
//
//==============================================================================
function TDStringList.GetCount: Integer;
begin
  result := FCount;
end;

//==============================================================================
//
// TDStringList.GetObject
//
//==============================================================================
function TDStringList.GetObject(Index: Integer): TObject;
begin
  if (Index >= 0) and (Index < FCount) then
    result := FList[Index].FObject
  else
    result := nil;
end;

//==============================================================================
//
// TDStringList.Grow
//
//==============================================================================
procedure TDStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

//==============================================================================
//
// TDStringList.Insert
//
//==============================================================================
procedure TDStringList.Insert(Index: Integer; const S: string);
begin
  if (Index >= 0) and (Index <= FCount) then
    InsertItem(Index, S);
end;

//==============================================================================
//
// TDStringList.InsertItem
//
//==============================================================================
procedure TDStringList.InsertItem(Index: Integer; const S: string);
begin
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(TStringItem));
  with FList[Index] do
  begin
    Pointer(FString) := nil;
    FObject := nil;
    FString := S;
  end;
  inc(FCount);
end;

//==============================================================================
//
// TDStringList.Put
//
//==============================================================================
procedure TDStringList.Put(Index: Integer; const S: string);
begin
  if (Index > 0) and (Index < FCount) then
    FList[Index].FString := S;
end;

//==============================================================================
//
// TDStringList.PutObject
//
//==============================================================================
procedure TDStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index >= 0) and (Index < FCount) then
    FList[Index].FObject := AObject;
end;

//==============================================================================
//
// TDStringList.SetCapacity
//
//==============================================================================
procedure TDStringList.SetCapacity(NewCapacity: Integer);
begin
  realloc(pointer(FList), FCapacity * SizeOf(TStringItem), NewCapacity * SizeOf(TStringItem));
  FCapacity := NewCapacity;
end;

//==============================================================================
// getenv
//
////////////////////////////////////////////////////////////////////////////////
//
//==============================================================================
function getenv(const env: string): string;
var
  buf: array[0..2047] of char;
begin
  ZeroMemory(@buf, SizeOf(buf));
  GetEnvironmentVariable(PChar(env), buf, 2047);
  result := Trim(StringVal(buf));
end;

//==============================================================================
//
// fexists
//
//==============================================================================
function fexists(const filename: string): boolean;
begin
  result := FileExists(filename);
end;

//==============================================================================
//
// direxists
//
//==============================================================================
function direxists(const dirname: string): boolean;
begin
  result := DirectoryExists(dirname);
end;

//==============================================================================
//
// fexpand
//
//==============================================================================
function fexpand(const filename: string): string;
begin
  result := ExpandFileName(filename);
end;

//==============================================================================
//
// fpath
//
//==============================================================================
function fpath(const filename: string): string;
begin
  result := ExtractFilePath(filename);
end;

//==============================================================================
//
// fdelete
//
//==============================================================================
procedure fdelete(const filename: string);
begin
  if fexists(filename) then
    DeleteFile(filename);
end;

//==============================================================================
//
// frename
//
//==============================================================================
procedure frename(const src, dest: string);
begin
  if fexists(src) then
  begin
    fdelete(dest);
    RenameFile(src, dest);
  end;
end;

//==============================================================================
//
// fext
//
//==============================================================================
function fext(const filename: string): string;
begin
  result := ExtractFileExt(filename);
end;

//==============================================================================
//
// fname
//
//==============================================================================
function fname(const filename: string): string;
begin
  result := ExtractFileName(filename);
end;

//==============================================================================
//
// fmask
//
//==============================================================================
function fmask(const mask: string): string;
begin
  result := mask;
  if result = '' then
    result := '*.*';
end;

//==============================================================================
//
// findfile
//
//==============================================================================
function findfile(const mask: string): string;
var
  sr: TSearchRec;
  mask1: string;
begin
  mask1 := fmask(mask);
  if FindFirst(mask1, faAnyFile, sr) = 0 then
  begin
    result := sr.Name;
    FindClose(sr);
  end
  else
    result := '';
end;

//==============================================================================
//
// findfiles
//
//==============================================================================
function findfiles(const mask: string): TDStringList;
var
  sr: TSearchRec;
  mask1: string;
begin
  result := TDStringList.Create;
  mask1 := fmask(mask);
  if FindFirst(mask1, faAnyFile, sr) = 0 then
  begin
    result.Add(sr.Name);
    while FindNext(sr) = 0 do
      result.Add(sr.Name);
    FindClose(sr);
  end;
end;

//==============================================================================
//
// tan
//
//==============================================================================
function tan(const x: extended): extended;
var
  a: single;
  b: single;
begin
  b := cos(x);
  if b <> 0 then
  begin
    a := sin(x);
    result := a / b;
  end
  else
    result := 0.0;
end;

//==============================================================================
//
// strcmp
//
//==============================================================================
function strcmp(const s1, s2: string): integer;
begin
  if s1 = s2 then
    result := 0
  else if s1 < s2 then
    result := -1
  else
    result := 1;
end;

//==============================================================================
//
// strupper
//
//==============================================================================
function strupper(const S: string): string;
var
  Ch: Char;
  L: Integer;
  Source, Dest: PChar;
begin
  L := Length(S);
  SetLength(result, L);
  Source := Pointer(S);
  Dest := Pointer(result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'a') and (Ch <= 'z') then dec(Ch, 32);
    Dest^ := Ch;
    inc(Source);
    inc(Dest);
    dec(L);
  end;
end;

//==============================================================================
//
// strupperproc
//
//==============================================================================
procedure strupperproc(var S: string);
var
  Ch: Char;
  i, L: Integer;
begin
  L := Length(S);
  for i := 1 to L do
  begin
    Ch := S[i];
    if (Ch >= 'a') and (Ch <= 'z') then
    begin
      dec(Ch, 32);
      S[i] := Ch;
    end;
  end;
end;

//==============================================================================
//
// strlower
//
//==============================================================================
function strlower(const S: string): string;
var
  Ch: Char;
  L: Integer;
  Source, Dest: PChar;
begin
  L := Length(S);
  SetLength(result, L);
  Source := Pointer(S);
  Dest := Pointer(result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'A') and (Ch <= 'Z') then inc(Ch, 32);
    Dest^ := Ch;
    inc(Source);
    inc(Dest);
    dec(L);
  end;
end;

//==============================================================================
//
// strlowerproc
//
//==============================================================================
procedure strlowerproc(var S: string);
var
  Ch: Char;
  i, L: Integer;
begin
  L := Length(S);
  for i := 1 to L do
  begin
    Ch := S[i];
    if (Ch >= 'A') and (Ch <= 'Z') then
    begin
      inc(Ch, 32);
      S[i] := Ch;
    end;
  end;
end;

//==============================================================================
//
// toupper
//
//==============================================================================
function toupper(ch: Char): Char;
asm
{ ->    AL      Character       }
{ <-    AL      result          }

  cmp al, 'a'
  jb  @@exit
  cmp al, 'z'
  ja  @@exit
  sub al, 'a' - 'A'
@@exit:
end;

//==============================================================================
//
// tolower
//
//==============================================================================
function tolower(ch: Char): Char;
asm
{ ->    AL      Character       }
{ <-    AL      result          }

  cmp al, 'A'
  jb  @@exit
  cmp al, 'Z'
  ja  @@exit
  sub al, 'A' - 'a'
@@exit:
end;

//==============================================================================
//
// strremovespaces
//
//==============================================================================
function strremovespaces(const s: string): string;
var
  i: integer;
begin
  result := '';
  for i := 1 to Length(s) do
    if s[i] <> ' ' then
      result := result + s[i];
end;

//==============================================================================
//
// _SHL
//
//==============================================================================
function _SHL(const x: integer; const bits: integer): integer; assembler;
asm
  mov ecx, edx
  sal eax, cl
end;

//==============================================================================
//
// _SHLW
//
//==============================================================================
function _SHLW(const x: LongWord; const bits: LongWord): LongWord;
begin
  result := x shl bits;
end;

//==============================================================================
//
// _SHR
//
//==============================================================================
function _SHR(const x: integer; const bits: integer): integer; assembler;
asm
  mov ecx, edx
  sar eax, cl
end;

//==============================================================================
//
// _SHR1
//
//==============================================================================
function _SHR1(const x: integer): integer; assembler;
asm
  sar eax, 1
end;

//==============================================================================
//
// _SHR2
//
//==============================================================================
function _SHR2(const x: integer): integer; assembler;
asm
  sar eax, 2
end;

//==============================================================================
//
// _SHR3
//
//==============================================================================
function _SHR3(const x: integer): integer; assembler;
asm
  sar eax, 3
end;

//==============================================================================
//
// _SHR4
//
//==============================================================================
function _SHR4(const x: integer): integer; assembler;
asm
  sar eax, 4
end;

//==============================================================================
//
// _SHR5
//
//==============================================================================
function _SHR5(const x: integer): integer;
asm
  sar eax, 5
end;

//==============================================================================
//
// _SHR7
//
//==============================================================================
function _SHR7(const x: integer): integer; assembler;
asm
  sar eax, 7
end;

//==============================================================================
//
// _SHR8
//
//==============================================================================
function _SHR8(const x: integer): integer; assembler;
asm
  sar eax, 8
end;

//==============================================================================
//
// _SHR11
//
//==============================================================================
function _SHR11(const x: integer): integer; assembler;
asm
  sar eax, 11
end;

//==============================================================================
//
// _SHR14
//
//==============================================================================
function _SHR14(const x: integer): integer; assembler;
asm
  sar eax, 14
end;

//==============================================================================
//
// _SHRW
//
//==============================================================================
function _SHRW(const x: LongWord; const bits: LongWord): LongWord;
begin
  result := x shr bits;
end;

//==============================================================================
//
// StringVal
//
//==============================================================================
function StringVal(const Str: PChar): string;
begin
  sprintf(result, '%s', [Str]);
end;

//==============================================================================
//
// ZeroMemory
//
//==============================================================================
procedure ZeroMemory(const dest0: pointer; const count0: integer);
{$IFNDEF FPC}
var
  data: union_8b;
  pdat: pointer;
  dest: PByte;
  count: integer;
{$ENDIF}
begin
  {$IFNDEF FPC}
  if mmxMachine = 0 then
  begin
  {$ENDIF}
    FillChar(dest0^, count0, 0);
  {$IFNDEF FPC}
    exit;
  end;

  dest := PByte(dest0);
  count := count0;

  while (count > 0) and (integer(dest) and 7 <> 0) do
  begin
    dest^ := 0;
    inc(dest);
    dec(count);
  end;

  if count = 0 then
  begin
    exit;
  end;

  data.dwords[0] := 0;
  data.dwords[1] := 0;
  pdat := @data;

  if count >= 64 then
  begin
    asm
      push esi
      push edi

      mov edi, dest
      mov esi, pdat

      mov ecx, count
      // 64 bytes per iteration
      shr ecx, 6
      // Read in source data
      movq mm1, [esi]
      movq mm2, mm1
      movq mm3, mm1
      movq mm4, mm1
      movq mm5, mm1
      movq mm6, mm1
      movq mm7, mm1
      movq mm0, mm1
@@loop1:
      // Non-temporal stores
      movntq [edi], mm1
      movntq [edi + 8], mm2
      movntq [edi + 16], mm3
      movntq [edi + 24], mm4
      movntq [edi + 32], mm5
      movntq [edi + 40], mm6
      movntq [edi + 48], mm7
      movntq [edi + 56], mm0

      add edi, 64
      dec ecx
      jnz @@loop1

      pop edi
      pop esi
    end;

    inc(dest, count and not 63);
    count := count and 63;
  end;

  if count >= 8 then
  begin
    asm
      push esi
      push edi

      mov edi, dest
      mov esi, pdat

      mov ecx, count
      // 8 bytes per iteration
      shr ecx, 3
      // Read in source data
      movq mm1, [esi]
@@loop2:
      // Non-temporal stores
      movntq  [edi], mm1

      add edi, 8
      dec ecx
      jnz @@loop2

      pop edi
      pop esi
    end;
    inc(dest, count and not 7);
    count := count and 7;
  end;

  while count > 0 do
  begin
    dest^ := 0;
    inc(dest);
    dec(count);
  end;

  asm
    emms
  end;
{$ENDIF}
end;

//==============================================================================
//
// fopen
//
//==============================================================================
function fopen(var f: file; const FileName: string; const mode: integer): boolean;
begin
  assign(f, FileName);
  {$I-}
  if mode = fCreate then
  begin
    FileMode := 2;
    rewrite(f, 1);
  end
  else if mode = fOpenReadOnly then
  begin
    FileMode := 0;
    reset(f, 1);
  end
  else if mode = fOpenReadWrite then
  begin
    FileMode := 2;
    reset(f, 1);
  end
  else
  begin
    result := false;
    exit;
  end;
  {$I+}
  result := IOresult = 0;
end;

//==============================================================================
//
// fwrite
//
//==============================================================================
function fwrite(const data: pointer; const sz1, sz2: integer; var f: file): boolean;
var
  N1: integer;
  N2: integer;
begin
  N1 := sz1 * sz2;
  {$I-}
  BlockWrite(f, PByteArray(data)^, N1, N2);
  {$I+}
  result := N1 = N2;
end;

//==============================================================================
//
// fsize
//
//==============================================================================
function fsize(const FileName: string): integer;
var
  f: file;
begin
  if fopen(f, FileName, fOpenReadOnly) then
  begin
  {$I-}
    result := FileSize(f);
    close(f);
  {$I+}
  end
  else
    result := 0;
end;

//==============================================================================
//
// fshortname
//
//==============================================================================
function fshortname(const FileName: string): string;
var
  i: integer;
begin
  result := '';
  for i := Length(FileName) downto 1 do
  begin
    if FileName[i] in ['\', '/'] then
      break;
    result := FileName[i] + result;
  end;
end;

//==============================================================================
//
// fixslashpath
//
//==============================================================================
function fixslashpath(const apath: string): string;
var
  i: integer;
begin
  result := apath;
  for i := 1 to length(result) do
    if result[i] = '/' then
      result[i] := '\';
end;

//==============================================================================
//
// strtrim
//
//==============================================================================
function strtrim(const S: string): string;
var
  I, L: Integer;
  len: integer;
begin
  len := Length(S);
  L := len;
  I := 1;
  while (I <= L) and (S[I] <= ' ') do inc(I);
  if I > L then
    result := ''
  else
  begin
    while S[L] <= ' ' do dec(L);
    if (I = 1) and (L = len) then
      result := S
    else
      result := Copy(S, I, L - I + 1);
  end;
end;

//==============================================================================
//
// capitalizedstring
//
//==============================================================================
function capitalizedstring(const S: string; const splitter: char = ' '): string;
var
  i: integer;
  c: string;
begin
  if S = '' then
  begin
    result := '';
    exit;
  end;

  result := strlower(S);
  result[1] := toupper(result[1]);
  c := tolower(splitter);
  for i := 2 to Length(result) do
  begin
    if result[i - 1] = c then
      result[i] := toupper(result[i])
  end;
end;

//==============================================================================
//
// splitstring
//
//==============================================================================
procedure splitstring(const inp: string; var out1, out2: string; const splitter: string = ' ');
var
  p: integer;
begin
  p := Pos(splitter, inp);
  if p = 0 then
  begin
    out1 := inp;
    out2 := '';
  end
  else
  begin
    out1 := strtrim(Copy(inp, 1, p - 1));
    out2 := strtrim(Copy(inp, p + 1, Length(inp) - p));
  end;
end;

//==============================================================================
//
// splitstring
//
//==============================================================================
procedure splitstring(const inp: string; var out1, out2: string; const splitters: charset_t);
var
  i: integer;
  p: integer;
  inp1: string;
begin
  inp1 := inp;
  p := 0;
  for i := 1 to Length(inp1) do
    if inp1[i] in splitters then
    begin
      p := i;
      break;
    end;
  if p = 0 then
  begin
    out1 := inp1;
    out2 := '';
  end
  else
  begin
    out1 := strtrim(Copy(inp1, 1, p - 1));
    out2 := strtrim(Copy(inp1, p + 1, Length(inp) - p));
  end;
end;

//==============================================================================
//
// trimproc
//
//==============================================================================
procedure trimproc(var s: string);
var
  I, L: Integer;
  len: integer;
  j: integer;
begin
  len := Length(S);
  L := len;
  I := 1;
  while (I <= L) and (S[I] <= ' ') do inc(I);
  if I > L then
    S := ''
  else
  begin
    while S[L] <= ' ' do dec(L);
    if (I = 1) and (L = len) then
    else
    begin
      len := L - I + 1;
      if i <> 1 then
        for j := 1 to L do
        begin
          s[j] := s[i];
          Inc(i);
        end;
      SetLength(s, len);
    end;
  end;
end;

//==============================================================================
//
// trimprocU
//
//==============================================================================
procedure trimprocU(var s: string);
var
  I, L: Integer;
  len: integer;
  j: integer;
  Ch: char;
begin
  len := Length(S);
  L := len;
  I := 1;
  while (I <= L) and (S[I] <= ' ') do inc(I);
  if I > L then
    S := ''
  else
  begin
    while S[L] <= ' ' do dec(L);
    if I = 1 then
    begin
      for j := 1 to L do
      begin
        Ch := s[j];
        if (Ch >= 'a') and (Ch <= 'z') then
          Dec(s[j], 32);
      end;
      SetLength(s, L);
    end
    else
    begin
      len := L - I + 1;
      if i <> 1 then
        for j := 1 to L do
        begin
          Ch := s[i];
          if (Ch >= 'a') and (Ch <= 'z') then
            Dec(Ch, 32);
          s[j] := Ch;
          Inc(i);
        end;
      SetLength(s, len);
    end;
  end;
end;

//==============================================================================
//
// splitstring_ch
//
//==============================================================================
procedure splitstring_ch(const inp: string; var out1, out2: string; const splitter: char = ' ');
var
  p, len: integer;
  i: integer;
begin
  p := 1;
  len := Length(inp);
  while p <= len do
  begin
    if inp[p] = splitter then
      break;
    Inc(p);
  end;
  if p > len then
  begin
    out1 := inp;
    out2 := '';
  end
  else
  begin
    SetLength(out1, p - 1);
    for i := 1 to p - 1 do
      out1[i] := inp[i];
    trimproc(out1);

    SetLength(out2, len - p);
    for i := p + 1 to len do
      out2[i - p] := inp[i];
    trimproc(out2);
  end;
end;

//==============================================================================
//
// firstword
//
//==============================================================================
function firstword(const inp: string; const splitter: string = ' '): string;
var
  tmp: string;
begin
  splitstring(inp, result, tmp, splitter);
end;

//==============================================================================
//
// firstword_ch
//
//==============================================================================
function firstword_ch(const inp: string; const splitter: char = ' '): string;
var
  p, len: integer;
begin
  p := 1;
  len := Length(inp);
  SetLength(result, len);
  while p <= len do
  begin
    if inp[p] = splitter then
      break;
    result[p] := inp[p];
    Inc(p);
  end;
  SetLength(result, p - 1);
end;

//==============================================================================
//
// firstword
//
//==============================================================================
function firstword(const inp: string; const splitters: charset_t): string;
var
  tmp: string;
begin
  splitstring(inp, result, tmp, splitters);
end;

//==============================================================================
//
// parsefirstword
//
//==============================================================================
function parsefirstword(const inp: string): string;
var
  st: string;
  tmp: string;
  i: integer;
begin
  st := strtrim(inp);
  if st = '' then
  begin
    result := '';
    exit;
  end;

  if st[1] = '"' then
  begin
    result := '';
    for i := 2 to Length(st) do
    begin
      if st[i] = '"' then
        break;
      result := result + st[i];
    end;
    exit;
  end;

  splitstring_ch(st, result, tmp, ' ');
end;

//==============================================================================
//
// secondword
//
//==============================================================================
function secondword(const inp: string; const splitter: string = ' '): string;
var
  tmp: string;
begin
  splitstring(inp, tmp, result, splitter);
end;

//==============================================================================
//
// secondword
//
//==============================================================================
function secondword(const inp: string; const splitters: charset_t): string;
var
  tmp: string;
begin
  splitstring(inp, tmp, result, splitters);
end;

//==============================================================================
//
// lastword
//
//==============================================================================
function lastword(const inp: string; const splitter: string = ' '): string;
var
  i: integer;
begin
  result := '';
  i := length(inp);
  while i > 0 do
  begin
    if inp[i] = splitter then
      exit
    else
    begin
      result := inp[i] + result;
      dec(i);
    end;
  end;
end;

//==============================================================================
//
// lastword
//
//==============================================================================
function lastword(const inp: string; const splitters: charset_t): string; overload;
var
  i: integer;
begin
  result := '';
  i := length(inp);
  while i > 0 do
  begin
    if inp[i] in splitters then
      exit
    else
    begin
      result := inp[i] + result;
      dec(i);
    end;
  end;
end;

//==============================================================================
//
// FreeAndNil
//
//==============================================================================
procedure FreeAndNil(var Obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;

//==============================================================================
//
// StrLCopy
//
//==============================================================================
function StrLCopy(Dest: PChar; const Source: PChar; MaxLen: Cardinal): PChar; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EBX,ECX
        XOR     AL,AL
        TEST    ECX,ECX
        JZ      @@1
        REPNE   SCASB
        JNE     @@1
        INC     ECX
@@1:    SUB     EBX,ECX
        MOV     EDI,ESI
        MOV     ESI,EDX
        MOV     EDX,EDI
        MOV     ECX,EBX
        SHR     ECX,2
        REP     MOVSD
        MOV     ECX,EBX
        AND     ECX,3
        REP     MOVSB
        STOSB
        MOV     EAX,EDX
        POP     EBX
        POP     ESI
        POP     EDI
end;

//==============================================================================
//
// fabs
//
//==============================================================================
function fabs(const f: float): float;
begin
  if f >= 0 then
    result := f
  else
    result := -f;
end;

//==============================================================================
//
// MakeDir
//
//==============================================================================
procedure MakeDir(const dir: string);
begin
  CreateDir(dir);
end;

//==============================================================================
//
// PascalText
//
//==============================================================================
function PascalText(src: PChar): string;
var
  prev: char;
begin
  result := '';
  if src^ = #0 then
    exit;
  repeat
    prev := src^;
    inc(src);
    if (src^ = #10) and (prev <> #13) then
      result := result + prev + #13#10
    else if not (prev in [#10, #13]) then
      result := result + prev;
  until src^ = #0;
end;

//==============================================================================
//
// PascalText
//
//==============================================================================
function PascalText(src: PChar; const maxsize: integer): string;
var
  prev: char;
  cnt: integer;
begin
  result := '';
  if src^ = #0 then
    exit;
  cnt := 0;
  repeat
    prev := src^;
    inc(src);
    inc(cnt);
    if (src^ = #10) and (prev <> #13) then
      result := result + prev + #13#10
    else if not (prev in [#10, #13]) then
      result := result + prev;
  until (cnt = maxsize) or (src^ = #0);
end;

//==============================================================================
//
// CopyFile
//
//==============================================================================
procedure CopyFile(const sname, dname: string);
var
  FromF, ToF: file;
  NumRead, NumWritten: Integer;
  Buf: array[1..8192] of Char;
begin
  if fexists(sname) then
  begin
    AssignFile(FromF, sname);
    Reset(FromF, 1);
    AssignFile(ToF, dname);
    Rewrite(ToF, 1);
    repeat
      BlockRead(FromF, Buf, SizeOf(Buf), NumRead);
      BlockWrite(ToF, Buf, NumRead, NumWritten);
    until (NumRead = 0) or (NumWritten <> NumRead);
    CloseFile(FromF);
    CloseFile(ToF);
  end
  else
  begin
    AssignFile(ToF, dname);
    Rewrite(ToF, 1);
    CloseFile(ToF);
  end;
end;

//==============================================================================
//
// CopyFile2
//
//==============================================================================
procedure CopyFile2(const FromN, ToN: string);
var
  FromF, ToF: file;
  NumRead, NumWritten: Integer;
  Buf: array[1..8192] of Char;
begin
  if fexists(FromN) then
  begin
    AssignFile(FromF, FromN);
    Reset(FromF, 1);
    AssignFile(ToF, ToN);
    Rewrite(ToF, 1);
    repeat
      BlockRead(FromF, Buf, SizeOf(Buf), NumRead);
      BlockWrite(ToF, Buf, NumRead, NumWritten);
    until (NumRead = 0) or (NumWritten <> NumRead);
    CloseFile(FromF);
    CloseFile(ToF);
  end;
end;

//==============================================================================
//
// IsIntegerInRange
//
//==============================================================================
function IsIntegerInRange(const test, f1, f2: integer): boolean;
begin
  if f1 < f2 then
    result := (test >= f1) and (test <= f2)
  else
    result := (test >= f2) and (test <= f1)
end;

//==============================================================================
//
// IsLongWordInRange
//
//==============================================================================
function IsLongWordInRange(const test, f1, f2: LongWord): boolean;
begin
  if f1 < f2 then
    result := (test >= f1) and (test <= f2)
  else
    result := (test >= f2) and (test <= f1)
end;

//==============================================================================
//
// IsFloatInRange
//
//==============================================================================
function IsFloatInRange(const test, f1, f2: float): boolean;
begin
  if f1 < f2 then
    result := (test >= f1) and (test <= f2)
  else
    result := (test >= f2) and (test <= f1)
end;

//==============================================================================
//
// IsDoubleInRange
//
//==============================================================================
function IsDoubleInRange(const test, f1, f2: double): boolean;
begin
  if f1 < f2 then
    result := (test >= f1) and (test <= f2)
  else
    result := (test >= f2) and (test <= f1)
end;

//==============================================================================
//
// IsExtendedInRange
//
//==============================================================================
function IsExtendedInRange(const test, f1, f2: Extended): boolean;
begin
  if f1 < f2 then
    result := (test >= f1) and (test <= f2)
  else
    result := (test >= f2) and (test <= f1)
end;

//==============================================================================
//
// GetAllocMemSize
//
//==============================================================================
function GetAllocMemSize: integer;
{$IF CompilerVersion >= 18}
var
  I: Integer;
  MemMgrState: TMemoryManagerState;
{$IFEND}
begin
{$IF CompilerVersion < 18}
  Result := AllocMemSize;
{$ELSE}
  GetMemoryManagerState(MemMgrState);
  Result := MemMgrState.TotalAllocatedMediumBlockSize +
    MemMgrState.TotalAllocatedLargeBlockSize;
  for I := 0 to High(MemMgrState.SmallBlockTypeStates) do
  begin
    inc(result, MemMgrState.SmallBlockTypeStates[I].InternalBlockSize);
    inc(result, MemMgrState.SmallBlockTypeStates[I].UseableBlockSize);
  end;
{$IFEND}
end;

//==============================================================================
//
// MkDir
//
//==============================================================================
function MkDir(const d: string): boolean;
begin
  try
    if direxists(d) then
    begin
      result := true;
      exit;
    end;

    result := CreateDir(d);
    if not result then
      result := ForceDirectories(d);
  except
    result := false;
  end;
end;

//==============================================================================
//
// TString.Create
//
//==============================================================================
constructor TString.Create(const astring: string);
begin
  str := astring;
end;

//==============================================================================
//
// TInteger.Create
//
//==============================================================================
constructor TInteger.Create(const aint: integer);
begin
  intnum := aint;
end;

//==============================================================================
//
// TFloat.Create
//
//==============================================================================
constructor TFloat.Create(const aflt: float);
begin
  floatnum := aflt;
end;

//==============================================================================
//
// MinI
//
//==============================================================================
function MinI(const a, b: integer): integer;
begin
  if a > b then
    result := b
  else
    result := a;
end;

//==============================================================================
//
// MaxI
//
//==============================================================================
function MaxI(const a, b: integer): integer;
begin
  if a > b then
    result := a
  else
    result := b;
end;

//==============================================================================
//
// NowTime
//
//==============================================================================
function NowTime: TDateTime;
begin
  result := Now;
end;

//==============================================================================
//
// formatDateTimeAsString
//
//==============================================================================
function formatDateTimeAsString(const Format: string; DateTime: TDateTime): string;
begin
  DateTimeToString(Result, Format, DateTime);
end;

//==============================================================================
//
// QSortIntegers
//
//==============================================================================
procedure QSortIntegers(const A: PIntegerArray; const Len: integer);

  procedure qsortI(l, r: Integer);
  var
    i, j: integer;
    t: integer;
    d: integer;
  begin
    repeat
      i := l;
      j := r;
      d := A[(l + r) shr 1];
      repeat
        while A[i] < d do
          inc(i);
        while A[j] > d do
          dec(j);
        if i <= j then
        begin
          t := A[i];
          A[i] := A[j];
          A[j] := t;
          inc(i);
          dec(j);
        end;
      until i > j;
      if l < j then
        qsortI(l, j);
      l := i;
    until i >= r;
  end;

begin
  if Len > 1 then
    qsortI(0, Len - 1);
end;

//==============================================================================
//
// QSortFloats
//
//==============================================================================
procedure QSortFloats(const A: PFloatArray; const Len: integer);

  procedure qsortF(l, r: Integer);
  var
    i, j: integer;
    t: float;
    f: float;
  begin
    repeat
      i := l;
      j := r;
      f := A[(l + r) shr 1];
      repeat
        while A[i] < f do
          inc(i);
        while A[j] > f do
          dec(j);
        if i <= j then
        begin
          t := A[i];
          A[i] := A[j];
          A[j] := t;
          inc(i);
          dec(j);
        end;
      until i > j;
      if l < j then
        qsortF(l, j);
      l := i;
    until i >= r;
  end;

begin
  if Len > 1 then
    qsortF(0, Len - 1);
end;

//==============================================================================
//
// StrIsInteger
//
//==============================================================================
function StrIsInteger(const s: string): Boolean;
var
  check: string;
begin
  if CharPos('-', s) = 1 then
  begin
    check := s;
    delete(check, 1, 1);
    result := StrIsLongWord(check)
  end
  else
    result := StrIsLongWord(s);
end;

//==============================================================================
//
// StrIsLongWord
//
//==============================================================================
function StrIsLongWord(const s: string): Boolean;
var
  i: integer;
begin
  Result := False;
  for i := 1 to Length(s) do
  begin
    if s[i] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] then
      Result := True
    else
    begin
      Result := False;
      Exit;
    end;
  end;
end;

//==============================================================================
//
// StrIsFloat
//
//==============================================================================
function StrIsFloat(const s: string): Boolean;
var
  i: integer;
  check: string;
  dot: Boolean;
begin
  Result := False;
  dot := False;
  check := s;
  if check = '' then
    exit;
  if check[1] = '-' then
    delete(check, 1, 1);
  for i := 1 to Length(check) do
  begin
    if check[i] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] then
      Result := True
    else if check[i] in ['.', ',', DecimalSeparator] then
    begin
      if dot then
      begin
        Result := False;
        Exit;
      end
      else
        dot := True;
    end
    else
    begin
      Result := False;
      Exit;
    end;
  end;
end;

//==============================================================================
//
// SaveStreamToFile
//
//==============================================================================
function SaveStreamToFile(const s: TDStream; const fname: string): boolean;
var
  p: integer;
  sz: integer;
  buf: PByteArray;
  fs: TFile;
begin
  result := true;
  try
    p := s.Position;
    sz := s.Size;
    s.Seek(0, sFromBeginning);
    buf := malloc(sz);
    s.Read(buf^, sz);
    s.Seek(p, sFromBeginning);

    fs := TFile.Create(fname, fcreate);
    try
      fs.Write(buf^, sz);
    finally
      fs.Free;
    end;

    memfree(Pointer(buf), sz);
  except
    result := false;
  end;
end;

//==============================================================================
//
// GetIntegerInRange
//
//==============================================================================
function GetIntegerInRange(const val, f1, f2: integer): integer;
begin
  if val < f1 then
    result := f1
  else if val > f2 then
    result := f2
  else
    result := val;
end;

//==============================================================================
//
// GetInt64InRange
//
//==============================================================================
function GetInt64InRange(const val, f1, f2: int64): integer;
begin
  if val < f1 then
    result := f1
  else if val > f2 then
    result := f2
  else
    result := val;
end;

//==============================================================================
//
// ibetween
//
//==============================================================================
function ibetween(const x: integer; const x1, x2: integer): integer;
begin
  if x <= x1 then
    result := x1
  else if x >= x2 then
    result := x2
  else
    result := x;
end;

//==============================================================================
//
// LeftStr
//
//==============================================================================
function LeftStr(const s: string; const l: integer): string;
var
  i: integer;
  inplen: integer;
begin
  result := '';
  inplen := Length(s);
  for i := 1 to l do
  begin
    if i > inplen then
      Break;
    result := result + s[i];
  end;
end;

//==============================================================================
//
// RightStr
//
//==============================================================================
function RightStr(const s: string; const l: integer): string;
var
  i: integer;
  inplen: integer;
begin
  result := '';
  inplen := Length(s);
  for i := inplen downto inplen - l + 1 do
  begin
    if i < 1 then
      Break;
    result := s[i] + result;
  end;
end;

//==============================================================================
//
// fixpathname
//
//==============================================================================
function fixpathname(const s: string): string;
var
  i: integer;
  tmp: string;
  last: char;
begin
  tmp := s;
  for i := 1 to Length(tmp) do
    if tmp[i] = '/' then
      tmp[i] := '\';

  result := '';
  last := Chr(0);
  for i := 1 to Length(tmp) do
  begin
    if (last <> '\') or (tmp[i] <> '\') then
      result := result + tmp[i];
    last := tmp[i];
  end;
end;

//==============================================================================
//
// logtofile
//
//==============================================================================
procedure logtofile(const fname: string; const str: string);
var
  f: file;
begin
  if not fexists(fname) then
    fopen(f, fname, fCreate)
  else
  begin
    fopen(f, fname, fOpenReadWrite);
    system.Seek(f, FileSize(f));
  end;
  {$I-}
  BlockWrite(f, Pointer(str)^, Length(str));
  {$I+}
  close(f);
end;

//==============================================================================
//
// wordstolist
//
//==============================================================================
function wordstolist(const inp: string; const splitters: charset_t): TDStringList;
var
  i: integer;
  stmp: string;
begin
  result := TDStringList.Create;
  stmp := '';
  for i := 1 to length(inp) do
  begin
    if inp[i] in splitters then
    begin
      trimproc(stmp);
      if stmp <> '' then
      begin
        result.Add(stmp);
        stmp := '';
      end;
    end
    else
      stmp := stmp + inp[i];
  end;
  if stmp <> '' then
  begin
    result.Add(stmp);
    stmp := '';
  end;
end;

//==============================================================================
//
// RemoveQuotesFromString
//
//==============================================================================
function RemoveQuotesFromString(const s: string): string;
begin
  Result := s;
  if Result = '' then
    Exit;
  if Result[1] = '"' then
    Delete(Result, 1, 1);
  if (Result <> '') and (Result[Length(Result)] = '"') then
    Delete(Result, Length(Result), 1);
end;

//==============================================================================
//
// isdigit
//
//==============================================================================
function isdigit(const c: char): boolean;
begin
  result := c in ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'];
end;

//==============================================================================
//
// Isign
//
//==============================================================================
function Isign(const x: integer): integer;
begin
  if x < 0 then
    result := -1
  else if x > 0 then
    result := 1
  else
    result := 0;
end;

//==============================================================================
//
// readablestring
//
//==============================================================================
function readablestring(const s: string): string;
var
  i: integer;
  h: string;
begin
  result := '';
  h := '0123456789ABCDEF';
  for i := 1 to Length(s) do
  begin
    if Pos(toupper(s[i]), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789') > 0 then
      result := result + toupper(s[i])
    else
      result := result + h[Ord(s[i]) div 16 + 1] + h[Ord(s[i]) mod 16 + 1];
  end;
end;

//==============================================================================
//
// CharPos
//
//==============================================================================
function CharPos(const ch: Char; const s: string): integer;
var
  i: integer;
begin
  for i := 1 to Length(s) do
    if s[i] = ch then
    begin
      result := i;
      exit;
    end;
  result := 0;
end;

function IsZeroes(const p: Pointer; const size: integer): boolean;
var
  pb: PByteArray;
  i: integer;
begin
  pb := p;
  for i := 0 to size - 1 do
    if pb[i] <> 0 then
    begin
      result := false;
      exit;
    end;
  result := true;
end;

procedure FillDWord(const dest: Pointer; Count: Integer; Value: LongWord); assembler; register;
asm
  push edi
  mov  edi, eax  // assign Destination
  mov  eax, ecx  // assign Value
  mov  ecx, edx
  rep  stosd
  pop edi
end;

end.

