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
//    Delphi specific routines
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
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

  fourbytes_t = packed record
    byte1, byte2, byte3, byte4: byte;
  end;

  eightbytes_t = packed record
    byte1, byte2, byte3, byte4: byte;
    byte5, byte6, byte7, byte8: byte;
  end;

  twointegers_t = packed record
    int1, int2: integer;
  end;

  twolongwords_t = packed record
    longword1, longword2: LongWord;
  end;

  TOutProc = procedure (const s: string);

var
  outproc: TOutProc = nil;

procedure sprintf(var s: string; const Fmt: string; const Args: array of const);

procedure printf(const str: string); overload;

procedure printf(const Fmt: string; const Args: array of const); overload;

function itoa(i: integer): string;

function uitoa(l: longword): string;

function ftoa(f: single): string;

function ftoafmt(const fmt: string; f: single): string;

function atoi(const s: string): integer; overload;

function atoi(const s: string; const default: integer): integer; overload;

function atoui(const s: string): LongWord; overload;

function atoui(const s: string; const default: LongWord): LongWord; overload;

function atof(const s: string): single; overload;

function atof(const s: string; const default: single): single; overload;

//
// Memory functions
//
function memmove(const destination, source: pointer; count: integer): pointer;

procedure memcpy(const dest0: pointer; const src0: pointer; count0: integer);

function memset(const dest0: pointer; const val: integer; const count0: integer): pointer;
function memsetsi(const dest0: pointer; const val: smallint; count0: integer): pointer;
function memseti(const dest0: pointer; const val: integer; const count0: integer): pointer;

function malloc(const size: integer): Pointer;

function mallocA(var Size: integer; const Align: integer; var original: pointer): pointer;

function mallocz(const size: integer): Pointer;

procedure realloc(var p: pointer; const oldsize, newsize: integer);

procedure memfree(var p: pointer; const size: integer);

var
  memoryusage: integer = 0;

function IntToStrZfill(const z: integer; const x: integer): string;

function intval(const b: boolean): integer;

function decide(const condition: boolean;
  const iftrue: integer; const iffalse: integer): integer; overload;

function decide(const condition: boolean;
  const iftrue: boolean; const iffalse: boolean): boolean; overload;

function decide(const condition: boolean;
  const iftrue: string; const iffalse: string): string; overload;

function decide(const condition: boolean;
  const iftrue: pointer; const iffalse: pointer): pointer; overload;

function decide(const condition: integer;
  const iftrue: integer; const iffalse: integer): integer; overload;

function decide(const condition: integer;
  const iftrue: boolean; const iffalse: boolean): boolean; overload;

function decide(const condition: integer;
  const iftrue: string; const iffalse: string): string; overload;

function decide(const condition: integer;
  const iftrue: pointer; const iffalse: pointer): pointer; overload;

function decidef(const condition: boolean;
  const iftrue: single; const iffalse: single): single;

function incp(var p: pointer; const size: integer = 1): pointer;

function pDiff(const p1, p2: pointer; const size: integer): integer;

function getenv(const env: string): string;

function fexists(const filename: string): boolean;

function direxists(const dirname: string): boolean;

function fexpand(const filename: string): string;

function fpath(const filename: string): string;

procedure fdelete(const filename: string);

procedure frename(const src, dest: string);

function fext(const filename: string): string;

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
    procedure Add(const value: string); overload; virtual;
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
    function  LoadFromFile(const FileName: string): boolean; virtual;
    function  LoadFromStream(const strm: TDStream): boolean; virtual;
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

function findfile(const mask: string): string;

function findfiles(const mask: string): TDStringList;

procedure fprintf(var f: file; const str: string); overload;

procedure fprintf(var f: file; const Fmt: string; const Args: array of const); overload;

procedure fprintf(const f: TFile; const str: string); overload;

procedure fprintf(const f: TFile; const Fmt: string; const Args: array of const); overload;

function tan(const x: extended): extended;

function strupper(const S: string): string;

function strlower(const S: string): string;

function toupper(ch: Char): Char;

function tolower(ch: Char): Char;

function strremovespaces(const s: string): string;

function _SHL(const x: integer; const bits: integer): integer;

function _SHLW(const x: LongWord; const bits: LongWord): LongWord;

function _SHR(const x: integer; const bits: integer): integer;
function _SHR1(const x: integer): integer;
function _SHR2(const x: integer): integer;
function _SHR3(const x: integer): integer;
function _SHR4(const x: integer): integer;
function _SHR7(const x: integer): integer;
function _SHR8(const x: integer): integer;
function _SHR14(const x: integer): integer;

function _SHRW(const x: LongWord; const bits: LongWord): LongWord;

function StringVal(const Str: PChar): string;

procedure ZeroMemory(const dest0: pointer; const count0: integer);

function fopen(var f: file; const FileName: string; const mode: integer): boolean;

function fwrite(const data: pointer; const sz1, sz2: integer; var f: file): boolean;

function fsize(const FileName: string): integer;

function fshortname(const FileName: string): string;

function fixslashpath(const apath: string): string;

function strtrim(const S: string): string;

function capitalizedstring(const S: string; const splitter: char = ' '): string;

procedure splitstring(const inp: string; var out1, out2: string; const splitter: string = ' '); overload;

procedure splitstring(const inp: string; var out1, out2: string; const splitters: charset_t); overload;

function firstword(const inp: string; const splitter: string = ' '): string; overload;

function firstword(const inp: string; const splitters: charset_t): string; overload;

function parsefirstword(const inp: string): string;

function secondword(const inp: string; const splitter: string = ' '): string; overload;

function secondword(const inp: string; const splitters: charset_t): string; overload;

function lastword(const inp: string; const splitter: string = ' '): string; overload;

function lastword(const inp: string; const splitters: charset_t): string; overload;

procedure FreeAndNil(var Obj);

function StrLCopy(Dest: PChar; const Source: PChar; MaxLen: Cardinal): PChar;

function fabs(const f: float): float;

procedure MakeDir(const dir: string);

function PascalText(src: PChar): string; overload;

function PascalText(src: PChar; const maxsize: integer): string; overload;

procedure CopyFile(const sname, dname: string);

procedure CopyFile2(const FromN, ToN: string);

function IsIntegerInRange(const test, f1, f2: integer): boolean;

function IsLongWordInRange(const test, f1, f2: LongWord): boolean;

function IsFloatInRange(const test, f1, f2: float): boolean;

function IsDoubleInRange(const test, f1, f2: double): boolean;

function IsExtendedInRange(const test, f1, f2: Extended): boolean;

function GetIntegerInRange(const val, f1, f2: integer): integer;

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

function GetAllocMemSize: integer;

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

function MinI(const a, b: integer): integer;

function MaxI(const a, b: integer): integer;

function NowTime: TDateTime;

function formatDateTimeAsString(const Format: string; DateTime: TDateTime): string;

procedure QSortIntegers(const A: PIntegerArray; const Len: integer);

procedure QSortFloats(const A: PFloatArray; const Len: integer);

function StrIsInteger(const s: string): Boolean;

function StrIsLongWord(const s: string): Boolean;

function StrIsFloat(const s: string): Boolean;

function SaveStreamToFile(const s: TDStream; const fname: string): boolean;

function ibetween(const x: integer; const x1, x2: integer): integer;

function LeftStr(const s: string; const l: integer): string;

function RightStr(const s: string; const l: integer): string;

function fixpathname(const s: string): string;

procedure logtofile(const fname: string; const str: string);

function wordstolist(const inp: string; const splitters: charset_t): TDStringList;

function RemoveQuotesFromString(const s: string): string;

function isdigit(const c: char): boolean;

function Isign(const x: integer): integer;

function readablestring(const s: string): string;

implementation

uses
  Windows,
  SysUtils
{$IFDEF FPC}
  ,d_fpc
{$ENDIF}
  ;

procedure sprintf(var s: string; const Fmt: string; const Args: array of const);
begin
  FmtStr(s, Fmt, Args);
end;

procedure printf(const str: string);
begin
  if Assigned(outproc) then
    outproc(str)
  else if IsConsole then
    write(str);
end;

procedure printf(const Fmt: string; const Args: array of const);
var
  s: string;
begin
  sprintf(s, Fmt, Args);
  printf(s);
end;

procedure fprintf(var f: file; const str: string);
begin
  {$I-}
  BlockWrite(f, (@str[1])^, Length(str));
  {$I+}
end;

procedure fprintf(var f: file; const Fmt: string; const Args: array of const);
var
  s: string;
begin
  sprintf(s, Fmt, Args);
  fprintf(f, s);
end;

procedure fprintf(const f: TFile; const str: string);
begin
  if f <> nil then
    fprintf(f.f, str);
end;

procedure fprintf(const f: TFile; const Fmt: string; const Args: array of const);
begin
  if f <> nil then
    fprintf(f.f, Fmt, Args);
end;

function itoa(i: integer): string;
begin
  sprintf(result, '%d', [i]);
end;

function uitoa(l: longword): string;
begin
  sprintf(result, '%d', [l]);
end;

function ftoa(f: single): string;
begin
  ThousandSeparator := #0;
  DecimalSeparator := '.';

  result := FloatToStr(f);
end;

function ftoafmt(const fmt: string; f: single): string;
begin
  ThousandSeparator := #0;
  DecimalSeparator := '.';

  sprintf(result, '%' + fmt + 'f', [f]);
end;

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
    else if Pos('#', s) = 1 then
      val(Copy(s, 2, Length(s) - 1), ret2, code);
    if code = 0 then
      result := ret2
    else
      result := 0;
  end;
end;

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
    else if Pos('#', s) = 1 then
      val(Copy(s, 2, Length(s) - 1), ret2, code);
    if code = 0 then
      result := ret2
    else
      result := default;
  end;
end;

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
    else if Pos('#', s) = 1 then
      val(Copy(s, 2, Length(s) - 1), ret2, code);
    if code = 0 then
      result := ret2
    else
      result := 0;
  end;
end;

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
    else if Pos('#', s) = 1 then
      val(Copy(s, 2, Length(s) - 1), ret2, code);
    if code = 0 then
      result := ret2
    else
      result := default;
  end;
end;

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

function memmove(const destination, source: pointer; count: integer): pointer;
begin
  Move(source^, destination^, count);
  result := destination;
end;

procedure memcpy(const dest0: pointer; const src0: pointer; count0: integer);
begin
  Move(src0^, dest0^, count0);
end;

{$IFNDEF FPC}
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

function mallocA(var Size: integer; const Align: integer; var original: pointer): pointer;
begin
  Size := Size + Align;
  result := malloc(Size);
  original := result;
  if result <> nil then
    result := pointer(integer(result) and (1 - Align) + Align);
end;

function mallocz(const size: integer): Pointer;
begin
  result := malloc(size);
  if result <> nil then
    ZeroMemory(result, size);
end;

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

procedure memfree(var p: pointer; const size: integer);
begin
  if p <> nil then
  begin
    FreeMem(p, size);
    p := nil;
    memoryusage := memoryusage - size;
  end;
end;

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

function intval(const b: boolean): integer;
begin
  if b then
    result := 1
  else
    result := 0;
end;

function decide(const condition: boolean;
  const iftrue: integer; const iffalse: integer): integer;
begin
  if condition then
    result := iftrue
  else
    result := iffalse;
end;

function decide(const condition: boolean;
  const iftrue: boolean; const iffalse: boolean): boolean;
begin
  if condition then
    result := iftrue
  else
    result := iffalse;
end;

function decide(const condition: boolean;
  const iftrue: string; const iffalse: string): string;
begin
  if condition then
    result := iftrue
  else
    result := iffalse;
end;

function decide(const condition: boolean;
  const iftrue: pointer; const iffalse: pointer): pointer;
begin
  if condition then
    result := iftrue
  else
    result := iffalse;
end;

function decide(const condition: integer;
  const iftrue: integer; const iffalse: integer): integer;
begin
  if condition <> 0 then
    result := iftrue
  else
    result := iffalse;
end;

function decide(const condition: integer;
  const iftrue: boolean; const iffalse: boolean): boolean;
begin
  if condition <> 0 then
    result := iftrue
  else
    result := iffalse;
end;

function decide(const condition: integer;
  const iftrue: string; const iffalse: string): string;
begin
  if condition <> 0 then
    result := iftrue
  else
    result := iffalse;
end;

function decide(const condition: integer;
  const iftrue: pointer; const iffalse: pointer): pointer;
begin
  if condition <> 0 then
    result := iftrue
  else
    result := iffalse;
end;

function decidef(const condition: boolean;
  const iftrue: single; const iffalse: single): single;
begin
  if condition then
    result := iftrue
  else
    result := iffalse;
end;

function incp(var p: pointer; const size: integer = 1): pointer;
begin
  result := Pointer(integer(p) + size);
  p := result;
end;

function pDiff(const p1, p2: pointer; const size: integer): integer;
begin
  result := (Integer(p1) - Integer(p2)) div size;
end;

////////////////////////////////////////////////////////////////////////////////
// TStream
constructor TDStream.Create;
begin
  FIOResult := 0;
end;

function TDStream.IOResult: integer;
begin
  result := FIOResult;
  FIOResult := 0;
end;

////////////////////////////////////////////////////////////////////////////////
// TMemoryStream
constructor TDMemoryStream.Create;
begin
  Inherited Create;
  FSize := 0;
  FRealSize := 0;
  FPosition := 0;
  FMemory := nil;
end;

destructor TDMemoryStream.Destroy;
begin
  Resize(0);
  Inherited Destroy;
end;

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

function TDMemoryStream.Read(var Buffer; Count: Longint): Longint;
begin
  if Count + FPosition > FSize then
    result := FSize - FPosition
  else
    result := Count;

  memcpy(@Buffer, pointer(integer(FMemory) + FPosition), result);
  FPosition := FPosition + result;
end;

function TDMemoryStream.Write(const Buffer; Count: Longint): Longint;
begin
  if Count + FPosition > FSize then
    resize(Count + FPosition);
  memcpy(pointer(integer(FMemory) + FPosition), @Buffer, Count);
  FPosition := FPosition + Count;
  result := Count;
end;

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

function TDMemoryStream.Size: Longint;
begin
  result := FSize;
end;

function TDMemoryStream.Position: integer;
begin
  result := FPosition;
end;

////////////////////////////////////////////////////////////////////////////////
// TAttachableMemoryStream
constructor TAttachableMemoryStream.Create;
begin
  Inherited Create;
  FSize := 0;
  FPosition := 0;
  FMemory := nil;
end;

destructor TAttachableMemoryStream.Destroy;
begin
  Inherited Destroy;
end;

procedure TAttachableMemoryStream.Attach(const amemory: pointer; const asize: integer);
begin
  FMemory := amemory;
  FSize := asize;
  FPosition := 0;
end;

function TAttachableMemoryStream.Read(var Buffer; Count: Longint): Longint;
begin
  if Count + FPosition > FSize then
    result := FSize - FPosition
  else
    result := Count;

  memcpy(@Buffer, pointer(integer(FMemory) + FPosition), result);
  FPosition := FPosition + result;
end;

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

function TAttachableMemoryStream.Size: Longint;
begin
  result := FSize;
end;

function TAttachableMemoryStream.Position: integer;
begin
  result := FPosition;
end;

////////////////////////////////////////////////////////////////////////////////
// TFile
// File class
constructor TFile.Create(const FileName: string; const mode: integer);
begin
  Inherited Create;
  OnBeginBusy := nil;
  OnEndBusy := nil;

  fopen(f, FileName, mode);
end;

destructor TFile.Destroy;
begin
  close(f);
  Inherited;
end;

function TFile.Read(var Buffer; Count: Longint): Longint;
begin
  if Assigned(OnBeginBusy) then OnBeginBusy;

  {$I-}
  BlockRead(f, Buffer, Count, result);
  {$I+}
  FIOResult := IOResult;

  if Assigned(OnEndBusy) then OnEndBusy;
end;

function TFile.Write(const Buffer; Count: Longint): Longint;
begin
  if Assigned(OnBeginBusy) then OnBeginBusy;

  {$I-}
  BlockWrite(f, Buffer, Count, result);
  {$I+}
  FIOResult := IOResult;

  if Assigned(OnEndBusy) then OnEndBusy;
end;

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

function TFile.Size: Longint;
begin
  {$I-}
  result := FileSize(f);
  {$I+}
  FIOResult := IOResult;
end;

function TFile.Position: integer;
begin
  {$I-}
  result := FilePos(f);
  {$I+}
  FIOResult := IOResult;
end;

////////////////////////////////////////////////////////////////////////////////
// TCachedFile
// Cache read file class
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

procedure TCachedFile.ResetBuffer;
begin
  fBufferStart := -1;
  fBufferEnd := -1;
end;

destructor TCachedFile.Destroy;
begin
  memfree(fBuffer, fBufSize);
  Inherited;
end;

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

function TCachedFile.Write(const Buffer; Count: Longint): Longint;
begin
  fPosition := Inherited Seek(fPosition, sFromBeginning);
  result := Inherited Write(Buffer, Count);
  fPosition := fPosition + result;
  if fSize < fPosition then
    fSize := fPosition;
end;

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

procedure TCachedFile.SetSize(NewSize: Longint);
begin
  Inherited;
  fSize := NewSize;
end;

function TCachedFile.Position: integer;
begin
  result := FPosition;
end;

////////////////////////////////////////////////////////////////////////////////
// TDByteList
constructor TDByteList.Create;
begin
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

destructor TDByteList.Destroy;
begin
  Clear;
end;

function TDByteList.Get(Index: Integer): byte;
begin
  if (Index < 0) or (Index >= fNumItems) then
    result := 0
  else
    result := fList[Index];
end;

procedure TDByteList.Put(Index: Integer; const value: byte);
begin
  fList[Index] := value;
end;

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

procedure TDByteList.Clear;
begin
  realloc(pointer(fList), fRealNumItems * SizeOf(byte), 0);
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

procedure TDByteList.FastClear;
begin
  fNumItems := 0;
end;

////////////////////////////////////////////////////////////////////////////////
// TDNumberList
constructor TDNumberList.Create;
begin
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

destructor TDNumberList.Destroy;
begin
  Clear;
end;

function TDNumberList.Get(Index: Integer): integer;
begin
  if (Index < 0) or (Index >= fNumItems) then
    result := 0
  else
    result := fList[Index];
end;

procedure TDNumberList.Put(Index: Integer; const value: integer);
begin
  fList[Index] := value;
end;

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

procedure TDNumberList.Add(const nlist: TDNumberList);
var
  i: integer;
begin
  for i := 0 to nlist.Count - 1 do
    Add(nlist[i]);
end;

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

procedure TDNumberList.Clear;
begin
  realloc(pointer(fList), fRealNumItems * SizeOf(integer), 0);
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

procedure TDNumberList.FastClear;
begin
  fNumItems := 0;
end;

procedure TDNumberList.Sort;
begin
  QSortIntegers(fList, fNumItems);
end;

function TDNumberList.Sum: integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to fNumItems - 1 do
    result := result + fList[i];
end;

////////////////////////////////////////////////////////////////////////////////
// T2DNumberList
constructor T2DNumberList.Create;
begin
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

destructor T2DNumberList.Destroy;
begin
  Clear;
end;

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

procedure T2DNumberList.Put(Index: Integer; const value: twointeger_t);
begin
  fList[Index] := value;
end;

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

function T2DNumberList.Add(const value: twointeger_t): integer;
begin
  result := Add(value.num1, value.num2);
end;

procedure T2DNumberList.Add(const nlist: T2DNumberList);
var
  i: integer;
begin
  for i := 0 to nlist.Count - 1 do
    Add(nlist[i]);
end;

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

procedure T2DNumberList.Clear;
begin
  realloc(pointer(fList), fRealNumItems * SizeOf(twointeger_t), 0);
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

procedure T2DNumberList.FastClear;
begin
  fNumItems := 0;
end;

////////////////////////////////////////////////////////////////////////////////
// TDLimitNumberList
constructor TDLimitNumberList.Create;
begin
  inherited Create;
  fLimit := MAXINT;
end;

constructor TDLimitNumberList.CreateLimited(const v: Integer);
begin
  inherited Create;
  if v > 0 then // ?
    fLimit := v;
end;

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

////////////////////////////////////////////////////////////////////////////////
// TDFloatList
constructor TDFloatList.Create;
begin
  fList := nil;
  fNumItems := 0;
end;

destructor TDFloatList.Destroy;
begin
  Clear;
end;

function TDFloatList.Get(Index: Integer): float;
begin
  if (Index < 0) or (Index >= fNumItems) then
    result := 0
  else
    result := fList[Index];
end;

procedure TDFloatList.Put(Index: Integer; const value: float);
begin
  fList[Index] := value;
end;

function TDFloatList.Add(const value: float): integer;
begin
  realloc(pointer(fList), fNumItems * SizeOf(float), (fNumItems + 1) * SizeOf(float));
  Put(fNumItems, value);
  result := fNumItems;
  inc(fNumItems);
end;

procedure TDFloatList.Add(const nlist: TDFloatList);
var
  i: integer;
begin
  for i := 0 to nlist.Count - 1 do
    Add(nlist[i]);
end;

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

procedure TDFloatList.Clear;
begin
  realloc(pointer(fList), fNumItems * SizeOf(float), 0);
  fList := nil;
  fNumItems := 0;
end;

procedure TDFloatList.Sort;
begin
  QSortFloats(fList, fNumItems);
end;

function TDFloatList.Sum: float;
var
  i: integer;
begin
  result := 0.0;
  for i := 0 to fNumItems - 1 do
    result := result + fList[i];
end;

////////////////////////////////////////////////////////////////////////////////
// TDPointerList
constructor TDPointerList.Create;
begin
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

destructor TDPointerList.Destroy;
begin
  Clear;
end;

function TDPointerList.Get(Index: Integer): pointer;
begin
  if (Index < 0) or (Index >= fNumItems) then
    result := nil
  else
    result := fList[Index];
end;

procedure TDPointerList.Put(Index: Integer; const value: pointer);
begin
  fList[Index] := value;
end;

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

procedure TDPointerList.Add(const nlist: TDPointerList);
var
  i: integer;
begin
  for i := 0 to nlist.Count - 1 do
    Add(nlist[i]);
end;

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

procedure TDPointerList.Clear;
begin
  realloc(pointer(fList), fRealNumItems * SizeOf(pointer), 0);
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

procedure TDPointerList.FastClear;
begin
  fNumItems := 0;
end;

////////////////////////////////////////////////////////////////////////////////
// TDHashNumberList
constructor TDHashNumberList.Create;
begin
  Inherited Create;
  memset(@fhash, SizeOf(fhash), 0);
  fsorted := True;
end;

procedure TDHashNumberList.Put(Index: Integer; const value: integer);
begin
  Inherited Put(Index, value);
  if fsorted then
    if fnumitems > 1 then
      fsorted := fList[fNumItems - 1] >= fList[fNumItems - 2];
  fhash[value and (NLHASHSIZE - 1)] := Index;
end;

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

procedure TDHashNumberList.Sort;
begin
  if not fsorted then
  begin
    Inherited Sort;
    fsorted := True;
    CreateHashTable;
  end;
end;

procedure TDHashNumberList.CreateHashTable;
var
  i: integer;
begin
  for i := 0 to fNumItems - 1 do
    fhash[fList[i] and (NLHASHSIZE - 1)] := i;
end;

////////////////////////////////////////////////////////////////////////////////
// TDTextList
constructor TDTextList.Create;
begin
  fList := nil;
  fNumItems := 0;
end;

destructor TDTextList.Destroy;
begin
  Clear;
end;

function TDTextList.Get(Index: Integer): string;
begin
  if (Index < 0) or (Index >= fNumItems) then
    result := ''
  else
    result := fList[Index];
end;

procedure TDTextList.Put(Index: Integer; const value: string);
begin
  fList[Index] := value;
end;

procedure TDTextList.Add(const value: string);
begin
  realloc(pointer(fList), fNumItems * 256, (fNumItems + 1) * 256);
  Put(fNumItems, value);
  inc(fNumItems);
end;

procedure TDTextList.Add(const nlist: TDTextList);
var
  i: integer;
begin
  for i := 0 to nlist.Count - 1 do
    Add(nlist[i]);
end;

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

procedure TDTextList.Clear;
begin
  realloc(pointer(fList), fNumItems * 256, 0);
  fList := nil;
  fNumItems := 0;
end;

////////////////////////////////////////////////////////////////////////////////
// TDStrings
function TDStrings.Add(const S: string): Integer;
begin
  result := GetCount;
  Insert(result, S);
end;

function TDStrings.Add(const Fmt: string; const Args: array of const): integer;
var
  str: string;
begin
  sprintf(str, Fmt, Args);
  result := Add(str);
end;

function TDStrings.AddObject(const S: string; AObject: TObject): Integer;
begin
  result := Add(S);
  PutObject(result, AObject);
end;

procedure TDStrings.Append(const S: string);
begin
  Add(S);
end;

procedure TDStrings.AddStrings(Strings: TDStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    AddObject(Strings[I], Strings.Objects[I]);
end;

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

function TDStrings.GetCapacity: Integer;
begin  // descendants may optionally override/replace this default implementation
  result := Count;
end;

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

function TDStrings.GetObject(Index: Integer): TObject;
begin
  result := nil;
end;

function TDStrings.GetText: PChar;
begin
  result := StrNew(PChar(GetTextStr));
end;

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

function TDStrings.GetValue(const Name: string): string;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    result := Copy(Get(I), Length(Name) + 2, MaxInt) else
    result := '';
end;

function TDStrings.GetValueIdx(const idx: integer): string;
var
  tmp: string;
begin
  splitstring(Get(idx), tmp, result, '=');
end;

function TDStrings.IndexOf(const S: string): Integer;
begin
  for result := 0 to GetCount - 1 do
    if AnsiCompareText(Get(result), S) = 0 then
      Exit;
  result := -1;
end;

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

function TDStrings.IndexOfObject(AObject: TObject): Integer;
begin
  for result := 0 to GetCount - 1 do
    if GetObject(result) = AObject then
      Exit;
  result := -1;
end;

procedure TDStrings.InsertObject(Index: Integer; const S: string;
  AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

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

procedure TDStrings.Put(Index: Integer; const S: string);
var
  TempObject: TObject;
begin
  TempObject := GetObject(Index);
  Delete(Index);
  InsertObject(Index, S, TempObject);
end;

procedure TDStrings.PutObject(Index: Integer; AObject: TObject);
begin
end;

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

procedure TDStrings.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendants may optionally implement this method
end;

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

procedure TDStrings.SetText(Text: PChar);
begin
  SetTextStr(Text);
end;

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

////////////////////////////////////////////////////////////////////////////////
// TStringList
destructor TDStringList.Destroy;
begin
  inherited Destroy;
  if FCount <> 0 then
    Finalize(FList[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

function TDStringList.Add(const S: string): Integer;
begin
  result := FCount;
  InsertItem(result, S);
end;

procedure TDStringList.Clear;
begin
  if FCount <> 0 then
  begin
    Finalize(FList[0], FCount);
    FCount := 0;
    SetCapacity(0);
  end;
end;

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

procedure TDStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    exit;
  if (Index2 < 0) or (Index2 >= FCount) then
    exit;
  ExchangeItems(Index1, Index2);
end;

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

function TDStringList.Get(Index: Integer): string;
begin
  if (Index >= 0) and (Index < FCount) then
    result := FList[Index].FString
  else
    result := '';
end;

function TDStringList.GetCapacity: Integer;
begin
  result := FCapacity;
end;

function TDStringList.GetCount: Integer;
begin
  result := FCount;
end;

function TDStringList.GetObject(Index: Integer): TObject;
begin
  if (Index >= 0) and (Index < FCount) then
    result := FList[Index].FObject
  else
    result := nil;
end;

procedure TDStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

procedure TDStringList.Insert(Index: Integer; const S: string);
begin
  if (Index >= 0) and (Index <= FCount) then
    InsertItem(Index, S);
end;

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

procedure TDStringList.Put(Index: Integer; const S: string);
begin
  if (Index > 0) and (Index < FCount) then
    FList[Index].FString := S;
end;

procedure TDStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index >= 0) and (Index < FCount) then
    FList[Index].FObject := AObject;
end;

procedure TDStringList.SetCapacity(NewCapacity: Integer);
begin
  realloc(pointer(FList), FCapacity * SizeOf(TStringItem), NewCapacity * SizeOf(TStringItem));
  FCapacity := NewCapacity;
end;

////////////////////////////////////////////////////////////////////////////////

function getenv(const env: string): string;
var
  buf: array[0..2047] of char;
begin
  ZeroMemory(@buf, SizeOf(buf));
  GetEnvironmentVariable(PChar(env), buf, 2047);
  result := Trim(StringVal(buf));
end;

function fexists(const filename: string): boolean;
begin
  result := FileExists(filename);
end;

function direxists(const dirname: string): boolean;
begin
  result := DirectoryExists(dirname);
end;

function fexpand(const filename: string): string;
begin
  result := ExpandFileName(filename);
end;

function fpath(const filename: string): string;
begin
  result := ExtractFilePath(filename);
end;

procedure fdelete(const filename: string);
begin
  if fexists(filename) then
    DeleteFile(filename);
end;

procedure frename(const src, dest: string);
begin
  if fexists(src) then
  begin
    fdelete(dest);
    RenameFile(src, dest);
  end;
end;

function fext(const filename: string): string;
begin
  result := ExtractFileExt(filename);
end;

function fname(const filename: string): string;
begin
  result := ExtractFileName(filename);
end;

function fmask(const mask: string): string;
begin
  result := mask;
  if result = '' then
    result := '*.*';
end;

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

function strremovespaces(const s: string): string;
var
  i: integer;
begin
  result := '';
  for i := 1 to Length(s) do
    if s[i] <> ' ' then
      result := result + s[i];
end;

function _SHL(const x: integer; const bits: integer): integer; assembler;
asm
  mov ecx, edx
  sal eax, cl
end;

function _SHLW(const x: LongWord; const bits: LongWord): LongWord;
begin
  result := x shl bits;
end;

function _SHR(const x: integer; const bits: integer): integer; assembler;
asm
  mov ecx, edx
  sar eax, cl
end;

function _SHR1(const x: integer): integer; assembler;
asm
  sar eax, 1
end;

function _SHR2(const x: integer): integer; assembler;
asm
  sar eax, 2
end;

function _SHR3(const x: integer): integer; assembler;
asm
  sar eax, 3
end;

function _SHR4(const x: integer): integer; assembler;
asm
  sar eax, 4
end;

function _SHR7(const x: integer): integer; assembler;
asm
  sar eax, 7
end;

function _SHR8(const x: integer): integer; assembler;
asm
  sar eax, 8
end;

function _SHR14(const x: integer): integer; assembler;
asm
  sar eax, 14
end;

function _SHRW(const x: LongWord; const bits: LongWord): LongWord;
begin
  result := x shr bits;
end;

function StringVal(const Str: PChar): string;
begin
  sprintf(result, '%s', [Str]);
end;

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

function fixslashpath(const apath: string): string;
var
  i: integer;
begin
  result := apath;
  for i := 1 to length(result) do
    if result[i] = '/' then
      result[i] := '\';
end;

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

function firstword(const inp: string; const splitter: string = ' '): string;
var
  tmp: string;
begin
  splitstring(inp, result, tmp, splitter);
end;

function firstword(const inp: string; const splitters: charset_t): string;
var
  tmp: string;
begin
  splitstring(inp, result, tmp, splitters);
end;

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

  splitstring(st, result, tmp, ' ');
end;

function secondword(const inp: string; const splitter: string = ' '): string;
var
  tmp: string;
begin
  splitstring(inp, tmp, result, splitter);
end;

function secondword(const inp: string; const splitters: charset_t): string;
var
  tmp: string;
begin
  splitstring(inp, tmp, result, splitters);
end;

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

procedure FreeAndNil(var Obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;

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

function fabs(const f: float): float;
begin
  if f >= 0 then
    result := f
  else
    result := -f;
end;

procedure MakeDir(const dir: string);
begin
  CreateDir(dir);
end;

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

function IsIntegerInRange(const test, f1, f2: integer): boolean;
begin
  if f1 < f2 then
    result := (test >= f1) and (test <= f2)
  else
    result := (test >= f2) and (test <= f1)
end;

function IsLongWordInRange(const test, f1, f2: LongWord): boolean;
begin
  if f1 < f2 then
    result := (test >= f1) and (test <= f2)
  else
    result := (test >= f2) and (test <= f1)
end;

function IsFloatInRange(const test, f1, f2: float): boolean;
begin
  if f1 < f2 then
    result := (test >= f1) and (test <= f2)
  else
    result := (test >= f2) and (test <= f1)
end;

function IsDoubleInRange(const test, f1, f2: double): boolean;
begin
  if f1 < f2 then
    result := (test >= f1) and (test <= f2)
  else
    result := (test >= f2) and (test <= f1)
end;

function IsExtendedInRange(const test, f1, f2: Extended): boolean;
begin
  if f1 < f2 then
    result := (test >= f1) and (test <= f2)
  else
    result := (test >= f2) and (test <= f1)
end;

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

constructor TString.Create(const astring: string);
begin
  str := astring;
end;

constructor TInteger.Create(const aint: integer);
begin
  intnum := aint;
end;

constructor TFloat.Create(const aflt: float);
begin
  floatnum := aflt;
end;

function MinI(const a, b: integer): integer;
begin
  if a > b then
    result := b
  else
    result := a;
end;

function MaxI(const a, b: integer): integer;
begin
  if a > b then
    result := a
  else
    result := b;
end;

function NowTime: TDateTime;
begin
  result := Now;
end;

function formatDateTimeAsString(const Format: string; DateTime: TDateTime): string;
begin
  DateTimeToString(Result, Format, DateTime);
end;

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

function StrIsInteger(const s: string): Boolean;
var
  check: string;
begin
  if Pos('-', s) = 1 then
  begin
    check := s;
    delete(check, 1, 1);
    result := StrIsLongWord(check)
  end
  else
    result := StrIsLongWord(s);
end;

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

function GetIntegerInRange(const val, f1, f2: integer): integer;
begin
  if val < f1 then
    result := f1
  else if val > f2 then
    result := f2
  else
    result := val;
end;

function GetInt64InRange(const val, f1, f2: int64): integer;
begin
  if val < f1 then
    result := f1
  else if val > f2 then
    result := f2
  else
    result := val;
end;

function ibetween(const x: integer; const x1, x2: integer): integer;
begin
  if x <= x1 then
    result := x1
  else if x >= x2 then
    result := x2
  else
    result := x;
end;

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
      stmp := strtrim(stmp);
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

function isdigit(const c: char): boolean;
begin
  result := c in ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'];
end;

function Isign(const x: integer): integer;
begin
  if x < 0 then
    result := -1
  else if x > 0 then
    result := 1
  else
    result := 0;
end;

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

end.

