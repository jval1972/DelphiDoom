unit ps_utils;
{$I PascalScript.inc}

interface

uses
  Classes, SysUtils {$IFDEF VER130}, Windows {$ENDIF}, ps_defs;

const
  PSMainProcName = '!MAIN';
  PSMainProcNameOrg = 'Main Proc';
  PSLowBuildSupport = 12;
  PSCurrentBuildNo = 23;
  PSCurrentversion = '1.31';
  PSValidHeader = 1397769801;
  PSAddrStackStart = 1610612736;
  PSAddrNegativeStackStart = 1073741824;

type
  TPSBaseType = Byte;
  TPSVariableType = (ivtGlobal, ivtParam, ivtVariable);

const
  btReturnAddress   = 0;
  btU8              = 1;
  btS8              = 2;
  btU16             = 3;
  btS16             = 4;
  btU32             = 5;
  btS32             = 6;
  btSingle          = 7;
  btDouble          = 8;
  btExtended        = 9;
  btString          = 10;
  btRecord          = 11;
  btArray           = 12;
  btPointer         = 13;
  btPChar           = 14;
  btResourcePointer = 15;
  btVariant         = 16;
  {$IFNDEF PS_NOINT64}
  btS64             = 17;
  {$ENDIF}
  btChar            = 18;
  {$IFNDEF PS_NOWIDESTRING}
  btWideString      = 19;
  btWideChar        = 20;
  {$ENDIF}
  btProcPtr         = 21;
  btStaticArray     = 22;
  btSet             = 23;
  btCurrency        = 24;
  btClass           = 25;
  btInterface       = 26;
  btNotificationVariant = 27;
  btUnicodeString = 28;
  btEnum = 129;
  btType = 130;
  btExtClass = 131;

function MakeHash(const s:  TbtString): Longint;

const
{ Script internal command: Assign command<br>
    Command: TPSCommand;<br>
    VarDest, // no data<br>
    VarSrc: TPSVariable;<br>
}
  CM_A = 0;
{ Script internal command: Calculate Command<br>
    Command: TPSCommand; <br>
    CalcType: Byte;<br>
    <i><br>
      0 = +<br>
      1 = -<br>
      2 = *<br>
      3 = /<br>
      4 = MOD<br>
      5 = SHL<br>
      6 = SHR<br>
      7 = AND<br>
      8 = OR<br>
      9 = XOR<br>
    </i><br>
    VarDest, // no data<br>
    VarSrc: TPSVariable;<br>
<br>
}
  CM_CA = 1;
{ Script internal command: Push<br>
    Command: TPSCommand; <br>
    Var: TPSVariable;<br>
}
  CM_P = 2;
{ Script internal command: Push Var<br>
    Command: TPSCommand; <br>
    Var: TPSVariable;<br>
}
  CM_PV = 3;
{ Script internal command: Pop<br>
    Command: TPSCommand; <br>
}
  CM_PO = 4;
{ Script internal command: Call<br>
    Command: TPSCommand; <br>
    ProcNo: Longword;<br>
}
  CM_C = 5;
{ Script internal command: Goto<br>
    Command: TPSCommand; <br>
    NewPosition: Longint; //relative to end of this instruction<br>
}
  CM_G = 6;
{ Script internal command: Conditional Goto<br>
    Command: TPSCommand; <br>
    NewPosition: LongWord; //relative to end of this instruction<br>
    Var: TPSVariable; // no data<br>
}
  CM_CG = 7;
{ Script internal command: Conditional NOT Goto<br>
    Command: TPSCommand; <br>
    NewPosition: LongWord; // relative to end of this instruction<br>
    Var: TPSVariable; // no data<br>
}
  CM_CNG = 8;
{ Script internal command: Ret<br>
    Command: TPSCommand; <br>
}
  CM_R = 9;
{ Script internal command: Set Stack Type<br>
    Command: TPSCommand; <br>
    NewType: LongWord;<br>
    OffsetFromBase: LongWord;<br>
}
  CM_ST = 10;
{ Script internal command: Push Type<br>
    Command: TPSCommand; <br>
    FType: LongWord;<br>
}
  CM_PT = 11;
{ Script internal command: Compare<br>
    Command: TPSCommand; <br>
    CompareType: Byte;<br>
    <i><br>
     0 = &gt;=<br>
     1 = &lt;=<br>
     2 = &gt;<br>
     3 = &lt;<br>
     4 = &lt;&gt<br>
     5 = =<br>
    <i><br>
    IntoVar: TPSAssignment;<br>
    Compare1, Compare2: TPSAssigment;<br>
}
  CM_CO = 12;
{ Script internal command: Call Var<br>
    Command: TPSCommand; <br>
    Var: TPSVariable;<br>
}
  CM_CV = 13;
{ Script internal command: Set Pointer<br>
    Command: TPSCommand; <br>
    VarDest: TPSVariable;<br>
    VarSrc: TPSVariable;<br>
}
  CM_SP = 14;
{ Script internal command: Boolean NOT<br>
    Command: TPSCommand; <br>
    Var: TPSVariable;<br>
}
  CM_BN = 15;
{ Script internal command: Var Minus<br>
    Command: TPSCommand; <br>
    Var: TPSVariable;
}
  CM_VM = 16;
{ Script internal command: Set Flag<br>
    Command: TPSCommand; <br>
    Var: TPSVariable;<br>
    DoNot: Boolean;<br>
}
  CM_SF = 17;
{ Script internal command: Flag Goto<br>
    Command: TPSCommand; <br>
    Where: Cardinal;<br>
}
  CM_FG = 18;
{ Script internal command: Push Exception Handler<br>
    Command: TPSCommand; <br>
    FinallyOffset,<br>
    ExceptionOffset, // FinallyOffset or ExceptionOffset need to be set.<br>
    Finally2Offset,<br>
    EndOfBlock: Cardinal;<br>
}
  CM_PUEXH = 19;
{ Script internal command: Pop Exception Handler<br>
    Command:TPSCommand; <br>
    Position: Byte;<br>
    <i> 0 = end of try/finally/exception block;<br>
      1 = end of first finally<br>
      2 = end of except<br>
      3 = end of second finally<br>
    </i><br>
}
  CM_POEXH = 20;
{ Script internal command: Integer NOT<br>
    Command: TPSCommand; <br>
    Where: Cardinal;<br>
}
  CM_IN = 21;
  {Script internal command: Set Stack Pointer To Copy<br>
      Command: TPSCommand; <br>
    Where: Cardinal;<br>
}
  CM_SPC = 22;
  {Script internal command: Inc<br>
    Command: TPSCommand; <br>
    Var: TPSVariable;<br>
  }
  CM_INC = 23;
  {Script internal command: Dec<br>
      Command: TPSCommand; <br>
    Var: TPSVariable;<br>
  }
  CM_DEC = 24;
  {Script internal command: nop<br>
      Command: TPSCommand; <br>}
  CM_NOP = 255;
{ Script internal command: Pop and Goto<br>
    Command: TPSCommand; <br>
    NewPosition: Longint; //relative to end of this instruction<br>
}
  CM_PG = 25;
{ Script internal command: Pop*2 and Goto<br>
    Command: TPSCommand; <br>
    NewPosition: Longint; //relative to end of this instruction<br>
}
  CM_P2G = 26;


type
  TbtU8 = Byte;
  TbtS8 = ShortInt;
  TbtU16 = Word;
  TbtS16 = SmallInt;
  TbtU32 = Cardinal;
  TbtS32 = Longint;
  TbtSingle = Single;
  TbtDouble = double;
  TbtExtended = Extended;
  TbtCurrency = Currency;
  {$IFNDEF PS_NOINT64}
  TbtS64 = int64;
  {$ENDIF}
  TbtChar = {$IFDEF DELPHI4UP}AnsiChar{$ELSE}Char{$ENDIF};
  {$IFNDEF PS_NOWIDESTRING}
  TbtWideString = widestring;
  TbtUnicodeString = {$IFDEF DELPHI2009UP}UnicodeString{$ELSE}widestring{$ENDIF};
  TbtWideChar = widechar;
  tbtNativeString = {$IFDEF DELPHI2009UP}TbtUnicodeString{$ELSE}TbtString{$ENDIF};
  {$ENDIF}
  {$IFDEF FPC}
  IPointer = PtrUInt;
  {$ELSE}
  {$IFDEF CPUX64}
  IPointer = IntPtr;
  {$ELSE}
  {$IFDEF CPU64}
  IPointer = LongWord;
  {$ELSE}
  IPointer = Cardinal;
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  TPSCallingConvention = (cdRegister, cdPascal, cdCdecl, cdStdCall, cdSafeCall);

const
  PointerSize = IPointer({$IFDEF CPU64}8{$ELSE}4{$ENDIF});
  PointerSize2 = IPointer(2 * PointerSize);
  MaxListSize = Maxint div 16;

type
  PPointerList = ^TPointerList;

  TPointerList = array[0..MaxListSize - 1] of Pointer;

  TPSList = class(TObject)
  protected
    FData: PPointerList;
    FCapacity: Cardinal;
    FCount: Cardinal;
    FCheckCount: Cardinal;
  private
    function GetItem(Nr: Cardinal): Pointer;
    procedure SetItem(Nr: Cardinal; P: Pointer);
  public
    {$IFNDEF PS_NOSMARTLIST}
    procedure Recreate;
    {$ENDIF}
    property Data: PPointerList read FData;
    constructor Create;
    function IndexOf(P: Pointer): Longint;
    destructor Destroy; override;
    property Count: Cardinal read FCount;
    property Items[nr: Cardinal]: Pointer read GetItem write SetItem; default;
    function Add(P: Pointer): Longint;
    procedure AddBlock(List: PPointerList; Count: Longint);
    procedure Remove(P: Pointer);
    procedure Delete(Nr: Cardinal);
    procedure DeleteLast;
    procedure Clear; virtual;
  end;
  TIFList = TPSList;

  TPSStringList = class(TObject)
  private
    List: TPSList;
    function GetItem(Nr: LongInt): TbtString;
    procedure SetItem(Nr: LongInt; const s: TbtString);
  public
    function Count: LongInt;
    property Items[Nr: Longint]: TbtString read GetItem write SetItem; default;
    procedure Add(const P: TbtString);
    procedure Delete(NR: LongInt);
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;
  TIFStringList = TPSStringList;

  TPSUnitList = class;

  TPSUnit = class(TObject)
  private
    fList     : TPSUnitList;
    fUnits    : TPSList;
    fUnitName : TbtString;
    procedure SetUnitName(const Value: TbtString);
  public
    constructor Create(List: TPSUnitList);
    destructor Destroy; override;
    procedure AddUses(pUnitName: TbtString);
    function HasUses(pUnitName: TbtString): Boolean;
    {$WARNINGS OFF}
    property UnitName: TbtString read fUnitName write SetUnitName;
    {$WARNINGS ON}
  end;

  TPSUnitList = class
  private
    fList: TPSList;
    function Add: TPSUnit;
  public
    constructor Create;
    function GetUnit(UnitName: TbtString): TPSUnit;
    destructor Destroy; override;
  end;

type
  TPSParserErrorKind = (
    iNoError,
    iCommentError,
    iStringError,
    iCharError,
    iSyntaxError
  );

  TPSParserErrorEvent = procedure (Parser: TObject; Kind: TPSParserErrorKind) of object;

  TPSPascalParser = class(TObject)
  private
    fkeywords: PRTabArray;
    fnumkeywords: integer;
  protected
    FData: TbtString;
    FText: {$IFDEF DELPHI4UP}PAnsiChar{$ELSE}PChar{$ENDIF};
    FLastEnterPos, FRow, FRealPosition, FTokenLength: Cardinal;
    FTokenId: TPSPasToken;
    FToken: TbtString;
    FOriginalToken: TbtString;
    FParserError: TPSParserErrorEvent;
    FEnableComments: Boolean;
    FEnableWhitespaces: Boolean;
    function GetCol: Cardinal;
    // only applicable when Token in [CSTI_Identifier, CSTI_Integer, CSTI_Real, CSTI_String, CSTI_Char, CSTI_HexInt]
  public
    constructor Create(const akeywords: PRTabArray; const anumkeywords: integer); virtual;
    property EnableComments: Boolean read FEnableComments write FEnableComments;
    property EnableWhitespaces: Boolean read FEnableWhitespaces write FEnableWhitespaces;
    procedure Next; virtual;
    property GetToken: TbtString read FToken;
    property OriginalToken: TbtString read FOriginalToken;
    property CurrTokenPos: Cardinal read FRealPosition;
    property CurrTokenID: TPSPasToken read FTokenId;
    property Row: Cardinal read FRow;
    property Col: Cardinal read GetCol;
    procedure SetText(const Data: TbtString); virtual;
    property OnParserError: TPSParserErrorEvent read FParserError write FParserError;
  end;

function FloatToStr(E: Extended): TbtString;

function FastLowerCase(const s: TbtString): TbtString;

function Fw(const S: TbtString): TbtString;

function IntToStr(I: {$IFNDEF PS_NOINT64}Int64{$ELSE}LongInt{$ENDIF}): TbtString;

function StrToIntDef(const S: TbtString; Def: {$IFNDEF PS_NOINT64}Int64{$ELSE}LongInt{$ENDIF}): {$IFNDEF PS_NOINT64}Int64{$ELSE}LongInt{$ENDIF};

function StrToInt(const S: TbtString): {$IFNDEF PS_NOINT64}Int64{$ELSE}LongInt{$ENDIF};
function StrToFloat(const s: TbtString): Extended;

function FastUpperCase(const s: TbtString): TbtString;

function GRFW(var s: TbtString): TbtString;
function GRLW(var s: TbtString): TbtString;

const
  FCapacityInc = 32;
  {$IFNDEF PS_NOSMARTLIST}
  FMaxCheckCount = (FCapacityInc div 4) * 64;
  {$ENDIF}

{$IFDEF VER130}
function WideUpperCase(const S: WideString): WideString;
function WideLowerCase(const S: WideString): WideString;
{$ENDIF}

implementation

{$IFDEF DELPHI3UP }
resourceString
{$ELSE }
const
{$ENDIF }
  RPS_InvalidFloat = 'Invalid float';

{$IFDEF VER130}
function WideUpperCase(const S: WideString): WideString;
var
  Len: Integer;
begin
  // CharUpperBuffW is stubbed out on Win9x platofmrs
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    Len := Length(S);
    SetString(Result, PWideChar(S), Len);
    if Len > 0 then CharUpperBuffW(Pointer(Result), Len);
  end
  else
    Result := AnsiUpperCase(S);
end;

function WideLowerCase(const S: WideString): WideString;
var
  Len: Integer;
begin
  // CharLowerBuffW is stubbed out on Win9x platofmrs
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    Len := Length(S);
    SetString(Result, PWideChar(S), Len);
    if Len > 0 then CharLowerBuffW(Pointer(Result), Len);
  end
  else
    Result := AnsiLowerCase(S);
end;
{$ENDIF}

function MakeHash(const s: TbtString): Longint;
{small hash maker}
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(s) do
    Result := ((Result shl 7) or (Result shr 25)) + Ord(s[I]);
end;

function GRFW(var s: TbtString): TbtString;
var
  l: Longint;
begin
  l := 1;
  while l <= Length(s) do
  begin
    if s[l] = ' ' then
    begin
      Result := Copy(s, 1, l - 1);
      Delete(s, 1, l);
      Exit;
    end;
    l := l + 1;
  end;
  Result := s;
  s := '';
end;

function GRLW(var s: TbtString): TbtString;
var
  l: Longint;
begin
  l := Length(s);
  while l >= 1 do
  begin
    if s[l] = ' ' then
    begin
      Result := Copy(s, l + 1, MaxInt);
      Delete(s, l, MaxInt);
      Exit;
    end;
    Dec(l);
  end;
  Result := s;
  s := '';
end;

function StrToFloat(const s: TbtString): Extended;
var
  i: longint;
  str: TbtString;
begin
  ThousandSeparator := #0;
  DecimalSeparator := '.';

  Val(string(s), Result, i);
  if i <> 0 then
  begin
    str := s;
    for i := 1 to Length(str) do
      if str[i] in ['.', ','] then
        str[i] := DecimalSeparator;
    Val(string(s), Result, i);
    if i <> 0 then
      raise Exception.Create(RPS_InvalidFloat);
  end;
end;
//-------------------------------------------------------------------

function IntToStr(I: {$IFNDEF PS_NOINT64}Int64{$ELSE}LongInt{$ENDIF}): TbtString;
var
  s: TbtString;
begin
  ThousandSeparator := #0;
  DecimalSeparator := '.';

  Str(i, s);
  IntToStr := s;
end;
//-------------------------------------------------------------------

function FloatToStr(E: Extended): TbtString;
var
  s: TbtString;
begin
  ThousandSeparator := #0;
  DecimalSeparator := '.';

  Str(e:0:12, s);
  Result := s;
end;

function StrToInt(const S: TbtString): {$IFNDEF PS_NOINT64}Int64{$ELSE}LongInt{$ENDIF};
var
  e: Integer;
  Res: {$IFNDEF PS_NOINT64}Int64{$ELSE}LongInt{$ENDIF};
begin
  ThousandSeparator := #0;
  DecimalSeparator := '.';

  Val(string(S), Res, e);
  if e <> 0 then
    StrToInt := -1
  else
    StrToInt := Res;
end;
//-------------------------------------------------------------------

function StrToIntDef(const S: TbtString; Def: {$IFNDEF PS_NOINT64}Int64{$ELSE}LongInt{$ENDIF}): {$IFNDEF PS_NOINT64}Int64{$ELSE}LongInt{$ENDIF};
var
  e: Integer;
  Res: {$IFNDEF PS_NOINT64}Int64{$ELSE}LongInt{$ENDIF};
begin
  ThousandSeparator := #0;
  DecimalSeparator := '.';

  Val(string(S), Res, e);
  if e <> 0 then
    StrToIntDef := Def
  else
    StrToIntDef := Res;
end;
//-------------------------------------------------------------------

constructor TPSList.Create;
begin
  inherited Create;
  FCount := 0;
  FCapacity := 16;
  {$IFNDEF PS_NOSMARTLIST}
  FCheckCount := 0;
  {$ENDIF}
  GetMem(FData, FCapacity * PointerSize);
end;

function MM(i1,i2: Integer): Integer;
begin
  if ((i1 div i2) * i2) < i1 then
    Result := (i1 div i2 + 1) * i2
  else
    Result := (i1 div i2) * i2;
end;

{$IFNDEF PS_NOSMARTLIST}
procedure TPSList.Recreate;
var
  NewData: PPointerList;
  NewCapacity: Cardinal;
  I: Longint;
begin
  FCheckCount := 0;
  NewCapacity := MM(FCount, FCapacityInc);
  if NewCapacity < 64 then
    NewCapacity := 64;
  GetMem(NewData, NewCapacity * PointerSize);
  for I := 0 to Longint(FCount) - 1 do
  begin
    NewData^[i] := FData^[I];
  end;
  FreeMem(FData, FCapacity * PointerSize);
  FData := NewData;
  FCapacity := NewCapacity;
end;
{$ENDIF}

//-------------------------------------------------------------------

function TPSList.Add(P: Pointer): Longint;
begin
  if FCount >= FCapacity then
  begin
    Inc(FCapacity, FCapacityInc);
    ReAllocMem(FData, FCapacity * PointerSize);
  end;
  FData[FCount] := P; // Instead of SetItem
  Result := FCount;
  Inc(FCount);
  {$IFNDEF PS_NOSMARTLIST}
  Inc(FCheckCount);
  if FCheckCount > FMaxCheckCount then
    Recreate;
  {$ENDIF}
end;

procedure TPSList.AddBlock(List: PPointerList; Count: Longint);
var
  L: Longint;
begin
  if Longint(FCount) + Count > Longint(FCapacity) then
  begin
    Inc(FCapacity, MM(Count, FCapacityInc));
    ReAllocMem(FData, FCapacity * PointerSize);
  end;
  for L := 0 to Count - 1 do
  begin
    FData^[FCount] := List^[L];
    Inc(FCount);
  end;
  {$IFNDEF PS_NOSMARTLIST}
  Inc(FCheckCount);
  if FCheckCount > FMaxCheckCount then
    Recreate;
  {$ENDIF}
end;

//-------------------------------------------------------------------
procedure TPSList.DeleteLast;
begin
  if FCount = 0 then
    Exit;
  Dec(FCount);
  {$IFNDEF PS_NOSMARTLIST}
  Inc(FCheckCount);
  if FCheckCount > FMaxCheckCount then
    Recreate;
  {$ENDIF}
end;

procedure TPSList.Delete(Nr: Cardinal);
begin
  if FCount = 0 then
    Exit;
  if Nr < FCount then
  begin
    {dec count first, so we move one element less in the move below}
    Dec(FCount);
    {only move if we aren't deleting the last element}
    if Nr < FCount then
      Move(FData[Nr + 1], FData[Nr], (FCount - Nr) * PointerSize);
    {$IFNDEF PS_NOSMARTLIST}
    Inc(FCheckCount);
    if FCheckCount > FMaxCheckCount then
      Recreate;
    {$ENDIF}
  end;
end;
//-------------------------------------------------------------------

procedure TPSList.Remove(P: Pointer);
var
  I: Cardinal;
begin
  if FCount = 0 then
    Exit;
  I := 0;
  while I < FCount do
  begin
    if FData[I] = P then
    begin
      Delete(I);
      Exit;
    end;
    Inc(I);
  end;
end;
//-------------------------------------------------------------------

procedure TPSList.Clear;
begin
  FCount := 0;
  {$IFNDEF PS_NOSMARTLIST}
  Recreate;
  {$ENDIF}
end;
//-------------------------------------------------------------------

destructor TPSList.Destroy;
begin
  FreeMem(FData, FCapacity * PointerSize);
  inherited Destroy;
end;
//-------------------------------------------------------------------

procedure TPSList.SetItem(Nr: Cardinal; P: Pointer);
begin
  if (FCount = 0) or (Nr >= FCount) then
    Exit;
  FData[Nr] := P;
end;
//-------------------------------------------------------------------

function TPSList.GetItem(Nr: Cardinal): Pointer;  {12}
begin
  if Nr < FCount then
     GetItem := FData[Nr]
  else
    GetItem := nil;
end;

//-------------------------------------------------------------------
function TPSStringList.Count: LongInt;
begin
  count := List.count;
end;

type
  PStr = ^TbtString;

//-------------------------------------------------------------------
function TPSStringList.GetItem(Nr: LongInt): TbtString;
var
  S: PStr;
begin
  s := List.GetItem(Nr);
  if s = nil then
    Result := ''
  else
    Result := s^;
end;
//-------------------------------------------------------------------

procedure TPSStringList.SetItem(Nr: LongInt; const s: TbtString);
var
  p: PStr;
begin
  p := List.GetItem(Nr);
  if p = nil then
    Exit;
  p^ := s;
end;
//-------------------------------------------------------------------

procedure TPSStringList.Add(const P: TbtString);
var
  w: PStr;
begin
  new(w);
  w^ := p;
  List.Add(w);
end;
//-------------------------------------------------------------------

procedure TPSStringList.Delete(NR: LongInt);
var
  w: PStr;
begin
  w := list.getitem(nr);
  if w <> nil then
  begin
    dispose(w);
  end;
  list.Delete(Nr);
end;

procedure TPSStringList.Clear;
begin
  while List.Count > 0 do
    Delete(Pred(List.Count));
end;

constructor TPSStringList.Create;
begin
  inherited Create;
  List := TPSList.Create;
end;

destructor TPSStringList.Destroy;
begin
  while List.Count > 0 do
    Delete(0);
  List.Destroy;
  inherited Destroy;
end;

//-------------------------------------------------------------------
function Fw(const S: TbtString): TbtString; //  First word
var
  x: integer;
begin
  x := Pos(TbtString(' '), s);
  if x > 0 then
    Result := Copy(S, 1, x - 1)
  else
    Result := S;
end;
//-------------------------------------------------------------------
function FastUpperCase(const s: TbtString): TbtString;
{Fast uppercase}
var
  I: Integer;
  C: TbtChar;
begin
  Result := S;
  I := Length(Result);
  while I > 0 do
  begin
    C := Result[I];
    if C in [#97..#122] then
      Result[I] := TbtChar(Ord(Result[I]) - 32);
    Dec(I);
  end;
end;

function FastLowerCase(const s: TbtString): TbtString;
{Fast lowercase}
var
  I: Integer;
  C: TbtChar;
begin
  Result := S;
  I := Length(Result);
  while I > 0 do
  begin
    C := Result[I];
    if C in [#65..#90] then
      Result[I] := TbtChar(Ord(Result[I]) + 32);
    Dec(I);
  end;
end;
//-------------------------------------------------------------------
constructor TPSPascalParser.Create(const akeywords: PRTabArray; const anumkeywords: integer);
begin
  fkeywords := akeywords;
  fnumkeywords := anumkeywords;
  Inherited Create;
end;

function TPSPascalParser.GetCol: Cardinal;
begin
  Result := FRealPosition - FLastEnterPos + 1;
end;

procedure TPSPascalParser.Next;
var
  Err: TPSParserErrorKind;
  FLastUpToken: TbtString;

  function CheckReserved(Const S: ShortString; var CurrTokenId: TPSPasToken): Boolean;
  var
    L, H, I: LongInt;
    J: TbtChar;
    SName: ShortString;
  begin
    L := 0;
    J := S[0];
    H := fnumkeywords - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      SName := fkeywords[i].Name;
      if J = SName[0] then
      begin
        if S = SName then
        begin
          CheckReserved := True;
          CurrTokenId := fkeywords[I].c;
          Exit;
        end;
        if S > SName then
          L := I + 1
        else
          H := I - 1;
      end
      else if S > SName then
        L := I + 1
      else
        H := I - 1;
    end;
    CheckReserved := False;
  end;
  //-------------------------------------------------------------------

  function _GetToken(CurrTokenPos, CurrTokenLen: Cardinal): TbtString;
  var
    s: TbtString;
  begin
    SetLength(s, CurrTokenLen);
    Move(FText[CurrTokenPos], S[1], CurrtokenLen);
    Result := s;
  end;

  function ParseToken(var CurrTokenPos, CurrTokenLen: Cardinal; var CurrTokenId: TPSPasToken): TPSParserErrorKind;
  {Parse the token}
  var
    ct, ci: Cardinal;
    hs: Boolean;
    p: {$IFDEF DELPHI4UP}PAnsiChar{$ELSE}PChar{$ENDIF};
  begin
    ParseToken := iNoError;
    ct := CurrTokenPos;
    case FText[ct] of
      #0:
        begin
          CurrTokenId := CSTI_EOF;
          CurrTokenLen := 0;
        end;
      'A'..'Z', 'a'..'z', '_':
        begin
          ci := ct + 1;
          while (FText[ci] in ['_', '0'..'9', 'a'..'z', 'A'..'Z']) do
          begin
            Inc(ci);
          end;
          CurrTokenLen := ci - ct;

          FLastUpToken := _GetToken(CurrTokenPos, CurrtokenLen);
          p := {$IFDEF DELPHI4UP}PAnsiChar{$ELSE}PChar{$ENDIF}(FLastUpToken);
          while p^ <> #0 do
          begin
            if p^ in [#97..#122] then
              Dec(Byte(p^), 32);
            inc(p);
          end;
          if not CheckReserved(FLastUpToken, CurrTokenId) then
          begin
            CurrTokenId := CSTI_Identifier;
          end;
        end;
      '$':
        begin
          ci := ct + 1;

          while (FText[ci] in ['0'..'9', 'a'..'f', 'A'..'F'])
            do Inc(ci);

          CurrTokenId := CSTI_HexInt;
          CurrTokenLen := ci - ct;
        end;

      '0'..'9':
        begin
          hs := False;
          ci := ct;
          while (FText[ci] in ['0'..'9']) do
          begin
            Inc(ci);
            if (FText[ci] = '.') and (not hs) then
            begin
              if FText[ci + 1] = '.' then
                Break;
              hs := True;
              Inc(ci);
            end;
          end;
          if (FText[ci] in ['E', 'e']) and ((FText[ci + 1] in ['0'..'9'])
            or ((FText[ci + 1] in ['+', '-']) and (FText[ci + 2] in ['0'..'9']))) then
          begin
            hs := True;
            Inc(ci);
            if FText[ci] in ['+', '-'] then
              Inc(ci);
            repeat
              Inc(ci);
            until not (FText[ci] in ['0'..'9']);
          end;

          if hs then
            CurrTokenId := CSTI_Real
          else
            CurrTokenId := CSTI_Integer;

          CurrTokenLen := ci - ct;
        end;

      #39:
        begin
          ci := ct + 1;
          while True do
          begin
            if (FText[ci] = #0) or (FText[ci] = #13) or (FText[ci] = #10) then
              Break;
            if (FText[ci] = #39) then
            begin
              if FText[ci + 1] = #39 then
                Inc(ci)
              else
                Break;
            end;
            Inc(ci);
          end;
          if FText[ci] = #39 then
            CurrTokenId := CSTI_String
          else
          begin
            CurrTokenId := CSTI_String;
            ParseToken := iStringError;
          end;
          CurrTokenLen := ci - ct + 1;
        end;
      '#':
        begin
          ci := ct + 1;
          if FText[ci] = '$' then
          begin
            inc(ci);
            while (FText[ci] in ['A'..'F', 'a'..'f', '0'..'9']) do
            begin
              Inc(ci);
            end;
            CurrTokenId := CSTI_Char;
            CurrTokenLen := ci - ct;
          end
          else
          begin
            while (FText[ci] in ['0'..'9']) do
            begin
              Inc(ci);
            end;
            if FText[ci] in ['A'..'Z', 'a'..'z', '_'] then
            begin
              ParseToken := iCharError;
              CurrTokenId := CSTI_Char;
            end
            else
              CurrTokenId := CSTI_Char;
            CurrTokenLen := ci - ct;
          end;
        end;
      '=':
        begin
          CurrTokenId := CSTI_Equal;
          CurrTokenLen := 1;
        end;
      '>':
        begin
          if FText[ct + 1] = '=' then
          begin
            CurrTokenid := CSTI_GreaterEqual;
            CurrTokenLen := 2;
          end
          else
          begin
            CurrTokenid := CSTI_Greater;
            CurrTokenLen := 1;
          end;
        end;
      '<':
        begin
          if FText[ct + 1] = '=' then
          begin
            CurrTokenId := CSTI_LessEqual;
            CurrTokenLen := 2;
          end
          else if FText[ct + 1] = '>' then
          begin
            CurrTokenId := CSTI_NotEqual;
            CurrTokenLen := 2;
          end
          else
          begin
            CurrTokenId := CSTI_Less;
            CurrTokenLen := 1;
          end;
        end;
      ')':
        begin
          CurrTokenId := CSTI_CloseRound;
          CurrTokenLen := 1;
        end;
      '(':
        begin
          if FText[ct + 1] = '*' then
          begin
            ci := ct + 1;
            while (FText[ci] <> #0) do
            begin
              if (FText[ci] = '*') and (FText[ci + 1] = ')') then
                Break;
              if FText[ci] = #13 then
              begin
                inc(FRow);
                if FText[ci + 1] = #10 then
                  inc(ci);
                FLastEnterPos := ci + 1;
              end
              else if FText[ci] = #10 then
              begin
                inc(FRow);
                FLastEnterPos := ci + 1;
              end;
              Inc(ci);
            end;
            if (FText[ci] = #0) then
            begin
              CurrTokenId := CSTIINT_Comment;
              ParseToken := iCommentError;
            end
            else
            begin
              CurrTokenId := CSTIINT_Comment;
              Inc(ci, 2);
            end;
            CurrTokenLen := ci - ct;
          end
          else
          begin
            CurrTokenId := CSTI_OpenRound;
            CurrTokenLen := 1;
          end;
        end;
      '[':
        begin
          CurrTokenId := CSTI_OpenBlock;
          CurrTokenLen := 1;
        end;
      ']':
        begin
          CurrTokenId := CSTI_CloseBlock;
          CurrTokenLen := 1;
        end;
      ',':
        begin
          CurrTokenId := CSTI_Comma;
          CurrTokenLen := 1;
        end;
      '.':
        begin
          if FText[ct + 1] = '.' then
          begin
            CurrTokenLen := 2;
            CurrTokenId := CSTI_TwoDots;
          end
          else
          begin
            CurrTokenId := CSTI_Period;
            CurrTokenLen := 1;
          end;
        end;
      '@':
        begin
          CurrTokenId := CSTI_AddressOf;
          CurrTokenLen := 1;
        end;
      '^':
        begin
          CurrTokenId := CSTI_Dereference;
          CurrTokenLen := 1;
        end;
      ';':
        begin
          CurrTokenId := CSTI_Semicolon;
          CurrTokenLen := 1;
        end;
      ':':
        begin
          if FText[ct + 1] = '=' then
          begin
            CurrTokenId := CSTI_Assignment;
            CurrTokenLen := 2;
          end
          else
          begin
            CurrTokenId := CSTI_Colon;
            CurrTokenLen := 1;
          end;
        end;
      '+':
        begin
          CurrTokenId := CSTI_Plus;
          CurrTokenLen := 1;
        end;
      '-':
        begin
          CurrTokenId := CSTI_Minus;
          CurrTokenLen := 1;
        end;
      '*':
        begin
          CurrTokenId := CSTI_Multiply;
          CurrTokenLen := 1;
        end;
      '/':
        begin
          if FText[ct + 1] = '/' then
          begin
            ci := ct + 1;
            while (FText[ci] <> #0) and (FText[ci] <> #13) and (FText[ci] <> #10) do
            begin
              Inc(ci);
            end;
            if (FText[ci] = #0) then
            begin
              CurrTokenId := CSTIINT_Comment;
            end
            else
            begin
              CurrTokenId := CSTIINT_Comment;
            end;
            CurrTokenLen := ci - ct;
          end
          else
          begin
            CurrTokenId := CSTI_Divide;
            CurrTokenLen := 1;
          end;
        end;
      #32, #9, #13, #10:
        begin
          ci := ct;
          while (FText[ci] in [#32, #9, #13, #10]) do
          begin
            if FText[ci] = #13 then
            begin
              inc(FRow);
              if FText[ci + 1] = #10 then
                inc(ci);
              FLastEnterPos := ci + 1;
            end
            else if FText[ci] = #10 then
            begin
              inc(FRow);
              FLastEnterPos := ci + 1;
            end;
            Inc(ci);
          end;
          CurrTokenId := CSTIINT_WhiteSpace;
          CurrTokenLen := ci - ct;
        end;
      '{':
        begin
          ci := ct + 1;
          while (FText[ci] <> #0) and (FText[ci] <> '}') do
          begin
            if FText[ci] = #13 then
            begin
              inc(FRow);
              if FText[ci + 1] = #10 then
                inc(ci);
              FLastEnterPos := ci + 1;
            end
            else if FText[ci] = #10 then
            begin
              inc(FRow);
              FLastEnterPos := ci + 1;
            end;
            Inc(ci);
          end;
          if (FText[ci] = #0) then
          begin
            CurrTokenId := CSTIINT_Comment;
            ParseToken := iCommentError;
          end
          else
            CurrTokenId := CSTIINT_Comment;
          CurrTokenLen := ci - ct + 1;
        end;
    else
      begin
        ParseToken := iSyntaxError;
        CurrTokenId := CSTIINT_Comment;
        CurrTokenLen := 1;
      end;
    end;
  end;
  //-------------------------------------------------------------------
begin
  if FText = nil then
  begin
    FTokenLength := 0;
    FRealPosition := 0;
    FTokenId := CSTI_EOF;
    Exit;
  end;
  repeat
    FRealPosition := FRealPosition + Cardinal(FTokenLength);
    Err := ParseToken(FRealPosition, Cardinal(FTokenLength), FTokenID);
    if Err <> iNoError then
    begin
      FTokenLength := 0;
      FTokenId := CSTI_EOF;
      FToken := '';
      FOriginalToken := '';
      if @FParserError <> nil then
        FParserError(Self, Err);
      Exit;
    end;

    case FTokenID of
      CSTIINT_Comment:
        if not FEnableComments then
          Continue
        else
        begin
          SetLength(FOriginalToken, FTokenLength);
          Move(FText[CurrTokenPos], FOriginalToken[1], FTokenLength);
          FToken := FOriginalToken;
        end;
      CSTIINT_WhiteSpace:
        if not FEnableWhitespaces then
          Continue
        else
        begin
          SetLength(FOriginalToken, FTokenLength);
          Move(FText[CurrTokenPos], FOriginalToken[1], FTokenLength);
          FToken := FOriginalToken;
        end;
      CSTI_Integer,
      CSTI_Real,
      CSTI_String,
      CSTI_Char,
      CSTI_HexInt:
        begin
          SetLength(FOriginalToken, FTokenLength);
          Move(FText[CurrTokenPos], FOriginalToken[1], FTokenLength);
          FToken := FOriginalToken;
        end;
      CSTI_Identifier:
        begin
          SetLength(FOriginalToken, FTokenLength);
          Move(FText[CurrTokenPos], FOriginalToken[1], FTokenLength);
          FToken := FLastUpToken;
        end;
    else
      begin
        FOriginalToken := '';
        FToken := '';
      end;
    end;
    Break;
  until False;
end;

procedure TPSPascalParser.SetText(const Data: TbtString);
begin
  FData := Data;
  FText := Pointer(FData);
  FTokenLength := 0;
  FRealPosition := 0;
  FTokenId := CSTI_EOF;
  FLastEnterPos := 0;
  FRow := 1;
  Next;
end;

function TPSList.IndexOf(P: Pointer): Longint;
var
  i: Integer;
begin
  for i := FCount - 1 downto 0 do
  begin
    if FData[i] = p then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

{ TPSUnitList }

function TPSUnitList.Add: TPSUnit;
begin
  Result := TPSUnit.Create(Self);
  fList.Add(Result);
end;

constructor TPSUnitList.Create;
begin
  fList := TPSList.Create;
end;

destructor TPSUnitList.Destroy;
var
  Dummy: Integer;
begin
  for Dummy := 0 to fList.Count - 1 do
    TObject(fList[Dummy]).Free;
  FreeAndNil(fList);

  inherited;
end;

function TPSUnitList.GetUnit(UnitName: TbtString): TPSUnit;
var
  Dummy: Integer;
begin
  UnitName := FastUpperCase(UnitName);
  for Dummy := 0 to fList.Count - 1 do
  begin
    if TPSUnit(fList[Dummy]).UnitName=UnitName then
    begin
      Result := TPSUnit(fList[Dummy]);
      Exit;
    end;
  end;

  Result := Add;

  Result.UnitName := UnitName;
end;

{ TPSUnit }

procedure TPSUnit.AddUses(pUnitName: TbtString);
var
  UsesUnit: TPSUnit;
begin
  UsesUnit := fList.GetUnit(pUnitName);
  fUnits.Add(UsesUnit);
end;

constructor TPSUnit.Create(List: TPSUnitList);
begin
  fUnits := TPSList.Create;
  fList := List;
end;

destructor TPSUnit.Destroy;
begin
  FreeAndNIl(fUnits);
  inherited;
end;

function TPSUnit.HasUses(pUnitName: TbtString): Boolean;
var
  Dummy: Integer;
begin
  pUnitName := FastUpperCase(pUnitName);

  if fUnitName = pUnitName then
  begin
    Result := True;
    Exit;
  end;

  Result := False;

  for Dummy := 0 to fUnits.Count - 1 do
  begin
    Result := TPSUnit(fUnits[Dummy]).HasUses(pUnitName);
    if Result then
      Exit;
  end;
end;

procedure TPSUnit.SetUnitName(const Value: TbtString);
begin
  fUnitName := FastUpperCase(Value);
end;

end.

