unit ps_runtime;
{$I PascalScript.inc}
{

RemObjects Pascal Script III
Copyright (C) 2000-2009 by Carlo Kok (ck@carlo-kok.com)

}

interface

uses
  SysUtils, ps_utils{$IFDEF DELPHI6UP}, variants{$ENDIF}
  {$IFDEF MACOS},uPSCMac{$ELSE}{$IFNDEF PS_NOIDISPATCH}
  {$IFDEF DELPHI3UP}, ActiveX, Windows{$ELSE}, Ole2{$ENDIF}{$ENDIF}{$ENDIF},
  ps_defs;

type
  TPSExec = class;
  TPSStack = class;
  TPSRuntimeAttributes = class;
  TPSRuntimeAttribute = class;

  TPSError = (
    erNoError,
    erCannotImport,
    erInvalidType,
    erInternalError,
    erInvalidHeader,
    erInvalidOpcode,
    erInvalidOpcodeParameter,
    erNoMainProc,
    erOutOfGlobalVarsRange,
    erOutOfProcRange,
    erOutOfRange,
    erOutOfStackRange,
    erTypeMismatch,
    erUnexpectedEof,
    erVersionError,
    erDivideByZero,
    erMathError,
    erCouldNotCallProc,
    erOutofRecordRange,
    erOutOfMemory,
    erException,
    erNullPointerException,
    erNullVariantError,
    erInterfaceNotSupported,
    erCustomError
  );

  TPSStatus = (
    isNotLoaded,
    isLoaded,
    isRunning,
    isPaused
  );

  PByteArray = ^TByteArray;

  TByteArray = array[0..1023] of Byte;

  PDWordArray = ^TDWordArray;

  TDWordArray = array[0..1023] of Cardinal;
{@link(TPSProcRec)
  PIFProcRec is a pointer to a TIProcRec record}
  TPSProcRec = class;

  TIFProcRec = TPSProcRec;

  TPSExternalProcRec = class;

  TIFPSExternalProcRec = TPSExternalProcRec;

  TIFExternalProcRec = TPSExternalProcRec;

  PIFProcRec = TPSProcRec;

  PProcRec = ^TProcRec;

  TPSProcPtr = function(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;

  TPSFreeProc = procedure (Caller: TPSExec; p: PProcRec);

  TPSProcRec = class
  private
    FAttributes: TPSRuntimeAttributes;
  public
    constructor Create(Owner: TPSExec);
    destructor Destroy; override;
    property Attributes: TPSRuntimeAttributes read FAttributes;
  end;

  TPSExternalProcRec = class(TPSProcRec)
  private
    FExt1: Pointer;
    FExt2: Pointer;
    FName: TbtString;
    FProcPtr: TPSProcPtr;
    FDecl: TbtString;
  public
    property Name: TbtString read FName write FName;
    property Decl: TbtString read FDecl write FDecl;
    property Ext1: Pointer read FExt1 write FExt1;
    property Ext2: Pointer read FExt2 write FExt2;
    property ProcPtr: TPSProcPtr read FProcPtr write FProcPtr;
  end;

  TPSInternalProcRec = class(TPSProcRec)
  private
    FData: PByteArray;
    FLength: Cardinal;
    FExportNameHash: Longint;
    FExportDecl: TbtString;
    FExportName: TbtString;
  public
    property Data: PByteArray read FData;
    property Length: Cardinal read FLength;
    property ExportNameHash: Longint read FExportNameHash;
    property ExportName: TbtString read FExportName write FExportName;
    property ExportDecl: TbtString read FExportDecl write FExportDecl;
    destructor Destroy; override;
  end;

  TProcRec = record
    Name: ShortString;
    Hash: Longint;
    ProcPtr: TPSProcPtr;
    FreeProc: TPSFreeProc;
    Ext1, Ext2: Pointer;
  end;

  PBTReturnAddress = ^TBTReturnAddress;

  TBTReturnAddress = packed record
    ProcNo: TPSInternalProcRec;
    Position, StackBase: Cardinal;
  end;

  TPSTypeRec = class
  private
    FExportNameHash: Longint;
    FExportName: TbtString;
    FBaseType: TPSBaseType;
    FAttributes: TPSRuntimeAttributes;
  protected
    FRealSize: Cardinal;
  public
    property RealSize: Cardinal read FRealSize;
    property BaseType: TPSBaseType read FBaseType write FBaseType;
    property ExportName: TbtString read FExportName write FExportName;
    property ExportNameHash: Longint read FExportNameHash write FExportNameHash;
    property Attributes: TPSRuntimeAttributes read FAttributes write FAttributes;
    procedure CalcSize; virtual;
    constructor Create(Owner: TPSExec);
    destructor Destroy; override;
  end;

  TPSTypeRec_ProcPtr = class(TPSTypeRec)
  private
    FParamInfo: TbtString;
  public
    property ParamInfo: TbtString read FParamInfo write FParamInfo;
    procedure CalcSize; override;
  end;
  PIFTypeRec = TPSTypeRec;

  TPSTypeRec_Class = class(TPSTypeRec)
  private
    FCN: TbtString;
  public
    property CN: TbtString read FCN write FCN;
  end;

  {$IFNDEF PS_NOINTERFACES}
  TPSTypeRec_Interface = class(TPSTypeRec)
  private
    FGuid: TGUID;
  public
    property Guid: TGUID read FGuid write FGuid;
  end;
  {$ENDIF}

  TPSTypeRec_Array = class(TPSTypeRec)
  private
    FArrayType: TPSTypeRec;
  public
    property ArrayType: TPSTypeRec read FArrayType write FArrayType;
    procedure CalcSize; override;
  end;

  TPSTypeRec_StaticArray = class(TPSTypeRec_Array)
  private
    FSize: Longint;
    FStartOffset: Longint;
  public
    property Size: Longint read FSize write FSize;
    property StartOffset: Longint read FStartOffset write FStartOffset;
    procedure CalcSize; override;
  end;

  TPSTypeRec_Set = class(TPSTypeRec)
  private
    FBitSize: Longint;
    FByteSize: Longint;
  public
    {The number of bytes this would require (same as realsize)}
    property aByteSize: Longint read FByteSize write FByteSize;
    property aBitSize: Longint read FBitSize write FBitSize;
    procedure CalcSize; override;
  end;

  TPSTypeRec_Record = class(TPSTypeRec)
  private
    FFieldTypes: TPSList;
    FRealFieldOffsets: TPSList;
  public
    property FieldTypes: TPSList read FFieldTypes;
    property RealFieldOffsets: TPSList read FRealFieldOffsets;
    procedure CalcSize; override;
    constructor Create(Owner: TPSExec);
    destructor Destroy; override;
  end;

  PPSVariant = ^TPSVariant;

  PIFVariant = PPSVariant;

  TPSVariant = packed record
    FType: TPSTypeRec;
  end;

  PPSVariantData = ^TPSVariantData;

  TPSVariantData = packed record
    VI: TPSVariant;
    Data: array[0..0] of Byte;
  end;

  PPSVariantU8 = ^TPSVariantU8;

  TPSVariantU8 = packed record
    VI: TPSVariant;
    Data: TbtU8;
  end;

  PPSVariantS8 = ^TPSVariantS8;
  TPSVariantS8 = packed record
    VI: TPSVariant;
    Data: TbtS8;
  end;

  PPSVariantU16 = ^TPSVariantU16;
  TPSVariantU16 = packed record
    VI: TPSVariant;
    Data: TbtU16;
  end;

  PPSVariantS16 = ^TPSVariantS16;
  TPSVariantS16 = packed record
    VI: TPSVariant;
    Data: TbtS16;
  end;

  PPSVariantU32 = ^TPSVariantU32;
  TPSVariantU32 = packed record
    VI: TPSVariant;
    Data: TbtU32;
  end;

  PPSVariantS32 = ^TPSVariantS32;
  TPSVariantS32 = packed record
    VI: TPSVariant;
    Data: TbtS32;
  end;

  {$IFNDEF PS_NOINT64}
  PPSVariantS64 = ^TPSVariantS64;
  TPSVariantS64 = packed record
    VI: TPSVariant;
    Data: TbtS64;
  end;
  {$ENDIF}

  PPSVariantAChar = ^TPSVariantAChar;
  TPSVariantAChar = packed record
    VI: TPSVariant;
    Data: TbtChar;
  end;

  {$IFNDEF PS_NOWIDESTRING}
  PPSVariantWChar = ^TPSVariantWChar;
  TPSVariantWChar = packed record
    VI: TPSVariant;
    Data: TbtWideChar;
  end;
  {$ENDIF}

  PPSVariantAString = ^TPSVariantAString;
  TPSVariantAString = packed record
    VI: TPSVariant;
    Data: TbtString;
  end;

  {$IFNDEF PS_NOWIDESTRING}
  PPSVariantWString = ^TPSVariantWString;
  TPSVariantWString = {$IFNDEF DELPHI2009UP}packed {$ENDIF}record
    VI: TPSVariant;
    Data: TbtWideString;
  end;

  PPSVariantUString = ^TPSVariantUString;
  TPSVariantUString = {$IFNDEF DELPHI2009UP}packed {$ENDIF}record
    VI: TPSVariant;
    Data: TbtUnicodeString;
  end;
  {$ENDIF}

  PPSVariantSingle = ^TPSVariantSingle;
  TPSVariantSingle = packed record
    VI: TPSVariant;
    Data: TbtSingle;
  end;

  PPSVariantDouble = ^TPSVariantDouble;
  TPSVariantDouble = packed record
    VI: TPSVariant;
    Data: TbtDouble;
  end;

  PPSVariantExtended = ^TPSVariantExtended;
  TPSVariantExtended = packed record
    VI: TPSVariant;
    Data: TbtExtended;
  end;

  PPSVariantCurrency = ^TPSVariantCurrency;
  TPSVariantCurrency = packed record
    VI: TPSVariant;
    Data: TbtCurrency;
  end;

  PPSVariantSet = ^TPSVariantSet;
  TPSVariantSet = packed record
    VI: TPSVariant;
    Data: array[0..0] of Byte;
  end;

  {$IFNDEF PS_NOINTERFACES}
  PPSVariantInterface = ^TPSVariantInterface;
  TPSVariantInterface = packed record
    VI: TPSVariant;
    Data: IUnknown;
  end;
  {$ENDIF}

  PPSVariantClass = ^TPSVariantClass;
  TPSVariantClass = packed record
    VI: TPSVariant;
    Data: TObject;
  end;

  PPSVariantRecord = ^TPSVariantRecord;
  TPSVariantRecord = packed record
    VI: TPSVariant;
    data: array[0..0] of byte;
  end;

  PPSVariantDynamicArray = ^TPSVariantDynamicArray;
  TPSVariantDynamicArray = packed record
    VI: TPSVariant;
    Data: Pointer;
  end;

  PPSVariantStaticArray = ^TPSVariantStaticArray;
  TPSVariantStaticArray = packed record
    VI: TPSVariant;
    data: array[0..0] of byte;
  end;

  PPSVariantPointer = ^TPSVariantPointer;
  TPSVariantPointer = packed record
    VI: TPSVariant;
    DataDest: Pointer;
    DestType: TPSTypeRec;
    FreeIt: LongBool;
  end;

  PPSVariantReturnAddress = ^TPSVariantReturnAddress;
  TPSVariantReturnAddress = packed record
    VI: TPSVariant;
    Addr: TBTReturnAddress;
  end;

  PPSVariantVariant = ^TPSVariantVariant;
  TPSVariantVariant = packed record
    VI: TPSVariant;
    Data: Variant;
  end;

  PPSVariantProcPtr = ^TPSVariantProcPtr;
  TPSVariantProcPtr = packed record
    VI: TPSVariant;
    ProcNo: Cardinal;
    Self: Pointer;
    Ptr: Pointer;
    {
      ProcNo = 0  means Self/Ptr become active (Ptr = nil means it's nil)
    }
  end;

  TPSVarFreeType = (
    vtNone,
    vtTempVar
  );

  TPSResultData = packed record
    P: Pointer;
    aType: TPSTypeRec;
    FreeType: TPSVarFreeType;
  end;

  PPSResource = ^TPSResource;
  TPSResource = record
    Proc: Pointer;
    P: Pointer;
  end;

  TPSAttributeUseProc = function (Sender: TPSExec; const AttribType: TbtString; Attr: TPSRuntimeAttribute): Boolean;

  TPSAttributeType = class
  private
    FTypeName: TbtString;
    FUseProc: TPSAttributeUseProc;
    FTypeNameHash: Longint;
  public
    property UseProc: TPSAttributeUseProc read FUseProc write FUseProc;
    property TypeName: TbtString read FTypeName write FTypeName;
    property TypeNameHash: Longint read FTypeNameHash write FTypeNameHash;
  end;

  PClassItem = ^TClassItem;
  TClassItem = record
    FName: TbtString;
    FNameHash: Longint;
    b: byte;
    case byte of
      0: (Ptr: Pointer);
      1: (PointerInList: Pointer);
      3: (FReadFunc, FWriteFunc: Pointer); {Property Helper}
      4: (Ptr2: Pointer);
      5: (PointerInList2: Pointer);
      6: (); {Property helper, like 3}
      7: (); {Property helper that will pass it's name}
  end;

  PPSVariantIFC = ^TPSVariantIFC;
  {Temporary variant into record}
  TPSVariantIFC = packed record
    Dta: Pointer;
    aType: TPSTypeRec;
    VarParam: Boolean;
  end;
  PIFPSVariantIFC = PPSVariantIFC;
  TIFPSVariantIFC = TPSVariantIFC;

  TPSRuntimeAttribute = class(TObject)
  private
    FValues: TPSStack;
    FAttribType: TbtString;
    FOwner: TPSRuntimeAttributes;
    FAttribTypeHash: Longint;
    function GetValue(I: Longint): PIFVariant;
    function GetValueCount: Longint;
  public
    property Owner: TPSRuntimeAttributes read FOwner;
    property AttribType: TbtString read FAttribType write FAttribType;
    property AttribTypeHash: Longint read FAttribTypeHash write FAttribTypeHash;
    property ValueCount: Longint read GetValueCount;
    property Value[I: Longint]: PIFVariant read GetValue;
    function AddValue(aType: TPSTypeRec): PPSVariant;
    procedure DeleteValue(i: Longint);
    procedure AdjustSize;
    constructor Create(Owner: TPSRuntimeAttributes);
    destructor Destroy; override;
  end;

  TPSRuntimeAttributes = class(TObject)
  private
    FAttributes: TPSList;
    FOwner: TPSExec;
    function GetCount: Longint;
    function GetItem(I: Longint): TPSRuntimeAttribute;
  public
    property Owner: TPSExec read FOwner;
    property Count: Longint read GetCount;
    property Items[I: Longint]: TPSRuntimeAttribute read GetItem; default;
    procedure Delete(I: Longint);
    function Add: TPSRuntimeAttribute;
    function FindAttribute(const Name: TbtString): TPSRuntimeAttribute;
    constructor Create(AOwner: TPSExec);
    destructor Destroy; override;
  end;

  TPSOnGetNVariant = function (Sender: TPSExec; const Name: TbtString): Variant;

  TPSOnSetNVariant = procedure (Sender: TPSExec; const Name: TbtString; V: Variant);

  TPSOnLineEvent = procedure(Sender: TPSExec);

  TPSOnSpecialProcImport = function (Sender: TPSExec; p: TPSExternalProcRec; Tag: Pointer): Boolean;

  TPSOnException = procedure (Sender: TPSExec; ExError: TPSError; const ExParam: TbtString; ExObject: TObject; ProcNo, Position: Cardinal);

  TPSExec = class(TObject)
  private
    FOnGetNVariant: TPSOnGetNVariant;
    FOnSetNVariant: TPSOnSetNVariant;
    FId: Pointer;
    FJumpFlag: Boolean;
    FCallCleanup: Boolean;
    FOnException: TPSOnException;
    function ReadData(var Data; Len: Cardinal): Boolean;
    function ReadLong(var b: Cardinal): Boolean;
    function DoCalc(var1, Var2: Pointer; var1Type, var2type: TPSTypeRec; CalcType: Cardinal): Boolean;
    function DoBooleanCalc(var1, Var2, into: Pointer; var1Type, var2type, intotype: TPSTypeRec; Cmd: Cardinal): Boolean;
    function SetVariantValue(dest, Src: Pointer; desttype, srctype: TPSTypeRec): Boolean;
    function ReadVariable(var Dest: TPSResultData; UsePointer: Boolean): Boolean;
    function DoBooleanNot(Dta: Pointer; aType: TPSTypeRec): Boolean;
    function DoMinus(Dta: Pointer; aType: TPSTypeRec): Boolean;
    function DoIntegerNot(Dta: Pointer; aType: TPSTypeRec): Boolean;
    procedure RegisterStandardProcs;
  protected
    FReturnAddressType: TPSTypeRec;
    FVariantType: TPSTypeRec;
    FVariantArrayType: TPSTypeRec;
    FAttributeTypes: TPSList;
    FExceptionStack: TPSList;
    FResources: TPSList;
    FExportedVars: TPSList;
    FTypes: TPSList;
    FProcs: TPSList;
    FGlobalVars: TPSStack;
    FTempVars: TPSStack;
    FStack: TPSStack;
    FMainProc: Cardinal;
    FStatus: TPSStatus;
    FCurrProc: TPSInternalProcRec;
    FData: PByteArray;
    FDataLength: Cardinal;
    FCurrentPosition: Cardinal;
    FCurrStackBase: Cardinal;
    FOnRunLine: TPSOnLineEvent;
    FSpecialProcList: TPSList;
    FRegProcs: TPSList;
    ExObject: TObject;
    ExProc: Cardinal;
    ExPos: Cardinal;
    ExEx: TPSError;
    ExParam: TbtString;
    FAllowNullClasses: Boolean;
    function InvokeExternalMethod(At: TPSTypeRec_ProcPtr; Slf, Ptr: Pointer): Boolean;
    function InnerfuseCall(_Self, Address: Pointer; CallingConv: TPSCallingConvention; Params: TPSList; res: PPSVariantIFC): Boolean;
    procedure RunLine; virtual;
    function ImportProc(const Name: ShortString; proc: TPSExternalProcRec): Boolean; virtual;
    procedure ExceptionProc(proc, Position: Cardinal; Ex: TPSError; const s: TbtString; NewObject: TObject); virtual;
    function FindSpecialProcImport(P: TPSOnSpecialProcImport): pointer;
  public
    function LastEx: TPSError;
    function LastExParam: TbtString;
    function LastExProc: Integer;
    function LastExPos: Integer;
    function LastExObject: TObject;
    procedure CMD_Err(EC: TPSError);
    procedure CMD_Err2(EC: TPSError; const Param: TbtString);
    procedure CMD_Err3(EC: TPSError; const Param: TbtString; ExObject: TObject);
    property Id: Pointer read FID write FID;
    class function About: TbtString;
    function RunProc(Params: TPSList; ProcNo: Cardinal): Boolean;
    function RunProcP(const Params: array of Variant; const Procno: Cardinal): Variant;
    function RunProcPVar(var Params: array of Variant; const Procno: Cardinal): Variant;
    function RunProcPN(const Params: array of Variant; const ProcName: TbtString): Variant;
    function FindType(StartAt: Cardinal; BaseType: TPSBaseType; var l: Cardinal): PIFTypeRec;
    function FindType2(BaseType: TPSBaseType): PIFTypeRec;
    function GetTypeNo(l: Cardinal): PIFTypeRec;
    function GetType(const Name: TbtString): Cardinal;
    function GetProc(const Name: TbtString): Cardinal;
    function GetVar(const Name: TbtString): Cardinal;
    function GetVar2(const Name: TbtString): PIFVariant;
    function GetVarNo(C: Cardinal): PIFVariant;
    function GetProcNo(C: Cardinal): PIFProcRec;
    function GetProcCount: Cardinal;
    function GetVarCount: Longint;
    function GetTypeCount: Longint;
    constructor Create; virtual;
    destructor Destroy; Override;
    function RunScript: Boolean;
    function LoadData(const s: TbtString): Boolean; virtual;
    procedure Clear; virtual;
    procedure Cleanup; virtual;
    procedure Stop; virtual;
    procedure Pause; virtual;
    property CallCleanup: Boolean read FCallCleanup write FCallCleanup;
    property Status: TPSStatus read FStatus;
    property OnRunLine: TPSOnLineEvent read FOnRunLine write FOnRunLine;
    procedure ClearspecialProcImports;
    procedure AddSpecialProcImport(const FName: TbtString; P: TPSOnSpecialProcImport; Tag: Pointer);
    function RegisterFunctionName(const Name: TbtString; ProcPtr: TPSProcPtr;
      Ext1, Ext2: Pointer): PProcRec;
    procedure RegisterDelphiFunction(ProcPtr: Pointer; const Name: TbtString; CC: TPSCallingConvention);
    procedure RegisterDelphiMethod(Slf, ProcPtr: Pointer; const Name: TbtString; CC: TPSCallingConvention);
    function GetProcAsMethod(const ProcNo: Cardinal): TMethod;
    function GetProcAsMethodN(const ProcName: TbtString): TMethod;
    procedure RegisterAttributeType(useproc: TPSAttributeUseProc; const TypeName: TbtString);
    procedure ClearFunctionList;
    property ExceptionProcNo: Cardinal read ExProc;
    property ExceptionPos: Cardinal read ExPos;
    property ExceptionCode: TPSError read ExEx;
    property ExceptionString: TbtString read ExParam;
    property ExceptionObject: TObject read ExObject write ExObject;
    procedure AddResource(Proc, P: Pointer);
    function IsValidResource(Proc, P: Pointer): Boolean;
    procedure DeleteResource(P: Pointer);
    function FindProcResource(Proc: Pointer): Pointer;
    function FindProcResource2(Proc: Pointer; var StartAt: Longint): Pointer;
    procedure RaiseCurrentException;
    property OnException: TPSOnException read FOnException write FOnException;
    property OnGetNVariant: TPSOnGetNVariant read FOnGetNVariant write FOnGetNVariant;
    property OnSetNVariant: TPSOnSetNVariant read FOnSetNVariant write FOnSetNVariant;
    property AllowNullClasses: Boolean read FAllowNullClasses write FAllowNullClasses;
  end;

  TPSStack = class(TPSList)
  private
    FDataPtr: Pointer;
    FCapacity,
    FLength: Longint;
    function GetItem(I: Longint): PPSVariant;
    procedure SetCapacity(const Value: Longint);
    procedure AdjustLength;
  public
    property DataPtr: Pointer read FDataPtr;
    property Capacity: Longint read FCapacity write SetCapacity;
    property Length: Longint read FLength;
    constructor Create;
    destructor Destroy; override;
    procedure Clear; {$IFDEF DELPHI5UP} reintroduce;{$ELSE} override; {$ENDIF}
    function Push(TotalSize: Longint): PPSVariant;
    function PushType(aType: TPSTypeRec): PPSVariant;
    procedure Pop;
    function GetInt(ItemNo: Longint): Longint;
    function GetUInt(ItemNo: Longint): Cardinal;
    {$IFNDEF PS_NOINT64}
    function GetInt64(ItemNo: Longint): Int64;
    {$ENDIF}
    function GetString(ItemNo: Longint): string; // calls the native method
    function GetAnsiString(ItemNo: Longint): TbtString;
    {$IFNDEF PS_NOWIDESTRING}
    function GetWideString(ItemNo: Longint): TbtWideString;
    function GetUnicodeString(ItemNo: Longint): TbtUnicodeString;
    {$ENDIF}
    function GetReal(ItemNo: Longint): Extended;
    function GetCurrency(ItemNo: Longint): Currency;
    function GetBool(ItemNo: Longint): Boolean;
    function GetClass(ItemNo: Longint): TObject;
    procedure SetInt(ItemNo: Longint; const Data: Longint);
    procedure SetUInt(ItemNo: Longint; const Data: Cardinal);
    {$IFNDEF PS_NOINT64}
    procedure SetInt64(ItemNo: Longint; const Data: Int64);
    {$ENDIF}
    procedure SetString(ItemNo: Longint; const Data: string);
    procedure SetAnsiString(ItemNo: Longint; const Data: TbtString);
    {$IFNDEF PS_NOWIDESTRING}
    procedure SetWideString(ItemNo: Longint; const Data: TbtWideString);
    procedure SetUnicodeString(ItemNo: Longint; const Data: TbtUnicodeString);
    {$ENDIF}
    procedure SetReal(ItemNo: Longint; const Data: Extended);
    procedure SetCurrency(ItemNo: Longint; const Data: Currency);
    procedure SetBool(ItemNo: Longint; const Data: Boolean);
    procedure SetClass(ItemNo: Longint; const Data: TObject);
    property Items[I: Longint]: PPSVariant read GetItem; default;
  end;

//==============================================================================
//
// PSErrorToString
//
//==============================================================================
function PSErrorToString(x: TPSError; const Param: TbtString): TbtString;

//==============================================================================
//
// TIFErrorToString
//
//==============================================================================
function TIFErrorToString(x: TPSError; const Param: TbtString): TbtString;

//==============================================================================
//
// CreateHeapVariant
//
//==============================================================================
function CreateHeapVariant(aType: TPSTypeRec): PPSVariant;

//==============================================================================
//
// DestroyHeapVariant
//
//==============================================================================
procedure DestroyHeapVariant(v: PPSVariant);

//==============================================================================
//
// FreePIFVariantList
//
//==============================================================================
procedure FreePIFVariantList(l: TPSList);

//==============================================================================
//
// FreePSVariantList
//
//==============================================================================
procedure FreePSVariantList(l: TPSList);

const
  ENoError = ERNoError;

//==============================================================================
//
// PIFVariantToVariant
//
//==============================================================================
function PIFVariantToVariant(Src: PIFVariant; var Dest: Variant): Boolean;

//==============================================================================
//
// VariantToPIFVariant
//
//==============================================================================
function VariantToPIFVariant(Exec: TPSExec; const Src: Variant; Dest: PIFVariant): Boolean;

//==============================================================================
//
// PSGetRecField
//
//==============================================================================
function PSGetRecField(const avar: TPSVariantIFC; Fieldno: Longint): TPSVariantIFC;

//==============================================================================
//
// PSGetArrayField
//
//==============================================================================
function PSGetArrayField(const avar: TPSVariantIFC; Fieldno: Longint): TPSVariantIFC;

//==============================================================================
//
// NewTPSVariantRecordIFC
//
//==============================================================================
function NewTPSVariantRecordIFC(avar: PPSVariant; Fieldno: Longint): TPSVariantIFC;

//==============================================================================
//
// NewTPSVariantIFC
//
//==============================================================================
function NewTPSVariantIFC(avar: PPSVariant; varparam: boolean): TPSVariantIFC;

//==============================================================================
//
// NewPPSVariantIFC
//
//==============================================================================
function NewPPSVariantIFC(avar: PPSVariant; varparam: boolean): PPSVariantIFC;

//==============================================================================
//
// DisposePPSVariantIFC
//
//==============================================================================
procedure DisposePPSVariantIFC(aVar: PPSVariantIFC);

//==============================================================================
//
// DisposePPSVariantIFCList
//
//==============================================================================
procedure DisposePPSVariantIFCList(list: TPSList);

//==============================================================================
//
// PSGetObject
//
//==============================================================================
function PSGetObject(Src: Pointer; aType: TPSTypeRec): TObject;

//==============================================================================
//
// PSGetUInt
//
//==============================================================================
function PSGetUInt(Src: Pointer; aType: TPSTypeRec): Cardinal;

{$IFNDEF PS_NOINT64}

//==============================================================================
//
// PSGetInt64
//
//==============================================================================
function PSGetInt64(Src: Pointer; aType: TPSTypeRec): Int64;
{$ENDIF}

//==============================================================================
//
// PSGetReal
//
//==============================================================================
function PSGetReal(Src: Pointer; aType: TPSTypeRec): Extended;

//==============================================================================
//
// PSGetCurrency
//
//==============================================================================
function PSGetCurrency(Src: Pointer; aType: TPSTypeRec): Currency;

//==============================================================================
//
// PSGetInt
//
//==============================================================================
function PSGetInt(Src: Pointer; aType: TPSTypeRec): Longint;

//==============================================================================
//
// PSGetString
//
//==============================================================================
function PSGetString(Src: Pointer; aType: TPSTypeRec): string;

//==============================================================================
//
// PSGetAnsiString
//
//==============================================================================
function PSGetAnsiString(Src: Pointer; aType: TPSTypeRec): TbtString;

{$IFNDEF PS_NOWIDESTRING}

//==============================================================================
//
// PSGetWideString
//
//==============================================================================
function PSGetWideString(Src: Pointer; aType: TPSTypeRec): TbtWideString;

//==============================================================================
//
// PSGetUnicodeString
//
//==============================================================================
function PSGetUnicodeString(Src: Pointer; aType: TPSTypeRec): TbtUnicodeString;
{$ENDIF}

//==============================================================================
//
// PSSetObject
//
//==============================================================================
procedure PSSetObject(Src: Pointer; aType: TPSTypeRec; var OK: Boolean; Const val: TObject);

//==============================================================================
//
// PSSetUInt
//
//==============================================================================
procedure PSSetUInt(Src: Pointer; aType: TPSTypeRec; var OK: Boolean; const Val: Cardinal);

{$IFNDEF PS_NOINT64}

//==============================================================================
//
// PSSetInt64
//
//==============================================================================
procedure PSSetInt64(Src: Pointer; aType: TPSTypeRec; var OK: Boolean; const Val: Int64);
{$ENDIF}

//==============================================================================
//
// PSSetReal
//
//==============================================================================
procedure PSSetReal(Src: Pointer; aType: TPSTypeRec; var OK: Boolean; const Val: Extended);

//==============================================================================
//
// PSSetCurrency
//
//==============================================================================
procedure PSSetCurrency(Src: Pointer; aType: TPSTypeRec; var OK: Boolean; const Val: Currency);

//==============================================================================
//
// PSSetInt
//
//==============================================================================
procedure PSSetInt(Src: Pointer; aType: TPSTypeRec; var OK: Boolean; const Val: Longint);

//==============================================================================
//
// PSSetString
//
//==============================================================================
procedure PSSetString(Src: Pointer; aType: TPSTypeRec; var OK: Boolean; const Val: String);

//==============================================================================
//
// PSSetAnsiString
//
//==============================================================================
procedure PSSetAnsiString(Src: Pointer; aType: TPSTypeRec; var OK: Boolean; const Val: TbtString);

{$IFNDEF PS_NOWIDESTRING}

//==============================================================================
//
// PSSetWideString
//
//==============================================================================
procedure PSSetWideString(Src: Pointer; aType: TPSTypeRec; var OK: Boolean; const Val: TbtWideString);

//==============================================================================
//
// PSSetUnicodeString
//
//==============================================================================
procedure PSSetUnicodeString(Src: Pointer; aType: TPSTypeRec; var OK: Boolean; const Val: TbtUnicodeString);
{$ENDIF}

//==============================================================================
//
// VNSetPointerTo
//
//==============================================================================
procedure VNSetPointerTo(const Src: TPSVariantIFC; Data: Pointer; aType: TPSTypeRec);

//==============================================================================
//
// VNGetUInt
//
//==============================================================================
function VNGetUInt(const Src: TPSVariantIFC): Cardinal;

{$IFNDEF PS_NOINT64}

//==============================================================================
//
// VNGetInt64
//
//==============================================================================
function VNGetInt64(const Src: TPSVariantIFC): Int64;
{$ENDIF}

//==============================================================================
//
// VNGetReal
//
//==============================================================================
function VNGetReal(const Src: TPSVariantIFC): Extended;

//==============================================================================
//
// VNGetCurrency
//
//==============================================================================
function VNGetCurrency(const Src: TPSVariantIFC): Currency;

//==============================================================================
//
// VNGetInt
//
//==============================================================================
function VNGetInt(const Src: TPSVariantIFC): Longint;

//==============================================================================
//
// VNGetString
//
//==============================================================================
function VNGetString(const Src: TPSVariantIFC): String;

//==============================================================================
//
// VNGetAnsiString
//
//==============================================================================
function VNGetAnsiString(const Src: TPSVariantIFC): TbtString;

{$IFNDEF PS_NOWIDESTRING}

//==============================================================================
//
// VNGetWideString
//
//==============================================================================
function VNGetWideString(const Src: TPSVariantIFC): TbtWideString;

//==============================================================================
//
// VNGetUnicodeString
//
//==============================================================================
function VNGetUnicodeString(const Src: TPSVariantIFC): TbtUnicodeString;
{$ENDIF}

//==============================================================================
//
// VNSetUInt
//
//==============================================================================
procedure VNSetUInt(const Src: TPSVariantIFC; const Val: Cardinal);

{$IFNDEF PS_NOINT64}

//==============================================================================
//
// VNSetInt64
//
//==============================================================================
procedure VNSetInt64(const Src: TPSVariantIFC; const Val: Int64);
{$ENDIF}

//==============================================================================
//
// VNSetReal
//
//==============================================================================
procedure VNSetReal(const Src: TPSVariantIFC; const Val: Extended);

//==============================================================================
//
// VNSetCurrency
//
//==============================================================================
procedure VNSetCurrency(const Src: TPSVariantIFC; const Val: Currency);

//==============================================================================
//
// VNSetInt
//
//==============================================================================
procedure VNSetInt(const Src: TPSVariantIFC; const Val: Longint);

//==============================================================================
//
// VNSetString
//
//==============================================================================
procedure VNSetString(const Src: TPSVariantIFC; const Val: String);

//==============================================================================
//
// VNSetAnsiString
//
//==============================================================================
procedure VNSetAnsiString(const Src: TPSVariantIFC; const Val: TbtString);

{$IFNDEF PS_NOWIDESTRING}

//==============================================================================
//
// VNSetWideString
//
//==============================================================================
procedure VNSetWideString(const Src: TPSVariantIFC; const Val: TbtWideString);

//==============================================================================
//
// VNSetUnicodeString
//
//==============================================================================
procedure VNSetUnicodeString(const Src: TPSVariantIFC; const Val: TbtUnicodeString);
{$ENDIF}

//==============================================================================
//
// VGetUInt
//
//==============================================================================
function VGetUInt(const Src: PIFVariant): Cardinal;

{$IFNDEF PS_NOINT64}

//==============================================================================
//
// VGetInt64
//
//==============================================================================
function VGetInt64(const Src: PIFVariant): Int64;
{$ENDIF}

//==============================================================================
//
// VGetReal
//
//==============================================================================
function VGetReal(const Src: PIFVariant): Extended;

//==============================================================================
//
// VGetCurrency
//
//==============================================================================
function VGetCurrency(const Src: PIFVariant): Currency;

//==============================================================================
//
// VGetInt
//
//==============================================================================
function VGetInt(const Src: PIFVariant): Longint;

//==============================================================================
//
// VGetString
//
//==============================================================================
function VGetString(const Src: PIFVariant): String;

//==============================================================================
//
// VGetAnsiString
//
//==============================================================================
function VGetAnsiString(const Src: PIFVariant): TbtString;

{$IFNDEF PS_NOWIDESTRING}

//==============================================================================
//
// VGetWideString
//
//==============================================================================
function VGetWideString(const Src: PIFVariant): TbtWideString;

//==============================================================================
//
// VGetUnicodeString
//
//==============================================================================
function VGetUnicodeString(const Src: PIFVariant): TbtUnicodeString;
{$ENDIF}

//==============================================================================
//
// VSetPointerTo
//
//==============================================================================
procedure VSetPointerTo(const Src: PIFVariant; Data: Pointer; aType: TPSTypeRec);

//==============================================================================
//
// VSetUInt
//
//==============================================================================
procedure VSetUInt(const Src: PIFVariant; const Val: Cardinal);

{$IFNDEF PS_NOINT64}

//==============================================================================
//
// VSetInt64
//
//==============================================================================
procedure VSetInt64(const Src: PIFVariant; const Val: Int64);
{$ENDIF}

//==============================================================================
//
// VSetReal
//
//==============================================================================
procedure VSetReal(const Src: PIFVariant; const Val: Extended);

//==============================================================================
//
// VSetCurrency
//
//==============================================================================
procedure VSetCurrency(const Src: PIFVariant; const Val: Currency);

//==============================================================================
//
// VSetInt
//
//==============================================================================
procedure VSetInt(const Src: PIFVariant; const Val: Longint);

//==============================================================================
//
// VSetString
//
//==============================================================================
procedure VSetString(const Src: PIFVariant; const Val: string);

//==============================================================================
//
// VSetAnsiString
//
//==============================================================================
procedure VSetAnsiString(const Src: PIFVariant; const Val: TbtString);

{$IFNDEF PS_NOWIDESTRING}

//==============================================================================
//
// VSetWideString
//
//==============================================================================
procedure VSetWideString(const Src: PIFVariant; const Val: TbtWideString);

//==============================================================================
//
// VSetUnicodeString
//
//==============================================================================
procedure VSetUnicodeString(const Src: PIFVariant; const Val: TbtUnicodeString);
{$ENDIF}

type
  EPSException = class(Exception)
  private
    FProcPos: Cardinal;
    FProcNo: Cardinal;
    FExec: TPSExec;
  public
    constructor Create(const Error: TbtString; Exec: TPSExec; Procno, ProcPos: Cardinal);
    property ProcNo: Cardinal read FProcNo;
    property ProcPos: Cardinal read FProcPos;
    property Exec: TPSExec read FExec;
  end;

  TPSRuntimeClass = class
  protected
    FClassName: TbtString;
    FClassNameHash: Longint;
    FClassItems: TPSList;
    FClass: TClass;
    FEndOfVmt: Longint;
  public
    procedure RegisterConstructor(ProcPtr: Pointer; const Name: TbtString);
    procedure RegisterVirtualConstructor(ProcPtr: Pointer; const Name: TbtString);
    procedure RegisterMethod(ProcPtr: Pointer; const Name: TbtString);
    procedure RegisterVirtualMethod(ProcPtr: Pointer; const Name: TbtString);
    procedure RegisterVirtualAbstractMethod(ClassDef: TClass; ProcPtr: Pointer; const Name: TbtString);
    procedure RegisterPropertyHelper(ReadFunc, WriteFunc: Pointer; const Name: TbtString);
    procedure RegisterPropertyHelperName(ReadFunc, WriteFunc: Pointer; const Name: TbtString);
    procedure RegisterEventPropertyHelper(ReadFunc, WriteFunc: Pointer; const Name: TbtString);
    constructor Create(aClass: TClass; const AName: TbtString);
    destructor Destroy; override;
  end;

  TPSRuntimeClassImporter = class
  private
    FClasses: TPSList;
  public
    constructor Create; virtual;
    constructor CreateAndRegister(Exec: TPSExec; AutoFree: Boolean);
    destructor Destroy; override;
    function Add(aClass: TClass): TPSRuntimeClass;
    function Add2(aClass: TClass; const Name: TbtString): TPSRuntimeClass;
    procedure Clear;
    function FindClass(const Name: TbtString): TPSRuntimeClass;
  end;
  TIFPSRuntimeClassImporter = TPSRuntimeClassImporter;
  TPSResourceFreeProc = procedure (Sender: TPSExec; P: TPSRuntimeClassImporter);

//==============================================================================
//
// RegisterClassLibraryRuntime
//
//==============================================================================
procedure RegisterClassLibraryRuntime(SE: TPSExec; Importer: TPSRuntimeClassImporter);

//==============================================================================
//
// SetVariantToClass
//
//==============================================================================
procedure SetVariantToClass(V: PIFVariant; Cl: TObject);

{$IFNDEF PS_NOINTERFACES}

//==============================================================================
//
// SetVariantToInterface
//
//==============================================================================
procedure SetVariantToInterface(V: PIFVariant; Cl: IUnknown);
{$ENDIF}

//==============================================================================
//
// MyAllMethodsHandler
//
//==============================================================================
procedure MyAllMethodsHandler;

//==============================================================================
//
// GetMethodInfoRec
//
//==============================================================================
function GetMethodInfoRec(SE: TPSExec; ProcNo: Cardinal): Pointer;

//==============================================================================
//
// MkMethod
//
//==============================================================================
function MkMethod(FSE: TPSExec; No: Cardinal): TMethod;

type
  TIFInternalProcRec = TPSInternalProcRec;

  TIFError = TPSError;

  TIFStatus = TPSStatus;

  TIFPSExec = TPSExec;

  TIFPSStack = TPSStack;

  TIFTypeRec = TPSTypeRec;

  TPSCallingConvention = ps_utils.TPSCallingConvention;

const
  cdRegister = ps_utils.cdRegister;
  cdPascal = ps_utils.cdPascal;
  cdCdecl = ps_utils.cdCdecl;
  cdStdCall = ps_utils.cdStdCall;
  InvalidVal = Cardinal(-1);

//==============================================================================
//
//  PSDynArrayGetLength
//
//==============================================================================
function PSDynArrayGetLength(arr: Pointer; aType: TPSTypeRec): Longint;

//==============================================================================
//
// PSDynArraySetLength
//
//==============================================================================
procedure PSDynArraySetLength(var arr: Pointer; aType: TPSTypeRec; NewLength: Longint);

//==============================================================================
//
//  GetPSArrayLength
//
//==============================================================================
function GetPSArrayLength(Arr: PIFVariant): Longint;

//==============================================================================
//
// SetPSArrayLength
//
//==============================================================================
procedure SetPSArrayLength(Arr: PIFVariant; NewLength: Longint);

//==============================================================================
//
// PSVariantToString
//
//==============================================================================
function PSVariantToString(const p: TPSVariantIFC; const ClassProperties: TbtString): TbtString;

//==============================================================================
//
// MakeString
//
//==============================================================================
function MakeString(const s: TbtString): TbtString;

{$IFNDEF PS_NOWIDESTRING}

//==============================================================================
//
// MakeWString
//
//==============================================================================
function MakeWString(const s: TbtUnicodeString): TbtString;
{$ENDIF}

{$IFNDEF PS_NOIDISPATCH}

//==============================================================================
//
// IDispatchInvoke
//
//==============================================================================
function IDispatchInvoke(Self: IDispatch; PropertySet: Boolean; const Name: TbtString; const Par: array of Variant): Variant;
{$ENDIF}

implementation

uses
  TypInfo {$IFDEF DELPHI3UP}{$IFNDEF FPC}{$IFNDEF KYLIX} , ComObj {$ENDIF}{$ENDIF}{$ENDIF}{$IFDEF PS_FPC_HAS_COM}, ComObj{$ENDIF};

{$IFDEF DELPHI3UP }
resourceString
{$ELSE }
const
{$ENDIF }
  RPS_UnknownIdentifier = 'Unknown Identifier';
  RPS_Exception = 'Exception: %s';
  RPS_Invalid = '[Invalid]';
  //- PSErrorToString
  RPS_NoError = 'No Error';
  RPS_CannotImport = 'Cannot Import %s';
  RPS_InvalidType = 'Invalid Type';
  RPS_InternalError = 'Internal error';
  RPS_InvalidHeader = 'Invalid Header';
  RPS_InvalidOpcode = 'Invalid Opcode';
  RPS_InvalidOpcodeParameter = 'Invalid Opcode Parameter';
  RPS_NoMainProc = 'no Main Proc';
  RPS_OutOfGlobalVarsRange = 'Out of Global Vars range';
  RPS_OutOfProcRange = 'Out of Proc Range';
  RPS_OutOfRange = 'Out Of Range';
  RPS_OutOfStackRange = 'Out Of Stack Range';
  RPS_TypeMismatch = 'Type Mismatch';
  RPS_UnexpectedEof = 'Unexpected End Of File';
  RPS_VersionError = 'Version error';
  RPS_DivideByZero = 'divide by Zero';
  RPS_MathError = 'Math error';
  RPS_CouldNotCallProc = 'Could not call proc';
  RPS_OutofRecordRange = 'Out of Record Fields Range';
  RPS_NullPointerException = 'Null Pointer Exception';
  RPS_NullVariantError = 'Null variant error';
  RPS_OutOfMemory = 'Out Of Memory';
  RPS_InterfaceNotSupported = 'Interface not supported';
  RPS_UnknownError = 'Unknown error';

  RPS_InvalidVariable = 'Invalid variable';
  RPS_InvalidArray = 'Invalid array';
  RPS_OLEError = 'OLE error %.8x';
  RPS_UnknownProcedure = 'Unknown procedure';
  RPS_NotEnoughParameters = 'Not enough parameters';
  RPS_InvalidParameter = 'Invalid parameter';
  RPS_TooManyParameters = 'Too many parameters';
  RPS_OutOfStringRange = 'Out of string range';
  RPS_CannotCastInterface = 'Cannot cast an interface';
  RPS_CannotCastObject = 'Cannot cast an object';
  RPS_CapacityLength = 'Capacity < Length';
  RPS_CanOnlySendLastItem = 'Can only remove last item from stack';
  RPS_NILInterfaceException = 'Nil interface';
  RPS_UnknownMethod = 'Unknown method';

type
  PPSExportedVar = ^TPSExportedVar;
  TPSExportedVar = record
    FName: TbtString;
    FNameHash: Longint;
    FVarNo: Cardinal;
  end;

  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
    ExceptionRecord: Pointer;
  end;

  TPSExceptionHandler = class
    CurrProc: TPSInternalProcRec;
    BasePtr, StackSize: Cardinal;
    FinallyOffset, ExceptOffset, Finally2Offset, EndOfBlock: Cardinal;
    ExceptionData: TPSError;
    ExceptionObject: TObject;
    ExceptionParam: TbtString;
    destructor Destroy; override;
  end;

  TPSHeader = packed record
    HDR: Cardinal;
    PSBuildNo: Cardinal;
    TypeCount: Cardinal;
    ProcCount: Cardinal;
    VarCount: Cardinal;
    MainProcNo: Cardinal;
    ImportTableSize: Cardinal;
  end;

  TPSExportItem = packed record
    ProcNo: Cardinal;
    NameLength: Cardinal;
    DeclLength: Cardinal;
  end;

  TPSType = packed record
    BaseType: TPSBaseType;
  end;

  TPSProc = packed record
    Flags: Byte;
  end;

  TPSVar = packed record
    TypeNo: Cardinal;
    Flags: Byte;
  end;

  PSpecialProc = ^TSpecialProc;
  TSpecialProc = record
    P: TPSOnSpecialProcImport;
    namehash: Longint;
    Name: TbtString;
    tag: pointer;
  end;

//==============================================================================
//
// TPSExceptionHandler.Destroy
//
//==============================================================================
destructor TPSExceptionHandler.Destroy;
begin
  ExceptionObject.Free;
  inherited;
end;

//==============================================================================
//
// P_CM_A
//
//==============================================================================
procedure P_CM_A; begin end;

//==============================================================================
//
// P_CM_CA
//
//==============================================================================
procedure P_CM_CA; begin end;

//==============================================================================
//
// P_CM_P
//
//==============================================================================
procedure P_CM_P; begin end;

//==============================================================================
//
// P_CM_PV
//
//==============================================================================
procedure P_CM_PV; begin end;

//==============================================================================
//
// P_CM_PO
//
//==============================================================================
procedure P_CM_PO; begin end;

//==============================================================================
//
// P_CM_C
//
//==============================================================================
procedure P_CM_C; begin end;

//==============================================================================
//
// P_CM_G
//
//==============================================================================
procedure P_CM_G; begin end;

//==============================================================================
//
// P_CM_CG
//
//==============================================================================
procedure P_CM_CG; begin end;

//==============================================================================
//
// P_CM_CNG
//
//==============================================================================
procedure P_CM_CNG; begin end;

//==============================================================================
//
// P_CM_R
//
//==============================================================================
procedure P_CM_R; begin end;

//==============================================================================
//
// P_CM_ST
//
//==============================================================================
procedure P_CM_ST; begin end;

//==============================================================================
//
// P_CM_PT
//
//==============================================================================
procedure P_CM_PT; begin end;

//==============================================================================
//
// P_CM_CO
//
//==============================================================================
procedure P_CM_CO; begin end;

//==============================================================================
//
// P_CM_CV
//
//==============================================================================
procedure P_CM_CV; begin end;

//==============================================================================
//
// P_CM_SP
//
//==============================================================================
procedure P_CM_SP; begin end;

//==============================================================================
//
// P_CM_BN
//
//==============================================================================
procedure P_CM_BN; begin end;

//==============================================================================
//
// P_CM_VM
//
//==============================================================================
procedure P_CM_VM; begin end;

//==============================================================================
//
// P_CM_SF
//
//==============================================================================
procedure P_CM_SF; begin end;

//==============================================================================
//
// P_CM_FG
//
//==============================================================================
procedure P_CM_FG; begin end;

//==============================================================================
//
// P_CM_PUEXH
//
//==============================================================================
procedure P_CM_PUEXH; begin end;

//==============================================================================
//
// P_CM_POEXH
//
//==============================================================================
procedure P_CM_POEXH; begin end;

//==============================================================================
//
// P_CM_IN
//
//==============================================================================
procedure P_CM_IN; begin end;

//==============================================================================
//
// P_CM_SPB
//
//==============================================================================
procedure P_CM_SPB; begin end;

//==============================================================================
//
// P_CM_INC
//
//==============================================================================
procedure P_CM_INC; begin end;

//==============================================================================
//
// P_CM_DEC
//
//==============================================================================
procedure P_CM_DEC; begin end;

//==============================================================================
//
// IntPIFVariantToVariant
//
//==============================================================================
function IntPIFVariantToVariant(Src: pointer; aType: TPSTypeRec; var Dest: Variant): Boolean; forward;

//==============================================================================
//
// Set_Union
//
//==============================================================================
procedure Set_Union(Dest, Src: PByteArray; ByteSize: Integer);
var
  i: Longint;
begin
  for i := ByteSize - 1 downto 0 do
    Dest^[i] := Dest^[i] or Src^[i];
end;

//==============================================================================
//
// Set_Diff
//
//==============================================================================
procedure Set_Diff(Dest, Src: PByteArray; ByteSize: Integer);
var
  i: Longint;
begin
  for i := ByteSize - 1 downto 0 do
    Dest^[i] := Dest^[i] and not Src^[i];
end;

//==============================================================================
//
// Set_Intersect
//
//==============================================================================
procedure Set_Intersect(Dest, Src: PByteArray; ByteSize: Integer);
var
  i: Longint;
begin
  for i := ByteSize - 1 downto 0 do
    Dest^[i] := Dest^[i] and Src^[i];
end;

//==============================================================================
//
// Set_Subset
//
//==============================================================================
procedure Set_Subset(Dest, Src: PByteArray; ByteSize: Integer; var Val: Boolean);
var
  i: Integer;
begin
  for i := ByteSize - 1 downto 0 do
  begin
    if not (Src^[i] and Dest^[i] = Dest^[i]) then
    begin
      Val := False;
      Exit;
    end;
  end;
  Val := True;
end;

//==============================================================================
//
// Set_Equal
//
//==============================================================================
procedure Set_Equal(Dest, Src: PByteArray; ByteSize: Integer; var Val: Boolean);
var
  i: Longint;
begin
  for i := ByteSize - 1 downto 0 do
  begin
    if Dest^[i] <> Src^[i] then
    begin
      Val := False;
      Exit;
    end;
  end;
  val := True;
end;

//==============================================================================
//
// Set_membership
//
//==============================================================================
procedure Set_membership(Item: Longint; Src: PByteArray; var Val: Boolean);
begin
  Val := (Src^[Item shr 3] and (1 shl (Item and 7))) <> 0;
end;

//==============================================================================
//
// RCIFreeProc
//
//==============================================================================
procedure RCIFreeProc(Sender: TPSExec; P: TPSRuntimeClassImporter);
begin
  p.Free;
end;

//==============================================================================
//
// Trim
//
//==============================================================================
function Trim(const s: TbtString): TbtString;
begin
  Result := s;
  while (Length(Result) > 0) and (Result[1] = #32) do
    Delete(Result, 1, 1);
  while (Length(Result) > 0) and (Result[Length(Result)] = #32) do
    Delete(Result, Length(Result), 1);
end;

//==============================================================================
//
// Padl
//
//==============================================================================
function Padl(s: TbtString; i: Longint): TbtString;
begin
  Result := StringOfChar(TbtChar(' '), i - Length(s)) + s;
end;

//==============================================================================
//
// Padz
//
//==============================================================================
function Padz(s: TbtString; i: Longint): TbtString;
begin
  Result := StringOfChar(TbtChar('0'), i - Length(s)) + s;
end;

//==============================================================================
//
// Padr
//
//==============================================================================
function Padr(s: TbtString; i: Longint): TbtString;
begin
  Result := s + StringOfChar(TbtChar(' '), i - Length(s));
end;
//-------------------------------------------------------------------

{$IFNDEF PS_NOWIDESTRING}

//==============================================================================
//
// wPadl
//
//==============================================================================
function wPadl(s: TbtWideString; i: Longint): TbtWideString;
begin
  Result := StringOfChar(TbtWideChar(' '), i - Length(s)) + s;
end;

//==============================================================================
// wPadz
//
//-------------------------------------------------------------------
//
//==============================================================================
function wPadz(s: TbtWideString; i: Longint): TbtWideString;
begin
  Result := StringOfChar(TbtWideChar('0'), i - Length(s)) + s;
end;

//==============================================================================
// wPadr
//
//-------------------------------------------------------------------
//
//==============================================================================
function wPadr(s: TbtWideString; i: Longint): TbtWideString;
begin
  Result := s + StringOfChar(TbtWideChar(' '), i - Length(s));
end;

//==============================================================================
//
// uPadl
//
//==============================================================================
function uPadl(s: TbtUnicodeString; i: Longint): TbtUnicodeString;
begin
  Result := StringOfChar(TbtWideChar(' '), i - Length(s)) + s;
end;

//==============================================================================
// uPadz
//
//-------------------------------------------------------------------
//
//==============================================================================
function uPadz(s: TbtUnicodeString; i: Longint): TbtUnicodeString;
begin
  Result := StringOfChar(TbtWideChar('0'), i - Length(s)) + s;
end;

//==============================================================================
// uPadr
//
//-------------------------------------------------------------------
//
//==============================================================================
function uPadr(s: TbtUnicodeString; i: Longint): TbtUnicodeString;
begin
  Result := s + StringOfChar(TbtWideChar(' '), i - Length(s));
end;

{$ENDIF}
{$IFNDEF PS_NOWIDESTRING}

//==============================================================================
//
// MakeWString
//
//==============================================================================
function MakeWString(const s: TbtUnicodeString): TbtString;
var
  i: Longint;
  e: TbtString;
  b: boolean;
begin
  Result := TbtString(s);
  i := 1;
  b := False;
  while i <= Length(Result) do
  begin
    if Result[i] = '''' then
    begin
      if not b then
      begin
        b := True;
        Insert('''', Result, i);
        Inc(i);
      end;
      Insert('''', Result, i);
      Inc(i, 2);
    end
    else if (Result[i] < #32) or (Result[i] > #255) then
    begin
      e := '#' + IntToStr(ord(Result[i]));
      Delete(Result, i, 1);
      if b then
      begin
        b := False;
        Insert('''', Result, i);
        Inc(i);
      end;
      Insert(e, Result, i);
      Inc(i, Length(e));
    end
    else
    begin
      if not b then
      begin
        b := True;
        Insert('''', Result, i);
        Inc(i, 2);
      end
      else
        Inc(i);
    end;
  end;
  if b then
  begin
    Result := Result + '''';
  end;
  if Result = '' then
    Result := '''''';
end;
{$ENDIF}

//==============================================================================
//
// MakeString
//
//==============================================================================
function MakeString(const s: TbtString): TbtString;
var
  i: Longint;
  e: TbtString;
  b: boolean;
begin
  Result := s;
  i := 1;
  b := False;
  while i <= Length(Result) do
  begin
    if Result[i] = '''' then
    begin
      if not b then
      begin
        b := True;
        Insert('''', Result, i);
        Inc(i);
      end;
      Insert('''', Result, i);
      Inc(i, 2);
    end
    else if (Result[i] < #32) then
    begin
      e := '#' + IntToStr(ord(Result[i]));
      Delete(Result, i, 1);
      if b then
      begin
        b := False;
        Insert('''', Result, i);
        Inc(i);
      end;
      Insert(e, Result, i);
      Inc(i, Length(e));
    end
    else
    begin
      if not b then
      begin
        b := True;
        Insert('''', Result, i);
        Inc(i, 2);
      end
      else
        Inc(i);
    end;
  end;
  if b then
  begin
    Result := Result + '''';
  end;
  if Result = '' then
    Result := '''''';
end;

//==============================================================================
//
// SafeStr
//
//==============================================================================
function SafeStr(const s: TbtString): TbtString;
var
  i: Longint;
begin
  Result := s;
  for i := 1 to Length(s) do
  begin
    if s[i] in [#0..#31] then
    begin
      Result := Copy(s, 1, i - 1);
      Exit;
    end;
  end;

end;

//==============================================================================
//
// PropertyToString
//
//==============================================================================
function PropertyToString(Instance: TObject; PName: TbtString): TbtString;
var
  s: TbtString;
  i: Longint;
  PP: PPropInfo;
begin
  if PName = '' then
  begin
    Result := TbtString(Instance.ClassName);
    Exit;
  end;
  while Length(PName) > 0 do
  begin
    i := Pos(TbtChar('.'), PName);
    if i = 0 then
    begin
      s := Trim(PName);
      PName := '';
    end
    else
    begin
      s := trim(Copy(PName, 1, i - 1));
      Delete(PName, 1, i);
    end;
    pp := GetPropInfo(PTypeInfo(Instance.ClassInfo), string(s));
    if pp = nil then
    begin
      Result := TbtString(RPS_UnknownIdentifier);
      Exit;
    end;

    case pp^.PropType^.Kind of
      tkInteger:
        begin
          Result := IntToStr(GetOrdProp(Instance, pp));
          Exit;
        end;
      tkChar:
        begin
          Result := '#' + IntToStr(GetOrdProp(Instance, pp));
          Exit;
        end;
      tkEnumeration:
        begin
          Result := TbtString(GetEnumName(pp^.PropType{$IFNDEF FPC}{$IFDEF DELPHI3UP}^{$ENDIF}{$ENDIF}, GetOrdProp(Instance, pp)));
          Exit;
        end;
      tkFloat:
        begin
          Result := FloatToStr(GetFloatProp(Instance, PP));
          Exit;
        end;
      tkString, tkLString:
        begin
          Result := '''' + TbtString(GetStrProp(Instance, PP)) + '''';
          Exit;
        end;
      tkSet:
        begin
          Result := '[Set]';
          Exit;
        end;
      tkClass:
        begin
          Instance := TObject(GetOrdProp(Instance, pp));
        end;
      tkMethod:
        begin
          Result := '[Method]';
          Exit;
        end;
      tkVariant:
        begin
          Result := '[Variant]';
          Exit;
        end;
     {$IFDEF DELPHI6UP}
     {$IFNDEF PS_NOWIDESTRING}
      tkWString:
        begin
          Result := '''' + TbtString(GetWideStrProp(Instance, pp)) + '';
        end;
      {$IFDEF DELPHI2009UP}
      tkUString:
        begin
          Result := '''' + TbtString(GetUnicodeStrProp(Instance, pp)) + '';
        end;
      {$ENDIF}
      {$ENDIF}
      {$ENDIF}
      else
        begin
          Result := '[Unknown]';
          Exit;
        end;
    end;
    if Instance = nil then
    begin
      Result := 'nil';
      Exit;
    end;
  end;
  Result := TbtString(Instance.ClassName);
end;

//==============================================================================
//
// ClassVariantInfo
//
//==============================================================================
function ClassVariantInfo(const pvar: TPSVariantIFC; const PropertyName: TbtString): TbtString;
begin
  if pvar.aType.BaseType = btClass then
  begin
    if TObject(pvar.Dta^) = nil then
      Result := 'nil'
    else
      Result := PropertyToString(TObject(pvar.Dta^), PropertyName);
  end
  else if pvar.atype.basetype = btInterface then
    Result := 'Interface'
  else
    Result := TbtString(RPS_InvalidType);
end;

//==============================================================================
//
// PSVariantToString
//
//==============================================================================
function PSVariantToString(const p: TPSVariantIFC; const ClassProperties: TbtString): TbtString;
var
  i, n: Longint;
begin
  ThousandSeparator := #0;
  DecimalSeparator := '.';

  if p.Dta = nil then
  begin
    Result := 'nil';
    Exit;
  end;
  if (p.aType.BaseType = btVariant) then
  begin
    try
      if TVarData(p.Dta^).VType = varDispatch then
        Result := 'Variant(IDispatch)'
      else if TVarData(p.Dta^).VType = varNull then
        REsult := 'Null'
      else if (TVarData(p.Dta^).VType = varOleStr) then
        {$IFDEF PS_NOWIDESTRING}
        Result := MakeString(Variant(p.Dta^))
        {$ELSE}
        Result := MakeWString(variant(p.dta^))
        {$ENDIF}
      else if TVarData(p.Dta^).VType = varString then
        Result := MakeString(TbtString(variant(p.Dta^)))
      else
      Result := TbtString(Variant(p.Dta^));
    except
      on e: Exception do
        Result := TbtString(Format (RPS_Exception, [e.Message]));
    end;
    Exit;
  end;
  case p.aType.BaseType of
    btProcptr:
      begin
        Result := 'Proc: ' +
        IntToStr(TbtU32(p.Dta^));
      end;
    btU8:
      str(TbtU8(p.dta^), Result);
    btS8:
      str(TbtS8(p.dta^), Result);
    btU16:
      str(TbtU16(p.dta^), Result);
    btS16:
      str(TbtS16(p.dta^), Result);
    btU32:
      str(TbtU32(p.dta^), Result);
    btS32:
      str(TbtS32(p.dta^), Result);
    btSingle:
      str(TbtSingle(p.dta^), Result);
    btDouble:
      str(TbtDouble(p.dta^), Result);
    btExtended:
      str(TbtExtended(p.dta^), Result);
    btString:
      Result := makestring(TbtString(p.dta^));
    btPChar:
      begin
        if PAnsiChar(p.dta^) = nil then
          Result := 'nil'
        else
          Result := MakeString(PAnsiChar(p.dta^));
      end;
    btChar:
      Result := MakeString(TbtChar(p.dta^));
    {$IFNDEF PS_NOWIDESTRING}
    btwidechar:
      Result := MakeWString(TbtWideChar(p.dta^));
    btWideString:
      Result := MakeWString(TbtWideString(p.dta^));
    btUnicodeString:
      Result := MakeWString(TbtUnicodeString(p.dta^));
    {$ENDIF}
    {$IFNDEF PS_NOINT64}
    btS64:
      str(TbtS64(p.dta^), Result);
    {$ENDIF}
    btStaticArray,
    btArray:
      begin
        Result := '';
        if p.aType.BaseType = btStaticArray then
          n := TPSTypeRec_StaticArray(p.aType).Size
        else
          n := PSDynArrayGetLength(Pointer(p.dta^), p.aType);
        for i := 0 to n - 1 do
        begin
          if Result <> '' then
            Result := Result + ', ';
          Result := Result + PSVariantToString(PSGetArrayField(p, i), '');
        end;
        Result := '[' + Result + ']';
      end;
    btRecord:
      begin
        Result := '';
        n := TPSTypeRec_Record(p.aType).FFieldTypes.Count;
        for i := 0 to n - 1 do
        begin
          if Result <> '' then
            Result := Result + ', ';
          Result := Result + PSVariantToString(PSGetRecField(p, i), '');
        end;
        Result := '(' + Result + ')';
      end;
    btPointer:
      Result := 'Nil';
    btClass, btInterface:
      begin
        Result := ClassVariantInfo(p, ClassProperties)
      end;
  else
    Result := TbtString(RPS_Invalid);
  end;
end;

//==============================================================================
//
// TIFErrorToString
//
//==============================================================================
function TIFErrorToString(x: TPSError; const Param: TbtString): TbtString;
begin
  Result := PSErrorToString(x,param);
end;

//==============================================================================
//
// PSErrorToString
//
//==============================================================================
function PSErrorToString(x: TPSError; const Param: TbtString): TbtString;
begin
  case x of
    erNoError:
      Result := TbtString(RPS_NoError);
    erCannotImport:
      Result := TbtString(Format (RPS_CannotImport, [Safestr(Param)]));
    erInvalidType:
      Result := TbtString(RPS_InvalidType);
    erInternalError:
      Result := TbtString(RPS_InternalError);
    erInvalidHeader:
      Result := TbtString(RPS_InvalidHeader);
    erInvalidOpcode:
      Result := TbtString(RPS_InvalidOpcode);
    erInvalidOpcodeParameter:
      Result := TbtString(RPS_InvalidOpcodeParameter);
    erNoMainProc:
      Result := TbtString(RPS_NoMainProc);
    erOutOfGlobalVarsRange:
      Result := TbtString(RPS_OutOfGlobalVarsRange);
    erOutOfProcRange:
      Result := TbtString(RPS_OutOfProcRange);
    erOutOfRange:
      Result := TbtString(RPS_OutOfRange);
    erOutOfStackRange:
      Result := TbtString(RPS_OutOfStackRange);
    erTypeMismatch:
      Result := TbtString(RPS_TypeMismatch);
    erUnexpectedEof:
      Result := TbtString(RPS_UnexpectedEof);
    erVersionError:
      Result := TbtString(RPS_VersionError);
    erDivideByZero:
      Result := TbtString(RPS_DivideByZero);
    erMathError:
      Result := TbtString(RPS_MathError);
    erCouldNotCallProc:
      begin
        Result := TbtString(RPS_CouldNotCallProc);
        if (Param <> '') then
          Result := Result + ' (' + Param + ')';
      end;
    erOutofRecordRange:
      Result := TbtString(RPS_OutofRecordRange);
    erNullPointerException:
      Result := TbtString(RPS_NullPointerException);
    erNullVariantError:
      Result := TbtString(RPS_NullVariantError);
    erOutOfMemory:
      Result := TbtString(RPS_OutOfMemory);
    erException:
      Result := TbtString(Format (RPS_Exception, [Param]));
    erInterfaceNotSupported:
      Result := TbtString(RPS_InterfaceNotSupported);
    erCustomError:
      Result := Param;
  else
    Result := TbtString(RPS_UnknownError);
  end;
  //
end;

//==============================================================================
//
// TPSTypeRec.CalcSize
//
//==============================================================================
procedure TPSTypeRec.CalcSize;
begin
  case BaseType of
    btVariant:
      FRealSize := SizeOf(Variant);
    btChar,
    btS8,
    btU8:
      FrealSize := 1;
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar,
    {$ENDIF}
    btS16,
    btU16:
      FrealSize := 2;
    {$IFNDEF PS_NOWIDESTRING}
    btWideString,
    btUnicodeString,
    {$ENDIF}
    {$IFNDEF PS_NOINTERFACES}
    btInterface,
    {$ENDIF}
    btClass,
    btPChar,
    btString:
      FrealSize := PointerSize;
    btSingle,
    btS32,
    btU32:
      FRealSize := 4;
    btProcPtr:
      FRealSize := 2 * SizeOf(Pointer) + SizeOf(Cardinal);
    btCurrency:
      FrealSize := SizeOf(Currency);
    btPointer:
      FRealSize := 2 * SizeOf(Pointer) + SizeOf(LongBool); // ptr, type, freewhendone
    btDouble
    {$IFNDEF PS_NOINT64},
    bts64{$ENDIF}:
      FrealSize := 8;
    btExtended:
      FrealSize := SizeOf(Extended);
    btReturnAddress:
      FrealSize := SizeOf(TBTReturnAddress);
  else
    FrealSize := 0;
  end;
end;

//==============================================================================
//
// TPSTypeRec.Create
//
//==============================================================================
constructor TPSTypeRec.Create(Owner: TPSExec);
begin
  inherited Create;
  FAttributes := TPSRuntimeAttributes.Create(Owner);
end;

//==============================================================================
//
// TPSTypeRec.Destroy
//
//==============================================================================
destructor TPSTypeRec.Destroy;
begin
  FAttributes.Free;
  inherited destroy;
end;

{ TPSTypeRec_Record }

//==============================================================================
//
// TPSTypeRec_Record.CalcSize
//
//==============================================================================
procedure TPSTypeRec_Record.CalcSize;
begin
  inherited;
  FrealSize := TPSTypeRec(FFieldTypes[FFieldTypes.Count - 1]).RealSize +
    IPointer(RealFieldOffsets[RealFieldOffsets.Count - 1]);
end;

//==============================================================================
//
// TPSTypeRec_Record.Create
//
//==============================================================================
constructor TPSTypeRec_Record.Create(Owner: TPSExec);
begin
  inherited Create(Owner);
  FRealFieldOffsets := TPSList.Create;
  FFieldTypes := TPSList.Create;
end;

//==============================================================================
//
// TPSTypeRec_Record.Destroy
//
//==============================================================================
destructor TPSTypeRec_Record.Destroy;
begin
  FFieldTypes.Free;
  FRealFieldOffsets.Free;
  inherited Destroy;
end;

const
  RTTISize = SizeOf(TPSVariant);

//==============================================================================
//
// InitializeVariant
//
//==============================================================================
procedure InitializeVariant(p: Pointer; aType: TPSTypeRec);
var
  t: TPSTypeRec;
  i: Longint;
begin
  case aType.BaseType of
    btChar,
    btS8,
    btU8:
      TbtU8(p^) := 0;
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar,
    {$ENDIF}
    btS16,
    btU16:
      TbtU16(p^) := 0;
    btSingle:
      TbtSingle(P^) := 0;
    btS32,
    btU32:
      TbtU32(P^) := 0;
    btPChar,
    btString,
    {$IFNDEF PS_NOWIDESTRING}
    btUnicodeString,
    btWideString,
    {$ENDIF}
    btClass,
    btInterface,
    btArray:
      Pointer(P^) := nil;
    btPointer:
      begin
        Pointer(p^) := nil;
        Pointer(Pointer(IPointer(p) + PointerSize)^) := nil;
        Pointer(Pointer(IPointer(p) + (2 * PointerSize))^) := nil;
      end;
    btProcPtr:
      begin
        Longint(p^) := 0;
        Pointer(Pointer(IPointer(p) + PointerSize)^) := nil;
        Pointer(Pointer(IPointer(p) + (2 * PointerSize))^) := nil;
      end;
    btCurrency:
      TbtCurrency(P^) := 0;
    btDouble
    {$IFNDEF PS_NOINT64},
    bts64
    {$ENDIF}:
      {$IFNDEF PS_NOINT64}
      TbtS64(P^) := 0
      {$ELSE}
      TbtDouble(p^) := 0
      {$ENDIF};
    btExtended:
      TbtExtended(p^) := 0;
    btVariant:
      Initialize(Variant(p^));
    btReturnAddress:; // there is no point in initializing a return address
    btRecord:
      begin
        for i := 0 to TPSTypeRec_Record(aType).FFieldTypes.Count - 1 do
        begin
          t := TPSTypeRec_Record(aType).FieldTypes[i];
          InitializeVariant(P, t);
          p := Pointer(IPointer(p) + t.FrealSize);
        end;
      end;
    btStaticArray:
      begin
        t := TPSTypeRec_Array(aType).ArrayType;
        for i := 0 to TPSTypeRec_StaticArray(aType).Size - 1 do
        begin
          InitializeVariant(p, t);
          p := Pointer(IPointer(p) + t.RealSize);
        end;
      end;
    btSet:
      begin
        FillChar(p^, TPSTypeRec_Set(aType).RealSize, 0);
      end;
  end;
end;

//==============================================================================
//
// DestroyHeapVariant2
//
//==============================================================================
procedure DestroyHeapVariant2(v: Pointer; aType: TPSTypeRec); forward;

const
  NeedFinalization = [
    btStaticArray,
    btRecord,
    btArray,
    btPointer,
    btVariant,
    {$IFNDEF PS_NOINTERFACES}
    btInterface,
    {$ENDIF}
    btString
    {$IFNDEF PS_NOWIDESTRING},
    btUnicodeString,
    btWideString
    {$ENDIF}];

type
  TDynArrayRecHeader = packed record
    {$IFDEF FPC}
    refCnt: ptrint;
    high: tdynarrayindex;
    {$ELSE}
    {$IFDEF CPUX64}
    _Padding: Longint; // Delphi XE2+ expects 16 byte align
    {$ENDIF}
    /// dynamic array reference count (basic garbage memory mechanism)
    refCnt: Longint;
    /// length in element count
    // - size in bytes = length*ElemSize
    length: IPointer;
    {$ENDIF}
  end;

  TDynArrayRec = packed record
    header: TDynArrayRecHeader;
    datas: pointer;
  end;
  PDynArrayRec = ^TDynArrayRec;

//==============================================================================
//
// FinalizeVariant
//
//==============================================================================
procedure FinalizeVariant(p: Pointer; aType: TPSTypeRec);
var
  t: TPSTypeRec;
  elsize: Cardinal;
  i, l: Longint;
  darr: PDynArrayRec;
begin
  case aType.BaseType of
    btString:
      TbtString(p^) := '';
    {$IFNDEF PS_NOWIDESTRING}
    btWideString:
      TbtWideString(p^) := '';
    btUnicodeString:
      TbtUnicodeString(p^) := '';
    {$ENDIF}
    {$IFNDEF PS_NOINTERFACES}
    btInterface:
      begin
        {$IFNDEF DELPHI3UP}
        if IUnknown(p^) <> nil then
          IUnknown(p^).Release;
        {$ENDIF}
        IUnknown(p^) := nil;
      end;
    {$ENDIF}
    btVariant:
      begin
        try
          Finalize(Variant(p^));
        except
        end;
      end;
    btPointer:
      if Pointer(Pointer(IPointer(p) + PointerSize2)^) <> nil then
      begin
        DestroyHeapVariant2(Pointer(p^), Pointer(Pointer(IPointer(p) + PointerSize)^));
        Pointer(p^) := nil;
      end;
    btArray:
      begin
        if IPointer(P^) = 0 then
          Exit;
        darr := PDynArrayRec(IPointer(p^) - SizeOf(TDynArrayRecHeader));
        if darr^.header.refCnt < 0 then
          Exit;// refcount < 0 means don't free
        Dec(darr^.header.refCnt);
        if darr^.header.refCnt <> 0 then
          Exit;
        t := TPSTypeRec_Array(aType).ArrayType;
        elsize := t.RealSize;
        {$IFDEF FPC}
        l := darr^.header.high + 1;
        {$ELSE}
        l := darr^.header.length;
        {$ENDIF FPC}
        darr := @darr^.datas;
        case t.BaseType of
          btString,
          {$IFNDEF PS_NOWIDESTRING}
          btUnicodeString,
          btWideString,
          {$ENDIF}
          {$IFNDEF PS_NOINTERFACES}
          btInterface,
          {$ENDIF}
          btArray,
          btStaticArray,
          btRecord,
          btPointer,
          btVariant:
            begin
              for i := 0 to l - 1 do
              begin
                FinalizeVariant(darr, t);
                darr := Pointer(IPointer(darr) + elsize);
              end;
            end;
        end;
        FreeMem(Pointer(IPointer(p^) - SizeOf(TDynArrayRecHeader)), IPointer(Cardinal(l) * elsize) + SizeOf(TDynArrayRecHeader));
        Pointer(P^) := nil;
      end;
    btRecord:
      begin
        for i := 0 to TPSTypeRec_Record(aType).FFieldTypes.Count - 1 do
        begin
          t := TPSTypeRec_Record(aType).FieldTypes[i];
          case t.BaseType of
            btString,
            {$IFNDEF PS_NOWIDESTRING}
            btUnicodeString,
            btWideString,
            {$ENDIF}
            {$IFNDEF PS_NOINTERFACES}
            btInterface,
            {$ENDIF}
            btArray,
            btStaticArray,
            btRecord:
              FinalizeVariant(p, t);
          end;
          p := Pointer(IPointer(p) + t.FrealSize);
        end;
      end;
    btStaticArray:
      begin
        t := TPSTypeRec_Array(aType).ArrayType;
        case t.BaseType of
          btString,
          {$IFNDEF PS_NOWIDESTRING}
          btUnicodeString,
          btWideString,
          {$ENDIF}
          {$IFNDEF PS_NOINTERFACES}
          btInterface,
          {$ENDIF}
          btArray,
          btStaticArray,
          btRecord: ;
          else
            Exit;
        end;
        for i := 0 to TPSTypeRec_StaticArray(aType).Size - 1 do
        begin
          FinalizeVariant(p, t);
          p := Pointer(IPointer(p) + t.RealSize);
        end;
      end;
  end;
end;

//==============================================================================
//
// CreateHeapVariant2
//
//==============================================================================
function CreateHeapVariant2(aType: TPSTypeRec): Pointer;
begin
  GetMem(Result, aType.RealSize);
  InitializeVariant(Result, aType);
end;

//==============================================================================
//
// DestroyHeapVariant2
//
//==============================================================================
procedure DestroyHeapVariant2(v: Pointer; aType: TPSTypeRec);
begin
  if v = nil then
    Exit;
  if atype.BaseType in NeedFinalization then
    FinalizeVariant(v, aType);
  FreeMem(v, aType.RealSize);
end;

//==============================================================================
//
// CreateHeapVariant
//
//==============================================================================
function CreateHeapVariant(aType: TPSTypeRec): PPSVariant;
var
  aSize: Longint;
begin
  aSize := aType.RealSize + RTTISize;
  GetMem(Result, aSize);
  Result.FType := aType;
  InitializeVariant(Pointer(IPointer(Result) + PointerSize), aType);
end;

//==============================================================================
//
// DestroyHeapVariant
//
//==============================================================================
procedure DestroyHeapVariant(v: PPSVariant);
begin
  if v = nil then
    Exit;
  if v.FType.BaseType in NeedFinalization then
    FinalizeVariant(Pointer(IPointer(v) + PointerSize), v.FType);
  FreeMem(v, v.FType.RealSize + RTTISize);
end;

//==============================================================================
//
// FreePSVariantList
//
//==============================================================================
procedure FreePSVariantList(l: TPSList);
var
  i: Longint;
begin
  for i := l.Count - 1 downto 0 do
    DestroyHeapVariant(l[i]);
  l.free;
end;

//==============================================================================
//
// FreePIFVariantList
//
//==============================================================================
procedure FreePIFVariantList(l: TPSList);
begin
  FreePsVariantList(l);
end;

{ TPSExec }

//==============================================================================
//
// TPSExec.ClearFunctionList
//
//==============================================================================
procedure TPSExec.ClearFunctionList;
var
  x: PProcRec;
  l: Longint;
begin
  for l := FAttributeTypes.Count - 1 downto 0 do
  begin
    TPSAttributeType(FAttributeTypes.Data^[l]).Free;
  end;
  FAttributeTypes.Clear;

  for l := 0 to FRegProcs.Count - 1 do
  begin
    x := FRegProcs.Data^[l];
    if @x^.FreeProc <> nil then
      x^.FreeProc(Self, x);
    Dispose(x);
  end;
  FRegProcs.Clear;
  RegisterStandardProcs;
end;

class function TPSExec.About: TbtString;
begin
  Result := 'RemObjects Pascal Script. Copyright (c) 2004-2010 by RemObjects Software';
end;

//==============================================================================
//
// TPSExec.Cleanup
//
//==============================================================================
procedure TPSExec.Cleanup;
var
  I: Longint;
  p: Pointer;
begin
  if FStatus <> isLoaded then
    Exit;
  FStack.Clear;
  FTempVars.Clear;
  for I := Longint(FGlobalVars.Count) - 1 downto 0 do
  begin
    p := FGlobalVars.Items[i];
    if PIFTypeRec(P^).BaseType in NeedFinalization then
      FinalizeVariant(Pointer(IPointer(p) + PointerSize), Pointer(P^));
    InitializeVariant(Pointer(IPointer(p) + PointerSize), Pointer(P^));
  end;
end;

//==============================================================================
//
// TPSExec.Clear
//
//==============================================================================
procedure TPSExec.Clear;
var
  I: Longint;
  temp: PPSResource;
  Proc: TPSResourceFreeProc;
  pp: TPSExceptionHandler;
begin
  for i := Longint(FExceptionStack.Count) - 1 downto 0 do
  begin
    pp := FExceptionStack.Data^[i];
    pp.Free;
  end;
  for i := Longint(FResources.Count) - 1 downto 0 do
  begin
    Temp := FResources.Data^[i];
    Proc := Temp^.Proc;
    Proc(Self, Temp^.P);
    Dispose(Temp);
  end;
  for i := Longint(FExportedVars.Count) - 1 downto 0 do
    Dispose(PPSExportedVar(FExportedVars.Data^[I]));
  for I := Longint(FProcs.Count) - 1 downto 0  do
    TPSProcRec(FProcs.Data^[i]).Destroy;
  FProcs.Clear;
  FGlobalVars.Clear;
  FStack.Clear;
  for I := Longint(FTypes.Count) - 1 downto 0  do
    TPSTypeRec(FTypes.Data^[i]).Free;
  FTypes.Clear;
  FStatus := isNotLoaded;
  FResources.Clear;
  FExportedVars.Clear;
  FExceptionStack.Clear;
  FCurrStackBase := InvalidVal;
end;

//==============================================================================
//
// TPSExec.Create
//
//==============================================================================
constructor TPSExec.Create;
begin
  inherited Create;
  FAttributeTypes := TPSList.Create;
  FExceptionStack := TPSList.Create;
  FCallCleanup := False;
  FResources := TPSList.Create;
  FTypes := TPSList.Create;
  FProcs := TPSList.Create;
  FGlobalVars := TPSStack.Create;
  FTempVars := TPSStack.Create;
  FMainProc := 0;
  FStatus := isNotLoaded;
  FRegProcs := TPSList.Create;
  FExportedVars := TPSList.create;
  FSpecialProcList := TPSList.Create;
  RegisterStandardProcs;
  FReturnAddressType := TPSTypeRec.Create(Self);
  FReturnAddressType.BaseType := btReturnAddress;
  FReturnAddressType.CalcSize;
  FVariantType := TPSTypeRec.Create(Self);
  FVariantType.BaseType := btVariant;
  FVariantType.CalcSize;
  FVariantArrayType := TPSTypeRec_Array.Create(Self);
  FVariantArrayType.BaseType := btArray;
  FVariantArrayType.CalcSize;
  TPSTypeRec_Array(FVariantArrayType).ArrayType := FVariantType;
  FStack := TPSStack.Create;
  FAllowNullClasses := False;
end;

//==============================================================================
//
// TPSExec.Destroy
//
//==============================================================================
destructor TPSExec.Destroy;
var
  I: Longint;
  x: PProcRec;
  P: PSpecialProc;
begin
  Clear;
  FReturnAddressType.Free;
  FVariantType.Free;
  FVariantArrayType.Free;

  if ExObject <> nil then ExObject.Free;
  for I := FSpecialProcList.Count - 1 downto 0 do
  begin
    P := FSpecialProcList.Data^[I];
    Dispose(p);
  end;
  FResources.Free;
  FExportedVars.Free;
  FTempVars.Free;
  FStack.Free;
  FGlobalVars.Free;
  FProcs.Free;
  FTypes.Free;
  FSpecialProcList.Free;
  for i := FRegProcs.Count - 1 downto 0 do
  begin
    x := FRegProcs.Data^[i];
    if @x^.FreeProc <> nil then
      x^.FreeProc(Self, x);
    Dispose(x);
  end;
  FRegProcs.Free;
  FExceptionStack.Free;
  for i := FAttributeTypes.Count - 1 downto 0 do
    TPSAttributeType(FAttributeTypes[i]).Free;
  FAttributeTypes.Free;
  inherited Destroy;
end;

//==============================================================================
//
// TPSExec.ExceptionProc
//
//==============================================================================
procedure TPSExec.ExceptionProc(proc, Position: Cardinal; Ex: TPSError; const s: TbtString; NewObject: TObject);
var
  d, l: Longint;
  pp: TPSExceptionHandler;
begin
  ExProc := proc;
  ExPos := Position;
  ExEx := Ex;
  ExParam := s;
  if ExObject <> nil then
    ExObject.Free;
  ExObject := NewObject;
  if Ex = eNoError then
    Exit;
  for d := FExceptionStack.Count - 1 downto 0 do
  begin
    pp := FExceptionStack[d];
    if Cardinal(FStack.Count) > pp.StackSize then
    begin
      for l := Longint(FStack.count) - 1 downto Longint(pp.StackSize) do
        FStack.Pop;
    end;
    if pp.CurrProc = nil then // no point in continuing
    begin
      pp.Free;
      FExceptionStack.DeleteLast;

      FCurrStackBase := InvalidVal;
      FStatus := isPaused;
      Exit;
    end;
    FCurrProc := pp.CurrProc;
    FData := FCurrProc.Data;
    FDataLength := FCurrProc.Length;

    FCurrStackBase := pp.BasePtr;
    if pp.FinallyOffset <> InvalidVal then
    begin
      FCurrentPosition := pp.FinallyOffset;
      pp.FinallyOffset := InvalidVal;
      Exit;
    end
    else if (pp.ExceptOffset <> InvalidVal) and (pp.ExceptOffset <> Cardinal(InvalidVal - 1)) then
    begin
      FCurrentPosition := pp.ExceptOffset;
      pp.ExceptOffset := Cardinal(InvalidVal - 1);
      pp.ExceptionObject := ExObject;
      pp.ExceptionData := ExEx;
      pp.ExceptionParam := ExParam;
      ExObject := nil;
      ExEx := ENoError;
      Exit;
    end
    else if pp.Finally2Offset <> InvalidVal then
    begin
      FCurrentPosition := pp.Finally2Offset;
      pp.Finally2Offset := InvalidVal;
      Exit;
    end;
    pp.Free;
    FExceptionStack.DeleteLast;
  end;
  if FStatus <> isNotLoaded then
    FStatus := isPaused;
end;

//==============================================================================
//
// LookupProc
//
//==============================================================================
function LookupProc(List: TPSList; const Name: ShortString): PProcRec;
var
  h, l: Longint;
  p: PProcRec;
begin
  h := MakeHash(Name);
  for l := List.Count - 1 downto 0 do
  begin
    p := List.Data^[l];
    if (p^.Hash = h) and (p^.Name = Name) then
    begin
      Result := List[l];
      Exit;
    end;
  end;
  Result := nil;
end;

//==============================================================================
//
// TPSExec.ImportProc
//
//==============================================================================
function TPSExec.ImportProc(const Name: ShortString; proc: TPSExternalProcRec): Boolean;
var
  u: PProcRec;
  fname: TbtString;
  I, fnh: Longint;
  P: PSpecialProc;
begin
  if name = '' then
  begin
    fname := proc.Decl;
    fname := Copy(fname, 1, Pos(TbtChar(':'), fname) - 1);
    fnh := MakeHash(fname);
    for I := FSpecialProcList.Count - 1 downto 0 do
    begin
      p := FSpecialProcList[I];
      IF (p^.name = '') or ((p^.namehash = fnh) and (p^.name = fname)) then
      begin
        if p^.P(Self, Proc, p^.tag) then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
    Result := False;
    Exit;
  end;
  u := LookupProc(FRegProcs, Name);
  if u = nil then
  begin
    Result := False;
    Exit;
  end;
  proc.ProcPtr := u^.ProcPtr;
  proc.Ext1 := u^.Ext1;
  proc.Ext2 := u^.Ext2;
  Result := True;
end;

//==============================================================================
//
// TPSExec.RegisterFunctionName
//
//==============================================================================
function TPSExec.RegisterFunctionName(const Name: TbtString; ProcPtr: TPSProcPtr; Ext1, Ext2: Pointer): PProcRec;
var
  p: PProcRec;
  s: TbtString;
begin
  s := FastUpperCase(Name);
  New(p);
  p^.Name := s;
  p^.Hash := MakeHash(s);
  p^.ProcPtr := ProcPtr;
  p^.FreeProc := nil;
  p^.Ext1 := Ext1;
  p^.Ext2 := Ext2;
  FRegProcs.Add(p);
  Result := P;
end;

//==============================================================================
//
// TPSExec.LoadData
//
//==============================================================================
function TPSExec.LoadData(const s: TbtString): Boolean;
var
  HDR: TPSHeader;
  Pos: Cardinal;

  function DoRead(var Data; Len: Cardinal): Boolean;
  begin
    if Longint(Pos + Len) <= Length(s) then
    begin
      Move(s[Pos + 1], Data, Len);
      Pos := Pos + Len;
      Result := True;
    end
    else
      Result := False;
  end;

  function ReadAttributes(Dest: TPSRuntimeAttributes): Boolean;
  var
    Count: Cardinal;
    i: Integer;

    function ReadAttrib: Boolean;
    var
      NameLen: Longint;
      Name: TbtString;
      TypeNo: Cardinal;
      i, h, FieldCount: Longint;
      att: TPSRuntimeAttribute;
      varp: PIFVariant;
    begin
      if (not DoRead(NameLen, 4)) or (NameLen > Length(s) - Longint(Pos)) then
      begin
        CMD_Err(erOutOfRange);
        Result := False;
        Exit;
      end;
      SetLength(Name, NameLen);
      if not DoRead(Name[1], NameLen) then
      begin
        CMD_Err(erOutOfRange);
        Result := False;
        Exit;
      end;
      if not DoRead(FieldCount, 4) then
      begin
        CMD_Err(erOutOfRange);
        Result := False;
        Exit;
      end;
      att := Dest.Add;
      att.AttribType := Name;
      att.AttribTypeHash := MakeHash(att.AttribType);
      for i := 0 to FieldCount - 1 do
      begin
        if (not DoRead(TypeNo, 4)) or (TypeNo >= Cardinal(FTypes.Count)) then
        begin
          CMD_Err(erOutOfRange);
          Result := False;
          Exit;
        end;

        varp := att.AddValue(FTypes[TypeNo]);
        case VarP^.FType.BaseType of
          btSet:
            begin
              if not DoRead(PPSVariantSet(varp).Data, TPSTypeRec_Set(varp.FType).aByteSize) then
              begin
                CMD_Err(erOutOfRange);

                DestroyHeapVariant(VarP);
                Result := False;
                Exit;
              end;
            end;
          btS8,
          btChar,
          btU8:
            if not DoRead(PPSVariantU8(VarP)^.data, 1) then
            begin
              CMD_Err(erOutOfRange);
              DestroyHeapVariant(VarP);
              Result := False;
              Exit;
            end;
          btS16,
          {$IFNDEF PS_NOWIDESTRING}
          btwidechar,
          {$ENDIF}
          btU16:
            if not DoRead(PPSVariantU16(Varp)^.Data, SizeOf(TbtU16)) then
            begin
              CMD_Err(erOutOfRange);
              DestroyHeapVariant(VarP);
              Result := False;
              Exit;
            end;
          btS32,
          btU32:
            begin
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err(erOutOfRange);
                DestroyHeapVariant(VarP);
                Result := False;
                Exit;
              end;
              {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
              PPSVariantU32(varp)^.Data := unaligned(Cardinal((@FData^[FCurrentPosition])^));
              {$ELSE}
              PPSVariantU32(varp)^.Data := Cardinal((@FData^[FCurrentPosition])^);
              {$ENDIF}
              Inc(FCurrentPosition, 4);
            end;
          btProcPtr:
            begin
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err(erOutOfRange);
                DestroyHeapVariant(VarP);
                Result := False;
                Exit;
              end;
              {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
              PPSVariantU32(varp)^.Data := unaligned(Cardinal((@FData^[FCurrentPosition])^));
              {$ELSE}
              PPSVariantU32(varp)^.Data := Cardinal((@FData^[FCurrentPosition])^);
              {$ENDIF}
              if PPSVariantU32(varp)^.Data = 0 then
              begin
                PPSVariantProcPtr(varp)^.Ptr := nil;
                PPSVariantProcPtr(varp)^.Self := nil;
              end;
              Inc(FCurrentPosition, 4);
            end;
          {$IFNDEF PS_NOINT64}
          bts64:
            if not DoRead(PPSVariantS64(VarP)^.Data, SizeOf(TbtS64)) then
            begin
              CMD_Err(erOutOfRange);
              DestroyHeapVariant(VarP);
              Result := False;
              Exit;
            end;
          {$ENDIF}
          btSingle:
            if not DoRead(PPSVariantSingle(VarP)^.Data, SizeOf(TbtSingle)) then
            begin
              CMD_Err(erOutOfRange);
              DestroyHeapVariant(VarP);
              Result := False;
              Exit;
            end;
          btDouble:
            if not DoRead(PPSVariantDouble(varp)^.Data, SizeOf(TbtDouble)) then
            begin
              CMD_Err(erOutOfRange);
              DestroyHeapVariant(VarP);
              Result := False;
              Exit;
            end;
          btExtended:
            if not DoRead(PPSVariantExtended(varp)^.Data, SizeOf(TbtExtended)) then
            begin
              CMD_Err(erOutOfRange);
              DestroyHeapVariant(VarP);
              Result := False;
              Exit;
            end;
          btCurrency:
            if not DoRead(PPSVariantExtended(varp)^.Data, SizeOf(TbtCurrency)) then
            begin
              CMD_Err(erOutOfRange);
              DestroyHeapVariant(VarP);
              Result := False;
              Exit;
            end;
          btPchar,
          btString:
            begin
              if not DoRead(NameLen, 4) then
              begin
                CMD_Err(erOutOfRange);
                DestroyHeapVariant(VarP);
                Result := False;
                Exit;
              end;
              Inc(FCurrentPosition, 4);
              SetLength(PPSVariantAString(varp)^.Data, NameLen);
              if not DoRead(PPSVariantAString(varp)^.Data[1], NameLen) then
              begin
                CMD_Err(erOutOfRange);
                DestroyHeapVariant(VarP);
                Result := False;
                Exit;
              end;
            end;
          {$IFNDEF PS_NOWIDESTRING}
          btWideString:
            begin
              if not DoRead(NameLen, 4) then
              begin
                CMD_Err(erOutOfRange);
                DestroyHeapVariant(VarP);
                Result := False;
                Exit;
              end;
              Inc(FCurrentPosition, 4);
              SetLength(PPSVariantWString(varp).Data, NameLen);
              if not DoRead(PPSVariantWString(varp).Data[1], NameLen * 2) then
              begin
                CMD_Err(erOutOfRange);
                DestroyHeapVariant(VarP);
                Result := False;
                Exit;
              end;
            end;
          btUnicodeString:
            begin
              if not DoRead(NameLen, 4) then
              begin
                CMD_Err(erOutOfRange);
                DestroyHeapVariant(VarP);
                Result := False;
                Exit;
              end;
              Inc(FCurrentPosition, 4);
              SetLength(PPSVariantUString(varp).Data, NameLen);
              if not DoRead(PPSVariantUString(varp).Data[1], NameLen * 2) then
              begin
                CMD_Err(erOutOfRange);
                DestroyHeapVariant(VarP);
                Result := False;
                Exit;
              end;
            end;
          {$ENDIF}
        else
          begin
            CMD_Err(erInvalidType);
            DestroyHeapVariant(VarP);
            Result := False;
            Exit;
          end;
        end;
      end;
      h := MakeHash(att.AttribType);
      for i := FAttributeTypes.Count - 1 downto 0 do
      begin
        if (TPSAttributeType(FAttributeTypes.Data^[i]).TypeNameHash = h) and
          (TPSAttributeType(FAttributeTypes.Data^[i]).TypeName = att.AttribType) then
        begin
          if not TPSAttributeType(FAttributeTypes.Data^[i]).UseProc(Self, att.AttribType, Att) then
          begin
            Result := False;
            Exit;
          end;
        end;
      end;
      Result := True;
    end;

  begin
    if not DoRead(Count, 4) then
    begin
      CMD_Err(erOutofRange);
      Result := False;
      Exit;
    end;
    for i := 0 to Count - 1 do
    begin
      if not ReadAttrib then
      begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
  end;

  {$WARNINGS OFF}

  function LoadTypes: Boolean;
  var
    currf: TPSType;
    Curr: PIFTypeRec;
    fe: Boolean;
    l2, l: Longint;
    d: Cardinal;

    function resolve(Dta: TPSTypeRec_Record): Boolean;
    var
      offs, l: Longint;
    begin
      offs := 0;
      for l := 0 to Dta.FieldTypes.Count - 1 do
      begin
        Dta.RealFieldOffsets.Add(Pointer(offs));
        offs := offs + TPSTypeRec(Dta.FieldTypes[l]).RealSize;
      end;
      Result := True;
    end;

  begin
    LoadTypes := True;
    for l := 0 to HDR.TypeCount - 1 do
    begin
      if not DoRead(currf, SizeOf(currf)) then
      begin
        CMD_Err(erUnexpectedEof);
        LoadTypes := False;
        Exit;
      end;
      if (currf.BaseType and 128) <> 0 then
      begin
        fe := True;
        currf.BaseType := currf.BaseType - 128;
      end
      else
        fe := False;
      case currf.BaseType of
        {$IFNDEF PS_NOINT64}
        bts64,
        {$ENDIF}
        btU8,
        btS8,
        btU16,
        btS16,
        btU32,
        btS32,
        btSingle,
        btDouble,
        btCurrency,
        btExtended,
        btString,
        btPointer,
        btPChar,
        btVariant,
        btChar
        {$IFNDEF PS_NOWIDESTRING},
        btUnicodeString,
        btWideString,
        btWideChar
        {$ENDIF}:
          begin
            curr := TPSTypeRec.Create(Self);
            Curr.BaseType := currf.BaseType;
            FTypes.Add(Curr);
          end;
        btClass:
          begin
            Curr := TPSTypeRec_Class.Create(Self);
            if (not DoRead(d, 4)) or (d > 255) then
            begin
              curr.Free;
              CMD_Err(erUnexpectedEof);
              LoadTypes := False;
              Exit;
            end;
            SetLength(TPSTypeRec_Class(Curr).FCN, d);
            if not DoRead(TPSTypeRec_Class(Curr).FCN[1], d) then
            begin
              curr.Free;
              CMD_Err(erUnexpectedEof);
              LoadTypes := False;
              Exit;
            end;
            Curr.BaseType := currf.BaseType;
            FTypes.Add(Curr);
          end;
        btProcPtr:
          begin
            Curr := TPSTypeRec_ProcPtr.Create(Self);
            if (not DoRead(d, 4)) or (d > 255) then
            begin
              curr.Free;
              CMD_Err(erUnexpectedEof);
              LoadTypes := False;
              Exit;
            end;
            SetLength(TPSTypeRec_ProcPtr(Curr).FParamInfo, d);
            if not DoRead(TPSTypeRec_ProcPtr(Curr).FParamInfo[1], d) then
            begin
              curr.Free;
              CMD_Err(erUnexpectedEof);
              LoadTypes := False;
              Exit;
            end;
            Curr.BaseType := currf.BaseType;
            FTypes.Add(Curr);
          end;
        {$IFNDEF PS_NOINTERFACES}
        btInterface:
          begin
            Curr := TPSTypeRec_Interface.Create(Self);
            if not DoRead(TPSTypeRec_Interface(Curr).FGUID, SizeOf(TGuid)) then
            begin
              curr.Free;
              CMD_Err(erUnexpectedEof);
              LoadTypes := False;
              Exit;
            end;
            Curr.BaseType := currf.BaseType;
            FTypes.Add(Curr);
          end;
        {$ENDIF}
        btSet:
          begin
            Curr := TPSTypeRec_Set.Create(Self);
            if not DoRead(d, 4) then
            begin
              curr.Free;
              CMD_Err(erUnexpectedEof);
              LoadTypes := False;
              Exit;
            end;
            if (d > 256) then
            begin
              curr.Free;
              CMD_Err(erTypeMismatch);
              LoadTypes := False;
              Exit;
            end;

            TPSTypeRec_Set(curr).aBitSize := d;
            TPSTypeRec_Set(curr).aByteSize := TPSTypeRec_Set(curr).aBitSize shr 3;
            if (TPSTypeRec_Set(curr).aBitSize and 7) <> 0 then Inc(TPSTypeRec_Set(curr).fbytesize);
            Curr.BaseType := currf.BaseType;
            FTypes.Add(Curr);
          end;
        btStaticArray:
          begin
            curr := TPSTypeRec_StaticArray.Create(Self);
            if not DoRead(d, 4) then
            begin
              curr.Free;
              CMD_Err(erUnexpectedEof);
              LoadTypes := False;
              Exit;
            end;
            if (d >= FTypes.Count) then
            begin
              curr.Free;
              CMD_Err(erTypeMismatch);
              LoadTypes := False;
              Exit;
            end;
            TPSTypeRec_StaticArray(curr).ArrayType := FTypes[d];
            if not DoRead(d, 4) then
            begin
              curr.Free;
              CMD_Err(erUnexpectedEof);
              LoadTypes := False;
              Exit;
            end;
            if d > (MaxInt div 4) then
            begin
              curr.Free;
              CMD_Err(erUnexpectedEof);
              LoadTypes := False;
              Exit;
            end;
            TPSTypeRec_StaticArray(curr).Size := d;
            if not DoRead(d, 4) then //<-additional StartOffset
            begin
              curr.Free;
              CMD_Err(erUnexpectedEof);
              LoadTypes := False;
              Exit;
            end;
            TPSTypeRec_StaticArray(curr).StartOffset:=d;

            Curr.BaseType := currf.BaseType;
            FTypes.Add(Curr);
          end;
        btArray:
          begin
            Curr := TPSTypeRec_Array.Create(Self);
            if not DoRead(d, 4) then
            begin // Read type
              curr.Free;
              CMD_Err(erUnexpectedEof);
              LoadTypes := False;
              Exit;
            end;
            if (d >= FTypes.Count) then
            begin
              curr.Free;
              CMD_Err(erTypeMismatch);
              LoadTypes := False;
              Exit;
            end;
            Curr.BaseType := currf.BaseType;
            TPSTypeRec_Array(curr).ArrayType := FTypes[d];
            FTypes.Add(Curr);
          end;
        btRecord:
          begin
            curr := TPSTypeRec_Record.Create(Self);
            if not DoRead(d, 4) or (d = 0) then
            begin
              curr.Free;
              CMD_Err(erUnexpectedEof);
              LoadTypes := False;
              Exit;
            end;
            while d > 0 do
            begin
              if not DoRead(l2, 4) then
              begin
                curr.Free;
                CMD_Err(erUnexpectedEof);
                LoadTypes := False;
                Exit;
              end;
              if Cardinal(l2) >= FTypes.Count then
              begin
                curr.Free;
                CMD_Err(erOutOfRange);
                LoadTypes := False;
                Exit;
              end;
              TPSTypeRec_Record(curR).FFieldTypes.Add(FTypes[l2]);
              Dec(D);
            end;
            if not resolve(TPSTypeRec_Record(curr)) then
            begin
              curr.Free;
              CMD_Err(erInvalidType);
              LoadTypes := False;
              Exit;
            end;
            Curr.BaseType := currf.BaseType;
            FTypes.Add(Curr);
          end;
      else
        begin
          LoadTypes := False;
          CMD_Err(erInvalidType);
          Exit;
        end;
      end;
      if fe then
      begin
        if not DoRead(d, 4) then
        begin
          CMD_Err(erUnexpectedEof);
          LoadTypes := False;
          Exit;
        end;
        if d > PSAddrNegativeStackStart then
        begin
          CMD_Err(erInvalidType);
          LoadTypes := False;
          Exit;
        end;
        SetLength(Curr.FExportName, d);
        if not DoRead(Curr.fExportName[1], d) then
        begin
          CMD_Err(erUnexpectedEof);
          LoadTypes := False;
          Exit;
        end;
        Curr.ExportNameHash := MakeHash(Curr.ExportName);
      end;
      curr.CalcSize;
      if HDR.PSBuildNo >= 21 then // since build 21 we support attributes
      begin
        if not ReadAttributes(Curr.Attributes) then
        begin
          LoadTypes := False;
          Exit;
        end;
      end;
    end;
  end;

  function LoadProcs: Boolean;
  var
    Rec: TPSProc;
    n: TbtString;
    b: Byte;
    l, L2, L3: Longint;
    Curr: TPSProcRec;
  begin
    LoadProcs := True;
    for l := 0 to HDR.ProcCount - 1 do
    begin
      if not DoRead(Rec, SizeOf(Rec)) then
      begin
        CMD_Err(erUnexpectedEof);
        LoadProcs := False;
        Exit;
      end;
      if Rec.Flags and 1 <> 0 then
      begin
        Curr := TPSExternalProcRec.Create(Self);
        if not DoRead(b, 1) then
        begin
          Curr.Free;
          CMD_Err(erUnexpectedEof);
          LoadProcs := False;
          Exit;
        end;
        SetLength(n, b);
        if not DoRead(n[1], b) then
        begin
          Curr.Free;
          CMD_Err(erUnexpectedEof);
          LoadProcs := False;
          Exit;
        end;
        TPSExternalProcRec(Curr).Name := n;
        if (Rec.Flags and 3 = 3) then
        begin
          if (not DoRead(L2, 4)) or (L2 > Length(s) - Pos) then
          begin
            Curr.Free;
            CMD_Err(erUnexpectedEof);
            LoadProcs := False;
            Exit;
          end;
          SetLength(n, L2);
          DoRead(n[1], L2); // no check is needed
          TPSExternalProcRec(Curr).FDecl := n;
        end;
        if not ImportProc(TPSExternalProcRec(Curr).Name, TPSExternalProcRec(Curr)) then
        begin
          if TPSExternalProcRec(Curr).Name <> '' then
            CMD_Err2(erCannotImport, TPSExternalProcRec(Curr).Name)
          else
            CMD_Err2(erCannotImport, TPSExternalProcRec(curr).Decl);
          Curr.Free;
          LoadProcs := False;
          Exit;
        end;
      end
      else
      begin
        Curr := TPSInternalProcRec.Create(Self);
        if not DoRead(L2, 4) then
        begin
          Curr.Free;
          CMD_Err(erUnexpectedEof);
          LoadProcs := False;
          Exit;
        end;
        if not DoRead(L3, 4) then
        begin
          Curr.Free;
          CMD_Err(erUnexpectedEof);
          LoadProcs := False;
          Exit;
        end;
        if (L2 < 0) or (L2 >= Length(s)) or (L2 + L3 > Length(s)) or (L3 = 0) then
        begin
          Curr.Free;
          CMD_Err(erUnexpectedEof);
          LoadProcs := False;
          Exit;
        end;

        GetMem(TPSInternalProcRec(Curr).FData, L3);
        Move(s[L2 + 1], TPSInternalProcRec(Curr).FData^, L3);
        TPSInternalProcRec(Curr).FLength := L3;
        if Rec.Flags and 2 <> 0 then
        begin // exported
          if not DoRead(L3, 4) then
          begin
            Curr.Free;
            CMD_Err(erUnexpectedEof);
            LoadProcs := False;
            Exit;
          end;
          if L3 > PSAddrNegativeStackStart then
          begin
            Curr.Free;
            CMD_Err(erUnexpectedEof);
            LoadProcs := False;
            Exit;
          end;
          SetLength(TPSInternalProcRec(Curr).FExportName, L3);
          if not DoRead(TPSInternalProcRec(Curr).FExportName[1], L3) then
          begin
            Curr.Free;
            CMD_Err(erUnexpectedEof);
            LoadProcs := False;
            Exit;
          end;
          if not DoRead(L3, 4) then
          begin
            Curr.Free;
            CMD_Err(erUnexpectedEof);
            LoadProcs := False;
            Exit;
          end;
          if L3 > PSAddrNegativeStackStart then
          begin
            Curr.Free;
            CMD_Err(erUnexpectedEof);
            LoadProcs := False;
            Exit;
          end;
          SetLength(TPSInternalProcRec(Curr).FExportDecl, L3);
          if not DoRead(TPSInternalProcRec(Curr).FExportDecl[1], L3) then
          begin
            Curr.Free;
            CMD_Err(erUnexpectedEof);
            LoadProcs := False;
            Exit;
          end;
          TPSInternalProcRec(Curr).FExportNameHash := MakeHash(TPSInternalProcRec(Curr).ExportName);
        end;
      end;
      if Rec.Flags and 4 <> 0 then
      begin
        if not ReadAttributes(Curr.Attributes) then
        begin
          Curr.Free;
          LoadProcs := False;
          Exit;
        end;
      end;
      FProcs.Add(Curr);
    end;
  end;

  {$WARNINGS ON}

  function LoadVars: Boolean;
  var
    l, n: Longint;
    e: PPSExportedVar;
    Rec: TPSVar;
    Curr: PIfVariant;
  begin
    LoadVars := True;
    for l := 0 to HDR.VarCount - 1 do
    begin
      if not DoRead(Rec, SizeOf(Rec)) then
      begin
        CMD_Err(erUnexpectedEof);
        LoadVars := False;
        Exit;
      end;
      if Rec.TypeNo >= HDR.TypeCount then
      begin
        CMD_Err(erInvalidType);
        LoadVars := False;
        Exit;
      end;
      Curr := FGlobalVars.PushType(FTypes.Data^[Rec.TypeNo]);
      if Curr = nil then
      begin
        CMD_Err(erInvalidType);
        LoadVars := False;
        Exit;
      end;
      if Rec.Flags and 1 <> 0 then
      begin
        if not DoRead(n, 4) then
        begin
          CMD_Err(erUnexpectedEof);
          LoadVars := False;
          Exit;
        end;
        new(e);
        try
          SetLength(e^.FName, n);
          if not DoRead(e^.FName[1], n) then
          begin
            dispose(e);
            CMD_Err(erUnexpectedEof);
            LoadVars := False;
            Exit;
          end;
          e^.FNameHash := MakeHash(e^.FName);
          e^.FVarNo := FGlobalVars.Count;
          FExportedVars.Add(E);
        except
          dispose(e);
          CMD_Err(erInvalidType);
          LoadVars := False;
          Exit;
        end;
      end;
    end;
  end;

begin
  Clear;
  Pos := 0;
  LoadData := False;
  if not DoRead(HDR, SizeOf(HDR)) then
  begin
    CMD_Err(erInvalidHeader);
    Exit;
  end;
  if HDR.HDR <> PSValidHeader then
  begin
    CMD_Err(erInvalidHeader);
    Exit;
  end;
  if (HDR.PSBuildNo > PSCurrentBuildNo) or (HDR.PSBuildNo < PSLowBuildSupport) then
  begin
    CMD_Err(erInvalidHeader);
    Exit;
  end;
  if not LoadTypes then
  begin
    Clear;
    Exit;
  end;
  if not LoadProcs then
  begin
    Clear;
    Exit;
  end;
  if not LoadVars then
  begin
    Clear;
    Exit;
  end;
  if (HDR.MainProcNo >= FProcs.Count) and (HDR.MainProcNo <> InvalidVal) then
  begin
    CMD_Err(erNoMainProc);
    Clear;
    Exit;
  end;
  // Load Import Table
  FMainProc := HDR.MainProcNo;
  FStatus := isLoaded;
  Result := True;
end;

//==============================================================================
//
// TPSExec.Pause
//
//==============================================================================
procedure TPSExec.Pause;
begin
  if FStatus = isRunning then
    FStatus := isPaused;
end;

//==============================================================================
//
// TPSExec.ReadData
//
//==============================================================================
function TPSExec.ReadData(var Data; Len: Cardinal): Boolean;
begin
  if FCurrentPosition + Len <= FDataLength then
  begin
    Move(FData^[FCurrentPosition], Data, Len);
    FCurrentPosition := FCurrentPosition + Len;
    Result := True;
  end
  else
    Result := False;
end;

//==============================================================================
//
// TPSExec.CMD_Err
//
//==============================================================================
procedure TPSExec.CMD_Err(EC: TPSError); // Error
begin
  CMD_Err3(ec, '', nil);
end;

//==============================================================================
//
// VNSetPointerTo
//
//==============================================================================
procedure VNSetPointerTo(const Src: TPSVariantIFC; Data: Pointer; aType: TPSTypeRec);
begin
  if Src.aType.BaseType = btPointer then
  begin
    if atype.BaseType in NeedFinalization then
      FinalizeVariant(src.Dta, Src.aType);
    Pointer(Src.Dta^) := Data;
    Pointer(Pointer(IPointer(Src.Dta) + PointerSize)^) := aType;
    Pointer(Pointer(IPointer(Src.Dta) + (2 * PointerSize))^) := nil;
  end;
end;

//==============================================================================
//
// VNGetUInt
//
//==============================================================================
function VNGetUInt(const Src: TPSVariantIFC): Cardinal;
begin
  Result := PSGetUInt(Src.Dta, Src.aType);
end;

{$IFNDEF PS_NOINT64}

//==============================================================================
//
// VNGetInt64
//
//==============================================================================
function VNGetInt64(const Src: TPSVariantIFC): Int64;
begin
  Result := PSGetInt64(Src.Dta, Src.aType);
end;
{$ENDIF}

//==============================================================================
//
// VNGetReal
//
//==============================================================================
function VNGetReal(const Src: TPSVariantIFC): Extended;
begin
  Result := PSGetReal(Src.Dta, Src.aType);
end;

//==============================================================================
//
// VNGetCurrency
//
//==============================================================================
function VNGetCurrency(const Src: TPSVariantIFC): Currency;
begin
  Result := PSGetCurrency(Src.Dta, Src.aType);
end;

//==============================================================================
//
// VNGetInt
//
//==============================================================================
function VNGetInt(const Src: TPSVariantIFC): Longint;
begin
  Result := PSGetInt(Src.Dta, Src.aType);
end;

//==============================================================================
//
// VNGetAnsiString
//
//==============================================================================
function VNGetAnsiString(const Src: TPSVariantIFC): TbtString;
begin
  Result := PSGetAnsiString(Src.Dta, Src.aType);
end;

{$IFNDEF PS_NOWIDESTRING}

//==============================================================================
//
// VNGetWideString
//
//==============================================================================
function VNGetWideString(const Src: TPSVariantIFC): TbtWideString;
begin
  Result := PSGetWideString(Src.Dta, Src.aType);
end;

//==============================================================================
//
// VNGetUnicodeString
//
//==============================================================================
function VNGetUnicodeString(const Src: TPSVariantIFC): TbtUnicodeString;
begin
  Result := PSGetUnicodeString(Src.Dta, Src.aType);
end;
{$ENDIF}

//==============================================================================
//
// VNSetUInt
//
//==============================================================================
procedure VNSetUInt(const Src: TPSVariantIFC; const Val: Cardinal);
var
  Dummy: Boolean;
begin
  PSSetUInt(Src.Dta, Src.aType, Dummy, Val);
end;

{$IFNDEF PS_NOINT64}

//==============================================================================
//
// VNSetInt64
//
//==============================================================================
procedure VNSetInt64(const Src: TPSVariantIFC; const Val: Int64);
var
  Dummy: Boolean;
begin
  PSSetInt64(Src.Dta, Src.aType, Dummy, Val);
end;
{$ENDIF}

//==============================================================================
//
// VNSetReal
//
//==============================================================================
procedure VNSetReal(const Src: TPSVariantIFC; const Val: Extended);
var
  Dummy: Boolean;
begin
  PSSetReal(Src.Dta, Src.aType, Dummy, Val);
end;

//==============================================================================
//
// VNSetCurrency
//
//==============================================================================
procedure VNSetCurrency(const Src: TPSVariantIFC; const Val: Currency);
var
  Dummy: Boolean;
begin
  PSSetCurrency(Src.Dta, Src.aType, Dummy, Val);
end;

//==============================================================================
//
// VNSetInt
//
//==============================================================================
procedure VNSetInt(const Src: TPSVariantIFC; const Val: Longint);
var
  Dummy: Boolean;
begin
  PSSetInt(Src.Dta, Src.aType, Dummy, Val);
end;

//==============================================================================
//
// VNSetAnsiString
//
//==============================================================================
procedure VNSetAnsiString(const Src: TPSVariantIFC; const Val: TbtString);
var
  Dummy: Boolean;
begin
  PSSetAnsiString(Src.Dta, Src.aType, Dummy, Val);
end;

//==============================================================================
//
// VNGetString
//
//==============================================================================
function VNGetString(const Src: TPSVariantIFC): String;
begin
  {$IFNDEF PS_NOWIDESTRING}
    {$IFDEF DELPHI2009UP}
    Result := VNGetUnicodeString(Src);
    {$ELSE}
    Result := VNGetAnsiString(Src);
    {$ENDIF}
  {$ELSE}
  Result := VNGetAnsiString(Src);
  {$ENDIF}
end;

//==============================================================================
//
// VNSetString
//
//==============================================================================
procedure VNSetString(const Src: TPSVariantIFC; const Val: String);
begin
  {$IFNDEF PS_NOWIDESTRING}
    {$IFDEF DELPHI2009UP}
    VNSetUnicodeString(Src, Val);
    {$ELSE}
    VNSetAnsiString(Src, Val);
    {$ENDIF}
  {$ELSE}
  VNSetAnsiString(Src, Val);
  {$ENDIF}
end;

{$IFNDEF PS_NOWIDESTRING}

//==============================================================================
//
// VNSetWideString
//
//==============================================================================
procedure VNSetWideString(const Src: TPSVariantIFC; const Val: TbtWideString);
var
  Dummy: Boolean;
begin
  PSSetWideString(Src.Dta, Src.aType, Dummy, Val);
end;

//==============================================================================
//
// VNSetUnicodeString
//
//==============================================================================
procedure VNSetUnicodeString(const Src: TPSVariantIFC; const Val: TbtUnicodeString);
var
  Dummy: Boolean;
begin
  PSSetUnicodeString(Src.Dta, Src.aType, Dummy, Val);
end;

{$ENDIF}

//==============================================================================
//
// VGetUInt
//
//==============================================================================
function VGetUInt(const Src: PIFVariant): Cardinal;
begin
  Result := PSGetUInt(@PPSVariantData(src).Data, src.FType);
end;

{$IFNDEF PS_NOINT64}

//==============================================================================
//
// VGetInt64
//
//==============================================================================
function VGetInt64(const Src: PIFVariant): Int64;
begin
  Result := PSGetInt64(@PPSVariantData(src).Data, src.FType);
end;
{$ENDIF}

//==============================================================================
//
// VGetReal
//
//==============================================================================
function VGetReal(const Src: PIFVariant): Extended;
begin
  Result := PSGetReal(@PPSVariantData(src).Data, src.FType);
end;

//==============================================================================
//
// VGetCurrency
//
//==============================================================================
function VGetCurrency(const Src: PIFVariant): Currency;
begin
  Result := PSGetCurrency(@PPSVariantData(src).Data, src.FType);
end;

//==============================================================================
//
// VGetInt
//
//==============================================================================
function VGetInt(const Src: PIFVariant): Longint;
begin
  Result := PSGetInt(@PPSVariantData(src).Data, src.FType);
end;

//==============================================================================
//
// VGetAnsiString
//
//==============================================================================
function VGetAnsiString(const Src: PIFVariant): TbtString;
begin
  Result := PSGetAnsiString(@PPSVariantData(src).Data, src.FType);
end;

{$IFNDEF PS_NOWIDESTRING}

//==============================================================================
//
// VGetWideString
//
//==============================================================================
function VGetWideString(const Src: PIFVariant): TbtWideString;
begin
  Result := PSGetWideString(@PPSVariantData(src).Data, src.FType);
end;

//==============================================================================
//
// VGetUnicodeString
//
//==============================================================================
function VGetUnicodeString(const Src: PIFVariant): TbtUnicodeString;
begin
  Result := PSGetUnicodeString(@PPSVariantData(src).Data, src.FType);
end;
{$ENDIF}

//==============================================================================
//
// VSetPointerTo
//
//==============================================================================
procedure VSetPointerTo(const Src: PIFVariant; Data: Pointer; aType: TPSTypeRec);
var
  temp: TPSVariantIFC;
begin
  if (Atype = nil) or (Data = nil) or (Src = nil) then
    raise Exception.Create(RPS_InvalidVariable);
  temp.Dta := @PPSVariantData(Src).Data;
  temp.aType := Src.FType;
  temp.VarParam := False;
  VNSetPointerTo(temp, Data, AType);
end;

//==============================================================================
//
// VSetUInt
//
//==============================================================================
procedure VSetUInt(const Src: PIFVariant; const Val: Cardinal);
var
  Dummy: Boolean;
begin
  PSSetUInt(@PPSVariantData(src).Data, src.FType, Dummy, Val);
end;

{$IFNDEF PS_NOINT64}

//==============================================================================
//
// VSetInt64
//
//==============================================================================
procedure VSetInt64(const Src: PIFVariant; const Val: Int64);
var
  Dummy: Boolean;
begin
  PSSetInt64(@PPSVariantData(src).Data, src.FType, Dummy, Val);
end;
{$ENDIF}

//==============================================================================
//
// VSetReal
//
//==============================================================================
procedure VSetReal(const Src: PIFVariant; const Val: Extended);
var
  Dummy: Boolean;
begin
  PSSetReal(@PPSVariantData(src).Data, src.FType, Dummy, Val);
end;

//==============================================================================
//
// VSetCurrency
//
//==============================================================================
procedure VSetCurrency(const Src: PIFVariant; const Val: Currency);
var
  Dummy: Boolean;
begin
  PSSetCurrency(@PPSVariantData(src).Data, src.FType, Dummy, Val);
end;

//==============================================================================
//
// VSetInt
//
//==============================================================================
procedure VSetInt(const Src: PIFVariant; const Val: Longint);
var
  Dummy: Boolean;
begin
  PSSetInt(@PPSVariantData(src).Data, src.FType, Dummy, Val);
end;

//==============================================================================
//
// VSetAnsiString
//
//==============================================================================
procedure VSetAnsiString(const Src: PIFVariant; const Val: TbtString);
var
  Dummy: Boolean;
begin
  PSSetAnsiString(@PPSVariantData(src).Data, src.FType, Dummy, Val);
end;

//==============================================================================
//
// VGetString
//
//==============================================================================
function VGetString(const Src: PIFVariant): String;
begin
  {$IFNDEF PS_NOWIDESTRING}
  {$IFDEF DELPHI2009UP}
  Result := PSGetUnicodeString(@PPSVariantData(src).Data, src.FType);
  {$ELSE}
  Result := PSGetAnsiString(@PPSVariantData(src).Data, src.FType);
  {$ENDIF}
  {$ELSE}
  Result := PSGetAnsiString(@PPSVariantData(src).Data, src.FType);
  {$ENDIF}
end;

//==============================================================================
//
// VSetString
//
//==============================================================================
procedure VSetString(const Src: PIFVariant; const Val: string);
var
  Dummy: Boolean;
begin
  {$IFNDEF PS_NOWIDESTRING}
  {$IFDEF DELPHI2009UP}
  PSSetUnicodeString(@PPSVariantData(src).Data, src.FType, Dummy, Val);
  {$ELSE}
  PSSetAnsiString(@PPSVariantData(src).Data, src.FType, Dummy, Val);
  {$ENDIF}
  {$ELSE}
  PSSetAnsiString(@PPSVariantData(src).Data, src.FType, Dummy, Val);
  {$ENDIF}
end;

{$IFNDEF PS_NOWIDESTRING}

//==============================================================================
//
// VSetWideString
//
//==============================================================================
procedure VSetWideString(const Src: PIFVariant; const Val: TbtWideString);
var
  Dummy: Boolean;
begin
  PSSetWideString(@PPSVariantData(src).Data, src.FType, Dummy, Val);
end;

//==============================================================================
//
// VSetUnicodeString
//
//==============================================================================
procedure VSetUnicodeString(const Src: PIFVariant; const Val: TbtUnicodeString);
var
  Dummy: Boolean;
begin
  PSSetUnicodeString(@PPSVariantData(src).Data, src.FType, Dummy, Val);
end;
{$ENDIF}

{$IFNDEF PS_NOWIDESTRING}

//==============================================================================
//
// VarToWideStr
//
//==============================================================================
function VarToWideStr(const Data: Variant): TbtUnicodeString;
begin
  if not VarIsNull(Data) then
    Result := Data
  else
    Result := '';
end;
{$ENDIF}

//==============================================================================
//
// PSGetUInt
//
//==============================================================================
function PSGetUInt(Src: Pointer; aType: TPSTypeRec): Cardinal;
begin
  if aType.BaseType = btPointer then
  begin
    atype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
    Src := Pointer(Src^);
    if (src = nil) or (aType = nil) then
      raise Exception.Create(RPS_TypeMismatch);
  end;
  case aType.BaseType of
    btU8:
      Result := TbtU8(src^);
    btS8:
      Result := TbtS8(src^);
    btU16:
      Result := TbtU16(src^);
    btS16:
      Result := TbtS16(src^);
    btU32:
      Result := TbtU32(src^);
    btS32:
      Result := TbtS32(src^);
    {$IFNDEF PS_NOINT64}
    btS64:
      Result := TbtS64(src^);
    {$ENDIF}
    btChar:
      Result := Ord(TbtChar(Src^));
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar:
      Result := Ord(TbtWideChar(Src^));{$ENDIF}
    btVariant:
      case VarType(Variant(Src^)) of
        varString:
          if Length(VarToStr(Variant(Src^))) = 1 then
            Result := Ord(VarToStr(Variant(Src^))[1])
          else
            raise Exception.Create(RPS_TypeMismatch);
        {$IFNDEF PS_NOWIDESTRING}
        varOleStr:
          if Length(VarToWideStr(Variant(Src^))) = 1 then
            Result := Ord(VarToWideStr(Variant(Src^))[1])
          else
            raise Exception.Create(RPS_TypeMismatch);
        {$ENDIF}
        else
          Result := Variant(src^);
       end;
    else
      raise Exception.Create(RPS_TypeMismatch);
  end;
end;

//==============================================================================
//
// PSGetObject
//
//==============================================================================
function PSGetObject(Src: Pointer; aType: TPSTypeRec): TObject;
begin
  if aType.BaseType = btPointer then
  begin
    atype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
    Src := Pointer(Src^);
    if (src = nil) or (aType = nil) then
      raise Exception.Create(RPS_TypeMismatch);
  end;
  case aType.BaseType of
    btClass:
      Result := TObject(Src^);
    else
      raise Exception.Create(RPS_TypeMismatch);
  end;
end;

//==============================================================================
//
// PSSetObject
//
//==============================================================================
procedure PSSetObject(Src: Pointer; aType: TPSTypeRec; var OK: Boolean; Const val: TObject);
begin
  if aType.BaseType = btPointer then
  begin
    atype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
    Src := Pointer(Src^);
    if (src = nil) or (aType = nil) then
      raise Exception.Create(RPS_TypeMismatch);
  end;
  case aType.BaseType of
    btClass:
      TObject(Src^) := Val;
  else
    raise Exception.Create(RPS_TypeMismatch);
  end;
end;

{$IFNDEF PS_NOINT64}

//==============================================================================
//
// PSGetInt64
//
//==============================================================================
function PSGetInt64(Src: Pointer; aType: TPSTypeRec): Int64;
begin
  if aType.BaseType = btPointer then
  begin
    atype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
    Src := Pointer(Src^);
    if (src = nil) or (aType = nil) then
      raise Exception.Create(RPS_TypeMismatch);
  end;
  case aType.BaseType of
    btU8:
      Result := TbtU8(src^);
    btS8:
      Result := TbtS8(src^);
    btU16:
      Result := TbtU16(src^);
    btS16:
      Result := TbtS16(src^);
    btU32:
      Result := TbtU32(src^);
    btS32:
      Result := TbtS32(src^);
    btS64:
      Result := TbtS64(src^);
    btChar:
      Result := Ord(TbtChar(Src^));
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar:
      Result := Ord(TbtWideChar(Src^));
    {$ENDIF}
    {$IFDEF DELPHI6UP}
    btVariant:
      Result := Variant(src^);
    {$ENDIF}
    else
      raise Exception.Create(RPS_TypeMismatch);
  end;
end;
{$ENDIF}

//==============================================================================
//
// PSGetReal
//
//==============================================================================
function PSGetReal(Src: Pointer; aType: TPSTypeRec): Extended;
begin
  if aType.BaseType = btPointer then
  begin
    atype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
    Src := Pointer(Src^);
    if (src = nil) or (aType = nil) then
      raise Exception.Create(RPS_TypeMismatch);
  end;
  case aType.BaseType of
    btU8:
      Result := TbtU8(src^);
    btS8:
      Result := TbtS8(src^);
    btU16:
      Result := TbtU16(src^);
    btS16:
      Result := TbtS16(src^);
    btU32:
      Result := TbtU32(src^);
    btS32:
      Result := TbtS32(src^);
    {$IFNDEF PS_NOINT64}
    btS64:
      Result := TbtS64(src^);
    {$ENDIF}
    btSingle:
      Result := TbtSingle(Src^);
    btDouble:
      Result := TbtDouble(Src^);
    btExtended:
      Result := TbtExtended(Src^);
    btCurrency:
      Result := TbtCurrency(Src^);
    btVariant:
      Result := Variant(src^);
  else
    raise Exception.Create(RPS_TypeMismatch);
  end;
end;

//==============================================================================
//
// PSGetCurrency
//
//==============================================================================
function PSGetCurrency(Src: Pointer; aType: TPSTypeRec): Currency;
begin
  if aType.BaseType = btPointer then
  begin
    atype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
    Src := Pointer(Src^);
    if (src = nil) or (aType = nil) then
      raise Exception.Create(RPS_TypeMismatch);
  end;
  case aType.BaseType of
    btU8:
      Result := TbtU8(src^);
    btS8:
      Result := TbtS8(src^);
    btU16:
      Result := TbtU16(src^);
    btS16:
      Result := TbtS16(src^);
    btU32:
      Result := TbtU32(src^);
    btS32:
      Result := TbtS32(src^);
    {$IFNDEF PS_NOINT64}
    btS64:
      Result := TbtS64(src^);
    {$ENDIF}
    btSingle:
      Result := TbtSingle(Src^);
    btDouble:
      Result := TbtDouble(Src^);
    btExtended:
      Result := TbtExtended(Src^);
    btCurrency:
      Result := TbtCurrency(Src^);
    btVariant:
      Result := Variant(src^);
    else
      raise Exception.Create(RPS_TypeMismatch);
  end;
end;

//==============================================================================
//
// PSGetInt
//
//==============================================================================
function PSGetInt(Src: Pointer; aType: TPSTypeRec): Longint;
begin
  if aType.BaseType = btPointer then
  begin
    atype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
    Src := Pointer(Src^);
    if (src = nil) or (aType = nil) then
      raise Exception.Create(RPS_TypeMismatch);
  end;
  case aType.BaseType of
    btU8:
      Result := TbtU8(src^);
    btS8:
      Result := TbtS8(src^);
    btU16:
      Result := TbtU16(src^);
    btS16:
      Result := TbtS16(src^);
    btU32:
      Result := TbtU32(src^);
    btS32:
      Result := TbtS32(src^);
    {$IFNDEF PS_NOINT64}
    btS64:
      Result := TbtS64(src^);
    {$ENDIF}
    btChar:
      Result := Ord(TbtChar(Src^));
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar:
      Result := Ord(TbtWideChar(Src^));
    {$ENDIF}
    btVariant:
      Result := Variant(src^);
    else
      raise Exception.Create(RPS_TypeMismatch);
  end;
end;

//==============================================================================
//
// PSGetAnsiString
//
//==============================================================================
function PSGetAnsiString(Src: Pointer; aType: TPSTypeRec): TbtString;
begin
  if aType.BaseType = btPointer then
  begin
    atype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
    Src := Pointer(Src^);
    if (src = nil) or (aType = nil) then
      raise Exception.Create(RPS_TypeMismatch);
  end;
  case aType.BaseType of
    btU8:
      Result := TbtChar(TbtU8(src^));
    btChar:
      Result := TbtChar(Src^);
    btPchar:
      Result := PAnsiChar(src^);
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar:
      Result := TbtString(TbtWideChar(Src^));
    {$ENDIF}
    btString:
      Result := TbtString(src^);
    {$IFNDEF PS_NOWIDESTRING}
    btUnicodeString:
      Result := TbtString(TbtUnicodeString(src^));
    btWideString:
      Result := TbtString(TbtWideString(src^));
    {$ENDIF}
    btVariant:
      Result := TbtString(Variant(src^));
    else
      raise Exception.Create(RPS_TypeMismatch);
  end;
end;

{$IFNDEF PS_NOWIDESTRING}

//==============================================================================
//
// PSGetWideString
//
//==============================================================================
function PSGetWideString(Src: Pointer; aType: TPSTypeRec): TbtWideString;
begin
  if aType.BaseType = btPointer then
  begin
    atype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
    Src := Pointer(Src^);
    if (src = nil) or (aType = nil) then
      raise Exception.Create(RPS_TypeMismatch);
  end;
  case aType.BaseType of
    btU8:
      Result := chr(TbtU8(src^));
    btU16:
      Result := widechar(src^);
    btChar:
      Result := TbtWideString(TbtChar(Src^));
    btPchar:
      Result := TbtWideString(PAnsiChar(src^));
    btWideChar:
      Result := TbtWideChar(Src^);
    btString:
      Result := TbtWideString(TbtString(src^));
    btWideString:
      Result := TbtWideString(src^);
    btVariant:
      Result := Variant(src^);
    btUnicodeString:
      Result := TbtUnicodeString(src^);
    else
      raise Exception.Create(RPS_TypeMismatch);
  end;
end;

//==============================================================================
//
// PSGetUnicodeString
//
//==============================================================================
function PSGetUnicodeString(Src: Pointer; aType: TPSTypeRec): TbtUnicodeString;
begin
  if aType.BaseType = btPointer then
  begin
    atype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
    Src := Pointer(Src^);
    if (src = nil) or (aType = nil) then
      raise Exception.Create(RPS_TypeMismatch);
  end;
  case aType.BaseType of
    btU8:
      Result := chr(TbtU8(src^));
    btU16:
      Result := widechar(src^);
    btChar:
      Result := TbtUnicodeString(TbtChar(Src^));
    btPchar:
      Result := TbtUnicodeString(PAnsiChar(src^));
    btWideChar:
      Result := TbtWideChar(Src^);
    btString:
      Result := TbtUnicodeString(TbtString(src^));
    btWideString:
      Result := TbtWideString(src^);
    btVariant:
      Result := Variant(src^);
    btUnicodeString:
      Result := TbtUnicodeString(src^);
    else
      raise Exception.Create(RPS_TypeMismatch);
  end;
end;
{$ENDIF}

//==============================================================================
//
// PSSetUInt
//
//==============================================================================
procedure PSSetUInt(Src: Pointer; aType: TPSTypeRec; var OK: Boolean; const Val: Cardinal);
begin
  if (Src = nil) or (aType = nil) then
  begin
    OK := False;
    Exit;
  end;
  if aType.BaseType = btPointer then
  begin
    atype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
    Src := Pointer(Src^);
    if (src = nil) or (aType = nil) then
    begin
      OK := False;
      Exit;
    end;
  end;
  case aType.BaseType of
    btU8:
      TbtU8(src^) := Val;
    btS8:
      TbtS8(src^) := Val;
    btU16:
      TbtU16(src^) := Val;
    btS16:
      TbtS16(src^) := Val;
    btProcPtr:
      begin
        TbtU32(src^) := Val;
        Pointer(Pointer(IPointer(Src) + PointerSize)^) := nil;
        Pointer(Pointer(IPointer(Src) + PointerSize2)^) := nil;
      end;
    btU32:
      TbtU32(src^) := Val;
    btS32:
      TbtS32(src^) := Val;
    {$IFNDEF PS_NOINT64}
    btS64:
      TbtS64(src^) := Val;
    {$ENDIF}
    btChar:
      TbtChar(Src^) := TbtChar(Val);
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar:
      TbtWideChar(Src^) := TbtWideChar(Val);
    {$ENDIF}
    btSingle:
      TbtSingle(src^) := Val;
    btDouble:
      TbtDouble(src^) := Val;
    btCurrency:
      TbtCurrency(src^) := Val;
    btExtended:
      TbtExtended(src^) := Val;
    btVariant:
      begin
        try
          Variant(src^) := {$IFDEF DELPHI6UP}val{$ELSE}TbtS32(val){$ENDIF};
        except
          OK := False;
        end;
      end;
    else
      OK := False;
  end;
end;

{$IFNDEF PS_NOINT64}

//==============================================================================
//
// PSSetInt64
//
//==============================================================================
procedure PSSetInt64(Src: Pointer; aType: TPSTypeRec; var OK: Boolean; const Val: Int64);
begin
  if (Src = nil) or (aType = nil) then
  begin
    OK := False;
    Exit;
  end;
  if aType.BaseType = btPointer then
  begin
    atype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
    Src := Pointer(Src^);
    if (src = nil) or (aType = nil) then
    begin
      OK := False;
      Exit;
    end;
  end;
  case aType.BaseType of
    btU8:
      TbtU8(src^) := Val;
    btS8:
      TbtS8(src^) := Val;
    btU16:
      TbtU16(src^) := Val;
    btS16:
      TbtS16(src^) := Val;
    btU32:
      TbtU32(src^) := Val;
    btS32:
      TbtS32(src^) := Val;
    btS64:
      TbtS64(src^) := Val;
    btChar:
      TbtChar(Src^) := TbtChar(Val);
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar:
      TbtWideChar(Src^) := TbtWideChar(Val);
    {$ENDIF}
    btSingle:
      TbtSingle(src^) := Val;
    btDouble:
      TbtDouble(src^) := Val;
    btCurrency:
      TbtCurrency(src^) := Val;
    btExtended:
      TbtExtended(src^) := Val;
    {$IFDEF DELPHI6UP}
    btVariant:
      begin
        try
          Variant(src^) := Val;
        except
          OK := False;
        end;
      end;
    {$ENDIF}
    else
      OK := False;
  end;
end;
{$ENDIF}

//==============================================================================
//
// PSSetReal
//
//==============================================================================
procedure PSSetReal(Src: Pointer; aType: TPSTypeRec; var OK: Boolean; const Val: Extended);
begin
  if (Src = nil) or (aType = nil) then
  begin
    OK := False;
    Exit;
  end;
  if aType.BaseType = btPointer then
  begin
    atype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
    Src := Pointer(Src^);
    if (src = nil) or (aType = nil) then
    begin
      OK := False;
      Exit;
    end;
  end;
  case aType.BaseType of
    btSingle:
      TbtSingle(src^) := Val;
    btDouble:
      TbtDouble(src^) := Val;
    btCurrency:
      TbtCurrency(src^) := Val;
    btExtended:
      TbtExtended(src^) := Val;
    btVariant:
      begin
        try
          Variant(src^) := Val;
        except
          OK := False;
        end;
      end;
    else
      OK := False;
  end;
end;

//==============================================================================
//
// PSSetCurrency
//
//==============================================================================
procedure PSSetCurrency(Src: Pointer; aType: TPSTypeRec; var OK: Boolean; const Val: Currency);
begin
  if (Src = nil) or (aType = nil) then
  begin
    OK := False;
    Exit;
  end;
  if aType.BaseType = btPointer then
  begin
    atype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
    Src := Pointer(Src^);
    if (src = nil) or (aType = nil) then
    begin
      OK := False;
      Exit;
    end;
  end;
  case aType.BaseType of
    btSingle:
      TbtSingle(src^) := Val;
    btDouble:
      TbtDouble(src^) := Val;
    btCurrency:
      TbtCurrency(src^) := Val;
    btExtended:
      TbtExtended(src^) := Val;
    btVariant:
      begin
        try
          Variant(src^) := Val;
        except
          OK := False;
        end;
      end;
    else
      OK := False;
  end;
end;

//==============================================================================
//
// PSSetInt
//
//==============================================================================
procedure PSSetInt(Src: Pointer; aType: TPSTypeRec; var OK: Boolean; const Val: Longint);
begin
  if (Src = nil) or (aType = nil) then
  begin
    OK := False;
    Exit;
  end;
  if aType.BaseType = btPointer then
  begin
    atype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
    Src := Pointer(Src^);
    if (src = nil) or (aType = nil) then
    begin
      OK := False;
      Exit;
    end;
  end;
  case aType.BaseType of
    btU8:
      TbtU8(src^) := Val;
    btS8:
      TbtS8(src^) := Val;
    btU16:
      TbtU16(src^) := Val;
    btS16:
      TbtS16(src^) := Val;
    btProcPtr:
      begin
        TbtU32(src^) := Val;
        Pointer(Pointer(IPointer(Src) + PointerSize)^) := nil;
        Pointer(Pointer(IPointer(Src) + PointerSize2)^) := nil;
      end;
    btU32:
      TbtU32(src^) := Val;
    btS32:
      TbtS32(src^) := Val;
    {$IFNDEF PS_NOINT64}
    btS64:
      TbtS64(src^) := Val;
    {$ENDIF}
    btChar:
      TbtChar(Src^) := TbtChar(Val);
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar:
      TbtWideChar(Src^) := TbtWideChar(Val);
    {$ENDIF}
    btSingle:
      TbtSingle(src^) := Val;
    btDouble:
      TbtDouble(src^) := Val;
    btCurrency:
      TbtCurrency(src^) := Val;
    btExtended:
      TbtExtended(src^) := Val;
    btVariant:
      begin
        try
          Variant(src^) := Val;
        except
          OK := False;
        end;
      end;
    else
      OK := False;
  end;
end;

//==============================================================================
//
// PSSetAnsiString
//
//==============================================================================
procedure PSSetAnsiString(Src: Pointer; aType: TPSTypeRec; var OK: Boolean; const Val: TbtString);
begin
  if (Src = nil) or (aType = nil) then
  begin
    OK := False;
    Exit;
  end;
  if aType.BaseType = btPointer then
  begin
    atype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
    Src := Pointer(Src^);
    if (src = nil) or (aType = nil) then
    begin
      OK := False;
      Exit;
    end;
  end;
  case aType.BaseType of
    btString:
      TbtString(src^) := val;
    btChar:
      if AnsiString(val) <> '' then
        TbtChar(src^) := AnsiString(val)[1];
    {$IFNDEF PS_NOWIDESTRING}
    btUnicodeString:
      TbtUnicodeString(src^) := TbtUnicodeString(AnsiString(val));
    btWideString:
      TbtWideString(src^) := TbtWideString(AnsiString(val));
    btWideChar:
      if AnsiString(val) <> '' then
        TbtWideChar(src^) := TbtWideChar(AnsiString(val)[1]);
    {$ENDIF}
    btVariant:
      begin
        try
          Variant(src^) := Val;
        except
          OK := False;
        end;
      end;
    else
      OK := False;
  end;
end;

{$IFNDEF PS_NOWIDESTRING}

//==============================================================================
//
// PSSetWideString
//
//==============================================================================
procedure PSSetWideString(Src: Pointer; aType: TPSTypeRec; var OK: Boolean; const Val: TbtWideString);
begin
  if (Src = nil) or (aType = nil) then
  begin
    OK := False;
    Exit;
  end;
  if aType.BaseType = btPointer then
  begin
    atype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
    Src := Pointer(Src^);
    if (src = nil) or (aType = nil) then
    begin
      OK := False;
      Exit;
    end;
  end;
  case aType.BaseType of
    btChar:
      if val <> '' then
        TbtChar(src^) := TbtChar(val[1]);
    btWideChar:
      if val <> '' then
        TbtWideChar(src^) := val[1];
    btString:
      TbtString(src^) := TbtString(val);
    btWideString:
      TbtWideString(src^) := val;
    btUnicodeString:
      TbtUnicodeString(src^) := val;
    btVariant:
      begin
        try
          Variant(src^) := Val;
        except
          OK := False;
        end;
      end;
    else
      OK := False;
  end;
end;

//==============================================================================
//
// PSSetUnicodeString
//
//==============================================================================
procedure PSSetUnicodeString(Src: Pointer; aType: TPSTypeRec; var OK: Boolean; const Val: TbtUnicodeString);
begin
  if (Src = nil) or (aType = nil) then
  begin
    OK := False;
    Exit;
  end;
  if aType.BaseType = btPointer then
  begin
    atype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
    Src := Pointer(Src^);
    if (src = nil) or (aType = nil) then
    begin
      OK := False;
      Exit;
    end;
  end;
  case aType.BaseType of
    btString:
      TbtString(src^) := TbtString(val);
    btWideString:
      TbtWideString(src^) := val;
    btUnicodeString:
      TbtUnicodeString(src^) := val;
    btVariant:
      begin
        try
          Variant(src^) := Val;
        except
          OK := False;
        end;
      end;
    else
      OK := False;
  end;
end;
{$ENDIF}

//==============================================================================
//
// PSGetString
//
//==============================================================================
function PSGetString(Src: Pointer; aType: TPSTypeRec): string;
begin
  {$IFNDEF PS_NOWIDESTRING}
  {$IFDEF DELPHI2009UP}
  Result := PSGetUnicodeString(Src, aType);
  {$ELSE}
  Result := PSGetAnsiString(Src, aType);
  {$ENDIF}
  {$ELSE}
  Result := PSGetAnsiString(Src, aType);
  {$ENDIF}
end;

//==============================================================================
//
// PSSetString
//
//==============================================================================
procedure PSSetString(Src: Pointer; aType: TPSTypeRec; var OK: Boolean; const Val: String);
begin
  {$IFNDEF PS_NOWIDESTRING}
  {$IFDEF DELPHI2009UP}
  PSSetUnicodeString(Src, aType, OK, Val);
  {$ELSE}
  PSSetAnsiString(Src, aType, OK, Val);
  {$ENDIF}
  {$ELSE}
  PSSetAnsiString(Src, aType, OK, Val);
  {$ENDIF}
end;

//==============================================================================
//
// CopyArrayContents
//
//==============================================================================
function CopyArrayContents(dest, src: Pointer; Len: Longint; aType: TPSTypeRec): Boolean; forward;

//==============================================================================
//
// CopyRecordContents
//
//==============================================================================
function CopyRecordContents(dest, src: Pointer; aType: TPSTypeRec_Record): Boolean;
var
  o, i: Longint;
begin
  for i := 0 to aType.FieldTypes.Count - 1 do
  begin
    o := Longint(atype.RealFieldOffsets[i]);
    CopyArrayContents(Pointer(IPointer(Dest) + Cardinal(o)), Pointer(IPointer(Src) + Cardinal(o)), 1, aType.FieldTypes[i]);
  end;
  Result := True;
end;

//==============================================================================
//
// CreateArrayFromVariant
//
//==============================================================================
function CreateArrayFromVariant(Exec: TPSExec; dest: Pointer; src: Variant; DestType: TPSTypeRec): Boolean;
var
  i: Integer;
  r: Pointer;
  lVarType: TPSTypeRec;
  v: variant;
begin
  lVarType := Exec.FindType2(btVariant);
  if lVarType = nil then
  begin
    Result := False;
    Exit;
  end;
  PSDynArraySetLength(Pointer(dest^), desttype, VarArrayHighBound(src, 1) - VarArrayLowBound(src, 1) + 1);
  r := Pointer(Dest^);
  DestType := TPSTypeRec_Array(DestType).ArrayType;
  for i := 0 to VarArrayHighBound(src, 1) - VarArrayLowBound(src, 1) do
  begin
    v := src[i + VarArrayLowBound(src, 1)];
    if not Exec.SetVariantValue(r, @v, desttype, lVarType) then
    begin
      Result := False;
      Exit;
    end;
    //r := Pointer(IPointer(r) + Longint(DestType.RealSize));
    r := Pointer(IPointer(r) + DestType.RealSize);
  end;
  Result := True;
end;

//==============================================================================
//
// CopyArrayContents
//
//==============================================================================
function CopyArrayContents(dest, src: Pointer; Len: Longint; aType: TPSTypeRec): Boolean;
var
  elsize: Cardinal;
  i: Longint;
begin
  try
    case aType.BaseType of
      btU8,
      btS8,
      btChar:
        for i := 0 to Len - 1 do
        begin
          TbtU8(Dest^) := TbtU8(Src^);
          Dest := Pointer(IPointer(Dest) + 1);
          Src := Pointer(IPointer(Src) + 1);
        end;
      btU16,
      btS16
      {$IFNDEF PS_NOWIDESTRING}, btWideChar{$ENDIF}:
        for i := 0 to Len - 1 do
        begin
          TbtU16(Dest^) := TbtU16(Src^);
          Dest := Pointer(IPointer(Dest) + 2);
          Src := Pointer(IPointer(Src) + 2);
        end;
      btProcPtr:
        for i := 0 to Len - 1 do
        begin
          TbtU32(Dest^) := TbtU32(Src^);
          Dest := Pointer(IPointer(Dest) + 4);
          Src := Pointer(IPointer(Src) + 4);
          Pointer(Dest^) := Pointer(Src^);
          Dest := Pointer(IPointer(Dest) + PointerSize);
          Src := Pointer(IPointer(Src) + PointerSize);
          Pointer(Dest^) := Pointer(Src^);
          Dest := Pointer(IPointer(Dest) + PointerSize);
          Src := Pointer(IPointer(Src) + PointerSize);
        end;
      btClass,
      btpchar:
        for i := 0 to Len - 1 do
        begin
          Pointer(Dest^) := Pointer(Src^);
          Dest := Pointer(IPointer(Dest) + PointerSize);
          Src := Pointer(IPointer(Src) + PointerSize);
        end;
      btU32,
      btS32,
      btSingle:
        for i := 0 to Len - 1 do
        begin
          TbtU32(Dest^) := TbtU32(Src^);
          Dest := Pointer(IPointer(Dest) + 4);
          Src := Pointer(IPointer(Src) + 4);
        end;
      btDouble:
        for i := 0 to Len - 1 do
        begin
          TbtDouble(Dest^) := TbtDouble(Src^);
          Dest := Pointer(IPointer(Dest) + 8);
          Src := Pointer(IPointer(Src) + 8);
        end;
      {$IFNDEF PS_NOINT64}
      bts64:
        for i := 0 to Len - 1 do
        begin
          TbtS64(Dest^) := TbtS64(Src^);
          Dest := Pointer(IPointer(Dest) + 8);
          Src := Pointer(IPointer(Src) + 8);
        end;
      {$ENDIF}
      btExtended:
        for i := 0 to Len - 1 do
        begin
          TbtExtended(Dest^) := TbtExtended(Src^);
          Dest := Pointer(IPointer(Dest) + SizeOf(Extended));
          Src := Pointer(IPointer(Src) + SizeOf(Extended));
        end;
      btCurrency:
        for i := 0 to Len - 1 do
        begin
          TbtCurrency(Dest^) := TbtCurrency(Src^);
          Dest := Pointer(IPointer(Dest) + SizeOf(Currency));
          Src := Pointer(IPointer(Src) + SizeOf(Currency));
        end;
      btVariant:
        for i := 0 to Len - 1 do
        begin
          variant(Dest^) := variant(Src^);
          Dest := Pointer(IPointer(Dest) + SizeOf(Variant));
          Src := Pointer(IPointer(Src) + SizeOf(Variant));
        end;
      btString:
        for i := 0 to Len - 1 do
        begin
          TbtString(Dest^) := TbtString(Src^);
          Dest := Pointer(IPointer(Dest) + PointerSize);
          Src := Pointer(IPointer(Src) + PointerSize);
        end;
      {$IFNDEF PS_NOWIDESTRING}
      btUnicodeString:
        for i := 0 to Len - 1 do
        begin
          TbtUnicodeString(Dest^) := TbtUnicodeString(Src^);
          Dest := Pointer(IPointer(Dest) + PointerSize);
          Src := Pointer(IPointer(Src) + PointerSize);
        end;
      btWideString:
        for i := 0 to Len - 1 do
        begin
          TbtWideString(Dest^) := TbtWideString(Src^);
          Dest := Pointer(IPointer(Dest) + PointerSize);
          Src := Pointer(IPointer(Src) + PointerSize);
        end;
    {$ENDIF}
      btStaticArray:
        begin
          elSize := aType.RealSize;
          for i := 0 to Len - 1 do
          begin
            if not CopyArrayContents(Dest, Src, TPSTypeRec_StaticArray(aType).Size, TPSTypeRec_StaticArray(aType).ArrayType) then
            begin
              Result := False;
              Exit;
            end;
            Dest := Pointer(IPointer(Dest) + elsize);
            Src := Pointer(IPointer(Src) + elsize);
          end;
        end;
      btArray:
        begin
          for i := 0 to Len - 1 do
          begin
            if Pointer(Dest^) <> nil then
            begin
              PSDynArraySetLength(Pointer(Dest^), aType, 0);
            end;
            Pointer(Dest^) := Pointer(Src^);
            if Pointer(Dest^) <> nil then
            begin
              Inc(PDynArrayRec(PAnsiChar(Dest^) - SizeOf(TDynArrayRecHeader))^.header.refCnt);
            end;
            Dest := Pointer(IPointer(Dest) + PointerSize);
            Src := Pointer(IPointer(Src) + PointerSize);
          end;
        end;
      btRecord:
        begin
          elSize := aType.RealSize;
          for i := 0 to Len - 1 do
          begin
            if not CopyRecordContents(Dest, Src, TPSTypeRec_Record(aType)) then
            begin
              Result := False;
              Exit;
            end;
            Dest := Pointer(IPointer(Dest) + elsize);
            Src := Pointer(IPointer(Src) + elsize);
          end;
        end;
      btSet:
        begin
          elSize := aType.RealSize;
          for i := 0 to Len - 1 do
          begin
            Move(Src^, Dest^, elSize);
            Dest := Pointer(IPointer(Dest) + elsize);
            Src := Pointer(IPointer(Src) + elsize);
          end;
        end;
      {$IFNDEF PS_NOINTERFACES}
      btInterface:
        begin
          for i := 0 to Len - 1 do
          begin
            {$IFNDEF DELPHI3UP}
            if IUnknown(Dest^) <> nil then
            begin
              IUnknown(Dest^).Release;
              IUnknown(Dest^) := nil;
            end;
            {$ENDIF}
            IUnknown(Dest^) := IUnknown(Src^);
            {$IFNDEF DELPHI3UP}
            if IUnknown(Dest^) <> nil then
              IUnknown(Dest^).AddRef;
            {$ENDIF}
            Dest := Pointer(IPointer(Dest) + PointerSize);
            Src := Pointer(IPointer(Src) + PointerSize);
          end;
        end;
      {$ENDIF}
      btPointer:
        begin
          if (Pointer(Pointer(IPointer(Dest) + PointerSize2)^) = nil) and
             (Pointer(Pointer(IPointer(Src) + PointerSize2)^) = nil) then
          begin
            for i := 0 to Len - 1 do
            begin
              Pointer(Dest^) := Pointer(Src^);
              Dest := Pointer(IPointer(Dest) + PointerSize);
              Src := Pointer(IPointer(Src) + PointerSize);
              Pointer(Dest^) := Pointer(Src^);
              Dest := Pointer(IPointer(Dest) + PointerSize);
              Src := Pointer(IPointer(Src) + PointerSize);
              LongBool(Dest^) := False;
              Dest := Pointer(IPointer(Dest) + SizeOf(LongBool));
              Src := Pointer(IPointer(Src) + SizeOf(LongBool));
            end;
          end
          else
          begin
            for i := 0 to Len - 1 do
            begin
              if Pointer(Pointer(IPointer(Dest) + PointerSize2)^) <> nil then
                DestroyHeapVariant2(Pointer(Dest^), Pointer(Pointer(IPointer(Dest) + PointerSize)^));
              if Pointer(Src^) <> nil then
              begin
                if not LongBool(Pointer(IPointer(Src) + PointerSize2)^) then
                begin
                  Pointer(Dest^) := Pointer(Src^);
                  Pointer(Pointer(IPointer(Dest) + PointerSize)^) := Pointer(Pointer(IPointer(Src) + PointerSize)^);
                  Pointer(Pointer(IPointer(Dest) + PointerSize2)^) := Pointer(Pointer(IPointer(Src) + PointerSize2)^);
                end
                else
                begin
                  Pointer(Dest^) := CreateHeapVariant2(Pointer(Pointer(IPointer(Src) + PointerSize)^));
                  Pointer(Pointer(IPointer(Dest) + PointerSize)^) := Pointer(Pointer(IPointer(Src) + PointerSize)^);
                  LongBool(Pointer(IPointer(Dest) + PointerSize2)^) := True;
                  if not CopyArrayContents(Pointer(Dest^), Pointer(Src^), 1, Pointer(Pointer(IPointer(Dest) + PointerSize)^)) then
                  begin
                    Result := False;
                    Exit;
                  end;
                end;
              end
              else
              begin
                Pointer(Dest^) := nil;
                Pointer(Pointer(IPointer(Dest) + PointerSize)^) := nil;
                Pointer(Pointer(IPointer(Dest) + PointerSize2)^) := nil;
              end;
              Dest := Pointer(IPointer(Dest) + PointerSize * 2 + SizeOf(LongBool));
              Src := Pointer(IPointer(Src) + PointerSize * 2 + SizeOf(LongBool));
            end;
          end;
        end;
//      btResourcePointer = 15;
//      btVariant = 16;
    else
      Result := False;
      Exit;
    end;
  except
    Result := False;
    Exit;
  end;
  Result := True;
end;

//==============================================================================
//
//  GetPSArrayLength
//
//==============================================================================
function GetPSArrayLength(Arr: PIFVariant): Longint;
begin
  Result := PSDynArrayGetLength(PPSVariantDynamicArray(arr).Data, arr.FType);
end;

//==============================================================================
//
// SetPSArrayLength
//
//==============================================================================
procedure SetPSArrayLength(Arr: PIFVariant; NewLength: Longint);
begin
  PSDynArraySetLength(PPSVariantDynamicArray(arr).Data, arr.FType, NewLength);
end;

//==============================================================================
//
// PSDynArrayGetLength
//
//==============================================================================
function PSDynArrayGetLength(arr: Pointer; aType: TPSTypeRec): Longint;
begin
  if aType.BaseType <> btArray then
    raise Exception.Create(RPS_InvalidArray);
  if arr = nil then
    Result := 0
  else
    Result := PDynArrayRec(PAnsiChar(arr) - SizeOf(TDynArrayRecHeader))^.header.{$IFDEF FPC}high + 1{$ELSE}Length{$ENDIF FPC};
end;

//==============================================================================
//
// PSDynArraySetLength
//
//==============================================================================
procedure PSDynArraySetLength(var arr: Pointer; aType: TPSTypeRec; NewLength: Longint);
var
  elSize, i, OldLen: Longint;
  darr: PDynArrayRec;
begin
  if aType.BaseType <> btArray then
    raise Exception.Create(RPS_InvalidArray);
  OldLen := PSDynArrayGetLength(arr, aType);
  elSize := TPSTypeRec_Array(aType).ArrayType.RealSize;
  if NewLength < 0 then
     NewLength := 0;
  if (OldLen = NewLength) then
    Exit; // already same size, noop
  darr := PDynArrayRec(PAnsiChar(Arr) - SizeOf(TDynArrayRecHeader));
  if (OldLen <> 0) and (darr^.header.refCnt = 1) then // unique Copy of this dynamic array
  begin
    for i := NewLength to OldLen - 1 do
    begin
      if TPSTypeRec_Array(aType).ArrayType.BaseType in NeedFinalization then
        FinalizeVariant(Pointer(IPointer(arr) + Cardinal(elsize * i)), TPSTypeRec_Array(aType).ArrayType);
    end;
    if NewLength <= 0 then
    begin
      FreeMem(darr);
      arr := nil;
      Exit;
    end;
    ReallocMem(darr, Longint(NewLength * elSize) + SizeOf(TDynArrayRecHeader));
    {$IFDEF FPC}
    darr^.header.high := NewLength  - 1;
    {$ELSE}
    darr^.header.length := NewLength;
    {$ENDIF}
    arr := @darr^.datas;
    for i := OldLen to NewLength - 1 do
    begin
      InitializeVariant(Pointer(IPointer(arr) + Cardinal(elsize * i)), TPSTypeRec_Array(aType).ArrayType);
    end;
  end
  else
  begin
    if NewLength = 0 then
    begin
      FinalizeVariant(@arr, aType);
      arr := nil;
      Exit;
    end;
    GetMem(darr, Longint(NewLength * elSize) + SizeOf(TDynArrayRecHeader));
    darr^.header.refCnt := 1;
    {$IFDEF FPC}
    darr^.header.high := NewLength - 1;
    {$ELSE}
    {$IFDEF CPUX64}
    darr^.header._Padding := 0;
    {$ENDIF CPUX64}
    darr^.header.length := NewLength;
    {$ENDIF FPC}
    for i := 0 to NewLength - 1 do
    begin
      InitializeVariant(Pointer(IPointer(@darr^.datas) + Cardinal(elsize * i)), TPSTypeRec_Array(aType).ArrayType);
    end;
    if OldLen <> 0 then
    begin
      if OldLen > NewLength then
        CopyArrayContents(@darr^.datas, arr, NewLength, TPSTypeRec_Array(aType).ArrayType)
      else
        CopyArrayContents(@darr^.datas, arr, OldLen, TPSTypeRec_Array(aType).ArrayType);
      FinalizeVariant(@arr, aType);
    end;
    arr := @darr^.datas;
  end;
end;

{$IFDEF FPC}
{$DEFINE FPC_OR_KYLIX}
{$ENDIF}
{$IFDEF KYLIX}
{$DEFINE FPC_OR_KYLIX}
{$ENDIF}

{$IFDEF FPC_OR_KYLIX}

//==============================================================================
//
// OleErrorMessage
//
//==============================================================================
function OleErrorMessage(ErrorCode: HResult): TbtString;
begin
  Result := SysErrorMessage(ErrorCode);
  if Result = '' then
    Result := Format(RPS_OLEError, [ErrorCode]);
end;

//==============================================================================
//
// OleError
//
//==============================================================================
procedure OleError(ErrorCode: HResult);
begin
  raise Exception.Create(OleErrorMessage(ErrorCode));
end;

//==============================================================================
//
// OleCheck
//
//==============================================================================
procedure OleCheck(Result: HResult);
begin
  if Result < 0 then OleError(Result);
end;
{$ENDIF}

{$IFNDEF DELPHI3UP}

//==============================================================================
//
// OleErrorMessage
//
//==============================================================================
function OleErrorMessage(ErrorCode: HResult): TbtString;
begin
  Result := SysErrorMessage(ErrorCode);
  if Result = '' then
    Result := Format(RPS_OLEError, [ErrorCode]);
end;

//==============================================================================
//
// OleError
//
//==============================================================================
procedure OleError(ErrorCode: HResult);
begin
  raise Exception.Create(OleErrorMessage(ErrorCode));
end;

//==============================================================================
//
// OleCheck
//
//==============================================================================
procedure OleCheck(Result: HResult);
begin
  if Result < 0 then OleError(Result);
end;

//==============================================================================
//
// AssignInterface
//
//==============================================================================
procedure AssignInterface(var Dest: IUnknown; const Src: IUnknown);
var
  OldDest: IUnknown;
begin
  { Like Delphi 3+'s _IntfCopy, reference source before releasing old dest.
    so that Self assignment (I := I) works right }
  OldDest := Dest;
  Dest := Src;
  if Src <> nil then
    Src.AddRef;
  if OldDest <> nil then
    OldDest.Release;
end;

//==============================================================================
//
// AssignVariantFromIDispatch
//
//==============================================================================
procedure AssignVariantFromIDispatch(var Dest: Variant; const Src: IDispatch);
begin
  VarClear(Dest);
  TVarData(Dest).VDispatch := Src;
  TVarData(Dest).VType := varDispatch;
  if Src <> nil then
    Src.AddRef;
end;

//==============================================================================
//
// AssignIDispatchFromVariant
//
//==============================================================================
procedure AssignIDispatchFromVariant(var Dest: IDispatch; const Src: Variant);
const
  RPS_InvalidVariantRef = 'Invalid variant reference';
var
  NewDest: IDispatch;
begin
  case TVarData(Src).VType of
    varEmpty:
      NewDest := nil;
    varDispatch:
      NewDest := TVarData(Src).VDispatch;
    varDispatch or varByRef:
      NewDest := Pointer(TVarData(Src).VPointer^);
  else
    raise Exception.Create(RPS_InvalidVariantRef);
  end;
  AssignInterface(IUnknown(Dest), NewDest);
end;
{$ENDIF}

//==============================================================================
//
// TPSExec.SetVariantValue
//
//==============================================================================
function TPSExec.SetVariantValue(dest, Src: Pointer; desttype, srctype: TPSTypeRec): Boolean;
var
  Tmp: TObject;
  tt: TPSVariantPointer;
begin
  Result := True;
  try
    case desttype.BaseType of
      btSet:
        begin
          if desttype = srctype then
            Move(Src^, Dest^, TPSTypeRec_Set(desttype).aByteSize)
          else
            Result := False;
        end;
      btU8:
        TbtU8(Dest^) := PSGetUInt(Src, srctype);
      btS8:
        TbtS8(Dest^) := PSGetInt(Src, srctype);
      btU16:
        TbtU16(Dest^) := PSGetUInt(Src, srctype);
      btS16:
        TbtS16(Dest^) := PSGetInt(Src, srctype);
      btProcPtr:
        begin
          if srctype.BaseType = btPointer then
          begin
            srctype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
            Src := Pointer(Src^);
            if (src = nil) or (srctype = nil) then
              raise Exception.Create(RPS_TypeMismatch);
          end;
          case srctype.BaseType of
            btU32:
              begin
                Pointer(Dest^) := Pointer(Src^);
              end;
            btProcPtr:
              begin
                Pointer(Dest^) := Pointer(Src^);
                Pointer(Pointer(IPointer(Dest) + PointerSize)^) := Pointer(Pointer(IPointer(Src) + PointerSize)^);
                Pointer(Pointer(IPointer(Dest) + PointerSize2)^) := Pointer(Pointer(IPointer(Src) + PointerSize2)^);
              end;
            else
              raise Exception.Create(RPS_TypeMismatch);
          end;
        end;
      btU32:
        begin
          if srctype.BaseType = btPointer then
          begin
            srctype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
            Src := Pointer(Src^);
            if (src = nil) or (srctype = nil) then
              raise Exception.Create(RPS_TypeMismatch);
          end;
          case srctype.BaseType of
            btU8:
              TbtU32(Dest^) := TbtU8(src^);
            btS8:
              TbtU32(Dest^) := TbtS8(src^);
            btU16:
              TbtU32(Dest^) := TbtU16(src^);
            btS16:
              TbtU32(Dest^) := TbtS16(src^);
            btU32:
              TbtU32(Dest^) := TbtU32(src^);
            btS32:
              TbtU32(Dest^) := TbtS32(src^);
            {$IFNDEF PS_NOINT64}
            btS64:
              TbtU32(Dest^) := TbtS64(src^);
            {$ENDIF}
            btChar:
              TbtU32(Dest^) := Ord(TbtChar(Src^));
            {$IFNDEF PS_NOWIDESTRING}
            btWideChar:
              TbtU32(Dest^) := Ord(TbtWideChar(Src^));
            {$ENDIF}
            btVariant:
              TbtU32(Dest^) := Variant(src^);
            else
              raise Exception.Create(RPS_TypeMismatch);
          end;
        end;
      btS32:
        begin
          if srctype.BaseType = btPointer then
          begin
            srctype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
            Src := Pointer(Src^);
            if (src = nil) or (srctype = nil) then
              raise Exception.Create(RPS_TypeMismatch);
          end;
          case srctype.BaseType of
            btU8:
              TbtS32(Dest^) := TbtU8(src^);
            btS8:
              TbtS32(Dest^) := TbtS8(src^);
            btU16:
              TbtS32(Dest^) := TbtU16(src^);
            btS16:
              TbtS32(Dest^) := TbtS16(src^);
            btU32:
              TbtS32(Dest^) := TbtU32(src^);
            btS32:
              TbtS32(Dest^) := TbtS32(src^);
            {$IFNDEF PS_NOINT64}
            btS64:
              TbtS32(Dest^) := TbtS64(src^);
            {$ENDIF}
            btChar:
              TbtS32(Dest^) := Ord(TbtChar(Src^));
            {$IFNDEF PS_NOWIDESTRING}
            btWideChar:
              TbtS32(Dest^) := Ord(TbtWideChar(Src^));
            {$ENDIF}
            btVariant:
              TbtS32(Dest^) := Variant(src^);
            // nx change start - allow assignment of class
            btClass:
              TbtU32(Dest^) := TbtU32(src^);
            // nx change start
            else
              raise Exception.Create(RPS_TypeMismatch);
          end;
        end;
      {$IFNDEF PS_NOINT64}
      btS64:
        TbtS64(Dest^) := PSGetInt64(Src, srctype);
      {$ENDIF}
      btSingle:
        begin
          if srctype.BaseType = btPointer then
          begin
            srctype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
            Src := Pointer(Src^);
            if (src = nil) or (srctype = nil) then
              raise Exception.Create(RPS_TypeMismatch);
          end;
          case srctype.BaseType of
            btU8:
              TbtSingle(Dest^) := TbtU8(src^);
            btS8:
              TbtSingle(Dest^) := TbtS8(src^);
            btU16:
              TbtSingle(Dest^) := TbtU16(src^);
            btS16:
              TbtSingle(Dest^) := TbtS16(src^);
            btU32:
              TbtSingle(Dest^) := TbtU32(src^);
            btS32:
              TbtSingle(Dest^) := TbtS32(src^);
            {$IFNDEF PS_NOINT64}
            btS64:
              TbtSingle(Dest^) := TbtS64(src^);
            {$ENDIF}
            btSingle:
              TbtSingle(Dest^) := TbtSingle(Src^);
            btDouble:
              TbtSingle(Dest^) := TbtDouble(Src^);
            btExtended:
              TbtSingle(Dest^) := TbtExtended(Src^);
            btCurrency:
              TbtSingle(Dest^) := TbtCurrency(Src^);
            btVariant:
              TbtSingle(Dest^) := Variant(src^);
            else
              raise Exception.Create(RPS_TypeMismatch);
          end;
        end;
      btDouble:
        begin
          if srctype.BaseType = btPointer then
          begin
            srctype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
            Src := Pointer(Src^);
            if (src = nil) or (srctype = nil) then
              raise Exception.Create(RPS_TypeMismatch);
          end;
          case srctype.BaseType of
            btU8:
              TbtDouble(Dest^) := TbtU8(src^);
            btS8:
              TbtDouble(Dest^) := TbtS8(src^);
            btU16:
              TbtDouble(Dest^) := TbtU16(src^);
            btS16:
              TbtDouble(Dest^) := TbtS16(src^);
            btU32:
              TbtDouble(Dest^) := TbtU32(src^);
            btS32:
              TbtDouble(Dest^) := TbtS32(src^);
            {$IFNDEF PS_NOINT64}
            btS64:
              TbtDouble(Dest^) := TbtS64(src^);
            {$ENDIF}
            btSingle:
              TbtDouble(Dest^) := TbtSingle(Src^);
            btDouble:
              TbtDouble(Dest^) := TbtDouble(Src^);
            btExtended:
              TbtDouble(Dest^) := TbtExtended(Src^);
            btCurrency:
              TbtDouble(Dest^) := TbtCurrency(Src^);
            btVariant:
              TbtDouble(Dest^) := Variant(src^);
            else
              raise Exception.Create(RPS_TypeMismatch);
          end;

        end;
      btExtended:
        begin
          if srctype.BaseType = btPointer then
          begin
            srctype := PIFTypeRec(Pointer(IPointer(Src) + PointerSize)^);
            Src := Pointer(Src^);
            if (src = nil) or (srctype = nil) then
              raise Exception.Create(RPS_TypeMismatch);
          end;
          case srctype.BaseType of
            btU8:
              TbtExtended(Dest^) := TbtU8(src^);
            btS8:
              TbtExtended(Dest^) := TbtS8(src^);
            btU16:
              TbtExtended(Dest^) := TbtU16(src^);
            btS16:
              TbtExtended(Dest^) := TbtS16(src^);
            btU32:
              TbtExtended(Dest^) := TbtU32(src^);
            btS32:
              TbtExtended(Dest^) := TbtS32(src^);
            {$IFNDEF PS_NOINT64}
            btS64:
              TbtExtended(Dest^) := TbtS64(src^);
            {$ENDIF}
            btSingle:
              TbtExtended(Dest^) := TbtSingle(Src^);
            btDouble:
              TbtExtended(Dest^) := TbtDouble(Src^);
            btExtended:
              TbtExtended(Dest^) := TbtExtended(Src^);
            btCurrency:
              TbtExtended(Dest^) := TbtCurrency(Src^);
            btVariant:
              TbtExtended(Dest^) := Variant(src^);
            else
              raise Exception.Create(RPS_TypeMismatch);
          end;
        end;
      btCurrency: TbtCurrency(Dest^) := PSGetCurrency(Src, srctype);
      btPChar: PAnsiChar(dest^) := PAnsiChar(PSGetAnsiString(Src, srctype));
      btString:
        TbtString(dest^) := PSGetAnsiString(Src, srctype);
      btChar: TbtChar(dest^) := TbtChar(PSGetUInt(Src, srctype));
      {$IFNDEF PS_NOWIDESTRING}
      btWideString: TbtWideString(dest^) := PSGetWideString(Src, srctype);
      btUnicodeString: TbtUnicodeString(dest^) := PSGetUnicodeString(Src, srctype);
      btWideChar: TbtWideChar(dest^) := widechar(PSGetUInt(Src, srctype));
      {$ENDIF}
      btStaticArray:
        begin
          if desttype <> srctype then
            Result := False
          else
            CopyArrayContents(dest, Src, TPSTypeRec_StaticArray(desttype).Size, TPSTypeRec_StaticArray(desttype).ArrayType);
        end;
      btArray:
        begin
          if (srctype.BaseType = btStaticArray) and (TPSTypeRec_Array(desttype).ArrayType = TPSTypeRec_Array(srctype).ArrayType) then
          begin
            PSDynArraySetLength(Pointer(Dest^), desttype, TPSTypeRec_StaticArray(srctype).Size);
            CopyArrayContents(Pointer(dest^), Src, TPSTypeRec_StaticArray(srctype).Size, TPSTypeRec_StaticArray(srctype).ArrayType);
          end
          else if (srctype.BaseType = btvariant) and VarIsArray(Variant(src^)) then
            Result := CreateArrayFromVariant(Self, dest, Variant(src^), desttype)
          else if (desttype <> srctype) and not ((desttype.BaseType = btarray) and (srctype.BaseType = btArray)
            and (TPSTypeRec_Array(desttype).ArrayType = TPSTypeRec_Array(srctype).ArrayType)) then
            Result := False
          else
            CopyArrayContents(dest, src, 1, desttype);
        end;
      btRecord:
        begin
          if desttype <> srctype then
            Result := False
          else
            CopyArrayContents(dest, Src, 1, desttype);
        end;
      btVariant:
        begin
          {$IFNDEF PS_NOINTERFACES}
          if srctype.ExportName = 'IDISPATCH' then
          begin
            {$IFDEF DELPHI3UP}
            Variant(Dest^) := IDispatch(Src^);
            {$ELSE}
            AssignVariantFromIDispatch(Variant(Dest^), IDispatch(Src^));
            {$ENDIF}
          end
          else
          {$ENDIF}
          if srctype.BaseType = btVariant then
            variant(Dest^) := variant(src^)
          else
          begin
            tt.VI.FType := FindType2(btPointer);
            tt.DestType := srctype;
            tt.DataDest := src;
            tt.FreeIt := False;
            Result := PIFVariantToVariant(@tt, variant(dest^));
          end;
        end;
      btClass:
        begin
          if srctype.BaseType = btClass then
            TObject(Dest^) := TObject(Src^)
          else
          // nx change start
          if (srctype.BaseType in [btS32, btU32]) then
            TbtU32(Dest^) := TbtU32(Src^)
          else
          // nx change end
            Result := False;
        end;
      {$IFNDEF PS_NOINTERFACES}
      btInterface:
        begin
          if Srctype.BaseType = btVariant then
          begin
            if desttype.ExportName = 'IDISPATCH' then
            begin
              {$IFDEF Delphi3UP}
              IDispatch(Dest^) := IDispatch(Variant(Src^));
              {$ELSE}
              AssignIDispatchFromVariant(IDispatch(Dest^), Variant(Src^));
              {$ENDIF}
            end
            else
              Result := False;
          {$IFDEF Delphi3UP}
          end
          else if srctype.BaseType = btClass then
          begin
            if (TObject(Src^) = nil) or not TObject(Src^).GetInterface(TPSTypeRec_Interface(desttype).Guid, IUnknown(Dest^)) then
            begin
              Result := False;
              CMD_Err(erInterfaceNotSupported);
              Exit;
            end;
          {$ENDIF}
          end
          else if srctype.BaseType = btInterface then
          begin
            {$IFNDEF Delphi3UP}
            if IUnknown(Dest^) <> nil then
            begin
              IUnknown(Dest^).Release;
              IUnknown(Dest^) := nil;
            end;
            {$ENDIF}
            IUnknown(Dest^) := IUnknown(Src^);
            {$IFNDEF Delphi3UP}
            if IUnknown(Dest^) <> nil then
              IUnknown(Dest^).AddRef;
            {$ENDIF}
          end
          else
            Result := False;
        end;
        {$ENDIF}
    else
      begin
        Result := False;
      end;
    end;
    if Result = False then
      CMD_Err(erTypeMismatch);
  except
    {$IFDEF DELPHI6UP}
    Tmp := AcquireExceptionObject;
    {$ELSE}
    if RaiseList <> nil then
    begin
      Tmp := Exception(PRaiseFrame(RaiseList)^.ExceptObject);
      PRaiseFrame(RaiseList)^.ExceptObject := nil;
    end
    else
      Tmp := nil;
    {$ENDIF}
    if Tmp <> nil then
    begin
      if Tmp is EPSException then
      begin
        Result := False;
        ExceptionProc(EPSException(tmp).ProcNo, EPSException(tmp).ProcPos, erCustomError, TbtString(EPSException(tmp).Message), nil);
        Exit;
      end
      else
      if Tmp is EDivByZero then
      begin
        Result := False;
        CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
        Exit;
      end;
      if Tmp is EZeroDivide then
      begin
        Result := False;
        CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
        Exit;
      end;
      if Tmp is EMathError then
      begin
        Result := False;
        CMD_Err3(erMathError, TbtString(Exception(Tmp).Message), Tmp);
        Exit;
      end;
    end;
    if (tmp <> nil) and (Tmp is Exception) then
      CMD_Err3(erException, TbtString(Exception(Tmp).Message), Tmp)
    else
      CMD_Err3(erException, '', Tmp);
    Result := False;
  end;
end;

//==============================================================================
//
// SpecImport
//
//==============================================================================
function SpecImport(Sender: TPSExec; p: TPSExternalProcRec; Tag: Pointer): Boolean; forward;

//==============================================================================
//
// Class_IS
//
//==============================================================================
function Class_IS(Self: TPSExec; Obj: TObject; var2type: TPSTypeRec): Boolean;
var
  R: TPSRuntimeClassImporter;
  cc: TPSRuntimeClass;
begin
  if Obj = nil then
  begin
    Result := False;
    Exit;
  end;
  r := Self.FindSpecialProcImport(SpecImport);
  if R = nil then
  begin
    Result := False;
    Exit;
  end;
  cc := r.FindClass(var2type.ExportName);
  if cc = nil then
  begin
    Result := False;
    Exit;
  end;
  try
    Result := Obj is cc.FClass;
  except
    Result := False;
  end;
end;

type
  TVariantArray = array of Variant;
  PVariantArray = ^TVariantArray;

//==============================================================================
//
// VariantInArray
//
//==============================================================================
function VariantInArray(var1: Pointer; var1Type: TPSTypeRec; var2: PVariantArray): Boolean;
var
  lDest: Variant;
  i: Integer;
begin
  IntPIFVariantToVariant(var1, var1Type, lDest);
  Result := False;
  for i := 0 to Length(var2^) - 1 do
  begin
    if var2^[i] = lDest then
    begin
      Result := True;
      break;
    end;
  end;
end;

//==============================================================================
//
// TPSExec.DoBooleanCalc
//
//==============================================================================
function TPSExec.DoBooleanCalc(var1, Var2, into: Pointer; var1Type, var2type, intotype: TPSTypeRec; Cmd: Cardinal): Boolean;
var
  b: Boolean;
  Tmp: TObject;
  tvar: Variant;

  procedure SetBoolean(b: Boolean; var OK: Boolean);
  begin
    OK := True;
    case IntoType.BaseType of
      btU8:
        TbtU8(Into^):= Cardinal(b);
      btS8:
        TbtS8(Into^) := Longint(b);
      btU16:
        TbtU16(Into^) := Cardinal(b);
      btS16:
        TbtS16(Into^) := Longint(b);
      btU32:
        TbtU32(Into^) := Cardinal(b);
      btS32:
        TbtS32(Into^) := Longint(b);
      btVariant:
        Variant(Into^) := b;
      else
        begin
          CMD_Err(erTypeMismatch);
          OK := False;
        end;
    end;
  end;

begin
  Result := True;
  try
    case Cmd of
      0:
        begin { >= }
          case var1Type.BaseType of
            btU8:
              if (var2Type.BaseType = btString) or (Var2Type.BaseType = btPChar) then
                b := TbtChar(TbtU8(var1^)) >= PSGetAnsiString(Var2, var2type)
              else
                b := TbtU8(var1^) >= PSGetUInt(Var2, var2type);
            btS8:
              b := TbtS8(var1^) >= PSGetInt(Var2, var2type);
            btU16:
              b := TbtU16(var1^) >= PSGetUInt(Var2, var2type);
            btS16:
              b := TbtS16(var1^) >= PSGetInt(Var2, var2type);
            btU32:
              b := TbtU32(var1^) >= PSGetUInt(Var2, var2type);
            btS32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    b := TbtS32(var1^) >= TbtU8(Var2^);
                  btS8:
                    b := TbtS32(var1^) >= TbtS8(Var2^);
                  btU16:
                    b := TbtS32(var1^) >= TbtU16(Var2^);
                  btS16:
                    b := TbtS32(var1^) >= TbtS16(Var2^);
                  btU32:
                    b := TbtS32(var1^) >= Longint(TbtU32(Var2^));
                  btS32:
                    b := TbtS32(var1^) >= TbtS32(Var2^);
                  btDouble:
                    b := PSGetReal(Var1, var1type) >= TbtDouble(var2^);
                  btSingle:
                    B := psGetReal(Var1, var1Type) >= TbtSingle(var2^);
                  btExtended:
                    B := psGetReal(Var1, var1Type) >= TbtExtended(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    b := TbtS32(var1^) >= TbtS64(Var2^);
                  {$ENDIF}
                  btChar:
                    b := TbtS32(var1^) >= Ord(TbtChar(Var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar:
                    b := TbtS32(var1^) >= Ord(TbtWideChar(Var2^));
                  {$ENDIF}
                  btVariant:
                    b := TbtS32(var1^) >= Variant(Var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btSingle:
              b := TbtSingle(var1^) >= PSGetReal(Var2, var2type);
            btDouble:
              b := TbtDouble(var1^) >= PSGetReal(Var2, var2type);
            btCurrency:
              b := TbtCurrency(var1^) >= PSGetCurrency(Var2, var2type);
            btExtended:
              b := TbtExtended(var1^) >= PSGetReal(Var2, var2type);
            {$IFNDEF PS_NOINT64}
            btS64:
              b := TbtS64(var1^) >= PSGetInt64(Var2, var2type);
            {$ENDIF}
            btPChar,
            btString:
              b := TbtString(var1^) >= PSGetAnsiString(Var2, var2type);
            btChar:
              b := TbtChar(var1^) >= PSGetAnsiString(Var2, var2type);
            {$IFNDEF PS_NOWIDESTRING}
            btWideChar:
              b := TbtWideChar(var1^) >= PSGetWideString(Var2, var2type);
            btWideString:
              b := TbtWideString(var1^) >= PSGetWideString(Var2, var2type);
            btUnicodeString:
              b := TbtUnicodeString(var1^) >= PSGetUnicodeString(Var2, var2type);
            {$ENDIF}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end
                else
                  b := Variant(var1^) >= tvar;
              end;
            btSet:
              begin
                if var1Type = var2Type then
                begin
                  Set_Subset(var2, var1, TPSTypeRec_Set(var1Type).aByteSize, b);
                end
                else
                  Result := False;
              end;
          else
            begin
              CMD_Err(erTypeMismatch);
              Exit;
            end;
          end;
          if not Result then
          begin
            CMD_Err(erTypeMismatch);
            Exit;
          end;
          SetBoolean(b, Result);
        end;
      1:
        begin { <= }
          case var1Type.BaseType of
            btU8:
              if (var2Type.BaseType = btString) or (Var2Type.BaseType = btPChar) then
                b := TbtChar(TbtU8(var1^)) <= PSGetAnsiString(Var2, var2type)
              else
                b := TbtU8(var1^) <= PSGetUInt(Var2, var2type);
            btS8:
              b := TbtS8(var1^) <= PSGetInt(Var2, var2type);
            btU16:
              b := TbtU16(var1^) <= PSGetUInt(Var2, var2type);
            btS16:
              b := TbtS16(var1^) <= PSGetInt(Var2, var2type);
            btU32:
              b := TbtU32(var1^) <= PSGetUInt(Var2, var2type);
            btS32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    b := TbtS32(var1^) <= TbtU8(Var2^);
                  btS8:
                    b := TbtS32(var1^) <= TbtS8(Var2^);
                  btU16:
                    b := TbtS32(var1^) <= TbtU16(Var2^);
                  btS16:
                    b := TbtS32(var1^) <= TbtS16(Var2^);
                  btU32:
                    b := TbtS32(var1^) <= Longint(TbtU32(Var2^));
                  btS32:
                    b := TbtS32(var1^) <= TbtS32(Var2^);
                  btDouble:
                    b := PSGetReal(Var1, var1type) <= TbtDouble(var2^);
                  btSingle:
                    b := psGetReal(Var1, var1Type) <= TbtSingle(var2^);
                  btExtended:
                    b := psGetReal(Var1, var1Type) <= TbtExtended(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    b := TbtS32(var1^) <= TbtS64(Var2^);
                  {$ENDIF}
                  btChar:
                    b := TbtS32(var1^) <= Ord(TbtChar(Var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar:
                    b := TbtS32(var1^) <= Ord(TbtWideChar(Var2^));
                  {$ENDIF}
                  btVariant:
                    b := TbtS32(var1^) <= Variant(Var2^);
                else
                  raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btSingle:
              b := TbtSingle(var1^) <= PSGetReal(Var2, var2type);
            btCurrency:
              b := TbtCurrency(var1^) <= PSGetCurrency(Var2, var2type);
            btDouble:
              b := TbtDouble(var1^) <= PSGetReal(Var2, var2type);
            btExtended:
              b := TbtExtended(var1^) <= PSGetReal(Var2, var2type);
            {$IFNDEF PS_NOINT64}
            btS64:
              b := TbtS64(var1^) <= PSGetInt64(Var2, var2type);
            {$ENDIF}
            btPChar,
            btString:
              b := TbtString(var1^) <= PSGetAnsiString(Var2, var2type);
            btChar:
              b := TbtChar(var1^) <= PSGetAnsiString(Var2, var2type);
            {$IFNDEF PS_NOWIDESTRING}
            btWideChar:
              b := TbtWideChar(var1^) <= PSGetWideString(Var2, var2type);
            btWideString:
              b := TbtWideString(var1^) <= PSGetWideString(Var2, var2type);
            btUnicodeString:
              b := TbtUnicodeString(var1^) <= PSGetUnicodeString(Var2, var2type);
            {$ENDIF}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end
                else
                  b := Variant(var1^) <= tvar;
              end;
            btSet:
              begin
                if var1Type = var2Type then
                begin
                  Set_Subset(var1, var2, TPSTypeRec_Set(var1Type).aByteSize, b);
                end
                else
                  Result := False;
              end;
          else
            begin
              CMD_Err(erTypeMismatch);
              Exit;
            end;
          end;
          if not Result then
          begin
            CMD_Err(erTypeMismatch);
            Exit;
          end;
          SetBoolean(b, Result);
        end;
      2:
        begin { > }
          case var1Type.BaseType of
            btU8:
              if (var2Type.BaseType = btString) or (Var2Type.BaseType = btPChar) then
                b := TbtChar(TbtU8(var1^)) > PSGetAnsiString(Var2, var2type)
              else
                b := TbtU8(var1^) > PSGetUInt(Var2, var2type);
            btS8:
              b := TbtS8(var1^) > PSGetInt(Var2, var2type);
            btU16:
              b := TbtU16(var1^) > PSGetUInt(Var2, var2type);
            btS16:
              b := TbtS16(var1^) > PSGetInt(Var2, var2type);
            btU32:
              b := TbtU32(var1^) > PSGetUInt(Var2, var2type);
            btS32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    b := TbtS32(var1^) > TbtU8(Var2^);
                  btS8:
                    b := TbtS32(var1^) > TbtS8(Var2^);
                  btU16:
                    b := TbtS32(var1^) > TbtU16(Var2^);
                  btS16:
                    b := TbtS32(var1^) > TbtS16(Var2^);
                  btU32:
                    b := TbtS32(var1^) > Longint(TbtU32(Var2^));
                  btS32:
                    b := TbtS32(var1^) > TbtS32(Var2^);
                  btDouble:
                    b := PSGetReal(Var1, var1type) > TbtDouble(var2^);
                  btSingle:
                    b := psGetReal(Var1, var1Type) > TbtSingle(var2^);
                  btExtended:
                    b := psGetReal(Var1, var1Type) > TbtExtended(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    b := TbtS32(var1^) > TbtS64(Var2^);
                  {$ENDIF}
                  btChar:
                    b := TbtS32(var1^) > Ord(TbtChar(Var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar:
                    b := TbtS32(var1^) = Ord(TbtWideChar(Var2^));
                  {$ENDIF}
                  btVariant:
                    b := TbtS32(var1^) > Variant(Var2^);
                else
                  raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btSingle:
              b := TbtSingle(var1^) > PSGetReal(Var2, var2type);
            btDouble:
              b := TbtDouble(var1^) > PSGetReal(Var2, var2type);
            btExtended:
              b := TbtExtended(var1^) > PSGetReal(Var2, var2type);
            btCurrency:
              b := TbtCurrency(var1^) > PSGetCurrency(Var2, var2type);
            {$IFNDEF PS_NOINT64}
            btS64:
              b := TbtS64(var1^) > PSGetInt64(Var2, var2type);
            {$ENDIF}
            btPChar,
            btString:
              b := TbtString(var1^) > PSGetAnsiString(Var2, var2type);
            btChar:
              b := TbtChar(var1^) > PSGetAnsiString(Var2, var2type);
            {$IFNDEF PS_NOWIDESTRING}
            btWideChar:
              b := TbtWideChar(var1^) > PSGetWideString(Var2, var2type);
            btWideString:
              b := TbtWideString(var1^) > PSGetWideString(Var2, var2type);
            btUnicodeString:
              b := TbtUnicodeString(var1^) > PSGetUnicodeString(Var2, var2type);
            {$ENDIF}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end
                else
                  b := Variant(var1^) > tvar;
              end;
          else
            begin
              CMD_Err(erTypeMismatch);
              Exit;
            end;
          end;
          if not Result then
          begin
            CMD_Err(erTypeMismatch);
            Exit;
          end;
          SetBoolean(b, Result);
        end;
      3:
        begin { < }
          case var1Type.BaseType of
            btU8:
              if (var2Type.BaseType = btString) or (Var2Type.BaseType = btPChar) then
                b := TbtChar(TbtU8(var1^)) < PSGetAnsiString(Var2, var2type)
              else
                b := TbtU8(var1^) < PSGetUInt(Var2, var2type);
            btS8:
              b := TbtS8(var1^) < PSGetInt(Var2, var2type);
            btU16:
              b := TbtU16(var1^) < PSGetUInt(Var2, var2type);
            btS16:
              b := TbtS16(var1^) < PSGetInt(Var2, var2type);
            btU32:
              b := TbtU32(var1^) < PSGetUInt(Var2, var2type);
            btS32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    b := TbtS32(var1^) < TbtU8(Var2^);
                  btS8:
                    b := TbtS32(var1^) < TbtS8(Var2^);
                  btU16:
                    b := TbtS32(var1^) < TbtU16(Var2^);
                  btS16:
                    b := TbtS32(var1^) < TbtS16(Var2^);
                  btU32:
                    b := TbtS32(var1^) < Longint(TbtU32(Var2^));
                  btS32:
                    b := TbtS32(var1^) < TbtS32(Var2^);
                  btDouble:
                    b := PSGetReal(Var1, var1type) < TbtDouble(var2^);
                  btSingle:
                    b := psGetReal(Var1, var1Type) < TbtSingle(var2^);
                  btExtended:
                    b := psGetReal(Var1, var1Type) < TbtExtended(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    b := TbtS32(var1^) < TbtS64(Var2^);
                  {$ENDIF}
                  btChar:
                    b := TbtS32(var1^) < Ord(TbtChar(Var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar:
                    b := TbtS32(var1^) < Ord(TbtWideChar(Var2^));
                  {$ENDIF}
                  btVariant:
                    b := TbtS32(var1^) < Variant(Var2^);
                else
                  raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btSingle:
              b := TbtSingle(var1^) < PSGetReal(Var2, var2type);
            btDouble:
              b := TbtDouble(var1^) < PSGetReal(Var2, var2type);
            btCurrency:
              b := TbtCurrency(var1^) < PSGetCurrency(Var2, var2type);
            btExtended:
              b := TbtExtended(var1^) < PSGetReal(Var2, var2type);
            {$IFNDEF PS_NOINT64}
            btS64:
              b := TbtS64(var1^) < PSGetInt64(Var2, var2type);
            {$ENDIF}
            btPChar,
            btString:
              b := TbtString(var1^) < PSGetAnsiString(Var2, var2type);
            btChar:
              b := TbtChar(var1^) < PSGetAnsiString(Var2, var2type);
            {$IFNDEF PS_NOWIDESTRING}
            btWideChar:
              b := TbtWideChar(var1^) < PSGetWideString(Var2, var2type);
            btWideString:
              b := TbtWideString(var1^) < PSGetWideString(Var2, var2type);
            btUnicodeString:
              b := TbtUnicodeString(var1^) < PSGetUnicodeString(Var2, var2type);
            {$ENDIF}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end
                else
                  b := Variant(var1^) < tvar;
              end;
          else
            begin
              CMD_Err(erTypeMismatch);
              Exit;
            end;
          end;
          if not Result then
          begin
            CMD_Err(erTypeMismatch);
            Exit;
          end;
          SetBoolean(b, Result);
        end;
      4:
        begin { <> }
          case var1Type.BaseType of
            btInterface:
              begin
                if var2Type.BaseType = btInterface then
                  b := Pointer(var1^) <> Pointer(var2^) // no need to cast it to IUnknown
                else
                  Result := False;
              end;
            btClass:
              begin
                if var2Type.BaseType = btClass then
                  b := TObject(var1^) <> TObject(var2^)
                else
                  Result := False;
              end;
            btU8:
              if (var2Type.BaseType = btString) or (Var2Type.BaseType = btPChar) then
                b := TbtChar(TbtU8(var1^)) <> PSGetAnsiString(Var2, var2type)
              else
                b := TbtU8(var1^) <> PSGetUInt(Var2, var2type);
            btS8:
              b := TbtS8(var1^) <> PSGetInt(Var2, var2type);
            btU16:
              b := TbtU16(var1^) <> PSGetUInt(Var2, var2type);
            btS16:
              b := TbtS16(var1^) <> PSGetInt(Var2, var2type);
            btProcPtr:
              begin
                if Pointer(Var1^) = Pointer(Var2^) then
                begin
                  if Longint(Var1^) = 0 then
                    b := ((Pointer(Pointer(IPointer(Var1) + PointerSize2)^) <> Pointer(Pointer(IPointer(Var2) + PointerSize2)^)) or
                   (Pointer(Pointer(IPointer(Var1) + PointerSize2)^) <> Pointer(Pointer(IPointer(Var2) + PointerSize2)^)))
                  else
                    b := False;
                end
                else
                  b := True;
              end;
            btU32:
              b := TbtU32(var1^) <> PSGetUInt(Var2, var2type);
            btS32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    b := TbtS32(var1^) <> TbtU8(Var2^);
                  btS8:
                    b := TbtS32(var1^) <> TbtS8(Var2^);
                  btU16:
                    b := TbtS32(var1^) <> TbtU16(Var2^);
                  btS16:
                    b := TbtS32(var1^) <> TbtS16(Var2^);
                  btProcPtr,
                  btU32:
                    b := TbtS32(var1^)<> Longint(TbtU32(Var2^));
                  btS32:
                    b := TbtS32(var1^) <> TbtS32(Var2^);
                  btDouble:
                    b := PSGetReal(Var1, var1type) <> TbtDouble(var2^);
                  btSingle:
                    B := psGetReal(Var1, var1Type) <> TbtSingle(var2^);
                  btExtended:
                    B := psGetReal(Var1, var1Type) <> TbtExtended(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    b := TbtS32(var1^) <> TbtS64(Var2^);
                  {$ENDIF}
                  btChar:
                    b := TbtS32(var1^) <> Ord(TbtChar(Var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar:
                    b := TbtS32(var1^) <> Ord(TbtWideChar(Var2^));
                  {$ENDIF}
                  btVariant:
                    b := TbtS32(var1^) <> Variant(Var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btSingle:
              b := TbtSingle(var1^) <> PSGetReal(Var2, var2type);
            btDouble:
              b := TbtDouble(var1^) <> PSGetReal(Var2, var2type);
            btExtended:
              b := TbtExtended(var1^) <> PSGetReal(Var2, var2type);
            btCurrency:
              b := TbtCurrency(var1^) <> PSGetCurrency(Var2, var2type);
            btPChar,
            btString:
              b := TbtString(var1^) <> PSGetAnsiString(Var2, var2type);
            {$IFNDEF PS_NOINT64}
            btS64:
              b := TbtS64(var1^) <> PSGetInt64(Var2, var2type);
            {$ENDIF}
            btChar:
              b := TbtChar(var1^) <> PSGetAnsiString(Var2, var2type);
            {$IFNDEF PS_NOWIDESTRING}
            btWideChar:
              b := TbtWideChar(var1^) <> PSGetWideString(Var2, var2type);
            btWideString:
              b := TbtWideString(var1^) <> PSGetWideString(Var2, var2type);
            btUnicodeString:
              b := TbtUnicodeString(var1^) <> PSGetUnicodeString(Var2, var2type);
            {$ENDIF}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end
                else
                  b := Variant(var1^) <> tvar;
              end;
            btSet:
              begin
                if var1Type = var2Type then
                begin
                  Set_Equal(var1, var2, TPSTypeRec_Set(var1Type).aByteSize, b);
                  b := not b;
                end
                else
                  Result := False;
              end;
            btRecord:
              begin
                if var1Type = var2Type then
                begin
                  Set_Equal(var1, var2, TPSTypeRec_Record(var1Type).RealSize, b);
                  b := not b;
                end
                else
                  Result := False;
              end

          else
            begin
              CMD_Err(erTypeMismatch);
              Exit;
            end;
          end;
          if not Result then
          begin
            CMD_Err(erTypeMismatch);
            Exit;
          end;
          SetBoolean(b, Result);
        end;
      5:
        begin { = }
          case var1Type.BaseType of
            btInterface:
              begin
                if var2Type.BaseType = btInterface then
                  b := Pointer(var1^) = Pointer(var2^) // no need to cast it to IUnknown
                else
                  Result := False;
              end;
            btClass:
              begin
                if var2Type.BaseType = btClass then
                  b := TObject(var1^) = TObject(var2^)
                else
                  Result := False;
              end;
            btU8:
            if (var2Type.BaseType = btString) or (Var2Type.BaseType = btPChar) then
              b := TbtChar(TbtU8(var1^)) = PSGetAnsiString(Var2, var2type)
            else
              b := TbtU8(var1^) = PSGetUInt(Var2, var2type);
            btS8:
              b := TbtS8(var1^) = PSGetInt(Var2, var2type);
            btU16:
              b := TbtU16(var1^) = PSGetUInt(Var2, var2type);
            btS16:
              b := TbtS16(var1^) = PSGetInt(Var2, var2type);
            btU32:
              b := TbtU32(var1^) = PSGetUInt(Var2, var2type);
            btProcPtr:
              begin
                if Pointer(Var1^) = Pointer(Var2^) then
                begin
                  if Longint(Var1^) = 0 then
                    b := ((Pointer(Pointer(IPointer(Var1) + PointerSize2)^) = Pointer(Pointer(IPointer(Var2) + PointerSize2)^)) and
                   (Pointer(Pointer(IPointer(Var1) + PointerSize2)^) = Pointer(Pointer(IPointer(Var2) + PointerSize2)^)))
                  else
                    b := True;
                end
                else
                  b := False;
              end;
            btS32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    b := TbtS32(var1^) = TbtU8(Var2^);
                  btS8:
                    b := TbtS32(var1^) = TbtS8(Var2^);
                  btU16:
                    b := TbtS32(var1^) = TbtU16(Var2^);
                  btS16:
                    b := TbtS32(var1^) = TbtS16(Var2^);
                  btProcPtr,
                  btU32:
                    b := TbtS32(var1^) = Longint(TbtU32(Var2^));
                  btS32:
                    b := TbtS32(var1^) = TbtS32(Var2^);
                  btDouble:
                    b := PSGetReal(Var1, var1type) = TbtDouble(var2^);
                  btSingle:
                    b := psGetReal(Var1, var1Type) = TbtSingle(var2^);
                  btExtended:
                    b := psGetReal(Var1, var1Type) = TbtExtended(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    b := TbtS32(var1^) = TbtS64(Var2^);
                  {$ENDIF}
                  btChar:
                    b := TbtS32(var1^) = Ord(TbtChar(Var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar:
                    b := TbtS32(var1^) = Ord(TbtWideChar(Var2^));
                  {$ENDIF}
                  btVariant:
                    b := TbtS32(var1^) = Variant(Var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btSingle:
              b := TbtSingle(var1^) = PSGetReal(Var2, var2type);
            btDouble:
              b := TbtDouble(var1^) = PSGetReal(Var2, var2type);
            btExtended:
              b := TbtExtended(var1^) = PSGetReal(Var2, var2type);
            btCurrency:
              b := TbtCurrency(var1^) = PSGetCurrency(Var2, var2type);
            btPchar,
            btString:
              b := TbtString(var1^) = PSGetAnsiString(Var2, var2type);
            {$IFNDEF PS_NOINT64}
            btS64:
              b := TbtS64(var1^) = PSGetInt64(Var2, var2type);
            {$ENDIF}
            btChar:
              b := TbtChar(var1^) = PSGetAnsiString(Var2, var2type);
            {$IFNDEF PS_NOWIDESTRING}
            btWideChar:
              b := TbtWideChar(var1^) = PSGetWideString(Var2, var2type);
            btWideString:
              b := TbtWideString(var1^) = PSGetWideString(Var2, var2type);
            btUnicodeString:
              b := TbtUnicodeString(var1^) = PSGetUnicodeString(Var2, var2type);
            {$ENDIF}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end
                else
                  b := Variant(var1^) = tvar;
              end;
            btSet:
              begin
                if var1Type = var2Type then
                begin
                  Set_Equal(var1, var2, TPSTypeRec_Set(var1Type).aByteSize, b);
                end
                else
                  Result := False;
              end;
            btRecord:
              begin
                if var1Type = var2Type then
                begin
                  Set_Equal(var1, var2, TPSTypeRec_Record(var1Type).RealSize, b);
                end
                else
                  Result := False;
              end
          else
            begin
              CMD_Err(erTypeMismatch);
              Exit;
            end;
          end;
          if not Result then
          begin
            CMD_Err(erTypeMismatch);
            Exit;
          end;
          SetBoolean(b, Result);
        end;
      6:
        begin { in }
          if (var2Type.BaseType = btArray) and (TPSTypeRec_Array(var2type).ArrayType.BaseType = btVariant) then
          begin
            b := VariantInArray(var1, var1Type, var2);
            SetBoolean(b, Result);
          end
          else if var2Type.BaseType = btSet then
          begin
            Cmd := PSGetUInt(var1, var1type);
            if not Result then
            begin
              CMD_Err(erTypeMismatch);
              Exit;
            end;
            if Cmd >= Cardinal(TPSTypeRec_Set(var2Type).aBitSize) then
            begin
              CMD_Err(erOutofRecordRange);
              Result := False;
              Exit;
            end;
            Set_membership(Cmd, var2, b);
            SetBoolean(b, Result);
          end
          else
          begin
            CMD_Err(erTypeMismatch);
            Exit;
          end;
        end;
      7:
        begin // is
          case var1Type.BaseType of
            btClass:
              begin
                if var2type.BaseType <> btU32 then
                  Result := False
                else
                begin
                  var2type := FTypes[TbtU32(var2^)];
                  if (var2type = nil) or (var2type.BaseType <> btClass) then
                    Result := False
                  else
                  begin
                    Setboolean(Class_IS(Self, TObject(var1^), var2type), Result);
                  end;
                end;
              end;
          else
            begin
              CMD_Err(erTypeMismatch);
              Exit;
            end;
          end;
          if not Result then
          begin
            CMD_Err(erTypeMismatch);
            Exit;
          end;
        end;
    else
      begin
        Result := False;
        CMD_Err(erInvalidOpcodeParameter);
        Exit;
      end;
    end;
  except
    {$IFDEF DELPHI6UP}
    Tmp := AcquireExceptionObject;
    {$ELSE}
    if RaiseList <> nil then
    begin
      Tmp := Exception(PRaiseFrame(RaiseList)^.ExceptObject);
      PRaiseFrame(RaiseList)^.ExceptObject := nil;
    end
    else
      Tmp := nil;
    {$ENDIF}
    if Tmp <> nil then
    begin
      if Tmp is EPSException then
      begin
        Result := False;
        ExceptionProc(EPSException(tmp).ProcNo, EPSException(tmp).ProcPos, erCustomError, TbtString(EPSException(tmp).Message), nil);
        Exit;
      end
      else if Tmp is EDivByZero then
      begin
        Result := False;
        CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
        Exit;
      end;
      if Tmp is EZeroDivide then
      begin
        Result := False;
        CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
        Exit;
      end;
      if Tmp is EMathError then
      begin
        Result := False;
        CMD_Err3(erMathError, TbtString(Exception(Tmp).Message), Tmp);
        Exit;
      end;
    end;
    if (tmp <> nil) and (Tmp is Exception) then
      CMD_Err3(erException, TbtString(Exception(Tmp).Message), Tmp)
    else
      CMD_Err3(erException, '', Tmp);
    Result := False;
  end;
end;

//==============================================================================
//
// VarIsFloat
//
//==============================================================================
function VarIsFloat(const V: Variant): Boolean;
begin
  Result := VarType(V) in [varSingle, varDouble, varCurrency];
end;

//==============================================================================
//
// TPSExec.DoCalc
//
//==============================================================================
function TPSExec.DoCalc(var1, var2: Pointer; var1Type, var2type: TPSTypeRec; CalcType: Cardinal): Boolean;
    { var1=dest, var2=src }
var
  Tmp: TObject;
  tvar: Variant;
begin
  try
    Result := True;
    case CalcType of
      0:
        begin { + }
          case var1Type.BaseType of
            btU8:
              TbtU8(var1^) := TbtU8(var1^) + PSGetUInt(Var2, var2type);
            btS8:
              TbtS8(var1^) := TbtS8(var1^) + PSGetInt(var2, var2type);
            btU16:
              TbtU16(var1^) := TbtU16(var1^) + PSGetUInt(var2, var2type);
            btS16:
              TbtS16(var1^) := TbtS16(var1^) + PSGetInt(var2, var2type);
            btU32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtU32(var1^) := TbtU32(var1^) + TbtU8(var2^);
                  btS8:
                    TbtU32(var1^) := TbtU32(var1^) + Cardinal(Longint(TbtS8(var2^)));
                  btU16:
                    TbtU32(var1^) := TbtU32(var1^) + TbtU16(var2^);
                  btS16:
                    TbtU32(var1^) := TbtU32(var1^) + Cardinal(Longint(TbtS16(var2^)));
                  btU32:
                    TbtU32(var1^) := TbtU32(var1^) + TbtU32(var2^);
                  btS32:
                    TbtU32(var1^) := TbtU32(var1^) + Cardinal(TbtS32(var2^));
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtU32(var1^) := TbtU32(var1^) + TbtS64(var2^);
                  {$ENDIF}
                  btChar:
                    TbtU32(var1^) := TbtU32(var1^) +  Ord(TbtChar(var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar:
                    TbtU32(var1^) := TbtU32(var1^) + Ord(TbtWideChar(var2^));
                  {$ENDIF}
                  btVariant:
                    TbtU32(var1^) := TbtU32(var1^) + Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btS32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtS32(var1^) := TbtS32(var1^) + TbtU8(var2^);
                  btS8:
                    TbtS32(var1^) := TbtS32(var1^) + TbtS8(var2^);
                  btU16:
                    TbtS32(var1^) := TbtS32(var1^) + TbtU16(var2^);
                  btS16:
                    TbtS32(var1^) := TbtS32(var1^) + TbtS16(var2^);
                  btU32:
                    TbtS32(var1^) := TbtS32(var1^) + Longint(TbtU32(var2^));
                  btS32:
                    TbtS32(var1^) := TbtS32(var1^) + TbtS32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtS32(var1^) := TbtS32(var1^) + TbtS64(var2^);
                  {$ENDIF}
                  btChar:
                    TbtS32(var1^) := TbtS32(var1^) +  Ord(TbtChar(var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar:
                    TbtS32(var1^) := TbtS32(var1^) + Ord(TbtWideChar(var2^));
                  {$ENDIF}
                  btVariant:
                    TbtS32(var1^) := TbtS32(var1^) + Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
           {$IFNDEF PS_NOINT64}
            btS64:
              TbtS64(var1^) := TbtS64(var1^) + PSGetInt64(var2, var2type);
           {$ENDIF}
            btSingle:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtSingle(var1^) := TbtSingle(var1^) + TbtU8(var2^);
                  btS8:
                    TbtSingle(var1^) := TbtSingle(var1^) + TbtS8(var2^);
                  btU16:
                    TbtSingle(var1^) := TbtSingle(var1^) + TbtU16(var2^);
                  btS16:
                    TbtSingle(var1^) := TbtSingle(var1^) + TbtS16(var2^);
                  btU32:
                    TbtSingle(var1^) := TbtSingle(var1^) + TbtU32(var2^);
                  btS32:
                    TbtSingle(var1^) := TbtSingle(var1^) + TbtS32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtSingle(var1^) := TbtSingle(var1^) + TbtS64(var2^);
                  {$ENDIF}
                  btSingle:
                    TbtSingle(var1^) := TbtSingle(var1^) + TbtSingle(var2^);
                  btDouble:
                    TbtSingle(var1^) := TbtSingle(var1^) + TbtDouble(var2^);
                  btExtended:
                    TbtSingle(var1^) := TbtSingle(var1^) + TbtExtended(var2^);
                  btCurrency:
                    TbtSingle(var1^) := TbtSingle(var1^) + TbtCurrency(var2^);
                  btVariant:
                    TbtSingle(var1^) := TbtSingle(var1^) +  Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btDouble:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtDouble(var1^) := TbtDouble(var1^) + TbtU8(var2^);
                  btS8:
                    TbtDouble(var1^) := TbtDouble(var1^) + TbtS8(var2^);
                  btU16:
                    TbtDouble(var1^) := TbtDouble(var1^) + TbtU16(var2^);
                  btS16:
                    TbtDouble(var1^) := TbtDouble(var1^) + TbtS16(var2^);
                  btU32:
                    TbtDouble(var1^) := TbtDouble(var1^) + TbtU32(var2^);
                  btS32:
                    TbtDouble(var1^) := TbtDouble(var1^) + TbtS32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtDouble(var1^) := TbtDouble(var1^) + TbtS64(var2^);
                  {$ENDIF}
                  btSingle:
                    TbtDouble(var1^) := TbtDouble(var1^) + TbtSingle(var2^);
                  btDouble:
                    TbtDouble(var1^) := TbtDouble(var1^) + TbtDouble(var2^);
                  btExtended:
                    TbtDouble(var1^) := TbtDouble(var1^) + TbtExtended(var2^);
                  btCurrency:
                    TbtDouble(var1^) := TbtDouble(var1^) + TbtCurrency(var2^);
                  btVariant:
                    TbtDouble(var1^) := TbtDouble(var1^) +  Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btCurrency:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtCurrency(var1^) := TbtCurrency(var1^) + TbtU8(var2^);
                  btS8:
                    TbtCurrency(var1^) := TbtCurrency(var1^) + TbtS8(var2^);
                  btU16:
                    TbtCurrency(var1^) := TbtCurrency(var1^) + TbtU16(var2^);
                  btS16:
                    TbtCurrency(var1^) := TbtCurrency(var1^) + TbtS16(var2^);
                  btU32:
                    TbtCurrency(var1^) := TbtDouble(var1^) + TbtU32(var2^);
                  btS32:
                    TbtCurrency(var1^) := TbtCurrency(var1^) + TbtS32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtCurrency(var1^) := TbtDouble(var1^) + TbtS64(var2^);
                  {$ENDIF}
                  btSingle:
                    TbtCurrency(var1^) := TbtCurrency(var1^) + TbtSingle(var2^);
                  btDouble:
                    TbtCurrency(var1^) := TbtCurrency(var1^) + TbtDouble(var2^);
                  btExtended:
                    TbtCurrency(var1^) := TbtCurrency(var1^) + TbtExtended(var2^);
                  btCurrency:
                    TbtCurrency(var1^) := TbtCurrency(var1^) + TbtCurrency(var2^);
                  btVariant:
                    TbtCurrency(var1^) := TbtCurrency(var1^) +  Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btExtended:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtExtended(var1^) := TbtExtended(var1^) + TbtU8(var2^);
                  btS8:
                    TbtExtended(var1^) := TbtExtended(var1^) + TbtS8(var2^);
                  btU16:
                    TbtExtended(var1^) := TbtExtended(var1^) + TbtU16(var2^);
                  btS16:
                    TbtExtended(var1^) := TbtExtended(var1^) + TbtS16(var2^);
                  btU32:
                    TbtExtended(var1^) := TbtExtended(var1^) + TbtU32(var2^);
                  btS32:
                    TbtExtended(var1^) := TbtExtended(var1^) + TbtS32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtExtended(var1^) := TbtExtended(var1^) + TbtS64(var2^);
                  {$ENDIF}
                  btSingle:
                    TbtExtended(var1^) := TbtExtended(var1^) + TbtSingle(var2^);
                  btDouble:
                    TbtExtended(var1^) := TbtExtended(var1^) + TbtDouble(var2^);
                  btExtended:
                    TbtExtended(var1^) := TbtExtended(var1^) + TbtExtended(var2^);
                  btCurrency:
                    TbtExtended(var1^) := TbtExtended(var1^) + TbtCurrency(var2^);
                  btVariant:
                    TbtExtended(var1^) := TbtExtended(var1^) +  Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btPchar,
            btString:
              TbtString(var1^) := TbtString(var1^) + PSGetAnsiString(Var2, var2type);
            btChar:
              TbtChar(var1^) := TbtChar(ord(TbtChar(var1^)) +  PSGetUInt(Var2, var2type));
            {$IFNDEF PS_NOWIDESTRING}
            btWideChar:
              TbtWideChar(var1^) := widechar(ord(TbtWideChar(var1^)) + PSGetUInt(Var2, var2type));
            btWideString:
              TbtWideString(var1^) := TbtWideString(var1^) + PSGetWideString(Var2, var2type);
            btUnicodeString:
              TbtUnicodeString(var1^) := TbtUnicodeString(var1^) + PSGetUnicodeString(Var2, var2type);
            {$ENDIF}
            btVariant:
              begin
                tvar := null;
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end
                else
                  Variant(var1^) := Variant(var1^) + tvar;
              end;
            btSet:
              begin
                if var1Type = var2Type then
                begin
                  Set_Union(var1, var2, TPSTypeRec_Set(var1Type).aByteSize);
                end
                else
                  Result := False;
              end;

          else
            begin
              CMD_Err(erTypeMismatch);
              Exit;
            end;
          end;
          if not Result then
          begin
            CMD_Err(erTypeMismatch);
            Exit;
          end;
        end;
      1:
        begin { - }
          case var1Type.BaseType of
            btU8:
              TbtU8(var1^) := TbtU8(var1^) - PSGetUInt(Var2, var2type);
            btS8:
              TbtS8(var1^) := TbtS8(var1^) - PSGetInt(Var2, var2type);
            btU16:
              TbtU16(var1^) := TbtU16(var1^) - PSGetUInt(Var2, var2type);
            btS16:
              TbtS16(var1^) := TbtS16(var1^) - PSGetInt(Var2, var2type);
            btU32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtU32(var1^) := TbtU32(var1^) - TbtU8(var2^);
                  btS8:
                    TbtU32(var1^) := TbtU32(var1^) - Cardinal(Longint(TbtS8(var2^)));
                  btU16:
                    TbtU32(var1^) := TbtU32(var1^) - TbtU16(var2^);
                  btS16:
                    TbtU32(var1^) := TbtU32(var1^) - Cardinal(Longint(TbtS16(var2^)));
                  btU32:
                    TbtU32(var1^) := TbtU32(var1^) - TbtU32(var2^);
                  btS32:
                    TbtU32(var1^) := TbtU32(var1^) - Cardinal(TbtS32(var2^));
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtU32(var1^) := TbtU32(var1^) - TbtS64(var2^);
                  {$ENDIF}
                  btChar:
                    TbtU32(var1^) := TbtU32(var1^) -  Ord(TbtChar(var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar:
                    TbtU32(var1^) := TbtU32(var1^) - Ord(TbtWideChar(var2^));
                  {$ENDIF}
                  btVariant:
                    TbtU32(var1^) := TbtU32(var1^) - Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btS32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtS32(var1^) := TbtS32(var1^) - TbtU8(var2^);
                  btS8:
                    TbtS32(var1^) := TbtS32(var1^) - TbtS8(var2^);
                  btU16:
                    TbtS32(var1^) := TbtS32(var1^) - TbtU16(var2^);
                  btS16:
                    TbtS32(var1^) := TbtS32(var1^) - TbtS16(var2^);
                  btU32:
                    TbtS32(var1^) := TbtS32(var1^) - Longint(TbtU32(var2^));
                  btS32:
                    TbtS32(var1^) := TbtS32(var1^) - TbtS32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtS32(var1^) := TbtS32(var1^) - TbtS64(var2^);
                  {$ENDIF}
                  btChar:
                    TbtS32(var1^) := TbtS32(var1^) -  Ord(TbtChar(var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar:
                    TbtS32(var1^) := TbtS32(var1^) - Ord(TbtWideChar(var2^));
                  {$ENDIF}
                  btVariant:
                    TbtS32(var1^) := TbtS32(var1^) - Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
           {$IFNDEF PS_NOINT64}
            btS64: TbtS64(var1^) := TbtS64(var1^) - PSGetInt64(var2, var2type);
           {$ENDIF}
            btSingle:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtSingle(var1^) := TbtSingle(var1^) - TbtU8(var2^);
                  btS8:
                    TbtSingle(var1^) := TbtSingle(var1^) - TbtS8(var2^);
                  btU16:
                    TbtSingle(var1^) := TbtSingle(var1^) - TbtU16(var2^);
                  btS16:
                    TbtSingle(var1^) := TbtSingle(var1^) - TbtS16(var2^);
                  btU32:
                    TbtSingle(var1^) := TbtSingle(var1^) - TbtU32(var2^);
                  btS32:
                    TbtSingle(var1^) := TbtSingle(var1^) - TbtS32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtSingle(var1^) := TbtSingle(var1^) - TbtS64(var2^);
                  {$ENDIF}
                  btSingle:
                    TbtSingle(var1^) := TbtSingle(var1^) - TbtSingle(var2^);
                  btDouble:
                    TbtSingle(var1^) := TbtSingle(var1^) - TbtDouble(var2^);
                  btExtended:
                    TbtSingle(var1^) := TbtSingle(var1^) - TbtExtended(var2^);
                  btCurrency:
                    TbtSingle(var1^) := TbtSingle(var1^) - TbtCurrency(var2^);
                  btVariant:
                    TbtSingle(var1^) := TbtSingle(var1^) - Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btCurrency:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtCurrency(var1^) := TbtCurrency(var1^) - TbtU8(var2^);
                  btS8:
                    TbtCurrency(var1^) := TbtCurrency(var1^) - TbtS8(var2^);
                  btU16:
                    TbtCurrency(var1^) := TbtCurrency(var1^) - TbtU16(var2^);
                  btS16:
                    TbtCurrency(var1^) := TbtCurrency(var1^) - TbtS16(var2^);
                  btU32:
                    TbtCurrency(var1^) := TbtDouble(var1^) - TbtU32(var2^);
                  btS32:
                    TbtCurrency(var1^) := TbtCurrency(var1^) - TbtS32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtCurrency(var1^) := TbtDouble(var1^) - TbtS64(var2^);
                  {$ENDIF}
                  btSingle:
                    TbtCurrency(var1^) := TbtCurrency(var1^) - TbtSingle(var2^);
                  btDouble:
                    TbtCurrency(var1^) := TbtCurrency(var1^) - TbtDouble(var2^);
                  btExtended:
                    TbtCurrency(var1^) := TbtCurrency(var1^) - TbtExtended(var2^);
                  btCurrency:
                    TbtCurrency(var1^) := TbtCurrency(var1^) - TbtCurrency(var2^);
                  btVariant:
                    TbtCurrency(var1^) := TbtCurrency(var1^) - Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btDouble:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtDouble(var1^) := TbtDouble(var1^) - TbtU8(var2^);
                  btS8:
                    TbtDouble(var1^) := TbtDouble(var1^) - TbtS8(var2^);
                  btU16:
                    TbtDouble(var1^) := TbtDouble(var1^) - TbtU16(var2^);
                  btS16:
                    TbtDouble(var1^) := TbtDouble(var1^) - TbtS16(var2^);
                  btU32:
                    TbtDouble(var1^) := TbtDouble(var1^) - TbtU32(var2^);
                  btS32:
                    TbtDouble(var1^) := TbtDouble(var1^) - TbtS32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtDouble(var1^) := TbtDouble(var1^) - TbtS64(var2^);
                  {$ENDIF}
                  btSingle:
                    TbtDouble(var1^) := TbtDouble(var1^) - TbtSingle(var2^);
                  btDouble:
                    TbtDouble(var1^) := TbtDouble(var1^) - TbtDouble(var2^);
                  btExtended:
                    TbtDouble(var1^) := TbtDouble(var1^) - TbtExtended(var2^);
                  btCurrency:
                    TbtDouble(var1^) := TbtDouble(var1^) - TbtCurrency(var2^);
                  btVariant:
                    TbtDouble(var1^) := TbtDouble(var1^) -  Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btExtended:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtExtended(var1^) := TbtExtended(var1^) - TbtU8(var2^);
                  btS8:
                    TbtExtended(var1^) := TbtExtended(var1^) - TbtS8(var2^);
                  btU16:
                    TbtExtended(var1^) := TbtExtended(var1^) - TbtU16(var2^);
                  btS16:
                    TbtExtended(var1^) := TbtExtended(var1^) - TbtS16(var2^);
                  btU32:
                    TbtExtended(var1^) := TbtExtended(var1^) - TbtU32(var2^);
                  btS32:
                    TbtExtended(var1^) := TbtExtended(var1^) - TbtS32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtExtended(var1^) := TbtExtended(var1^) -+TbtS64(var2^);
                  {$ENDIF}
                  btSingle:
                    TbtExtended(var1^) := TbtExtended(var1^) - TbtSingle(var2^);
                  btDouble:
                    TbtExtended(var1^) := TbtExtended(var1^) - TbtDouble(var2^);
                  btExtended:
                    TbtExtended(var1^) := TbtExtended(var1^) - TbtExtended(var2^);
                  btCurrency:
                    TbtExtended(var1^) := TbtExtended(var1^) - TbtCurrency(var2^);
                  btVariant:
                    TbtExtended(var1^) := TbtExtended(var1^) - Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btChar:
              TbtChar(var1^):= TbtChar(ord(TbtChar(var1^)) - PSGetUInt(Var2, var2type));
            {$IFNDEF PS_NOWIDESTRING}
            btWideChar:
              TbtWideChar(var1^) := widechar(ord(TbtWideChar(var1^)) - PSGetUInt(Var2, var2type));
            {$ENDIF}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end
                else
                  Variant(var1^) := Variant(var1^) - tvar;
              end;
            btSet:
              begin
                if var1Type = var2Type then
                begin
                  Set_Diff(var1, var2, TPSTypeRec_Set(var1Type).aByteSize);
                end
                else
                  Result := False;
              end;
          else
            begin
              CMD_Err(erTypeMismatch);
              Exit;
            end;
          end;
          if not Result then
          begin
            CMD_Err(erTypeMismatch);
            Exit;
          end;
        end;
      2:
        begin { * }
          case var1Type.BaseType of
            btU8:
              TbtU8(var1^) := TbtU8(var1^) * PSGetUInt(Var2, var2type);
            btS8:
              TbtS8(var1^) := TbtS8(var1^) * PSGetInt(Var2, var2type);
            btU16:
              TbtU16(var1^) := TbtU16(var1^) * PSGetUInt(Var2, var2type);
            btS16:
              TbtS16(var1^) := TbtS16(var1^) * PSGetInt(Var2, var2type);
            btU32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtU32(var1^) := TbtU32(var1^) * TbtU8(var2^);
                  btS8:
                    TbtU32(var1^) := TbtU32(var1^) * Cardinal(Longint(TbtS8(var2^)));
                  btU16:
                    TbtU32(var1^) := TbtU32(var1^) * TbtU16(var2^);
                  btS16:
                    TbtU32(var1^) := TbtU32(var1^) * Cardinal(Longint(TbtS16(var2^)));
                  btU32:
                    TbtU32(var1^) := TbtU32(var1^) * TbtU32(var2^);
                  btS32:
                    TbtU32(var1^) := TbtU32(var1^) * Cardinal(TbtS32(var2^));
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtU32(var1^) := TbtU32(var1^) * TbtS64(var2^);
                  {$ENDIF}
                  btChar:
                    TbtU32(var1^) := TbtU32(var1^) *  Ord(TbtChar(var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar:
                    TbtU32(var1^) := TbtU32(var1^) * Ord(TbtWideChar(var2^));
                  {$ENDIF}
                  btVariant:
                    TbtU32(var1^) := TbtU32(var1^) * Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btS32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtS32(var1^) := TbtS32(var1^) * TbtU8(var2^);
                  btS8:
                    TbtS32(var1^) := TbtS32(var1^) * TbtS8(var2^);
                  btU16:
                    TbtS32(var1^) := TbtS32(var1^) * TbtU16(var2^);
                  btS16:
                    TbtS32(var1^) := TbtS32(var1^) * TbtS16(var2^);
                  btU32:
                    TbtS32(var1^) := TbtS32(var1^) * Longint(TbtU32(var2^));
                  btS32:
                    TbtS32(var1^) := TbtS32(var1^) * TbtS32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtS32(var1^) := TbtS32(var1^) * TbtS64(var2^);
                  {$ENDIF}
                  btChar:
                    TbtS32(var1^) := TbtS32(var1^) *  Ord(TbtChar(var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar:
                    TbtS32(var1^) := TbtS32(var1^) * Ord(TbtWideChar(var2^));
                  {$ENDIF}
                  btVariant:
                    TbtS32(var1^) := TbtS32(var1^) * Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
           {$IFNDEF PS_NOINT64}
            btS64:
              TbtS64(var1^) := TbtS64(var1^) * PSGetInt64(var2, var2type);
           {$ENDIF}
            btCurrency:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtCurrency(var1^) := TbtCurrency(var1^) * TbtU8(var2^);
                  btS8:
                    TbtCurrency(var1^) := TbtCurrency(var1^) * TbtS8(var2^);
                  btU16:
                    TbtCurrency(var1^) := TbtCurrency(var1^) * TbtU16(var2^);
                  btS16:
                    TbtCurrency(var1^) := TbtCurrency(var1^) * TbtS16(var2^);
                  btU32:
                    TbtCurrency(var1^) := TbtDouble(var1^) * TbtU32(var2^);
                  btS32:
                    TbtCurrency(var1^) := TbtCurrency(var1^) * TbtS32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtCurrency(var1^) := TbtDouble(var1^) * TbtS64(var2^);
                  {$ENDIF}
                  btSingle:
                    TbtCurrency(var1^) := TbtCurrency(var1^) * TbtSingle(var2^);
                  btDouble:
                    TbtCurrency(var1^) := TbtCurrency(var1^) * TbtDouble(var2^);
                  btExtended:
                    TbtCurrency(var1^) := TbtCurrency(var1^) * TbtExtended(var2^);
                  btCurrency:
                    TbtCurrency(var1^) := TbtCurrency(var1^) * TbtCurrency(var2^);
                  btVariant:
                    TbtCurrency(var1^) := TbtCurrency(var1^) *  Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btSingle:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtSingle(var1^) := TbtSingle(var1^) * TbtU8(var2^);
                  btS8:
                    TbtSingle(var1^) := TbtSingle(var1^) * TbtS8(var2^);
                  btU16:
                    TbtSingle(var1^) := TbtSingle(var1^) * TbtU16(var2^);
                  btS16:
                    TbtSingle(var1^) := TbtSingle(var1^) * TbtS16(var2^);
                  btU32:
                    TbtSingle(var1^) := TbtSingle(var1^) * TbtU32(var2^);
                  btS32:
                    TbtSingle(var1^) := TbtSingle(var1^) * TbtS32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtSingle(var1^) := TbtSingle(var1^) * TbtS64(var2^);
                  {$ENDIF}
                  btSingle:
                    TbtSingle(var1^) := TbtSingle(var1^) * TbtSingle(var2^);
                  btDouble:
                    TbtSingle(var1^) := TbtSingle(var1^) * TbtDouble(var2^);
                  btExtended:
                    TbtSingle(var1^) := TbtSingle(var1^) * TbtExtended(var2^);
                  btCurrency:
                    TbtSingle(var1^) := TbtSingle(var1^) * TbtCurrency(var2^);
                  btVariant:
                    TbtSingle(var1^) := TbtSingle(var1^) * Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btDouble:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtDouble(var1^) := TbtDouble(var1^) * TbtU8(var2^);
                  btS8:
                    TbtDouble(var1^) := TbtDouble(var1^) * TbtS8(var2^);
                  btU16:
                    TbtDouble(var1^) := TbtDouble(var1^) * TbtU16(var2^);
                  btS16:
                    TbtDouble(var1^) := TbtDouble(var1^) * TbtS16(var2^);
                  btU32:
                    TbtDouble(var1^) := TbtDouble(var1^) * TbtU32(var2^);
                  btS32:
                    TbtDouble(var1^) := TbtDouble(var1^) * TbtS32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtDouble(var1^) := TbtDouble(var1^) * TbtS64(var2^);
                  {$ENDIF}
                  btSingle:
                    TbtDouble(var1^) := TbtDouble(var1^) * TbtSingle(var2^);
                  btDouble:
                    TbtDouble(var1^) := TbtDouble(var1^) * TbtDouble(var2^);
                  btExtended:
                    TbtDouble(var1^) := TbtDouble(var1^) * TbtExtended(var2^);
                  btCurrency:
                    TbtDouble(var1^) := TbtDouble(var1^) * TbtCurrency(var2^);
                  btVariant:
                    TbtDouble(var1^) := TbtDouble(var1^) * Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btExtended:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtExtended(var1^) := TbtExtended(var1^) *TbtU8(var2^);
                  btS8:
                    TbtExtended(var1^) := TbtExtended(var1^) * TbtS8(var2^);
                  btU16:
                    TbtExtended(var1^) := TbtExtended(var1^) * TbtU16(var2^);
                  btS16:
                    TbtExtended(var1^) := TbtExtended(var1^) * TbtS16(var2^);
                  btU32:
                    TbtExtended(var1^) := TbtExtended(var1^) * TbtU32(var2^);
                  btS32:
                    TbtExtended(var1^) := TbtExtended(var1^) * TbtS32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtExtended(var1^) := TbtExtended(var1^) * TbtS64(var2^);
                  {$ENDIF}
                  btSingle:
                    TbtExtended(var1^) := TbtExtended(var1^) * TbtSingle(var2^);
                  btDouble:
                    TbtExtended(var1^) := TbtExtended(var1^) * TbtDouble(var2^);
                  btExtended:
                    TbtExtended(var1^) := TbtExtended(var1^) * TbtExtended(var2^);
                  btCurrency:
                    TbtExtended(var1^) := TbtExtended(var1^) * TbtCurrency(var2^);
                  btVariant:
                    TbtExtended(var1^) := TbtExtended(var1^) * Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end
                else
                  Variant(var1^) := Variant(var1^) * tvar;
              end;
            btSet:
              begin
                if var1Type = var2Type then
                begin
                  Set_Intersect(var1, var2, TPSTypeRec_Set(var1Type).aByteSize);
                end
                else
                  Result := False;
              end;
          else
            begin
              CMD_Err(erTypeMismatch);
              Exit;
            end;
          end;
          if not Result then
          begin
            CMD_Err(erTypeMismatch);
            Exit;
          end;
        end;
      3:
        begin { / }
          case var1Type.BaseType of
            btU8:
              TbtU8(var1^) := TbtU8(var1^) div PSGetUInt(Var2, var2type);
            btS8:
              TbtS8(var1^) := TbtS8(var1^) div PSGetInt(Var2, var2type);
            btU16:
              TbtU16(var1^) := TbtU16(var1^) div PSGetUInt(Var2, var2type);
            btS16:
              TbtS16(var1^) := TbtS16(var1^) div PSGetInt(Var2, var2type);
            btU32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtU32(var1^) := TbtU32(var1^) div TbtU8(var2^);
                  btS8:
                    TbtU32(var1^) := TbtU32(var1^) div Cardinal(Longint(TbtS8(var2^)));
                  btU16:
                    TbtU32(var1^) := TbtU32(var1^) div TbtU16(var2^);
                  btS16:
                    TbtU32(var1^) := TbtU32(var1^) div Cardinal(Longint(TbtS16(var2^)));
                  btU32:
                    TbtU32(var1^) := TbtU32(var1^) div TbtU32(var2^);
                  btS32:
                    TbtU32(var1^) := TbtU32(var1^) div Cardinal(TbtS32(var2^));
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtU32(var1^) := TbtU32(var1^) div TbtS64(var2^);
                  {$ENDIF}
                  btChar:
                    TbtU32(var1^) := TbtU32(var1^) div  Ord(TbtChar(var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar:
                    TbtU32(var1^) := TbtU32(var1^) div Ord(TbtWideChar(var2^));
                  {$ENDIF}
                  btVariant:
                    TbtU32(var1^) := TbtU32(var1^) div Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btS32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtS32(var1^) := TbtS32(var1^) div TbtU8(var2^);
                  btS8:
                    TbtS32(var1^) := TbtS32(var1^) div TbtS8(var2^);
                  btU16:
                    TbtS32(var1^) := TbtS32(var1^) div TbtU16(var2^);
                  btS16:
                    TbtS32(var1^) := TbtS32(var1^) div TbtS16(var2^);
                  btU32:
                    TbtS32(var1^) := TbtS32(var1^) div Longint(TbtU32(var2^));
                  btS32:
                    TbtS32(var1^) := TbtS32(var1^) div TbtS32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtS32(var1^) := TbtS32(var1^) div TbtS64(var2^);
                  {$ENDIF}
                  btChar:
                    TbtS32(var1^) := TbtS32(var1^) div  Ord(TbtChar(var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar:
                    TbtS32(var1^) := TbtS32(var1^) div Ord(TbtWideChar(var2^));
                  {$ENDIF}
                  btVariant:
                    TbtS32(var1^) := TbtS32(var1^) div Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
           {$IFNDEF PS_NOINT64}
            btS64:
              TbtS64(var1^) := TbtS64(var1^) div PSGetInt64(var2, var2type);
           {$ENDIF}
            btSingle:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtSingle(var1^) := TbtSingle(var1^) / TbtU8(var2^);
                  btS8:
                    TbtSingle(var1^) := TbtSingle(var1^) / TbtS8(var2^);
                  btU16:
                    TbtSingle(var1^) := TbtSingle(var1^) / TbtU16(var2^);
                  btS16:
                    TbtSingle(var1^) := TbtSingle(var1^) / TbtS16(var2^);
                  btU32:
                    TbtSingle(var1^) := TbtSingle(var1^) / TbtU32(var2^);
                  btS32:
                    TbtSingle(var1^) := TbtSingle(var1^) / TbtS32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtSingle(var1^) := TbtSingle(var1^) / TbtS64(var2^);
                  {$ENDIF}
                  btSingle:
                    TbtSingle(var1^) := TbtSingle(var1^) / TbtSingle(var2^);
                  btDouble:
                    TbtSingle(var1^) := TbtSingle(var1^) / TbtDouble(var2^);
                  btExtended:
                    TbtSingle(var1^) := TbtSingle(var1^) / TbtExtended(var2^);
                  btCurrency:
                    TbtSingle(var1^) := TbtSingle(var1^) / TbtCurrency(var2^);
                  btVariant:
                    TbtSingle(var1^) := TbtSingle(var1^) /  Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btCurrency:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtCurrency(var1^) := TbtCurrency(var1^) / TbtU8(var2^);
                  btS8:
                    TbtCurrency(var1^) := TbtCurrency(var1^) / TbtS8(var2^);
                  btU16:
                    TbtCurrency(var1^) := TbtCurrency(var1^) / TbtU16(var2^);
                  btS16:
                    TbtCurrency(var1^) := TbtCurrency(var1^) / TbtS16(var2^);
                  btU32:
                    TbtCurrency(var1^) := TbtDouble(var1^) / TbtU32(var2^);
                  btS32:
                    TbtCurrency(var1^) := TbtCurrency(var1^) / TbtS32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtCurrency(var1^) := TbtDouble(var1^) / TbtS64(var2^);
                  {$ENDIF}
                  btSingle:
                    TbtCurrency(var1^) := TbtCurrency(var1^) / TbtSingle(var2^);
                  btDouble:
                    TbtCurrency(var1^) := TbtCurrency(var1^) / TbtDouble(var2^);
                  btExtended:
                    TbtCurrency(var1^) := TbtCurrency(var1^) / TbtExtended(var2^);
                  btCurrency:
                    TbtCurrency(var1^) := TbtCurrency(var1^) / TbtCurrency(var2^);
                  btVariant:
                    TbtCurrency(var1^) := TbtCurrency(var1^) /  Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btDouble:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtDouble(var1^) := TbtDouble(var1^) / TbtU8(var2^);
                  btS8:
                    TbtDouble(var1^) := TbtDouble(var1^) / TbtS8(var2^);
                  btU16:
                    TbtDouble(var1^) := TbtDouble(var1^) / TbtU16(var2^);
                  btS16:
                    TbtDouble(var1^) := TbtDouble(var1^) / TbtS16(var2^);
                  btU32:
                    TbtDouble(var1^) := TbtDouble(var1^) / TbtU32(var2^);
                  btS32:
                    TbtDouble(var1^) := TbtDouble(var1^) / TbtS32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtDouble(var1^) := TbtDouble(var1^) / TbtS64(var2^);
                  {$ENDIF}
                  btSingle:
                    TbtDouble(var1^) := TbtDouble(var1^) / TbtSingle(var2^);
                  btDouble:
                    TbtDouble(var1^) := TbtDouble(var1^) / TbtDouble(var2^);
                  btExtended:
                    TbtDouble(var1^) := TbtDouble(var1^) / TbtExtended(var2^);
                  btCurrency:
                    TbtDouble(var1^) := TbtDouble(var1^) / TbtCurrency(var2^);
                  btVariant:
                    TbtDouble(var1^) := TbtDouble(var1^) /  Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btExtended:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtExtended(var1^) := TbtExtended(var1^) / TbtU8(var2^);
                  btS8:
                    TbtExtended(var1^) := TbtExtended(var1^) / TbtS8(var2^);
                  btU16:
                    TbtExtended(var1^) := TbtExtended(var1^) / TbtU16(var2^);
                  btS16:
                    TbtExtended(var1^) := TbtExtended(var1^) / TbtS16(var2^);
                  btU32:
                    TbtExtended(var1^) := TbtExtended(var1^) / TbtU32(var2^);
                  btS32:
                    TbtExtended(var1^) := TbtExtended(var1^) / TbtS32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtExtended(var1^) := TbtExtended(var1^) / TbtS64(var2^);
                  {$ENDIF}
                  btSingle:
                    TbtExtended(var1^) := TbtExtended(var1^) / TbtSingle(var2^);
                  btDouble:
                    TbtExtended(var1^) := TbtExtended(var1^) / TbtDouble(var2^);
                  btExtended:
                    TbtExtended(var1^) := TbtExtended(var1^) / TbtExtended(var2^);
                  btCurrency:
                    TbtExtended(var1^) := TbtExtended(var1^) / TbtCurrency(var2^);
                  btVariant:
                    TbtExtended(var1^) := TbtExtended(var1^) /  Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end
                else
                begin
                  if VarIsFloat(variant(var1^)) then
                    Variant(var1^) := Variant(var1^) / tvar
                  else
                    Variant(var1^) := Variant(var1^) div tvar;
                end;
              end;
          else
            begin
              CMD_Err(erTypeMismatch);
              Exit;
            end;
          end;
          if not Result then
          begin
            CMD_Err(erTypeMismatch);
            Exit;
          end;
        end;
      4:
        begin { MOD }
          case var1Type.BaseType of
            btU8:
              TbtU8(var1^) := TbtU8(var1^) mod PSGetUInt(Var2, var2type);
            btS8:
              TbtS8(var1^) := TbtS8(var1^) mod PSGetInt(Var2, var2type);
            btU16:
              TbtU16(var1^) := TbtU16(var1^) mod PSGetUInt(Var2, var2type);
            btS16:
              TbtS16(var1^) := TbtS16(var1^) mod PSGetInt(Var2, var2type);
            btU32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtU32(var1^) := TbtU32(var1^) mod TbtU8(var2^);
                  btS8:
                    TbtU32(var1^) := TbtU32(var1^) mod Cardinal(Longint(TbtS8(var2^)));
                  btU16:
                    TbtU32(var1^) := TbtU32(var1^) mod TbtU16(var2^);
                  btS16:
                    TbtU32(var1^) := TbtU32(var1^) mod Cardinal(Longint(TbtS16(var2^)));
                  btU32:
                    TbtU32(var1^) := TbtU32(var1^) mod TbtU32(var2^);
                  btS32:
                    TbtU32(var1^) := TbtU32(var1^) mod Cardinal(TbtS32(var2^));
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtU32(var1^) := TbtU32(var1^) mod TbtS64(var2^);
                  {$ENDIF}
                  btChar:
                    TbtU32(var1^) := TbtU32(var1^) mod  Ord(TbtChar(var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar:
                    TbtU32(var1^) := TbtU32(var1^) mod Ord(TbtWideChar(var2^));
                  {$ENDIF}
                  btVariant:
                    TbtU32(var1^) := TbtU32(var1^) mod Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
            btS32:
              begin
                if var2type.BaseType = btPointer then
                begin
                  var2type := PIFTypeRec(Pointer(IPointer(var2) + PointerSize)^);
                  var2 := Pointer(var2^);
                  if (var2 = nil) or (var2type = nil) then
                    raise Exception.Create(RPS_TypeMismatch);
                end;
                case var2type.BaseType of
                  btU8:
                    TbtS32(var1^) := TbtS32(var1^) mod TbtU8(var2^);
                  btS8:
                    TbtS32(var1^) := TbtS32(var1^) mod TbtS8(var2^);
                  btU16:
                    TbtS32(var1^) := TbtS32(var1^) mod TbtU16(var2^);
                  btS16:
                    TbtS32(var1^) := TbtS32(var1^) mod TbtS16(var2^);
                  btU32:
                    TbtS32(var1^) := TbtS32(var1^) mod Longint(TbtU32(var2^));
                  btS32:
                    TbtS32(var1^) := TbtS32(var1^) mod TbtS32(var2^);
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    TbtS32(var1^) := TbtS32(var1^) mod TbtS64(var2^);
                  {$ENDIF}
                  btChar:
                    TbtS32(var1^) := TbtS32(var1^) mod  Ord(TbtChar(var2^));
                  {$IFNDEF PS_NOWIDESTRING}
                  btWideChar:
                    TbtS32(var1^) := TbtS32(var1^) mod Ord(TbtWideChar(var2^));
                  {$ENDIF}
                  btVariant:
                    TbtS32(var1^) := TbtS32(var1^) mod Variant(var2^);
                  else
                    raise Exception.Create(RPS_TypeMismatch);
                end;
              end;
           {$IFNDEF PS_NOINT64}
            btS64:
              TbtS64(var1^) := TbtS64(var1^) mod PSGetInt64(var2, var2type);
           {$ENDIF}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end
                else
                  Variant(var1^) := Variant(var1^) mod tvar;
              end;
          else
            begin
              CMD_Err(erTypeMismatch);
              Exit;
            end;
          end;
          if not Result then
          begin
            CMD_Err(erTypeMismatch);
            Exit;
          end;
        end;
      5:
        begin { SHL }
          case var1Type.BaseType of
            btU8:
              TbtU8(var1^) := TbtU8(var1^) shl PSGetUInt(Var2, var2type);
            btS8:
              TbtS8(var1^) := TbtS8(var1^) shl PSGetInt(Var2, var2type);
            btU16:
              TbtU16(var1^) := TbtU16(var1^) shl PSGetUInt(Var2, var2type);
            btS16:
              TbtS16(var1^) := TbtS16(var1^) shl PSGetInt(Var2, var2type);
            btU32:
              TbtU32(var1^) := TbtU32(var1^) shl PSGetUInt(Var2, var2type);
            btS32:
              TbtS32(var1^) := TbtS32(var1^) shl PSGetInt(Var2, var2type);
           {$IFNDEF PS_NOINT64}
            btS64:
              TbtS64(var1^) := TbtS64(var1^) shl PSGetInt64(var2, var2type);
           {$ENDIF}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end
                else
                  Variant(var1^) := Variant(var1^) shl tvar;
              end;
          else
            begin
              CMD_Err(erTypeMismatch);
              Exit;
            end;
          end;
          if not Result then
          begin
            CMD_Err(erTypeMismatch);
            Exit;
          end;
        end;
      6:
        begin { SHR }
          case var1Type.BaseType of
            btU8:
              TbtU8(var1^) := TbtU8(var1^) shr PSGetUInt(var2, var2type);
            btS8:
              TbtS8(var1^) := TbtS8(var1^) shr PSGetInt(var2, var2type);
            btU16:
              TbtU16(var1^) := TbtU16(var1^) shr PSGetUInt(var2, var2type);
            btS16:
              TbtS16(var1^) := TbtS16(var1^) shr PSGetInt(var2, var2type);
            btU32:
              TbtU32(var1^) := TbtU32(var1^) shr PSGetUInt(var2, var2type);
            btS32:
              TbtS32(var1^) := TbtS32(var1^) shr PSGetInt(var2, var2type);
           {$IFNDEF PS_NOINT64}
            btS64:
              TbtS64(var1^) := TbtS64(var1^) shr PSGetInt64(var2, var2type);
           {$ENDIF}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end
                else
                  Variant(var1^) := Variant(var1^) shr tvar;
              end;
          else
            begin
              CMD_Err(erTypeMismatch);
              Exit;
            end;
          end;
          if not Result then
          begin
            CMD_Err(erTypeMismatch);
            Exit;
          end;
        end;
      7:
        begin { AND }
          case var1Type.BaseType of
            btU8:
              TbtU8(var1^) := TbtU8(var1^) and PSGetUInt(var2, var2type);
            btS8:
              TbtS8(var1^) := TbtS8(var1^) and PSGetInt(var2, var2type);
            btU16:
              TbtU16(var1^) := TbtU16(var1^) and PSGetUInt(var2, var2type);
            btS16:
              TbtS16(var1^) := TbtS16(var1^) and PSGetInt(var2, var2type);
            btU32:
              TbtU32(var1^) := TbtU32(var1^) and PSGetUInt(var2, var2type);
            btS32:
              TbtS32(var1^) := TbtS32(var1^) and PSGetInt(var2, var2type);
           {$IFNDEF PS_NOINT64}
            btS64:
              TbtS64(var1^) := TbtS64(var1^) and PSGetInt64(var2, var2type);
           {$ENDIF}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end
                else
                  Variant(var1^) := Variant(var1^) and tvar;
              end;
          else
            begin
              CMD_Err(erTypeMismatch);
              Exit;
            end;
          end;
          if not Result then
          begin
            CMD_Err(erTypeMismatch);
            Exit;
          end;
        end;
      8:
        begin { OR }
          case var1Type.BaseType of
            btU8:
              TbtU8(var1^) := TbtU8(var1^) or PSGetUInt(var2, var2type);
            btS8:
              TbtS8(var1^) := TbtS8(var1^) or PSGetInt(var2, var2type);
            btU16:
              TbtU16(var1^) := TbtU16(var1^) or PSGetUInt(var2, var2type);
            btS16:
              TbtS16(var1^) := TbtS16(var1^) or PSGetInt(var2, var2type);
            btU32:
              TbtU32(var1^) := TbtU32(var1^) or PSGetUInt(var2, var2type);
            btS32:
              TbtS32(var1^) := TbtS32(var1^) or PSGetInt(var2, var2type);
           {$IFNDEF PS_NOINT64}
            btS64:
              TbtS64(var1^) := TbtS64(var1^) or PSGetInt64(var2, var2type);
           {$ENDIF}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end
                else
                  Variant(var1^) := Variant(var1^) or tvar;
              end;
          else
            begin
              CMD_Err(erTypeMismatch);
              Exit;
            end;
          end;
          if not Result then
          begin
            CMD_Err(erTypeMismatch);
            Exit;
          end;
        end;
      9:
        begin { XOR }
          case var1Type.BaseType of
            btU8:
              TbtU8(var1^) := TbtU8(var1^) xor PSGetUInt(var2, var2type);
            btS8:
              TbtS8(var1^) := TbtS8(var1^) xor PSGetInt(var2, var2type);
            btU16:
              TbtU16(var1^) := TbtU16(var1^) xor PSGetUInt(var2, var2type);
            btS16:
              TbtS16(var1^) := TbtS16(var1^) xor PSGetInt(var2, var2type);
            btU32:
              TbtU32(var1^) := TbtU32(var1^) xor PSGetUInt(var2, var2type);
            btS32:
              TbtS32(var1^) := TbtS32(var1^) xor PSGetInt(var2, var2type);
           {$IFNDEF PS_NOINT64}
            btS64:
              TbtS64(var1^) := TbtS64(var1^) xor PSGetInt64(var2, var2type);
           {$ENDIF}
            btVariant:
              begin
                if not IntPIFVariantToVariant(var2, var2type, tvar) then
                begin
                  Result := False;
                end
                else
                  Variant(var1^) := Variant(var1^) xor tvar;
              end;
          else
            begin
              CMD_Err(erTypeMismatch);
              Exit;
            end;
          end;
          if not Result then
          begin
            CMD_Err(erTypeMismatch);
            Exit;
          end;
        end;
      10:
        begin // as
          case var1Type.BaseType of
            btClass:
              begin
                if var2type.BaseType <> btU32 then
                  Result := False
                else
                begin
                  var2type := FTypes[TbtU32(var2^)];
                  if (var2type = nil) or (var2type.BaseType <> btClass) then
                    Result := False
                  else
                  begin
                    if not Class_IS(Self, TObject(var1^), var2type) then
                      Result := False
                  end;
                end;
              end;
          else
            begin
              CMD_Err(erTypeMismatch);
              Exit;
            end;
          end;
          if not Result then
          begin
            CMD_Err(erTypeMismatch);
            Exit;
          end;
        end;
    else
      begin
        Result := False;
        CMD_Err(erInvalidOpcodeParameter);
        Exit;
      end;
    end;
  except
    {$IFDEF DELPHI6UP}
    Tmp := AcquireExceptionObject;
    {$ELSE}
    if RaiseList <> nil then
    begin
      Tmp := Exception(PRaiseFrame(RaiseList)^.ExceptObject);
      PRaiseFrame(RaiseList)^.ExceptObject := nil;
    end
    else
      Tmp := nil;
    {$ENDIF}
    if Tmp <> nil then
    begin
      if Tmp is EPSException then
      begin
        Result := False;
        ExceptionProc(EPSException(tmp).ProcNo, EPSException(tmp).ProcPos, erCustomError, TbtString(EPSException(tmp).Message), nil);
        Exit;
      end
      else if Tmp is EDivByZero then
      begin
        Result := False;
        CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
        Exit;
      end;
      if Tmp is EZeroDivide then
      begin
        Result := False;
        CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
        Exit;
      end;
      if Tmp is EMathError then
      begin
        Result := False;
        CMD_Err3(erMathError,TbtString(Exception(Tmp).Message), Tmp);
        Exit;
      end;
    end;
    if (tmp <> nil) and (Tmp is Exception) then
      CMD_Err3(erException, TbtString(Exception(Tmp).Message), Tmp)
    else
      CMD_Err3(erException, '', Tmp);
    Result := False;
  end;
end;

//==============================================================================
//
// TPSExec.ReadVariable
//
//==============================================================================
function TPSExec.ReadVariable(var Dest: TPSResultData; UsePointer: Boolean): Boolean;
var
  VarType: Cardinal;
  Param: Cardinal;
  Tmp: PIfVariant;
  at: TPSTypeRec;
begin
  if FCurrentPosition + 4 >= FDataLength then
  begin
    CMD_Err(erOutOfRange); // Error
    Result := False;
    Exit;
  end;
  VarType := FData^[FCurrentPosition];
  Inc(FCurrentPosition);
  {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
  Param := unaligned(Cardinal((@FData^[FCurrentPosition])^));
  {$ELSE}
  Param := Cardinal((@FData^[FCurrentPosition])^);
  {$ENDIF}
  Inc(FCurrentPosition, 4);
  case VarType of
    0:
      begin
        Dest.FreeType := vtNone;
        if Param < PSAddrNegativeStackStart then
        begin
          if Param >= Cardinal(FGlobalVars.Count) then
          begin
            CMD_Err(erOutOfGlobalVarsRange);
            Result := False;
            Exit;
          end;
          Tmp := FGlobalVars.Data[param];
        end
        else
        begin
          Param := Cardinal(Longint(-PSAddrStackStart) +
            Longint(FCurrStackBase) + Longint(Param));
          if Param >= Cardinal(FStack.Count) then
          begin
            CMD_Err(erOutOfStackRange);
            Result := False;
            Exit;
          end;
          Tmp := FStack.Data[param];
        end;
        if (UsePointer) and (Tmp.FType.BaseType = btPointer) then
        begin
          Dest.aType := PPSVariantPointer(Tmp).DestType;
          Dest.P := PPSVariantPointer(Tmp).DataDest;
          if Dest.P = nil then
          begin
            CMD_Err(erNullPointerException);
            Result := False;
            Exit;
          end;
        end
        else
        begin
          Dest.aType := PPSVariantData(Tmp).vi.FType;
          Dest.P := @PPSVariantData(Tmp).Data;
        end;
      end;
    1:
      begin
        if Param >= FTypes.Count then
        begin
          CMD_Err(erInvalidType);
          Result := False;
          Exit;
        end;
        at := FTypes.Data^[Param];
        Param := FTempVars.FLength;
        FTempVars.FLength := Cardinal(Longint(Param) + Longint(at.RealSize) + Longint(RTTISize + 3)) and not 3;
        if FTempVars.FLength > FTempVars.FCapacity then
          FtempVars.AdjustLength;
        Tmp := Pointer(IPointer(FtempVars.FDataPtr) + IPointer(Param));

        if Cardinal(FTempVars.FCount) >= Cardinal(FTempVars.FCapacity) then
        begin
          Inc(FTempVars.FCapacity, FCapacityInc);// := FCount + 1;
          ReAllocMem(FTempVars.FData, FTempVars.FCapacity shl 2);
        end;
        FTempVars.FData[FTempVars.FCount] := Tmp; // Instead of SetItem
        Inc(FTempVars.FCount);
        {$IFNDEF PS_NOSMARTLIST}
        Inc(FTempVars.FCheckCount);
        if FTempVars.FCheckCount > FMaxCheckCount then
          FTempVars.Recreate;
        {$ENDIF}

        Tmp.FType := at;
        Dest.P := @PPSVariantData(Tmp).Data;
        Dest.aType := tmp.FType;
        dest.FreeType := vtTempVar;
        case Dest.aType.BaseType of
          btSet:
            begin
              if not ReadData(Dest.P^, TPSTypeRec_Set(Dest.aType).aByteSize) then
              begin
                CMD_Err(erOutOfRange);
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
            end;
          btS8,
          btChar,
          btU8:
            begin
              if FCurrentPosition >= FDataLength then
              begin
                CMD_Err(erOutOfRange);
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
              TbtU8(dest.p^) := FData^[FCurrentPosition];
              Inc(FCurrentPosition);
            end;
          btS16,
          {$IFNDEF PS_NOWIDESTRING}
          btwidechar,
          {$ENDIF}
          btU16:
            begin
              if FCurrentPosition + 1 >= FDataLength then
              begin
                CMD_Err(erOutOfRange);
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
              {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
              TbtU16(dest.p^) := unaligned(TbtU16((@FData^[FCurrentPosition])^));
              {$ELSE}
              TbtU16(dest.p^) := TbtU16((@FData^[FCurrentPosition])^);
              {$ENDIF}
              Inc(FCurrentPosition, 2);
            end;
          btS32,
          btU32:
            begin
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err(erOutOfRange);
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
              {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
              TbtU32(dest.p^) := unaligned(TbtU32((@FData^[FCurrentPosition])^));
              {$ELSE}
              TbtU32(dest.p^) := TbtU32((@FData^[FCurrentPosition])^);
              {$ENDIF}
              Inc(FCurrentPosition, 4);
            end;
          btProcPtr:
            begin
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err(erOutOfRange);
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
              {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
              TbtU32(dest.p^) := unaligned(TbtU32((@FData^[FCurrentPosition])^));
              {$ELSE}
              TbtU32(dest.p^) := TbtU32((@FData^[FCurrentPosition])^);
              {$ENDIF}
              TbtU32(Pointer(IPointer(dest.p) + PointerSize)^) := 0;
              TbtU32(Pointer(IPointer(dest.p) + PointerSize)^) := 0;
              Inc(FCurrentPosition, 4);
            end;
          {$IFNDEF PS_NOINT64}
          bts64:
            begin
              if FCurrentPosition + 7 >= FDataLength then
              begin
                CMD_Err(erOutOfRange);
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
              {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
              TbtS64(dest.p^) := unaligned(TbtS64((@FData^[FCurrentPosition])^));
              {$ELSE}
              TbtS64(dest.p^) := TbtS64((@FData^[FCurrentPosition])^);
              {$ENDIF}
              Inc(FCurrentPosition, 8);
            end;
          {$ENDIF}
          btSingle:
            begin
              if FCurrentPosition + (SizeOf(Single) - 1) >= FDataLength then
              begin
                CMD_Err(erOutOfRange);
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
              {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
              TbtSingle(dest.p^) := unaligned(TbtSingle((@FData^[FCurrentPosition])^));
              {$ELSE}
              TbtSingle(dest.p^) := TbtSingle((@FData^[FCurrentPosition])^);
              {$ENDIF}
              Inc(FCurrentPosition, SizeOf(Single));
            end;
          btDouble:
            begin
              if FCurrentPosition + (SizeOf(Double) - 1) >= FDataLength then
              begin
                CMD_Err(erOutOfRange);
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
              {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
              TbtDouble(dest.p^) := unaligned(TbtDouble((@FData^[FCurrentPosition])^));
              {$ELSE}
              TbtDouble(dest.p^) := TbtDouble((@FData^[FCurrentPosition])^);
              {$ENDIF}
              Inc(FCurrentPosition, SizeOf(double));
            end;
          btExtended:
            begin
              if FCurrentPosition + (SizeOf(Extended) - 1) >= FDataLength then
              begin
                CMD_Err(erOutOfRange);
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
              {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
              TbtExtended(dest.p^) := unaligned(TbtExtended((@FData^[FCurrentPosition])^));
              {$ELSE}
              TbtExtended(dest.p^) := TbtExtended((@FData^[FCurrentPosition])^);
              {$ENDIF}
              Inc(FCurrentPosition, SizeOf(Extended));
            end;
          btPchar,
          btString:
            begin
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err(erOutOfRange);
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
              {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
              Param := unaligned(Cardinal((@FData^[FCurrentPosition])^));
              {$ELSE}
              Param := Cardinal((@FData^[FCurrentPosition])^);
              {$ENDIF}
              Inc(FCurrentPosition, 4);
              Pointer(Dest.P^) := nil;
              SetLength(TbtString(Dest.P^), Param);
              if Param <> 0 then
              begin
                if not ReadData(TbtString(Dest.P^)[1], Param) then
                begin
                  CMD_Err(erOutOfRange);
                  FTempVars.Pop;
                  Result := False;
                  Exit;
                end;
                PAnsiChar(dest.p^)[Param] := #0;
              end;
            end;
          {$IFNDEF PS_NOWIDESTRING}
          btWideString:
            begin
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err(erOutOfRange);
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
              {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
              Param := unaligned(Cardinal((@FData^[FCurrentPosition])^));
              {$ELSE}
              Param := Cardinal((@FData^[FCurrentPosition])^);
              {$ENDIF}
              Inc(FCurrentPosition, 4);
              Pointer(Dest.P^) := nil;
              SetLength(TbtWideString(Dest.P^), Param);
              if not ReadData(TbtWideString(Dest.P^)[1], Param * 2) then
              begin
                CMD_Err(erOutOfRange);
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
            end;
          btUnicodeString:
            begin
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err(erOutOfRange);
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
              {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
              Param := unaligned(Cardinal((@FData^[FCurrentPosition])^));
              {$ELSE}
              Param := Cardinal((@FData^[FCurrentPosition])^);
              {$ENDIF}
              Inc(FCurrentPosition, 4);
              Pointer(Dest.P^) := nil;
              SetLength(TbtUnicodeString(Dest.P^), Param);
              if not ReadData(TbtUnicodeString(Dest.P^)[1], Param * 2) then
              begin
                CMD_Err(erOutOfRange);
                FTempVars.Pop;
                Result := False;
                Exit;
              end;
            end;
            {$ENDIF}
        else
          begin
            CMD_Err(erInvalidType);
            FTempVars.Pop;
            Result := False;
            Exit;
          end;
        end;
      end;
    2:
      begin
        Dest.FreeType := vtNone;
        if Param < PSAddrNegativeStackStart then
        begin
          if Param >= Cardinal(FGlobalVars.Count) then
          begin
            CMD_Err(erOutOfGlobalVarsRange);
            Result := False;
            Exit;
          end;
          Tmp := FGlobalVars.Data[param];
        end
        else
          begin
          Param := Cardinal(Longint(-PSAddrStackStart) + Longint(FCurrStackBase) + Longint(Param));
          if Param >= Cardinal(FStack.Count) then
          begin
            CMD_Err(erOutOfStackRange);
            Result := False;
            Exit;
          end;
          Tmp := FStack.Data[param];
        end;
        if Tmp.FType.BaseType = btPointer then
        begin
          Dest.aType := PPSVariantPointer(Tmp).DestType;
          Dest.P := PPSVariantPointer(Tmp).DataDest;
          if Dest.P = nil then
          begin
            CMD_Err(erNullPointerException);
            Result := False;
            Exit;
          end;
        end
        else
        begin
          Dest.aType := PPSVariantData(Tmp).vi.FType;
          Dest.P := @PPSVariantData(Tmp).Data;
        end;
        if FCurrentPosition + 3 >= FDataLength then
        begin
          CMD_Err(erOutOfRange);
          Result := False;
          Exit;
        end;
        {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
        Param := unaligned(Cardinal((@FData^[FCurrentPosition])^));
        {$ELSE}
        Param := Cardinal((@FData^[FCurrentPosition])^);
        {$ENDIF}
        Inc(FCurrentPosition, 4);
        case Dest.aType.BaseType of
          btRecord:
            begin
              if Param > Cardinal(TPSTypeRec_Record(Dest.aType).FFieldTypes.Count) then
              begin
                CMD_Err(erOutOfRange);
                Result := False;
                Exit;
              end;
              Dest.P := Pointer(IPointer(Dest.P) + IPointer(TPSTypeRec_Record(Dest.aType).RealFieldOffsets[Param]));
              Dest.aType := TPSTypeRec_Record(Dest.aType).FieldTypes[Param];
            end;
          btArray:
            begin
              if Param >= Cardinal(PSDynArrayGetLength(Pointer(Dest.P^), dest.aType)) then
              begin
                CMD_Err(erOutOfRange);
                Result := False;
                Exit;
              end;
              Dest.P := Pointer(IPointer(Dest.P^) + (Param * TPSTypeRec_Array(Dest.aType).FArrayType.RealSize));
              Dest.aType := TPSTypeRec_Array(dest.aType).ArrayType;
            end;
          btStaticArray:
            begin
              if Param >= Cardinal(TPSTypeRec_StaticArray(Dest.aType).Size) then
              begin
                CMD_Err(erOutOfRange);
                Result := False;
                Exit;
              end;
              Dest.P := Pointer(IPointer(Dest.P) + (Param * TPSTypeRec_Array(Dest.aType).FArrayType.RealSize));
              Dest.aType := TPSTypeRec_Array(dest.aType).ArrayType;
            end;
        else
          CMD_Err(erInvalidType);
          Result := False;
          Exit;
        end;

        if UsePointer and (Dest.aType.BaseType = btPointer) then
        begin
          Dest.aType := TPSTypeRec(Pointer(IPointer(Dest.p) + PointerSize)^);
          Dest.P := Pointer(Dest.p^);
          if Dest.P = nil then
          begin
            CMD_Err(erNullPointerException);
            Result := False;
            Exit;
          end;
        end;
      end;
    3:
      begin
        Dest.FreeType := vtNone;
        if Param < PSAddrNegativeStackStart then
        begin
          if Param >= Cardinal(FGlobalVars.Count) then
          begin
            CMD_Err(erOutOfGlobalVarsRange);
            Result := False;
            Exit;
          end;
          Tmp := FGlobalVars.Data[param];
        end
        else
        begin
          Param := Cardinal(Longint(-PSAddrStackStart) + Longint(FCurrStackBase) + Longint(Param));
          if Param >= Cardinal(FStack.Count) then
          begin
            CMD_Err(erOutOfStackRange);
            Result := False;
            Exit;
          end;
          Tmp := FStack.Data[param];
        end;
        if (Tmp.FType.BaseType = btPointer) then
        begin
          Dest.aType := PPSVariantPointer(Tmp).DestType;
          Dest.P := PPSVariantPointer(Tmp).DataDest;
          if Dest.P = nil then
          begin
            CMD_Err(erNullPointerException);
            Result := False;
            Exit;
          end;
        end
        else
        begin
          Dest.aType := PPSVariantData(Tmp).vi.FType;
          Dest.P := @PPSVariantData(Tmp).Data;
        end;
        {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
        Param := unaligned(Cardinal((@FData^[FCurrentPosition])^));
        {$ELSE}
        Param := Cardinal((@FData^[FCurrentPosition])^);
        {$ENDIF}
        Inc(FCurrentPosition, 4);
        if Param < PSAddrNegativeStackStart then
        begin
          if Param >= Cardinal(FGlobalVars.Count) then
          begin
            CMD_Err(erOutOfGlobalVarsRange);
            Result := False;
            Exit;
          end;
          Tmp := FGlobalVars[Param];
        end
        else
        begin
          Param := Cardinal(Longint(-PSAddrStackStart) + Longint(FCurrStackBase) + Longint(Param));
          if Cardinal(Param) >= Cardinal(FStack.Count) then
          begin
            CMD_Err(erOutOfStackRange);
            Result := False;
            Exit;
          end;
          Tmp := FStack[Param];
        end;
        case Tmp.FType.BaseType of
          btu8:
            Param := PPSVariantU8(Tmp).Data;
          btS8:
            Param := PPSVariantS8(Tmp).Data;
          btu16:
            Param := PPSVariantU16(Tmp).Data;
          btS16:
            Param := PPSVariantS16(Tmp).Data;
          btU32:
            Param := PPSVariantU32(Tmp).Data;
          btS32:
            Param := PPSVariantS32(Tmp).Data;
          btPointer:
            begin
              if PPSVariantPointer(tmp).DestType <> nil then
              begin
                case PPSVariantPointer(tmp).DestType.BaseType of
                  btu8:
                    Param := TbtU8(PPSVariantPointer(tmp).DataDest^);
                  btS8:
                    Param := TbtS8(PPSVariantPointer(tmp).DataDest^);
                  btu16:
                    Param := TbtU16(PPSVariantPointer(tmp).DataDest^);
                  btS16:
                    Param := TbtS16(PPSVariantPointer(tmp).DataDest^);
                  btU32,
                    btProcPtr: Param := TbtU32(PPSVariantPointer(tmp).DataDest^);
                  btS32:
                    Param := TbtS32(PPSVariantPointer(tmp).DataDest^);
                  else
                    begin
                      CMD_Err(erTypeMismatch);
                      Result := False;
                      Exit;
                    end;
                end;
              end
              else
              begin
                CMD_Err(erTypeMismatch);
                Result := False;
                Exit;
              end;
            end;
        else
          CMD_Err(erTypeMismatch);
          Result := False;
          Exit;
        end;
        case Dest.aType.BaseType of
          btRecord:
            begin
              if Param > Cardinal(TPSTypeRec_Record(Dest.aType).FFieldTypes.Count) then
              begin
                CMD_Err(erOutOfRange);
                Result := False;
                Exit;
              end;
              Dest.P := Pointer(IPointer(Dest.P) + IPointer(TPSTypeRec_Record(Dest.aType).RealFieldOffsets[Param]));
              Dest.aType := TPSTypeRec_Record(Dest.aType).FieldTypes[Param];
            end;
          btArray:
            begin
              if Cardinal(Param) >= Cardinal(PSDynArrayGetLength(Pointer(Dest.P^), dest.aType)) then
              begin
                CMD_Err(erOutOfRange);
                Result := False;
                Exit;
              end;
              Dest.P := Pointer(IPointer(Dest.P^) + (Param * TPSTypeRec_Array(Dest.aType).FArrayType.RealSize));
              Dest.aType := TPSTypeRec_Array(dest.aType).ArrayType;
            end;
          btStaticArray:
            begin
              if Param >= Cardinal(TPSTypeRec_StaticArray(Dest.aType).Size) then
              begin
                CMD_Err(erOutOfRange);
                Result := False;
                Exit;
              end;
              Dest.P := Pointer(IPointer(Dest.P) + (Param * TPSTypeRec_Array(Dest.aType).FArrayType.RealSize));
              Dest.aType := TPSTypeRec_Array(dest.aType).ArrayType;
            end;
        else
          CMD_Err(erInvalidType);
          Result := False;
          Exit;
        end;
        if UsePointer and (Dest.aType.BaseType = btPointer) then
        begin
          Dest.aType := TPSTypeRec(Pointer(IPointer(Dest.p) + PointerSize)^);
          Dest.P := Pointer(Dest.p^);
          if Dest.P = nil then
          begin
            CMD_Err(erNullPointerException);
            Result := False;
            Exit;
          end;
        end;
      end;
  else
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

//==============================================================================
//
// TPSExec.DoMinus
//
//==============================================================================
function TPSExec.DoMinus(Dta: Pointer; aType: TPSTypeRec): Boolean;
begin
  case atype.BaseType of
    btU8:
      TbtU8(dta^) := -TbtU8(dta^);
    btU16:
      TbtU16(dta^) := -TbtU16(dta^);
    btU32:
      TbtU32(dta^) := -TbtU32(dta^);
    btS8:
      TbtS8(dta^) := -TbtS8(dta^);
    btS16:
      TbtS16(dta^) := -TbtS16(dta^);
    btS32:
      TbtS32(dta^) := -TbtS32(dta^);
    {$IFNDEF PS_NOINT64}
    bts64:
      TbtS64(dta^) := -TbtS64(dta^);
    {$ENDIF}
    btSingle:
      TbtSingle(dta^) := -TbtSingle(dta^);
    btDouble:
      TbtDouble(dta^) := -TbtDouble(dta^);
    btExtended:
      TbtExtended(dta^) := -TbtExtended(dta^);
    btCurrency:
      TbtCurrency(dta^) := -TbtCurrency(dta^);
    btVariant:
      begin
        try
          Variant(dta^) := -Variant(dta^);
        except
          CMD_Err(erTypeMismatch);
          Result := False;
          Exit;
        end;
      end;
  else
    begin
      CMD_Err(erTypeMismatch);
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

//==============================================================================
//
// TPSExec.DoBooleanNot
//
//==============================================================================
function TPSExec.DoBooleanNot(Dta: Pointer; aType: TPSTypeRec): Boolean;
begin
  case aType.BaseType of
    btU8:
      TbtU8(dta^) := TbtU8(TbtU8(dta^) = 0);
    btU16:
      TbtU16(dta^) := TbtU16(TbtU16(dta^) = 0);
    btU32:
      TbtU32(dta^) := TbtU32(TbtU32(dta^) = 0);
    btS8:
      TbtS8(dta^) := TbtS8(TbtS8(dta^) = 0);
    btS16:
      TbtS16(dta^) := TbtS16(TbtS16(dta^) = 0);
    btS32:
      TbtS32(dta^) := TbtS32(TbtS32(dta^) = 0);
    {$IFNDEF PS_NOINT64}
    bts64:
      TbtS64(dta^) := TbtS64(TbtS64(dta^) = 0);
    {$ENDIF}
    btVariant:
      begin
        try
          Variant(dta^) := Variant(dta^) = 0;
        except
          CMD_Err(erTypeMismatch);
          Result := False;
          Exit;
        end;
      end;
  else
    begin
      CMD_Err(erTypeMismatch);
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

//==============================================================================
//
// TPSExec.Stop
//
//==============================================================================
procedure TPSExec.Stop;
begin
  if FStatus = isRunning then
    FStatus := isLoaded
  else if FStatus = isPaused then
  begin
    FStatus := isLoaded;
    FStack.Clear;
    FTempVars.Clear;
  end;
end;

//==============================================================================
//
// TPSExec.ReadLong
//
//==============================================================================
function TPSExec.ReadLong(var b: Cardinal): Boolean;
begin
  if FCurrentPosition + 3 < FDataLength then
  begin
    {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
    b := unaligned(Cardinal((@FData^[FCurrentPosition])^));
    {$ELSE}
    b := Cardinal((@FData^[FCurrentPosition])^);
    {$ENDIF}
    Inc(FCurrentPosition, 4);
    Result := True;
  end
  else
    Result := False;
end;

//==============================================================================
//
// TPSExec.RunProcP
//
//==============================================================================
function TPSExec.RunProcP(const Params: array of Variant; const Procno: Cardinal): Variant;
var
  ParamList: TPSList;
  ct: PIFTypeRec;
  pvar: PPSVariant;
  res, s: TbtString;
  Proc: TPSInternalProcRec;
  i: Longint;
begin
  if ProcNo >= FProcs.Count then
    raise Exception.Create(RPS_UnknownProcedure);
  Proc := GetProcNo(ProcNo) as TPSInternalProcRec;
  ParamList := TPSList.Create;
  try
    s := Proc.ExportDecl;
    res := GRFW(s);
    i := High(Params);
    while s <> '' do
    begin
      if i < 0 then
        raise Exception.Create(RPS_NotEnoughParameters);
      ct := FTypes[StrToInt(Copy(GRLW(s), 2, MaxInt))];
      if ct = nil then
        raise Exception.Create(RPS_InvalidParameter);
      pvar := CreateHeapVariant(ct);
      ParamList.Add(pvar);

      if not VariantToPIFVariant(Self, Params[i], pvar) then
        raise Exception.Create(RPS_InvalidParameter);

      Dec(i);
    end;
    if I > -1 then
      raise Exception.Create(RPS_TooManyParameters);
    if res <> '-1' then
    begin
      pvar := CreateHeapVariant(FTypes[StrToInt(res)]);
      ParamList.Add(pvar);
    end
    else
      pvar := nil;

    RunProc(ParamList, ProcNo);

    RaiseCurrentException;

    if pvar <> nil then
    begin
      PIFVariantToVariant(PVar, Result);
    end
    else
      Result := Null;
  finally
    FreePIFVariantList(ParamList);
  end;
end;

//==============================================================================
//
// TPSExec.RunProcPVar
//
//==============================================================================
function TPSExec.RunProcPVar(var Params: array of Variant; const Procno: Cardinal): Variant;
var
  ParamList: TPSList;
  ct: PIFTypeRec;
  pvar: PPSVariant;
  res, s: TbtString;
  Proc: TPSInternalProcRec;
  i: Longint;
begin
  if ProcNo >= FProcs.Count then
    raise Exception.Create(RPS_UnknownProcedure);
  Proc := GetProcNo(ProcNo) as TPSInternalProcRec;
  ParamList := TPSList.Create;
  try
    s := Proc.ExportDecl;
    res := GRFW(s);
    i := High(Params);
    while s <> '' do
    begin
      if i < 0 then
        raise Exception.Create(RPS_NotEnoughParameters);
      ct := FTypes[StrToInt(Copy(GRLW(s), 2, MaxInt))];
      if ct = nil then
        raise Exception.Create(RPS_InvalidParameter);
      pvar := CreateHeapVariant(ct);
      ParamList.Add(pvar);

      if not VariantToPIFVariant(Self, Params[i], pvar) then
        raise Exception.Create(RPS_InvalidParameter);

      Dec(i);
    end;
    if I > -1 then
      raise Exception.Create(RPS_TooManyParameters);
    if res <> '-1' then
    begin
      pvar := CreateHeapVariant(FTypes[StrToInt(res)]);
      ParamList.Add(pvar);
    end
    else
      pvar := nil;

    RunProc(ParamList, ProcNo);

    RaiseCurrentException;

    for i := 0 to Length(Params) - 1 do
      PIFVariantToVariant(ParamList[i],
                          Params[(Length(Params) - 1) - i]);

    if pvar <> nil then
    begin
      PIFVariantToVariant(PVar, Result);
    end
    else
      Result := Null;
  finally
    FreePIFVariantList(ParamList);
  end;
end;

//==============================================================================
//
// TPSExec.RunProcPN
//
//==============================================================================
function TPSExec.RunProcPN(const Params: array of Variant; const ProcName: TbtString): Variant;
var
  ProcNo: Cardinal;
begin
  ProcNo := GetProc(ProcName);
  if ProcNo = InvalidVal then
    raise Exception.Create(RPS_UnknownProcedure);
  Result := RunProcP(Params, ProcNo);
end;

//==============================================================================
//
// TPSExec.RunProc
//
//==============================================================================
function TPSExec.RunProc(Params: TPSList; ProcNo: Cardinal): Boolean;
var
  I, I2: Integer;
  vnew, Vd: PIfVariant;
  Cp: TPSInternalProcRec;
  oldStatus: TPSStatus;
  tmp: TObject;
begin
  if FStatus <> isNotLoaded then
  begin
    if ProcNo >= FProcs.Count then
    begin
      CMD_Err(erOutOfProcRange);
      Result := False;
      Exit;
    end;
    if Params <> nil then
    begin
      for I := 0 to Params.Count - 1 do
      begin
        vd := Params[I];
        if vd = nil then
        begin
          Result := False;
          Exit;
        end;
        vnew := FStack.PushType(FindType2(btPointer));
        if vd.FType.BaseType = btPointer then
        begin
          PPSVariantPointer(vnew).DestType := PPSVariantPointer(vd).DestType;
          PPSVariantPointer(vnew).DataDest := PPSVariantPointer(vd).DataDest;
        end
        else
        begin
          PPSVariantPointer(vnew).DestType := vd.FType;
          PPSVariantPointer(vnew).DataDest := @PPSVariantData(vd).Data;
        end;
      end;
    end;
    I := FStack.Count;
    Cp := FCurrProc;
    oldStatus := FStatus;
    if TPSProcRec(FProcs.Data^[ProcNo]).ClassType <> TPSExternalProcRec then
    begin
      vd := FStack.PushType(FReturnAddressType);
      PPSVariantReturnAddress(vd).Addr.ProcNo := nil;
      PPSVariantReturnAddress(vd).Addr.Position := FCurrentPosition;
      PPSVariantReturnAddress(vd).Addr.StackBase := FCurrStackBase;
      FCurrStackBase := FStack.Count - 1;
      FCurrProc := FProcs.Data^[ProcNo];
      FData := FCurrProc.Data;
      FDataLength := FCurrProc.Length;
      FCurrentPosition := 0;
      FStatus := isPaused;
      Result := RunScript;
    end
    else
    begin
      try
        Result := TPSExternalProcRec(FProcs.Data^[ProcNo]).ProcPtr(Self, TPSExternalProcRec(FProcs.Data^[ProcNo]), FGlobalVars, FStack);
        if not Result then
        begin
          if ExEx = erNoError then
            CMD_Err(erCouldNotCallProc);
        end;
      except
        {$IFDEF DELPHI6UP}
        Tmp := AcquireExceptionObject;
        {$ELSE}
        if RaiseList <> nil then
        begin
          Tmp := Exception(PRaiseFrame(RaiseList)^.ExceptObject);
          PRaiseFrame(RaiseList)^.ExceptObject := nil;
        end
        else
          Tmp := nil;
        {$ENDIF}
        if Tmp <> nil then
        begin
          if Tmp is EPSException then
          begin
            Result := False;
            ExceptionProc(EPSException(tmp).ProcNo, EPSException(tmp).ProcPos, erCustomError, TbtString(EPSException(tmp).Message), nil);
            Exit;
          end
          else if Tmp is EDivByZero then
          begin
            Result := False;
            CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
            Exit;
          end;
          if Tmp is EZeroDivide then
          begin
            Result := False;
            CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
            Exit;
          end;
          if Tmp is EMathError then
          begin
            Result := False;
            CMD_Err3(erMathError, TbtString(Exception(Tmp).Message), Tmp);
            Exit;
          end;
        end;
        if (Tmp <> nil) and (Tmp is Exception) then
          CMD_Err3(erException, TbtString(Exception(Tmp).Message), Tmp) else
          CMD_Err3(erException, '', Tmp);
        Result := False;
        Exit;
      end;
    end;
    if Cardinal(FStack.Count) > Cardinal(I) then
    begin
      vd := FStack[I];
      if (vd <> nil) and (vd.FType = FReturnAddressType) then
      begin
        for i2 := FStack.Count - 1 downto I + 1 do
          FStack.Pop;
        FCurrentPosition := PPSVariantReturnAddress(vd).Addr.Position;
        FCurrStackBase := PPSVariantReturnAddress(vd).Addr.StackBase;
        FStack.Pop;
      end;
    end;
    if Params <> nil then
    begin
      for I := Params.Count - 1 downto 0 do
      begin
        if FStack.Count = 0 then
          Break
        else
          FStack.Pop;
      end;
    end;
    FStatus := oldStatus;
    FCurrProc := Cp;
    if FCurrProc <> nil then
    begin
      FData := FCurrProc.Data;
      FDataLength := FCurrProc.Length;
    end;
  end
  else
    Result := False;
end;

//==============================================================================
//
// TPSExec.FindType2
//
//==============================================================================
function TPSExec.FindType2(BaseType: TPSBaseType): PIFTypeRec;
var
  l: Cardinal;
begin
  FindType2 := FindType(0, BaseType, l);

end;

//==============================================================================
//
// TPSExec.FindType
//
//==============================================================================
function TPSExec.FindType(StartAt: Cardinal; BaseType: TPSBaseType; var l: Cardinal): PIFTypeRec;
var
  I: Integer;
  n: PIFTypeRec;
begin
  for I := StartAt to FTypes.Count - 1 do
  begin
    n := FTypes[I];
    if n.BaseType = BaseType then
    begin
      l := I;
      Result := n;
      Exit;
    end;
  end;
  Result := nil;
end;

//==============================================================================
//
// TPSExec.GetTypeNo
//
//==============================================================================
function TPSExec.GetTypeNo(l: Cardinal): PIFTypeRec;
begin
  Result := FTypes[l];
end;

//==============================================================================
//
// TPSExec.GetProc
//
//==============================================================================
function TPSExec.GetProc(const Name: TbtString): Cardinal;
var
  MM, I: Longint;
  n: PIFProcRec;
  s: TbtString;
begin
  s := FastUpperCase(name);
  MM := MakeHash(s);
  for I := FProcs.Count - 1 downto 0 do
  begin
    n := FProcs.Data^[I];
    if (n.ClassType = TPSInternalProcRec) and (TPSInternalProcRec(n).ExportNameHash = MM) and (TPSInternalProcRec(n).ExportName = s) then
    begin
      Result := I;
      Exit;
    end
    else if (n.ClassType = TPSExternalProcRec) and (TPSExternalProcRec(n).Name = s) then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := InvalidVal;
end;

//==============================================================================
//
// TPSExec.GetType
//
//==============================================================================
function TPSExec.GetType(const Name: TbtString): Cardinal;
var
  MM, I: Longint;
  n: PIFTypeRec;
  s: TbtString;
begin
  s := FastUpperCase(name);
  MM := MakeHash(s);
  for I := 0 to FTypes.Count - 1 do
  begin
    n := FTypes.Data^[I];
    if (Length(n.ExportName) <> 0) and (n.ExportNameHash = MM) and (n.ExportName = s) then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := InvalidVal;
end;

//==============================================================================
//
// TPSExec.AddResource
//
//==============================================================================
procedure TPSExec.AddResource(Proc, P: Pointer);
var
  Temp: PPSResource;
begin
  New(Temp);
  Temp^.Proc := Proc;
  Temp^.P := p;
  FResources.Add(temp);
end;

//==============================================================================
//
// TPSExec.DeleteResource
//
//==============================================================================
procedure TPSExec.DeleteResource(P: Pointer);
var
  i: Longint;
begin
  for i := Longint(FResources.Count) - 1 downto 0 do
  begin
    if PPSResource(FResources[I])^.P = P then
    begin
      FResources.Delete(I);
      Exit;
    end;
  end;
end;

//==============================================================================
//
// TPSExec.FindProcResource
//
//==============================================================================
function TPSExec.FindProcResource(Proc: Pointer): Pointer;
var
  I: Longint;
  temp: PPSResource;
begin
  for i := Longint(FResources.Count) - 1 downto 0 do
  begin
    temp := FResources[I];
    if temp^.Proc = proc then
    begin
      Result := Temp^.P;
      Exit;
    end;
  end;
  Result := nil;
end;

//==============================================================================
//
// TPSExec.IsValidResource
//
//==============================================================================
function TPSExec.IsValidResource(Proc, P: Pointer): Boolean;
var
  i: Longint;
  temp: PPSResource;
begin
  for i := 0 to Longint(FResources.Count) - 1 do
  begin
    temp := FResources[i];
    if temp^.p = p then
    begin
      Result := temp^.Proc = Proc;
      Exit;
    end;
  end;
  Result := False;
end;

//==============================================================================
//
// TPSExec.FindProcResource2
//
//==============================================================================
function TPSExec.FindProcResource2(Proc: Pointer;
  var StartAt: Longint): Pointer;
var
  I: Longint;
  temp: PPSResource;
begin
  if StartAt > Longint(FResources.Count) - 1 then
    StartAt := Longint(FResources.Count) - 1;
  for i := StartAt downto 0 do
  begin
    temp := FResources[I];
    if temp^.Proc = proc then
    begin
      Result := Temp^.P;
      StartAt := i - 1;
      Exit;
    end;
  end;
  StartAt := -1;
  Result := nil;
end;

//==============================================================================
//
// TPSExec.RunLine
//
//==============================================================================
procedure TPSExec.RunLine;
begin
  if @FOnRunLine <> nil then
    FOnRunLine(Self);
end;

//==============================================================================
//
// TPSExec.CMD_Err3
//
//==============================================================================
procedure TPSExec.CMD_Err3(EC: TPSError; const Param: TbtString; ExObject: TObject);
var
  l: Longint;
  C: Cardinal;
begin
  C := InvalidVal;
  for l := FProcs.Count - 1 downto 0 do
  begin
    if FProcs.Data^[l] = FCurrProc then
    begin
      C := l;
      break;
    end;
  end;
  if @FOnException <> nil then
    FOnException(Self, Ec, Param, ExObject, C, FCurrentPosition);
  ExceptionProc(C, FCurrentPosition, EC, Param, ExObject);
end;

//==============================================================================
//
// TPSExec.AddSpecialProcImport
//
//==============================================================================
procedure TPSExec.AddSpecialProcImport(const FName: TbtString;
  P: TPSOnSpecialProcImport; Tag: Pointer);
var
  N: PSpecialProc;
begin
  New(n);
  n^.P := P;
  N^.Name := FName;
  n^.namehash := MakeHash(N^.Name);
  n^.Tag := Tag;
  FSpecialProcList.Add(n);
end;

//==============================================================================
//
// TPSExec.GetVar
//
//==============================================================================
function TPSExec.GetVar(const Name: TbtString): Cardinal;
var
  l: Longint;
  h: Longint;
  s: TbtString;
  p: PPSExportedVar;
begin
  s := FastUpperCase(name);
  h := MakeHash(s);
  for l := FExportedVars.Count - 1 downto 0 do
  begin
    p := FexportedVars.Data^[L];
    if (p^.FNameHash = h) and (p^.FName = s) then
    begin
      Result := L;
      Exit;
    end;
  end;
  Result := InvalidVal;
end;

//==============================================================================
//
// TPSExec.GetVarNo
//
//==============================================================================
function TPSExec.GetVarNo(C: Cardinal): PIFVariant;
begin
  Result := FGlobalVars[c];
end;

//==============================================================================
//
// TPSExec.GetVar2
//
//==============================================================================
function TPSExec.GetVar2(const Name: TbtString): PIFVariant;
begin
  Result := GetVarNo(GetVar(Name));
end;

//==============================================================================
//
// TPSExec.GetProcNo
//
//==============================================================================
function TPSExec.GetProcNo(C: Cardinal): PIFProcRec;
begin
  Result := FProcs[c];
end;

//==============================================================================
//
// TPSExec.DoIntegerNot
//
//==============================================================================
function TPSExec.DoIntegerNot(Dta: Pointer; aType: TPSTypeRec): Boolean;
begin
  case aType.BaseType of
    btU8:
      TbtU8(dta^) := not TbtU8(dta^);
    btU16:
      TbtU16(dta^) := not TbtU16(dta^);
    btU32:
      TbtU32(dta^) := not TbtU32(dta^);
    btS8:
      TbtS8(dta^) := not TbtS8(dta^);
    btS16:
      TbtS16(dta^) := not TbtS16(dta^);
    btS32:
      TbtS32(dta^) := not TbtS32(dta^);
    {$IFNDEF PS_NOINT64}
    bts64:
      TbtS64(dta^) := not TbtS64(dta^);
    {$ENDIF}
    btVariant:
      begin
        try
          Variant(dta^) := not Variant(dta^);
        except
          CMD_Err(erTypeMismatch);
          Result := False;
          Exit;
        end;
      end;
  else
    begin
      CMD_Err(erTypeMismatch);
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

type
  TMyRunLine = procedure(Self: TPSExec);
  TPSRunLine = procedure of object;

//==============================================================================
//
// GetRunLine
//
//==============================================================================
function GetRunLine(FOnRunLine: TPSOnLineEvent; meth: TPSRunLine): TMyRunLine;
begin
  if (TMethod(Meth).Code = @TPSExec.RunLine) and (@FOnRunLine = nil) then
    Result := nil
  else
    Result := TMethod(Meth).Code;
end;

//==============================================================================
//
// TPSExec.RunScript
//
//==============================================================================
function TPSExec.RunScript: Boolean;
var
  CalcType: Cardinal;
  vd, vs, v3: TPSResultData;
  vtemp: PIFVariant;
  p: Cardinal;
  P2: Longint;
  u: PIFProcRec;
  Cmd: Cardinal;
  I: Longint;
  pp: TPSExceptionHandler;
  FExitPoint: Cardinal;
  FOldStatus: TPSStatus;
  Tmp: TObject;
  btemp: Boolean;
  CallRunline: TMyRunLine;
begin
  FExitPoint := InvalidVal;
  if FStatus = isLoaded then
  begin
    for i := FExceptionStack.Count - 1 downto 0 do
    begin
      pp := FExceptionStack.Data[i];
      pp.Free;
    end;
    FExceptionStack.Clear;
  end;
  ExceptionProc(InvalidVal, InvalidVal, erNoError, '', nil);
  RunScript := True;
  FOldStatus := FStatus;
  case FStatus of
    isLoaded:
      begin
        if FMainProc = InvalidVal then
        begin
          RunScript := False;
          Exit;
        end;
        FStatus := isRunning;
        FCurrProc := FProcs.Data^[FMainProc];
        if FCurrProc.ClassType = TPSExternalProcRec then
        begin
          CMD_Err(erNoMainProc);
          FStatus := isLoaded;
          Exit;
        end;
        FData := FCurrProc.Data;
        FDataLength := FCurrProc.Length;
        FCurrStackBase := InvalidVal;
        FCurrentPosition := 0;
      end;
    isPaused:
      begin
        FStatus := isRunning;
      end;
  else
    begin
      RunScript := False;
      Exit;
    end;
  end;
  CallRunLine := GetRunLine(FOnRunLine, Self.RunLine);
  repeat
    FStatus := isRunning;
//    Cmd := InvalidVal;
    while FStatus = isRunning do
    begin
      if @CallRunLine <> nil then
        CallRunLine(Self);
      if FCurrentPosition >= FDataLength then
      begin
        CMD_Err(erOutOfRange); // Error
        break;
      end;
//      if cmd <> invalidval then ProfilerExitProc(Cmd + 1);
      cmd := FData^[FCurrentPosition];
//      ProfilerEnterProc(Cmd+1);
      Inc(FCurrentPosition);
        case Cmd of
          CM_A:
            begin
              if not ReadVariable(vd, True) then
                break;
              if vd.FreeType <> vtNone then
              begin
                if vd.aType.BaseType in NeedFinalization then
                  FinalizeVariant(vd.P, vd.aType);
                p := IPointer(FTempVars.Data^[FtempVars.Count - 1]) - IPointer(FtempVars.DataPtr);
                Dec(FTempVars.FCount);
                {$IFNDEF PS_NOSMARTLIST}
                Inc(FTempVars.FCheckCount);
                if FTempVars.FCheckCount > FMaxCheckCount then
                  FTempVars.Recreate;
                {$ENDIF}
                FTempVars.FLength := P;
                if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then
                  FTempVars.AdjustLength;

                CMD_Err(erInvalidOpcodeParameter);
                break;
              end;
              if not ReadVariable(vs, True) then
                Break;
              // nx change end
{              if (vd.aType.BaseType = btClass) and (vs.aType.BaseType in [btS32]) then
                DWord(vd.P^):=Dword(vs.P^)
              else
              if (vd.aType.BaseType in [btS32]) and (vs.aType.BaseType = btClass) then
                DWord(vd.P^):=Dword(vs.P^)
              else}
              // nx change start
              if not SetVariantValue(vd.P, vs.P, vd.aType, vs.aType) then
              begin
                if vs.FreeType <> vtNone then
                begin
                  if vs.aType.BaseType in NeedFinalization then
                  FinalizeVariant(vs.P, vs.aType);
                  p := IPointer(FTempVars.Data^[FtempVars.Count - 1]) - IPointer(FtempVars.DataPtr);
                  Dec(FTempVars.FCount);
                  {$IFNDEF PS_NOSMARTLIST}
                  Inc(FTempVars.FCheckCount);
                  if FTempVars.FCheckCount > FMaxCheckCount then
                    FTempVars.Recreate;
                  {$ENDIF}
                  FTempVars.FLength := P;
                  if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then
                    FTempVars.AdjustLength;
                end;
                Break;
              end;
              if vs.FreeType <> vtNone then
              begin
                if vs.aType.BaseType in NeedFinalization then
                  FinalizeVariant(vs.P, vs.aType);
                p := IPointer(FTempVars.Data^[FtempVars.Count - 1]) - IPointer(FtempVars.DataPtr);
                Dec(FTempVars.FCount);
                {$IFNDEF PS_NOSMARTLIST}
                Inc(FTempVars.FCheckCount);
                if FTempVars.FCheckCount > FMaxCheckCount then
                  FTempVars.Recreate;
                {$ENDIF}
                FTempVars.FLength := P;
                if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then
                  FTempVars.AdjustLength;
              end;
            end;
          CM_CA:
            begin
              if FCurrentPosition >= FDataLength then
              begin
                CMD_Err(erOutOfRange); // Error
                break;
              end;
              calctype := FData^[FCurrentPosition];
              Inc(FCurrentPosition);
              if not ReadVariable(vd, True) then
                break;
              if vd.FreeType <> vtNone then
              begin
                if vd.aType.BaseType in NeedFinalization then
                  FinalizeVariant(vd.P, vd.aType);
                p := IPointer(FTempVars.Data^[FtempVars.Count - 1]) - IPointer(FtempVars.DataPtr);
                Dec(FTempVars.FCount);
                {$IFNDEF PS_NOSMARTLIST}
                Inc(FTempVars.FCheckCount);
                if FTempVars.FCheckCount > FMaxCheckCount then
                  FTempVars.Recreate;
                {$ENDIF}
                FTempVars.FLength := P;
                if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then
                  FTempVars.AdjustLength;
                CMD_Err(erInvalidOpcodeParameter);
                break;
              end;
              if not ReadVariable(vs, True) then
                Break;
              if not DoCalc(vd.P, vs.p, vd.aType, vs.aType, CalcType) then
              begin
                if vs.FreeType <> vtNone then
                begin
                  if vs.aType.BaseType in NeedFinalization then
                  FinalizeVariant(vs.P, vs.aType);
                  p := IPointer(FTempVars.Data^[FtempVars.Count - 1]) - IPointer(FtempVars.DataPtr);
                  Dec(FTempVars.FCount);
                  {$IFNDEF PS_NOSMARTLIST}
                  Inc(FTempVars.FCheckCount);
                  if FTempVars.FCheckCount > FMaxCheckCount then
                    FTempVars.Recreate;
                  {$ENDIF}
                  FTempVars.FLength := P;
                  if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then
                    FTempVars.AdjustLength;
                end;
                Break;
              end;
              if vs.FreeType <> vtNone then
              begin
                if vs.aType.BaseType in NeedFinalization then
                  FinalizeVariant(vs.P, vs.aType);
                p := IPointer(FTempVars.Data^[FtempVars.Count - 1]) - IPointer(FtempVars.DataPtr);
                Dec(FTempVars.FCount);
                {$IFNDEF PS_NOSMARTLIST}
                Inc(FTempVars.FCheckCount);
                if FTempVars.FCheckCount > FMaxCheckCount then
                  FTempVars.Recreate;
                {$ENDIF}
                FTempVars.FLength := P;
                if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then
                  FTempVars.AdjustLength;
              end;
            end;
          CM_P:
            begin
              if not ReadVariable(vs, True) then
                Break;
              vtemp := FStack.PushType(vs.aType);
              vd.P := Pointer(IPointer(vtemp) + PointerSize);
              vd.aType := Pointer(vtemp^);
              vd.FreeType := vtNone;
              if not SetVariantValue(Vd.P, vs.P, vd.aType, vs.aType) then
              begin
                if vs.FreeType <> vtnone then
                begin
                  if vs.aType.BaseType in NeedFinalization then
                    FinalizeVariant(vs.P, vs.aType);
                  p := IPointer(FTempVars.Data^[FtempVars.Count - 1]) - IPointer(FtempVars.DataPtr);
                  Dec(FTempVars.FCount);
                  {$IFNDEF PS_NOSMARTLIST}
                  Inc(FTempVars.FCheckCount);
                  if FTempVars.FCheckCount > FMaxCheckCount then
                    FTempVars.Recreate;
                  {$ENDIF}
                  FTempVars.FLength := P;
                  if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then
                    FTempVars.AdjustLength;
                end;
                break;
              end;
              if vs.FreeType <> vtnone then
              begin
                if vs.aType.BaseType in NeedFinalization then
                  FinalizeVariant(vs.P, vs.aType);
                p := IPointer(FTempVars.Data^[FtempVars.Count - 1]) - IPointer(FtempVars.DataPtr);
                Dec(FTempVars.FCount);
                {$IFNDEF PS_NOSMARTLIST}
                Inc(FTempVars.FCheckCount);
                if FTempVars.FCheckCount > FMaxCheckCount then
                  FTempVars.Recreate;
                {$ENDIF}
                FTempVars.FLength := P;
                if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then
                  FTempVars.AdjustLength;
              end;
            end;
          CM_PV:
            begin
              if not ReadVariable(vs, True) then
                Break;
              if vs.FreeType <> vtnone then
              begin
                FTempVars.Pop;
                CMD_Err(erInvalidOpcodeParameter);
                break;
              end;
              vtemp := FStack.PushType(FindType2(btPointer));
              if vs.aType.BaseType = btPointer then
              begin
                PPSVariantPointer(vtemp).DataDest := Pointer(vs.p^);
                PPSVariantPointer(vtemp).DestType := Pointer(Pointer(IPointer(vs.p) + PointerSize)^);
                PPSVariantPointer(vtemp).FreeIt := False;
              end
              else
              begin
                PPSVariantPointer(vtemp).DataDest := vs.p;
                PPSVariantPointer(vtemp).DestType := vs.aType;
                PPSVariantPointer(vtemp).FreeIt := False;
              end;
            end;
          CM_PO:
            begin
              if FStack.Count = 0 then
              begin
                CMD_Err(erOutOfStackRange);
                break;
              end;
              vtemp := FStack.Data^[FStack.Count - 1];
              if (vtemp = nil) or (vtemp.FType.BaseType = btReturnAddress) then
              begin
                CMD_Err(erOutOfStackRange);
                break;
              end;
              FStack.Pop;
(*              Dec(FStack.FCount);
              {$IFNDEF PS_NOSMARTLIST}
              Inc(FStack.FCheckCount);
              if FStack.FCheckCount > FMaxCheckCount then FStack.Recreate;
              {$ENDIF}
              FStack.FLength := Longint(IPointer(vtemp) - IPointer(FStack.DataPtr));
              if TPSTypeRec(vtemp^).BaseType in NeedFinalization then
                FinalizeVariant(Pointer(IPointer(vtemp)+PointerSize), Pointer(vtemp^));
              if ((FStack.FCapacity - FStack.FLength) shr 12) > 2 then FStack.AdjustLength;*)
            end;
          CM_C:
            begin
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err(erOutOfRange);
                Break;
              end;
              {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
              p := unaligned(Cardinal((@FData^[FCurrentPosition])^));
              {$ELSE}
              p := Cardinal((@FData^[FCurrentPosition])^);
              {$ENDIF}
              Inc(FCurrentPosition, 4);
              if p >= FProcs.Count then
              begin
                CMD_Err(erOutOfProcRange);
                break;
              end;
              u := FProcs.Data^[p];
              if u.ClassType = TPSExternalProcRec then
              begin
                try
                  if not TPSExternalProcRec(u).ProcPtr(Self, TPSExternalProcRec(u), FGlobalVars, FStack) then
                  begin
                    if ExEx = erNoError then
                      CMD_Err(erCouldNotCallProc);
                    Break;
                  end;
                except
                  {$IFDEF DELPHI6UP}
                  Tmp := AcquireExceptionObject;
                  {$ELSE}
                  if RaiseList <> nil then
                  begin
                    Tmp := Exception(PRaiseFrame(RaiseList)^.ExceptObject);
                    PRaiseFrame(RaiseList)^.ExceptObject := nil;
                  end
                  else
                    Tmp := nil;
                  {$ENDIF}
                  if Tmp <> nil then
                  begin
                    if Tmp is EPSException then
                    begin
                      ExceptionProc(EPSException(tmp).ProcNo, EPSException(tmp).ProcPos, erCustomError, TbtString(EPSException(tmp).Message), nil);
                      Break;
                    end
                    else if Tmp is EDivByZero then
                    begin
                      CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
                      Break;
                    end;
                    if Tmp is EZeroDivide then
                    begin
                      CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
                      Break;
                    end;
                    if Tmp is EMathError then
                    begin
                      CMD_Err3(erMathError, TbtString(Exception(Tmp).Message), Tmp);
                      Break;
                    end;
                  end;
                  if (Tmp <> nil) and (Tmp is Exception) then
                    CMD_Err3(erException, TbtString(Exception(Tmp).Message), Tmp) else
                    CMD_Err3(erException, '', Tmp);
                  Break;
                end;
              end
              else
              begin
                Vtemp := Fstack.PushType(FReturnAddressType);
                vd.P := Pointer(IPointer(VTemp) + PointerSize);
                vd.aType := pointer(vtemp^);
                vd.FreeType := vtNone;
                PPSVariantReturnAddress(vtemp).Addr.ProcNo := FCurrProc;
                PPSVariantReturnAddress(vtemp).Addr.Position := FCurrentPosition;
                PPSVariantReturnAddress(vtemp).Addr.StackBase := FCurrStackBase;

                FCurrStackBase := FStack.Count - 1;
                FCurrProc := TPSInternalProcRec(u);
                FData := FCurrProc.Data;
                FDataLength := FCurrProc.Length;
                FCurrentPosition := 0;
              end;
            end;
          CM_PG:
            begin
              FStack.Pop;
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err(erOutOfRange);
                Break;
              end;
              {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
              p := unaligned(Cardinal((@FData^[FCurrentPosition])^));
              {$ELSE}
              p := Cardinal((@FData^[FCurrentPosition])^);
              {$ENDIF}
              Inc(FCurrentPosition, 4);
              FCurrentPosition := FCurrentPosition + p;
            end;
          CM_P2G:
            begin
              FStack.Pop;
              FStack.Pop;
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err(erOutOfRange);
                Break;
              end;
              {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
              p := unaligned(Cardinal((@FData^[FCurrentPosition])^));
              {$ELSE}
              p := Cardinal((@FData^[FCurrentPosition])^);
              {$ENDIF}
              Inc(FCurrentPosition, 4);
              FCurrentPosition := FCurrentPosition + p;
            end;
          CM_G:
            begin
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err(erOutOfRange);
                Break;
              end;
              {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
              p := unaligned(Cardinal((@FData^[FCurrentPosition])^));
              {$ELSE}
              p := Cardinal((@FData^[FCurrentPosition])^);
              {$ENDIF}
              Inc(FCurrentPosition, 4);
              FCurrentPosition := FCurrentPosition + p;
            end;
          CM_CG:
            begin
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err(erOutOfRange);
                Break;
              end;
              {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
              p := unaligned(Cardinal((@FData^[FCurrentPosition])^));
              {$ELSE}
              p := Cardinal((@FData^[FCurrentPosition])^);
              {$ENDIF}
              Inc(FCurrentPosition, 4);
              btemp := True;
              if not ReadVariable(vs, btemp) then
                Break;
              case Vs.aType.BaseType of
                btU8:
                  btemp := TbtU8(vs.p^) <> 0;
                btS8:
                  btemp := TbtS8(vs.p^) <> 0;
                btU16:
                  btemp := TbtU16(vs.p^) <> 0;
                btS16:
                  btemp := TbtS16(vs.p^) <> 0;
                btU32:
                  btemp := TbtU32(vs.p^) <> 0;
                btS32:
                  btemp := TbtS32(vs.p^) <> 0;
              else
                begin
                  CMD_Err(erInvalidType);
                  if vs.FreeType <> vtNone then
                    FTempVars.Pop;
                  break;
                end;
              end;
              if vs.FreeType <> vtNone then
                FTempVars.Pop;
              if btemp then
                FCurrentPosition := FCurrentPosition + p;
            end;
          CM_CNG:
            begin
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err(erOutOfRange);
                Break;
              end;
              {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
              p := unaligned(Cardinal((@FData^[FCurrentPosition])^));
              {$ELSE}
              p := Cardinal((@FData^[FCurrentPosition])^);
              {$ENDIF}
              Inc(FCurrentPosition, 4);
              btemp := True;
              if not ReadVariable(vs, BTemp) then
                Break;
              case Vs.aType.BaseType of
                btU8:
                  btemp := TbtU8(vs.p^) = 0;
                btS8:
                  btemp := TbtS8(vs.p^) = 0;
                btU16:
                  btemp := TbtU16(vs.p^) = 0;
                btS16:
                  btemp := TbtS16(vs.p^) = 0;
                btU32:
                  btemp := TbtU32(vs.p^) = 0;
                btS32:
                  btemp := TbtS32(vs.p^) = 0;
              else
                begin
                  CMD_Err(erInvalidType);
                  if vs.FreeType <> vtNone then
                    FTempVars.Pop;
                  break;
                end;
              end;
              if vs.FreeType <> vtNone then
                FTempVars.Pop;
              if btemp then
                FCurrentPosition := FCurrentPosition + p;
            end;
          CM_R:
            begin
              FExitPoint := FCurrentPosition - 1;
              P2 := 0;
              if FExceptionStack.Count > 0 then
              begin
                pp := FExceptionStack.Data[FExceptionStack.Count - 1];
                while (pp.BasePtr = FCurrStackBase) or ((pp.BasePtr > FCurrStackBase) and (pp.BasePtr <> InvalidVal)) do
                begin
                  if pp.StackSize < Cardinal(FStack.Count) then
                  begin
                    for p := Longint(FStack.count) - 1 downto Longint(pp.StackSize) do
                      FStack.Pop
                  end;
                  FCurrStackBase := pp.BasePtr;
                  if pp.FinallyOffset <> InvalidVal then
                  begin
                    FCurrentPosition := pp.FinallyOffset;
                    pp.FinallyOffset := InvalidVal;
                    p2 := 1;
                    break;
                  end
                  else if pp.Finally2Offset <> InvalidVal then
                  begin
                    FCurrentPosition := pp.Finally2Offset;
                    pp.Finally2Offset := InvalidVal;
                    p2 := 1;
                    break;
                  end
                  else
                  begin
                    pp.Free;
                    FExceptionStack.DeleteLast;
                    if FExceptionStack.Count = 0 then
                      break;
                    pp := FExceptionStack.Data[FExceptionStack.Count - 1];
                  end;
                end;
              end;
              if p2 = 0 then
              begin
                FExitPoint := InvalidVal;
                if FCurrStackBase = InvalidVal then
                begin
                  FStatus := FOldStatus;
                  break;
                end;
                for P2 := FStack.Count - 1 downto FCurrStackBase + 1 do
                  FStack.Pop;
                if FCurrStackBase >= FStack.Count then
                begin
                  FStatus := FOldStatus;
                  break;
                end;
                vtemp := FStack.Data[FCurrStackBase];
                FCurrProc := PPSVariantReturnAddress(vtemp).Addr.ProcNo;
                FCurrentPosition := PPSVariantReturnAddress(vtemp).Addr.Position;
                FCurrStackBase := PPSVariantReturnAddress(vtemp).Addr.StackBase;
                FStack.Pop;
                if FCurrProc = nil then
                begin
                  FStatus := FOldStatus;
                  break;
                end;
                FData := FCurrProc.Data;
                FDataLength := FCurrProc.Length;
              end;
            end;
          CM_PT:
            begin
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err(erOutOfRange);
                Break;
              end;
              {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
              p := unaligned(Cardinal((@FData^[FCurrentPosition])^));
              {$ELSE}
              p := Cardinal((@FData^[FCurrentPosition])^);
              {$ENDIF}
              Inc(FCurrentPosition, 4);
              if p > FTypes.Count then
              begin
                CMD_Err(erInvalidType);
                break;
              end;
              FStack.PushType(FTypes.Data^[p]);
            end;
          CM_BN:
            begin
              if not ReadVariable(vd, True) then
                Break;
              if vd.FreeType <> vtNone then
                FTempVars.Pop;
              if not DoBooleanNot(vd.P, vd.aType) then
                break;
            end;
          CM_IN:
            begin
              if not ReadVariable(vd, True) then
                Break;
              if vd.FreeType <> vtNone then
                FTempVars.Pop;
              if not DoIntegerNot(Vd.P, vd.aType) then
                break;
            end;
          CM_VM:
            begin
              if not ReadVariable(vd, True) then
                Break;
              if vd.FreeType <> vtNone then
                FTempVars.Pop;
              if not DoMinus(Vd.P, vd.aType) then
                break;
            end;
          CM_SF:
            begin
              if not ReadVariable(vd, True) then
                Break;
              if FCurrentPosition >= FDataLength then
              begin
                CMD_Err(erOutOfRange); // Error
                if vd.FreeType <> vtNone then
                  FTempVars.Pop;
                break;
              end;
              p := FData^[FCurrentPosition];
              Inc(FCurrentPosition);
              case Vd.aType.BaseType of
                btU8:
                  FJumpFlag := TbtU8(Vd.p^) <> 0;
                btS8:
                  FJumpFlag := TbtS8(Vd.p^) <> 0;
                btU16:
                  FJumpFlag := TbtU16(Vd.p^) <> 0;
                btS16:
                  FJumpFlag := TbtS16(Vd.p^) <> 0;
                btU32:
                  FJumpFlag := TbtU32(Vd.p^) <> 0;
                btS32:
                  FJumpFlag := TbtS32(Vd.p^) <> 0;
              else
                begin
                  CMD_Err(erInvalidType);
                  if vd.FreeType <> vtNone then
                    FTempVars.Pop;
                  break;
                end;
              end;
              if p <> 0 then
                FJumpFlag := not FJumpFlag;
              if vd.FreeType <> vtNone then
                FTempVars.Pop;
            end;
          CM_FG:
            begin
              if FCurrentPosition + 3 >= FDataLength then
              begin
                CMD_Err(erOutOfRange);
                Break;
              end;
              {$IFDEF FPC_REQUIRES_PROPER_ALIGNMENT}
              p := unaligned(Cardinal((@FData^[FCurrentPosition])^));
              {$ELSE}
              p := Cardinal((@FData^[FCurrentPosition])^);
              {$ENDIF}
              Inc(FCurrentPosition, 4);
              if FJumpFlag then
                FCurrentPosition := FCurrentPosition + p;
            end;
          CM_PUEXH:
            begin
              pp := TPSExceptionHandler.Create;
              pp.CurrProc := FCurrProc;
              pp.BasePtr := FCurrStackBase;
              pp.StackSize := FStack.Count;
              if not ReadLong(pp.FinallyOffset) then
              begin
                CMD_Err(erOutOfRange);
                pp.Free;
                Break;
              end;
              if not ReadLong(pp.ExceptOffset) then
              begin
                CMD_Err(erOutOfRange);
                pp.Free;
                Break;
              end;
              if not ReadLong(pp.Finally2Offset) then
              begin
                CMD_Err(erOutOfRange);
                pp.Free;
                Break;
              end;
              if not ReadLong(pp.EndOfBlock) then
              begin
                CMD_Err(erOutOfRange);
                pp.Free;
                Break;
              end;
              if pp.FinallyOffset <> InvalidVal then
                pp.FinallyOffset := pp.FinallyOffset + FCurrentPosition;
              if pp.ExceptOffset <> InvalidVal then
                pp.ExceptOffset := pp.ExceptOffset + FCurrentPosition;
              if pp.Finally2Offset <> InvalidVal then
                pp.Finally2Offset := pp.Finally2Offset + FCurrentPosition;
              if pp.EndOfBlock <> InvalidVal then
                pp.EndOfBlock := pp.EndOfBlock + FCurrentPosition;
              if ((pp.FinallyOffset <> InvalidVal) and (pp.FinallyOffset >= FDataLength)) or
                 ((pp.ExceptOffset <> InvalidVal) and (pp.ExceptOffset >= FDataLength)) or
                 ((pp.Finally2Offset <> InvalidVal) and (pp.Finally2Offset >= FDataLength)) or
                 ((pp.EndOfBlock <> InvalidVal) and (pp.EndOfBlock >= FDataLength)) then
              begin
                CMD_Err(erOutOfRange);
                pp.Free;
                Break;
              end;
              FExceptionStack.Add(pp);
            end;
          CM_POEXH:
            begin
              if FCurrentPosition >= FDataLength then
              begin
                CMD_Err(erOutOfRange); // Error
                break;
              end;
              p := FData^[FCurrentPosition];
              Inc(FCurrentPosition);
              case p of
                2:
                  begin
                    if (FExceptionStack.Count = 0) then
                    begin
                      CMD_Err(erOutOfRange);
                      Break;
                    end;
                    pp := FExceptionStack.Data^[FExceptionStack.Count - 1];
                    if pp = nil then
                    begin
                      CMD_Err(erOutOfRange);
                      Break;
                    end;
                    pp.ExceptOffset := InvalidVal;
                    if pp.Finally2Offset <> InvalidVal then
                    begin
                      FCurrentPosition := pp.Finally2Offset;
                      pp.Finally2Offset := InvalidVal;
                    end
                    else
                    begin
                      p := pp.EndOfBlock;
                      pp.Free;
                      FExceptionStack.DeleteLast;
                      if FExitPoint <> InvalidVal then
                      begin
                        FCurrentPosition := FExitPoint;
                      end
                      else
                      begin
                        FCurrentPosition := p;
                      end;
                    end;
                  end;
                0:
                  begin
                    pp := FExceptionStack.Data^[FExceptionStack.Count - 1];
                    if pp = nil then
                    begin
                      CMD_Err(erOutOfRange);
                      Break;
                    end;
                    if pp.FinallyOffset <> InvalidVal then
                    begin
                      FCurrentPosition := pp.FinallyOffset;
                      pp.FinallyOffset := InvalidVal;
                    end
                    else if pp.Finally2Offset <> InvalidVal then
                    begin
                       FCurrentPosition := pp.Finally2Offset;
                       pp.ExceptOffset := InvalidVal;
                    end
                    else
                    begin
                      p := pp.EndOfBlock;
                      pp.Free;
                      FExceptionStack.DeleteLast;
                      if ExEx <> eNoError then
                      begin
                        Tmp := ExObject;
                        ExObject := nil;
                        ExceptionProc(ExProc, ExPos, ExEx, ExParam, Tmp);
                      end
                      else if FExitPoint <> InvalidVal then
                      begin
                        FCurrentPosition := FExitPoint;
                      end
                      else
                      begin
                        FCurrentPosition := p;
                      end;
                    end;
                  end;
                1:
                  begin
                    pp := FExceptionStack.Data^[FExceptionStack.Count - 1];
                    if pp = nil then
                    begin
                      CMD_Err(erOutOfRange);
                      Break;
                    end;
                    if (ExEx <> ENoError) and (pp.ExceptOffset <> InvalidVal) and (pp.ExceptOffset <> InvalidVal - 1) then
                    begin
                      FCurrentPosition := pp.ExceptOffset;
                      pp.ExceptOffset := Cardinal(InvalidVal - 1);
                      pp.ExceptionData := ExEx;
                      pp.ExceptionObject := ExObject;
                      pp.ExceptionParam := ExParam;
                      ExEx := erNoError;
                      ExObject := nil;
                    end
                    else if (pp.Finally2Offset <> InvalidVal) then
                    begin
                      FCurrentPosition := pp.Finally2Offset;
                      pp.Finally2Offset := InvalidVal;
                    end
                    else
                    begin
                      p := pp.EndOfBlock;
                      pp.Free;
                      FExceptionStack.DeleteLast;
                      if (ExEx <> eNoError) and (p <> InvalidVal) then
                      begin
                        Tmp := ExObject;
                        ExObject := nil;
                        ExceptionProc(ExProc, ExPos, ExEx, ExParam, Tmp);
                      end
                      else
                      if FExitPoint <> InvalidVal then
                      begin
                        FCurrentPosition := FExitPoint;
                      end
                      else
                      begin
                        FCurrentPosition := p;
                      end;
                    end;
                  end;
                3:
                  begin
                    pp := FExceptionStack.Data^[FExceptionStack.Count - 1];
                    if pp = nil then
                    begin
                      CMD_Err(erOutOfRange);
                      Break;
                    end;
                    p := pp.EndOfBlock;
                    pp.Free;
                    FExceptionStack.DeleteLast;
                    if ExEx <> eNoError then
                    begin
                      Tmp := ExObject;
                      ExObject := nil;
                      ExceptionProc(ExProc, ExPos, ExEx, ExParam, Tmp);
                    end
                    else if FExitPoint <> InvalidVal then
                    begin
                      FCurrentPosition := FExitPoint;
                    end
                    else
                    begin
                      FCurrentPosition := p;
                    end;
                  end;
              end;
            end;
          CM_SPC:
            begin
              if not ReadVariable(vd, False) then
                Break;
              if vd.FreeType <> vtNone then
              begin
                FTempVars.Pop;
                CMD_Err(erInvalidOpcodeParameter);
                break;
              end;
              if (Vd.aType.BaseType <> btPointer) then
              begin
                CMD_Err(erInvalidOpcodeParameter);
                break;
              end;
              if not ReadVariable(vs, False) then
                Break;
              if Pointer(Pointer(IPointer(vD.P) + PointerSize2)^) <> nil then
                DestroyHeapVariant2(Pointer(vD.P^), Pointer(Pointer(IPointer(vd.P) + PointerSize)^));
              if vs.aType.BaseType = btPointer then
              begin
                if Pointer(vs.P^) <> nil then
                begin
                  Pointer(vd.P^) := CreateHeapVariant2(Pointer(Pointer(IPointer(vs.P) + PointerSize)^));
                  Pointer(Pointer(IPointer(vd.P) + PointerSize)^) := Pointer(Pointer(IPointer(vs.P) + PointerSize)^);
                  Pointer(Pointer(IPointer(vd.P) + PointerSize2)^) := Pointer(1);
                  if not CopyArrayContents(Pointer(vd.P^), Pointer(vs.P^), 1, Pointer(Pointer(IPointer(vd.P) + PointerSize)^)) then
                  begin
                    if vs.FreeType <> vtNone then
                      FTempVars.Pop;
                    CMD_Err(erTypeMismatch);
                    break;
                  end;
                end
                else
                begin
                  Pointer(vd.P^) := nil;
                  Pointer(Pointer(IPointer(vd.P) + PointerSize)^) := nil;
                  Pointer(Pointer(IPointer(vd.P) + PointerSize2)^) := nil;
                end;
              end
              else
              begin
                Pointer(vd.P^) := CreateHeapVariant2(vs.aType);
                Pointer(Pointer(IPointer(vd.P) + PointerSize)^) := vs.aType;
                LongBool(Pointer(IPointer(vd.P) + PointerSize2)^) := True;
                if not CopyArrayContents(Pointer(vd.P^), vs.P, 1, vs.aType) then
                begin
                  if vs.FreeType <> vtNone then
                    FTempVars.Pop;
                  CMD_Err(erTypeMismatch);
                  break;
                end;
              end;
              if vs.FreeType <> vtNone then
                FTempVars.Pop;
            end;
          CM_NOP:;
          CM_DEC:
            begin
              if not ReadVariable(vd, True) then
                Break;
              if vd.FreeType <> vtNone then
              begin
                FTempVars.Pop;
                CMD_Err(erInvalidOpcodeParameter);
                break;
              end;
              case vd.aType.BaseType of
                btu8:
                  dec(TbtU8(vd.P^));
                btS8:
                  dec(TbtS8(vd.P^));
                btu16:
                  dec(TbtU16(vd.P^));
                btS16:
                  dec(TbtS16(vd.P^));
                btU32:
                  dec(TbtU32(vd.P^));
                btS32:
                  dec(TbtS32(vd.P^));
                {$IFNDEF PS_NOINT64}
                bts64:
                  dec(TbtS64(vd.P^));
                {$ENDIF}
              else
                begin
                  CMD_Err(erTypeMismatch);
                  Break;
                end;
              end;
            end;
          CM_INC:
            begin
              if not ReadVariable(vd, True) then
                Break;
              if vd.FreeType <> vtNone then
              begin
                FTempVars.Pop;
                CMD_Err(erInvalidOpcodeParameter);
                break;
              end;
              case vd.aType.BaseType of
                btu8:
                  Inc(TbtU8(vd.P^));
                btS8:
                  Inc(TbtS8(vd.P^));
                btu16:
                  Inc(TbtU16(vd.P^));
                btS16:
                  Inc(TbtS16(vd.P^));
                btU32:
                  Inc(TbtU32(vd.P^));
                btS32:
                  Inc(TbtS32(vd.P^));
                {$IFNDEF PS_NOINT64}
                bts64:
                  Inc(TbtS64(vd.P^));
                {$ENDIF}
              else
                begin
                  CMD_Err(erTypeMismatch);
                  Break;
                end;
              end;
            end;
          CM_SP:
            begin
              if not ReadVariable(vd, False) then
                Break;
              if vd.FreeType <> vtNone then
              begin
                FTempVars.Pop;
                CMD_Err(erInvalidOpcodeParameter);
                break;
              end;
              if (Vd.aType.BaseType <> btPointer) then
              begin
                CMD_Err(erInvalidOpcodeParameter);
                break;
              end;
              if not ReadVariable(vs, False) then
                Break;
              if vs.FreeType <> vtNone then
              begin
                FTempVars.Pop;
                CMD_Err(erInvalidOpcodeParameter);
                break;
              end;
              if vs.aType.BaseType = btPointer then
              begin
                Pointer(vd.P^) := Pointer(vs.p^);
                Pointer(Pointer(IPointer(vd.P) + PointerSize)^) := Pointer(Pointer(IPointer(vs.P) + PointerSize)^);
              end
              else
              begin
                Pointer(vd.P^) := vs.P;
                Pointer(Pointer(IPointer(vd.P) + PointerSize)^) := vs.aType;
              end;
            end;
          CM_CV:
            begin
              if not ReadVariable(vd, True) then
                Break;
              if vd.aType.BaseType <> btProcPtr then
              begin
                if vd.FreeType <> vtNone then
                  FTempVars.Pop;
                CMD_Err(erTypeMismatch);
                break;
              end;
              p := TbtU32(vd.P^);
              if vd.FreeType <> vtNone then
                FTempVars.Pop;
              if (p = 0) and (Pointer(Pointer(IPointer(vd.p) + PointerSize2)^) <> nil) then
              begin
                if not InvokeExternalMethod(TPSTypeRec_ProcPtr(vd.aType), Pointer(Pointer(IPointer(vd.p) + PointerSize)^), Pointer(Pointer(IPointer(vd.p) + PointerSize2)^)) then
                  Break;
              end
              else
              begin
                if (p >= FProcs.Count) or (p = FMainProc) then
                begin
                  CMD_Err(erOutOfProcRange);
                  break;
                end;
                u := FProcs.Data^[p];
                if u.ClassType = TPSExternalProcRec then
                begin
                  try
                    if not TPSExternalProcRec(u).ProcPtr(Self, TPSExternalProcRec(u), FGlobalVars, FStack) then
                    begin
                      if ExEx = erNoError then
                        CMD_Err(erCouldNotCallProc);
                      Break;
                    end;
                  except
                    {$IFDEF DELPHI6UP}
                    Tmp := AcquireExceptionObject;
                    {$ELSE}
                    if RaiseList <> nil then
                    begin
                      Tmp := Exception(PRaiseFrame(RaiseList)^.ExceptObject);
                      PRaiseFrame(RaiseList)^.ExceptObject := nil;
                    end
                    else
                      Tmp := nil;
                    {$ENDIF}
                    if Tmp <> nil then
                    begin
                      if Tmp is EPSException then
                      begin
                        ExceptionProc(EPSException(tmp).ProcNo, EPSException(tmp).ProcPos, erCustomError, TbtString(EPSException(tmp).Message), nil);
                        break;
                      end
                      else if Tmp is EDivByZero then
                      begin
                        CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
                        break;
                      end;
                      if Tmp is EZeroDivide then
                      begin
                        CMD_Err3(erDivideByZero, TbtString(Exception(Tmp).Message), Tmp);
                        break;
                      end;
                      if Tmp is EMathError then
                      begin
                        CMD_Err3(erMathError, TbtString(Exception(Tmp).Message), Tmp);
                        break;
                      end;
                    end;
                    if (Tmp <> nil) and (Tmp is Exception) then
                      CMD_Err3(erException, TbtString(Exception(Tmp).Message), Tmp) else
                      CMD_Err3(erException, '', Tmp);
                    Break;
                  end;
                end
                else
                begin
                  vtemp := FStack.PushType(FReturnAddressType);
                  PPSVariantReturnAddress(vtemp).Addr.ProcNo := FCurrProc;
                  PPSVariantReturnAddress(vtemp).Addr.Position := FCurrentPosition;
                  PPSVariantReturnAddress(vtemp).Addr.StackBase := FCurrStackBase;
                  FCurrStackBase := FStack.Count - 1;
                  FCurrProc := TPSInternalProcRec(u);
                  FData := FCurrProc.Data;
                  FDataLength := FCurrProc.Length;
                  FCurrentPosition := 0;
                end;
              end;
            end;
          CM_CO:
            begin
              if FCurrentPosition >= FDataLength then
              begin
                CMD_Err(erOutOfRange); // Error
                break;
              end;
              calctype := FData^[FCurrentPosition];
              Inc(FCurrentPosition);
              if not ReadVariable(v3, True) then
                Break;
              if v3.FreeType <> vtNone then
              begin
                if v3.aType.BaseType in NeedFinalization then
                  FinalizeVariant(v3.P, v3.aType);
                p := IPointer(FTempVars.Data^[FtempVars.Count - 1]) - IPointer(FtempVars.DataPtr);
                Dec(FTempVars.FCount);
                {$IFNDEF PS_NOSMARTLIST}
                Inc(FTempVars.FCheckCount);
                if FTempVars.FCheckCount > FMaxCheckCount then
                  FTempVars.Recreate;
                {$ENDIF}
                FTempVars.FLength := P;
                if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then
                  FTempVars.AdjustLength;
                CMD_Err(erInvalidOpcodeParameter);
                break;
              end;
              if not ReadVariable(vs, True) then
                Break;
              if not ReadVariable(vd, True) then
              begin
                if vs.FreeType <> vtNone then
                begin
                  if vs.aType.BaseType in NeedFinalization then
                    FinalizeVariant(vs.P, vs.aType);
                  p := IPointer(FTempVars.Data^[FtempVars.Count - 1]) - IPointer(FtempVars.DataPtr);
                  Dec(FTempVars.FCount);
                  {$IFNDEF PS_NOSMARTLIST}
                  Inc(FTempVars.FCheckCount);
                  if FTempVars.FCheckCount > FMaxCheckCount then
                    FTempVars.Recreate;
                  {$ENDIF}
                  FTempVars.FLength := P;
                  if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then
                    FTempVars.AdjustLength;
                end;
                Break;
              end;
              DoBooleanCalc(Vs.P, Vd.P, v3.P, vs.aType, vd.aType, v3.aType, CalcType);
              if vd.FreeType <> vtNone then
              begin
                if vd.aType.BaseType in NeedFinalization then
                  FinalizeVariant(vd.P, vd.aType);
                p := IPointer(FTempVars.Data^[FtempVars.Count - 1]) - IPointer(FtempVars.DataPtr);
                Dec(FTempVars.FCount);
                {$IFNDEF PS_NOSMARTLIST}
                Inc(FTempVars.FCheckCount);
                if FTempVars.FCheckCount > FMaxCheckCount then
                  FTempVars.Recreate;
                {$ENDIF}
                FTempVars.FLength := P;
                if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then
                  FTempVars.AdjustLength;
              end;
              if vs.FreeType <> vtNone then
              begin
                if vs.aType.BaseType in NeedFinalization then
                  FinalizeVariant(vs.P, vs.aType);
                p := IPointer(FTempVars.Data^[FtempVars.Count - 1]) - IPointer(FtempVars.DataPtr);
                Dec(FTempVars.FCount);
                {$IFNDEF PS_NOSMARTLIST}
                Inc(FTempVars.FCheckCount);
                if FTempVars.FCheckCount > FMaxCheckCount then
                  FTempVars.Recreate;
                {$ENDIF}
                FTempVars.FLength := P;
                if ((FTempVars.FCapacity - FTempVars.FLength) shr 12) > 2 then
                  FTempVars.AdjustLength;
              end;
            end;
        else
          CMD_Err(erInvalidOpcode); // Error
        end;
    end;
//    if cmd <> invalidval then ProfilerExitProc(Cmd+1);
//    if ExEx <> erNoError then FStatus := FOldStatus;
  until (FExceptionStack.Count = 0) or (Fstatus <> IsRunning);
  if FStatus = isLoaded then
  begin
    for I := Longint(FStack.Count) - 1 downto 0 do
      FStack.Pop;
    FStack.Clear;
    if FCallCleanup then
      Cleanup;
  end;
  Result := ExEx = erNoError;
end;

//==============================================================================
//
// NVarProc
//
//==============================================================================
function NVarProc(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  tmp: TPSVariantIFC;
begin
   case Longint(p.Ext1) of
    0:
      begin
        if @Caller.FOnSetNVariant = nil then
        begin
          Result := False;
          Exit;
        end;
        tmp := NewTPSVariantIFC(Stack.Items[Stack.Count - 2], True);
        if (Tmp.Dta = nil) or (tmp.aType.BaseType <> btVariant) then
        begin
          Result := False;
          Exit;
        end;
        Caller.FOnSetNVariant(Caller, Stack.GetAnsiString(-1), Variant(tmp.Dta^));
        Result := True;
      end;
    1:
      begin
        if @Caller.FOnGetNVariant = nil then
        begin
          Result := False;
          Exit;
        end;
        tmp := NewTPSVariantIFC(Stack.Items[Stack.Count - 1], False);
        if (Tmp.Dta = nil) or (tmp.aType.BaseType <> btVariant) then
        begin
          Result := False;
          Exit;
        end;
        Variant(tmp.Dta^) := Caller.FOnGetNVariant(Caller, Stack.GetAnsiString(-2));
        Result := True;
      end;
  else
    Result := False;
  end;
end;

//==============================================================================
//
// DefProc
//
//==============================================================================
function DefProc(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  temp: TPSVariantIFC;
  I: Longint;
  b: Boolean;
  pex: TPSExceptionHandler;
  Tmp: TObject;
begin
  { The following needs to be in synch in these 3 functions:
    -UPSCompiler.TPSPascalCompiler.DefineStandardProcedures
    -UPSRuntime.DefProc
    -UPSRuntime.TPSExec.RegisterStandardProcs
  }
  case Longint(p.Ext1) of
    0:
      Stack.SetAnsiString(-1, TbtString(SysUtils.IntToStr(Stack.{$IFNDEF PS_NOINT64}GetInt64{$ELSE}GetInt{$ENDIF}(-2)))); // IntToStr
    1:
      Stack.SetInt(-1, StrToInt(Stack.GetAnsiString(-2))); // strtoint
    2:
      Stack.SetInt(-1, StrToIntDef(Stack.GetAnsiString(-2), Stack.GetInt(-3))); // strtointdef
    3:
      {$IFNDEF PS_NOWIDESTRING}
      if Stack.GetItem(Stack.Count - 2)^.FType.BaseType = btUnicodeString then
        Stack.SetInt(-1, Pos(Stack.GetUnicodeString(-2), Stack.GetUnicodeString(-3)))// pos
      else
      if Stack.GetItem(Stack.Count - 2)^.FType.BaseType = btWideString then
        Stack.SetInt(-1, Pos(Stack.GetWideString(-2), Stack.GetWideString(-3)))// pos
      else
      {$ENDIF}
        Stack.SetInt(-1, Pos(Stack.GetAnsiString(-2), Stack.GetAnsiString(-3)));// pos
    4:
      {$IFNDEF PS_NOWIDESTRING}
      if Stack.GetItem(Stack.Count - 2)^.FType.BaseType = btWideString then
        Stack.SetWideString(-1, Copy(Stack.GetWideString(-2), Stack.GetInt(-3), Stack.GetInt(-4))) // Copy
      else
      if Stack.GetItem(Stack.Count - 2)^.FType.BaseType = btUnicodeString then
        Stack.SetUnicodeString(-1, Copy(Stack.GetUnicodeString(-2), Stack.GetInt(-3), Stack.GetInt(-4))) // Copy
      else
      {$ENDIF}
        Stack.SetAnsiString(-1, Copy(Stack.GetAnsiString(-2), Stack.GetInt(-3), Stack.GetInt(-4))); // Copy
    5: //delete
      begin
        temp := NewTPSVariantIFC(Stack[Stack.Count - 1], True);
        {$IFNDEF PS_NOWIDESTRING}
        if (temp.Dta <> nil) and (temp.aType.BaseType = btUnicodeString) then
        begin
          Delete(TbtUnicodeString(temp.Dta^), Stack.GetInt(-2), Stack.GetInt(-3));
        end
        else if (temp.Dta <> nil) and (temp.aType.BaseType = btWideString) then
        begin
          Delete(TbtWideString(temp.Dta^), Stack.GetInt(-2), Stack.GetInt(-3));
        end
        else
        {$ENDIF}
        begin
          if (temp.Dta = nil) or (temp.aType.BaseType <> btString) then
          begin
            Result := False;
            Exit;
          end;
          Delete(TbtString(temp.Dta^), Stack.GetInt(-2), Stack.GetInt(-3));
        end;
      end;
    6: // insert
      begin
        temp := NewTPSVariantIFC(Stack[Stack.Count - 2], True);
        {$IFNDEF PS_NOWIDESTRING}
        if (temp.Dta <> nil) and (temp.aType.BaseType = btUnicodeString) then
        begin
          Insert(Stack.GetUnicodeString(-1), TbtUnicodeString(temp.Dta^), Stack.GetInt(-3));
        end
        else if (temp.Dta <> nil) and (temp.aType.BaseType = btWideString) then
        begin
          Insert(Stack.GetWideString(-1), TbtWideString(temp.Dta^), Stack.GetInt(-3));
        end
        else
        {$ENDIF}
        begin
          if (temp.Dta = nil) or (temp.aType.BaseType <> btString) then
          begin
            Result := False;
            Exit;
          end;
          Insert(Stack.GetAnsiString(-1), TbtString(temp.Dta^), Stack.GetInt(-3));
        end;
      end;
    7: // StrGet
      begin
        temp :=  NewTPSVariantIFC(Stack[Stack.Count - 2], True);
        if (temp.Dta = nil) or not (temp.aType.BaseType in [btString, btUnicodeString]) then
        begin
          Result := False;
          Exit;
        end;
        I := Stack.GetInt(-3);
        if (i < 1) or (i > Length(TbtString(temp.Dta^))) then
        begin
          Caller.CMD_Err2(erCustomError, TbtString(RPS_OutOfStringRange));
          Result := False;
          Exit;
        end;
        Stack.SetInt(-1, Ord(TbtString(temp.Dta^)[i]));
      end;
    8: // StrSet
      begin
        temp := NewTPSVariantIFC(Stack[Stack.Count - 3], True);
        if (temp.Dta = nil) or not (temp.aType.BaseType in [btString, btUnicodeString]) then
        begin
          Result := False;
          Exit;
        end;
        I := Stack.GetInt(-2);
        if (i < 1) or (i > Length(TbtString(temp.Dta^))) then
        begin
          Caller.CMD_Err2(erCustomError, TbtString(RPS_OutOfStringRange));
          Result := True;
          Exit;
        end;
        TbtString(temp.Dta^)[i] := TbtChar(Stack.GetInt(-1));
      end;
    10:
      {$IFNDEF PS_NOWIDESTRING}
      {$IFDEF DELPHI2009UP}
      if Stack.GetItem(Stack.Count - 2)^.FType.BaseType = btUnicodeString then
        Stack.SetUnicodeString(-1, UpperCase(Stack.GetUnicodeString(-2))) // Uppercase
      else
      {$ENDIF}
      if (Stack.GetItem(Stack.Count - 2)^.FType.BaseType = btWideString) or
        (Stack.GetItem(Stack.Count - 2)^.FType.BaseType = btUnicodeString) then
        Stack.SetWideString(-1, WideUpperCase(Stack.GetWideString(-2))) // Uppercase
      else
      {$ENDIF}
        Stack.SetAnsiString(-1, FastUpperCase(Stack.GetAnsiString(-2))); // Uppercase
    11:
      {$IFNDEF PS_NOWIDESTRING}
      {$IFDEF DELPHI2009UP}
      if Stack.GetItem(Stack.Count - 2)^.FType.BaseType = btUnicodeString then
        Stack.SetUnicodeString(-1, LowerCase(Stack.GetUnicodeString(-2))) // LowerCase
      else
      {$ENDIF}
      if (Stack.GetItem(Stack.Count - 2)^.FType.BaseType = btWideString) or
        (Stack.GetItem(Stack.Count - 2)^.FType.BaseType = btUnicodeString) then
        Stack.SetWideString(-1, WideLowerCase(Stack.GetWideString(-2))) // LowerCase
      else
      {$ENDIF}
        Stack.SetAnsiString(-1, FastLowercase(Stack.GetAnsiString(-2)));// LowerCase
    12:
      {$IFNDEF PS_NOWIDESTRING}
      if Stack.GetItem(Stack.Count - 2)^.FType.BaseType = btUnicodeString then
        Stack.SetUnicodeString(-1, SysUtils.Trim(Stack.GetUnicodestring(-2))) // Trim
      else if Stack.GetItem(Stack.Count - 2)^.FType.BaseType = btWideString then
        Stack.SetWideString(-1, SysUtils.Trim(Stack.GetWideString(-2))) // Trim
      else
      {$ENDIF}
        Stack.SetAnsiString(-1, AnsiString(SysUtils.Trim(String(Stack.GetAnsiString(-2)))));// Trim
    13:
      Stack.SetInt(-1, Length(Stack.GetAnsiString(-2))); // Length
    14: // SetLength
      begin
        temp := NewTPSVariantIFC(Stack[Stack.Count - 1], True);
        if (temp.Dta = nil) or (temp.aType.BaseType <> btString) then
        begin
          Result := False;
          Exit;
        end;
        SetLength(TbtString(temp.Dta^), Stack.GetInt(-2));
      end;
    15:
      Stack.SetReal(-1, Sin(Stack.GetReal(-2))); // Sin
    16:
      Stack.SetReal(-1, Cos(Stack.GetReal(-2)));  // Cos
    17:
      Stack.SetReal(-1, Sqrt(Stack.GetReal(-2))); // Sqrt
    18:
      Stack.SetInt(-1, Round(Stack.GetReal(-2))); // Round
    19:
      Stack.SetInt(-1, Trunc(Stack.GetReal(-2))); // Trunc
    20:
      Stack.SetReal(-1, Int(Stack.GetReal(-2))); // Int
    45:
      Stack.SetReal(-1, Sqr(Stack.GetReal(-2))); // Sqr
    21:
      Stack.SetReal(-1, Pi); // Pi
    22:
      Stack.SetReal(-1, Abs(Stack.GetReal(-2))); // Abs
    23:
      Stack.SetReal(-1, StrToFloat(Stack.GetAnsiString(-2))); // StrToFloat
    24:
      Stack.SetAnsiString(-1, FloatToStr(Stack.GetReal(-2)));// FloatToStr
    25:
      {$IFNDEF PS_NOWIDESTRING}
      if Stack.GetItem(Stack.Count - 2)^.FType.BaseType = btUnicodeString then
        Stack.SetUnicodeString(-1, upadL(Stack.GetUnicodeString(-2), Stack.GetInt(-3))) //  PadL
      else if Stack.GetItem(Stack.Count - 2)^.FType.BaseType = btWideString then
        Stack.SetWideString(-1, wPadL(Stack.GetWideString(-2), Stack.GetInt(-3))) //  PadL
      else
      {$ENDIF}
        Stack.SetAnsiString(-1, PadL(Stack.GetAnsiString(-2), Stack.GetInt(-3))); //  PadL
    26:
      {$IFNDEF PS_NOWIDESTRING}
      if Stack.GetItem(Stack.Count - 2)^.FType.BaseType = btUnicodeString then
        Stack.SetUnicodeString(-1, uPadR(Stack.GetUnicodeString(-2), Stack.GetInt(-3))) // PadR
      else if Stack.GetItem(Stack.Count - 2)^.FType.BaseType = btWideString then
        Stack.SetWideString(-1, wPadR(Stack.GetWideString(-2), Stack.GetInt(-3))) // PadR
      else
      {$ENDIF}
        Stack.SetAnsiString(-1, PadR(Stack.GetAnsiString(-2), Stack.GetInt(-3))); // PadR
    27:
      {$IFNDEF PS_NOWIDESTRING}
      if Stack.GetItem(Stack.Count - 2)^.FType.BaseType = btUnicodeString then
        Stack.SetUnicodeString(-1, uPadZ(Stack.GetUnicodeString(-2), Stack.GetInt(-3)))// PadZ
      else if Stack.GetItem(Stack.Count - 2)^.FType.BaseType = btWideString then
        Stack.SetWideString(-1, wPadZ(Stack.GetWideString(-2), Stack.GetInt(-3)))// PadZ
      else
      {$ENDIF}
        Stack.SetAnsiString(-1, PadZ(Stack.GetAnsiString(-2), Stack.GetInt(-3)));// PadZ
    28:
      Stack.SetAnsiString(-1, StringOfChar(TbtChar(Stack.GetInt(-2)), Stack.GetInt(-3))); // Replicate/StrOfChar
    29: // Assigned
      begin
        temp := NewTPSVariantIFC(Stack[Stack.Count - 2], True);
        if Temp.dta = nil then
        begin
          Result := False;
          Exit;
        end;
        case temp.aType.BaseType of
          btU8,
          btS8:
            b := TbtU8(temp.dta^) <> 0;
          btU16,
          btS16:
            b := TbtU16(temp.dta^) <> 0;
          btU32,
          btS32:
            b := TbtU32(temp.dta^) <> 0;
          btString,
          btPChar:
            b := TbtString(temp.dta^) <> '';
          {$IFNDEF PS_NOWIDESTRING}
          btWideString:
            b := TbtWideString(temp.dta^)<> '';
          btUnicodeString:
            b := TbtUnicodeString(temp.dta^)<> '';
          {$ENDIF}
          btArray,
          btClass
          {$IFNDEF PS_NOINTERFACES},
          btInterface
          {$ENDIF}:
            b := Pointer(temp.dta^) <> nil;
        else
          Result := False;
          Exit;
        end;
        if b then
          Stack.SetInt(-1, 1)
        else
          Stack.SetInt(-1, 0);
      end;
    30:
      begin {RaiseLastException}
        if (Caller.FExceptionStack.Count > 0) then
        begin
          pex := Caller.FExceptionStack.Data[Caller.fExceptionStack.Count - 1];
          if pex.ExceptOffset = Cardinal(InvalidVal - 1) then
          begin
            Tmp := pex.ExceptionObject;
            pex.ExceptionObject := nil;
            Caller.ExceptionProc(Caller.ExProc, pex.ExceptOffset, pex.ExceptionData, pex.ExceptionParam, tmp);
          end;
        end;
      end;
    31:
      Caller.CMD_Err2(TPSError(Stack.GetInt(-1)), Stack.GetAnsiString(-2)); {RaiseExeption}
    32:
      Stack.SetInt(-1, Ord(Caller.LastEx)); {ExceptionType}
    33:
      Stack.SetAnsiString(-1, Caller.LastExParam); {ExceptionParam}
    34:
      Stack.SetInt(-1, Caller.LastExProc); {ExceptionProc}
    35:
      Stack.SetInt(-1, Caller.LastExPos); {ExceptionPos}
    36:
      Stack.SetAnsiString(-1, PSErrorToString(TPSError(Stack.GetInt(-2)), Stack.GetAnsiString(-3))); {ExceptionToString}
    37:
      Stack.SetAnsiString(-1, TbtString(AnsiUpperCase(string(Stack.GetAnsiString(-2))))); // AnsiUppercase
    38:
      Stack.SetAnsiString(-1, TbtString(AnsiLowercase(string(Stack.GetAnsiString(-2))))); // AnsiLowerCase
    {$IFNDEF PS_NOINT64}
    39:
      Stack.SetInt64(-1, StrToInt64(string(Stack.GetAnsiString(-2))));  // StrToInt64
    40:
      Stack.SetAnsiString(-1, TbtString(SysUtils.IntToStr(Stack.GetInt64(-2))));// Int64ToStr
    41:
      Stack.SetInt64(-1, StrToInt64Def(string(Stack.GetAnsiString(-2)), Stack.GetInt64(-3))); // StrToInt64Def
    {$ENDIF}
    42:  // SizeOf
      begin
        temp := NewTPSVariantIFC(Stack[Stack.Count - 2], False);
        if Temp.aType = nil then
          Stack.SetInt(-1, 0)
        else
          Stack.SetInt(-1, Temp.aType.RealSize)
      end;
    {$IFNDEF PS_NOWIDESTRING}
    43: // WStrGet
      begin
        temp :=  NewTPSVariantIFC(Stack[Stack.Count - 2], True);
        if temp.dta = nil then
        begin
          Result := False;
          Exit;
        end;
        case temp.aType.BaseType of
          btWideString:
            begin
              I := Stack.GetInt(-3);
              if (i < 1) or (i > Length(TbtWideString(temp.Dta^))) then
              begin
                Caller.CMD_Err2(erCustomError, TbtString(RPS_OutOfStringRange));
                Result := False;
                Exit;
              end;
              Stack.SetInt(-1, Ord(TbtWideString(temp.Dta^)[i]));
            end;
          btUnicodeString:
            begin
              I := Stack.GetInt(-3);
              if (i < 1) or (i > Length(TbtUnicodeString(temp.Dta^))) then
              begin
                Caller.CMD_Err2(erCustomError, TbtString(RPS_OutOfStringRange));
                Result := False;
                Exit;
              end;
              Stack.SetInt(-1, Ord(TbtUnicodeString(temp.Dta^)[i]));
            end;

        else
          begin
            Result := False;
            Exit;
          end;
        end;
      end;
    44: // WStrSet
      begin
        temp := NewTPSVariantIFC(Stack[Stack.Count - 3], True);
        if temp.Dta = nil then
        begin
          Result := False;
          Exit;
        end;
        case temp.aType.BaseType of
          btWideString:
            begin
              I := Stack.GetInt(-2);
              if (i < 1) or (i > Length(TbtWideString(temp.Dta^))) then
              begin
                Caller.CMD_Err2(erCustomError, TbtString(RPS_OutOfStringRange));
                Result := True;
                Exit;
              end;
              TbtWideString(temp.Dta^)[i] := WideChar(Stack.GetInt(-1));
            end;

          btUnicodeString:
            begin
              I := Stack.GetInt(-2);
              if (i < 1) or (i > Length(TbtUnicodeString(temp.Dta^))) then
              begin
                Caller.CMD_Err2(erCustomError, TbtString(RPS_OutOfStringRange));
                Result := True;
                Exit;
              end;
              TbtUnicodeString(temp.Dta^)[i] := WideChar(Stack.GetInt(-1));
            end;
        else
          begin
            Result := False;
            Exit;
          end;
        end;
      end;
      {$ENDIF}
    else
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

//==============================================================================
//
// GetArrayLength
//
//==============================================================================
function GetArrayLength(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  arr: TPSVariantIFC;
begin
  Arr := NewTPSVariantIFC(Stack[Stack.Count - 2], True);
  if (arr.aType.BaseType <> btStaticArray) and ((arr.Dta = nil) or (arr.aType.BaseType <> btArray)) then
  begin
    Result := False;
    Exit;
  end;
  if arr.aType.BaseType = btStaticArray then
    Stack.SetInt(-1, TPSTypeRec_StaticArray(arr.aType).Size)
  else
    Stack.SetInt(-1, PSDynArrayGetLength(Pointer(arr.Dta^), arr.aType));
  Result := True;
end;

//==============================================================================
//
// SetArrayLength
//
//==============================================================================
function SetArrayLength(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  arr: TPSVariantIFC;
begin
  Arr := NewTPSVariantIFC(Stack[Stack.Count - 1], True);
  if (arr.Dta = nil) or (arr.aType.BaseType <> btArray) then
  begin
    Result := False;
    Exit;
  end;
  PSDynArraySetLength(Pointer(arr.Dta^), arr.aType, Stack.GetInt(-2));
  Result := True;
end;

//==============================================================================
//
// InterfaceProc
//
//==============================================================================
function InterfaceProc(Sender: TPSExec; p: TPSExternalProcRec; Tag: Pointer): Boolean; forward;

//==============================================================================
//
// RegisterInterfaceLibraryRuntime
//
//==============================================================================
procedure RegisterInterfaceLibraryRuntime(Se: TPSExec);
begin
  SE.AddSpecialProcImport('intf', InterfaceProc, nil);
end;

{$IFNDEF DELPHI6UP}

//==============================================================================
//
// Null
//
//==============================================================================
function Null: Variant;
begin
  Result := System.Null;
end;

//==============================================================================
//
// Unassigned
//
//==============================================================================
function Unassigned: Variant;
begin
  Result := System.Unassigned;
end;
{$ENDIF}

//==============================================================================
//
// Length_
//
//==============================================================================
function Length_(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  arr: TPSVariantIFC;
begin
  arr := NewTPSVariantIFC(Stack[Stack.Count - 2], False);
  case arr.aType.BaseType of
    btArray:
      begin
        Stack.SetInt(-1, PSDynArrayGetLength(Pointer(arr.Dta^), arr.aType));
        Result := True;
      end;
    btStaticArray:
      begin
        Stack.SetInt(-1, TPSTypeRec_StaticArray(arr.aType).Size);
        Result := True;
      end;
    btString:
      begin
        Stack.SetInt(-1, Length(TbtString(arr.Dta^)));
        Result := True;
      end;
    btChar:
      begin
        Stack.SetInt(-1, 1);
        Result := True;
      end;
    {$IFNDEF PS_NOWIDESTRING}
    btWideString:
      begin
        Stack.SetInt(-1, Length(TbtWideString(arr.Dta^)));
        Result := True;
      end;
    btUnicodeString:
      begin
        Stack.SetInt(-1, Length(TbtUnicodeString(arr.Dta^)));
        Result := True;
      end;
    {$ENDIF}
    btvariant:
      begin
        Stack.SetInt(-1, Length(Variant(arr.Dta^)));
        Result := True;
      end;
  else
    begin
      Caller.CMD_Err(erTypeMismatch);
      Result := True;
    end;
  end;
end;

//==============================================================================
//
// SetLength_
//
//==============================================================================
function SetLength_(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  arr: TPSVariantIFC;
begin
  Result := False;
  arr := NewTPSVariantIFC(Stack[Stack.Count - 1], True);
  if arr.aType.BaseType = btArray then
  begin
    PSDynArraySetLength(Pointer(arr.Dta^), arr.aType, Stack.GetInt(-2));
    Result := True;
  end
  else if arr.aType.BaseType = btString then
  begin
    SetLength(TbtString(arr.Dta^), Stack.GetInt(-2));
    Result := True;
  {$IFNDEF PS_NOWIDESTRING}
  end
  else if arr.aType.BaseType = btWideString then
  begin
    SetLength(TbtWideString(arr.Dta^), Stack.GetInt(-2));
    Result := True;
  end
  else if arr.aType.BaseType=btUnicodeString then
  begin
    SetLength(TbtUnicodeString(arr.Dta^), Stack.GetInt(-2));
    Result := True;
  {$ENDIF}
  end;
end;

//==============================================================================
//
// Low_
//
//==============================================================================
function Low_(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  arr: TPSVariantIFC;
begin
  Result := True;
  arr := NewTPSVariantIFC(Stack[Stack.Count - 2], False);
  case arr.aType.BaseType of
    btArray:
      Stack.SetInt(-1, 0);
    btStaticArray:
      Stack.SetInt(-1, TPSTypeRec_StaticArray(arr.aType).StartOffset);
    btString:
      Stack.SetInt(-1, 1);
    btU8:
      Stack.SetInt(-1, Low(Byte));      //Byte: 0
    btS8:
      Stack.SetInt(-1, Low(ShortInt));  //ShortInt: -128
    btU16:
      Stack.SetInt(-1, Low(Word));      //Word: 0
    btS16:
      Stack.SetInt(-1, Low(SmallInt));  //SmallInt: -32768
    btU32:
      Stack.SetInt(-1, Low(Cardinal));  //Cardinal/LongWord: 0
    btS32:
      Stack.SetInt(-1, Low(Integer));   //Integer/Longint: -2147483648
    {$IFNDEF PS_NOINT64}
    btS64:
      Stack.SetInt64(-1, Low(Int64));   //Int64: -9223372036854775808
    {$ENDIF}
    else
      Result := False;
  end;
end;

//==============================================================================
//
// High_
//
//==============================================================================
function High_(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  arr: TPSVariantIFC;
begin
  Result := True;
  arr := NewTPSVariantIFC(Stack[Stack.Count - 2], False);
  case arr.aType.BaseType of
    btArray:
      Stack.SetInt(-1, PSDynArrayGetLength(Pointer(arr.Dta^), arr.aType) - 1);
    btStaticArray:
      Stack.SetInt(-1, TPSTypeRec_StaticArray(arr.aType).StartOffset + TPSTypeRec_StaticArray(arr.aType).Size - 1);
    btString:
      Stack.SetInt(-1, Length(TbtString(arr.Dta^)));
    btU8:
      Stack.SetInt(-1, High(Byte));     //Byte: 255
    btS8:
      Stack.SetInt(-1, High(ShortInt)); //ShortInt: 127
    btU16:
      Stack.SetInt(-1, High(Word));     //Word: 65535
    btS16:
      Stack.SetInt(-1, High(SmallInt)); //SmallInt: 32767
    btU32:
      Stack.SetUInt(-1, High(Cardinal));//Cardinal/LongWord: 4294967295
    btS32:
      Stack.SetInt(-1, High(Integer));  //Integer/Longint: 2147483647
    {$IFNDEF PS_NOINT64}
    btS64:
      Stack.SetInt64(-1, High(Int64));  //Int64: 9223372036854775807
    {$ENDIF}
    else
      Result := False;
  end;
end;

//==============================================================================
//
// Dec_
//
//==============================================================================
function Dec_(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  arr: TPSVariantIFC;
begin
  Result := True;
  arr := NewTPSVariantIFC(Stack[Stack.Count - 1], True);
  case arr.aType.BaseType of
    btU8:
      Stack.SetInt(-1, TbtU8(arr.dta^) - 1);     //Byte
    btS8:
      Stack.SetInt(-1, TbtS8(arr.dta^) - 1);     //ShortInt
    btU16:
      Stack.SetInt(-1, TbtU16(arr.dta^) - 1);    //Word
    btS16:
      Stack.SetInt(-1, TbtS16(arr.dta^) - 1);    //SmallInt
    btU32:
      Stack.SetInt(-1, TbtU32(arr.dta^) - 1);    //Cardinal/LongWord
    btS32:
      Stack.SetInt(-1, TbtS32(arr.dta^) - 1);    //Integer/Longint
    {$IFNDEF PS_NOINT64}
    btS64:
      Stack.SetInt64(-1, Tbts64(arr.dta^) - 1);
    {$ENDIF}
  else
    Result := False;
  end;
end;

//==============================================================================
//
// Inc_
//
//==============================================================================
function Inc_(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  arr: TPSVariantIFC;
begin
  Result := True;
  arr := NewTPSVariantIFC(Stack[Stack.Count - 1], True);
  case arr.aType.BaseType of
    btU8:
      Stack.SetInt(-1, TbtU8(arr.dta^) + 1);  //Byte
    btS8:
      Stack.SetInt(-1, TbtS8(arr.dta^) + 1);  //ShortInt
    btU16:
      Stack.SetInt(-1, TbtU16(arr.dta^) + 1); //Word
    btS16:
      Stack.SetInt(-1, TbtS16(arr.dta^) + 1); //SmallInt
    btU32:
      Stack.SetInt(-1, TbtU32(arr.dta^) + 1); //Cardinal/LongWord
    btS32:
      Stack.SetInt(-1, TbtS32(arr.dta^) + 1); //Integer/Longint
    {$IFNDEF PS_NOINT64}
    btS64:
      Stack.SetInt64(-1, Tbts64(arr.dta^) + 1);
    {$ENDIF}
  else
    Result := False;
  end;
end;

//==============================================================================
//
// Include_
//
//==============================================================================
function Include_(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  TheSet, NewMember: TPSVariantIFC;
  SetData: PByteArray;
  Val: TbtU8;
begin
  TheSet := NewTPSVariantIFC(Stack[Stack.Count - 1], True);
  NewMember := NewTPSVariantIFC(Stack[Stack.Count - 2], False);
  Result := (TheSet.aType.BaseType = btSet) and (NewMember.aType.BaseType = btU8);
  if not Result then
    Exit;
  SetData := TheSet.Dta;
  Val := TbtU8(NewMember.dta^);
  SetData^[Val shr 3] := SetData^[Val shr 3] or (1 shl (Val and 7));
end;

//==============================================================================
//
// Exclude_
//
//==============================================================================
function Exclude_(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  TheSet, NewMember: TPSVariantIFC;
  SetData: PByteArray;
  Val: TbtU8;
begin
  TheSet := NewTPSVariantIFC(Stack[Stack.Count - 1], True);
  NewMember := NewTPSVariantIFC(Stack[Stack.Count - 2], False);
  Result := (TheSet.aType.BaseType = btSet) and (NewMember.aType.BaseType = btU8);
  if not Result then
    Exit;
  SetData := TheSet.Dta;
  Val := TbtU8(NewMember.dta^);
  SetData^[Val shr 3] := SetData^[Val shr 3] and not (1 shl (Val and 7));
end;

{$IFNDEF DELPHI6UP}

//==============================================================================
//
// _VarArrayGet
//
//==============================================================================
function _VarArrayGet(var S: Variant; I: Integer): Variant;
begin
  Result := VarArrayGet(S, [I]);
end;

//==============================================================================
//
// _VarArraySet
//
//==============================================================================
procedure _VarArraySet(const c: Variant; I: Integer; var s: Variant);
begin
  VarArrayPut(s, c, [i]);
end;
{$ENDIF}

//==============================================================================
//
// TPSExec.RegisterStandardProcs
//
//==============================================================================
procedure TPSExec.RegisterStandardProcs;
begin
  { The following needs to be in synch in these 3 functions:
    -UPSCompiler.TPSPascalCompiler.DefineStandardProcedures
    -UPSRuntime.DefProc
    -UPSRuntime.TPSExec.RegisterStandardProcs
  }
  RegisterFunctionName('!NOTIFICATIONVARIANTSET', NVarProc, Pointer(0), nil);
  RegisterFunctionName('!NOTIFICATIONVARIANTGET', NVarProc, Pointer(1), nil);

  RegisterFunctionName('INTTOSTR', DefProc, Pointer(0), nil);
  RegisterFunctionName('STRTOINT', DefProc, Pointer(1), nil);
  RegisterFunctionName('STRTOINTDEF', DefProc, Pointer(2), nil);
  RegisterFunctionName('POS', DefProc, Pointer(3), nil);
  RegisterFunctionName('COPY', DefProc, Pointer(4), nil);
  RegisterFunctionName('DELETE', DefProc, Pointer(5), nil);
  RegisterFunctionName('INSERT', DefProc, Pointer(6), nil);

  RegisterFunctionName('STRGET', DefProc, Pointer(7), nil);
  RegisterFunctionName('STRSET', DefProc, Pointer(8), nil);
  RegisterFunctionName('UPPERCASE', DefProc, Pointer(10), nil);
  RegisterFunctionName('LOWERCASE', DefProc, Pointer(11), nil);
  RegisterFunctionName('TRIM', DefProc, Pointer(12), nil);

  RegisterFunctionName('LENGTH', Length_, nil, nil);
  RegisterFunctionName('SETLENGTH', SetLength_, nil, nil);
  RegisterFunctionName('LOW', Low_, nil, nil);
  RegisterFunctionName('HIGH', High_, nil, nil);
  RegisterFunctionName('DEC', Dec_, nil, nil);
  RegisterFunctionName('INC', Inc_, nil, nil);
  RegisterFunctionName('INCLUDE', Include_, nil, nil);
  RegisterFunctionName('EXCLUDE', Exclude_, nil, nil);

  RegisterFunctionName('SIN', DefProc, Pointer(15), nil);
  RegisterFunctionName('COS', DefProc, Pointer(16), nil);
  RegisterFunctionName('SQRT', DefProc, Pointer(17), nil);
  RegisterFunctionName('ROUND', DefProc, Pointer(18), nil);
  RegisterFunctionName('TRUNC', DefProc, Pointer(19), nil);
  RegisterFunctionName('INT', DefProc, Pointer(20), nil);
  RegisterFunctionName('PI', DefProc, Pointer(21), nil);
  RegisterFunctionName('ABS', DefProc, Pointer(22), nil);
  RegisterFunctionName('STRTOFLOAT', DefProc, Pointer(23), nil);
  RegisterFunctionName('FLOATTOSTR', DefProc, Pointer(24), nil);
  RegisterFunctionName('PADL', DefProc, Pointer(25), nil);
  RegisterFunctionName('PADR', DefProc, Pointer(26), nil);
  RegisterFunctionName('PADZ', DefProc, Pointer(27), nil);
  RegisterFunctionName('REPLICATE', DefProc, Pointer(28), nil);
  RegisterFunctionName('STRINGOFCHAR', DefProc, Pointer(28), nil);
  RegisterFunctionName('!ASSIGNED', DefProc, Pointer(29), nil);

  RegisterDelphiFunction(@Unassigned, 'UNASSIGNED', cdRegister);
  RegisterDelphiFunction(@VarIsEmpty, 'VARISEMPTY', cdRegister);
  {$IFDEF DELPHI7UP}
  RegisterDelphiFunction(@VarIsClear, 'VARISCLEAR', cdRegister);
  {$ENDIF}
  RegisterDelphiFunction(@Null, 'NULL', cdRegister);
  RegisterDelphiFunction(@VarIsNull, 'VARISNULL', cdRegister);
  {$IFNDEF FPC}
  RegisterDelphiFunction(@VarType, 'VARTYPE', cdRegister);
  {$ENDIF}
  {$IFNDEF PS_NOIDISPATCH}
  RegisterDelphiFunction(@IDispatchInvoke, 'IDISPATCHINVOKE', cdregister);
  {$ENDIF}

  RegisterFunctionName('GETARRAYLENGTH', GetArrayLength, nil, nil);
  RegisterFunctionName('SETARRAYLENGTH', SetArrayLength, nil, nil);

  RegisterFunctionName('RAISELASTEXCEPTION', DefPRoc, Pointer(30), nil);
  RegisterFunctionName('RAISEEXCEPTION', DefPRoc, Pointer(31), nil);
  RegisterFunctionName('EXCEPTIONTYPE', DefPRoc, Pointer(32), nil);
  RegisterFunctionName('EXCEPTIONPARAM', DefPRoc, Pointer(33), nil);
  RegisterFunctionName('EXCEPTIONPROC', DefPRoc, Pointer(34), nil);
  RegisterFunctionName('EXCEPTIONPOS', DefPRoc, Pointer(35), nil);
  RegisterFunctionName('EXCEPTIONTOSTRING', DefProc, Pointer(36), nil);
  RegisterFunctionName('ANSIUPPERCASE', DefProc, Pointer(37), nil);
  RegisterFunctionName('ANSILOWERCASE', DefProc, Pointer(38), nil);

  {$IFNDEF PS_NOINT64}
  RegisterFunctionName('STRTOINT64', DefProc, Pointer(39), nil);
  RegisterFunctionName('INT64TOSTR', DefProc, Pointer(40), nil);
  RegisterFunctionName('STRTOINT64DEF', DefProc, Pointer(41), nil);
  {$ENDIF}
  RegisterFunctionName('SIZEOF', DefProc, Pointer(42), nil);

  {$IFNDEF PS_NOWIDESTRING}
  RegisterFunctionName('WSTRGET', DefProc, Pointer(43), nil);
  RegisterFunctionName('WSTRSET', DefProc, Pointer(44), nil);
  {$ENDIF}
  RegisterFunctionName('SQR', DefProc, Pointer(45), nil);
  {$IFNDEF DELPHI6UP}
  RegisterDelphiFunction(@_VarArrayGet, 'VARARRAYGET', cdRegister);
  RegisterDelphiFunction(@_VarArraySet, 'VARARRAYSET', cdRegister);
  {$ENDIF}
  RegisterInterfaceLibraryRuntime(Self);
end;

//==============================================================================
//
// ToString
//
//==============================================================================
function ToString(p: PAnsiChar): TbtString;
begin
  SetString(Result, p, StrLen(p));
end;

//==============================================================================
//
// IntPIFVariantToVariant
//
//==============================================================================
function IntPIFVariantToVariant(Src: pointer; aType: TPSTypeRec; var Dest: Variant): Boolean;

  function BuildArray(P: Pointer; aType: TPSTypeRec; Len: Longint): Boolean;
  var
    i, elsize: Longint;
    v: variant;
  begin
    elsize := aType.RealSize;
    Dest := VarArrayCreate([0, Len - 1], varVariant);
    for i := 0 to Len - 1 do
    begin
      if not IntPIFVariantToVariant(p, aType, v) then
      begin
        Result := False;
        Exit;
      end;
      Dest[i] := v;
      p := Pointer(IPointer(p) + Cardinal(elSize));
    end;
    Result := True;
  end;

begin
  if aType = nil then
  begin
    Dest := null;
    Result := True;
    Exit;
  end;
  if aType.BaseType = btPointer then
  begin
    aType := TPSTypeRec(Pointer(IPointer(src) + PointerSize)^);
    Src := Pointer(Pointer(Src)^);
  end;

  case aType.BaseType of
    btVariant:
      Dest := variant(src^);
    btArray:
      if not BuildArray(Pointer(Src^), TPSTypeRec_Array(aType).ArrayType, PSDynArrayGetLength(Pointer(src^), aType)) then
      begin
        Result := False;
        Exit;
      end;
    btStaticArray:
      if not BuildArray(Pointer(Src), TPSTypeRec_StaticArray(aType).ArrayType, PSDynArrayGetLength(Pointer(src^), aType)) then
      begin
        Result := False;
        Exit;
      end;
    btU8:
      if aType.ExportName = 'BOOLEAN' then
        Dest := boolean(TbtU8(Src^) <> 0)
      else
        Dest := TbtU8(Src^);
    btS8:
      Dest := TbtS8(Src^);
    btU16:
      Dest := TbtU16(Src^);
    btS16:
      Dest := TbtS16(Src^);
    btU32:
      Dest := {$IFDEF DELPHI6UP}TbtU32{$ELSE}TbtS32{$ENDIF}(Src^);
    btS32:
      Dest := TbtS32(Src^);
    btSingle:
      Dest := TbtSingle(Src^);
    btCurrency:
      Dest := TbtCurrency(Src^);
    btDouble:
      begin
        if aType.ExportName = 'TDATETIME' then
          Dest := TDateTime(TbtDouble(Src^))
        else
          Dest := TbtDouble(Src^);
      end;
    btExtended:
      Dest := TbtExtended(Src^);
    btString:
      Dest := TbtString(Src^);
    btPChar:
      Dest := ToString(PAnsiChar(Src^));
    {$IFNDEF PS_NOINT64}
    {$IFDEF DELPHI6UP}
    btS64:
      Dest := TbtS64(Src^);
    {$ELSE}
    bts64:
      begin
        Result := False;
        Exit;
      end;
    {$ENDIF}
    {$ENDIF}
    btChar:
      Dest := TbtString(TbtChar(src^));
    {$IFNDEF PS_NOWIDESTRING}
    btWideString:
      Dest := TbtWideString(src^);
    btWideChar:
      Dest := TbtWideString(TbtWideChar(src^));
    btUnicodeString:
      Dest := TbtUnicodeString(src^);
    {$ENDIF}
  else
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

//==============================================================================
//
// PIFVariantToVariant
//
//==============================================================================
function PIFVariantToVariant(Src: PIFVariant; var Dest: Variant): Boolean;
begin
  Result := IntPIFVariantToVariant(@PPSVariantData(src).Data, Src.FType, Dest);
end;

//==============================================================================
//
// VariantToPIFVariant
//
//==============================================================================
function VariantToPIFVariant(Exec: TPSExec; const Src: Variant; Dest: PIFVariant): Boolean;
var
  TT: PIFTypeRec;
begin
  if Dest = nil then
  begin
    Result := False;
    Exit;
  end;
  tt := Exec.FindType2(btVariant);
  if tt = nil then
  begin
    Result := False;
    Exit;
  end;
  if Dest.FType.BaseType = btPointer then
    Result := Exec.SetVariantValue(PPSVariantPointer(Dest).DataDest, @Src, PPSVariantPointer(Dest).DestType, tt)
  else
    Result := Exec.SetVariantValue(@PPSVariantData(Dest).Data, @Src, Dest.FType, tt);
end;

type
  POpenArray = ^TOpenArray;
  TOpenArray = record
    AType: Byte; {0}
    OrgVar: PPSVariantIFC;
    FreeIt: Boolean;
    ElementSize,
    ItemCount: Longint;
    Data: Pointer;
    VarParam: Boolean;
  end;

//==============================================================================
//
// CreateOpenArray
//
//==============================================================================
function CreateOpenArray(VarParam: Boolean; Sender: TPSExec; val: PPSVariantIFC): POpenArray;
var
  datap, p: Pointer;
  ctype: TPSTypeRec;
  cp: Pointer;
  i: Longint;
begin
  if (Val.aType.BaseType <> btArray) and (val.aType.BaseType <> btStaticArray) then
  begin
    Result := nil;
    Exit;
  end;
  New(Result);
  Result.AType := 0;
  Result.OrgVar := Val;
  Result.VarParam := VarParam;

  if val.aType.BaseType = btStaticArray then
  begin
    Result^.ItemCount := TPSTypeRec_StaticArray(val.aType).Size;
    datap := Val.Dta;
  end
  else
  begin
    Result^.ItemCount := PSDynArrayGetLength(Pointer(Val.Dta^), val.aType);
    datap := Pointer(Val.Dta^);
  end;
  if TPSTypeRec_Array(Val.aType).ArrayType.BaseType <> btPointer then
  begin
    Result.FreeIt := False;
    Result.ElementSize := 0;
    Result.Data := datap;
    Exit;
  end;
  Result.FreeIt := True;
  Result.ElementSize := SizeOf(TVarRec);
  GetMem(Result.Data, Result.ItemCount * Result.ElementSize);
  P := Result.Data;
  FillChar(p^, Result^.ItemCount * Result^.ElementSize, 0);
  for i := 0 to Result^.ItemCount - 1 do
  begin
    ctype := Pointer(Pointer(IPointer(datap) + PointerSize)^);
    cp := Pointer(Datap^);
    if cp = nil then
    begin
      TVarRec(p^).VType := vtPointer;
      TVarRec(p^).VPointer := nil;
    end
    else
    begin
       case ctype.BaseType of
        btVariant:
          begin
            TVarRec(p^).VType := vtVariant;
            TVarRec(p^).VVariant := cp;
          end;
        btChar:
          begin
            TVarRec(p^).VType := vtChar;
            TVarRec(p^).VChar := TbtChar(TbtChar(cp^));
          end;
        btSingle:
          begin
            TVarRec(p^).VType := vtExtended;
            New(TVarRec(p^).VExtended);
            TVarRec(p^).VExtended^ := TbtSingle(cp^);
          end;
        btExtended:
          begin
            TVarRec(p^).VType := vtExtended;
            New(TVarRec(p^).VExtended);
            TVarRec(p^).VExtended^ := TbtExtended(cp^);
          end;
        btDouble:
          begin
            TVarRec(p^).VType := vtExtended;
            New(TVarRec(p^).VExtended);
            TVarRec(p^).VExtended^ := TbtDouble(cp^);
          end;
        {$IFNDEF PS_NOWIDESTRING}
        btwidechar:
          begin
            TVarRec(p^).VType := vtWideChar;
            TVarRec(p^).VWideChar := TbtWideChar(cp^);
          end;
        {$IFDEF DELPHI2009UP}
        btUnicodeString:
          begin
            TVarRec(p^).VType := vtUnicodeString;
            TbtUnicodeString(TVarRec(p^).VUnicodeString) := TbtUnicodeString(cp^);
          end;
        {$ELSE}
        btUnicodeString,
        {$ENDIF}
        btWideString:
          begin
            TVarRec(p^).VType := vtWideString;
            TbtWideString(TVarRec(p^).VWideString) := TbtWideString(cp^);
          end;
        {$ENDIF}
        btU8:
          begin
            TVarRec(p^).VType := vtInteger;
            TVarRec(p^).VInteger := TbtU8(cp^);
          end;
        btS8:
          begin
            TVarRec(p^).VType := vtInteger;
            TVarRec(p^).VInteger := TbtS8(cp^);
          end;
        btU16:
          begin
            TVarRec(p^).VType := vtInteger;
            TVarRec(p^).VInteger := TbtU16(cp^);
          end;
        btS16:
          begin
            TVarRec(p^).VType := vtInteger;
            TVarRec(p^).VInteger := TbtS16(cp^);
          end;
        btU32:
          begin
            TVarRec(p^).VType := vtInteger;
            TVarRec(p^).VInteger := TbtU32(cp^);
          end;
        btS32:
          begin
            TVarRec(p^).VType := vtInteger;
            TVarRec(p^).VInteger := TbtS32(cp^);
          end;
        {$IFNDEF PS_NOINT64}
        btS64:
          begin
            TVarRec(p^).VType := vtInt64;
            New(TVarRec(p^).vInt64);
            TVarRec(p^).vInt64^ := TbtS64(cp^);
          end;
        {$ENDIF}
        btString:
          begin
            TVarRec(p^).VType := vtAnsiString;
            TbtString(TVarRec(p^).VAnsiString) := TbtString(cp^);
          end;
        btPChar:
          begin
            TVarRec(p^).VType := vtPchar;
            TVarRec(p^).VPChar := pointer(cp^);
          end;
        btClass:
          begin
            TVarRec(p^).VType := vtObject;
            TVarRec(p^).VObject := Pointer(cp^);
          end;
        {$IFNDEF PS_NOINTERFACES}
        {$IFDEF Delphi3UP}
        btInterface:
          begin
            TVarRec(p^).VType := vtInterface;
            IUnknown(TVarRec(p^).VInterface) := IUnknown(cp^);
          end;
        {$ENDIF}
        {$ENDIF}
      end;
    end;
    datap := Pointer(IPointer(datap) + (2 * SizeOf(Pointer) + SizeOf(LongBool)));
    p := PAnsiChar(p) + Result^.ElementSize;
  end;
end;

//==============================================================================
//
// DestroyOpenArray
//
//==============================================================================
procedure DestroyOpenArray(Sender: TPSExec; V: POpenArray);
var
  cp, datap: pointer;
  ctype: TPSTypeRec;
  p: PVarRec;
  i: Longint;
begin
  if v.FreeIt then // basetype = btPointer
  begin
    p := v^.Data;
    if v.OrgVar.aType.BaseType = btStaticArray then
      datap := v.OrgVar.Dta
    else
      datap := Pointer(v.OrgVar.Dta^);
    for i := 0 to v^.ItemCount - 1 do
    begin
      ctype := Pointer(Pointer(IPointer(datap) + PointerSize)^);
      cp := Pointer(Datap^);
      case ctype.BaseType of
        btU8:
          begin
            if v^.varParam then
              TbtU8(cp^) := TVarRec(p^).VInteger
          end;
        btS8:
          begin
            if v^.varParam then
              TbtS8(cp^) := TVarRec(p^).VInteger
          end;
        btU16:
          begin
            if v^.varParam then
              TbtU16(cp^) := TVarRec(p^).VInteger
          end;
        btS16:
          begin
            if v^.varParam then
              TbtS16(cp^) := TVarRec(p^).VInteger
          end;
        btU32:
          begin
            if v^.varParam then
              TbtU32(cp^) := TVarRec(p^).VInteger
          end;
        btS32:
          begin
            if v^.varParam then
              TbtS32(cp^) := TVarRec(p^).VInteger
          end;
        btChar:
          begin
            if v^.VarParam then
              TbtChar(cp^) := TbtChar(TVarRec(p^).VChar)
          end;
        btSingle:
          begin
            if v^.VarParam then
              TbtSingle(cp^) := TVarRec(p^).VExtended^;
            dispose(TVarRec(p^).VExtended);
          end;
        btDouble:
          begin
            if v^.VarParam then
              TbtDouble(cp^) := TVarRec(p^).VExtended^;
            dispose(TVarRec(p^).VExtended);
          end;
        btExtended:
          begin
            if v^.VarParam then
              TbtExtended(cp^) := TVarRec(p^).VExtended^;
            dispose(TVarRec(p^).VExtended);
          end;
        {$IFNDEF PS_NOINT64}
        btS64:
          begin
            if v^.VarParam then
              TbtS64(cp^) := TVarRec(p^).vInt64^;
            dispose(TVarRec(p^).vInt64);
          end;
        {$ENDIF}
        {$IFNDEF PS_NOWIDESTRING}
        btWideChar:
          begin
            if v^.varParam then
              TbtWideChar(cp^) := TVarRec(p^).VWideChar;
          end;
        {$IFDEF DELPHI2009UP}
        btUnicodeString:
          begin
            if v^.VarParam then
              TbtUnicodeString(cp^) := TbtUnicodeString(TVarRec(p^).VUnicodeString);
            finalize(TbtUnicodeString(TVarRec(p^).VUnicodeString));
          end;
        {$ELSE}
        btUnicodeString,
        {$ENDIF}
        btWideString:
          begin
            if v^.VarParam then
              TbtWideString(cp^) := TbtWideString(TVarRec(p^).VWideString);
            finalize(widestring(TVarRec(p^).VWideString));
          end;
        {$ENDIF}
        btString:
          begin
            if v^.VarParam then
              TbtString(cp^) := TbtString(TVarRec(p^).VString);
            finalize(TbtString(TVarRec(p^).VAnsiString));
          end;
        btClass:
          begin
            if v^.VarParam then
              Pointer(cp^) := TVarRec(p^).VObject;
          end;
          {$IFNDEF PS_NOINTERFACES}
          {$IFDEF Delphi3UP}
        btInterface:
          begin
            if v^.VarParam then
              IUnknown(cp^) := IUnknown(TVarRec(p^).VInterface);
            finalize(TbtString(TVarRec(p^).VAnsiString));
          end;
          {$ENDIF}
          {$ENDIF}
      end;
      datap := Pointer(IPointer(datap) + (2 * SizeOf(Pointer) + SizeOf(LongBool)));
      p := Pointer(IPointer(p) + Cardinal(v^.ElementSize));
    end;
    FreeMem(v.Data, v.ElementSize * v.ItemCount);
  end;
  Dispose(V);
end;

{$IFNDEF FPC}
{$IFDEF Delphi6UP}
  {$IFDEF CPUX64}
    {$include x64.inc}
  {$ELSE}
  {$include x86.inc}
  {$ENDIF}
{$ELSE}
  {$include x86.inc}
{$ENDIF}
{$ELSE}
{$IFDEF Delphi6UP}
  {$if defined(cpu86)}
    {$include x86.inc}
  {$elseif defined(cpupowerpc)}
    {$include powerpc.inc}
  {$elseif defined(cpuarm)}
    {$include arm.inc}
  {$elseif defined(CPUX86_64)}
    {$include x64.inc}
  {$ELSE}
    {$fatal Pascal Script is not supported for your architecture at the moment!}
  {$ifend}
{$ELSE}
{$include x86.inc}
{$ENDIF}
{$ENDIF}

type
  PScriptMethodInfo = ^TScriptMethodInfo;
  TScriptMethodInfo = record
    Se: TPSExec;
    ProcNo: Cardinal;
  end;

//==============================================================================
//
// MkMethod
//
//==============================================================================
function MkMethod(FSE: TPSExec; No: Cardinal): TMethod;
begin
  if (no = 0) or (no = InvalidVal) then
  begin
    Result.Code := nil;
    Result.Data := nil;
  end
  else
  begin
    Result.Code := @MyAllMethodsHandler;
    Result.Data := GetMethodInfoRec(FSE, No);
  end;
end;

//==============================================================================
//
// PFree
//
//==============================================================================
procedure PFree(Sender: TPSExec; P: PScriptMethodInfo);
begin
  Dispose(p);
end;

//==============================================================================
//
// GetMethodInfoRec
//
//==============================================================================
function GetMethodInfoRec(SE: TPSExec; ProcNo: Cardinal): Pointer;
var
  I: Longint;
  pp: PScriptMethodInfo;
begin
  if (ProcNo = 0) or (ProcNo = InvalidVal) then
  begin
    Result := nil;
    Exit;
  end;
  I := 2147483647;
  repeat
    pp := Se.FindProcResource2(@PFree, I);
    if (i <> -1) and (pp^.ProcNo = ProcNo) then
    begin
      Result := Pp;
      Exit;
    end;
  until i = -1;
  New(pp);
  pp^.Se := TPSExec(Se);
  pp^.ProcNo := Procno;
  Se.AddResource(@PFree, pp);
  Result := pp;
end;

type
  TPtrArr = array[0..1000] of Pointer;
  PPtrArr = ^TPtrArr;
  TByteArr = array[0..1000] of byte;
  PByteArr = ^TByteArr;
  PPointer = ^Pointer;

//==============================================================================
//
// VirtualMethodPtrToPtr
//
//==============================================================================
function VirtualMethodPtrToPtr(Ptr, FSelf: Pointer): Pointer;
{$IFDEF FPC}
var
  x: PPtrArr;
  {$ENDIF}
begin
  {$IFDEF FPC}
  x := Pointer(TObject(FSelf).ClassType) + vmtMethodStart;
  Result := x^[Longint(Ptr)];
  {$ELSE}
  Result := PPtrArr(PPointer(FSelf)^)^[Longint(Ptr)];
  {$ENDIF}
end;

//==============================================================================
//
// VirtualClassMethodPtrToPtr
//
//==============================================================================
function VirtualClassMethodPtrToPtr(Ptr, FSelf: Pointer): Pointer;
{$IFDEF FPC}
var
  x: PPtrArr;
  {$ENDIF}
begin
  {$IFDEF FPC}
  x := Pointer(FSelf) + vmtMethodStart;
  Result := x^[Longint(Ptr)];
  {$ELSE}
  Result := PPtrArr(FSelf)^[Longint(Ptr)];
  {$ENDIF}
end;

//==============================================================================
//
// CheckPackagePtr
//
//==============================================================================
procedure CheckPackagePtr(var P: PByteArr);
begin
  {$IFDEF Win32}
  if (word((@p[0])^) = $25FF) and (word((@p[6])^) = $C08B) then
  begin
    p := PPointer((@p[2])^)^;
  end;
  {$ENDIF}
  {$IFDEF Win64}
  if (word((@p[0])^) = $25FF) {and (word((@p[6])^)=$C08B)}then
  begin
    p := PPointer(NativeUInt(@P[0]) + Cardinal((@p[2])^) + 6{Instruction Size})^
  end;
  {$ENDIF}
end;

{$IFDEF VER90}{$DEFINE NO_vmtSelfPtr}{$ENDIF}
{$IFDEF FPC}{$DEFINE NO_vmtSelfPtr}{$ENDIF}

{$IFNDEF FPC}

//==============================================================================
//
// FindVirtualMethodPtr
//
//==============================================================================
function FindVirtualMethodPtr(Ret: TPSRuntimeClass; FClass: TClass; Ptr: Pointer): Pointer;
// Idea of getting the number of VMT items from GExperts
var
  p: PPtrArr;
  I: Longint;
begin
  p := Pointer(FClass);
  CheckPackagePtr(PByteArr(Ptr));
  if Ret.FEndOfVMT = MaxInt then
  begin
    I := {$IFDEF NO_vmtSelfPtr}-48{$ELSE}vmtSelfPtr{$ENDIF} div SizeOf(Pointer) + 1;
    while I < 0 do
    begin
      if I < 0 then
      begin
        if I <> ({$IFDEF VER90}-44{$ELSE}vmtTypeInfo{$ENDIF} div SizeOf(Pointer)) then
        begin // from GExperts code
          if (IPointer(p^[I]) > IPointer(p)) and ((IPointer(p^[I]) - IPointer(p))
            div
            //PointerSize < Ret.FEndOfVMT) then
            PointerSize < Cardinal(Ret.FEndOfVMT)) then
          begin
            Ret.FEndOfVMT := (IPointer(p^[I]) - IPointer(p)) div SizeOf(Pointer);
          end;
        end;
      end;
      Inc(I);
    end;
    if Ret.FEndOfVMT = MaxInt then
    begin
      Ret.FEndOfVMT := 0; // cound not find EndOfVMT
      Result := nil;
      Exit;
    end;
  end;
  I := 0;
  while I < Ret.FEndOfVMT do
  begin
    if p^[I] = Ptr then
    begin
      Result := Pointer(I);
      Exit;
    end;
    I := I + 1;
  end;
  Result := nil;
end;

{$ELSE}

//==============================================================================
//
// FindVirtualMethodPtr
//
//==============================================================================
function FindVirtualMethodPtr(Ret: TPSRuntimeClass; FClass: TClass; Ptr: Pointer): Pointer;
var
  x, p: PPtrArr;
  I: Longint;
  t: Pointer;
begin
  p := Pointer(FClass) + vmtMethodStart;
  I := 0;
  while (p^[I] <> nil) and (I < 10000) do
  begin
    if p^[I] = Ptr then
    begin
      Result := Pointer(I);
      x := Pointer(FClass) + vmtMethodStart;
      t := x^[I];
      Assert(t = Ptr, 'Computation of virtual method pointer fail : t<>Ptr');
      Exit;
    end;
    I := I + 1;
  end;
  Result := nil;
end;
{$ENDIF}

//==============================================================================
//
// NewTPSVariantIFC
//
//==============================================================================
function NewTPSVariantIFC(avar: PPSVariant; varparam: boolean): TPSVariantIFC;
begin
  Result.VarParam := varparam;
  if avar = nil then
  begin
    Result.aType := nil;
    Result.Dta := nil;
  end
  else
  begin
    Result.aType := avar.FType;
    Result.Dta := @PPSVariantData(avar).Data;
    if Result.aType.BaseType = btPointer then
    begin
      Result.aType := Pointer(Pointer(IPointer(Result.dta) + PointerSize)^);
      Result.Dta := Pointer(Result.dta^);
    end;
  end;
end;

//==============================================================================
//
// NewTPSVariantRecordIFC
//
//==============================================================================
function NewTPSVariantRecordIFC(avar: PPSVariant; Fieldno: Longint): TPSVariantIFC;
var
  offs: Cardinal;
begin
  Result := NewTPSVariantIFC(avar, False);
  if Result.aType.BaseType = btRecord then
  begin
    Offs := Cardinal(TPSTypeRec_Record(Result.aType).RealFieldOffsets[FieldNo]);
    Result.Dta := Pointer(IPointer(Result.dta) + Offs);
    Result.aType := TPSTypeRec_Record(Result.aType).FieldTypes[FieldNo];
  end
  else
  begin
    Result.Dta := nil;
    Result.aType := nil;
  end;
end;

//==============================================================================
//
// PSGetArrayField
//
//==============================================================================
function PSGetArrayField(const avar: TPSVariantIFC; Fieldno: Longint): TPSVariantIFC;
var
  offs: Cardinal;
  n: Longint;
begin
  Result := aVar;
  case Result.aType.BaseType of
    btStaticArray,
    btArray:
      begin
        if Result.aType.BaseType = btStaticArray then
          n := TPSTypeRec_StaticArray(Result.aType).Size
        else
          n := PSDynArrayGetLength(Pointer(Result.Dta^), Result.aType);
        if (FieldNo < 0) or (FieldNo >= n) then
        begin
          Result.Dta := nil;
          Result.aType := nil;
          Exit;
        end;
        Offs := TPSTypeRec_Array(Result.aType).ArrayType.RealSize * Cardinal(FieldNo);
        if Result.aType.BaseType = btStaticArray then
          Result.Dta := Pointer(IPointer(Result.dta) + Offs)
        else
          Result.Dta := Pointer(IPointer(Result.dta^) + Offs);
        Result.aType := TPSTypeRec_Array(Result.aType).ArrayType;
      end
  else
    Result.Dta := nil;
    Result.aType := nil;
  end;
end;

//==============================================================================
//
// PSGetRecField
//
//==============================================================================
function PSGetRecField(const avar: TPSVariantIFC; Fieldno: Longint): TPSVariantIFC;
var
  offs: Cardinal;
begin
  Result := aVar;
  if Result.aType.BaseType = btRecord then
  begin
    Offs := Cardinal(TPSTypeRec_Record(Result.aType).RealFieldOffsets[FieldNo]);
    Result.aType := TPSTypeRec_Record(Result.aType).FieldTypes[FieldNo];
    Result.Dta := Pointer(IPointer(Result.dta) + Offs);
  end
  else
  begin
    Result.Dta := nil;
    Result.aType := nil;
  end;
end;

//==============================================================================
//
// NewPPSVariantIFC
//
//==============================================================================
function NewPPSVariantIFC(avar: PPSVariant; varparam: boolean): PPSVariantIFC;
begin
  New(Result);
  Result^ := NewTPSVariantIFC(avar, varparam);
end;

//==============================================================================
//
// DisposePPSVariantIFC
//
//==============================================================================
procedure DisposePPSVariantIFC(aVar: PPSVariantIFC);
begin
  if avar <> nil then
    Dispose(avar);
end;

//==============================================================================
//
// DisposePPSVariantIFCList
//
//==============================================================================
procedure DisposePPSVariantIFCList(list: TPSList);
var
  i: Longint;
begin
  for i := list.Count - 1 downto 0 do
    DisposePPSVariantIFC(list[i]);
  list.free;
end;

//==============================================================================
//
// ClassCallProcMethod
//
//==============================================================================
function ClassCallProcMethod(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  i: Integer;
  MyList: TPSList;
  n: PIFVariant;
  v: PPSVariantIFC;
  FSelf: Pointer;
  CurrStack: Cardinal;
  cc: TPSCallingConvention;
  s: TbtString;
begin
  s := p.Decl;
  if Length(S) < 2 then
  begin
    Result := False;
    Exit;
  end;
  cc := TPSCallingConvention(s[1]);
  Delete(s, 1, 1);
  if s[1] = #0 then
    n := Stack[Stack.Count - 1]
  else
    n := Stack[Stack.Count - 2];
  if (n = nil) or (n^.FType.BaseType <> btClass) or (PPSVariantClass(n).Data = nil) then
  begin
    Caller.CMD_Err(erNullPointerException);
    Result := False;
    Exit;
  end;
  FSelf := PPSVariantClass(n).Data;
  CurrStack := Cardinal(Stack.Count) - Cardinal(Length(s)) - 1;
  if s[1] = #0 then
    Inc(CurrStack);
  MyList := TPSList.Create;
  for i := 2 to Length(s) do
  begin
    MyList.Add(nil);
  end;
  for i := Length(s) downto 2 do
  begin
    n := Stack[CurrStack];
    MyList[i - 2] := NewPPSVariantIFC(n, s[i] <> #0);
    Inc(CurrStack);
  end;
  if s[1] <> #0 then
  begin
    v := NewPPSVariantIFC(Stack[CurrStack + 1], True);
  end
  else
    v := nil;
  try
    if p.Ext2 = nil then
      Result := Caller.InnerfuseCall(FSelf, p.Ext1, cc, MyList, v)
    else
      Result := Caller.InnerfuseCall(FSelf, VirtualMethodPtrToPtr(p.Ext1, FSelf), cc, MyList, v);
  finally
    DisposePPSVariantIFC(v);
    DisposePPSVariantIFCList(mylist);
  end;
end;

//==============================================================================
//
// ClassCallProcConstructor
//
//==============================================================================
function ClassCallProcConstructor(Caller: TPSExec; p: TPSExternalProcRec;
  Global, Stack: TPSStack): Boolean;
var
  i, h: Longint;
  v: PPSVariantIFC;
  MyList: TPSList;
  n: PIFVariant;
  FSelf: Pointer;
  CurrStack: Cardinal;
  cc: TPSCallingConvention;
  s: TbtString;
  FType: PIFTypeRec;
  x: TPSRuntimeClass;
  IntVal: PIFVariant;
begin
  n := Stack[Stack.Count - 2];
  if (n = nil) or (n^.FType.BaseType <> btU32) then
  begin
    Result := False;
    Exit;
  end;
  FType := Caller.GetTypeNo(PPSVariantU32(n).Data);
  if FType = nil then
  begin
    Result := False;
    Exit;
  end;
  h := MakeHash(FType.ExportName);
  FSelf := nil;
  for i := 0 to TPSRuntimeClassImporter(p.Ext2).FClasses.Count - 1 do
  begin
    x := TPSRuntimeClassImporter(p.Ext2).FClasses[i];
    if (x.FClassNameHash = h) and (x.FClassName = FType.ExportName) then
    begin
      FSelf := x.FClass;
    end;
  end;
  if FSelf = nil then
  begin
    Result := False;
    Exit;
  end;
  s := p.Decl;
  if Length(S) < 2 then
  begin
    Result := False;
    Exit;
  end;
  cc := TPSCallingConvention(s[1]);
  Delete(s, 1, 1);
  CurrStack := Cardinal(Stack.Count) - Cardinal(Length(s)) - 1;
  if s[1] = #0 then
    Inc(CurrStack);
  {$IFDEF CPU64}
  IntVal := CreateHeapVariant(Caller.FindType2(btS64));
  {$ELSE}
  IntVal := CreateHeapVariant(Caller.FindType2(btU32));
  {$ENDIF}
  if IntVal = nil then
  begin
    Result := False;
    Exit;
  end;
  {$IFDEF FPC}
  // under FPC a constructor it's called with Self=0 (EAX) and
  // the VMT class pointer in EDX so they are effectively swaped
  // using register calling convention
  {$IFDEF CPU64}
  PPSVariantU32(IntVal).Data := Int64(FSelf);
  {$ELSE}
  PPSVariantU32(IntVal).Data := Cardinal(FSelf);
  {$ENDIF}
  FSelf := pointer(1);
  {$ELSE}
  PPSVariantU32(IntVal).Data := 1;
  {$ENDIF}
  MyList := TPSList.Create;
  MyList.Add(NewPPSVariantIFC(intval, False));
  for i := 2 to Length(s) do
  begin
    MyList.Add(nil);
  end;
  for i := Length(s) downto 2 do
  begin
    n :=Stack[CurrStack];
//    if s[i] <> #0 then
//    begin
//      MyList[i - 2] := NewPPSVariantIFC(n, s[i] <> #0);
//    end;
    MyList[i - 1] := NewPPSVariantIFC(n, s[i] <> #0);
    Inc(CurrStack);
  end;
  if s[1] <> #0 then
  begin
    v := NewPPSVariantIFC(Stack[CurrStack + 1], True);
  end
  else
    v := nil;
  try
    Result := Caller.InnerfuseCall(FSelf, p.Ext1, {$IFDEF FPC}TPSCallingConvention(Integer(cc) or 64){$ELSE}cc{$ENDIF}, MyList, v);
  finally
    DisposePPSVariantIFC(v);
    DisposePPSVariantIFCList(mylist);
    DestroyHeapVariant(intval);
  end;
end;

//==============================================================================
//
// ClassCallProcVirtualConstructor
//
//==============================================================================
function ClassCallProcVirtualConstructor(Caller: TPSExec; p: TPSExternalProcRec;
  Global, Stack: TPSStack): Boolean;
var
  i, h: Longint;
  v: PPSVariantIFC;
  MyList: TPSList;
  n: PIFVariant;
  FSelf: Pointer;
  CurrStack: Cardinal;
  cc: TPSCallingConvention;
  s: TbtString;
  FType: PIFTypeRec;
  x: TPSRuntimeClass;
  IntVal: PIFVariant;
begin
  n := Stack[Stack.Count - 2];
  if (n = nil) or (n^.FType.BaseType <> btU32) then
  begin
    Caller.CMD_Err(erNullPointerException);
    Result := False;
    Exit;
  end;
  FType := Caller.GetTypeNo(PPSVariantU32(N).Data);
  if FType = nil then
  begin
    Caller.CMD_Err(erNullPointerException);
    Result := False;
    Exit;
  end;
  h := MakeHash(FType.ExportName);
  FSelf := nil;
  for i := 0 to TPSRuntimeClassImporter(p.Ext2).FClasses.Count - 1 do
  begin
    x := TPSRuntimeClassImporter(p.Ext2).FClasses[i];
    if (x.FClassNameHash = h) and (x.FClassName = FType.ExportName) then
    begin
      FSelf := x.FClass;
    end;
  end;
  if FSelf = nil then
  begin
    Result := False;
    Exit;
  end;
  s := p.Decl;
  if Length(S) < 2 then
  begin
    Result := False;
    Exit;
  end;
  cc := TPSCallingConvention(s[1]);
  Delete(s, 1, 1);
  CurrStack := Cardinal(Stack.Count) - Cardinal(Length(s)) - 1;
  if s[1] = #0 then
    Inc(CurrStack);
  IntVal := CreateHeapVariant(Caller.FindType2(btU32));
  if IntVal = nil then
  begin
    Result := False;
    Exit;
  end;
  PPSVariantU32(IntVal).Data := 1;
  MyList := TPSList.Create;
  MyList.Add(NewPPSVariantIFC(intval, False));
  for i := 2 to Length(s) do
  begin
    MyList.Add(nil);
  end;
  for i := Length(s) downto 2 do
  begin
    n :=Stack[CurrStack];
    MyList[i - 1] := NewPPSVariantIFC(n, s[i] <> #0);
    Inc(CurrStack);
  end;
  if s[1] <> #0 then
  begin
    v := NewPPSVariantIFC(Stack[CurrStack + 1], True);
  end
  else
    v := nil;
  try
    Result := Caller.InnerfuseCall(FSelf, VirtualClassMethodPtrToPtr(p.Ext1, FSelf), {$IFDEF FPC}TPSCallingConvention(Integer(cc) or 128){$ELSE}cc{$ENDIF}, MyList, v);
  finally
    DisposePPSVariantIFC(v);
    DisposePPSVariantIFCList(mylist);
    DestroyHeapVariant(intval);
  end;
end;

//==============================================================================
//
// CastProc
//
//==============================================================================
function CastProc(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  TypeNo, InVar, ResVar: TPSVariantIFC;
  FSelf: TClass;
  FType: PIFTypeRec;
  H, I: Longint;
  x: TPSRuntimeClass;
begin
  TypeNo := NewTPSVariantIFC(Stack[Stack.Count - 3], False);
  InVar := NewTPSVariantIFC(Stack[Stack.Count - 2], False);
  ResVar := NewTPSVariantIFC(Stack[Stack.Count - 1], True);
  if (TypeNo.Dta = nil) or (InVar.Dta = nil) or (ResVar.Dta = nil) or
     (TypeNo.aType.BaseType <> btU32) or
     (resvar.aType <> Caller.FTypes[TbtU32(Typeno.dta^)]) then
  begin
    Result := False;
    Exit;
  end;
  {$IFNDEF PS_NOINTERFACES}
  if (invar.atype.BaseType = btInterface) and (resvar.aType.BaseType = btInterface) then
  begin
    {$IFNDEF Delphi3UP}
    if IUnknown(resvar.Dta^) <> nil then
      IUnknown(resvar.Dta^).Release;
    {$ENDIF}
    IUnknown(resvar.Dta^) := nil;
    if (IUnknown(invar.Dta^) = nil) or (IUnknown(invar.Dta^).QueryInterface(TPSTypeRec_Interface(ResVar.aType).Guid, IUnknown(resvar.Dta^)) <> 0) then
    begin
      Caller.CMD_Err2(erCustomError, TbtString(RPS_CannotCastInterface));
      Result := False;
      Exit;
    end;
  {$IFDEF Delphi3UP}
  end
  else if (Invar.aType.BaseType = btClass) and (resvar.aType.BaseType = btInterface) then
  begin
    {$IFNDEF Delphi3UP}
    if IUnknown(resvar.Dta^) <> nil then
      IUnknown(resvar.Dta^).Release;
    {$ENDIF}
    IUnknown(resvar.Dta^) := nil;
    if (TObject(invar.Dta^)= nil) or (not TObject(invar.dta^).GetInterface(TPSTypeRec_Interface(ResVar.aType).Guid, IUnknown(resvar.Dta^))) then
    begin
      Caller.CMD_Err2(erCustomError, TbtString(RPS_CannotCastInterface));
      Result := False;
      Exit;
    end;
  {$ENDIF}
  end
  else
  {$ENDIF}
  if (invar.aType.BaseType = btClass) and (resvar.aType.BaseType = btClass) then
  begin
    FType := Caller.GetTypeNo(TbtU32(TypeNo.Dta^));
    if FType = nil then
    begin
      Result := False;
      Exit;
    end;
    h := MakeHash(FType.ExportName);
    FSelf := nil;
    for i := 0 to TPSRuntimeClassImporter(p.Ext2).FClasses.Count - 1 do
    begin
      x := TPSRuntimeClassImporter(p.Ext2).FClasses[i];
      if (x.FClassNameHash = h) and (x.FClassName = FType.ExportName) then
      begin
        FSelf := x.FClass;
      end;
    end;
    if FSelf = nil then
    begin
      Result := False;
      Exit;
    end;

    try
      TObject(ResVar.Dta^) := TObject(InVar.Dta^) as FSelf;
    except
      Result := False;
      Caller.CMD_Err2(erCustomError, TbtString(RPS_CannotCastObject));
      Exit;
    end;
  end
  else
  begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

//==============================================================================
//
// NilProc
//
//==============================================================================
function NilProc(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  n: TPSVariantIFC;
begin
  n := NewTPSVariantIFC(Stack[Stack.Count - 1], True);
  if (n.Dta = nil) or ((n.aType.BaseType <> btClass) and (n.aType.BaseType <> btInterface)) then
  begin
    Result := False;
    Caller.CMD_Err(erNullPointerException);
    Exit;
  end;
  {$IFNDEF PS_NOINTERFACES}
  if n.aType.BaseType = btInterface then
  begin
    {$IFNDEF Delphi3UP}
    if IUnknown(n.Dta^) <> nil then
      IUnknown(n.Dta^).Release;
    {$ENDIF}
    IUnknown(n.Dta^) := nil;
  end
  else
  {$ENDIF}
    Pointer(n.Dta^) := nil;
  Result := True;
end;

//==============================================================================
//
// IntfCallProc
//
//==============================================================================
function IntfCallProc(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  i: Integer;
  MyList: TPSList;
  n: TPSVariantIFC;
  n2: PPSVariantIFC;
  FSelf: Pointer;
  CurrStack: Cardinal;
  cc: TPSCallingConvention;
  s: TbtString;
begin
  s := p.Decl;
  if Length(S) < 2 then
  begin
    Result := False;
    Exit;
  end;
  cc := TPSCallingConvention(s[1]);
  Delete(s, 1, 1);
  if s[1] = #0 then
    n := NewTPSVariantIFC(Stack[Stack.Count - 1], False)
  else
    n := NewTPSVariantIFC(Stack[Stack.Count - 2], False);
  if (n.dta = nil) or (n.atype.BaseType <> btInterface) or (Pointer(n.Dta^) = nil) then
  begin
    Caller.CMD_Err(erNullPointerException);
    Result := False;
    Exit;
  end;
  FSelf := Pointer(n.dta^);
  CurrStack := Cardinal(Stack.Count) - Cardinal(Length(s)) - 1;
  if s[1] = #0 then Inc(CurrStack);
  MyList := TPSList.Create;
  for i := 2 to Length(s) do
  begin
    MyList.Add(nil);
  end;
  for i := Length(s) downto 2 do
  begin
    MyList[i - 2] := NewPPSVariantIFC(Stack[CurrStack], s[i] <> #0);
    Inc(CurrStack);
  end;
  if s[1] <> #0 then
  begin
    n2 := NewPPSVariantIFC(Stack[CurrStack + 1], True);
  end
  else
    n2 := nil;
  try
    Caller.InnerfuseCall(FSelf, Pointer(Pointer(IPointer(FSelf^) + (Cardinal(p.Ext1) * SizeOf(Pointer)))^), cc, MyList, n2);
    Result := True;
  finally
    DisposePPSVariantIFC(n2);
    DisposePPSVariantIFCList(MyList);
  end;
end;

//==============================================================================
//
// InterfaceProc
//
//==============================================================================
function InterfaceProc(Sender: TPSExec; p: TPSExternalProcRec; Tag: Pointer): Boolean;
var
  s: TbtString;
begin
  s := p.Decl;
  Delete(s, 1, 5); // delete 'intf:'
  if s = '' then
  begin
    Result := False;
    Exit;
  end;
  if s[1] = '.'then
  begin
    Delete(s, 1, 1);
    if Length(S) < 6 then
    begin
      Result := False;
      Exit;
    end;
    p.ProcPtr := IntfCallProc;
    p.Ext1 := Pointer((@s[1])^); // Proc Offset
    Delete(s, 1, 4);
    P.Decl := s;
    Result := True;
  end
  else
    Result := False;
end;

//==============================================================================
//
// getMethodNo
//
//==============================================================================
function getMethodNo(P: TMethod; SE: TPSExec): Cardinal;
begin
  if (p.Code <> @MyAllMethodsHandler) or
     (p.Data = nil) or
     (PScriptMethodInfo(p.Data)^.Se <> se) then
    Result := 0
  else
  begin
    Result := PScriptMethodInfo(p.Data)^.ProcNo;
  end;
end;

//==============================================================================
//
// ClassCallProcProperty
//
//==============================================================================
function ClassCallProcProperty(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  n: TPSVariantIFC;
  ltemp: Longint;
  FSelf: Pointer;
  m: TMethod;
begin
  try
    if p.Ext2 = Pointer(0) then
    begin
      n := NewTPSVariantIFC(Stack[Stack.Count - 1], False);
      if (n.Dta = nil) or (n.aType.BaseType <> btClass) then
      begin
        Result := False;
        Caller.CMD_Err(erNullPointerException);
        Exit;
      end;
      FSelf := Pointer(n.dta^);
      if FSelf = nil then
      begin
        Caller.CMD_Err(erCouldNotCallProc);
        Result := False;
        Exit;
      end;
      n := NewTPSVariantIFC(Stack[Stack.Count - 2], False);
      if (PPropInfo(p.Ext1)^.PropType^.Kind = tkMethod) and ((n.aType.BaseType = btU32) or (n.aType.BaseType = btProcPtr)) then
      begin
        SetMethodProp(TObject(FSelf), PPropInfo(p.Ext1), MkMethod(Caller, TbtU32(n.dta^)));
      end
      else
      case n.aType.BaseType of
        btSet:
          begin
            ltemp := 0;
            Move(Byte(n.Dta^), ltemp, TPSTypeRec_Set(n.aType).aByteSize);
            SetOrdProp(TObject(FSelf), PPropInfo(p.Ext1), ltemp);
          end;
        btChar,
        btU8:
          SetOrdProp(TObject(FSelf), PPropInfo(p.Ext1), TbtU8(n.Dta^));
        btS8:
          SetOrdProp(TObject(FSelf), PPropInfo(p.Ext1), TbtS8(n.Dta^));
        {$IFNDEF PS_NOWIDESTRING}
        btwidechar,
        {$ENDIF}
        btU16:
          SetOrdProp(TObject(FSelf), PPropInfo(p.Ext1), TbtU16(n.Dta^));
        btS16:
          SetOrdProp(TObject(FSelf), PPropInfo(p.Ext1), TbtS16(n.Dta^));
        btU32:
          SetOrdProp(TObject(FSelf), PPropInfo(p.Ext1), TbtU32(n.Dta^));
        btS32:
          SetOrdProp(TObject(FSelf), PPropInfo(p.Ext1), TbtS32(n.Dta^));
        btSingle:
          SetFloatProp(TObject(FSelf), p.Ext1, TbtSingle(n.Dta^));
        btDouble:
          SetFloatProp(TObject(FSelf), p.Ext1, TbtDouble(n.Dta^));
        btExtended:
          SetFloatProp(TObject(FSelf), p.Ext1, TbtExtended(n.Dta^));
        btString:
          SetStrProp(TObject(FSelf), p.Ext1, string(TbtString(n.Dta^)));
        btPChar:
          SetStrProp(TObject(FSelf), p.Ext1, string(PAnsiChar(n.Dta^)));
        btClass:
          SetOrdProp(TObject(FSelf), P.Ext1, Longint(n.Dta^));
        {$IFDEF DELPHI6UP}
        {$IFNDEF PS_NOWIDESTRING}
        {$IFNDEF DELPHI2009UP}
        btUnicodeString,
        {$ENDIF}
        btWideString:
          SetWideStrProp(TObject(FSelf), P.Ext1, TbtWideString(n.dta^));
        {$IFDEF DELPHI2009UP}
        btUnicodeString:
          SetUnicodeStrProp(TObject(FSelf), P.Ext1, TbtUnicodeString(n.dta^));
        {$ENDIF}
        {$ENDIF}
        {$ENDIF}
        else
        begin
          Result := False;
          Exit;
        end;
      end;
      Result := True;
    end
    else
    begin
      n := NewTPSVariantIFC(Stack[Stack.Count - 2], False);
      if (n.dta = nil) or (n.aType.BaseType <> btClass) then
      begin
        Result := False;
        Caller.CMD_Err(erNullPointerException);
        Exit;
      end;
      FSelf := Pointer(n.dta^);
      if FSelf = nil then
      begin
        Caller.CMD_Err(erCouldNotCallProc);
        Result := False;
        Exit;
      end;
      n := NewTPSVariantIFC(Stack[Stack.Count - 1], False);
      if (PPropInfo(p.Ext1)^.PropType^.Kind = tkMethod) and ((n.aType.BaseType = btU32) or (n.aType.BaseType = btprocptr)) then
      begin
        m := GetMethodProp(TObject(FSelf), PPropInfo(p.Ext1));
        Cardinal(n.Dta^) := GetMethodNo(m, Caller);
        if Cardinal(n.dta^) = 0 then
        begin
          Pointer(Pointer((IPointer(n.dta) + PointerSize))^) := m.Data;
          Pointer(Pointer((IPointer(n.dta) + PointerSize2))^) := m.Code;
        end;
      end
      else
      case n.aType.BaseType of
        btSet:
          begin
            ltemp := GetOrdProp(TObject(FSelf), PPropInfo(p.Ext1));
            Move(ltemp, Byte(n.Dta^), TPSTypeRec_Set(n.aType).aByteSize);
          end;
        btU8:
          TbtU8(n.Dta^) := GetOrdProp(TObject(FSelf), p.Ext1);
        btS8:
          TbtS8(n.Dta^) := GetOrdProp(TObject(FSelf), p.Ext1);
        btU16:
          TbtU16(n.Dta^) := GetOrdProp(TObject(FSelf), p.Ext1);
        btS16:
          TbtS16(n.Dta^) := GetOrdProp(TObject(FSelf), p.Ext1);
        btU32:
          TbtU32(n.Dta^) := GetOrdProp(TObject(FSelf), p.Ext1);
        btS32:
          TbtS32(n.Dta^) := GetOrdProp(TObject(FSelf), p.Ext1);
        btSingle:
          TbtSingle(n.Dta^) := GetFloatProp(TObject(FSelf), p.Ext1);
        btDouble:
          TbtDouble(n.Dta^) := GetFloatProp(TObject(FSelf), p.Ext1);
        btExtended:
          TbtExtended(n.Dta^) := GetFloatProp(TObject(FSelf), p.Ext1);
        btString:
          TbtString(n.Dta^) := TbtString(GetStrProp(TObject(FSelf), p.Ext1));
        btClass:
          Longint(n.dta^) := GetOrdProp(TObject(FSelf), p.Ext1);
        {$IFDEF DELPHI6UP}
        {$IFNDEF PS_NOWIDESTRING}
        {$IFDEF DELPHI2009UP}
        btUnicodeString:
          TbtUnicodeString(n.dta^) := GetUnicodeStrProp(TObject(FSelf), P.Ext1);
        {$ELSE}
        btUnicodeString,
        {$ENDIF}
        btWideString:
          TbtWideString(n.dta^) := GetWideStrProp(TObject(FSelf), P.Ext1);
        {$ENDIF}
        {$ENDIF}
      else
        begin
          Result := False;
          Exit;
        end;
      end;
      Result := True;
    end;
  finally
  end;
end;

//==============================================================================
//
// ClassCallProcPropertyHelper
//
//==============================================================================
function ClassCallProcPropertyHelper(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  I, paramcnt: Longint;
  Params: TPSList;
  n: TPSVariantIFC;
  FSelf: Pointer;
begin
  if Length(P.Decl) < 4 then
  begin
    Result := False;
    Exit;
  end;
  paramcnt := Longint((@P.Decl[1])^);
  if Longint(Stack.Count) < paramcnt + 1 then
  begin
    Result := False;
    Exit;
  end;
  Dec(paramcnt);
  if p.Ext1 <> nil then // read
  begin
    n := NewTPSVariantIFC(Stack[Longint(Stack.Count) - 2], False);
    if (n.Dta = nil) or (n.aType.BaseType <> btClass) then
    begin
      Result := False;
      Caller.CMD_Err(erNullPointerException);
      Exit;
    end;
    FSelf := pointer(n.Dta^);
    if FSelf = nil then
    begin
      if not Caller.AllowNullClasses then
      begin
        Caller.CMD_Err(erCouldNotCallProc);
        Result := False;
        Exit;
      end;
    end;
    Params := TPSList.Create;
    Params.Add(NewPPSVariantIFC(Stack[Longint(Stack.Count) - 1], True));
    for i := Stack.Count - 3 downto Longint(Stack.Count) - paramcnt - 2 do
    begin
      Params.Add(NewPPSVariantIFC(Stack[I], False));
    end;
    try
      Result := Caller.InnerfuseCall(FSelf, p.Ext1, cdRegister, Params, nil);
    finally
      DisposePPSVariantIFCList(Params);
    end;
  end
  else
  begin
    n := NewTPSVariantIFC(Stack[Longint(Stack.Count) - 1], False);
    if (n.Dta = nil) or (n.aType.BaseType <> btClass) then
    begin
      Result := False;
      Caller.CMD_Err(erNullPointerException);
      Exit;
    end;
    FSelf := pointer(n.Dta^);
    if FSelf = nil then
    begin
      if not Caller.AllowNullClasses then
      begin
        Caller.CMD_Err(erCouldNotCallProc);
        Result := False;
        Exit;
      end;
    end;
    Params := TPSList.Create;
    Params.Add(NewPPSVariantIFC(Stack[Longint(Stack.Count) - paramcnt - 2], False));

    for i := Stack.Count - 2 downto Longint(Stack.Count) - paramcnt - 1 do
    begin
      Params.Add(NewPPSVariantIFC(Stack[I], False));
    end;
    try
      Result := Caller.InnerfuseCall(FSelf, p.Ext2, cdregister, Params, nil);
    finally
      DisposePPSVariantIFCList(Params);
    end;
  end;
end;

//==============================================================================
//
// ClassCallProcPropertyHelperName
//
//==============================================================================
function ClassCallProcPropertyHelperName(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  I, paramcnt: Longint;
  Params: TPSList;
  tt: PIFVariant;
  n: TPSVariantIFC;
  FSelf: Pointer;
begin
  if Length(P.Decl) < 4 then
  begin
    Result := False;
    Exit;
  end;
  paramcnt := Longint((@P.Decl[1])^);
  if Longint(Stack.Count) < paramcnt + 1 then
  begin
    Result := False;
    Exit;
  end;
  Dec(paramcnt);
  if p.Ext1 <> nil then // read
  begin
    n := NewTPSVariantIFC(Stack[Longint(Stack.Count) - 2], False);
    if (n.Dta = nil) or (n.aType.BaseType <> btClass) then
    begin
      Result := False;
      Caller.CMD_Err(erNullPointerException);
      Exit;
    end;
    FSelf := Tobject(n.dta^);
    Params := TPSList.Create;
    Params.Add(NewPPSVariantIFC(Stack[Longint(Stack.Count) - 1], True));
    for i := Stack.Count - 3 downto Longint(Stack.Count) - paramcnt - 2 do
      Params.Add(NewPPSVariantIFC(Stack[I], False));
    tt := CreateHeapVariant(Caller.FindType2(btString));
    if tt <> nil then
    begin
      PPSVariantAString(tt).Data := p.Name;
      Params.Add(NewPPSVariantIFC(tt, False));
    end;
    try
      Result := Caller.InnerfuseCall(FSelf, p.Ext1, cdRegister, Params, nil);
    finally
      DestroyHeapVariant(tt);
      DisposePPSVariantIFCList(Params);
    end;
  end
  else
  begin
    n := NewTPSVariantIFC(Stack[Longint(Stack.Count) - 1], False);
    if (n.Dta = nil) or (n.aType.BaseType <> btClass) then
    begin
      Result := False;
      Caller.CMD_Err(erNullPointerException);
      Exit;
    end;
    FSelf := Tobject(n.dta^);
    Params := TPSList.Create;
    Params.Add(NewPPSVariantIFC(Stack[Longint(Stack.Count) - 2], True));

    for i := Stack.Count - 2 downto Longint(Stack.Count) - paramcnt - 1 do
    begin
      Params.Add(NewPPSVariantIFC(Stack[I], False));
    end;
    tt := CreateHeapVariant(Caller.FindType2(btString));
    if tt <> nil then
    begin
      PPSVariantAString(tt).Data := p.Name;
      Params.Add(NewPPSVariantIFC(tt, False));
    end;
    try
      Result := Caller.InnerfuseCall(FSelf, p.Ext2, cdregister, Params, nil);
    finally
      DestroyHeapVariant(tt);
      DisposePPSVariantIFCList(Params);
    end;
  end;
end;

//==============================================================================
//
// ClassCallProcEventPropertyHelper
//
//==============================================================================
function ClassCallProcEventPropertyHelper(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
{Event property helper}
var
  I, paramcnt: Longint;
  Params: TPSList;
  n: TPSVariantIFC;
  data: TMethod;
  n2: PIFVariant;
  FSelf: Pointer;
begin
  if Length(P.Decl) < 4 then
  begin
    Result := False;
    Exit;
  end;
  paramcnt := Longint((@P.Decl[1])^);
  if Longint(Stack.Count) < paramcnt + 1 then
  begin
    Result := False;
    Exit;
  end;
  Dec(paramcnt);
  if p.Ext1 <> nil then // read
  begin
    n := NewTPSVariantIFC(Stack[Longint(Stack.Count) - 2], False);
    if (n.Dta = nil) or (n.aType.BaseType <> btClass) then
    begin
      Result := False;
      Caller.CMD_Err(erNullPointerException);
      Exit;
    end;
    FSelf := Tobject(n.dta^);
    n := NewTPSVariantIFC(Stack[Longint(Stack.Count) - 1], True); // Result
    if (n.aType.BaseType <> btU32) and (n.aType.BaseType <> btProcPtr) then
    begin
      Result := False;
      Caller.CMD_Err(erNullPointerException);
      Exit;
    end;
    n2 := CreateHeapVariant(Caller.FindType2(btPChar));
    if n2 = nil then
    begin
      Result := False;
      Exit;
    end;
    Params := TPSList.Create;
//{$IFDEF CPU64}
//{$ELSE}
    data.Code := nil;
    data.Data := nil;
//{$ENDIF}
    PPSVariantDynamicArray(n2)^.Data:= @data;
    Params.Add(NewPPSVariantIFC(n2, False));
    for i := Stack.Count - 3 downto Longint(Stack.Count) - paramcnt - 2 do
      Params.Add(NewPPSVariantIFC(Stack[i], False));
    try
      Result := Caller.InnerfuseCall(FSelf, p.Ext1, cdRegister, Params, nil);
    finally
      Cardinal(n.Dta^) := getMethodNo(data, Caller);
      if Cardinal(n.Dta^) = 0 then
      begin
        Pointer(Pointer((IPointer(n.dta) + PointerSize))^) := data.Data;
        Pointer(Pointer((IPointer(n.dta) + PointerSize2))^) := data.Code;
      end;
      DestroyHeapVariant(n2);
      DisposePPSVariantIFCList(Params);
    end;
  end
  else
  begin
    n := NewTPSVariantIFC(Stack[Longint(Stack.Count) - 1], False);
    if (n.Dta = nil) or (n.aType.BaseType <> btClass) then
    begin
      Result := False;
      Caller.CMD_Err(erNullPointerException);
      Exit;
    end;
    FSelf := Tobject(n.dta^);
    n := NewTPSVariantIFC(Stack[Longint(Stack.Count) - 2], False);
    if (n.Dta = nil) or ((n.aType.BaseType <> btU32) and (n.aType.BaseType <> btProcPtr)) then
    begin
      Result := False;
      Caller.CMD_Err(erNullPointerException);
      Exit;
    end;
    (*n2 := CreateHeapVariant(Caller.FindType2(btPchar));
    if n2 = nil then
    begin
      Result := False;
      Exit;
    end; *)

    //if (n.aType.BaseType = btProcPtr) and (Cardinal(n.dta^) = 0) then
    //  data := TMethod(Pointer(IPointer(n.dta^) + 4)^)
    //else
    //  data := MkMethod(Caller, Cardinal(n.dta^));

    Params := TPSList.Create;
    Params.Add(@n);

 //   for i := Stack.Count - 2 downto Longint(Stack.Count) - paramcnt - 1 do
 //   begin
//      Params.Add(NewPPSVariantIFC(Stack[I], False));
//    end;
    try
      Result := Caller.InnerfuseCall(FSelf, p.Ext2, cdregister, Params, nil);
    finally
      Params.Clear;
      //DestroyHeapVariant(n2);
      DisposePPSVariantIFCList(Params);
    end;
  end;
end;

{'class:'+CLASSNAME+'|'+FUNCNAME+'|'+chr(CallingConv)+chr(hasresult)+params

For property write functions there is an '@' after the funcname.
}

//==============================================================================
//
// SpecImport
//
//==============================================================================
function SpecImport(Sender: TPSExec; p: TPSExternalProcRec; Tag: Pointer): Boolean;
var
  H, I: Longint;
  S, s2: TbtString;
  CL: TPSRuntimeClass;
  Px: PClassItem;
  pp: PPropInfo;
  IsRead: Boolean;
begin
  s := p.Decl;
  Delete(s, 1, 6);
  if s = '-' then {nil function}
  begin
    p.ProcPtr := NilProc;
    Result := True;
    Exit;
  end;
  if s = '+' then {cast function}
  begin
    p.ProcPtr := CastProc;
    p.Ext2 := Tag;
    Result := True;
    Exit;
  end;
  s2 := Copy(S, 1, Pos(TbtChar('|'), s) - 1);
  Delete(s, 1, Length(s2) + 1);
  H := MakeHash(s2);
  ISRead := False;
  cl := nil;
  for I := TPSRuntimeClassImporter(Tag).FClasses.Count - 1 downto 0 do
  begin
    Cl := TPSRuntimeClassImporter(Tag).FClasses[I];
    if (Cl.FClassNameHash = h) and (cl.FClassName = s2) then
    begin
      IsRead := True;
      break;
    end;
  end;
  if not isRead then
  begin
    Result := False;
    Exit;
  end;
  s2 := Copy(S, 1, Pos(TbtChar('|'), s) - 1);
  Delete(s, 1, Length(s2) + 1);
  if (s2 <> '') and (s2[Length(s2)] = '@') then
  begin
    IsRead := False;
    Delete(S2, Length(s2), 1);
  end
  else
    IsRead := True;
  p.Name := s2;
  H := MakeHash(s2);
  for i := cl.FClassItems.Count - 1 downto 0 do
  begin
    px := cl.FClassItems[I];
    if (px^.FNameHash = h) and (px^.FName = s2) then
    begin
      p.Decl := s;
      case px^.b of
  {0: ext1=ptr}
  {1: ext1=pointerinlist}
  {2: ext1=propertyinfo}
  {3: ext1=readfunc; ext2=writefunc}
        4:
          begin
            p.ProcPtr := ClassCallProcConstructor;
            p.Ext1 := px^.Ptr;
            if p.Ext1 = nil then
            begin
              Result := False;
              Exit;
            end;
            p.Ext2 := Tag;
          end;
        5:
          begin
            p.ProcPtr := ClassCallProcVirtualConstructor;
            p.Ext1 := px^.Ptr;
            if p.Ext1 = nil then
            begin
              Result := False;
              Exit;
            end;
            p.Ext2 := Tag;
          end;
        6:
          begin
            p.ProcPtr := ClassCallProcEventPropertyHelper;
            if IsRead then
            begin
              p.Ext1 := px^.FReadFunc;
              if p.Ext1 = nil then
              begin
                Result := False;
                Exit;
              end;
              p.Ext2 := nil;
            end
            else
            begin
              p.Ext1 := nil;
              p.Ext2 := px^.FWriteFunc;
              if p.Ext2 = nil then
              begin
                Result := False;
                Exit;
              end;
            end;
          end;
        0:
          begin
            p.ProcPtr := ClassCallProcMethod;
            p.Ext1 := px^.Ptr;
            if p.Ext1 = nil then
            begin
              Result := False;
              Exit;
            end;
            p.Ext2 := nil;
          end;
        1:
          begin
            p.ProcPtr := ClassCallProcMethod;
            p.Ext1 := px^.PointerInList;
            //if p.Ext1 = nil then begin Result := False; Exit; end;
            p.ext2 := pointer(1);
          end;
        3:
          begin
            p.ProcPtr := ClassCallProcPropertyHelper;
            if IsRead then
            begin
              p.Ext1 := px^.FReadFunc;
              if p.Ext1 = nil then
              begin
                Result := False;
                Exit;
              end;
              p.Ext2 := nil;
            end
            else
            begin
              p.Ext1 := nil;
              p.Ext2 := px^.FWriteFunc;
              if p.Ext2 = nil then
              begin
                Result := False;
                Exit;
              end;
            end;
          end;
        7:
          begin
            p.ProcPtr := ClassCallProcPropertyHelperName;
            if IsRead then
            begin
              p.Ext1 := px^.FReadFunc;
              if p.Ext1 = nil then
              begin
                Result := False;
                Exit;
              end;
              p.Ext2 := nil;
            end
            else
            begin
              p.Ext1 := nil;
              p.Ext2 := px^.FWriteFunc;
              if p.Ext2 = nil then
              begin
                Result := False;
                Exit;
              end;
            end;
          end;
        else
         begin
           Result := False;
           Exit;
         end;
      end;
      Result := True;
      Exit;
    end;
  end;
  if cl.FClass.ClassInfo <> nil then
  begin
    pp := GetPropInfo(cl.FClass.ClassInfo, string(s2));
    if pp <> nil then
    begin
       p.ProcPtr := ClassCallProcProperty;
       p.Ext1 := pp;
       if IsRead then
         p.Ext2 := Pointer(1)
       else
         p.Ext2 := Pointer(0);
       Result := True;
    end
    else
      Result := False;
  end
  else
    Result := False;
end;

//==============================================================================
//
// RegisterClassLibraryRuntime
//
//==============================================================================
procedure RegisterClassLibraryRuntime(SE: TPSExec; Importer: TPSRuntimeClassImporter);
begin
  SE.AddSpecialProcImport('class', SpecImport, Importer);
end;

//==============================================================================
//
// TPSExec.ClearspecialProcImports
//
//==============================================================================
procedure TPSExec.ClearspecialProcImports;
var
  I: Longint;
  P: PSpecialProc;
begin
  for I := FSpecialProcList.Count - 1 downto 0 do
  begin
    P := FSpecialProcList[I];
    Dispose(p);
  end;
  FSpecialProcList.Clear;
end;

//==============================================================================
//
// TPSExec.RaiseCurrentException
//
//==============================================================================
procedure TPSExec.RaiseCurrentException;
var
  ExObj: TObject;
begin
  if ExEx = erNoError then
    Exit; // do nothing
  ExObj := Self.ExObject;
  if ExObj <> nil then
  begin
    Self.ExObject := nil;
    raise ExObj;
  end;
  raise EPSException.Create(PSErrorToString(ExceptionCode, ExceptionString), Self, ExProc, ExPos);
end;

//==============================================================================
//
// TPSExec.CMD_Err2
//
//==============================================================================
procedure TPSExec.CMD_Err2(EC: TPSError; const Param: TbtString);
begin
  CMD_Err3(EC, Param, Nil);
end;

//==============================================================================
//
// TPSExec.GetProcAsMethod
//
//==============================================================================
function TPSExec.GetProcAsMethod(const ProcNo: Cardinal): TMethod;
begin
  Result := MkMethod(Self, ProcNo);
end;

//==============================================================================
//
// TPSExec.GetProcAsMethodN
//
//==============================================================================
function TPSExec.GetProcAsMethodN(const ProcName: TbtString): TMethod;
var
  procno: Cardinal;
begin
  Procno := GetProc(ProcName);
  if Procno = InvalidVal then
  begin
    Result.Code := nil;
    Result.Data := nil;
  end
  else
    Result := MkMethod(Self, procno)
end;

//==============================================================================
//
// TPSExec.RegisterAttributeType
//
//==============================================================================
procedure TPSExec.RegisterAttributeType(useproc: TPSAttributeUseProc;
  const TypeName: TbtString);
var
  att: TPSAttributeType;
begin
  att := TPSAttributeType.Create;
  att.TypeName := TypeName;
  att.TypeNameHash := MakeHash(TypeName);
  att.UseProc := UseProc;
  FAttributeTypes.Add(att);
end;

//==============================================================================
//
// TPSExec.GetProcCount
//
//==============================================================================
function TPSExec.GetProcCount: Cardinal;
begin
  Result := FProcs.Count;
end;

//==============================================================================
//
// TPSExec.GetTypeCount
//
//==============================================================================
function TPSExec.GetTypeCount: Longint;
begin
  Result := FTypes.Count;
end;

//==============================================================================
//
// TPSExec.GetVarCount
//
//==============================================================================
function TPSExec.GetVarCount: Longint;
begin
  Result := FGlobalVars.Count;
end;

function TPSExec.FindSpecialProcImport(
  P: TPSOnSpecialProcImport): pointer;
var
  i: Longint;
  pr: PSpecialProc;
begin
  for i := FSpecialProcList.Count - 1 downto 0 do
  begin
    pr := FSpecialProcList[i];
    if @pr.P = @p then
    begin
      Result := pr.tag;
      Exit;
    end;
  end;
  Result := nil;
end;

//==============================================================================
//
// TPSExec.InvokeExternalMethod
//
//==============================================================================
function TPSExec.InvokeExternalMethod(At: TPSTypeRec_ProcPtr; Slf,
  Ptr: Pointer): Boolean;
var
  res: PPSVariantIFC;
  s: TbtString;
  CurrStack, i: Longint;
  n: PPSVariant;
  MyList: TPSList;
begin
  s := TPSTypeRec_ProcPtr(at).ParamInfo;
  CurrStack := Cardinal(FStack.Count) - Cardinal(Length(s));
  if s[1] = #0 then Inc(CurrStack);
  MyList := TPSList.Create;
  for i := 2 to Length(s) do
  begin
    MyList.Add(nil);
  end;
  for i := Length(s) downto 2 do
  begin
    n := FStack[CurrStack];
    MyList[i - 2] := NewPPSVariantIFC(n, s[i] <> #0);
    Inc(CurrStack);
  end;
  if s[1] <> #0 then
  begin
    res := NewPPSVariantIFC(FStack[CurrStack + 1], True);
  end
  else
    res := nil;
  Result := InnerfuseCall(Slf, Ptr, cdRegister, MyList, res);

  DisposePPSVariantIFC(res);
  DisposePPSVariantIFCList(mylist);
end;

//==============================================================================
//
// TPSExec.LastEx
//
//==============================================================================
function TPSExec.LastEx: TPSError;
var
  pp: TPSExceptionHandler;
begin
  if FExceptionStack.Count = 0 then
  begin
    Result := ExEx;
    Exit;
  end;
  pp := fExceptionStack[fExceptionStack.Count - 1];
  Result := pp.ExceptionData;
end;

//==============================================================================
//
// TPSExec.LastExParam
//
//==============================================================================
function TPSExec.LastExParam: TbtString;
var
  pp: TPSExceptionHandler;
begin
  if FExceptionStack.Count = 0 then
  begin
    Result := ExParam;
    Exit;
  end;
  pp := fExceptionStack[fExceptionStack.Count - 1];
  Result := pp.ExceptionParam;
end;

//==============================================================================
//
// TPSExec.LastExPos
//
//==============================================================================
function TPSExec.LastExPos: Integer;
var
  pp: TPSExceptionHandler;
begin
  if FExceptionStack.Count = 0 then
  begin
    Result := ExPos;
    Exit;
  end;
  pp := fExceptionStack[fExceptionStack.Count - 1];
  Result := pp.ExceptOffset;
end;

//==============================================================================
//
// TPSExec.LastExProc
//
//==============================================================================
function TPSExec.LastExProc: Integer;
var
  pp: TPSExceptionHandler;
begin
  if FExceptionStack.Count = 0 then
  begin
    Result := ExProc;
    Exit;
  end;
  pp := fExceptionStack[fExceptionStack.Count - 1];
  Result := FProcs.IndexOf(pp.CurrProc);
end;

//==============================================================================
//
// TPSExec.LastExObject
//
//==============================================================================
function TPSExec.LastExObject: TObject;
var
 pp: TPSExceptionHandler;
begin
 if FExceptionStack.Count = 0 then
 begin
   Result := ExObject;
   Exit;
 end;
 pp := fExceptionStack[fExceptionStack.Count - 1];
 Result := pp.ExceptionObject;
end;

{ TPSRuntimeClass }

//==============================================================================
//
// TPSRuntimeClass.Create
//
//==============================================================================
constructor TPSRuntimeClass.Create(aClass: TClass; const AName: TbtString);
begin
  inherited Create;
  FClass := AClass;
  if AName = '' then
  begin
    FClassName := FastUpperCase(TbtString(aClass.ClassName));
    FClassNameHash := MakeHash(FClassName);
  end
  else
  begin
    FClassName := FastUpperCase(AName);
    FClassNameHash := MakeHash(FClassName);
  end;
  FClassItems:= TPSList.Create;
  FEndOfVmt := MaxInt;
end;

//==============================================================================
//
// TPSRuntimeClass.Destroy
//
//==============================================================================
destructor TPSRuntimeClass.Destroy;
var
  I: Longint;
  P: PClassItem;
begin
  for i := FClassItems.Count - 1 downto 0 do
  begin
    P := FClassItems[I];
    Dispose(p);
  end;
  FClassItems.Free;
  inherited Destroy;
end;

//==============================================================================
//
// TPSRuntimeClass.RegisterVirtualAbstractMethod
//
//==============================================================================
procedure TPSRuntimeClass.RegisterVirtualAbstractMethod(ClassDef: TClass;
  ProcPtr: Pointer; const Name: TbtString);
var
  P: PClassItem;
begin
  New(P);
  p^.FName := FastUpperCase(Name);
  p^.FNameHash := MakeHash(p^.FName);
  p^.b := 1;
  p^.PointerInList := FindVirtualMethodPtr(Self, ClassDef, ProcPtr);
  FClassItems.Add(p);
end;

//==============================================================================
//
// TPSRuntimeClass.RegisterConstructor
//
//==============================================================================
procedure TPSRuntimeClass.RegisterConstructor(ProcPtr: Pointer; const Name: TbtString);
var
  P: PClassItem;
begin
  New(P);
  p^.FName := FastUpperCase(Name);
  p^.FNameHash := MakeHash(p^.FName);
  p^.b := 4;
  p^.Ptr := ProcPtr;
  FClassItems.Add(p);
end;

//==============================================================================
//
// TPSRuntimeClass.RegisterMethod
//
//==============================================================================
procedure TPSRuntimeClass.RegisterMethod(ProcPtr: Pointer; const Name: TbtString);
var
  P: PClassItem;
begin
  New(P);
  p^.FName := FastUpperCase(Name);
  p^.FNameHash := MakeHash(p^.FName);
  p^.b := 0;
  p^.Ptr := ProcPtr;
  FClassItems.Add(p);
end;

procedure TPSRuntimeClass.RegisterPropertyHelper(ReadFunc,
  WriteFunc: Pointer; const Name: TbtString);
var
  P: PClassItem;
begin
  New(P);
  p^.FName := FastUpperCase(Name);
  p^.FNameHash := MakeHash(p^.FName);
  p^.b := 3;
  p^.FReadFunc := ReadFunc;
  p^.FWriteFunc := WriteFunc;
  FClassItems.Add(p);
end;

//==============================================================================
//
// TPSRuntimeClass.RegisterVirtualConstructor
//
//==============================================================================
procedure TPSRuntimeClass.RegisterVirtualConstructor(ProcPtr: Pointer;
  const Name: TbtString);
var
  P: PClassItem;
begin
  New(P);
  p^.FName := FastUpperCase(Name);
  p^.FNameHash := MakeHash(p^.FName);
  p^.b := 5;
  p^.PointerInList := FindVirtualMethodPtr(Self, FClass, ProcPtr);
  FClassItems.Add(p);
end;

//==============================================================================
//
// TPSRuntimeClass.RegisterVirtualMethod
//
//==============================================================================
procedure TPSRuntimeClass.RegisterVirtualMethod(ProcPtr: Pointer; const Name: TbtString);
var
  P: PClassItem;
begin
  New(P);
  p^.FName := FastUpperCase(Name);
  p^.FNameHash := MakeHash(p^.FName);
  p^.b := 1;
  p^.PointerInList := FindVirtualMethodPtr(Self, FClass, ProcPtr);
  FClassItems.Add(p);
end;

procedure TPSRuntimeClass.RegisterEventPropertyHelper(ReadFunc,
  WriteFunc: Pointer; const Name: TbtString);
var
  P: PClassItem;
begin
  New(P);
  p^.FName := FastUpperCase(Name);
  p^.FNameHash := MakeHash(p^.FName);
  p^.b := 6;
  p^.FReadFunc := ReadFunc;
  p^.FWriteFunc := WriteFunc;
  FClassItems.Add(p);
end;

procedure TPSRuntimeClass.RegisterPropertyHelperName(ReadFunc,
  WriteFunc: Pointer; const Name: TbtString);
var
  P: PClassItem;
begin
  New(P);
  p^.FName := FastUpperCase(Name);
  p^.FNameHash := MakeHash(p^.FName);
  p^.b := 7;
  p^.FReadFunc := ReadFunc;
  p^.FWriteFunc := WriteFunc;
  FClassItems.Add(p);
end;

{ TPSRuntimeClassImporter }

//==============================================================================
//
// TPSRuntimeClassImporter.Add
//
//==============================================================================
function TPSRuntimeClassImporter.Add(aClass: TClass): TPSRuntimeClass;
begin
  Result := FindClass(TbtString(aClass.ClassName));
  if Result <> nil then
    Exit;
  Result := TPSRuntimeClass.Create(aClass, '');
  FClasses.Add(Result);
end;

//==============================================================================
//
// TPSRuntimeClassImporter.Add2
//
//==============================================================================
function TPSRuntimeClassImporter.Add2(aClass: TClass;
  const Name: TbtString): TPSRuntimeClass;
begin
  Result := FindClass(Name);
  if Result <> nil then
    Exit;
  Result := TPSRuntimeClass.Create(aClass, Name);
  FClasses.Add(Result);
end;

//==============================================================================
//
// TPSRuntimeClassImporter.Clear
//
//==============================================================================
procedure TPSRuntimeClassImporter.Clear;
var
  I: Longint;
begin
  for i := 0 to FClasses.Count - 1 do
  begin
    TPSRuntimeClass(FClasses[I]).Free;
  end;
  FClasses.Clear;
end;

//==============================================================================
//
// TPSRuntimeClassImporter.Create
//
//==============================================================================
constructor TPSRuntimeClassImporter.Create;
begin
  inherited Create;
  FClasses := TPSList.Create;
end;

//==============================================================================
//
// TPSRuntimeClassImporter.CreateAndRegister
//
//==============================================================================
constructor TPSRuntimeClassImporter.CreateAndRegister(Exec: TPSExec;
  AutoFree: Boolean);
begin
  inherited Create;
  FClasses := TPSList.Create;
  RegisterClassLibraryRuntime(Exec, Self);
  if AutoFree then
    Exec.AddResource(@RCIFreeProc, Self);
end;

//==============================================================================
//
// TPSRuntimeClassImporter.Destroy
//
//==============================================================================
destructor TPSRuntimeClassImporter.Destroy;
begin
  Clear;
  FClasses.Free;
  inherited Destroy;
end;

{$IFNDEF PS_NOINTERFACES}

//==============================================================================
//
// SetVariantToInterface
//
//==============================================================================
procedure SetVariantToInterface(V: PIFVariant; Cl: IUnknown);
begin
  if (v <> nil) and (v.FType.BaseType = btInterface) then
  begin
    PPSVariantinterface(v).Data := cl;
    {$IFNDEF Delphi3UP}
    if PPSVariantinterface(v).Data <> nil then
      PPSVariantinterface(v).Data.AddRef;
    {$ENDIF}
  end;
end;
{$ENDIF}

//==============================================================================
//
// SetVariantToClass
//
//==============================================================================
procedure SetVariantToClass(V: PIFVariant; Cl: TObject);
begin
  if (v <> nil) and (v.FType.BaseType = btClass) then
  begin
    PPSVariantclass(v).Data := cl;
  end;
end;

//==============================================================================
//
// BGRFW
//
//==============================================================================
function BGRFW(var s: TbtString): TbtString;
var
  l: Longint;
begin
  l := Length(s);
  while l > 0 do
  begin
    if s[l] = ' ' then
    begin
      Result := Copy(s, l + 1, Length(s) - l);
      Delete(s, l, Length(s) - l + 1);
      Exit;
    end;
    Dec(l);
  end;
  Result := s;
  s := '';
end;

{$IFDEF CPUX64}
{$DEFINE empty_methods_handler}
{$ENDIF}

{$IFDEF fpc}
  {$if defined(cpupowerpc) or defined(cpuarm) or defined(cpu64)}
    {$define empty_methods_handler}
  {$ifend}
{$ENDIF}

{$IFDEF empty_methods_handler}

//==============================================================================
//
// MyAllMethodsHandler
//
//==============================================================================
procedure MyAllMethodsHandler;
begin
end;
{$ELSE}

//==============================================================================
//
// MyAllMethodsHandler2
//
//==============================================================================
function MyAllMethodsHandler2(Self: PScriptMethodInfo; const Stack: PPointer; _EDX, _ECX: Pointer): Integer; forward;

//==============================================================================
//
// MyAllMethodsHandler
//
//==============================================================================
procedure MyAllMethodsHandler;
//  On entry:
//     EAX = Self pointer
//     EDX, ECX = param1 and param2
//     STACK = param3... paramcnt
asm
  push 0
  push ecx
  push edx
  mov edx, esp
  add edx, 16 // was 12
  pop ecx
  call MyAllMethodsHandler2
  pop ecx
  mov edx, [esp]
  add esp, eax
  mov [esp], edx
  mov eax, ecx
end;

//==============================================================================
//
// ResultAsRegister
//
//==============================================================================
function ResultAsRegister(b: TPSTypeRec): Boolean;
begin
  case b.BaseType of
    btSingle,
    btDouble,
    btExtended,
    btU8,
    btS8,
    btS16,
    btu16,
    btS32,
    btU32,
    {$IFDEF PS_FPCSTRINGWORKAROUND}
    btString,
    {$ENDIF}
    {$IFNDEF PS_NOINT64}
    bts64,
    {$ENDIF}
    btPChar,
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar,
    {$ENDIF}
    btChar,
    btClass,
    btEnum:
      Result := True;
    btSet:
      Result := b.RealSize <= PointerSize;
    btStaticArray:
      Result := b.RealSize <= PointerSize;
  else
    Result := False;
  end;
end;

//==============================================================================
//
// SupportsRegister
//
//==============================================================================
function SupportsRegister(b: TPSTypeRec): Boolean;
begin
  case b.BaseType of
    btU8,
    btS8,
    btS16,
    btu16,
    btS32,
    btU32,
    btstring,
    btClass,
    {$IFNDEF PS_NOINTERFACES}
    btinterface,
    {$ENDIF}
    btPChar,
    {$IFNDEF PS_NOWIDESTRING}
    btWideString,
    btUnicodeString,
    btWideChar,
    {$ENDIF}
    btChar,
    btArray,
    btEnum:
      Result := True;
    btSet:
      Result := b.RealSize <= PointerSize;
    btStaticArray:
      Result := b.RealSize <= PointerSize;
  else
    Result := False;
  end;
end;

//==============================================================================
//
// AlwaysAsVariable
//
//==============================================================================
function AlwaysAsVariable(aType: TPSTypeRec): Boolean;
begin
  case atype.BaseType of
    btVariant:
      Result := True;
    btSet:
      Result := atype.RealSize > PointerSize;
    btRecord:
      Result := atype.RealSize > PointerSize;
    btStaticArray:
      Result := atype.RealSize > PointerSize;
  else
    Result := False;
  end;
end;

//==============================================================================
//
// PutOnFPUStackExtended
//
//==============================================================================
procedure PutOnFPUStackExtended(ft: Extended);
asm
//  fstp tbyte ptr [ft]
  fld tbyte ptr [ft]
end;

//==============================================================================
//
// MyAllMethodsHandler2
//
//==============================================================================
function MyAllMethodsHandler2(Self: PScriptMethodInfo; const Stack: PPointer; _EDX, _ECX: Pointer): Integer;
var
  Decl: TbtString;
  I, C, regno: Integer;
  Params: TPSList;
  Res, Tmp: PIFVariant;
  cpt: PIFTypeRec;
  fmod: TbtChar;
  s,e: TbtString;
  FStack: pointer;
  ex: TPSExceptionHandler;
begin
  Decl := TPSInternalProcRec(Self^.Se.FProcs[Self^.ProcNo]).ExportDecl;
  FStack := Stack;
  Params := TPSList.Create;
  s := decl;
  GRFW(s);
  while s <> '' do
  begin
    Params.Add(nil);
    GRFW(s);
  end;
  c := Params.Count;
  regno := 0;
  Result := 0;
  s := decl;
  GRFW(s);
  for i := c - 1 downto 0 do
  begin
    e := GRFW(s);
    fmod := e[1];
    Delete(e, 1, 1);
    cpt := Self.Se.GetTypeNo(StrToInt(e));
    if ((fmod = '%') or (fmod = '!') or (AlwaysAsVariable(cpt))) and (RegNo < 2) then
    begin
      tmp := CreateHeapVariant(Self.Se.FindType2(btPointer));
      PPSVariantPointer(tmp).DestType := cpt;
      Params[i] := tmp;
      case regno of
        0:
          begin
            PPSVariantPointer(tmp).DataDest := Pointer(_EDX);
            Inc(regno);
          end;
        1:
          begin
            PPSVariantPointer(tmp).DataDest := Pointer(_ECX);
            Inc(regno);
          end;
(*        else begin
            PPSVariantPointer(tmp).DataDest := Pointer(FStack^);
            FStack := Pointer(IPointer(FStack) + 4);
          end;*)
      end;
    end
    else if SupportsRegister(cpt) and (RegNo < 2) then
    begin
      tmp := CreateHeapVariant(cpt);
      Params[i] := tmp;
      case regno of
        0:
          begin
            CopyArrayContents(@PPSVariantData(tmp)^.Data, @_EDX, 1, cpt);
            Inc(regno);
          end;
        1:
          begin
            CopyArrayContents(@PPSVariantData(tmp)^.Data, @_ECX, 1, cpt);
            Inc(regno);
          end;
(*        else
          begin
            CopyArrayContents(@PPSVariantData(tmp)^.Data, Pointer(FStack), 1, cpt);
            FStack := Pointer(IPointer(FStack) + 4);
          end;*)
      end;
(*  end
    else
    begin
      tmp := CreateHeapVariant(cpt);
      Params[i] := tmp;
      CopyArrayContents(@PPSVariantData(tmp)^.Data, Pointer(FStack), 1, cpt);
      FStack := Pointer(IPointer(FStack) + cpt.RealSize + 3 and not 3);*)
    end;
  end;
  s := decl;
  e := GRFW(s);

  if e <> '-1' then
  begin
    cpt := Self.Se.GetTypeNo(StrToInt(e));
    if not ResultAsRegister(cpt) then
    begin
      Res := CreateHeapVariant(Self.Se.FindType2(btPointer));
      PPSVariantPointer(Res).DestType := cpt;
      Params.Add(Res);
      case regno of
        0:
          begin
            PPSVariantPointer(Res).DataDest := Pointer(_EDX);
          end;
        1:
          begin
            PPSVariantPointer(Res).DataDest := Pointer(_ECX);
          end;
        else
          begin
            PPSVariantPointer(Res).DataDest := Pointer(FStack^);
            Inc(Result, PointerSize);
          end;
      end;
    end
    else
    begin
      Res := CreateHeapVariant(cpt);
      Params.Add(Res);
    end;
  end
  else
    Res := nil;
  s := decl;
  GRFW(s);
  for i := 0 to c - 1 do
  begin
    e := GRLW(s);
    fmod := e[1];
    Delete(e, 1, 1);
    if Params[i] <> nil then
      Continue;
    cpt := Self.Se.GetTypeNo(StrToInt(e));
    if (fmod = '%') or (fmod = '!') or (AlwaysAsVariable(cpt)) then
    begin
      tmp := CreateHeapVariant(Self.Se.FindType2(btPointer));
      PPSVariantPointer(tmp).DestType := cpt;
      Params[i] := tmp;
      PPSVariantPointer(tmp).DataDest := Pointer(FStack^);
      FStack := Pointer(IPointer(FStack) + PointerSize);
      Inc(Result, PointerSize);
    end
(*    else if SupportsRegister(cpt) then
    begin
      tmp := CreateHeapVariant(cpt);
      Params[i] := tmp;
      CopyArrayContents(@PPSVariantData(tmp)^.Data, Pointer(FStack), 1, cpt);
      FStack := Pointer(IPointer(FStack) + 4);
      end;
    end *)
    else
    begin
      tmp := CreateHeapVariant(cpt);
      Params[i] := tmp;
      CopyArrayContents(@PPSVariantData(tmp)^.Data, Pointer(FStack), 1, cpt);
      FStack := Pointer((IPointer(FStack) + cpt.RealSize + 3) and not 3);
      Inc(Result, (cpt.RealSize + 3) and not 3);
    end;
  end;
  ex := TPSExceptionHandler.Create;
  ex.FinallyOffset := InvalidVal;
  ex.ExceptOffset := InvalidVal;
  ex.Finally2Offset := InvalidVal;
  ex.EndOfBlock := InvalidVal;
  ex.CurrProc := nil;
  ex.BasePtr := Self.Se.FCurrStackBase;
  Ex.StackSize := Self.Se.FStack.Count;
  i := Self.Se.FExceptionStack.Add(ex);
  Self.Se.RunProc(Params, Self.ProcNo);
  if Self.Se.FExceptionStack[i] = ex then
  begin
    Self.Se.FExceptionStack.Remove(ex);
    ex.Free;
  end;

  if (Res <> nil) then
  begin
    Params.DeleteLast;
    if (ResultAsRegister(Res.FType)) then
    begin
      if (res^.FType.BaseType = btSingle) or
         (res^.FType.BaseType = btDouble) or
         (res^.FType.BaseType = btCurrency) or
         (res^.Ftype.BaseType = btExtended) then
      begin
        case Res^.FType.BaseType of
          btSingle:
            PutOnFPUStackExtended(PPSVariantSingle(res).Data);
          btDouble:
            PutOnFPUStackExtended(PPSVariantDouble(res).Data);
          btExtended:
            PutOnFPUStackExtended(PPSVariantExtended(res).Data);
          btCurrency:
            PutOnFPUStackExtended(PPSVariantCurrency(res).Data);
        end;
        DestroyHeapVariant(Res);
        Res := nil;
      end
      else
      begin
        {$IFNDEF PS_NOINT64}
        if res^.FType.BaseType <> btS64 then
        {$ENDIF}
          //CopyArrayContents(Pointer(Longint(Stack) - PointerSize2), @PPSVariantData(res)^.Data, 1, Res^.FType);
          CopyArrayContents(Pointer(Longint(Stack) - Longint(PointerSize2)), @PPSVariantData(res)^.Data, 1, Res^.FType);
      end;
    end;
    DestroyHeapVariant(res);
  end;
  for i := 0 to Params.Count - 1 do
    DestroyHeapVariant(Params[i]);
  Params.Free;
  if Self.Se.ExEx <> erNoError then
  begin
    if Self.Se.ExObject <> nil then
    begin
      FStack := Self.Se.ExObject;
      Self.Se.ExObject := nil;
      raise TObject(FStack);
    end
    else
      raise EPSException.Create(PSErrorToString(Self.SE.ExceptionCode, Self.Se.ExceptionString), Self.Se, Self.Se.ExProc, Self.Se.ExPos);
  end;
end;
{$ENDIF}

//==============================================================================
//
// TPSRuntimeClassImporter.FindClass
//
//==============================================================================
function TPSRuntimeClassImporter.FindClass(const Name: TbtString): TPSRuntimeClass;
var
  h, i: Longint;
  lName: TbtString;
  p: TPSRuntimeClass;
begin
  lName := FastUpperCase(Name);
  h := MakeHash(lName);
  for i := FClasses.Count - 1 downto 0 do
  begin
    p := FClasses[i];
    if (p.FClassNameHash = h) and (p.FClassName = lName) then
    begin
      Result := P;
      Exit;
    end;
  end;
  Result := nil;
end;

//==============================================================================
//
// DelphiFunctionProc
//
//==============================================================================
function DelphiFunctionProc(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack; CC: TPSCallingConvention): Boolean;
var
  i: Integer;
  MyList: TPSList;
  n: PPSVariantIFC;
  CurrStack: Cardinal;
  s: TbtString;
begin
  s := P.Decl;
  if Length(s) = 0 then
  begin
    Result := False;
    Exit;
  end;
  CurrStack := Cardinal(Stack.Count) - Cardinal(Length(s));
  if s[1] = #0 then
    Inc(CurrStack);
  MyList := TPSList.Create;

  for i := 2 to Length(s) do
  begin
    MyList.Add(nil);
  end;
  for i := Length(s) downto 2 do
  begin
    MyList[i - 2] := NewPPSVariantIFC(Stack[CurrStack], s[i] <> #0);
    Inc(CurrStack);
  end;
  if s[1] <> #0 then
  begin
    n := NewPPSVariantIFC(Stack[CurrStack], True);
  end
  else
    n := nil;
  try
    Result := Caller.InnerfuseCall(p.Ext2, p.Ext1, cc, MyList, n);
  finally
    DisposePPSVariantIFC(n);
    DisposePPSVariantIFCList(mylist);
  end;
end;

//==============================================================================
//
// DelphiFunctionProc_CDECL
//
//==============================================================================
function DelphiFunctionProc_CDECL(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  Result := DelphiFunctionProc(Caller, p, Global, Stack, cdCdecl);
end;

//==============================================================================
//
// DelphiFunctionProc_Register
//
//==============================================================================
function DelphiFunctionProc_Register(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  Result := DelphiFunctionProc(Caller, p, Global, Stack, cdRegister);
end;

//==============================================================================
//
// DelphiFunctionProc_Pascal
//
//==============================================================================
function DelphiFunctionProc_Pascal(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  Result := DelphiFunctionProc(Caller, p, Global, Stack, cdPascal);
end;

//==============================================================================
//
// DelphiFunctionProc_Stdcall
//
//==============================================================================
function DelphiFunctionProc_Stdcall(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  Result := DelphiFunctionProc(Caller, p, Global, Stack, cdStdCall);
end;

//==============================================================================
//
// DelphiFunctionProc_Safecall
//
//==============================================================================
function DelphiFunctionProc_Safecall(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  Result := DelphiFunctionProc(Caller, p, Global, Stack, cdSafeCall);
end;

//==============================================================================
//
// TPSExec.RegisterDelphiFunction
//
//==============================================================================
procedure TPSExec.RegisterDelphiFunction(ProcPtr: Pointer;
  const Name: TbtString; CC: TPSCallingConvention);
begin
  RegisterDelphiMethod(nil, ProcPtr, Name, CC);
end;

//==============================================================================
//
// TPSExec.RegisterDelphiMethod
//
//==============================================================================
procedure TPSExec.RegisterDelphiMethod(Slf, ProcPtr: Pointer;
  const Name: TbtString; CC: TPSCallingConvention);
begin
  case cc of
    cdRegister:
      RegisterFunctionName(Name, DelphiFunctionProc_Register, ProcPtr, Slf);
    cdPascal:
      RegisterFunctionName(Name, DelphiFunctionProc_Pascal, ProcPtr, Slf);
    cdStdCall:
      RegisterFunctionName(Name, DelphiFunctionProc_Stdcall, ProcPtr, Slf);
    cdSafeCall:
      RegisterFunctionName(Name, DelphiFunctionProc_Safecall, ProcPtr, Slf);
    cdCdecl:
      RegisterFunctionName(Name, DelphiFunctionProc_CDECL, ProcPtr, Slf);
  end;
end;

{ EPSException }

//==============================================================================
//
// EPSException.Create
//
//==============================================================================
constructor EPSException.Create(const Error: TbtString; Exec: TPSExec;
  Procno, ProcPos: Cardinal);
begin
 inherited Create(string(Error));
 FExec := Exec;
 FProcNo := Procno;
 FProcPos := ProcPos;
end;

{ TPSRuntimeAttribute }

//==============================================================================
//
// TPSRuntimeAttribute.AddValue
//
//==============================================================================
function TPSRuntimeAttribute.AddValue(aType: TPSTypeRec): PPSVariant;
begin
  Result := FValues.PushType(aType);
end;

//==============================================================================
//
// TPSRuntimeAttribute.AdjustSize
//
//==============================================================================
procedure TPSRuntimeAttribute.AdjustSize;
begin
  FValues.Capacity := FValues.Length;
end;

//==============================================================================
//
// TPSRuntimeAttribute.Create
//
//==============================================================================
constructor TPSRuntimeAttribute.Create(Owner: TPSRuntimeAttributes);
begin
  inherited Create;
  FOwner := Owner;
  FValues := TPSStack.Create;
end;

//==============================================================================
//
// TPSRuntimeAttribute.DeleteValue
//
//==============================================================================
procedure TPSRuntimeAttribute.DeleteValue(i: Longint);
begin
  if Cardinal(i) <> Cardinal(FValues.Count - 1) then
    raise Exception.Create(RPS_CanOnlySendLastItem);
  FValues.Pop;
end;

//==============================================================================
//
// TPSRuntimeAttribute.Destroy
//
//==============================================================================
destructor TPSRuntimeAttribute.Destroy;
begin
  FValues.Free;
  inherited Destroy;
end;

//==============================================================================
//
// TPSRuntimeAttribute.GetValue
//
//==============================================================================
function TPSRuntimeAttribute.GetValue(I: Longint): PIFVariant;
begin
  Result := FValues[i];
end;

//==============================================================================
//
// TPSRuntimeAttribute.GetValueCount
//
//==============================================================================
function TPSRuntimeAttribute.GetValueCount: Longint;
begin
  Result := FValues.Count;
end;

{ TPSRuntimeAttributes }

//==============================================================================
//
// TPSRuntimeAttributes.Add
//
//==============================================================================
function TPSRuntimeAttributes.Add: TPSRuntimeAttribute;
begin
  Result := TPSRuntimeAttribute.Create(Self);
  FAttributes.Add(Result);
end;

//==============================================================================
//
// TPSRuntimeAttributes.Create
//
//==============================================================================
constructor TPSRuntimeAttributes.Create(AOwner: TPSExec);
begin
  inherited Create;
  FAttributes := TPSList.Create;
  FOwner := AOwner;
end;

//==============================================================================
//
// TPSRuntimeAttributes.Delete
//
//==============================================================================
procedure TPSRuntimeAttributes.Delete(I: Longint);
begin
  TPSRuntimeAttribute(FAttributes[i]).Free;
  FAttributes.Delete(i);
end;

//==============================================================================
//
// TPSRuntimeAttributes.Destroy
//
//==============================================================================
destructor TPSRuntimeAttributes.Destroy;
var
  i: Longint;
begin
  for i := FAttributes.Count - 1 downto 0 do
    TPSRuntimeAttribute(FAttributes[i]).Free;
  FAttributes.Free;
  inherited Destroy;
end;

function TPSRuntimeAttributes.FindAttribute(
  const Name: TbtString): TPSRuntimeAttribute;
var
  n: TbtString;
  i, h: Longint;
begin
  n := FastUpperCase(Name);
  h := MakeHash(n);
  for i := 0 to FAttributes.Count - 1 do
  begin
    Result := FAttributes[i];
    if (Result.AttribTypeHash = h) and (Result.AttribType = n) then
      Exit;
  end;
  Result := nil;
end;

//==============================================================================
//
// TPSRuntimeAttributes.GetCount
//
//==============================================================================
function TPSRuntimeAttributes.GetCount: Longint;
begin
   Result := FAttributes.Count;
end;

//==============================================================================
//
// TPSRuntimeAttributes.GetItem
//
//==============================================================================
function TPSRuntimeAttributes.GetItem(I: Longint): TPSRuntimeAttribute;
begin
  Result := FAttributes[i];
end;

{ TPSInternalProcRec }

//==============================================================================
//
// TPSInternalProcRec.Destroy
//
//==============================================================================
destructor TPSInternalProcRec.Destroy;
begin
  if FData <> nil then
    Freemem(Fdata, FLength);
  inherited Destroy;
end;

{ TPsProcRec }

//==============================================================================
//
// TPSProcRec.Create
//
//==============================================================================
constructor TPSProcRec.Create(Owner: TPSExec);
begin
  inherited Create;
  FAttributes := TPSRuntimeAttributes.Create(Owner);
end;

//==============================================================================
//
// TPSProcRec.Destroy
//
//==============================================================================
destructor TPSProcRec.Destroy;
begin
  FAttributes.Free;
  inherited Destroy;
end;

{ TPSTypeRec_Array }

//==============================================================================
//
// TPSTypeRec_Array.CalcSize
//
//==============================================================================
procedure TPSTypeRec_Array.CalcSize;
begin
  FrealSize := PointerSize;
end;

{ TPSTypeRec_StaticArray }

//==============================================================================
//
// TPSTypeRec_StaticArray.CalcSize
//
//==============================================================================
procedure TPSTypeRec_StaticArray.CalcSize;
begin
  FrealSize := Cardinal(FArrayType.RealSize) * Cardinal(Size);
end;

{ TPSTypeRec_Set }

//==============================================================================
//
// TPSTypeRec_Set.CalcSize
//
//==============================================================================
procedure TPSTypeRec_Set.CalcSize;
begin
  FrealSize := FByteSize;
end;

const
  MemDelta = 4096;

{ TPSStack }

//==============================================================================
//
// TPSStack.AdjustLength
//
//==============================================================================
procedure TPSStack.AdjustLength;
var
  MyLen: Longint;
begin
  MyLen := ((FLength shr 12) + 1) shl 12;
  if fCapacity < MyLen then
    SetCapacity(((MyLen + MemDelta) div MemDelta) * MemDelta);
end;

//==============================================================================
//
// TPSStack.Clear
//
//==============================================================================
procedure TPSStack.Clear;
var
  v: Pointer;
  i: Longint;
begin
  for i := Count - 1 downto 0 do
  begin
    v := Data[i];
    if TPSTypeRec(v^).BaseType in NeedFinalization then
      FinalizeVariant(Pointer(IPointer(v) + PointerSize), TPSTypeRec(v^));
  end;
  inherited Clear;
  FLength := 0;
  SetCapacity(0);
end;

//==============================================================================
//
// TPSStack.Create
//
//==============================================================================
constructor TPSStack.Create;
begin
  inherited Create;
  GetMem(FDataPtr, MemDelta);
  FCapacity := MemDelta;
  FLength := 0;
end;

//==============================================================================
//
// TPSStack.Destroy
//
//==============================================================================
destructor TPSStack.Destroy;
var
  v: Pointer;
  i: Longint;
begin
  for i := Count - 1 downto 0 do
  begin
    v := Data[i];
    if TPSTypeRec(v^).BaseType in NeedFinalization then
    FinalizeVariant(Pointer(IPointer(v) + PointerSize), Pointer(v^));
  end;
  FreeMem(FDataPtr, FCapacity);
  inherited Destroy;
end;

//==============================================================================
//
// TPSStack.GetBool
//
//==============================================================================
function TPSStack.GetBool(ItemNo: Longint): Boolean;
var
  val: PPSVariant;
begin
  if ItemNo < 0 then
    val := Items[Longint(ItemNo) + Longint(Count)]
  else
    val := Items[ItemNo];
  Result := PSGetUInt(@PPSVariantData(val).Data, val.FType) <> 0;
end;

//==============================================================================
//
// TPSStack.GetClass
//
//==============================================================================
function TPSStack.GetClass(ItemNo: Longint): TObject;
var
  val: PPSVariant;
begin
  if ItemNo < 0 then
    val := Items[Longint(ItemNo) + Longint(Count)]
  else
    val := Items[ItemNo];
  Result := PSGetObject(@PPSVariantData(val).Data, val.FType);
end;

//==============================================================================
//
// TPSStack.GetCurrency
//
//==============================================================================
function TPSStack.GetCurrency(ItemNo: Longint): Currency;
var
  val: PPSVariant;
begin
  if ItemNo < 0 then
    val := Items[Longint(ItemNo) + Longint(Count)]
  else
    val := Items[ItemNo];
  Result := PSGetCurrency(@PPSVariantData(val).Data, val.FType);
end;

//==============================================================================
//
// TPSStack.GetInt
//
//==============================================================================
function TPSStack.GetInt(ItemNo: Longint): Longint;
var
  val: PPSVariant;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  Result := PSGetInt(@PPSVariantData(val).Data, val.FType);
end;

{$IFNDEF PS_NOINT64}

//==============================================================================
//
// TPSStack.GetInt64
//
//==============================================================================
function TPSStack.GetInt64(ItemNo: Longint): Int64;
var
  val: PPSVariant;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  Result := PSGetInt64(@PPSVariantData(val).Data, val.FType);
end;
{$ENDIF}

//==============================================================================
//
// TPSStack.GetItem
//
//==============================================================================
function TPSStack.GetItem(I: Longint): PPSVariant;
begin
  if Cardinal(I) >= Cardinal(Count) then
    Result := nil
  else
    Result := Data[i];
end;

//==============================================================================
//
// TPSStack.GetReal
//
//==============================================================================
function TPSStack.GetReal(ItemNo: Longint): Extended;
var
  val: PPSVariant;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  Result := PSGetreal(@PPSVariantData(val).Data, val.FType);
end;

//==============================================================================
//
// TPSStack.GetAnsiString
//
//==============================================================================
function TPSStack.GetAnsiString(ItemNo: Longint): TbtString;
var
  val: PPSVariant;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  Result := PSGetAnsiString(@PPSVariantData(val).Data, val.FType);
end;

//==============================================================================
//
// TPSStack.GetString
//
//==============================================================================
function TPSStack.GetString(ItemNo: Longint): string; // calls the native method
begin
  Result := {$IFNDEF PS_NOWIDESTRING}{$IFDEF DELPHI2009UP}GetUnicodeString(ItemNo){$ELSE}GetAnsiString(ItemNo){$ENDIF}{$ELSE}GetAnsiString(ItemNo){$ENDIF};
end;

//==============================================================================
//
// TPSStack.GetUInt
//
//==============================================================================
function TPSStack.GetUInt(ItemNo: Longint): Cardinal;
var
  val: PPSVariant;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  Result := PSGetUInt(@PPSVariantData(val).Data, val.FType);
end;

{$IFNDEF PS_NOWIDESTRING}

//==============================================================================
//
// TPSStack.GetUnicodeString
//
//==============================================================================
function TPSStack.GetUnicodeString(ItemNo: Integer): TbtUnicodeString;
var
  val: PPSVariant;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  Result := PSGetUnicodeString(@PPSVariantData(val).Data, val.FType);
end;

//==============================================================================
//
// TPSStack.GetWideString
//
//==============================================================================
function TPSStack.GetWideString(ItemNo: Longint): TbtWideString;
var
  val: PPSVariant;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  Result := PSGetWideString(@PPSVariantData(val).Data, val.FType);
end;
{$ENDIF}

//==============================================================================
//
// TPSStack.Pop
//
//==============================================================================
procedure TPSStack.Pop;
var
  p1: Pointer;
  c: Longint;
begin
  c := Count - 1;
  p1 := Data[c];
  DeleteLast;
  FLength := IPointer(p1) - IPointer(FDataPtr);
  if TPSTypeRec(p1^).BaseType in NeedFinalization then
    FinalizeVariant(Pointer(IPointer(p1) + PointerSize), Pointer(p1^));
  if ((FCapacity - FLength) shr 12) > 2 then
    AdjustLength;
end;

//==============================================================================
//
// TPSStack.Push
//
//==============================================================================
function TPSStack.Push(TotalSize: Longint): PPSVariant;
var
  o: Cardinal;
  p: Pointer;
begin
  o := FLength;
  FLength := (FLength + TotalSize);
  //if FLength mod PointerSize <> 0 then
  if FLength mod Longint(PointerSize) <> 0 then
    //FLength := FLength + (PointerSize - (FLength mod PointerSize));
    FLength := FLength + (Longint(PointerSize) - Longint((FLength mod Longint(PointerSize))));
  if FLength > FCapacity then
    AdjustLength;
  p := Pointer(IPointer(FDataPtr) + IPointer(o));
  Add(p);
  Result := P;
end;

//==============================================================================
//
// TPSStack.PushType
//
//==============================================================================
function TPSStack.PushType(aType: TPSTypeRec): PPSVariant;
begin
  Result := Push(aType.RealSize + SizeOf(Pointer));
  Result.FType := aType;
  InitializeVariant(Pointer(IPointer(Result) + PointerSize), aType);
end;

//==============================================================================
//
// TPSStack.SetBool
//
//==============================================================================
procedure TPSStack.SetBool(ItemNo: Longint; const Data: Boolean);
var
  val: PPSVariant;
  OK: Boolean;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  OK := True;
  if Data then
    PSSetUInt(@PPSVariantData(val).Data, val.FType, OK, 1)
  else
    PSSetUInt(@PPSVariantData(val).Data, val.FType, OK, 0);
  if not OK then
    raise Exception.Create(RPS_TypeMismatch);
end;

//==============================================================================
//
// TPSStack.SetCapacity
//
//==============================================================================
procedure TPSStack.SetCapacity(const Value: Longint);
var
  p: Pointer;
  OOFS: IPointer;
  I: Longint;
begin
  if Value < FLength then
    raise Exception.Create(RPS_CapacityLength);
  if Value = 0 then
  begin
    if FDataPtr <> nil then
    begin
      FreeMem(FDataPtr, FCapacity);
      FDataPtr := nil;
    end;
    FCapacity := 0;
  end;
  GetMem(p, Value);
  if FDataPtr <> nil then
  begin
    if FLength > FCapacity then
      OOFS := FCapacity
    else
      OOFS := FLength;
    Move(FDataPtr^, p^, OOFS);
    OOFS := IPointer(P) - IPointer(FDataPtr);

    for i := Count - 1 downto 0 do
    begin
      Data[i] := Pointer(IPointer(Data[i]) + OOFS);
      if Items[i].FType.FBaseType = btPointer then
      begin // check if pointer points to moved stack data
        if (IPointer(PPSVariantPointer(Data[i]).DataDest) >= IPointer(FDataPtr)) and
           (IPointer(PPSVariantPointer(Data[i]).DataDest) <  IPointer(FDataPtr) + IPointer(FLength)) then
          PPSVariantPointer(Data[i]).DataDest := Pointer(IPointer(PPSVariantPointer(Data[i]).DataDest) + OOFS);
      end;
    end;

    FreeMem(FDataPtr, FCapacity);
  end;
  FDataPtr := p;
  FCapacity := Value;
end;

//==============================================================================
//
// TPSStack.SetClass
//
//==============================================================================
procedure TPSStack.SetClass(ItemNo: Longint; const Data: TObject);
var
  val: PPSVariant;
  OK: Boolean;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  OK := True;
  PSSetObject(@PPSVariantData(val).Data, val.FType, OK, Data);
  if not OK then
    raise Exception.Create(RPS_TypeMismatch);
end;

//==============================================================================
//
// TPSStack.SetCurrency
//
//==============================================================================
procedure TPSStack.SetCurrency(ItemNo: Longint; const Data: Currency);
var
  val: PPSVariant;
  OK: Boolean;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  OK := True;
  PSSetCurrency(@PPSVariantData(val).Data, val.FType, OK, Data);
  if not OK then
    raise Exception.Create(RPS_TypeMismatch);
end;

//==============================================================================
//
// TPSStack.SetInt
//
//==============================================================================
procedure TPSStack.SetInt(ItemNo: Longint; const Data: Longint);
var
  val: PPSVariant;
  OK: Boolean;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  OK := True;
  PSSetInt(@PPSVariantData(val).Data, val.FType, OK, Data);
  if not OK then
    raise Exception.Create(RPS_TypeMismatch);
end;

{$IFNDEF PS_NOINT64}

//==============================================================================
//
// TPSStack.SetInt64
//
//==============================================================================
procedure TPSStack.SetInt64(ItemNo: Longint; const Data: Int64);
var
  val: PPSVariant;
  OK: Boolean;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  OK := True;
  PSSetInt64(@PPSVariantData(val).Data, val.FType, OK, Data);
  if not OK then
    raise Exception.Create(RPS_TypeMismatch);
end;
{$ENDIF}

//==============================================================================
//
// TPSStack.SetReal
//
//==============================================================================
procedure TPSStack.SetReal(ItemNo: Longint; const Data: Extended);
var
  val: PPSVariant;
  OK: Boolean;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  OK := True;
  PSSetReal(@PPSVariantData(val).Data, val.FType, OK, Data);
  if not OK then
    raise Exception.Create(RPS_TypeMismatch);
end;

//==============================================================================
//
// TPSStack.SetAnsiString
//
//==============================================================================
procedure TPSStack.SetAnsiString(ItemNo: Longint; const Data: TbtString);
var
  val: PPSVariant;
  OK: Boolean;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  OK := True;
  PSSetAnsiString(@PPSVariantData(val).Data, val.FType, OK, Data);
  if not OK then
    raise Exception.Create(RPS_TypeMismatch);
end;

//==============================================================================
//
// TPSStack.SetString
//
//==============================================================================
procedure TPSStack.SetString(ItemNo: Longint; const Data: string);
begin
  {$IFNDEF PS_NOWIDESTRING}
    {$IFDEF DELPHI2009UP}
    SetUnicodeString(ItemNo, Data);
    {$ELSE}
    SetAnsiString(ItemNo, Data);
    {$ENDIF}
  {$ELSE}
  SetAnsiString(ItemNo, Data);
  {$ENDIF}
end;

//==============================================================================
//
// TPSStack.SetUInt
//
//==============================================================================
procedure TPSStack.SetUInt(ItemNo: Longint; const Data: Cardinal);
var
  val: PPSVariant;
  OK: Boolean;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  OK := True;
  PSSetUInt(@PPSVariantData(val).Data, val.FType, OK, Data);
  if not OK then
    raise Exception.Create(RPS_TypeMismatch);
end;

{$IFNDEF PS_NOWIDESTRING}

//==============================================================================
//
// TPSStack.SetUnicodeString
//
//==============================================================================
procedure TPSStack.SetUnicodeString(ItemNo: Integer;
  const Data: TbtUnicodeString);
var
  val: PPSVariant;
  OK: Boolean;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  OK := True;
  PSSetUnicodeString(@PPSVariantData(val).Data, val.FType, OK, Data);
end;

//==============================================================================
//
// TPSStack.SetWideString
//
//==============================================================================
procedure TPSStack.SetWideString(ItemNo: Longint;
  const Data: TbtWideString);
var
  val: PPSVariant;
  OK: Boolean;
begin
  if ItemNo < 0 then
    val := items[Longint(ItemNo) + Longint(Count)]
  else
    val := items[ItemNo];
  OK := True;
  PSSetWideString(@PPSVariantData(val).Data, val.FType, OK, Data);
  if not OK then
    raise Exception.Create(RPS_TypeMismatch);
end;
{$ENDIF}

{$IFNDEF PS_NOIDISPATCH}
var
  DispPropertyPut: Integer = DISPID_PROPERTYPUT;

const
  LOCALE_SYSTEM_DEFAULT = 2 shl 10; // Delphi 2 doesn't define this

//==============================================================================
//
// IDispatchInvoke
//
//==============================================================================
function IDispatchInvoke(Self: IDispatch; PropertySet: Boolean;
  const Name: TbtString; const Par: array of Variant): Variant;
var
  Param: Word;
  i, ArgErr: Longint;
  DispatchId: Longint;
  DispParam: TDispParams;
  ExceptInfo: TExcepInfo;
  aName: PWideChar;
  WSFreeList: TPSList;
begin
  if Self = nil then
  begin
    raise EPSException.Create('Variant is null, cannot invoke', nil, 0, 0);
  end;
  FillChar(ExceptInfo, SizeOf(ExceptInfo), 0);
  if Name = '' then
  begin
   DispatchId := 0;
  end
  else
  begin
    aName := StringToOleStr(Name);
    try
      if Self = nil then
        raise Exception.Create(RPS_NILInterfaceException);
      if Self.GetIDsOfNames(GUID_NULL, @aName, 1, LOCALE_SYSTEM_DEFAULT, @DispatchId) <> S_OK then
        raise Exception.Create(RPS_UnknownMethod);
    finally
      SysFreeString(aName);
    end;
  end;
  DispParam.cNamedArgs := 0;
  DispParam.rgdispidNamedArgs := nil;
  DispParam.cArgs := (High(Par) + 1);

  if PropertySet then
  begin
    Param := DISPATCH_PROPERTYPUT;
    DispParam.cNamedArgs := 1;
    DispParam.rgdispidNamedArgs := @DispPropertyPut;
  end
  else
    Param := DISPATCH_METHOD or DISPATCH_PROPERTYGET;

  WSFreeList := TPSList.Create;
  try
    GetMem(DispParam.rgvarg, SizeOf(TVariantArg) * (High(Par) + 1));
    FillCHar(DispParam.rgvarg^, SizeOf(TVariantArg) * (High(Par) + 1), 0);
    try
      for i := 0 to High(Par)  do
      begin
        if PVarData(@Par[High(Par) - i]).VType = varString then
        begin
          DispParam.rgvarg[i].vt := VT_BSTR;
          DispParam.rgvarg[i].bstrVal := StringToOleStr(AnsiString(Par[High(Par) - i]));
          WSFreeList.Add(DispParam.rgvarg[i].bstrVal);
        {$IFDEF UNICODE}
        end
        else if (PVarData(@Par[High(Par) - i]).VType = varOleStr) or (PVarData(@Par[High(Par) - i]).VType = varUString) then
        begin
          DispParam.rgvarg[i].vt := VT_BSTR;
          DispParam.rgvarg[i].bstrVal := StringToOleStr(UnicodeString(Par[High(Par) - i]));
          WSFreeList.Add(DispParam.rgvarg[i].bstrVal);
        {$ENDIF}
        end
        else
        begin
          DispParam.rgvarg[i].vt := VT_VARIANT or VT_BYREF;
          New(
          {$IFDEF DELPHI4UP}
          POleVariant
          {$ELSE}
          PVariant{$ENDIF}
           (DispParam.rgvarg[i].pvarVal));
          (*
          {$IFDEF DELPHI4UP}
            POleVariant
          {$ELSE}
            PVariant
          {$ENDIF}
           (DispParam.rgvarg[i].pvarVal)^ := Par[High(Par) - i];
          *)
          Move(Par[High(Par) - i],Pointer(DispParam.rgvarg[i].pvarVal)^,
           SizeOf({$IFDEF DELPHI4UP}OleVariant{$ELSE}Variant{$ENDIF}));

        end;
      end;
      i := Self.Invoke(DispatchId, GUID_NULL, LOCALE_SYSTEM_DEFAULT, Param, DispParam, @Result, @ExceptInfo, @ArgErr);
      {$IFNDEF Delphi3UP}
      try
       if not Succeeded(i) then
       begin
         if i = DISP_E_EXCEPTION then
           raise Exception.Create(OleStrToString(ExceptInfo.bstrSource) + ': ' + OleStrToString(ExceptInfo.bstrDescription))
         else
           raise Exception.Create(SysErrorMessage(i));
       end;
      finally
        SysFreeString(ExceptInfo.bstrSource);
        SysFreeString(ExceptInfo.bstrDescription);
        SysFreeString(ExceptInfo.bstrHelpFile);
      end;
      {$ELSE}
       if not Succeeded(i) then
       begin
         if i = DISP_E_EXCEPTION then
           {$IFDEF FPC}
           raise Exception.Create(ExceptInfo.Source + ': ' + ExceptInfo.Description)
           {$ELSE}
           raise Exception.Create(ExceptInfo.bstrSource + ': ' + ExceptInfo.bstrDescription)
           {$ENDIF}
         else
           raise Exception.Create(SysErrorMessage(i));
       end;
      {$ENDIF}
    finally
      for i := 0 to High(Par)  do
      begin
        if DispParam.rgvarg[i].vt = (VT_VARIANT or VT_BYREF) then
        begin
          if{$IFDEF DELPHI4UP}POleVariant{$ELSE}PVariant{$ENDIF}
            (DispParam.rgvarg[i].pvarVal) <> nil then
            Dispose(
            {$IFDEF DELPHI4UP}
             POleVariant
            {$ELSE}
             PVariant
            {$ENDIF}
             (DispParam.rgvarg[i].pvarVal));
        end;
      end;
      FreeMem(DispParam.rgvarg, SizeOf(TVariantArg) * (High(Par) + 1));
    end;
  finally
    for i := WSFreeList.Count - 1 downto 0 do
      SysFreeString(WSFreeList[i]);
    WSFreeList.Free;
  end;
end;
{$ENDIF}

{ TPSTypeRec_ProcPtr }

//==============================================================================
//
// TPSTypeRec_ProcPtr.CalcSize
//
//==============================================================================
procedure TPSTypeRec_ProcPtr.CalcSize;
begin
  FRealSize := 2 * SizeOf(Pointer) + SizeOf(Cardinal);
end;

end.

