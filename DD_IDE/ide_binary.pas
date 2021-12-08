unit ide_binary;

interface

uses SysUtils, Classes, zLibpas, Math;

type
  TZProgressStage = (psStarting, psRunning, psEnding);
  TZProgressOperation = (poLoad, poSave);
  TZProgressEvent = procedure (Sender: TObject; Stage: TZProgressStage;
    PercentDone: Byte) of object;

  TBinary = class(TPersistent)
  private
    FOnProgress: TZProgressEvent;
    FModified: Boolean;
    FMemoryCache: TMemoryStream;
    FCompressedSize: integer;
    FCompressionLevel: TCompressionLevel;
    procedure SetModified(Value: Boolean);
  protected
    FOnChange: TNotifyEvent;
    procedure Changed(Sender: TObject); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    function Equals(Binary: TBinary): Boolean; virtual;
    procedure Progress(Sender: TObject; Stage: TZProgressStage;
      Operation: TZProgressOperation; PercentDone: Byte); dynamic;
    function GetEmpty: Boolean; virtual;
    function GetMemory: Pointer; virtual;
    function GetSize: integer; virtual;
    procedure ReadData(Stream: TStream); virtual;
    procedure WriteData(Stream: TStream); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromFile(const Filename: string; Compressed: boolean = false); virtual;
    procedure SaveToFile(const Filename: string; Compressed: boolean = false); virtual;
    procedure LoadFromStream(Stream: TStream; Compressed: boolean); virtual;
    procedure SaveToStream(Stream: TStream; Compressed: boolean); virtual;
    procedure Clear; virtual;
    property Empty: Boolean read GetEmpty;
    property Memory: Pointer read GetMemory;
    property Data: TMemoryStream read FMemoryCache;
    property Size: integer read GetSize;
    property CompressedSize: integer read FCompressedSize;
    property CompressionLevel: TCompressionLevel read FCompressionLevel write FCompressionLevel;
    property Modified: Boolean read FModified write SetModified;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnProgress: TZProgressEvent read FOnProgress write FOnProgress;
  end;

  TBinaryClass = class of TBinary;

  TCustomBinaryData = class(TObject)
  protected
    FBinary: TBinary;
    FOnChange: TNotifyEvent;
    FOnProgress: TZProgressEvent;
    function GetEmpty: Boolean; virtual;
    function GetMemory: Pointer; virtual;
    function GetSize: integer; virtual;
    function GetData: TMemoryStream; virtual;
    function GetCompressedSize: integer; virtual;
    function GetCompressionLevel: TCompressionLevel; virtual;
    procedure SetCompressionLevel(Value: TCompressionLevel); virtual;
    procedure SetBinary(Value: TBinary); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    function AsText: string;
  { Οι LoadFromFile, SaveToFile, LoadFromStream, SaveToStream όταν η παράμετρος
    Compressed είναι false προυποθέτουν το ότι στο αρχείο ή στο Stream υπάρχει
    ένα TBinary (δηλαδή διαβάζουμε όλα τα δεδομένα του αρχείου ή του Stream).
    Αν είναι true, τότε γράφεται στην αρχή το μέγεθος του συμπιεσμένου Stream
    με αποτέλεσμα να ξέρουμε πόσα data να διαβάσουμε! Με αυτό τον τρόπο μπορούμε
    να διαβάζουμε σειριακά πολλά αντικείμενα TBinary. }
    procedure LoadFromFile(const Filename: string; Compressed: boolean = false); virtual;
    procedure SaveToFile(const Filename: string; Compressed: boolean = false); virtual;
    procedure LoadFromStream(Stream: TStream; Compressed: boolean); virtual;
    procedure SaveToStream(Stream: TStream; Compressed: boolean); virtual;

    property Empty: Boolean read GetEmpty;
    property Memory: Pointer read GetMemory;
    property Data: TMemoryStream read GetData;
    property Size: integer read GetSize;
    property CompressedSize: integer read GetCompressedSize;
    property Binary: TBinary read FBinary write SetBinary;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnProgress: TZProgressEvent read FOnProgress write FOnProgress;
    property CompressionLevel: TCompressionLevel read GetCompressionLevel write SetCompressionLevel;
  end;

  TBinaryData = class(TCustomBinaryData)
  published
    property Binary: TBinary read FBinary write SetBinary;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnProgress: TZProgressEvent read FOnProgress write FOnProgress;
    property CompressionLevel: TCompressionLevel read GetCompressionLevel write SetCompressionLevel;
  end;

implementation

{ TBinary }

constructor TBinary.Create;
begin
  Inherited Create;
  FMemoryCache := TMemoryStream.Create;
  FCompressionLevel := clNone;
end;

destructor TBinary.Destroy;
begin
  FMemoryCache.Free;
  Inherited Destroy;
end;

procedure TBinary.SetModified(Value: Boolean);
begin
  if Value then
    Changed(Self)
  else
    FModified := False;
end;

procedure TBinary.Changed(Sender: TObject);
begin
  FModified := True;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TBinary.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not (Filer.Ancestor is TBinary) or
        not Equals(TBinary(Filer.Ancestor))
    else
      Result := not Empty;
  end;

begin
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, DoWrite);
end;

function TBinary.Equals(Binary: TBinary): Boolean;
var
  MyData, BinaryData: TMemoryStream;
begin
  Result := (Binary <> nil) and (ClassType = Binary.ClassType);
  if Empty or Binary.Empty then
  begin
    Result := Empty and Binary.Empty;
    Exit;
  end;
  if Result then
  begin
    MyData := TMemoryStream.Create;
    try
      WriteData(MyData);
      BinaryData := TMemoryStream.Create;
      try
        Binary.WriteData(BinaryData);
        Result := (MyData.Size = BinaryData.Size) and
          CompareMem(MyData.Memory, BinaryData.Memory, MyData.Size);
      finally
        BinaryData.Free;
      end;
    finally
      MyData.Free;
    end;
  end;
end;

procedure TBinary.Progress(Sender: TObject; Stage: TZProgressStage;
  Operation: TZProgressOperation; PercentDone: Byte);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Stage, PercentDone);
end;

function TBinary.GetEmpty: Boolean;
begin
  GetEmpty := FMemoryCache.Size = 0
end;

function TBinary.GetMemory: Pointer;
begin
  GetMemory := FMemoryCache.Memory;
end;

function TBinary.GetSize: integer;
begin
  GetSize := FMemoryCache.Size;
end;

procedure TBinary.LoadFromFile(const Filename: string; Compressed: boolean);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream, Compressed);
  finally
    Stream.Free;
  end;
end;

procedure TBinary.ReadData(Stream: TStream);
begin
  LoadFromStream(Stream, true);
end;

procedure TBinary.LoadFromStream(Stream: TStream; Compressed: boolean);
const BufSize = $FFFF;
var
  DStream: TDecompressionStream;
  Buf : Array [1..BufSize] of byte;
  Size, NumRead, pos: longint;
begin
  Clear;
  if Compressed then
  begin
    Stream.Read(size, SizeOf(size));
    FCompressedSize := Stream.Position;
    if Size > 0 then
    begin
      Progress(Self, psStarting, poLoad, 0);
      DStream := TDecompressionStream.Create(Stream);
      try
        pos := 0;
        // Κάνουμε ReAlloc μνήμης στο FMemoryCache
        FMemoryCache.Size := Size;
        while pos < Size do
        begin
          NumRead := DStream.Read(Buf, Min(SizeOf(Buf), Size - pos));
          inc(pos, NumRead);
          FMemoryCache.Write(Buf, NumRead);
          Progress(Self, psRunning, poLoad, Round(pos*100/Size));
        end;
      finally
        DStream.Free;
        FCompressedSize := Stream.Position - FCompressedSize;
        Progress(Self, psEnding, poLoad, 0);
      end;
    end;
  end
  else
  begin
    Progress(Self, psStarting, poLoad, 0);
    pos := 0;
    Size := Stream.Size;
    // Κάνουμε ReAlloc μνήμης στο FMemoryCache
    FMemoryCache.Size := Size;
    while pos < Size do
    begin
      NumRead := Stream.Read(Buf, Min(SizeOf(Buf), Size - pos));
      inc(pos, NumRead);
      FMemoryCache.Write(Buf, NumRead);
      Progress(Self, psRunning, poLoad, Round(pos*100/Stream.Size));
    end;
    FCompressedSize := FMemoryCache.Size;
    Progress(Self, psEnding, poLoad, 0);
  end
end;

procedure TBinary.SaveToFile(const Filename: string; Compressed: boolean);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(Stream, Compressed);
  finally
    Stream.Free;
  end;
end;

procedure TBinary.WriteData(Stream: TStream);
begin
  SaveToStream(Stream, true);
end;

procedure TBinary.SaveToStream(Stream: TStream; Compressed: boolean);
var
  CStream: TCompressionStream;
  Size, Pos: longint;
begin
  if Compressed then
  begin
  { Δεσμεύουμε χώρο στην αρχή του Stream.
    Στη συνέχεια θα γράψουμε το μέγεθος (size). }
    Pos := Stream.Position;
    Size := 0;
    Stream.Write(Size, SizeOf(Size));
    Progress(Self, psStarting, poSave, 0);
    CStream := TCompressionStream.Create(CompressionLevel, Stream);
    try
      FMemoryCache.SaveToStream(CStream);
      Size := CStream.Position;
    finally
      CStream.Free;
    end;
    Stream.Seek(Pos, soFromBeginning);
    Stream.Write(Size, SizeOf(Size));
    Stream.Seek(Size, soFromCurrent);
    Progress(Self, psEnding, poSave, 100);
    FCompressedSize := Size;
  end
  else
  begin
    Progress(Self, psStarting, poSave, 0);
    pos := Stream.Position;
    Stream.Size := Max(Stream.Size, pos + FMemoryCache.Size);
    Stream.Position := pos;

    Stream.CopyFrom(FMemoryCache, 0);
    Progress(Self, psEnding, poSave, 100);
  end;
end;

procedure TBinary.Clear;
begin
  FMemoryCache.Clear;
end;

procedure TBinary.Assign(Source: TPersistent);
begin
  if Source = nil then
    Clear
  else if Source is TBinary then
  begin
    Clear;
    CompressionLevel := (Source as TBinary).CompressionLevel;
    FMemoryCache.CopyFrom((Source as TBinary).FMemoryCache, 0)
  end
  else
    inherited Assign(Source);
end;

{ TCustomBinaryData }

constructor TCustomBinaryData.Create;
begin
  Inherited;
  FBinary := TBinary.Create;
  FBinary.OnChange := OnChange;
  FBinary.OnProgress := OnProgress;
end;

destructor TCustomBinaryData.Destroy;
begin
  FBinary.Free;
  Inherited;
end;

procedure TCustomBinaryData.Clear;
begin
  if Assigned(FBinary) then FBinary.Clear;
end;

function TCustomBinaryData.AsText: string;
var m: TMemoryStream;
begin
  m := TMemoryStream.Create;
  try
    SaveToStream(m, false);
    m.Seek(0, soFromBeginning);
    SetLength(result, m.Size);
    m.Read(result[1], m.Size);
  finally
    m.Free;
  end;
end;

procedure TCustomBinaryData.LoadFromFile(const Filename: string; Compressed: boolean);
begin
  if Assigned(FBinary) then FBinary.LoadFromFile(Filename, Compressed);
end;

procedure TCustomBinaryData.SaveToFile(const Filename: string; Compressed: boolean);
begin
  if Assigned(FBinary) then FBinary.SaveToFile(Filename, Compressed);
end;

procedure TCustomBinaryData.LoadFromStream(Stream: TStream; Compressed: boolean);
begin
  if Assigned(FBinary) then FBinary.LoadFromStream(Stream, Compressed);
end;

procedure TCustomBinaryData.SaveToStream(Stream: TStream; Compressed: boolean);
begin
  if Assigned(FBinary) then FBinary.SaveToStream(Stream, Compressed);
end;

function TCustomBinaryData.GetEmpty: Boolean;
begin
  if Assigned(FBinary) then
    result := FBinary.Empty
  else
    result := true;
end;

function TCustomBinaryData.GetMemory: Pointer;
begin
  if Assigned(FBinary) then
    result := FBinary.Memory
  else
    result := nil;
end;

function TCustomBinaryData.GetSize: integer;
begin
  if Assigned(FBinary) then
    result := FBinary.Size
  else
    result := 0;
end;

function TCustomBinaryData.GetData: TMemoryStream;
begin
  if Assigned(FBinary) then
    result := FBinary.Data
  else
    result := nil;
end;

function TCustomBinaryData.GetCompressedSize: integer;
begin
  if Assigned(FBinary) then
    result := FBinary.CompressedSize
  else
    result := 0;
end;

function TCustomBinaryData.GetCompressionLevel: TCompressionLevel;
begin
  if Assigned(FBinary) then
    result := FBinary.CompressionLevel
  else
    result := clDefault;
end;

procedure TCustomBinaryData.SetCompressionLevel(Value: TCompressionLevel);
begin
  if Assigned(FBinary) then
    FBinary.CompressionLevel := Value
end;

procedure TCustomBinaryData.SetBinary(Value: TBinary);
begin
  FBinary.Assign(Value);
end;

end.

