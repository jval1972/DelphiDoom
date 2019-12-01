unit zBitmap;

{$P+,S-,W-,R-,T-,X+,H+}
{$C PRELOAD}

interface

uses
  Windows, Forms, SysUtils, Classes, Graphics, Math, zLibPas;

type
  TZBitmap = class(TBitmap)
  private
    procedure WriteZStreamData(Stream: TStream);
    procedure ReadZStreamData(Stream: TStream);
  protected
    procedure WriteData(Stream: TStream); override;
    procedure ReadData(Stream: TStream); override;
  public
    CompressionLevel: TCompressionLevel;
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
  end;

implementation

{ TZBitmap }

constructor TZBitmap.Create;
begin
  Inherited;
  CompressionLevel := clDefault;
end;

procedure TZBitmap.WriteData(Stream: TStream);
begin
  WriteZStreamData(Stream);
end;

procedure TZBitmap.SaveToStream(Stream: TStream);
begin
  WriteZStreamData(Stream);
end;

procedure TZBitmap.LoadFromStream(Stream: TStream);
begin
  ReadZStreamData(Stream);
end;

procedure TZBitmap.ReadData(Stream: TStream);
begin
  ReadZStreamData(Stream);
end;

procedure TZBitmap.ReadZStreamData(Stream: TStream);
var
  aBitmap : TBitmap;
  f : TDecompressionStream;
  m : TMemoryStream;
  buf : Array [1..4096] of byte;
  size, numread : longint;
  pos: longint;
  i,j: integer;
  P,P1: PByteArray;
begin
  pos := Stream.Position;
  Stream.Seek(Stream.Size - SizeOf(size), soFromBeginning);
  Stream.Read(size, SizeOf(size));
  Stream.Seek(pos, soFromBeginning);
  m := TMemoryStream.Create;
  f := TDecompressionStream.Create(Stream);
  try
    while f.Position < size do
    begin
      numread := f.Read(buf, Min(SizeOf(buf), size - f.Position ));
      m.Write(buf, numRead);
    end;
    aBitmap := TBitmap.Create;
    m.Seek(0, soFromBeginning);
    aBitmap.LoadFromStream(m);
    Assign(aBitmap);
    aBitmap.Free;
    if PixelFormat = pf8bit then
    begin
      for j := 0 to Height - 1 do
      begin
        P := Scanline[j];
        P1 := aBitmap.Scanline[j];
        for i := 1 to Width - 1 do
          P[i] := P[i-1] + P1[i];
      end
    end;
    if PixelFormat = pf24bit then
    begin
      for j := 0 to Height - 1 do
      begin
        P := Scanline[j];
        P1 := aBitmap.Scanline[j];
        for i := 1 to Width - 1 do
        begin
          P[i*3] := P[i*3-3] + P1[i*3];
          P[i*3+1] := P[i*3-2] + P1[i*3+1];
          P[i*3+2] := P[i*3-1] + P1[i*3+2];
        end;
      end
    end;
  finally
    f.Free;
    m.free;
  end;
end;

procedure TZBitmap.WriteZStreamData(Stream: TStream);
var
  aBitmap: TBitmap;
  f: TCompressionStream;
  size: Longint;
  i,j: integer;
  P,P1: PByteArray;

begin
  aBitmap := TBitmap.Create;
  f := TCompressionStream.Create(CompressionLevel, Stream);
  try
    if not (PixelFormat in [pf1bit, pf4bit, pf8bit, pfCustom]) then
      PixelFormat := pf24bit;
    aBitmap.Assign(self);
    if PixelFormat = pf8bit then
    begin
      for j := 0 to Height - 1 do
      begin
        P := Scanline[j];
        P1 := aBitmap.Scanline[j];
        for i := 1 to Width - 1 do
          P1[i] := P[i] - P[i-1];
      end
    end
    else if PixelFormat = pf24bit then
    begin
      for j := 0 to Height - 1 do
      begin
        P := Scanline[j];
        P1 := aBitmap.Scanline[j];
        for i := 1 to Width - 1 do
        begin
          P1[i*3] := P[i*3] - P[i*3-3];
          P1[i*3+1] := P[i*3+1] - P[i*3-2];
          P1[i*3+2] := P[i*3+2] - P[i*3-1];
        end;
      end
    end;
    aBitmap.SaveToStream(f);
    // At f.Position is the uncompressed file size
    size := f.Position;
  finally
    aBitmap.Free;
    f.Free;
  end;
  Stream.Seek(0, soFromEnd);
  Stream.Write(size, SizeOf(size));
end;

initialization
  { Register the TZBitmap as a new graphic file format
    now all the TPicture storage stuff can access our new
    Zipped graphic format !
  }
  TPicture.RegisterFileFormat('BMZ','zBitmap', TZBitmap);

finalization
  TPicture.UnregisterGraphicClass(TZBitmap);

end.
