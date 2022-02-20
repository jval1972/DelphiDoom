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
//  Pascal Script load and save
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}
{$I PascalScript.inc}

unit ps_serializer;

interface

uses
  Classes,
  ps_import,
  ps_runtime,
  ps_utils;

const
  VALID_GLOBAL_VAR_TYPES: set of TPSBaseType = [
    btU8,             // Byte, Boolean, Enums
    btS8,
    btU16,
    btS16,
    btU32,            // LongWord
    btS32,            // Integer
    {$IFNDEF PS_NOINT64}
    btS64,
    {$ENDIF}
    btChar,
    btSingle,         // Single
    btDouble,         // Double
    btExtended,       // Extended
    btCurrency,       // Currency
    btString,         // AnsiString
    btWideString,     // WideString
    btUnicodeString,  // UnicodeString
    btStaticArray,    // Static Array
    btArray,          // Dynamic Array
    btRecord,         // Records
    btSet             // Sets
  ];

type
  TScriptSerializer = class(TObject)
  private
    fExec: TDoomExec;
    function SaveVariable(SaveStream: TStream; Src: Pointer; aType: TPSTypeRec): boolean;
    function LoadVariable(LoadStream: TStream; Src: Pointer; aType: TPSTypeRec): boolean;
  public
    constructor Create(const aExec: TDoomExec);
    function SaveToFile(const fname: string): boolean;
    function AppendToFile(const fname: string): boolean;
    function LoadFromFile(const fname: string): boolean;
    function LoadFromFilePos(const fname: string; var position: integer): boolean;
    function SaveToStream(const SaveStream: TStream): boolean;
    function LoadFromStream(const LoadStream: TStream): boolean;
    function SaveSize: Integer;
  end;

implementation

uses
  SysUtils,
  i_system,
  ps_defs;

//==============================================================================
//
// TScriptSerializer.Create
//
//==============================================================================
constructor TScriptSerializer.Create(const aExec: TDoomExec);
begin
  fExec := aExec;
end;

//==============================================================================
//
// TScriptSerializer.SaveVariable
//
//==============================================================================
function TScriptSerializer.SaveVariable(SaveStream: TStream; Src: Pointer; aType: TPSTypeRec): boolean;
var
  ElemCount: Integer;
  tmpstr: string;
  wtmpstr: WideString;
  wtmpc: WideChar;
  i: Integer;
  size: Integer;
  Offset: Cardinal;
begin
  Result := True;
  //See uPSRuntime line 1630 for algo idea
  case aType.BaseType of
    btU8:
      SaveStream.Write(TbtU8(Src^), SizeOf(TbtU8)); //Byte, Boolean
    btS8:
      SaveStream.Write(TbtS8(Src^), SizeOf(TbtS8)); //short int
    btU16:
      SaveStream.Write(TbtU16(Src^), SizeOf(TbtU16)); // word
    btS16:
      SaveStream.Write(TbtS16(Src^), SizeOf(TbtS16)); // smallint
    btU32:
      SaveStream.Write(TbtU32(Src^), SizeOf(TbtU32)); //Longword
    btS32:
      SaveStream.Write(TbtS32(Src^), SizeOf(TbtS32)); //Integer
    {$IFNDEF PS_NOINT64}
    btS64:
      SaveStream.Write(TbtS64(Src^), SizeOf(TbtS64)); //Int64
    {$ENDIF}
    btChar:
      SaveStream.Write(TbtChar(Src^), SizeOf(TbtChar)); //Char
    btWideChar:
      SaveStream.Write(TbtWideChar(Src^), SizeOf(TbtWideChar)); //WideChar
    btSingle:
      SaveStream.Write(TbtSingle(Src^), SizeOf(TbtSingle));
    btDouble:
      SaveStream.Write(TbtDouble(Src^), SizeOf(TbtDouble));
    btExtended:
      SaveStream.Write(TbtExtended(Src^), SizeOf(TbtExtended));
    btCurrency:
      SaveStream.Write(TbtCurrency(Src^), SizeOf(TbtCurrency));
    btString:
      begin
        tmpstr := TbtString(Src^);
        ElemCount := Length(tmpstr);
        SaveStream.Write(ElemCount, SizeOf(ElemCount));
        for i := 1 to ElemCount do
          SaveStream.Write(tmpstr[i], SizeOf(Char));
      end;
    btWideString:
      begin
        wtmpstr := TbtWideString(Src^);
        ElemCount := Length(wtmpstr);
        SaveStream.Write(ElemCount, SizeOf(ElemCount));
        for i := 1 to ElemCount do
        begin
          wtmpc := wtmpstr[i];
          SaveStream.Write(wtmpc, SizeOf(WideChar));
        end;
      end;
    btUnicodeString:
      begin
        wtmpstr := TbtUnicodeString(Src^);
        ElemCount := Length(wtmpstr);
        SaveStream.Write(ElemCount, SizeOf(ElemCount));
        for i := 1 to ElemCount do
        begin
          wtmpc := wtmpstr[i];
          SaveStream.Write(wtmpc, SizeOf(WideChar));
        end;
      end;
    btStaticArray:
      begin
        ElemCount := TPSTypeRec_StaticArray(aType).Size;
        SaveStream.Write(ElemCount, SizeOf(ElemCount));
        size := TPSTypeRec_Array(aType).ArrayType.RealSize;
        for i := 0 to ElemCount - 1 do
        begin
          Offset := size * i;
          SaveVariable(SaveStream, Pointer(IPointer(Src) + Offset), TPSTypeRec_Array(aType).ArrayType);
        end;
      end;
    btArray:
      begin
        ElemCount := PSDynArrayGetLength(Pointer(Src^), aType);
        SaveStream.Write(ElemCount, SizeOf(ElemCount));
        size := TPSTypeRec_Array(aType).ArrayType.RealSize;
        for i := 0 to ElemCount - 1 do
        begin
          Offset := size * i;
          SaveVariable(SaveStream, Pointer(IPointer(Src^) + Offset), TPSTypeRec_Array(aType).ArrayType);
        end;
      end;
    btRecord:
      begin
        ElemCount := TPSTypeRec_Record(aType).FieldTypes.Count;
        SaveStream.Write(ElemCount, SizeOf(ElemCount));
        for i := 0 to ElemCount - 1 do
        begin
          Offset := Cardinal(TPSTypeRec_Record(aType).RealFieldOffsets[I]);
          SaveVariable(SaveStream, Pointer(IPointer(Src) + Offset), TPSTypeRec_Record(aType).FieldTypes[I]);
        end;
      end;
    btSet:
      begin
        ElemCount := TPSTypeRec_Set(aType).RealSize;
        SaveStream.Write(ElemCount, SizeOf(ElemCount));
        SaveStream.Write(Src^, ElemCount);
      end;
  end;
end;

//==============================================================================
//
// TScriptSerializer.LoadVariable
//
//==============================================================================
function TScriptSerializer.LoadVariable(LoadStream: TStream; Src: Pointer; aType: TPSTypeRec): boolean;
var
  ElemCount: Integer;
  tmpstr: string;
  wtmpstr: WideString;
  wtmpc: WideChar;
  i: Integer;
  size: integer;
  Offset: Cardinal;
begin
  Result := True;
  case aType.BaseType of
    btU8:
      LoadStream.Read(TbtU8(Src^), SizeOf(TbtU8)); //Byte, Boolean
    btS8:
      LoadStream.Read(TbtS8(Src^), SizeOf(TbtS8)); //shortint
    btU16:
      LoadStream.Read(TbtU16(Src^), SizeOf(TbtU16)); //Longword
    btS16:
      LoadStream.Read(TbtS16(Src^), SizeOf(TbtS16)); //Integer
    btU32:
      LoadStream.Read(TbtU32(Src^), SizeOf(TbtU32)); //Longword
    btS32:
      LoadStream.Read(TbtS32(Src^), SizeOf(TbtS32)); //Integer
    {$IFNDEF PS_NOINT64}
    btS64:
      LoadStream.Read(TbtS64(Src^), SizeOf(TbtS64)); //Int64
    {$ENDIF}
    btChar:
      LoadStream.Read(TbtChar(Src^), SizeOf(TbtChar)); //Char
    btWideChar:
      LoadStream.Read(TbtWideChar(Src^), SizeOf(TbtWideChar)); //WideChar
    btSingle:
      LoadStream.Read(TbtSingle(Src^), SizeOf(TbtSingle)); // Single
    btDouble:
      LoadStream.Read(TbtDouble(Src^), SizeOf(TbtDouble)); // Double
    btExtended:
      LoadStream.Read(TbtExtended(Src^), SizeOf(TbtExtended)); // Extended
    btCurrency:
      LoadStream.Read(TbtCurrency(Src^), SizeOf(TbtCurrency)); // Currency
    btString:
      begin
        LoadStream.Read(ElemCount, SizeOf(ElemCount));
        SetLength(tmpstr, ElemCount);
        for i := 1 to ElemCount do
          LoadStream.Read(tmpstr[i], SizeOf(Char));
        TbtString(Src^) := tmpstr;
      end;
    btWideString:
      begin
        LoadStream.Read(ElemCount, SizeOf(ElemCount));
        SetLength(wtmpstr, ElemCount);
        for i := 1 to ElemCount do
        begin
          LoadStream.Read(wtmpc, SizeOf(WideChar));
          wtmpstr[i] := wtmpc;
        end;
        TbtWideString(Src^) := wtmpstr;
      end;
    btUnicodeString:
      begin
        LoadStream.Read(ElemCount, SizeOf(ElemCount));
        SetLength(wtmpstr, ElemCount);
        for i := 1 to ElemCount do
        begin
          LoadStream.Read(wtmpc, SizeOf(WideChar));
          wtmpstr[i] := wtmpc;
        end;
        TbtUnicodeString(Src^) := wtmpstr;
      end;
    btStaticArray:
      begin
        LoadStream.Read(ElemCount, SizeOf(ElemCount));
        if ElemCount <> TPSTypeRec_StaticArray(aType).Size then
        begin
          I_Warning('TScriptSerializer.LoadVariable(): Array "%s" element count does not match saved count'#13#10,
            [TPSTypeRec_StaticArray(aType).ExportName]);
          Result := False;
          Exit;
        end;
        size := TPSTypeRec_Array(aType).ArrayType.RealSize;
        for i := 0 to ElemCount - 1 do
        begin
          Offset := size * i;
          if not LoadVariable(LoadStream, Pointer(IPointer(Src) + Offset), TPSTypeRec_Array(aType).ArrayType) then
          begin
            Result := False;
            Exit;
          end;
        end;
      end;
    btArray:
      begin
        LoadStream.Read(ElemCount, SizeOf(ElemCount));
        PSDynArraySetLength(Pointer(Src^), aType, ElemCount);
        size := TPSTypeRec_Array(aType).ArrayType.RealSize;
        for i := 0 to ElemCount - 1 do
        begin
          Offset := size * i;
          if not LoadVariable(LoadStream, Pointer(IPointer(Src^) + Offset), TPSTypeRec_Array(aType).ArrayType) then
          begin
            Result := False;
            Exit;
          end;
        end;
      end;
    btRecord:
      begin
        LoadStream.Read(ElemCount, SizeOf(ElemCount));
        size := TPSTypeRec_Record(aType).FieldTypes.Count;
        if ElemCount <> size then
        begin
          I_Warning('TScriptSerializer.LoadVariable(): Record "%s" element count does not match saved count'#13#10,
            [TPSTypeRec_Record(aType).ExportName]);
          Result := False;
          Exit;
        end;
        for i := 0 to ElemCount - 1 do
        begin
          Offset := Cardinal(TPSTypeRec_Record(aType).RealFieldOffsets[i]);
          if not LoadVariable(LoadStream, Pointer(IPointer(Src) + Offset), TPSTypeRec_Record(aType).FieldTypes[i]) then
          begin
            Result := False;
            Exit;
          end;
        end;
      end;
    btSet:
      begin
        LoadStream.Read(ElemCount, SizeOf(ElemCount));
        size := TPSTypeRec_Set(aType).RealSize;
        if ElemCount <> size then
        begin
          I_Warning('TScriptSerializer.LoadVariable(): Set "%s" element count does not match saved count'#13#10,
            [TPSTypeRec_Set(aType).ExportName]);
          Result := False;
          Exit;
        end;
        LoadStream.Read(Src^, ElemCount);
      end;
  end;
end;

//==============================================================================
//
// TScriptSerializer.SaveToFile
//
//==============================================================================
function TScriptSerializer.SaveToFile(const fname: string): boolean;
var
  fs: TFileStream;
begin
  Result := True;
  try
    fs := TFileStream.Create(fname, fmCreate);
    if not SaveToStream(fs) then
      Result := False;
    fs.Free;
  except
    Result := False;
  end;
end;

//==============================================================================
//
// TScriptSerializer.AppendToFile
//
//==============================================================================
function TScriptSerializer.AppendToFile(const fname: string): boolean;
var
  fs: TFileStream;
begin
  Result := True;
  try
    fs := TFileStream.Create(fname, fmOpenReadWrite);
    fs.Position := fs.Size;
    if not SaveToStream(fs) then
      Result := False;
    fs.Free;
  except
    Result := False;
  end;
end;

//==============================================================================
//
// TScriptSerializer.LoadFromFile
//
//==============================================================================
function TScriptSerializer.LoadFromFile(const fname: string): boolean;
var
  fs: TFileStream;
begin
  Result := True;
  try
    fs := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);
    try
      if not LoadFromStream(fs) then
        Result := False;
    finally
      fs.Free;
    end;
  except
    Result := False;
  end;
end;

//==============================================================================
//
// TScriptSerializer.LoadFromFilePos
//
//==============================================================================
function TScriptSerializer.LoadFromFilePos(const fname: string; var position: integer): boolean;
var
  fs: TFileStream;
begin
  Result := True;
  try
    fs := TFileStream.Create(fname, fmOpenRead or fmShareDenyWrite);
    try
      fs.Position := position;
      if not LoadFromStream(fs) then
        Result := False;
      position := fs.Position;
    finally
      fs.Free;
    end;
  except
    Result := False;
  end;
end;

//==============================================================================
//
// TScriptSerializer.SaveToStream
//
//==============================================================================
function TScriptSerializer.SaveToStream(const SaveStream: TStream): boolean;
var
  ElemCount: Integer;
  i: Integer;
  V: PIFVariant;
  pos1, pos2: integer;
  size: integer;
begin
  pos1 := SaveStream.Position;
  size := 0;
  SaveStream.Write(size, SizeOf(size));

  Result := True;
  try
    ElemCount := fExec.GetVarCount;
    SaveStream.Write(ElemCount, SizeOf(ElemCount));
    for i := 0 to ElemCount - 1 do
    begin
      V := fExec.GetVarNo(i);
      if not SaveVariable(SaveStream, @PPSVariantData(V).Data, V.FType) then
      begin
        Result := False;
        Exit;
      end;
    end;
    pos2 := SaveStream.Position;
    SaveStream.Position := pos1;
    size := pos2 - pos1 - SizeOf(size);
    SaveStream.Write(size, SizeOf(size));
    SaveStream.Position := pos2;
  except
    Result := False;
  end;
end;

//==============================================================================
//
// TScriptSerializer.LoadFromStream
//
//==============================================================================
function TScriptSerializer.LoadFromStream(const LoadStream: TStream): boolean;
var
  ElemCount: Integer;
  i: Integer;
  V: PIFVariant;
  pos1, pos2: integer;
  size: integer;
begin
  pos1 := LoadStream.Position;
  LoadStream.Read(size, SizeOf(size));
  LoadStream.Read(ElemCount, SizeOf(ElemCount));
  if ElemCount <> fExec.GetVarCount then
  begin
    I_Warning('TScriptSerializer.LoadFromStream(): Variable count (%d) does not match saved variables count (%d)'#13#10,
      [fExec.GetVarCount, ElemCount]);
    Result := False;
    Exit;
  end;

  Result := True;
  try
    for I := 0 to fExec.GetVarCount - 1 do
    begin
      V := fExec.GetVarNo(I);
      if not LoadVariable(LoadStream, @PPSVariantData(V).Data, V.FType) then
      begin
        Result := False;
        Exit;
      end;
    end;
    pos2 := LoadStream.Position;
    if size <> pos2 - pos1 - SizeOf(size) then
    begin
      I_Warning('TScriptSerializer.LoadFromStream(): Invalid stream size'#13#10);
      Result := False;
      Exit;
    end;
  except
    Result := False;
  end;
end;

//==============================================================================
//
// TScriptSerializer.SaveSize
//
//==============================================================================
function TScriptSerializer.SaveSize: Integer;
var
  ms: TMemoryStream;
begin
  Result := 0;
  ms := TMemoryStream.Create;
  try
    if SaveToStream(ms) then
      Result := ms.Size;
  finally
    ms.Free;
  end;
end;

end.

