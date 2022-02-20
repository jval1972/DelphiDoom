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
//  WAD Writer
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

unit w_wadwriter;

interface

uses
  d_delphi;

type
  TWadWriter = class(TObject)
  private
    lumps: TDStringList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure AddData(const lumpname: string; const data: pointer; const size: integer);
    procedure AddString(const lumpname: string; const data: string);
    procedure AddSeparator(const lumpname: string);
    procedure SaveToStream(const strm: TDStream);
    procedure SaveToFile(const fname: string);
  end;

//==============================================================================
//
// AddDataToWAD
//
//==============================================================================
function AddDataToWAD(const wad: TWADWriter; const lumpname, data: string): boolean;

implementation

uses
  w_wad;

//==============================================================================
//
// TWadWriter.Create
//
//==============================================================================
constructor TWadWriter.Create;
begin
  lumps := TDStringList.Create;
  Inherited;
end;

//==============================================================================
//
// TWadWriter.Destroy
//
//==============================================================================
destructor TWadWriter.Destroy;
var
  i: integer;
begin
  for i := 0 to lumps.Count - 1 do
    if lumps.Objects[i] <> nil then
      lumps.Objects[i].Free;
  lumps.Free;
  Inherited;
end;

//==============================================================================
//
// TWadWriter.Clear
//
//==============================================================================
procedure TWadWriter.Clear;
var
  i: integer;
begin
  for i := 0 to lumps.Count - 1 do
    if lumps.Objects[i] <> nil then
      lumps.Objects[i].Free;
  lumps.Clear;
end;

//==============================================================================
//
// TWadWriter.AddData
//
//==============================================================================
procedure TWadWriter.AddData(const lumpname: string; const data: pointer; const size: integer);
var
  m: TDMemoryStream;
begin
  m := TDMemoryStream.Create;
  m.Write(data^, size);
  lumps.AddObject(strupper(lumpname), m);
end;

//==============================================================================
//
// TWadWriter.AddString
//
//==============================================================================
procedure TWadWriter.AddString(const lumpname: string; const data: string);
var
  m: TDMemoryStream;
  i: integer;
begin
  m := TDMemoryStream.Create;
  for i := 1 to Length(data) do
    m.Write(data[i], SizeOf(char));
  lumps.AddObject(strupper(lumpname), m);
end;

//==============================================================================
//
// TWadWriter.AddSeparator
//
//==============================================================================
procedure TWadWriter.AddSeparator(const lumpname: string);
begin
  lumps.Add(strupper(lumpname));
end;

//==============================================================================
//
// TWadWriter.SaveToStream
//
//==============================================================================
procedure TWadWriter.SaveToStream(const strm: TDStream);
var
  h: wadinfo_t;
  la: Pfilelump_tArray;
  i: integer;
  p, ssize: integer;
  m: TDMemoryStream;
begin
  p := strm.Position;
  h.identification := PWAD;
  h.numlumps := lumps.Count;
  h.infotableofs := p + SizeOf(wadinfo_t);
  strm.Write(h, SizeOf(h));
  p := strm.Position;
  GetMem(la, lumps.Count * SizeOf(filelump_t));
  strm.Write(la^, lumps.Count * SizeOf(filelump_t));

  for i := 0 to lumps.Count - 1 do
  begin
    la[i].filepos := strm.Position;
    m := lumps.Objects[i] as TDMemoryStream;
    if m <> nil then
    begin
      la[i].size := m.Size;
      m.Seek(0, sFromBeginning);
      strm.Write(m.Memory^, m.Size);
    end
    else
      la[i].size := 0;
    la[i].name := stringtochar8(lumps.Strings[i]);
  end;
  ssize := strm.Position;
  strm.Seek(p, sFromBeginning);
  strm.Write(la^, lumps.Count * SizeOf(filelump_t));
  FreeMem(la, lumps.Count * SizeOf(filelump_t));
  strm.Seek(ssize, sFromBeginning);
end;

//==============================================================================
//
// TWadWriter.SaveToFile
//
//==============================================================================
procedure TWadWriter.SaveToFile(const fname: string);
var
  fs: TFile;
begin
  fs := TFile.Create(fname, fCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

//==============================================================================
//
// AddDataToWAD
//
//==============================================================================
function AddDataToWAD(const wad: TWADWriter; const lumpname, data: string): boolean;
begin
  if wad <> nil then
  begin
    wad.AddString(lumpname, data);
    Result := True;
  end
  else
    Result := False;
end;

end.

