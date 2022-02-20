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
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit w_wadreader;

interface

uses
  d_delphi,
  w_wad;

type
  TWadReader = class
  private
    h: wadinfo_t;
    la: Pfilelump_tArray;
    fs: TFile;
    ffilename: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure OpenWadFile(const aname: string);
    function EntryAsString(const id: integer): string; overload;
    function EntryAsString(const aname: string): string; overload;
    function ReadEntry(const id: integer; var buf: pointer; var bufsize: integer): boolean; overload;
    function ReadEntry(const aname: string; var buf: pointer; var bufsize: integer): boolean; overload;
    function EntryName(const id: integer): string;
    function EntryId(const aname: string): integer;
    function EntryInfo(const id: integer): Pfilelump_t; overload;
    function EntryInfo(const aname: string): Pfilelump_t; overload;
    function NumEntries: integer;
    function FileSize: integer;
    property FileName: string read ffilename;
  end;

implementation

uses
  i_system;

//==============================================================================
//
// TWadReader.Create
//
//==============================================================================
constructor TWadReader.Create;
begin
  h.identification := 0;
  h.numlumps := 0;
  h.infotableofs := 0;
  la := nil;
  fs := nil;
  ffilename := '';
  Inherited;
end;

//==============================================================================
//
// TWadReader.Destroy
//
//==============================================================================
destructor TWadReader.Destroy;
begin
  Clear;
  Inherited;
end;

//==============================================================================
//
// TWadReader.Clear
//
//==============================================================================
procedure TWadReader.Clear;
begin
  if h.numlumps > 0 then
  begin
    MemFree(pointer(la), h.numlumps * SizeOf(filelump_t));
    h.identification := 0;
    h.numlumps := 0;
    h.infotableofs := 0;
    la := nil;
    ffilename := '';
  end
  else
  begin
    h.identification := 0;
    h.infotableofs := 0;
  end;
  if fs <> nil then
  begin
    fs.Free;
    fs := nil;
  end;
end;

//==============================================================================
//
// TWadReader.OpenWadFile
//
//==============================================================================
procedure TWadReader.OpenWadFile(const aname: string);
begin
  if aname = '' then
    Exit;
  {$IFDEF DEBUG}
  print('Opening WAD file ' + aname + #13#10);
  {$ENDIF}
  Clear;
  if fexists(aname) then
  begin
    fs := TFile.Create(aname, fOpenReadOnly);

    fs.Read(h, SizeOf(wadinfo_t));
    if (h.numlumps > 0) and (h.infotableofs < fs.Size) and ((h.identification = IWAD) or (h.identification = PWAD)) then
    begin
      fs.Seek(h.infotableofs, sFromBeginning);
      la := malloc(h.numlumps * SizeOf(filelump_t));
      fs.Read(la^, h.numlumps * SizeOf(filelump_t));
      ffilename := aname;
    end
    else
      I_Warning('TWadReader.OpenWadFile(): Invalid WAD file ' + aname + #13#10);
  end
  else
    I_Warning('TWadReader.OpenWadFile(): Can not find WAD file ' + aname + #13#10);
end;

//==============================================================================
//
// TWadReader.EntryAsString
//
//==============================================================================
function TWadReader.EntryAsString(const id: integer): string;
begin
  if (fs <> nil) and (id >= 0) and (id < h.numlumps) then
  begin
    SetLength(Result, la[id].size);
    fs.Seek(la[id].filepos, sFromBeginning);
    fs.Read((@Result[1])^, la[id].size);
  end
  else
    Result := '';
end;

//==============================================================================
//
// TWadReader.EntryAsString
//
//==============================================================================
function TWadReader.EntryAsString(const aname: string): string;
var
  id: integer;
begin
  id := EntryId(aname);
  if id >= 0 then
    Result := EntryAsString(id)
  else
    Result := '';
end;

//==============================================================================
//
// TWadReader.ReadEntry
//
//==============================================================================
function TWadReader.ReadEntry(const id: integer; var buf: pointer; var bufsize: integer): boolean;
begin
  if (fs <> nil) and (id >= 0) and (id < h.numlumps) then
  begin
    fs.Seek(la[id].filepos, sFromBeginning);
    bufsize := la[id].size;
    buf := malloc(bufsize);
    fs.Read(buf^, bufsize);
    Result := true;
  end
  else
    Result := false;
end;

//==============================================================================
//
// TWadReader.ReadEntry
//
//==============================================================================
function TWadReader.ReadEntry(const aname: string; var buf: pointer; var bufsize: integer): boolean;
var
  id: integer;
begin
  id := EntryId(aname);
  if id >= 0 then
    Result := ReadEntry(id, buf, bufsize)
  else
    Result := false;
end;

//==============================================================================
//
// TWadReader.EntryName
//
//==============================================================================
function TWadReader.EntryName(const id: integer): string;
begin
  if (id >= 0) and (id < h.numlumps) then
    Result := char8tostring(la[id].name)
  else
    Result := '';
end;

//==============================================================================
//
// TWadReader.EntryId
//
//==============================================================================
function TWadReader.EntryId(const aname: string): integer;
var
  i: integer;
  uname: string;
begin
  uname := strupper(aname);
  for i := h.numlumps - 1 downto 0 do
    if char8tostring(la[i].name) = uname then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

//==============================================================================
//
// TWadReader.EntryInfo
//
//==============================================================================
function TWadReader.EntryInfo(const id: integer): Pfilelump_t;
begin
  if (id >= 0) and (id < h.numlumps) then
    Result := @la[id]
  else
    Result := nil;
end;

//==============================================================================
//
// TWadReader.EntryInfo
//
//==============================================================================
function TWadReader.EntryInfo(const aname: string): Pfilelump_t;
begin
  result := EntryInfo(EntryId(aname));
end;

//==============================================================================
//
// TWadReader.NumEntries
//
//==============================================================================
function TWadReader.NumEntries: integer;
begin
  Result := h.numlumps;
end;

//==============================================================================
//
// TWadReader.FileSize
//
//==============================================================================
function TWadReader.FileSize: integer;
begin
  if fs <> nil then
    Result := fs.Size
  else
    Result := 0;
end;

end.
