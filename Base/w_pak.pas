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
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit w_pak;

interface

uses
  d_delphi{$IFNDEF FPC},
  z_files{$ENDIF};

const
  Pakid: integer = $4B434150;   // 'PACK' In Hex!
  WAD2id: integer = $32444157;  // 'WAD2' in Hex!
  WAD3id: integer = $33444157;  // 'WAD3' in Hex!

const
  PAKHEADSTRINGSIZE = 56;

type
  FPakHead = packed record // A PAK Directory Entry
    Name: packed array[1..PAKHEADSTRINGSIZE] of char;
    Offs: integer;
    Fsize: integer;
  end;

  TFPakHeadArray = packed array[0..$FFFF] of FPakHead;
  PFPakHeadArray = ^TFPakHeadArray;

  FWadHead = packed record // A WAD2/WAD3 Directory Entry
    Offs: integer;
    disksize: integer;
    size: integer;  // uncompressed
    _type: char;
    compression: char;
    pad1, pad2: char;
    name: packed array[1..16] of char; // must be null terminated
  end;

  TFWadHeadArray = packed array[0..$FFFF] of FWadHead;
  PFWadHeadArray = ^TFWadHeadArray;

type
  {$IFNDEF FPC}
  TCompressorCache = class(TObject)
  private
    fZip: TZipFile;
    fID: integer;
    fPosition: integer;
    fSize: integer;
    data: pointer;
  public
    constructor Create(aZip: TZipFile; aID: integer); virtual;
    destructor Destroy; override;
    function Read(var Buf; Sz: Integer): integer;
    function Seek(pos: integer): boolean;
    property Position: integer read fPosition;
    property Size: integer read fSize;
  end;
  {$ENDIF}

  TPakEntry = record // A Directory Entry Memory Image
    Pak: string[255];
    Name: string[255];
    ShortName: string[32];
    Offset, Size: Integer;
    Hash: Integer;
  {$IFNDEF FPC}
    ZIP: TZipFile;
  {$ENDIF}
  end;
  PPakEntry = ^TPakEntry;

  TPakEntries = array[0..$FFFF] of TPakEntry;
  PPakEntries = ^TPakEntries;

  TPakFile = record
    Entry: Integer;
    F: file;
  {$IFNDEF FPC}
    Z: TCompressorCache;
  {$ENDIF}
  end;
  PPakFile = ^TPakFile;

const
  PAKHASHSIZE = 8192;

type
  PPakHash = ^TPakHash;
  TPakHash = record
    index: integer;
    next: PPakHash;
  end;

  PPakHashArray = array[0..PAKHASHSIZE - 1] of PPakHash;
  PPakHashPArray = ^PPakHashArray;

  TPakManager = class
  private
    Entries: PPakEntries;
    NumEntries: Integer;
    MaxEntries: Integer;
    PAKS: TDStringList;
    HashTable: PPakHashPArray;
    procedure Grow;
    procedure AddEntry(var H: FPakHead; const Pakn: string); overload;
    procedure AddEntry(var HD: FWADhead; const Pakn: string); overload;
    procedure AddEntry(const aPos, aSize: integer; const aname: string; const Pakn: string); overload;
  {$IFNDEF FPC}
    procedure AddEntry(ZIPFILE: TZipFile; const ZIPFileName, EntryName: string; const index: integer); overload;
  {$ENDIF}
    function HashToHashTableIndex(const hash: integer): integer;
    procedure AddEntryToHashTable(const idx: integer);
    function POpenPreferedFileNameSearch(var F: TPakFile; const aName: string; prefdirs: TDStringList): boolean;
    function POpenPreferedFileNameHash(var F: TPakFile; const aName: string; prefdirs: TDStringList): boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure GetEntries(var s: TDStringList);
    function GetMatchingEntries(Name: string): TDNumberList;
    function POpenFileName(var F: TPakFile; Name: string): boolean;
    function POpenEntry(var F: TPakFile; const idx: integer): boolean;
    function POpenShortFileName(var F: TPakFile; Name: string): boolean;
    function POpenFileNameWithDirectory(var F: TPakFile; Name: string): boolean;
    function POpenPreferedFileName(var F: TPakFile; const aName: string; prefdirs: TDStringList): boolean;
    function PClosefile(var F: TPakFile): boolean;
    function PBlockRead(var F: TPakFile; var Buf; const Size: Integer): integer;
    function PSeek(var F: TPakFile; const Pos: Integer): boolean;
    function PFilePos(var F: TPakFile): Integer;
    function PFileSize(var F: TPakFile): Integer;
    procedure PAddDirectory(const path: string);
    function PAddFile(const FileName: string): boolean;
  end;

  pakmode_t = (
    pm_full,      // Full pathname match
    pm_short,     // Filename only match
    pm_prefered,  // Filename match but specified prefered directories
    pm_directory  // A dicectory will be provided
  );

function PAK_GetDirectoryListFromString(const aprefdirs: string): TDStringList;

type
  TPakStream = class(TDStream)
  private
    entry: TPakFile;
    manager: TPakManager;
    mode: pakmode_t;
    prefdirs: TDStringList;
  public
    constructor Create(const FileName: string; amode: pakmode_t; const aprefdirs: string = ''; const adir: string = ''); overload;
    constructor Create(const FileName: string; amode: pakmode_t; const apreflist: TDStringList; const adir: string = ''); overload;
    constructor Create(const idx: integer); overload;
    destructor Destroy; override;
    function Read(var Buffer; Count: integer): integer; override;
    function Write(const Buffer; Count: integer): integer; override;
    function Seek(Offset: integer; Origin: Word): integer; override;
    function Size: integer; override;
    function Position: integer; override;
  end;

procedure PAK_InitFileSystem;
procedure PAK_ShutDown;

procedure PAK_AddDirectory(const path: string);
function PAK_AddFile(const FileName: string): boolean;
function PAK_GetMatchingEntries(const Name: string): TDNumberList;

type
  stringparmproc_t = procedure(const atext: string);

function PAK_StringIterator(const filename: string; proc: stringparmproc_t): integer;

function PAK_FileNameIterator(proc: stringparmproc_t): integer;

function PAK_ReadFileAsString(const filename: string): string;

function PAK_ReadAllFilesAsString(const filename: string): string;

procedure PAK_LoadPendingPaks;

implementation

uses
  i_system,
  t_pcx,
  t_patch,
  w_wad;

{$IFNDEF FPC}
{******** TCompressorCache ********}
constructor TCompressorCache.Create(aZip: TZipFile; aID: integer);
begin
  Inherited Create;
  fZip := aZip;
  fID := aID;
  fZip.GetZipFileData(fID, data, fSize);
  fPosition := 0;
end;

destructor TCompressorCache.Destroy;
begin
  memfree(data, fSize);
  Inherited Destroy;
end;

function TCompressorCache.Read(var Buf; Sz: Integer): integer;
begin
  if fPosition + Sz > Size then
    result := Size - fPosition
  else
    result := Sz;

  memcpy(@buf, pointer(integer(data) + fPosition), result);
  fPosition := fPosition + result;
end;

function TCompressorCache.Seek(pos: integer): boolean;
begin
  if (pos < 0) or (pos > Size) then
    result := false
  else
  begin
    fPosition := pos;
    result := true;
  end;
end;
{$ENDIF}

function MkHash(const s: string): integer;
var
  i: integer;
begin
  result := 0;
  for i := 1 to length(s) do
  begin
    result := ((result shl 7) or (result shr 25)) + Ord(s[i]);
  end;
end;

{********* TPackDir *********}
constructor TPakManager.Create;
begin
  PAKS := TDStringList.Create;

  Entries := nil;
  NumEntries := 0;
  MaxEntries := 0;

  HashTable := mallocz(SizeOf(PPakHashArray));
end;

procedure TPakManager.PAddDirectory(const path: string);

  procedure DoLoad(const msk: string);
  var
    mask: string;
    sl: TDStringList;
    i: integer;
  begin
    mask := msk;
    if path <> '' then
      mask := path + '\' + mask;

    sl := findfiles(mask);
    for i := 0 to sl.Count - 1 do
      PAddFile(sl[i]);
    sl.Free;
  end;

begin
// JVAL
// Autoload file types in directory
// Does not load ZIP files, only PK3/PK4 files as well as PAK files and new WAD format.
  DoLoad('*.PAK');
{$IFNDEF FPC}
  DoLoad('*.PK3');
  DoLoad('*.PK4');
{$ENDIF}
  DoLoad('*.WAD');
end;

procedure TPakManager.Grow;
var
  newentries: integer;
begin
  inc(NumEntries);
  if NumEntries > MaxEntries then
  begin
    newentries := MaxEntries + 512;
    realloc(pointer(Entries), MaxEntries * Sizeof(TPakentry), newentries * SizeOf(TPakentry));
    MaxEntries := newentries;
  end;
end;

{$IFNDEF FPC}
// Add a ZIP file entry (ZIP/PK3/PK4)
procedure TPakManager.AddEntry(ZIPFILE: TZipFile; const ZIPFileName, EntryName: string; const index: integer);
var
  e: PPakEntry;
begin
  Grow;
  e := @Entries[NumEntries - 1];
  e.Pak := ZIPFileName;
  e.Name := fixpathname(strupper(EntryName));
  e.ShortName := fshortname(e.Name);
  e.Hash := MkHash(e.ShortName);
  e.Offset := index; // offset -> index to ZIP file
  e.Size := 0;
  e.ZIP := ZIPFILE;

  AddEntryToHashTable(NumEntries - 1);
end;
{$ENDIF}

// Add an entry from Quake PAK file
procedure TPakManager.AddEntry(var H: FPakHead; const Pakn: string); // Add A Pak Entry to Memory List
var
  S: string;
  I: Integer;
  e: PPakEntry;
begin
  Grow;

  S := '';
  for I := 1 to PAKHEADSTRINGSIZE do
    if H.Name[I] <> #0 then
      S := S + toupper(H.Name[I])
    else
      break;

  e := @Entries[NumEntries - 1];
  e.Pak := Pakn;
  e.Name := fixpathname(S);
  e.ShortName := fshortname(e.Name);
  e.Hash := MkHash(e.ShortName);
  e.Offset := H.Offs;
  e.Size := H.Fsize;
{$IFNDEF FPC}
  e.ZIP := nil;
{$ENDIF}

  AddEntryToHashTable(NumEntries - 1);
end;

// Add an entry from a WAD file (new WAD version)
procedure TPakManager.AddEntry(var HD: FWADhead; const Pakn: string);
var
  S: string;
  I: Integer;
  e: PPakEntry;
begin
  Grow;

  S := '';
  for I := 1 to 16 do
    if HD.Name[I] <> #0 then
      S := S + UpCase(HD.Name[I])
    else
      break;

  e := @Entries[NumEntries - 1];
  e.Pak := Pakn;
  e.Name := fixpathname(S);
  e.ShortName := fshortname(e.Name);
  e.Hash := MkHash(e.ShortName);
  e.Offset := HD.Offs;
  e.Size := HD.size;
{$IFNDEF FPC}
  e.ZIP := nil;
{$ENDIF}

  AddEntryToHashTable(NumEntries - 1);
end;

procedure TPakManager.AddEntry(const aPos, aSize: integer; const aname: string; const Pakn: string);
var
  e: PPakEntry;
begin
  Grow;

  e := @Entries[NumEntries - 1];
  e.Pak := Pakn;
  e.Name := fixpathname(strupper(aname));
  e.ShortName := fshortname(e.Name);
  e.Hash := MkHash(e.ShortName);
  e.Offset := aPos;
  e.Size := aSize;
{$IFNDEF FPC}
  e.ZIP := nil;
{$ENDIF}

  AddEntryToHashTable(NumEntries - 1);
end;

function TPakManager.HashToHashTableIndex(const hash: integer): integer;
begin
  result := abs(hash) mod PAKHASHSIZE;
end;

procedure TPakManager.AddEntryToHashTable(const idx: integer);
var
  hashidx: integer;
  parent: PPakHash;
begin
  hashidx := HashToHashTableIndex(Entries[idx].Hash);

  parent := HashTable[hashidx];

  HashTable[hashidx] := malloc(SizeOf(TPakHash));
  HashTable[hashidx].index := idx;
  HashTable[hashidx].next := parent;
end;

function TPakManager.PAddFile(const FileName: string): boolean; // Add A Pak file
var
  Nr: Integer;
  N, Id, Ofs:Integer;
  F: file;
  P: Pointer;
  I, J: Integer;
{$IFNDEF FPC}
  z: TZipFile;
{$ENDIF}
  pkid: integer;
  Fn: string;
  inHIRES: boolean;
  wadlump: filelump_t;
  b4: packed array[0..127] of byte;
  pk3lumps: TDStringList;
  pk3lump: string;
  wadlumpname: string;
  pc: integer;
  stmp: string;
  wads: TDStringList;

  procedure CheckWadEntry(const wadname: string);
  begin
    if strupper(fext(wadname)) = '.WAD' then
    begin
      if wads = nil then
        wads := TDStringList.Create;
      wads.Add(wadname);
    end;
  end;

  function _GetPakNameFromPakHead(const p: FPakHead): string;
  var
    x: integer;
  begin
    result := '';
    for x := 1 to PAKHEADSTRINGSIZE do
      if p.Name[x] = #0 then
        break
      else
        result := result + p.Name[x];
  end;

begin
  result := false;
  Fn := strupper(FileName);
  if PAKS.IndexOf(Fn) > -1 then
    exit;

  pkid := PAKS.Add(Fn);
  PAKS.Objects[pkid] := nil;

  if not fopen(F, fn, fOpenReadOnly) then
    exit;

  Blockread(F, Id, 4, N);
  if N <> 4 then
  begin
    close(F);
    exit;
  end;
  if (Id <> Pakid) and (Id <> WAD2Id) and (Id <> WAD3Id){$IFNDEF FPC} and (id <> ZIPFILESIGNATURE) {$ENDIF} and
     (Id <> IWAD) and (Id <> PWAD) and (Id <> DWAD) then
  begin
    result := false;
    close(F);
    exit;
  end;

  wads := nil;

  if Id = Pakid then // PAK file
  begin
    BlockRead(F, Ofs, 4, N);
    if N <> 4 then
    begin
      close(F);
      exit;
    end;
    BlockRead(F, Nr, 4, N);
    if N <> 4 then
    begin
      close(F);
      exit;
    end;
    Nr := Nr div SizeOf(FPakHead);
    Seek(F, Ofs);
    P := malloc(Nr * SizeOf(FPakHead));
    Blockread(f, P^, Nr * SizeOf(FPakHead), N);
    for i := 0 to N div SizeOf(FPakHead) - 1 do
    begin
      AddEntry(PFPakHeadArray(P)[i], Fn);
      CheckWadEntry(_GetPakNameFromPakHead(PFPakHeadArray(P)[i]));
    end;
    memfree(P, Nr * SizeOf(FPakHead));
  end
{$IFNDEF FPC}
  else if id = ZIPFILESIGNATURE then // zip, pk3, pk4 file
  begin
    z := TZipFile.Create(Fn);
    PAKS.Objects[pkid] := z;
    for i := 0 to z.FileCount - 1 do
    begin
      AddEntry(z, Fn, z.Files[i], i);
      CheckWadEntry(z.Files[i]);
    end;
  end
{$ENDIF}
  else if (Id = IWAD) or (Id = PWAD) or (Id = DWAD) then  // DOOM WAD
  begin
    BlockRead(F, Nr, 4, N);
    if N <> 4 then
    begin
      close(F);
      exit;
    end;
    BlockRead(F, Ofs, 4, N);
    if N <> 4 then
    begin
      close(F);
      exit;
    end;
    seek(F, Ofs);
    inHIRES := false;
    pk3lumps := TDStringList.Create;
    for i := 0 to Nr - 1 do
    begin
      BlockRead(F, wadlump, SizeOf(filelump_t), N);
      // 'PK3ENTRY' wad lump contains aliases for wad lumps (8 characters - short filename)
      // to the PK3/PAK filesystem (long filenames).
      wadlumpname := char8tostring(wadlump.name);
      if wadlumpname = 'PK3ENTRY' then
      begin
        Seek(F, wadlump.filepos);
        SetLength(pk3lump, wadlump.size);
        for j := 1 to wadlump.size do
          BlockRead(F, pk3lump[j], SizeOf(Char));
        pk3lumps.Text := pk3lumps.Text + pk3lump + #13#10;
        Seek(F, Ofs + (i + 1) * SizeOf(filelump_t));
      end
      else if wadlumpname = 'HI_START' then
        inHIRES := True
      else if wadlumpname = 'HI_END' then
        inHIRES := False
      else if inHIRES then
      begin
        Seek(F, wadlump.filepos);
        ZeroMemory(@b4, SizeOf(b4));
        BlockRead(F, b4, 128, N);
        if (b4[0] = 0) and (b4[1] = 0) and (b4[2] = 2) and (b4[3] = 0) then // TGA
          AddEntry(wadlump.filepos, wadlump.size, wadlumpname + '.TGA', Fn)
        else if (b4[1] = $50) and (b4[2] = $4E) and (b4[3] = $47) then // PNG
          AddEntry(wadlump.filepos, wadlump.size, wadlumpname + '.PNG', Fn)
        else if (b4[0] = $42) and (b4[1] = $4D) then // BMP
          AddEntry(wadlump.filepos, wadlump.size, wadlumpname + '.BMP', Fn)
        else if (b4[6] = $4A) and (b4[7] = $46) and (b4[8] = $49) and (b4[9] = $46) then // JPEG
          AddEntry(wadlump.filepos, wadlump.size, wadlumpname + '.JPG', Fn)
        else if (N >= 128) and T_IsValidPCXHeader(@b4) then // PCX detection
          AddEntry(wadlump.filepos, wadlump.size, wadlumpname + '.PCX', Fn)
        else if (N >= 4) and T_IsValidPatchImage(f, wadlump.filepos, wadlump.size) then
          AddEntry(wadlump.filepos, wadlump.size, wadlumpname + '.PATCH', Fn);

        Seek(F, Ofs + (i + 1) * SizeOf(filelump_t));
      end;
    end;
    // JVAL: 20190913 Allow comments inside PK3ENTRY lumps
    pk3lump := '';
    for i := 0 to pk3lumps.Count - 1 do
    begin
      stmp := pk3lumps.Strings[i];
      pc := Pos('//', stmp);
      if pc > 0 then
        SetLength(stmp, pc - 1);
      stmp := strremovespaces(stmp);
      if stmp <> '' then
        pk3lump := pk3lump + stmp + #13#10;
    end;
    pk3lumps.Text := pk3lump;
    // JVAL: 20170904 Remove blanc entries
    for i := pk3lumps.Count - 1 downto 0 do
      if pk3lumps.Strings[i] = '' then
        pk3lumps.Delete(i);
    if pk3lumps.Count > 0 then
    begin
      seek(F, Ofs);
      for i := 0 to Nr - 1 do
      begin
        BlockRead(F, wadlump, SizeOf(filelump_t), N);
        wadlumpname := char8tostring(wadlump.name);
        if pk3lumps.IndexOfName(wadlumpname) > -1 then
          AddEntry(wadlump.filepos, wadlump.size, pk3lumps.Values[wadlumpname], Fn)
      end;
    end;
    pk3lumps.Free;
  end
  else // WAD2 or WAD3
  begin
    BlockRead(F, Nr, 4, N);
    if N <> 4 then
    begin
      close(F);
      exit;
    end;
    BlockRead(F, Ofs, 4, N);
    if N <> 4 then
    begin
      close(F);
      exit;
    end;
    seek(F, Ofs);
    P := malloc(Nr * SizeOf(FWadHead));
    Blockread(f, P^, Nr * SizeOf(FWadHead), N);
    for i := 0 to N div SizeOf(FWadHead) - 1 do
      AddEntry(PFWadHeadArray(P)[i], Fn);
    memfree(P, Nr * SizeOf(FWadHead));

  end;
  close(F);
  result := true;
  printf(' adding %s'#13#10, [FileName]);

  if wads <> nil then
  begin
    for i := 0 to wads.Count - 1 do
      W_AddPendingWADfromPAK(wads.Strings[i]);
    wads.Free;
  end;
end;

procedure TPakManager.GetEntries(var s: TDStringList);
var i: integer;
begin
  if s = nil then
    s := TDStringList.Create;
  for i := NumEntries - 1 downto 0 do
    s.Add(Entries[I].Name);
end;

function TPakManager.GetMatchingEntries(Name: string): TDNumberList;
var
  i: integer;
  hcode: integer;
  pe: PPakEntry;
begin
  result := TDNumberList.Create;

  Name := strupper(fshortname(Name));
  hcode := MkHash(Name);

  for i := 0 to NumEntries - 1 do
  begin
    pe := @Entries[i];
    if hcode = pe.Hash then
      if pe.ShortName = Name then
      begin // Found In Pak
        result.Add(i)
      end;
  end;

end;

// Opens a file
function TPakManager.POpenFileName(var F: TPakFile; Name: string): boolean;
var
  I: Integer;
  hcode: integer;
  pe: PPakEntry;
  hashcheck: PPakHash;
begin
  result := false;
{$IFNDEF FPC}
  F.Z := nil;
{$ENDIF}

  if fopen(F.F, Name, fOpenReadOnly) then
  begin
    F.Entry := -1;
    result := true;
    Exit;
  end; // Disk file Overrides Pak file

  Name := strupper(Name);
  hcode := MkHash(fshortname(Name));

  hashcheck := HashTable[HashToHashTableIndex(hcode)];
  while hashcheck <> nil do
  begin
    pe := @Entries[hashcheck.index];
    if hcode = pe.Hash then   // Fast compare the hash values
      if pe.Name = Name then  // Slow compare strings
      begin // Found In Pak
      {$IFNDEF FPC}
        if pe.ZIP <> nil then // It's a zip (pk3/pk4) file
          F.Z := TCompressorCache.Create(pe.ZIP, pe.Offset)
        else
      {$ENDIF}
        begin // Standard Quake1/2 pak file
          if not fopen(F.F, string(pe.Pak), fOpenReadOnly) then
            exit;
          Seek(F.F, pe.Offset);
        end;
        F.Entry := hashcheck.index;
        result := true;
        exit;
      end;
    hashcheck := hashcheck.next;
  end;

  // Highly unlikely that we get to this point....
  for i := NumEntries - 1 downto 0 do // From last entry to zero, last file has priority
  begin
    pe := @Entries[i];
    if hcode = pe.Hash then   // Fast compare the hash values
      if pe.Name = Name then  // Slow compare strings
      begin // Found In Pak
      {$IFNDEF FPC}
        if pe.ZIP <> nil then // It's a zip (pk3/pk4) file
          F.Z := TCompressorCache.Create(pe.ZIP, pe.Offset)
        else
      {$ENDIF}
        begin // Standard Quake1/2 pak file
          if not fopen(F.F, string(pe.Pak), fOpenReadOnly) then
            exit;
          Seek(F.F, pe.Offset);
        end;
        F.Entry := i;
        result := true;
        exit;
      end;
  end;
end;

function TPakManager.POpenEntry(var F: TPakFile; const idx: integer): boolean;
var
  pe: PPakEntry;
begin
  result := false;

  if idx < 0 then
    exit;

  if idx >= NumEntries then
    exit;

  pe := @Entries[idx];
  {$IFNDEF FPC}
  if pe.ZIP <> nil then // It's a zip (pk3/pk4) file
    F.Z := TCompressorCache.Create(pe.ZIP, pe.Offset)
  else
  {$ENDIF}
  begin // Standard Quake1/2 pak file
    if not fopen(F.F, string(pe.Pak), fOpenReadOnly) then
      exit;
    Seek(F.F, pe.Offset);
  end;
  F.Entry := idx;
  result := true;
end;

// Opens a file without extensive search, checks filenames only, not directory structure!!
function TPakManager.POpenShortFileName(var F: TPakFile; Name: string): boolean;
var
  I: Integer;
  hcode: integer;
  pe: PPakEntry;
  hashcheck: PPakHash;
begin
  result := false;
{$IFNDEF FPC}
  F.Z := nil;
{$ENDIF}

  if fopen(F.F, Name, fOpenReadOnly) then
  begin
    F.Entry := -1;
    result := true;
    Exit;
  end; // Disk file Overrides Pak file

  Name := strupper(fshortname(Name));
  hcode := MkHash(Name);

  hashcheck := HashTable[HashToHashTableIndex(hcode)];
  while hashcheck <> nil do
  begin
    pe := @Entries[hashcheck.index];
    if hcode = pe.Hash then   // Fast compare the hash values
      if pe.ShortName = Name then  // Slow compare strings
      begin // Found In Pak
      {$IFNDEF FPC}
        if pe.ZIP <> nil then // It's a zip (pk3/pk4) file
          F.Z := TCompressorCache.Create(pe.ZIP, pe.Offset)
        else
      {$ENDIF}
        begin // Standard Quake1/2 pak file
          if not fopen(F.F, string(pe.Pak), fOpenReadOnly) then
            exit;
          Seek(F.F, pe.Offset);
        end;
        F.Entry := hashcheck.index;
        result := true;
        exit;
      end;
    hashcheck := hashcheck.next;
  end;

  // Highly unlikely that we get to this point....
  for I := NumEntries - 1 downto 0 do
  begin
    pe := @Entries[i];
    if hcode = pe.Hash then
      if pe.ShortName = Name then
      begin // Found In Pak
      {$IFNDEF FPC}
        if pe.ZIP <> nil then
          F.Z := TCompressorCache.Create(pe.ZIP, pe.Offset)
        else
      {$ENDIF}
        begin
          if not fopen(F.F, string(pe.Pak), fOpenReadOnly) then
            exit;
          Seek(F.F, pe.Offset);
        end;
        F.Entry := I;
        result := true;
        Exit;
      end;
  end;
end;

// Name contains a directory
function TPakManager.POpenFileNameWithDirectory(var F: TPakFile; Name: string): boolean;
var
  I, p: Integer;
  hcode: integer;
  pe: PPakEntry;
  hashcheck: PPakHash;
begin
  result := false;

  Name := fixpathname(Name);

  p := Pos('\', Name);
  if p = 1 then
  begin
    Delete(Name, 1, 1);
    p := Pos('\', Name);
    if p = 1 then
      Exit;
  end;

  Name := strtrim(Name);
  if Name = '' then
    Exit;

{$IFNDEF FPC}
  F.Z := nil;
{$ENDIF}

  if fopen(F.F, Name, fOpenReadOnly) then
  begin
    F.Entry := -1;
    result := true;
    Exit;
  end; // Disk file Overrides Pak file

  Name := strupper(fshortname(Name));
  hcode := MkHash(Name);

  hashcheck := HashTable[HashToHashTableIndex(hcode)];
  while hashcheck <> nil do
  begin
    pe := @Entries[hashcheck.index];
    if hcode = pe.Hash then   // Fast compare the hash values
      if (pe.Name = Name) or (RightStr(pe.Name, Length(Name) + 1) =  '\' + Name) then
      begin // Found In Pak
      {$IFNDEF FPC}
        if pe.ZIP <> nil then // It's a zip (pk3/pk4) file
          F.Z := TCompressorCache.Create(pe.ZIP, pe.Offset)
        else
      {$ENDIF}
        begin // Standard Quake1/2 pak file
          if not fopen(F.F, string(pe.Pak), fOpenReadOnly) then
            exit;
          Seek(F.F, pe.Offset);
        end;
        F.Entry := hashcheck.index;
        result := true;
        exit;
      end;
    hashcheck := hashcheck.next;
  end;

  // Highly unlikely that we get to this point....
  for I := NumEntries - 1 downto 0 do
  begin
    pe := @Entries[i];
    if hcode = pe.Hash then
      if (pe.Name = Name) or (RightStr(pe.Name, Length(Name) + 1) =  '\' + Name) then
      begin // Found In Pak
      {$IFNDEF FPC}
        if pe.ZIP <> nil then
          F.Z := TCompressorCache.Create(pe.ZIP, pe.Offset)
        else
      {$ENDIF}
        begin
          if not fopen(F.F, string(pe.Pak), fOpenReadOnly) then
            exit;
          Seek(F.F, pe.Offset);
        end;
        F.Entry := I;
        result := true;
        Exit;
      end;
  end;
end;

type
  Ppref_rec = ^pref_rec;
  pref_rec = record
    index: integer;
    priority: integer;
    next: Ppref_rec;
  end;

procedure recourcefreememlist(var list: Ppref_rec);
begin
  if list <> nil then
  begin
    recourcefreememlist(list.next);
    memfree(pointer(list), SizeOf(pref_rec));
  end;
end;

// Opens a file with extensive search prefering the pathname to be contained to prefdirs
// Serial search, does not use hash table
function TPakManager.POpenPreferedFileNameSearch(var F: TPakFile; const aName: string; prefdirs: TDStringList): boolean;
var
  I: Integer;
  hcode: integer;
  pe: PPakEntry;
  pref_head: Ppref_rec;
  pref_list: Ppref_rec;
  j: integer;
  bestpriority: integer;
  Name: string;
begin
  result := false;
{$IFNDEF FPC}
  F.Z := nil;
{$ENDIF}

  pref_head := malloc(SizeOf(pref_rec));
  pref_head.index := -1;
  pref_head.priority := MAXINT;
  pref_head.next := nil;
  pref_list := pref_head;

  Name := strupper(fshortname(aName));
  hcode := MkHash(Name);
  for I := NumEntries - 1 downto 0 do
  begin
    pe := @Entries[i];
    if hcode = pe.Hash then
      if pe.ShortName = Name then
      begin // Found In Pak
        pref_list.index := i;
        pref_list.priority := prefdirs.Count;
        for j := 0 to prefdirs.Count - 1 do
          if Pos(prefdirs[j], pe.Name) > 0 then
          begin
            pref_list.priority := j;
            break;
          end;
        pref_list.next := malloc(SizeOf(pref_rec));
        pref_list := pref_list.next;
        pref_list.index := -1;
        pref_list.next := nil;
      end;
  end;

  pref_list := pref_head;
  bestpriority := MAXINT;
  i := -1;
  while pref_list.index >= 0 do
  begin
    if pref_list.priority < bestpriority then
    begin
      i := pref_list.index;
      bestpriority := pref_list.priority;
    end;
    pref_list := pref_list.next;
  end;

  recourcefreememlist(pref_head);

  if i < 0 then
    exit;

  pe := @Entries[i];
  {$IFNDEF FPC}
  if pe.ZIP <> nil then
    F.Z := TCompressorCache.Create(pe.ZIP, pe.Offset)
  else
  {$ENDIF}
  begin
    if not fopen(F.F, string(pe.Pak), fOpenReadOnly) then
      exit;
    Seek(F.F, pe.Offset);
  end;
  F.Entry := I;
  result := true;
end;

// Opens a file with extensive search prefering the pathname to be contained to prefdirs
// Fast search using hash table
function TPakManager.POpenPreferedFileNameHash(var F: TPakFile; const aName: string; prefdirs: TDStringList): boolean;
var
  I: Integer;
  hcode: integer;
  pe: PPakEntry;
  pref_head: Ppref_rec;
  pref_list: Ppref_rec;
  j: integer;
  bestpriority: integer;
  Name: string;
  hashcheck: PPakHash;
begin
  result := false;
{$IFNDEF FPC}
  F.Z := nil;
{$ENDIF}

  if fopen(F.F, aName, fOpenReadOnly) then
  begin
    F.Entry := -1;
    result := true;
    Exit;
  end; // Disk file Overrides Pak file

  pref_head := malloc(SizeOf(pref_rec));
  pref_head.index := -1;
  pref_head.priority := MAXINT;
  pref_head.next := nil;
  pref_list := pref_head;

  Name := strupper(fshortname(aName));
  hcode := MkHash(Name);

  hashcheck := HashTable[HashToHashTableIndex(hcode)];
  while hashcheck <> nil do
  begin
    pe := @Entries[hashcheck.index];
    if hcode = pe.Hash then
      if pe.ShortName = Name then
      begin // Found In Pak
        pref_list.index := hashcheck.index;
        pref_list.priority := MAXINT - 1;
        for j := 0 to prefdirs.Count - 1 do
        begin
          if prefdirs[j] + Name = pe.Name then
          begin
            pref_list.priority := j;
            break;
          end
          else if Pos(prefdirs[j], pe.Name) > 0 then
          begin
            pref_list.priority := prefdirs.Count + j;
            break;
          end;
        end;
        pref_list.next := malloc(SizeOf(pref_rec));
        pref_list := pref_list.next;
        pref_list.index := -1;
        pref_list.next := nil;
      end;
    hashcheck := hashcheck.next;
  end;

  pref_list := pref_head;
  bestpriority := MAXINT;
  i := -1;
  while pref_list.index >= 0 do
  begin
    if pref_list.priority < bestpriority then
    begin
      i := pref_list.index;
      bestpriority := pref_list.priority;
    end;
    pref_list := pref_list.next;
  end;

  recourcefreememlist(pref_head);

  if i < 0 then
    exit;

  pe := @Entries[i];
  {$IFNDEF FPC}
  if pe.ZIP <> nil then
    F.Z := TCompressorCache.Create(pe.ZIP, pe.Offset)
  else
  {$ENDIF}
  begin
    if not fopen(F.F, string(pe.Pak), fOpenReadOnly) then
      exit;
    Seek(F.F, pe.Offset);
  end;
  F.Entry := I;
  result := true;
end;

function TPakManager.POpenPreferedFileName(var F: TPakFile; const aName: string; prefdirs: TDStringList): boolean;
begin
  result := POpenPreferedFileNameHash(F, aName, prefdirs);
  if not result then
    result := POpenPreferedFileNameSearch(F, aName, prefdirs);
end;

function TPakManager.PClosefile(var F: TPakFile): boolean;
begin
{$IFNDEF FPC}
  if F.Z <> nil then
  begin
    F.Z.Free;
    F.Z := nil;
    result := true;
  end
  else
{$ENDIF}
  begin
    {$I-}
    Close(F.F);
    {$I+}
    result := IOResult = 0;
  end;
end;

function TPakManager.PBlockRead(var F: TPakFile; var Buf; const Size: Integer): integer;
begin
{$IFNDEF FPC}
  if F.Z <> nil then
    result := F.Z.Read(Buf, Size)
  else
{$ENDIF}
  begin
    {$I-}
    Blockread(F.F, Buf, Size, result);
    {$I+}
  end;
end;

function TPakManager.PSeek(var F: TPakFile; const Pos: Integer): boolean;
begin
{$IFNDEF FPC}
  if F.Z <> nil then
    result := F.Z.Seek(pos)
  else
{$ENDIF}
  begin
  {$I-}
    if F.Entry = -1 then
      Seek(F.F, Pos)
    else
      Seek(F.F, Entries[F.Entry].Offset + Pos);
    {$I+}
    result := IOResult = 0;
  end;
end;

function TPakManager.PFilePos(var F: TPakFile): Integer;
begin
{$IFNDEF FPC}
  if F.Z <> nil then
    result := F.Z.Position
  else
{$ENDIF}
  begin
    result := FilePos(F.F);
    if F.Entry <> -1 then
      result := result - Entries[F.Entry].Offset;
  end;
end;

function TPakManager.PFileSize(var F: TPakFile): Integer;
begin
{$IFNDEF FPC}
  if F.Z <> nil then
    result := F.Z.Size
  else {$ENDIF} if F.Entry <> -1 then
    result := Entries[F.Entry].Size
  else
  begin
  {$I-}
    result := Filesize(F.F);
  {$I+}
    if IOResult <> 0 then
      result := 0;
  end;
end;

destructor TPakManager.Destroy;
var
  i: integer;

  procedure recoursivefreehashitem(var h: PPakHash);
  begin
    if h.next <> nil then
      recoursivefreehashitem(h.next);
    memfree(pointer(h), SizeOf(TPakHash));
  end;

begin
  for i := 0 to PAKHASHSIZE - 1 do
    if HashTable[i] <> nil then
      recoursivefreehashitem(HashTable[i]);

  memfree(pointer(HashTable), SizeOf(PPakHashArray));

  realloc(pointer(Entries), MaxEntries * Sizeof(TPakentry), 0);

  for i := 0 to PAKS.Count - 1 do
    if PAKS.Objects[i] <> nil then
      PAKS.Objects[i].Free;

  PAKS.Free;
end;

// Global Pak Loader Object
var
  pakmanager: TPakManager;

function PAK_GetDirectoryListFromString(const aprefdirs: string): TDStringList;
var
  i: integer;
  stmp: string;
  stmp2: string;
begin
  result := TDStringList.Create;
  if aprefdirs <> '' then
  begin
    stmp := '';
    for i := 1 to Length(aprefdirs) do
      if aprefdirs[i] in [' ', ','] then
        stmp := stmp + #13#10
      else
        stmp := stmp + toupper(aprefdirs[i]);
    result.Text := stmp;
    for i := result.count - 1 downto 0 do
      if result.strings[i] = '' then
        result.Delete(i);
    stmp := '';
    for i := 0 to result.Count - 1 do
    begin
      stmp2 := strtrim(result[i]);
      if Length(stmp2) > 0 then
      begin
        if stmp2[Length(stmp2)] <> '\' then
          stmp := stmp + stmp2 + '\'#13#10
        else
          stmp := stmp + stmp2 + #13#10;
      end;
    end;
    result.Text := stmp;
  end;
end;

//
// TPakStream
constructor TPakStream.Create(const FileName: string; amode: pakmode_t; const apreflist: TDStringList; const adir: string = '');
var
  ok, diddir: boolean;
  i: integer;
begin
  Inherited Create;

  OnBeginBusy := nil;
  OnEndBusy := nil;

  prefdirs := TDStringList.Create;

  if apreflist <> nil then
    for i := 0 to apreflist.Count - 1 do
      prefdirs.Add(apreflist[i]);

  manager := pakmanager;
  mode := amode;
  diddir := false;
  if (adir <> '') and (mode <> pm_full) then
  begin
    ok := manager.POpenFileNameWithDirectory(entry, adir + '\' + FileName);
    diddir := true;
  end
  else if mode = pm_full then
    ok := manager.POpenFileName(entry, FileName)
  else if mode = pm_short then
    ok := manager.POpenShortFileName(entry, FileName)
  else if mode = pm_prefered then
    ok := manager.POpenPreferedFileName(entry, FileName, prefdirs)
  else
    ok := false;

  if not ok then
  begin
    ok := manager.POpenFileNameWithDirectory(entry, FileName);
    if not ok then
      if not diddir then
        if adir <> '' then
          ok := manager.POpenFileNameWithDirectory(entry, adir + '\' + FileName);
  end;

  if not ok then
    FIOResult := 1
  else
    FIOResult := 0;
end;

constructor TPakStream.Create(const FileName: string; amode: pakmode_t; const aprefdirs: string = ''; const adir: string = '');
var
  apreflist: TDStringList;
begin
  apreflist := PAK_GetDirectoryListFromString(aprefdirs);
  Create(FileName, amode, apreflist, adir);
  apreflist.Free;
end;

constructor TPakStream.Create(const idx: integer);
var
  ok: boolean;
begin
  Inherited Create;

  OnBeginBusy := nil;
  OnEndBusy := nil;

  prefdirs := TDStringList.Create;

  manager := pakmanager;
  mode := pm_short;
  ok := manager.POpenEntry(entry, idx);
  if not ok then
    FIOResult := 1
  else
    FIOResult := 0;
end;

destructor TPakStream.Destroy;
begin
  manager.PClosefile(entry);
  prefdirs.Free;
  Inherited;
end;

function TPakStream.Read(var Buffer; Count: integer): integer;
begin
  result := manager.PBlockRead(entry, Buffer, Count);
  if system.IOResult <> 0 then
    inc(FIOResult);
end;

function TPakStream.Write(const Buffer; Count: integer): integer;
begin
  I_Warning('TPakStream::Write(): Pak managment is read-only'#13#10);
  inc(FIOResult);
  result := 0;
end;

function TPakStream.Seek(Offset: integer; Origin: Word): integer;
var
  p: integer;
begin
  if Origin = sFromBeginning then
    p := Offset
  else if Origin = sFromCurrent then
    p := manager.PFilePos(entry) + Offset
  else {sFromEnd}
    p := manager.PFileSize(entry) - Offset;

  if not manager.PSeek(entry, p) then
    inc(FIOResult);
  result := p;
end;

function TPakStream.Size: integer;
begin
  result := manager.PFileSize(entry)
end;

function TPakStream.Position: integer;
begin
  result := manager.PFilePos(entry);
end;

//
// PAK_InitFileSystem
//
procedure PAK_InitFileSystem;
begin
  pakmanager := TPakManager.Create;
end;

procedure PAK_ShutDown;
begin
  pakmanager.Free;
end;

var
  pendingpaks: TDStringList;

procedure PAK_LoadPendingPaks;
var
  i: integer;
begin
  if pendingpaks.Count = 0 then
    Exit;

  for i := 0 to pendingpaks.Count - 1 do
    if I_DirectoryExists(pendingpaks.Strings[i]) then
      pakmanager.PAddDirectory(pendingpaks.Strings[i])
    else
      pakmanager.PAddFile(pendingpaks.Strings[i]);
  pendingpaks.Clear;
end;

procedure PAK_AddDirectory(const path: string);
begin
  if pakmanager = nil then
  begin
    pendingpaks.Add(path);
    exit;
  end;

  PAK_LoadPendingPaks;

  printf(' adding directory %s'#13#10, [path]);
  pakmanager.PAddDirectory(path);
end;

function PAK_AddFile(const FileName: string): boolean;
begin
  if pakmanager = nil then
  begin
    pendingpaks.Add(FileName);
    Result := false;
    exit;
  end;

  PAK_LoadPendingPaks;

  if I_DirectoryExists(FileName) then
  begin
    result := true;
    PAK_AddDirectory(FileName)
  end
  else
    result := pakmanager.PAddFile(FileName);
end;

function PAK_GetMatchingEntries(const Name: string): TDNumberList;
begin
  result := pakmanager.GetMatchingEntries(Name);
end;

function PAK_StringIterator(const filename: string; proc: stringparmproc_t): integer;
var
  entries: TDNumberList;
  strm: TPakStream;
  list: TDStringList;
  i: integer;
begin
  result := 0;
  entries := PAK_GetMatchingEntries(filename);
  list := TDStringList.Create;
  for i := 0 to entries.Count - 1 do
  begin
    strm := TPakStream.Create(entries.Numbers[i]);
    if strm.IOResult = 0 then
    begin
      list.LoadFromStream(strm);
      proc(list.Text);
      inc(result);
    end;
    strm.Free;
  end;
  list.Free;
  entries.Free;
end;

function PAK_FileNameIterator(proc: stringparmproc_t): integer;
var
  i: integer;
begin
  result := pakmanager.NumEntries;
  for i := 0 to result - 1 do
    proc(pakmanager.Entries[i].Name);
end;

function PAK_ReadFileAsString(const filename: string): string;
var
  entries: TDNumberList;
  strm: TPakStream;
  list: TDStringList;
begin
  entries := PAK_GetMatchingEntries(filename);
  if entries.Count > 0 then
  begin
    list := TDStringList.Create;
    strm := TPakStream.Create(entries.Numbers[entries.Count - 1]);
    if strm.IOResult = 0 then
    begin
      list.LoadFromStream(strm);
      Result := list.Text;
    end;
    strm.Free;
    list.Free;
  end
  else
    Result := '';
  entries.Free;
end;

function PAK_ReadAllFilesAsString(const filename: string): string;
var
  entries: TDNumberList;
  strm: TPakStream;
  list: TDStringList;
  i: integer;
begin
  entries := PAK_GetMatchingEntries(filename);
  Result := '';
  for i := 0 to entries.Count - 1 do
  begin
    list := TDStringList.Create;
    strm := TPakStream.Create(entries.Numbers[i]);
    if strm.IOResult = 0 then
    begin
      list.LoadFromStream(strm);
      Result := Result + list.Text + #13#10;
    end;
    strm.Free;
    list.Free;
  end;
  entries.Free;
end;

initialization
  pendingpaks := TDStringList.Create;

finalization
  pendingpaks.Free;

end.

