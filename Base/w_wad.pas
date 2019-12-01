//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2016 by Jim Valavanis
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
//  WAD I/O functions.
//  Handles WAD file header, directory, lump I/O.
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit w_wad;

interface

uses
  d_delphi;

//
// TYPES
//

type
  char8_t = array[0..7] of char;

  lumpindicator_t = record
    _START: char8_t;
    _END: char8_t;
    _type: byte;
  end;

const
  NUMINDICATORS = 37;
  IND_FLOOR = 1;
  IND_SPRITE = 2;
  IND_PATCH = 3;
  IND_VOXEL = 4;
  IND_MAX = 5;
  TYPE_FLOOR = 1 shl IND_FLOOR;
  TYPE_SPRITE = 1 shl IND_SPRITE;
  TYPE_PATCH = 1 shl IND_PATCH;
  TYPE_VOXEL = 1 shl IND_VOXEL;

  lumpindicators: array[0..NUMINDICATORS - 1] of lumpindicator_t = (
    (_START: 'F_START';  _END: 'F_END';    _type: IND_FLOOR),
    (_START: 'F1_START'; _END: 'F1_END';   _type: IND_FLOOR),
    (_START: 'F2_START'; _END: 'F2_END';   _type: IND_FLOOR),
    (_START: 'F3_START'; _END: 'F3_END';   _type: IND_FLOOR),
    (_START: 'F4_START'; _END: 'F4_END';   _type: IND_FLOOR),
    (_START: 'F5_START'; _END: 'F5_END';   _type: IND_FLOOR),
    (_START: 'F6_START'; _END: 'F6_END';   _type: IND_FLOOR),
    (_START: 'F7_START'; _END: 'F7_END';   _type: IND_FLOOR),
    (_START: 'F8_START'; _END: 'F8_END';   _type: IND_FLOOR),
    (_START: 'F9_START'; _END: 'F9_END';   _type: IND_FLOOR),
    (_START: 'F0_START'; _END: 'F0_END';   _type: IND_FLOOR),
    (_START: 'FF_START'; _END: 'FF_END';   _type: IND_FLOOR),
    (_START: 'S_START';  _END: 'S_END';    _type: IND_SPRITE),
    (_START: 'S1_START'; _END: 'S1_END';   _type: IND_SPRITE),
    (_START: 'S2_START'; _END: 'S2_END';   _type: IND_SPRITE),
    (_START: 'S3_START'; _END: 'S3_END';   _type: IND_SPRITE),
    (_START: 'S4_START'; _END: 'S4_END';   _type: IND_SPRITE),
    (_START: 'S5_START'; _END: 'S5_END';   _type: IND_SPRITE),
    (_START: 'S6_START'; _END: 'S6_END';   _type: IND_SPRITE),
    (_START: 'S7_START'; _END: 'S7_END';   _type: IND_SPRITE),
    (_START: 'S8_START'; _END: 'S8_END';   _type: IND_SPRITE),
    (_START: 'S9_START'; _END: 'S9_END';   _type: IND_SPRITE),
    (_START: 'S0_START'; _END: 'S0_END';   _type: IND_SPRITE),
    (_START: 'SS_START'; _END: 'SS_END';   _type: IND_SPRITE),
    (_START: 'P_START';  _END: 'P_END';    _type: IND_PATCH),
    (_START: 'P1_START'; _END: 'P1_END';   _type: IND_PATCH),
    (_START: 'P2_START'; _END: 'P2_END';   _type: IND_PATCH),
    (_START: 'P3_START'; _END: 'P3_END';   _type: IND_PATCH),
    (_START: 'P4_START'; _END: 'P4_END';   _type: IND_PATCH),
    (_START: 'P5_START'; _END: 'P5_END';   _type: IND_PATCH),
    (_START: 'P6_START'; _END: 'P6_END';   _type: IND_PATCH),
    (_START: 'P7_START'; _END: 'P7_END';   _type: IND_PATCH),
    (_START: 'P8_START'; _END: 'P8_END';   _type: IND_PATCH),
    (_START: 'P9_START'; _END: 'P9_END';   _type: IND_PATCH),
    (_START: 'P0_START'; _END: 'P0_END';   _type: IND_PATCH),
    (_START: 'PP_START'; _END: 'PP_END';   _type: IND_PATCH),
    (_START: 'VX_START'; _END: 'VX_END';   _type: IND_VOXEL)
  );

type
  wadinfo_t = packed record
    // Should be "IWAD" or "PWAD".
    identification: integer;
    numlumps: integer;
    infotableofs: integer;
  end;
  Pwadinfo_t = ^wadinfo_t;

  filelump_t = packed record
    filepos: integer;
    size: integer;
    name: char8_t;
  end;
  Pfilelump_t = ^filelump_t;
  Tfilelump_tArray = packed array[0..$FFFF] of filelump_t;
  Pfilelump_tArray = ^Tfilelump_tArray;

//
// WADFILE I/O related stuff.
//
  lumpinfo_t = record
    handle: TCachedFile;
    position: integer;
    size: integer;
    flags: LongWord;
    case integer of
      0: (name: char8_t);
      1: (v1, v2: integer);
    end;

  Plumpinfo_t = ^lumpinfo_t;
  lumpinfo_tArray = array[0..$FFFF] of lumpinfo_t;
  Plumpinfo_tArray = ^lumpinfo_tArray;

type
  name8_t = record
    case integer of
      0: (s: char8_t);
      1: (x: array[0..1] of integer);
    end;

  PTransProcedure = procedure (const src: pointer; const size: integer; const dest: PPointer; const tag: integer);

function char8tostring(src: char8_t): string;

function stringtochar8(src: string): char8_t;

function W_InitMultipleFiles(const filenames: TDStringList): integer;

procedure W_ShutDown;

procedure W_Reload;

procedure W_RuntimeLoad(fname: string);

function W_NumLumps: integer;

function W_CheckNumForName(const name: string; const flags: LongWord = $0): integer;

function W_CheckNumForName2(const name: string; first: integer; last: integer): integer;

function W_GetNumForName(const name: string; const flags: LongWord = $0): integer;
function W_GetFirstNumForName(const name: string): integer;

function W_GetNameForNum(const lump: integer): char8_t;

function W_LumpLength(const lump: integer): integer;
procedure W_ReadLump(const lump: integer; dest: pointer);

function W_CacheLumpNum(const lump: integer; const tag: integer): pointer;
function W_CacheLumpNum2(const lump: integer; const tag: integer): pointer;
function W_CacheLumpName(const name: string; const tag: integer; const flags: LongWord = $0): pointer;
function W_TextLumpNum(const lump: integer): string;
function W_TextLumpName(const name: string): string;

procedure ExtractFileBase8(const path: string; var dest: char8_t);

var
  lumpinfo: Plumpinfo_tArray = nil;

implementation

uses
  i_system,
  z_zone;

var
  numlumps: integer;

const
  IWAD = integer(Ord('I') or
                (Ord('W') shl 8) or
                (Ord('A') shl 16) or
                (Ord('D') shl 24));

  PWAD = integer(Ord('P') or
                (Ord('W') shl 8) or
                (Ord('A') shl 16) or
                (Ord('D') shl 24));

// DelphiDoom specific
  DWAD = integer(Ord('D') or
                (Ord('W') shl 8) or
                (Ord('A') shl 16) or
                (Ord('D') shl 24));


const
  LUMPHASHBITS = 14;
  LUMPHASHSIZE = 1 shl LUMPHASHBITS;

type
  lumphash_t = record
    name: name8_t;
    position: integer;
  end;
  Plumphash_t = ^lumphash_t;
  lumphash_tArray = array[0..LUMPHASHSIZE - 1] of lumphash_t;
  Plumphash_tArray = ^lumphash_tArray;

var
  lumphash: Plumphash_tArray = nil;

// djb2Hash()
//
// Implements djb2 hash function
//
// This algorithm (k=33) was first reported by Dan Bernstein many years ago in comp.lang.c.
// Another version of this algorithm (now favored by bernstein) uses xor: hash(i) = hash(i - 1) * 33 ^ str[i];
// The magic of number 33 (why it works better than many other constants, prime or not)
// has never been adequately explained.
function djb2Hash(const name: char8_t): integer;
var
  b: PByte;
  i: integer;
begin
  b := PByte(@name[0]);

  result := 5381 * 33 + b^;
  inc(b);

  for i := 0 to 6 do
  begin
    if b^ = 0 then
      break;
    result := result * 33 + b^;
    inc(b);
  end;

  result := result and (LUMPHASHSIZE - 1);
end;

//
// GLOBALS
//

// Location of each lump on disk.
var
  lumpcache: PPointerArray;

function char8tostring(src: char8_t): string;
var
  i: integer;
begin
  result := '';
  i := 0;
  while (i < 8) and (src[i] <> #0) do
  begin
    result := result + src[i];
    inc(i);
  end;
end;

function stringtochar8(src: string): char8_t;
var
  i: integer;
  len: integer;
begin
  len := length(src);
  if len > 8 then
    I_Error('stringtochar8(): length of %s is > 8', [src]);

  i := 1;
  while (i <= len) do
  begin
    result[i - 1] := src[i];
    inc(i);
  end;

  for i := len to 7 do
    result[i] := #0;
end;

function filelength(handle: TCachedFile): integer;
begin
  try
    result := handle.Size;
  except
    result := 0;
    I_Error('filelength(): Error fstating');
  end;
end;

procedure ExtractFileBase(const path: string; var dest: string);
var
  i: integer;
  len: integer;
begin
  len := Length(path);
  i := len;
  while i > 0 do
  begin
    if path[i] in ['/', '\'] then
      break;
    dec(i);
  end;
  dest := '';
  while (i < len) do
  begin
    inc(i);
    if path[i] = '.' then
      break
    else
      dest := dest + toupper(path[i]);
  end;
  if Length(dest) > 8 then
    I_Error('ExtractFileBase(): Filename base of %s >8 chars', [path]);
end;

procedure ExtractFileBase8(const path: string; var dest: char8_t);
var
  dst: string;
begin
  dst := char8tostring(dest);
  ExtractFileBase(path, dst);
  dest := stringtochar8(dst);
end;

//
// LUMP BASED ROUTINES.
//

//
// W_AddFile
// All files are optional, but at least one file must be
//  found (PWAD, if all required lumps are present).
// Files with a .wad extension are wadlink files
//  with multiple lumps.
// Other files are single lumps with the base filename
//  for the lump name.
//
// If filename starts with a tilde, the file is handled
//  specially to allow map reloads.
// But: the reload feature is a fragile hack...
var
  reloadlump: integer;
  reloadname: string;

function W_AddFile(var filename: string): TStream;
var
  header: wadinfo_t;
  lump_p: Plumpinfo_t;
  i: integer;
  j: integer;
  handle: TCachedFile;
  len: integer;
  startlump: integer;
  fileinfo: Pfilelump_tArray;
  pfi: Pfilelump_t;
  singleinfo: filelump_t;
  storehandle: TCachedFile;
  ext: string;
  c: char;
begin
  // open the file and add to directory
  // handle reload indicator.
  if filename[1] = '~' then
  begin
    reloadname := Copy(filename, 2, Length(filename) - 1);
    Delete(filename, 1, 1);
    reloadlump := numlumps;
  end
  else
    reloadname := '';

  if not fexists(filename) then
  begin
    I_Warning('W_AddFile(): File %s does not exist' + #13#10, [filename]);
    result := nil;
    exit;
  end;

  try
    handle := TCachedFile.Create(filename, fOpenReadOnly);
  except
    I_Warning('W_AddFile(): couldn''t open %s' + #13#10, [filename]);
    result := nil;
    exit;
  end;

  result := handle;

  handle.OnBeginBusy := I_BeginDiskBusy;

  startlump := numlumps;

  ext := strupper(fext(filename));
  if (ext <> '.WAD') and (ext <> '.SWD') {$IFDEF OPENGL} and (ext <> '.GWA'){$ENDIF} then
  begin
    // single lump file
    len := 0;
    fileinfo := @singleinfo;
    singleinfo.filepos := 0;
    singleinfo.size := filelength(handle);
    ExtractFileBase8(filename, singleinfo.name);
    inc(numlumps);
    printf(' adding %s' + #13#10, [filename]);
  end
  else
  begin
    // WAD file
    handle.Read(header, SizeOf(header));
    if header.identification <> IWAD then
      // Homebrew levels?
      if header.identification <> PWAD then
        // DelphiDoom system ?
        if header.identification <> DWAD then
          I_Error('W_AddFile(): Wad file %s doesn''t have IWAD, PWAD or DWAD id' + #13#10, [filename]);

    len := header.numlumps * SizeOf(filelump_t);
    fileinfo := malloc(len);
    handle.Seek(header.infotableofs, sFromBeginning);
    handle.Read(fileinfo^, len);
    numlumps := numlumps + header.numlumps;
    printf(' adding %s (%d lumps)' + #13#10, [filename, header.numlumps]);
  end;


  // Fill in lumpinfo
  realloc(pointer(lumpinfo), startlump * SizeOf(lumpinfo_t), numlumps * SizeOf(lumpinfo_t));

  if lumpinfo = nil then
    I_Error('W_AddFile(): Couldn''t realloc lumpinfo');

  if reloadname <> '' then
    storehandle := nil
  else
    storehandle := handle;

  for i := startlump to numlumps - 1 do
  begin
    lump_p := @lumpinfo[i];
    lump_p.handle := storehandle;

    pfi := @fileinfo[i - startlump];
    lump_p.position := pfi.filepos;
    lump_p.size := pfi.size;
    c := #255;
    for j := 0 to 7 do
    begin
      // Prevent non null charactes after ending #0
      if c <> #0 then
        c := toupper(pfi.name[j]);
      lump_p.name[j] := c;
    end;
  end;

  if reloadname <> '' then
    handle.Free;

  if len > 0 then
    memfree(pointer(fileinfo), len);

end;

procedure W_InitLumpHash;
var
  i, j: integer;
  hash: integer;
  lumphit: integer;
  ind_A: array[0..IND_MAX - 1] of integer;

  procedure _check_indicator;
  var
    x: integer;
  begin
    for x := 0 to NUMINDICATORS - 1 do
    begin
      if char8tostring(lumpinfo[i].name) = char8tostring(lumpindicators[x]._START) then
      begin
        dec(ind_A[lumpindicators[x]._type]);
        if ind_A[lumpindicators[x]._type] < 0 then
          I_Warning('W_InitLumpHash(): Lump indicators misplaced, lump #%d (%s)'#13#10, [i, char8tostring(lumpindicators[x]._START)]);
        Break;
      end;
      if char8tostring(lumpinfo[i].name) = char8tostring(lumpindicators[x]._END) then
      begin
        inc(ind_A[lumpindicators[x]._type]);
        Break;
      end;
    end;
  end;


begin
  ZeroMemory(lumphash, SizeOf(lumphash_tArray));
  ZeroMemory(@ind_A, SizeOf(ind_A));
  for i := 0 to LUMPHASHSIZE - 1 do
    lumphash[i].position := -1;

  lumphit := 0;
  for i := numlumps - 1 downto 0 do
  begin
    _check_indicator;
    lumpinfo[i].flags := 0;
    for j := 0 to IND_MAX - 1 do
      if ind_A[j] > 0 then
        lumpinfo[i].flags := lumpinfo[i].flags or (1 shl j);
    hash := djb2Hash(lumpinfo[i].name);
    if lumphash[hash].position = -1 then
    begin
      inc(lumphit);
      lumphash[hash].position := i;
      lumphash[hash].name.x[0] := lumpinfo[i].v1;
      lumphash[hash].name.x[1] := lumpinfo[i].v2;
    end
  end;
  printf('W_InitLumpHash: Hash table indexed %d out of %d lumps.'#13#10, [lumphit, numlumps]);
end;

//
// W_Reload
// Flushes any of the reloadable lumps in memory
//  and reloads the directory.
//
procedure W_Reload;
var
  header: wadinfo_t;
  lumpcount: integer;
  lump_p: Plumpinfo_t;
  i: integer;
  handle: TCachedFile;
  length: integer;
  fileinfo: Pfilelump_tArray;
begin
  if reloadname = '' then
    exit;

  if not fexists(reloadname) then
    I_Error('W_Reload(): File %s does not exist' + #13#10, [reloadname]);

  handle := nil;
  try
    handle := TCachedFile.Create(reloadname, fOpenReadOnly);
  except
    handle.Free;
    I_Error('W_Reload(): couldn''t open %s', [reloadname]);
  end;

  handle.OnBeginBusy := I_BeginDiskBusy;

  handle.Read(header, SizeOf(header));
  lumpcount := header.numlumps;
  length := lumpcount * SizeOf(filelump_t);
  fileinfo := malloc(length);
  handle.Seek(header.infotableofs, sFromBeginning);
  handle.Read(fileinfo^, length);

  // Fill in lumpinfo

  for i := reloadlump to reloadlump + lumpcount - 1 do
  begin
    lump_p := @lumpinfo[i];
    if lumpcache[i] <> nil then
      Z_Free(lumpcache[i]);                   

    lump_p.position := fileinfo[i - reloadlump].filepos;
    lump_p.size := fileinfo[i - reloadlump].size;
  end;

  handle.Free;

  W_InitLumpHash;
end;

var
  rtllist: TDStringList = nil; // jval: Runtime wads

type
  rtlinfo_t = class
    filename: string;
    startlump: integer;
    numlumps: integer;
    stream: TCachedFile;
    constructor Create(const aname: string); virtual;
    destructor Destroy; override;
  end;

constructor rtlinfo_t.Create(const aname: string);
begin
  filename := aname;
  startlump := -1;
  numlumps := -1;
  stream := nil;
end;

destructor rtlinfo_t.Destroy;
begin
  stream.Free;
  inherited;
end;

procedure W_RuntimeLoad(fname: string);
var
  header: wadinfo_t;
  lumpcount: integer;
  lump_p: Plumpinfo_t;
  i: integer;
  handle: TCachedFile;
  length: integer;
  fileinfo: Pfilelump_tArray;
  idx: integer;
  rtlinf: rtlinfo_t;
  start, finish: integer;
begin
  printf('W_RuntimeLoad(): adding %s' + #13#10, [fname]);

  fname := strupper(fname);

  if not fexists(fname) then
  begin
    I_Warning('W_RuntimeLoad(): File %s does not exist'#13#10, [fname]);
    exit;
  end;

  if rtllist = nil then
    rtllist := TDStringList.Create;

  handle := nil;
  try
    handle := TCachedFile.Create(fname, fOpenReadOnly);
  except
    handle.Free;
    I_Warning('W_RuntimeLoad(): couldn''t open %s'#13#10, [fname]);
    exit;
  end;

  idx := rtllist.IndexOf(fname);
  if idx < 0 then
  begin
    idx := rtllist.Add(fname);
    rtlinf := rtlinfo_t.Create(fname);
    rtllist.Objects[idx] := rtlinf;
  end
  else
    rtlinf := rtllist.Objects[idx] as rtlinfo_t;

  handle.OnBeginBusy := I_BeginDiskBusy;

  handle.Read(header, SizeOf(header));
  lumpcount := header.numlumps;
  length := lumpcount * SizeOf(filelump_t);
  fileinfo := malloc(length);
  handle.Seek(header.infotableofs, sFromBeginning);
  handle.Read(fileinfo^, length);

  if (rtlinf.startlump >= 0) and (rtlinf.numlumps = lumpcount) then
  begin
    start := rtlinf.startlump;
    finish := start + lumpcount - 1;
    for i := start to finish do
      lumpinfo[i].handle.Free;
  end
  else
  begin
    // Resize lumpinfo
    realloc(pointer(lumpinfo), numlumps * SizeOf(lumpinfo_t), (numlumps + lumpcount) * SizeOf(lumpinfo_t));
    realloc(pointer(lumpcache), numlumps * SizeOf(pointer), (numlumps + lumpcount) * SizeOf(pointer));

    start := numlumps;
    finish := start + lumpcount - 1;
    inc(numlumps, lumpcount);

    rtlinf.startlump := start;
    rtlinf.numlumps := lumpcount;
  end;
  rtlinf.stream.Free;
  rtlinf.stream := handle;


  // Fill in lumpinfo

  for i := start to finish do
  begin
    lump_p := @lumpinfo[i];
    lump_p.handle := handle;
    if lumpcache[i] <> nil then
      Z_Free(lumpcache[i]);

    lump_p.position := fileinfo[i - start].filepos;
    lump_p.size := fileinfo[i - start].size;
    lump_p.name := fileinfo[i - start].name;
  end;

  W_InitLumpHash;

end;

//
// W_InitMultipleFiles
// Pass a null terminated list of files to use.
// All files are optional, but at least one file
//  must be found.
// Files with a .wad extension are idlink files
//  with multiple lumps.
// Other files are single lumps with the base filename
//  for the lump name.
// Lump names can appear multiple times.
// The name searcher looks backwards, so a later file
//  does override all earlier ones.
//
function W_InitMultipleFiles(const filenames: TDStringList): integer;
var
  size: integer;
  filename: string;
  i: integer;
begin
  if rtllist <> nil then
    rtllist := TDStringList.Create;
  // open all the files, load headers, and count lumps
  numlumps := 0;

  for i := 0 to filenames.Count - 1 do
  begin
    filename := filenames[i];
    filenames.Objects[i] := W_AddFile(filename);
    filenames[i] := filename;
  end;

  result := numlumps;

  if result > 0 then
  begin
    // set up caching
    size := numlumps * SizeOf(pointer);
    lumpcache := mallocz(size);

    if lumpcache = nil then
      I_Error('W_InitMultipleFiles(): Couldn''t allocate lumpcache');

  end;

  lumphash := malloc(SizeOf(lumphash_tArray));

  W_InitLumpHash;
end;

procedure W_ShutDown;
var
  i: integer;
begin
  memfree(pointer(lumpcache), numlumps * SizeOf(pointer));
  memfree(pointer(lumpinfo), numlumps * SizeOf(lumpinfo_t));
  memfree(pointer(lumphash), SizeOf(lumphash_tArray));
  if rtllist <> nil then
  begin
    for i := 0 to rtllist.Count - 1 do
      rtllist.Objects[i].Free;
    rtllist.Free;
  end;
end;

//
// W_InitFile
// Just initialize from a single file.
//
procedure W_InitFile(const filename: string);
var
  names: TDStringList;
begin
  names := TDSTringList.Create;
  try
    names.Add(filename);
    W_InitMultipleFiles(names);
  finally
    if names.Objects[0] <> nil then
      names.Objects[0].Free;
    names.Free;
  end;
end;

//
// W_NumLumps
//
function W_NumLumps: integer;
begin
  result := numlumps;
end;

//
// W_CheckNumForName
// Returns -1 if name not found.
//
function W_CheckNumForName(const name: string; const flags: LongWord = $0): integer;
var
  name8: name8_t;
  v1: integer;
  v2: integer;
  lump_p: Plumpinfo_t;
  len: integer;
  i: integer;
  lfirst: Plumpinfo_t;
  hash: integer;
begin
  len := Length(name);
  if len > 8 then // JVAL: Original an I_Error ocurred. Now we call I_DevError
  begin
    I_DevError('W_CheckNumForName(): name string has more than 8 characters: %s'#13#10, [name]);
    len := 8;
  end;

  // make the name into two integers for easy compares
  for i := 1 to len do
    name8.s[i - 1] := toupper(name[i]); // case insensitive
  for i := len to 7 do
    name8.s[i] := #0;

  v1 := name8.x[0];
  v2 := name8.x[1];

  // JVAL
  // Hash hit factor is at about 80% for standard WADs, good
  hash := djb2Hash(name8.s);
  if (lumphash[hash].name.x[0] = v1) and
     (lumphash[hash].name.x[1] = v2) then
    if (flags = 0) or (lumpinfo[lumphash[hash].position].flags and flags <> 0) then
    begin
      result := lumphash[hash].position;
      exit;
    end;

  // JVAL: If hash position is -1 then the lump does not exist!
  result := lumphash[hash].position;
  if result = -1 then
    exit;

  // scan backwards so patch lump files take precedence
  lfirst := @lumpinfo[0];
  // JVAL: Check only the smaller lumps (see W_InitLumpHash)
  lump_p := @lumpinfo[result];


  while integer(lump_p) > integer(lfirst) do
  begin
    dec(lump_p);
    if (lump_p.v1 = v1) and (lump_p.v2 = v2) then
      if (flags = 0) or (lump_p.flags or flags <> 0) then
      begin
        result := (integer(lump_p) - integer(lumpinfo)) div SizeOf(lumpinfo_t);
        exit;
      end;
  end;

  // TFB. Not found.
  result := -1;
end;

function W_CheckFirstNumForName(const name: string): integer;
var
  name8: name8_t;
  v1: integer;
  v2: integer;
  lump_p: Plumpinfo_t;
  len: integer;
  i: integer;
  llast: Plumpinfo_t;
begin
  len := Length(name);
  if len > 8 then // JVAL: Original an I_Error ocurred. Now we call I_DevError
  begin
    I_DevError('W_CheckNumForName(): name string has more than 8 characters: %s'#13#10, [name]);
    len := 8;
  end;

  // make the name into two integers for easy compares
  for i := 1 to len do
    name8.s[i - 1] := toupper(name[i]); // case insensitive
  for i := len to 7 do
    name8.s[i] := #0;

  v1 := name8.x[0];
  v2 := name8.x[1];

  // scan forward
  llast := @lumpinfo[numlumps];
  lump_p := @lumpinfo[0];

  while integer(lump_p) <> integer(llast) do
  begin
    if (lump_p.v1 = v1) and (lump_p.v2 = v2) then
    begin
      result := (integer(lump_p) - integer(lumpinfo)) div SizeOf(lumpinfo_t);
      exit;
    end;
    inc(lump_p);
  end;

  // TFB. Not found.
  result := -1;
end;

function W_CheckNumForName2(const name: string; first: integer; last: integer): integer;
var
  name8: name8_t;
  v1: integer;
  v2: integer;
  lump_p: Plumpinfo_t;
  len: integer;
  i: integer;
  lfirst: Plumpinfo_t;
  hash: integer;
begin
  len := Length(name);
  if len > 8 then // JVAL: Original an I_Error ocurred. Now we call I_DevError
  begin
    I_DevError('W_CheckNumForName(): name string has more than 8 characters: %s'#13#10, [name]);
    len := 8;
  end;

  // make the name into two integers for easy compares
  for i := 1 to len do
    name8.s[i - 1] := toupper(name[i]); // case insensitive
  for i := len to 7 do
    name8.s[i] := #0;

  v1 := name8.x[0];
  v2 := name8.x[1];

  hash := djb2Hash(name8.s);
  if (lumphash[hash].name.x[0] = v1) and
     (lumphash[hash].name.x[1] = v2) then
  begin
    result := lumphash[hash].position;
    if (result >= first) and (result <= last) then
      exit;
    if result < first then
    begin
    // JVAL: Hash table returns the higher value of lump position
      result := -1;
      exit;
    end;
  end;

  // scan backwards so patch lump files take precedence
  lfirst := @lumpinfo[first];
  lump_p := @lumpinfo[last + 1];

  while integer(lump_p) <> integer(lfirst) do
  begin
    dec(lump_p);
    if (lump_p.v1 = v1) and (lump_p.v2 = v2) then
    begin
      result := (integer(lump_p) - integer(lumpinfo)) div SizeOf(lumpinfo_t);
      exit;
    end;
  end;

  // TFB. Not found.
  result := -1;
end;

//
// W_GetNumForName
// Calls W_CheckNumForName, but bombs out if not found.
//
function W_GetNumForName(const name: string; const flags: LongWord = $0): integer;
begin
  result := W_CheckNumForName(name, flags);
  if result = -1 then
    I_Error('W_GetNumForName(): %s not found!', [name]);
end;

function W_GetFirstNumForName(const name: string): integer;
begin
  result := W_CheckFirstNumForName(name);
  if result = -1 then
    I_Error('W_GetNumForName(): %s not found!', [name]);
end;

function W_GetNameForNum(const lump: integer): char8_t;
begin
  result := lumpinfo[lump].name;
end;

//
// W_LumpLength
// Returns the buffer size needed to load the given lump.
//
function W_LumpLength(const lump: integer): integer;
begin
  if lump >= numlumps then
    I_Error('W_LumpLength(): %d >= numlumps', [lump]);

  result := lumpinfo[lump].size;
end;

//
// W_ReadLump
// Loads the lump into the given buffer,
//  which must be >= W_LumpLength().
//
procedure W_ReadLump(const lump: integer; dest: pointer);
var
  c: integer;
  l: Plumpinfo_t;
  handle: TCachedFile;
begin
  if lump >= numlumps then
    I_Error('W_ReadLump(): %d >= numlumps', [lump]);

  l := @lumpinfo[lump];

  if l.handle = nil then
  begin
    // reloadable file, so use open / read / close
    if not fexists(reloadname) then
      I_Error('W_ReadLump(): couldn''t open %s', [reloadname]);

    try
      handle := TCachedFile.Create(reloadname, fOpenReadOnly);
    except
      handle := nil;
      I_Error('W_ReadLump(): couldn''t open %s', [reloadname]);
    end
  end
  else
    handle := l.handle;

  handle.OnBeginBusy := I_BeginDiskBusy;

  handle.Seek(l.position, sFromBeginning);
  c := handle.Read(dest^, l.size);

  if c < l.size then
    I_Error('W_ReadLump(): only read %d of %d on lump %d', [c, l.size, lump]);

  if l.handle = nil then
    handle.Free;

end;

//
// W_CacheLumpNum
//
function W_CacheLumpNum(const lump: integer; const tag: integer): pointer;
begin
  if lump >= numlumps then
    I_Error('W_CacheLumpNum(): lump = %d, >= numlumps', [lump]);

  if lump < 0 then
    I_Error('W_CacheLumpNum(): lump = %d, < 0', [lump]);

  if lumpcache[lump] = nil then
  begin
    // read the lump in
    //printf ("cache miss on lump %i\n",lump);
    Z_Malloc(W_LumpLength(lump), tag, @lumpcache[lump]);
    W_ReadLump(lump, lumpcache[lump]);
  end
  else
  begin
    //printf ("cache hit on lump %i\n",lump);
    Z_ChangeTag(lumpcache[lump], tag);
  end;

  result := lumpcache[lump];
end;

function W_CacheLumpNum2(const lump: integer; const tag: integer): pointer;
begin
  if lumpcache[lump] = nil then
  begin
    // read the lump in
    //printf ("cache miss on lump %i\n",lump);
    Z_Malloc(W_LumpLength(lump), tag, @lumpcache[lump]);
    W_ReadLump(lump, lumpcache[lump]);
  end
  else
  begin
    //printf ("cache hit on lump %i\n",lump);
    Z_ChangeTag(lumpcache[lump], tag);
  end;

  result := lumpcache[lump];
end;

//
// W_CacheLumpName
//
function W_CacheLumpName(const name: string; const tag: integer; const flags: LongWord = $0): pointer;
begin
  result := W_CacheLumpNum(W_GetNumForName(name, flags), tag);
end;

function W_TextLumpNum(const lump: integer): string;
var
  p: PByteArray;
  i: integer;
  len: integer;
begin
  if lump < 0 then
  begin
    result := '';
    exit;
  end;

  p := W_CacheLumpNum(lump, PU_STATIC);
  len := W_LumpLength(lump);

  result := '';
  SetLength(result, len);
  for i := 0 to len - 1 do
    if p[i] <> 0 then
      result[i + 1] := Chr(p[i])
    else
      result[i + 1] := ' ';

  Z_Free(p);

end;

function W_TextLumpName(const name: string): string;
begin
  result := W_TextLumpNum(W_GetNumForName(name));
end;

end.
