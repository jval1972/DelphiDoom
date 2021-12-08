unit Launcher_wads;

interface

uses
  classes;

procedure GetWadMapNames(const wadfile: string; List: TStringList);

procedure GetWadDemoNames(const wadfile: string; List: TStringList);

function GetMapNameInfo(const mapname: string; var episode, map: integer): boolean;

function IsIWAD(const fname: string): boolean;

function IsPWAD(const fname: string): boolean;

function IsPak(const fname: string): boolean;

implementation

uses
  SysUtils;

type
  char8_t = array[0..7] of char;

  filelump_t = packed record
    filepos: integer;
    size: integer;
    name: char8_t;
  end;
  Pfilelump_t = ^filelump_t;
  Tfilelump_tArray = packed array[0..$FFFF] of filelump_t;
  Pfilelump_tArray = ^Tfilelump_tArray;

function LumpName(const lump: filelump_t): string;
var
  i: integer;
begin
  result := '';
  i := 0;
  while i < 8 do
  begin
    if lump.name[i] = #0 then
      exit
    else
    begin
      result := result + UpCase(lump.name[i]);
      inc(i);
    end;
  end;
end;

procedure GetWadMapNames(const wadfile: string; List: TStringList);
var
  i: integer;
  f: TFileStream;
  lumps: Pfilelump_tArray;
  numlumps: integer;
  start: integer;
  stmp, stmp2: string;
begin
  List.Clear;
  if not FileExists(wadfile) then
    exit;

  lumps := nil;
  numlumps := 0;

  f := TFileStream.Create(wadfile, fmOpenRead or fmShareDenyWrite);
  try
    f.Read(i, SizeOf(i)); // IWAD/PWAD id
    i := 0;
    f.Read(numlumps, SizeOf(numlumps));  // numlumps
    reallocmem(lumps, numlumps * SizeOf(filelump_t));
    f.Read(start, SizeOf(start)); // start of lump directory

    f.Position := start;

    f.Read(lumps^, numlumps * SizeOf(filelump_t));

    for i := 0 to numlumps - 1 do
    begin
      stmp := LumpName(lumps[i]);
      if Length(stmp) = 4 then
      begin
        if (stmp[1] = 'E') and (stmp[3] = 'M') then
          if (stmp[2] in ['1', '2', '3', '4', '5', '6', '7', '8', '9']) and (stmp[4] in ['1', '2', '3', '4', '5', '6', '7', '8', '9']) then
            List.Add(stmp)
      end
      else if Length(stmp) = 5 then
      begin
        if (stmp[1] = 'M') and (stmp[2] = 'A') and (stmp[3] = 'P') then
        begin
          stmp2 := stmp[4] + stmp[5];
          if (stmp2[1] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) and (stmp2[2] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) then
            List.Add(stmp)
        end;
      end;
    end;

  finally
    reallocmem(lumps, 0);
    f.Free;
  end;

  List.Sort;
end;

procedure GetWadDemoNames(const wadfile: string; List: TStringList);
var
  i: integer;
  f: TFileStream;
  lumps: Pfilelump_tArray;
  numlumps: integer;
  start: integer;
  stmp: string;
begin
  List.Clear;

  if not FileExists(wadfile) then
    exit;

  lumps := nil;
  numlumps := 0;

  f := TFileStream.Create(wadfile, fmOpenRead or fmShareDenyWrite);
  try
    f.Read(i, SizeOf(i)); // IWAD/PWAD id
    i := 0;
    f.Read(numlumps, SizeOf(numlumps));  // numlumps
    reallocmem(lumps, numlumps * SizeOf(filelump_t));
    f.Read(start, SizeOf(start)); // start of lump directory

    f.Position := start;

    f.Read(lumps^, numlumps * SizeOf(filelump_t));

    for i := 0 to numlumps - 1 do
    begin
      stmp := LumpName(lumps[i]);
      if Length(stmp) = 5 then
      begin
        if (stmp[1] = 'D') and (stmp[2] = 'E') and
           (stmp[3] = 'M') and (stmp[4] = 'O') and
           (stmp[5] in ['1', '2', '3', '4', '5', '6', '7', '8', '9']) then
            List.Add(stmp)
      end
    end;

  finally
    reallocmem(lumps, 0);
    f.Free;
  end;

  List.Sort;
end;

function GetMapNameInfo(const mapname: string; var episode, map: integer): boolean;
var
  stmp, stmp2: string;
begin
  result := false;
  stmp := UpperCase(mapname);
  if Length(stmp) = 4 then
  begin
    if (stmp[1] = 'E') and (stmp[3] = 'M') then
      if (stmp[2] in ['1', '2', '3', '4', '5', '6', '7', '8', '9']) and (stmp[4] in ['1', '2', '3', '4', '5', '6', '7', '8', '9']) then
      begin
        episode := StrToInt(stmp[2]);
        map := StrToInt(stmp[4]);
        result := true;
        exit;
      end;
  end
  else if Length(stmp) = 5 then
  begin
    if (stmp[1] = 'M') and (stmp[2] = 'A') and (stmp[3] = 'P') then
    begin
      stmp2 := stmp[4] + stmp[5];
      if (stmp2[1] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) and (stmp2[2] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) then
      begin
        episode := 0;
        map := StrToInt(stmp2);
        result := true;
        exit;
      end;
    end;
  end;
end;

const
  IWAD = integer(Ord('I') or
                (Ord('W') shl 8) or
                (Ord('A') shl 16) or
                (Ord('D') shl 24));

  PWAD = integer(Ord('P') or
                (Ord('W') shl 8) or
                (Ord('A') shl 16) or
                (Ord('D') shl 24));

function IsIWAD(const fname: string): boolean;
var
  f: TFileStream;
  id: integer;
begin
  result := false;
  f := TFileStream.Create(fname, fmOpenRead or fmShareDenyNone);
  try
    f.Read(id, 4);
    if id = IWAD then
      result := true;
  finally
    f.Free;
  end;
end;

function IsPWAD(const fname: string): boolean;
var
  f: TFileStream;
  id: integer;
begin
  result := false;
  f := TFileStream.Create(fname, fmOpenRead or fmShareDenyNone);
  try
    f.Read(id, 4);
    if id = PWAD then
      result := true;
  finally
    f.Free;
  end;
end;


const
  Pakid: Longint = $4B434150;   // 'PACK' In Hex!
  WAD2id: LongInt = $32444157;  // 'WAD2' in Hex!
  WAD3id: LongInt = $33444157;  // 'WAD3' in Hex!
  ZIPFILESIGNATURE = $04034b50;

function IsPak(const fname: string): boolean;
var
  f: TFileStream;
  id: integer;
begin
  result := false;
  f := TFileStream.Create(fname, fmOpenRead or fmShareDenyNone);
  try
    f.Read(id, 4);
    if (id = Pakid) or (id = WAD2id) or (id = WAD3id) or (id = ZIPFILESIGNATURE) then
      result := true;
  finally
    f.Free;
  end;
end;



end.
