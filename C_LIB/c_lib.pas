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

unit c_lib;

{$Z+}
{$H+}

interface

type
  PPointer = ^Pointer;

function sprintfsec(buffer: Pointer; format: Pointer; arguments: Pointer): Integer;

function _malloc(Size: Cardinal): Pointer; cdecl;

function _calloc(N: integer; Size: Cardinal): Pointer; cdecl;

function _realloc(p: pointer; Size: Cardinal): Pointer; cdecl;

procedure _free(p: Pointer); cdecl;

function _strcspn(s1, s2: PChar): Cardinal; cdecl;

function _strchr(const S: PChar; C: Integer): PChar; cdecl;

function _strlen(const Str: PChar): Cardinal; cdecl;

function _strncmp(S1, S2: PChar; MaxLen: Cardinal): Integer; cdecl;

function _strnicmp(S1, S2: PChar; MaxLen: Cardinal): Integer; cdecl;

function _strtod(str: PChar; endptr: PPointer): Double; cdecl;

function _atof(const str: PChar): Double; cdecl;

function __ftol: Integer; cdecl;

function __ftoul: Int64; cdecl;

procedure __llmod; cdecl;

procedure __lldiv; cdecl;

procedure __llmul; cdecl;

procedure __llushr; cdecl;

procedure __llshr; cdecl;

procedure __llshl; cdecl;

procedure __lludiv; cdecl;

procedure __llumod; cdecl;

function _memcpy(dest: Pointer; const src: Pointer; count: Cardinal): Pointer; cdecl;

function _memcmp(ptr1, ptr2: Pointer; count: Cardinal): Integer; cdecl;

function _memchr(const p: Pointer; value: integer; num: integer): Pointer; cdecl;

procedure _memset(a: Pointer; b: Integer; c: Cardinal); cdecl;

function _isprint(c: Integer): Integer; cdecl;

function _sprintf(buffer: Pointer; format: Pointer; arguments: Pointer): Integer; cdecl;

function _snprintf(buffer: Pointer; sz: LongWord; format: Pointer; arguments: Pointer): Integer; cdecl;

function _vsnprintf(buffer: Pointer; sz: LongWord; format: Pointer; arguments: Pointer): Integer; cdecl;

function _fprintf(stream: Pointer; format: Pointer; arguments: Pointer): Integer; cdecl;

function _printf(format: Pointer; arguments: Pointer): Integer; cdecl;

function _ceil(x: Double): Double; cdecl;

function _lround(x: Double): Integer; cdecl;

function _mylround(x: Double): Integer; cdecl;

function _floor(x: Double): Double; cdecl;

function _rint(x: Double): Integer; cdecl;

function _pow(x: Double; y: Double): Double; cdecl;

function _sqrt(x: Double): Double; cdecl;

function _atan(x: Double): Double; cdecl;

function _acos(x: Double): Double; cdecl;

function _asin(x: Double): Double; cdecl;

function _atan2(y: Double; x: Double): Double; cdecl;

function _cos(x: Double): Double; cdecl;

function _sin(x: Double): Double; cdecl;

function _exp(x: Double): Double; cdecl;

function _log(x: Double): Double; cdecl;

function _fabs(x: Double): Double; cdecl;

function _fabsf(x: Single): Single; cdecl;

function _frexp(arg: Double; var exp: Integer): Double; cdecl;

function _ldexp(arg: Double; exp: Integer): Double; cdecl;

function _fmod(numer, denom: double): double; cdecl;

function _lrintf(x: single): integer; cdecl;

function _lrint(x: double): integer; cdecl;

function _rand: Integer; cdecl;

function _strcmp(a: Pointer; b: Pointer): Integer; cdecl;

function _strncat(destination: PAnsiChar; const source: PAnsiChar; const num: Integer): PAnsiChar; cdecl;

function _strncpy(destination: PAnsiChar; const source: PAnsiChar; const num: Integer): PAnsiChar; cdecl;

function _strtol(const str: PChar; p: pointer; base: integer): Integer; cdecl;

function _strstr(const str1: PAnsiChar; const str2: PAnsiChar): PAnsiChar; cdecl;

function _strrchr(const str1: PAnsiChar; ch: integer): PAnsiChar; cdecl;

function _memmove(dest: Pointer; src: Pointer; n: Cardinal): Pointer; cdecl;

function _remove(fname: PChar): integer; cdecl;

function _time(t: PInteger): Integer; cdecl;

function _getenv(const env: PAnsiChar): PAnsiChar; cdecl;

function _fileopenw(fname: PAnsiChar): Integer; cdecl;

function _fileopena(fname: PAnsiChar): Integer; cdecl;

function _fileopenr(fname: PAnsiChar): Integer; cdecl;

function _fileunlink(fname: PAnsiChar): Integer; cdecl;

function _fileseek(stream: Integer; offset, origin: Integer): Integer; cdecl;

function _fileseek64(stream: Integer; offset: int64; origin: Integer): Integer; cdecl;

function __fseeki64(stream: Integer; offset: int64; origin: Integer): Integer; cdecl;

function __ftelli64(stream: Integer): int64; cdecl;

function _fileread(ptr: pointer; size, count: Integer; stream: Integer): Integer; cdecl;

function _filewrite(ptr: pointer; size, count: Integer; stream: Integer): Integer; cdecl;

function _fileerror: Integer; cdecl;

function _fileclose(stream: Integer): Integer; cdecl;

function _filetruncate(stream: Integer; len: Integer): Integer; cdecl;

function _filesize(stream: Integer): Integer; cdecl;

function _filetell(stream: Integer): Integer; cdecl;

function _filetell64(stream: Integer): Int64; cdecl;

function _fileeof(stream: Integer): Integer; cdecl;

function _filepos(stream: Integer): Integer; cdecl;

function _filerename(fin: PChar; fout: PChar): Integer; cdecl;

function _sscanf(Str: PChar; Format: PChar; Pointers: array of Pointer): Integer; cdecl;

function ___errno: Integer; cdecl;

procedure _myexit(funcname: PChar; code: Integer); cdecl;

procedure _myassert(cond: Integer); cdecl;

procedure _exit(ecode: Integer); cdecl;

function _stat(path: PChar; structstat: Pointer): Integer; cdecl;

function _alloca(size: integer): Pointer; cdecl;

function __setargv__(argv: Pointer; argc: Pointer): Integer; cdecl;

procedure _mybreakpoint(s: PChar); cdecl;

function _math_isnormal(x: double): integer; cdecl;

function _math_isfinite(x: double): integer; cdecl;

var
  __turboFloat: LongBool = False;
  __streams: Integer;

procedure _myqsort; external;
procedure _mystrcpy; external;
procedure _mystrcat; external;
procedure _mystrdup; external;

implementation

uses
  Windows, SysUtils, Math, DateUtils, scanf;

function sprintfsec(buffer: Pointer; format: Pointer; arguments: Pointer): Integer;
var
  Modifier: Integer;
  Width: Integer;
  m,ma: PByte;
  mb: Boolean;
  n: PByte;
  o: PByte;
  r: PByte;

  procedure Append(const p: String);
  var
    q: Integer;
  begin
    if Width > Length(p) then
    begin
      if buffer <> nil then
      begin
        for q := 0 to Width - Length(p) - 1 do
        begin
          o^ := Ord('0');
          Inc(o);
        end;
      end
      else
        Inc(o, Width - Length(p));
    end;
    if buffer <> nil then CopyMemory(o, PChar(p), Length(p));
    Inc(o, Length(p));
  end;

begin
  m := format;
  n := arguments;
  o := buffer;
  while True do
  begin
    if m^ = 0 then break;
    if m^ = Ord('%') then
    begin
      ma := m;
      mb := True;
      Inc(m);
      Width := -1;
      Modifier:=0;
      {flags}
      case m^ of
        Ord('-'): mb := False;
        Ord('+'): mb := False;
        Ord(' '): mb := False;
        Ord('#'): mb := False;
      end;
      if mb then
      begin
        {width}
        case m^ of
          Ord('1')..Ord('9'):
          begin
            Width := 0;
            while True do
            begin
              if (m^ < Ord('0')) or (Ord('9') < m^) then break;
              Width := Width * 10 + m^ - Ord('0');
              Inc(m);
            end;
          end;
          Ord('0'): mb := False;
          Ord('*'): mb := False;
        end;
      end;
      if mb then
      begin
        {prec}
        case m^ of
          Ord('.'): mb := False;
        end;
      end;
      if mb then
      begin
        {modifier}
        case m^ of
          Ord('F'): mb := False;
          Ord('N'): mb := False;
          Ord('h'): mb := False;
          Ord('l'):
          begin
            Modifier := 4;
            Inc(m);
          end;
          Ord('L'): mb := False;
        end;
      end;
      if mb then
      begin
        {type}
        case m^ of
          Ord('d'):
          begin
            case Modifier of
              0:
              begin
                Append(IntToStr(PInteger(n)^));
                Inc(m);
                Inc(n, SizeOf(Integer));
              end;
            else
              mb := False;
            end;
          end;
          Ord('i'): mb := False;
          Ord('o'): mb := False;
          Ord('u'):
          begin
            case Modifier of
              0, 4:
              begin
                Append(IntToStr(PCardinal(n)^));
                Inc(m);
                Inc(n, SizeOf(Cardinal));
              end;
            else
              mb := False;
            end;
          end;
          Ord('x'):
          begin
            case Modifier of
              0, 4:
              begin
                Append(IntToHex(PCardinal(n)^, 8));
                Inc(m);
                Inc(n, SizeOf(Cardinal));
              end;
            else
              mb := False;
            end;
          end;
          Ord('X'): mb := False;
          Ord('f'): mb := False;
          Ord('e'): mb := False;
          Ord('g'):
          begin
            case Modifier of
              0:
              begin
                Append(FloatToStr(PSingle(n)^));
                Inc(m);
                Inc(n,SizeOf(Single));
              end;
            else
              mb := False;
            end;
          end;
          Ord('E'): mb := False;
          Ord('G'): mb := False;
          Ord('c'): mb := False;
          Ord('s'):
          begin
            r := PPointer(n)^;
            while r^ <> 0 do
            begin
              if buffer <> nil then o^:=r^;
              Inc(o);
              Inc(r);
            end;
            Inc(n,SizeOf(Pointer));
            Inc(m);
          end;
          Ord('%'): mb := False;
          Ord('n'): mb := False;
          Ord('p'): mb := False;
        else
          raise Exception.Create('LibDelphi');
        end;
      end;
      if not mb then
      begin
        m := ma;
        if buffer <> nil then o^ := m^;
        Inc(o);
        Inc(m);
      end;
    end
    else if m^ = 10 then
    begin
      if buffer <> nil then o^ := 13;
      Inc(o);
      if buffer <> nil then o^ := 10;
      Inc(o);
      Inc(m);
    end
    else
    begin
      if buffer <> nil then o^ := m^;
      Inc(o);
      Inc(m);
    end;
  end;
  if buffer <> nil then o^ := 0;
  Inc(o);
  Result := (Cardinal(o) - Cardinal(buffer));
end;

function _malloc(Size: Cardinal): Pointer; cdecl;
var
  m: TMemoryManager;
begin
  GetMemoryManager(m);
  Result := m.GetMem(Size);
  ZeroMemory(Result, Size);
end;

function _calloc(N: integer; Size: Cardinal): Pointer; cdecl;
begin
  Result := _malloc(N * Size);
end;

function _realloc(p: pointer; Size: Cardinal): Pointer; cdecl;
var
  m: TMemoryManager;
begin
  GetMemoryManager(m);
  if p = nil then
    Result := _malloc(Size)
  else
    Result := m.ReallocMem(p, Size);
end;

procedure _free(p: Pointer); cdecl;
var
  m: TMemoryManager;
begin
  // For an ISO C compliant implementation it is ok to free a NULL pointer.
  if p <> nil then
  begin
    GetMemoryManager(m);
    m.FreeMem(p);
  end;
end;

function _strlen(const Str: PChar): Cardinal; cdecl;
begin
  Result := StrLen(Str);
end;

function _strcspn(s1, s2: PChar): Cardinal; cdecl;
var
  SrchS2: PChar;
begin
  Result := 0;
  while S1^ <> #0 do
  begin
    SrchS2 := S2;
    while SrchS2^ <> #0 do
    begin
      if S1^ = SrchS2^ then
        Exit;
      Inc(SrchS2);
    end;
    Inc(S1);
    Inc(Result);
  end;
end;

function _strchr(const S: PChar; C: Integer): PChar; cdecl;
begin
  Result := StrScan(S, Chr(C));
end;

function _strncmp(S1, S2: PChar; MaxLen: Cardinal): Integer; cdecl;
begin
  Result := StrLComp(S1, S2, MaxLen);
end;

function _strnicmp(S1, S2: PChar; MaxLen: Cardinal): Integer; cdecl;
begin
  Result := StrLIComp(S1, S2, MaxLen);
end;

function _strtod(str: PChar; endptr: PPointer): Double; cdecl;
var
  c: integer;
  s: string;
  p: PChar;
begin
  s := str;
  val(s, Result, c);
  if endptr <> nil then
  begin
    if c = 0 then
      c := Length(s);
    p := str;
    inc(p, c);
    endptr^ := p;
  end;
end;

function _atof(const str: PChar): Double; cdecl;
var
  pos_sep: integer;
  s: string;
begin
  s := str;
  s := Trim(s);
  if s = '' then
  begin
    Result := 0.0;
    Exit;
  end;

  pos_sep := Pos('.', s);
  if pos_sep = 0 then
     pos_sep := Pos(',', s);

  if pos_sep > 0 then
    s[pos_sep] := DecimalSeparator;
  Result := StrToFloat(s);
end;

procedure __llmod; cdecl;
asm
  jmp System.@_llmod;
end;

procedure __lldiv; cdecl;
asm
  jmp System.@_lldiv;
end;

procedure __llmul; cdecl;
asm
  jmp System.@_llmul;
end;

procedure __llushr; cdecl;
asm
  jmp System.@_llushr
end;

procedure __llshr; cdecl;
asm
  shrd    eax, edx, cl
  sar     edx, cl
  cmp     cl, 32
  jl      @@Done
  cmp     cl, 64
  jge     @@RetSign
  mov     eax, edx
  sar     edx, 31
  ret
@@RetSign:
  sar     edx, 31
  mov     eax, edx
@@Done:
end;

procedure __llshl; cdecl;
asm
  jmp System.@_llshl
end;

procedure __lludiv; cdecl;
asm
  PUSH EBP
  PUSH EDI
  PUSH ESI
  PUSH EBX
  MOV EBX, [ESP + 20]
  MOV ECX, [ESP + 24]
  OR ECX, ECX
  JNZ @Slow
  OR EDX, EDX
  JZ @Quick
  OR EBX, EBX
  JZ @Quick
  @Slow:
  MOV EBP, ECX
  MOV ECX, 64
  XOR EDI, EDI
  XOR ESI, ESI
  @Loop:
  SHL EAX, 1
  RCL EDX, 1
  RCL ESI, 1
  RCL EDI, 1
  CMP EDI, EBP
  JB @noSub
  JA @Sub
  CMP ESI, EBX
  JB @noSub
  @Sub:
  SUB ESI, EBX
  SBB EDI, EBP
  INC EAX
  @noSub:
  LOOP @Loop
  JMP @Exit
  @Quick:
  DIV EBX
  XOR EDX, EDX
  @Exit:
  POP EBX
  POP ESI
  POP EDI
  POP EBP
  RET 8
end;

procedure __llumod; cdecl;
asm
  PUSH EBP
  PUSH EDI
  PUSH ESI
  PUSH EBX
  MOV EBX, [ESP + 20]
  MOV ECX, [ESP + 24]
  OR ECX, ECX
  jnz @Slow
  OR EDX, EDX
  JZ @Quick
  OR EBX, EBX
  JZ @Quick
@Slow:
  MOV EBP, ECX
  MOV ECX, 64
  XOR EDI, EDI
  XOR ESI, ESI
@Loop:
  SHL EAX, 1
  RCL EDX, 1
  RCL ESI, 1
  RCL EDI, 1
  CMP EDI, EBP
  JB @noSub
  JA @Sub
  CMP ESI, EBX
  JB @noSub
@Sub:
  SUB ESI, EBX
  SBB EDI, EBP
  INC EAX
@noSub:
  LOOP @Loop
  MOV EAX, ESI
  MOV EDX, EDI
  JMP @Exit
@Quick:
  DIV EBX
  XCHG EAX, EDX
  XOR EDX, EDX
  @Exit:
  POP EBX
  POP ESI
  POP EDI
  POP EBP
  RET 8
end;

function __ftol: Integer; cdecl;
var
  f: double;
begin
  asm
    lea   eax, f              //  BC++ passes floats on the FPU stack
    fstp  qword ptr [eax]     //  Delphi passes floats on the CPU stack
  end;
  Result := Trunc(f);
end;

function __ftoul: Int64; cdecl;
// Borland C++ float to integer (Int64) conversion
asm
  jmp System.@Trunc  // FST(0) -> EDX:EAX, as expected by BCC32 compiler
end;

function _memcpy(dest: Pointer; const src: Pointer; count: Cardinal): Pointer; cdecl;
begin
  CopyMemory(dest, src, count);
  Result := dest;
end;

function _memcmp(ptr1, ptr2: Pointer; count: Cardinal): Integer; cdecl;
var
  b1, b2: PByte;
  i: integer;
begin
  b1 := ptr1;
  b2 := ptr2;
  for i := 0 to count - 1 do
  begin
    Result := b1^;
    Result := Result - b2^;
    if Result <> 0 then
      Exit;
    inc(b1);
    inc(b2);
  end;
  Result := 0;
end;

function _memchr(const p: Pointer; value: integer; num: integer): Pointer; cdecl;
var
  pc: PChar;
  c: char;
  i: integer;
begin
  pc := p;
  c := Chr(value);
  for i := 0 to num - 1 do
  begin
    if pc^ = c then
    begin
      Result := pc;
      Exit;
    end;
    inc(pc);
  end;
  Result := nil;
end;

procedure _memset(a: Pointer; b: Integer; c: Cardinal); cdecl;
begin
  FillMemory(a, c, b);
end;

function _isprint(c: Integer): Integer; cdecl;
begin
  if (c < 32) or (127 <= c) then
    Result := 0
  else
    Result := 1;
end;

function _sprintf(buffer: Pointer; format: Pointer; arguments: Pointer): Integer; cdecl;
begin
  Result := sprintfsec(buffer, format, @arguments);
end;

function _snprintf(buffer: Pointer; sz: LongWord; format: Pointer; arguments: Pointer): Integer; cdecl;
var
  m: Integer;
  n: Pointer;
  sz1: Integer;
begin
  m := sprintfsec(nil, format, @arguments);
  GetMem(n, m);
  sprintfsec(n, format, @arguments);
  sz1 := sz;
  if m < sz1 then
    sz1 := m;
  CopyMemory(buffer, n, sz1);
  FreeMem(n, m);
  Result := sz1;
end;

function _vsnprintf(buffer: Pointer; sz: LongWord; format: Pointer; arguments: Pointer): Integer; cdecl;
begin
  Result := _snprintf(buffer, sz, format, arguments);
end;

function _fprintf(stream: Pointer; format: Pointer; arguments: Pointer): Integer; cdecl;
var
  m: Integer;
  n: Pointer;
  o: Cardinal;
begin
  m := sprintfsec(nil, format, @arguments);
  GetMem(n, m);
  sprintfsec(n, format, @arguments);
  if WriteFile(Cardinal(stream), n^, Cardinal(m), o, nil) then
    Result := 1
  else
    Result := 0;
  FreeMem(n);
end;

function _printf(format: Pointer; arguments: Pointer): Integer; cdecl;
var
  m: Integer;
  n: PChar;
  s: string;
begin
  m := sprintfsec(nil, format, @arguments);
  GetMem(n, m + 1);
  sprintfsec(n, format, @arguments);
  n[m] := #0;
  s := n;
  if IsConsole then
    write(s);
  FreeMem(n);
  Result := m;
end;

function _ceil(x: Double): Double; cdecl;
begin
  Result := Ceil(x);
end;

function _lround(x: Double): Integer; cdecl;
begin
  Result := Round(x);
end;

function _mylround(x: Double): Integer; cdecl;
begin
  Result := Round(x);
end;

function _floor(x: Double): Double; cdecl;
begin
  Result := Trunc(x);
end;

function _rint(x: Double): Integer; cdecl;
begin
  Result := Trunc(x + 0.5);
end;

function _pow(x: Double; y: Double): Double; cdecl;
begin
  Result := Power(x, y);
end;

function _sqrt(x: Double): Double; cdecl;
begin
  Result := Sqrt(x);
end;

function _atan(x: Double): Double; cdecl;
begin
  Result := ArcTan(x);
end;

function _acos(x: Double): Double; cdecl;
begin
  Result := ArcCos(x);
end;

function _asin(x: Double): Double; cdecl;
begin
  Result := ArcSin(x);
end;

function _atan2(y: Double; x: Double): Double;
begin
  Result := ArcTan2(y, x);
end;

function _cos(x: Double): Double; cdecl;
begin
  Result := cos(x);
end;

function _sin(x: Double): Double; cdecl;
begin
  Result := sin(x);
end;

function _exp(x: Double): Double; cdecl;
begin
  Result := System.Exp(x);
end;

function _log(x: Double): Double; cdecl;
begin
  Result := Ln(x);
end;

function _fabs(x: Double): Double; cdecl;
begin
  if x >= 0 then
    Result := x
  else
    Result := -x;
end;

function _fabsf(x: Single): Single; cdecl;
begin
  if x >= 0 then
    Result := x
  else
    Result := -x;
end;

function _frexp(arg: Double; var exp: Integer): Double; cdecl;
var
  ret: extended;
begin
  FRexp(arg, ret, exp);
  Result := ret;
end;

function _ldexp(arg: Double; exp: Integer): Double; cdecl;
begin
  Result := arg * Power(2, exp);
end;

function _fmod(numer, denom: double): double; cdecl;
var
  d, n: double;
  i: integer;
begin
  d := numer / denom;
  if d < 0 then
    i := -trunc(-d)
  else
    i := trunc(d);
  n := i * denom;
  Result := numer - n;
end;

function _lrintf(x: single): integer; cdecl;
begin
  Result := Round(x);
end;

function _lrint(x: double): integer; cdecl;
begin
  Result := Round(x);
end;

function _rand: Integer; cdecl;
begin
  Result := Trunc(Random * ($7FFF + 1));
end;

function _strcmp(a: Pointer; b: Pointer): Integer; cdecl;
var
  ma, mb: PByte;
begin
  ma := a;
  mb := b;
  while True do
  begin
    if ma^ <> mb^ then
    begin
      if ma^ < mb^ then
        Result := -1
      else
        Result := 1;
      exit;
    end;
    if ma^ = 0 then
    begin
      Result := 0;
      exit;
    end;
    Inc(ma);
    Inc(mb);
  end;
  Result := 0;
end;

function _strncat(destination: PAnsiChar; const source: PAnsiChar; const num: Integer): PAnsiChar; cdecl;
var
  p, i: integer;
  d, s: PAnsiChar;
begin
  p := StrLen(destination);
  d := @destination[p];
  s := source;
  for i := 0 to num - 2 do
  begin
    if s^ <> #0 then
    begin
      d^ := s^;
      inc(d);
      inc(s);
    end
    else
    begin
      inc(d);
      Break;
    end;
  end;
  d^ := #0;
  Result := destination;
end;

function _strncpy(destination: PAnsiChar; const source: PAnsiChar; const num: Integer): PAnsiChar; cdecl;
var
  i: integer;
  d, s: PAnsiChar;
begin
  d := @destination[0];
  s := source;
  for i := 0 to num - 1 do
  begin
    d^ := s^;
    if s^ = #0 then
      break;
  end;
  Result := destination;
end;

function _strtol(const str: PChar; p: pointer; base: integer): Integer; cdecl;
var
  s: string;
  code: integer;
begin
  s := str;
  if Pos('0x', s) = 1 then
  begin
    s[1] := ' ';
    s[2] := '$';
    s := Trim(s);
  end;
  val(s, Result, code);
end;

function _strstr(const str1: PAnsiChar; const str2: PAnsiChar): PAnsiChar; cdecl;
var
  s1, s2: string;
  p: integer;
begin
  s1 := str1;
  s2 := str2;
  p := Pos(s2, s1);
  if p > 0 then
  begin
    Result := str1;
    inc(Result, p - 1);
  end
  else
    Result := nil;
end;

function _strrchr(const str1: PAnsiChar; ch: integer): PAnsiChar; cdecl;
var
  i: integer;
  c: char;
  s: string;
begin
  c := Chr(ch);
  s := str1;
  for i := Length(s) downto 1 do
    if s[i] = c then
    begin
      Result := str1;
      Inc(Result, i - 1);
      Exit;
    end;
  Result := nil;
end;

function _memmove(dest: Pointer; src: Pointer; n: Cardinal): Pointer; cdecl;
begin
  MoveMemory(dest, src, n);
  Result := dest;
end;

function _remove(fname: PChar): integer; cdecl;
var
  s: string;
begin
  s := fname;
  if DeleteFile(s) then
    Result := 1
  else
    Result := 0;
end;

function _time(t: PInteger): Integer; cdecl;
var
  fromDate, toDate: TDateTime;
begin
  fromDate := EncodeDateTime(1970, 1, 1, 0, 0, 0, 0);
  toDate := Now;

  Result := SecondsBetween(toDate, fromDate);
  if t <> nil then
    t^ := Result;
end;

var
  _getenvbuf: packed array[0..512] of char;

function _getenv(const env: PAnsiChar): PAnsiChar; cdecl;
begin
  ZeroMemory(@_getenvbuf, SizeOf(_getenvbuf));
  GetEnvironmentVariable(PChar(env), _getenvbuf, SizeOf(_getenvbuf));
  result := @_getenvbuf[0];
end;

function _fileopenw(fname: PAnsiChar): Integer; cdecl;
begin
  Result := FileCreate(fname);
  if Result < 1 then
    Result := 0;
end;

function _fileopena(fname: PAnsiChar): Integer; cdecl;
begin
  Result := FileCreate(fname);
  if Result < 1 then
    Result := 0;
  _fileseek(Result, 0, 2);
end;

function _fileopenr(fname: PAnsiChar): Integer; cdecl;
begin
  Result := FileOpen(fname, 2);
end;

function _fileunlink(fname: PAnsiChar): Integer; cdecl;
begin
  if DeleteFile(fname) then
    Result := 0
  else
    Result := -1;
end;

function _fileseek(stream: Integer; offset, origin: integer): Integer; cdecl;
begin
  Result := FileSeek(stream, offset, origin);
end;

function _fileseek64(stream: Integer; offset: int64; origin: Integer): Integer; cdecl;
begin
  Result := FileSeek(stream, offset, origin);
end;

function __fseeki64(stream: Integer; offset: int64; origin: Integer): Integer; cdecl;
begin
  Result := FileSeek(stream, offset, origin);
end;

function __ftelli64(stream: Integer): int64; cdecl;
begin
  Result := FileSeek(stream, 0, 1);
end;

const
  F_ERROR_R = 1;
  F_ERROR_W = 2;

var
  lasterror: integer = 0;

function _fileread(ptr: pointer; size, count: Integer; stream: Integer): Integer; cdecl;
begin
  if size * count > 0 then
  begin
    Result := FileRead(stream, ptr^, size * count) div size;
    if Result = -1 then
    begin
      lasterror := F_ERROR_R;
      Result := 0;
    end;
  end
  else
    Result := 0;
end;

function _filewrite(ptr: pointer; size, count: Integer; stream: Integer): Integer; cdecl;
begin
  if size * count > 0 then
  begin
    Result := FileWrite(stream, ptr^, size * count) div size;
    if Result = -1 then
    begin
      lasterror := F_ERROR_W;
      Result := 0;
    end;
  end
  else
    Result := 0;
end;

function _fileerror: Integer; cdecl;
begin
  Result := lasterror;
  lasterror := 0;
end;

function _fileclose(stream: Integer): Integer; cdecl;
begin
  if CloseHandle(THandle(stream)) then
    Result := 1
  else
    Result := 0;
end;

{$WARN SYMBOL_PLATFORM OFF}
function _filetruncate(stream: Integer; len: Integer): Integer; cdecl;
begin
  Result := 0;
  try
    FileSeek(stream, len, 0);
    Win32Check(SetEndOfFile(stream));
  except
    Result := -1;
  end;
end;

function _filesize(stream: Integer): Integer; cdecl;
var
  p:integer;
begin
  p := _fileseek(stream, 0, 1);
  Result := _fileseek(stream, 0, 2);
  _fileseek(stream, p, 0);
end;

function _filetell(stream: Integer): Integer; cdecl;
begin
  Result := _filepos(stream);
end;

function _filetell64(stream: Integer): Int64; cdecl;
begin
  Result := _filepos(stream);
end;

function _fileeof(stream: Integer): Integer; cdecl;
var
  p:integer;
begin
  p := _fileseek(stream, 0, 1);
  if p = _fileseek(stream, 0, 2) then
    Result := 1
  else
  begin
    Result := 0;
    _fileseek(stream, p, 0);
  end;
end;

function _filepos(stream: Integer): Integer; cdecl;
begin
  Result := _fileseek(stream, 0, 1);
end;

function _filerename(fin: PChar; fout: PChar): Integer; cdecl;
var
  FinF, FoutF: file;
  NumRead, NumWritten: Integer;
  Buf: array[1..8192] of Char;
  sname, dname: string;
begin
  sname := fin;
  if FileExists(sname) then
  begin
  {$I-}
    AssignFile(FinF, sname);
    Reset(FinF, 1);
    dname := fout;
    AssignFile(FoutF, dname);
    Rewrite(FoutF, 1);
    repeat
      BlockRead(FinF, Buf, SizeOf(Buf), NumRead);
      BlockWrite(FoutF, Buf, NumRead, NumWritten);
    until (NumRead = 0) or (NumWritten <> NumRead);
    CloseFile(FinF);
    CloseFile(FoutF);
    DeleteFile(sname);
  {$I+}
    if IOResult <> 0 then
      Result := 0
    else
      Result := 1;
  end
  else
    Result := 1;
end;

function _sscanf(Str : PChar; Format : PChar; Pointers: array of Pointer): Integer; cdecl;
begin
  Result := sscanf(Str, Format, Pointers);
end;

function ___errno: Integer; cdecl;
begin
  result := 0;
end;

var
  _myexit_funcname: string = '';
  _myexit_code: integer = 0;

procedure _myexit(funcname: PChar; code: Integer); cdecl;
begin
  // ToDo: Implement code
  _myexit_funcname := funcname;
  _myexit_code := code;
end;

var
  _myassert_cond: integer = 0;

procedure _myassert(cond: Integer); cdecl;
begin
  // ToDo: Implement code
  _myassert_cond := cond;
end;

var
  _exit_ecode: integer = 0;

procedure _exit(ecode: Integer); cdecl;
begin
  // ToDo: Implement code
  _exit_ecode := ecode;
end;

var
  _stat_path: string;
  _stat_structstat: Pointer;

function _stat(path: PChar; structstat: Pointer): Integer; cdecl;
begin
  // ToDo: Implement code
  _stat_path := path;
  _stat_structstat := structstat;
  Result := 1;
end;

var
  _alloca_size: Integer = 0;
  _alloca_result: pointer = nil;

function _alloca(size: integer): Pointer; cdecl;
var
  m: TMemoryManager;
begin
  _alloca_size := size;
  GetMemoryManager(m);
  Result := m.GetMem(size);
  _alloca_result := Result;
end;

var
  __setargv_argv: Pointer = nil;
  __setargv_argc: Pointer = nil;

function __setargv__(argv: Pointer; argc: Pointer): Integer; cdecl;
begin
  __setargv_argv := argv;
  __setargv_argc := argc;
  Result := 1;
end;

var
  _mybreakpoint_s: string = '';

procedure _mybreakpoint(s: PChar); cdecl;
begin
  _mybreakpoint_s := s;
end;

function _math_isnormal(x: double): integer; cdecl;
begin
  if x = 0.0 then
  begin
    Result := 0;
    Exit;
  end;
  if IsNan(x) or IsInfinite(x) then
  begin
    Result := 0;
    Exit;
  end;
  Result := 1;
end;

function _math_isfinite(x: double): integer; cdecl;
begin
  if IsNan(x) or IsInfinite(x) then
    Result := 1
  else
    Result := 0;
end;

// C_LIB
{$L .\obj\myqsort.obj}
{$L .\obj\mystrcpy.obj}
{$L .\obj\mystrcat.obj}
{$L .\obj\mystrdup.obj}

end.
