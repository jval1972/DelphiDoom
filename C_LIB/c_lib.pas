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

unit c_lib;

{$Z+}
{$H+}

interface

type
  PPointer = ^Pointer;

//==============================================================================
//
// sprintfsec
//
//==============================================================================
function sprintfsec(buffer: Pointer; format: Pointer; arguments: Pointer): Integer;

//==============================================================================
//
// _malloc
//
//==============================================================================
function _malloc(Size: Cardinal): Pointer; cdecl;

//==============================================================================
//
// _calloc
//
//==============================================================================
function _calloc(N: integer; Size: Cardinal): Pointer; cdecl;

//==============================================================================
//
// _realloc
//
//==============================================================================
function _realloc(p: pointer; Size: Cardinal): Pointer; cdecl;

//==============================================================================
//
// _free
//
//==============================================================================
procedure _free(p: Pointer); cdecl;

//==============================================================================
//
// _strcspn
//
//==============================================================================
function _strcspn(s1, s2: PChar): Cardinal; cdecl;

//==============================================================================
//
// _strchr
//
//==============================================================================
function _strchr(const S: PChar; C: Integer): PChar; cdecl;

//==============================================================================
//
// _strlen
//
//==============================================================================
function _strlen(const Str: PChar): Cardinal; cdecl;

//==============================================================================
//
// _strncmp
//
//==============================================================================
function _strncmp(S1, S2: PChar; MaxLen: Cardinal): Integer; cdecl;

//==============================================================================
//
// _strnicmp
//
//==============================================================================
function _strnicmp(S1, S2: PChar; MaxLen: Cardinal): Integer; cdecl;

//==============================================================================
//
// _strtod
//
//==============================================================================
function _strtod(str: PChar; endptr: PPointer): Double; cdecl;

//==============================================================================
//
// _atof
//
//==============================================================================
function _atof(const str: PChar): Double; cdecl;

//==============================================================================
//
// __ftol
//
//==============================================================================
function __ftol: Integer; cdecl;

//==============================================================================
//
// __ftoul
//
//==============================================================================
function __ftoul: Int64; cdecl;

//==============================================================================
//
// __llmod
//
//==============================================================================
procedure __llmod; cdecl;

//==============================================================================
//
// __lldiv
//
//==============================================================================
procedure __lldiv; cdecl;

//==============================================================================
//
// __llmul
//
//==============================================================================
procedure __llmul; cdecl;

//==============================================================================
//
// __llushr
//
//==============================================================================
procedure __llushr; cdecl;

//==============================================================================
//
// __llshr
//
//==============================================================================
procedure __llshr; cdecl;

//==============================================================================
//
// __llshl
//
//==============================================================================
procedure __llshl; cdecl;

//==============================================================================
//
// __lludiv
//
//==============================================================================
procedure __lludiv; cdecl;

//==============================================================================
//
// __llumod
//
//==============================================================================
procedure __llumod; cdecl;

//==============================================================================
//
// _memcpy
//
//==============================================================================
function _memcpy(dest: Pointer; const src: Pointer; count: Cardinal): Pointer; cdecl;

//==============================================================================
//
// _memcmp
//
//==============================================================================
function _memcmp(ptr1, ptr2: Pointer; count: Cardinal): Integer; cdecl;

//==============================================================================
//
// _memchr
//
//==============================================================================
function _memchr(const p: Pointer; value: integer; num: integer): Pointer; cdecl;

//==============================================================================
//
// _memset
//
//==============================================================================
procedure _memset(a: Pointer; b: Integer; c: Cardinal); cdecl;

//==============================================================================
//
// _isprint
//
//==============================================================================
function _isprint(c: Integer): Integer; cdecl;

//==============================================================================
//
// _sprintf
//
//==============================================================================
function _sprintf(buffer: Pointer; format: Pointer; arguments: Pointer): Integer; cdecl;

//==============================================================================
//
// _snprintf
//
//==============================================================================
function _snprintf(buffer: Pointer; sz: LongWord; format: Pointer; arguments: Pointer): Integer; cdecl;

//==============================================================================
//
// _vsnprintf
//
//==============================================================================
function _vsnprintf(buffer: Pointer; sz: LongWord; format: Pointer; arguments: Pointer): Integer; cdecl;

//==============================================================================
//
// _fprintf
//
//==============================================================================
function _fprintf(stream: Pointer; format: Pointer; arguments: Pointer): Integer; cdecl;

//==============================================================================
//
// _printf
//
//==============================================================================
function _printf(format: Pointer; arguments: Pointer): Integer; cdecl;

//==============================================================================
//
// _ceil
//
//==============================================================================
function _ceil(x: Double): Double; cdecl;

//==============================================================================
//
// _lround
//
//==============================================================================
function _lround(x: Double): Integer; cdecl;

//==============================================================================
//
// _mylround
//
//==============================================================================
function _mylround(x: Double): Integer; cdecl;

//==============================================================================
//
// _floor
//
//==============================================================================
function _floor(x: Double): Double; cdecl;

//==============================================================================
//
// _rint
//
//==============================================================================
function _rint(x: Double): Integer; cdecl;

//==============================================================================
//
// _pow
//
//==============================================================================
function _pow(x: Double; y: Double): Double; cdecl;

//==============================================================================
//
// _sqrt
//
//==============================================================================
function _sqrt(x: Double): Double; cdecl;

//==============================================================================
//
// _atan
//
//==============================================================================
function _atan(x: Double): Double; cdecl;

//==============================================================================
//
// _acos
//
//==============================================================================
function _acos(x: Double): Double; cdecl;

//==============================================================================
//
// _asin
//
//==============================================================================
function _asin(x: Double): Double; cdecl;

//==============================================================================
//
// _atan2
//
//==============================================================================
function _atan2(y: Double; x: Double): Double; cdecl;

//==============================================================================
//
// _cos
//
//==============================================================================
function _cos(x: Double): Double; cdecl;

//==============================================================================
//
// _sin
//
//==============================================================================
function _sin(x: Double): Double; cdecl;

//==============================================================================
//
// _exp
//
//==============================================================================
function _exp(x: Double): Double; cdecl;

//==============================================================================
//
// _log
//
//==============================================================================
function _log(x: Double): Double; cdecl;

//==============================================================================
//
// _fabs
//
//==============================================================================
function _fabs(x: Double): Double; cdecl;

//==============================================================================
//
// _fabsf
//
//==============================================================================
function _fabsf(x: Single): Single; cdecl;

//==============================================================================
//
// _frexp
//
//==============================================================================
function _frexp(arg: Double; var exp: Integer): Double; cdecl;

//==============================================================================
//
// _ldexp
//
//==============================================================================
function _ldexp(arg: Double; exp: Integer): Double; cdecl;

//==============================================================================
//
// _fmod
//
//==============================================================================
function _fmod(numer, denom: double): double; cdecl;

//==============================================================================
//
// _lrintf
//
//==============================================================================
function _lrintf(x: single): integer; cdecl;

//==============================================================================
//
// _lrint
//
//==============================================================================
function _lrint(x: double): integer; cdecl;

//==============================================================================
//
// _rand
//
//==============================================================================
function _rand: Integer; cdecl;

//==============================================================================
//
// _strcmp
//
//==============================================================================
function _strcmp(a: Pointer; b: Pointer): Integer; cdecl;

//==============================================================================
//
// _strncat
//
//==============================================================================
function _strncat(destination: PAnsiChar; const source: PAnsiChar; const num: Integer): PAnsiChar; cdecl;

//==============================================================================
//
// _strncpy
//
//==============================================================================
function _strncpy(destination: PAnsiChar; const source: PAnsiChar; const num: Integer): PAnsiChar; cdecl;

//==============================================================================
//
// _strtol
//
//==============================================================================
function _strtol(const str: PChar; p: pointer; base: integer): Integer; cdecl;

//==============================================================================
//
// _strstr
//
//==============================================================================
function _strstr(const str1: PAnsiChar; const str2: PAnsiChar): PAnsiChar; cdecl;

//==============================================================================
//
// _strrchr
//
//==============================================================================
function _strrchr(const str1: PAnsiChar; ch: integer): PAnsiChar; cdecl;

//==============================================================================
//
// _memmove
//
//==============================================================================
function _memmove(dest: Pointer; src: Pointer; n: Cardinal): Pointer; cdecl;

//==============================================================================
//
// _remove
//
//==============================================================================
function _remove(fname: PChar): integer; cdecl;

//==============================================================================
//
// _time
//
//==============================================================================
function _time(t: PInteger): Integer; cdecl;

//==============================================================================
//
// _getenv
//
//==============================================================================
function _getenv(const env: PAnsiChar): PAnsiChar; cdecl;

//==============================================================================
//
// _fileopenw
//
//==============================================================================
function _fileopenw(fname: PAnsiChar): Integer; cdecl;

//==============================================================================
//
// _fileopena
//
//==============================================================================
function _fileopena(fname: PAnsiChar): Integer; cdecl;

//==============================================================================
//
// _fileopenr
//
//==============================================================================
function _fileopenr(fname: PAnsiChar): Integer; cdecl;

//==============================================================================
//
// _fileunlink
//
//==============================================================================
function _fileunlink(fname: PAnsiChar): Integer; cdecl;

//==============================================================================
//
// _fileseek
//
//==============================================================================
function _fileseek(stream: Integer; offset, origin: Integer): Integer; cdecl;

//==============================================================================
//
// _fileseek64
//
//==============================================================================
function _fileseek64(stream: Integer; offset: int64; origin: Integer): Integer; cdecl;

//==============================================================================
//
// __fseeki64
//
//==============================================================================
function __fseeki64(stream: Integer; offset: int64; origin: Integer): Integer; cdecl;

//==============================================================================
//
// __ftelli64
//
//==============================================================================
function __ftelli64(stream: Integer): int64; cdecl;

//==============================================================================
//
// _fileread
//
//==============================================================================
function _fileread(ptr: pointer; size, count: Integer; stream: Integer): Integer; cdecl;

//==============================================================================
//
// _filewrite
//
//==============================================================================
function _filewrite(ptr: pointer; size, count: Integer; stream: Integer): Integer; cdecl;

//==============================================================================
//
// _fileerror
//
//==============================================================================
function _fileerror: Integer; cdecl;

//==============================================================================
//
// _fileclose
//
//==============================================================================
function _fileclose(stream: Integer): Integer; cdecl;

//==============================================================================
//
// _filetruncate
//
//==============================================================================
function _filetruncate(stream: Integer; len: Integer): Integer; cdecl;

//==============================================================================
//
// _filesize
//
//==============================================================================
function _filesize(stream: Integer): Integer; cdecl;

//==============================================================================
//
// _filetell
//
//==============================================================================
function _filetell(stream: Integer): Integer; cdecl;

//==============================================================================
//
// _filetell64
//
//==============================================================================
function _filetell64(stream: Integer): Int64; cdecl;

//==============================================================================
//
// _fileeof
//
//==============================================================================
function _fileeof(stream: Integer): Integer; cdecl;

//==============================================================================
//
// _filepos
//
//==============================================================================
function _filepos(stream: Integer): Integer; cdecl;

//==============================================================================
//
// _filerename
//
//==============================================================================
function _filerename(fin: PChar; fout: PChar): Integer; cdecl;

//==============================================================================
//
// _sscanf
//
//==============================================================================
function _sscanf(Str: PChar; Format: PChar; Pointers: array of Pointer): Integer; cdecl;

//==============================================================================
//
// ___errno
//
//==============================================================================
function ___errno: Integer; cdecl;

//==============================================================================
//
// _myexit
//
//==============================================================================
procedure _myexit(funcname: PChar; code: Integer); cdecl;

//==============================================================================
//
// _myassert
//
//==============================================================================
procedure _myassert(cond: Integer); cdecl;

//==============================================================================
//
// _exit
//
//==============================================================================
procedure _exit(ecode: Integer); cdecl;

//==============================================================================
//
// _stat
//
//==============================================================================
function _stat(path: PChar; structstat: Pointer): Integer; cdecl;

//==============================================================================
//
// _alloca
//
//==============================================================================
function _alloca(size: integer): Pointer; cdecl;

//==============================================================================
//
// __setargv__
//
//==============================================================================
function __setargv__(argv: Pointer; argc: Pointer): Integer; cdecl;

//==============================================================================
//
// _mybreakpoint
//
//==============================================================================
procedure _mybreakpoint(s: PChar); cdecl;

//==============================================================================
//
// _math_isnormal
//
//==============================================================================
function _math_isnormal(x: double): integer; cdecl;

//==============================================================================
//
// _math_isfinite
//
//==============================================================================
function _math_isfinite(x: double): integer; cdecl;

var
  __turboFloat: LongBool = False;
  __streams: Integer;

//==============================================================================
//
// _myqsort
//
//==============================================================================
procedure _myqsort; external;

//==============================================================================
//
// _mystrcpy
//
//==============================================================================
procedure _mystrcpy; external;

//==============================================================================
//
// _mystrcat
//
//==============================================================================
procedure _mystrcat; external;

//==============================================================================
//
// _mystrdup
//
//==============================================================================
procedure _mystrdup; external;

implementation

uses
  Windows, SysUtils, Math, DateUtils, scanf;

//==============================================================================
//
// sprintfsec
//
//==============================================================================
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
      Modifier := 0;
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
              if (m^ < Ord('0')) or (Ord('9') < m^) then
                break;
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

//==============================================================================
//
// _malloc
//
//==============================================================================
function _malloc(Size: Cardinal): Pointer; cdecl;
var
  m: TMemoryManager;
begin
  GetMemoryManager(m);
  Result := m.GetMem(Size);
  ZeroMemory(Result, Size);
end;

//==============================================================================
//
// _calloc
//
//==============================================================================
function _calloc(N: integer; Size: Cardinal): Pointer; cdecl;
begin
  Result := _malloc(N * Size);
end;

//==============================================================================
//
// _realloc
//
//==============================================================================
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

//==============================================================================
//
// _free
//
//==============================================================================
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

//==============================================================================
//
// _strlen
//
//==============================================================================
function _strlen(const Str: PChar): Cardinal; cdecl;
begin
  Result := StrLen(Str);
end;

//==============================================================================
//
// _strcspn
//
//==============================================================================
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

//==============================================================================
//
// _strchr
//
//==============================================================================
function _strchr(const S: PChar; C: Integer): PChar; cdecl;
begin
  Result := StrScan(S, Chr(C));
end;

//==============================================================================
//
// _strncmp
//
//==============================================================================
function _strncmp(S1, S2: PChar; MaxLen: Cardinal): Integer; cdecl;
begin
  Result := StrLComp(S1, S2, MaxLen);
end;

//==============================================================================
//
// _strnicmp
//
//==============================================================================
function _strnicmp(S1, S2: PChar; MaxLen: Cardinal): Integer; cdecl;
begin
  Result := StrLIComp(S1, S2, MaxLen);
end;

//==============================================================================
//
// _strtod
//
//==============================================================================
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

//==============================================================================
//
// _atof
//
//==============================================================================
function _atof(const str: PChar): Double; cdecl;
var
  pos_sep: integer;
  s: string;
  i, len: integer;
begin
  s := str;
  s := Trim(s);
  if s = '' then
  begin
    Result := 0.0;
    Exit;
  end;

  len := Length(s);

  pos_sep := 0;
  for i := 1 to len do
    if (s[i] = '.') or (s[i] = ',') then
    begin
      pos_sep := i;
      Break;
    end;

  if pos_sep > 0 then
    s[pos_sep] := DecimalSeparator;
  Result := StrToFloat(s);
end;

//==============================================================================
//
// __llmod
//
//==============================================================================
procedure __llmod; cdecl;
asm
  jmp System.@_llmod;
end;

//==============================================================================
//
// __lldiv
//
//==============================================================================
procedure __lldiv; cdecl;
asm
  jmp System.@_lldiv;
end;

//==============================================================================
//
// __llmul
//
//==============================================================================
procedure __llmul; cdecl;
asm
  jmp System.@_llmul;
end;

//==============================================================================
//
// __llushr
//
//==============================================================================
procedure __llushr; cdecl;
asm
  jmp System.@_llushr
end;

//==============================================================================
//
// __llshr
//
//==============================================================================
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

//==============================================================================
//
// __llshl
//
//==============================================================================
procedure __llshl; cdecl;
asm
  jmp System.@_llshl
end;

//==============================================================================
//
// __lludiv
//
//==============================================================================
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

//==============================================================================
//
// __llumod
//
//==============================================================================
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

//==============================================================================
//
// __ftol
//
//==============================================================================
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

//==============================================================================
//
// __ftoul
//
//==============================================================================
function __ftoul: Int64; cdecl;
// Borland C++ float to integer (Int64) conversion
asm
  jmp System.@Trunc  // FST(0) -> EDX:EAX, as expected by BCC32 compiler
end;

//==============================================================================
//
// _memcpy
//
//==============================================================================
function _memcpy(dest: Pointer; const src: Pointer; count: Cardinal): Pointer; cdecl;
begin
  CopyMemory(dest, src, count);
  Result := dest;
end;

//==============================================================================
//
// _memcmp
//
//==============================================================================
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

//==============================================================================
//
// _memchr
//
//==============================================================================
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

//==============================================================================
//
// _memset
//
//==============================================================================
procedure _memset(a: Pointer; b: Integer; c: Cardinal); cdecl;
begin
  FillMemory(a, c, b);
end;

//==============================================================================
//
// _isprint
//
//==============================================================================
function _isprint(c: Integer): Integer; cdecl;
begin
  if (c < 32) or (127 <= c) then
    Result := 0
  else
    Result := 1;
end;

//==============================================================================
//
// _sprintf
//
//==============================================================================
function _sprintf(buffer: Pointer; format: Pointer; arguments: Pointer): Integer; cdecl;
begin
  Result := sprintfsec(buffer, format, @arguments);
end;

//==============================================================================
//
// _snprintf
//
//==============================================================================
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

//==============================================================================
//
// _vsnprintf
//
//==============================================================================
function _vsnprintf(buffer: Pointer; sz: LongWord; format: Pointer; arguments: Pointer): Integer; cdecl;
begin
  Result := _snprintf(buffer, sz, format, arguments);
end;

//==============================================================================
//
// _fprintf
//
//==============================================================================
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

//==============================================================================
//
// _printf
//
//==============================================================================
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

//==============================================================================
//
// _ceil
//
//==============================================================================
function _ceil(x: Double): Double; cdecl;
begin
  Result := Ceil(x);
end;

//==============================================================================
//
// _lround
//
//==============================================================================
function _lround(x: Double): Integer; cdecl;
begin
  Result := Round(x);
end;

//==============================================================================
//
// _mylround
//
//==============================================================================
function _mylround(x: Double): Integer; cdecl;
begin
  Result := Round(x);
end;

//==============================================================================
//
// _floor
//
//==============================================================================
function _floor(x: Double): Double; cdecl;
begin
  Result := Trunc(x);
end;

//==============================================================================
//
// _rint
//
//==============================================================================
function _rint(x: Double): Integer; cdecl;
begin
  Result := Trunc(x + 0.5);
end;

//==============================================================================
//
// _pow
//
//==============================================================================
function _pow(x: Double; y: Double): Double; cdecl;
begin
  Result := Power(x, y);
end;

//==============================================================================
//
// _sqrt
//
//==============================================================================
function _sqrt(x: Double): Double; cdecl;
begin
  Result := Sqrt(x);
end;

//==============================================================================
//
// _atan
//
//==============================================================================
function _atan(x: Double): Double; cdecl;
begin
  Result := ArcTan(x);
end;

//==============================================================================
//
// _acos
//
//==============================================================================
function _acos(x: Double): Double; cdecl;
begin
  Result := ArcCos(x);
end;

//==============================================================================
//
// _asin
//
//==============================================================================
function _asin(x: Double): Double; cdecl;
begin
  Result := ArcSin(x);
end;

//==============================================================================
//
// _atan2
//
//==============================================================================
function _atan2(y: Double; x: Double): Double;
begin
  Result := ArcTan2(y, x);
end;

//==============================================================================
//
// _cos
//
//==============================================================================
function _cos(x: Double): Double; cdecl;
begin
  Result := cos(x);
end;

//==============================================================================
//
// _sin
//
//==============================================================================
function _sin(x: Double): Double; cdecl;
begin
  Result := sin(x);
end;

//==============================================================================
//
// _exp
//
//==============================================================================
function _exp(x: Double): Double; cdecl;
begin
  Result := System.Exp(x);
end;

//==============================================================================
//
// _log
//
//==============================================================================
function _log(x: Double): Double; cdecl;
begin
  Result := Ln(x);
end;

//==============================================================================
//
// _fabs
//
//==============================================================================
function _fabs(x: Double): Double; cdecl;
begin
  if x >= 0 then
    Result := x
  else
    Result := -x;
end;

//==============================================================================
//
// _fabsf
//
//==============================================================================
function _fabsf(x: Single): Single; cdecl;
begin
  if x >= 0 then
    Result := x
  else
    Result := -x;
end;

//==============================================================================
//
// _frexp
//
//==============================================================================
function _frexp(arg: Double; var exp: Integer): Double; cdecl;
var
  ret: extended;
begin
  FRexp(arg, ret, exp);
  Result := ret;
end;

//==============================================================================
//
// _ldexp
//
//==============================================================================
function _ldexp(arg: Double; exp: Integer): Double; cdecl;
begin
  Result := arg * Power(2, exp);
end;

//==============================================================================
//
// _fmod
//
//==============================================================================
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

//==============================================================================
//
// _lrintf
//
//==============================================================================
function _lrintf(x: single): integer; cdecl;
begin
  Result := Round(x);
end;

//==============================================================================
//
// _lrint
//
//==============================================================================
function _lrint(x: double): integer; cdecl;
begin
  Result := Round(x);
end;

//==============================================================================
//
// _rand
//
//==============================================================================
function _rand: Integer; cdecl;
begin
  Result := Trunc(Random * ($7FFF + 1));
end;

//==============================================================================
//
// _strcmp
//
//==============================================================================
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

//==============================================================================
//
// _strncat
//
//==============================================================================
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

//==============================================================================
//
// _strncpy
//
//==============================================================================
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

//==============================================================================
//
// _strtol
//
//==============================================================================
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

//==============================================================================
//
// _strstr
//
//==============================================================================
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

//==============================================================================
//
// _strrchr
//
//==============================================================================
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

//==============================================================================
//
// _memmove
//
//==============================================================================
function _memmove(dest: Pointer; src: Pointer; n: Cardinal): Pointer; cdecl;
begin
  MoveMemory(dest, src, n);
  Result := dest;
end;

//==============================================================================
//
// _remove
//
//==============================================================================
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

//==============================================================================
//
// _time
//
//==============================================================================
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

//==============================================================================
//
// _getenv
//
//==============================================================================
function _getenv(const env: PAnsiChar): PAnsiChar; cdecl;
begin
  ZeroMemory(@_getenvbuf, SizeOf(_getenvbuf));
  GetEnvironmentVariable(PChar(env), _getenvbuf, SizeOf(_getenvbuf));
  result := @_getenvbuf[0];
end;

//==============================================================================
//
// _fileopenw
//
//==============================================================================
function _fileopenw(fname: PAnsiChar): Integer; cdecl;
begin
  Result := FileCreate(fname);
  if Result < 1 then
    Result := 0;
end;

//==============================================================================
//
// _fileopena
//
//==============================================================================
function _fileopena(fname: PAnsiChar): Integer; cdecl;
begin
  Result := FileCreate(fname);
  if Result < 1 then
    Result := 0;
  _fileseek(Result, 0, 2);
end;

//==============================================================================
//
// _fileopenr
//
//==============================================================================
function _fileopenr(fname: PAnsiChar): Integer; cdecl;
begin
  Result := FileOpen(fname, 2);
end;

//==============================================================================
//
// _fileunlink
//
//==============================================================================
function _fileunlink(fname: PAnsiChar): Integer; cdecl;
begin
  if DeleteFile(fname) then
    Result := 0
  else
    Result := -1;
end;

//==============================================================================
//
// _fileseek
//
//==============================================================================
function _fileseek(stream: Integer; offset, origin: integer): Integer; cdecl;
begin
  Result := FileSeek(stream, offset, origin);
end;

//==============================================================================
//
// _fileseek64
//
//==============================================================================
function _fileseek64(stream: Integer; offset: int64; origin: Integer): Integer; cdecl;
begin
  Result := FileSeek(stream, offset, origin);
end;

//==============================================================================
//
// __fseeki64
//
//==============================================================================
function __fseeki64(stream: Integer; offset: int64; origin: Integer): Integer; cdecl;
begin
  Result := FileSeek(stream, offset, origin);
end;

//==============================================================================
//
// __ftelli64
//
//==============================================================================
function __ftelli64(stream: Integer): int64; cdecl;
begin
  Result := FileSeek(stream, 0, 1);
end;

const
  F_ERROR_R = 1;
  F_ERROR_W = 2;

var
  lasterror: integer = 0;

//==============================================================================
//
// _fileread
//
//==============================================================================
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

//==============================================================================
//
// _filewrite
//
//==============================================================================
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

//==============================================================================
//
// _fileerror
//
//==============================================================================
function _fileerror: Integer; cdecl;
begin
  Result := lasterror;
  lasterror := 0;
end;

//==============================================================================
//
// _fileclose
//
//==============================================================================
function _fileclose(stream: Integer): Integer; cdecl;
begin
  if CloseHandle(THandle(stream)) then
    Result := 1
  else
    Result := 0;
end;

{$WARN SYMBOL_PLATFORM OFF}

//==============================================================================
//
// _filetruncate
//
//==============================================================================
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

//==============================================================================
//
// _filesize
//
//==============================================================================
function _filesize(stream: Integer): Integer; cdecl;
var
  p: integer;
begin
  p := _fileseek(stream, 0, 1);
  Result := _fileseek(stream, 0, 2);
  _fileseek(stream, p, 0);
end;

//==============================================================================
//
// _filetell
//
//==============================================================================
function _filetell(stream: Integer): Integer; cdecl;
begin
  Result := _filepos(stream);
end;

//==============================================================================
//
// _filetell64
//
//==============================================================================
function _filetell64(stream: Integer): Int64; cdecl;
begin
  Result := _filepos(stream);
end;

//==============================================================================
//
// _fileeof
//
//==============================================================================
function _fileeof(stream: Integer): Integer; cdecl;
var
  p: integer;
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

//==============================================================================
//
// _filepos
//
//==============================================================================
function _filepos(stream: Integer): Integer; cdecl;
begin
  Result := _fileseek(stream, 0, 1);
end;

//==============================================================================
//
// _filerename
//
//==============================================================================
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

//==============================================================================
//
// _sscanf
//
//==============================================================================
function _sscanf(Str : PChar; Format : PChar; Pointers: array of Pointer): Integer; cdecl;
begin
  Result := sscanf(Str, Format, Pointers);
end;

//==============================================================================
//
// ___errno
//
//==============================================================================
function ___errno: Integer; cdecl;
begin
  result := 0;
end;

var
  _myexit_funcname: string = '';
  _myexit_code: integer = 0;

//==============================================================================
//
// _myexit
//
//==============================================================================
procedure _myexit(funcname: PChar; code: Integer); cdecl;
begin
  // ToDo: Implement code
  _myexit_funcname := funcname;
  _myexit_code := code;
end;

var
  _myassert_cond: integer = 0;

//==============================================================================
//
// _myassert
//
//==============================================================================
procedure _myassert(cond: Integer); cdecl;
begin
  // ToDo: Implement code
  _myassert_cond := cond;
end;

var
  _exit_ecode: integer = 0;

//==============================================================================
//
// _exit
//
//==============================================================================
procedure _exit(ecode: Integer); cdecl;
begin
  // ToDo: Implement code
  _exit_ecode := ecode;
end;

var
  _stat_path: string;
  _stat_structstat: Pointer;

//==============================================================================
//
// _stat
//
//==============================================================================
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

//==============================================================================
//
// _alloca
//
//==============================================================================
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

//==============================================================================
//
// __setargv__
//
//==============================================================================
function __setargv__(argv: Pointer; argc: Pointer): Integer; cdecl;
begin
  __setargv_argv := argv;
  __setargv_argc := argc;
  Result := 1;
end;

var
  _mybreakpoint_s: string = '';

//==============================================================================
//
// _mybreakpoint
//
//==============================================================================
procedure _mybreakpoint(s: PChar); cdecl;
begin
  _mybreakpoint_s := s;
end;

//==============================================================================
//
// _math_isnormal
//
//==============================================================================
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

//==============================================================================
//
// _math_isfinite
//
//==============================================================================
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
