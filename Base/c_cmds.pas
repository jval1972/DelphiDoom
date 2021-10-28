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

unit c_cmds;

interface

uses
  d_delphi;

type
  cmdproc_t = procedure(const parm1, parm2: string);

  cmd_t = record
    name: string[255];
    command: cmdproc_t;
  end;
  Pcmd_t = ^cmd_t;

procedure C_AddCmd(const name: string; proc: cmdproc_t);

function C_GetCmd(name: string; var cmd: cmd_t): boolean;

function C_CmdExists(name: string): boolean;

function C_ExecuteCmd(const cmd: Pcmd_t;
  const parm1: string = ''; const parm2: string = ''): boolean; overload;

function C_ExecuteCmd(const name: string): boolean; overload;

function C_ExecuteCmd(const name: string; const parm: string): boolean; overload;

function C_ExecuteCmd(name: string; const parm1, parm2: string): boolean; overload;

function C_BoolEval(parm: string; const default: boolean): boolean;

procedure C_CmdList(const mask: string);

function C_GetMachingList(const src: TDStringList; const mask: string): TDStringList;

procedure C_UnknowCommandMsg;

implementation

uses
  i_system;

const
  MAXCMDS = 300;

var
  CMDS: array[0..MAXCMDS - 1] of cmd_t;
  numcmds: integer = 0;
  cmdssorted: boolean = false;

const
  CMDSPLITSTR = ',';

procedure C_AddCmd(const name: string; proc: cmdproc_t);
var
  name1, name2: string;
begin
  if name = '' then
    exit;

  if numcmds = MAXCMDS then
  begin
    I_Error('C_AddCmd(): Can''t add %s, commands limit (%d) exceeded'#13#10, [name, MAXCMDS]);
    exit;
  end;

  if Pos(CMDSPLITSTR, name) = 0 then
  begin
    CMDS[numcmds].name := strupper(name);
    CMDS[numcmds].command := proc;
    inc(numcmds);
    cmdssorted := false;
  end
  else
  begin
    splitstring(name, name1, name2, CMDSPLITSTR);
    C_AddCmd(name1, proc);
    C_AddCmd(name2, proc);
  end;
end;

function C_ExecuteCmd(const name: string): boolean;
var
  name1,
  parm: string;
begin
  splitstring(name, name1, parm);
  result := C_ExecuteCmd(name1, parm);
end;

function C_ExecuteCmd(const name: string; const parm: string): boolean;
var
  parm1,
  parm2: string;
begin
  splitstring(parm, parm1, parm2);
  result := C_ExecuteCmd(name, parm1, parm2);
end;

procedure C_QuickSortCmds;

  procedure qsort(l, r: Integer);
  var
    i, j: Integer;
    t: cmd_t;
    p: cmd_t;
  begin
    repeat
      i := l;
      j := r;
      p := CMDS[(l + r) shr 1];
      repeat
        while CMDS[i].name < p.name do
          inc(i);
        while CMDS[j].name > p.name do
          dec(j);
        if i <= j then
        begin
          t := CMDS[i];
          CMDS[i] := CMDS[j];
          CMDS[j] := t;
          inc(i);
          dec(j);
        end;
      until i > j;
      if l < j then
        qsort(l, j);
      l := i;
    until i >= r;
  end;

begin
  if numcmds > 1 then
    qsort(0, numcmds - 1);
  cmdssorted := true;
end;

function C_GetCmd(name: string; var cmd: cmd_t): boolean;
var
  l, h, i: integer;
  c: Pcmd_t;
begin
  if not cmdssorted then
    C_QuickSortCmds;

  name := strupper(name);
  l := 0;
  h := numcmds - 1;
  while l <= h do
  begin
    i := (l + h) shr 1;
    c := @CMDs[i];
    if c.name < name then
      l := i + 1
    else
    begin
      if c.name = name then
      begin
        result := true;
        cmd := c^;
        exit;
      end;
      h := i - 1;
    end;
  end;
  result := false;
end;

function C_CmdExists(name: string): boolean;
var
  c: cmd_t;
begin
  result := C_GetCmd(name, c);
end;

function C_ExecuteCmd(const cmd: Pcmd_t;
  const parm1: string = ''; const parm2: string = ''): boolean;
begin
  result := false;

  if cmd = nil then
    exit;

  if not Assigned(cmd.command) then
    exit;

  cmd.command(parm1, parm2);
  result := true;
end;

function C_ExecuteCmd(name: string; const parm1, parm2: string): boolean;
var
  l, h, i: integer;
  c: Pcmd_t;
begin
  if not cmdssorted then
    C_QuickSortCmds;

  name := strupper(name);
  l := 0;
  h := numcmds - 1;
  while l <= h do
  begin
    i := (l + h) shr 1;
    c := @CMDs[i];
    if c.name < name then
      l := i + 1
    else
    begin
      if c.name = name then
      begin
        result := true;
        c.command(parm1, parm2);
        exit;
      end;
      h := i - 1;
    end;
  end;
  if (name <> 'GET') and (name <> 'SET') then
  begin
    if parm1 = '' then
      result := C_ExecuteCmd('GET', name)
    else if parm2 = '' then
      result := C_ExecuteCmd('SET', name, parm1)
    else
      result := false;
  end
  else
    result := false;
end;

const
  TRUE_VALUES: array[0..3] of string = (
    'TRUE',
    'ON',
    'YES',
    '1'
  );

  FALSE_VALUES: array[0..3] of string = (
    'FALSE',
    'OFF',
    'NO',
    '0'
  );

function C_BoolEval(parm: string; const default: boolean): boolean;
var
  i: integer;
begin
  parm := strupper(parm);

  for i := low(TRUE_VALUES) to high(TRUE_VALUES) do
    if parm = TRUE_VALUES[i] then
    begin
      result := true;
      exit;
    end;

  for i := low(FALSE_VALUES) to high(FALSE_VALUES) do
    if parm = FALSE_VALUES[i] then
    begin
      result := false;
      exit;
    end;

  result := default;

end;

function C_GetMachingList(const src: TDStringList; const mask: string): TDStringList;
var
  i, j: integer;
  lst: TDStringList;
  str: string;
  p: integer;
  last_p: integer;
  fits: boolean;
  first: boolean;
  check: string;
begin
  result := TDStringList.Create;

  str := '';
  lst := TDStringList.Create;
  try
    first := true;
    for i := 1 to Length(mask) do
    begin
      if mask[i] = '*' then
      begin
        if i = 1 then
          first := false;
        if str <> '' then
        begin
          lst.Add(str);
          str := '';
        end;
      end
      else
        str := str + upcase(mask[i]);
    end;
    if str <> '' then
      lst.Add(str);

    for i := 0 to src.Count - 1 do
    begin
      last_p := 0;
      fits := true;
      check := strupper(src[i]);
      for j := 0 to lst.Count - 1 do
      begin
        p := Pos(lst[j], check);
        if first and (j = 0) and (p <> 1) then
          fits := false
        else if p = 0 then
          fits := false
        else if p + Length(lst[j]) > last_p then
          last_p := p + Length(lst[j])
        else
          fits := false;
        if not fits then
          break;
      end;
      if fits then
        result.Add(src[i]);
    end;

  finally
    lst.Free;
  end;

end;

procedure C_CmdList(const mask: string);
var
  i: integer;
  clist: TDStringList;
  rlist: TDStringList;
  count: integer;
begin
  if mask = '' then
  begin
    count := numcmds;
    for i := 0 to count - 1 do
      printf('%s'#13#10, [CMDS[i].name]);
  end
  else
  begin
    clist := TDStringList.Create;
    try
      for i := 0 to numcmds - 1 do
        clist.Add(CMDS[i].name);

      rlist := C_GetMachingList(clist, mask);
      try
        count := rlist.Count;
        for i := 0 to rlist.Count - 1 do
          printf('%s'#13#10, [rlist[i]]);
      finally
        rlist.Free;
      end;
    finally
      clist.Free;
    end;
  end;
  printf('%d total commands.'#13#10, [count]);
end;

const
  S_UNKNOWNCOMMAND = 'Unknown command'#13#10;

procedure C_UnknowCommandMsg;
begin
  printf(S_UNKNOWNCOMMAND);
end;

end.
