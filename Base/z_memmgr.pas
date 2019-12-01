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
//   JVAL: Zone Memory Replacement
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

unit z_memmgr;

interface

uses
  d_delphi;

type
  memmanageritem_t = record
    size: integer;
    user: PPointer;
    tag: integer;
    index: integer;
  end;
  Pmemmanageritem_t = ^memmanageritem_t;

  memmanageritems_t = array[0..$FFF] of Pmemmanageritem_t;
  Pmemmanageritems_t = ^memmanageritems_t;

type
  TMemManager = class
  private
    items: Pmemmanageritems_t;
    numitems: integer;
    realsize: integer;
    function item2ptr(const id: integer): Pointer;
    function ptr2item(const ptr: Pointer): integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure M_Free(ptr: Pointer);
    procedure M_FreeTags(lowtag, hightag: integer);
    procedure M_ChangeTag(ptr: Pointer; tag: integer);
    function M_Malloc(size: integer; tag: integer; user: Pointer): pointer;
    function M_Realloc(ptr: Pointer; size: integer; tag: integer; user: Pointer): pointer;
  end;


implementation

constructor TMemManager.Create;
begin
  items := nil;
  numitems := 0;
  realsize := 0;
end;

destructor TMemManager.Destroy;
var
  i: integer;
begin
  for i := numitems - 1 downto 0 do
    FreeMem(items[i]);
  FreeMem(items);
  inherited;
end;

function TMemManager.item2ptr(const id: integer): Pointer;
begin
  result := pointer(integer(items[id]) + SizeOf(memmanageritem_t));
end;

function TMemManager.ptr2item(const ptr: Pointer): integer;
begin
  result := Pmemmanageritem_t(Integer(ptr) - SizeOf(memmanageritem_t)).index;
end;

procedure TMemManager.M_Free(ptr: Pointer);
var
  i: integer;
begin
  i := ptr2item(ptr);
  if items[i].user <> nil then
    items[i].user^ := nil;
  FreeMem(items[i]);
  if i < numitems - 1 then
  begin
    items[i] := items[numitems - 1];
    items[numitems - 1] := nil;
    items[i].index := i;
  end;
  dec(numitems);
end;

procedure TMemManager.M_FreeTags(lowtag, hightag: integer);
var
  i: integer;
begin
  for i := numitems - 1 downto 0 do
    if (items[i].tag >= lowtag) and (items[i].tag <= hightag) then
      M_Free(item2ptr(i));
end;

procedure TMemManager.M_ChangeTag(ptr: Pointer; tag: integer);
begin
  items[ptr2item(ptr)].tag := tag;
end;

function TMemManager.M_Malloc(size: integer; tag: integer; user: Pointer): pointer;
var
  i: integer;
begin
  if realsize <= numitems then
  begin
    realsize := (realsize * 4 div 3 + 64) and (not 7);
    ReallocMem(items, realsize * SizeOf(Pmemmanageritem_t));
    for i := numitems + 1 to realsize - 1 do
      items[i] := nil;
  end;

  items[numitems] := malloc(size + SizeOf(memmanageritem_t));
  items[numitems].size := size;
  items[numitems].tag := tag;
  items[numitems].index := numitems;
  items[numitems].user := user;
  result := item2ptr(numitems);
  inc(numitems);
  if user <> nil then
    PPointer(user)^ := result;
end;

function TMemManager.M_Realloc(ptr: Pointer; size: integer; tag: integer; user: Pointer): pointer;
var
  tmp: pointer;
  copysize: integer;
  i: integer;
begin
  if size = 0 then
  begin
    M_Free(ptr);
    result := nil;
    exit;
  end;

  if ptr = nil then
  begin
    result := M_Malloc(size, tag, user);
    exit;
  end;

  i := ptr2item(ptr);
  if items[i].size = size then
  begin
    result := ptr;
    exit;
  end;

  if size > items[i].size then
    copysize := items[i].size
  else
    copysize := size;

  tmp := malloc(copysize);
  memcpy(tmp, ptr, copysize);
  M_Free(ptr);
  result := M_Malloc(size, tag, user);
  memcpy(result, tmp, copysize);
  memfree(tmp, copysize);
end;

end.

