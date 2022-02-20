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
//   JVAL: Zone Memory Replacement
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit z_memmgr;

interface

uses
  d_delphi;

{$IFDEF DEBUG}
const
  MCONSISTANCY = $FACEFADE;
{$ENDIF}

type
  memmanageritem_t = record
    size: integer;
    user: PPointer;
    tag: integer;
{$IFDEF DEBUG}
    consistancy: LongWord;
{$ENDIF}
    index: integer;
  end;
  Pmemmanageritem_t = ^memmanageritem_t;

  memmanageritems_t = array[0..$FFF] of Pmemmanageritem_t;
  Pmemmanageritems_t = ^memmanageritems_t;

type
  TMemManager = class
  private
    fitems: Pmemmanageritems_t;
    fnumitems: integer;
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
    function M_Size(ptr: Pointer): integer;
    property items: Pmemmanageritems_t read fitems write fitems;
    property numitems: integer read fnumitems write fnumitems;
  end;

implementation

{$IFDEF DEBUG}
uses
  i_system;
{$ENDIF}

//==============================================================================
//
// TMemManager.Create
//
//==============================================================================
constructor TMemManager.Create;
begin
  fitems := nil;
  fnumitems := 0;
  realsize := 0;
end;

//==============================================================================
//
// TMemManager.Destroy
//
//==============================================================================
destructor TMemManager.Destroy;
var
  i: integer;
begin
  for i := fnumitems - 1 downto 0 do
    memfree(Pointer(fitems[i]), fitems[i].size + SizeOf(memmanageritem_t));
  memfree(pointer(fitems), realsize * SizeOf(Pmemmanageritem_t));
  inherited;
end;

//==============================================================================
//
// TMemManager.item2ptr
//
//==============================================================================
function TMemManager.item2ptr(const id: integer): Pointer;
begin
{$IFDEF DEBUG}
  if fitems[id].consistancy <> MCONSISTANCY then
    I_Error('TMemManager.item2ptr(): Consistancy failed!');
{$ENDIF}
  result := pointer(integer(fitems[id]) + SizeOf(memmanageritem_t));
end;

//==============================================================================
//
// TMemManager.ptr2item
//
//==============================================================================
function TMemManager.ptr2item(const ptr: Pointer): integer;
begin
  result := Pmemmanageritem_t(Integer(ptr) - SizeOf(memmanageritem_t)).index;
{$IFDEF DEBUG}
  if Pmemmanageritem_t(Integer(ptr) - SizeOf(memmanageritem_t)).consistancy <> MCONSISTANCY then
    I_Error('TMemManager.ptr2item(): Consistancy failed!');
{$ENDIF}
end;

//==============================================================================
//
// TMemManager.M_Free
//
//==============================================================================
procedure TMemManager.M_Free(ptr: Pointer);
var
  i: integer;
begin
  i := ptr2item(ptr);
  if fitems[i].user <> nil then
    fitems[i].user^ := nil;
  memfree(pointer(fitems[i]), fitems[i].size + SizeOf(memmanageritem_t));
  if i < fnumitems - 1 then
  begin
    fitems[i] := fitems[fnumitems - 1];
    fitems[fnumitems - 1] := nil;
    fitems[i].index := i;
  end
  else
    fitems[i] := nil;
  dec(fnumitems);
end;

//==============================================================================
//
// TMemManager.M_FreeTags
//
//==============================================================================
procedure TMemManager.M_FreeTags(lowtag, hightag: integer);
var
  i: integer;
begin
  for i := fnumitems - 1 downto 0 do
    if (fitems[i].tag >= lowtag) and (fitems[i].tag <= hightag) then
      M_Free(item2ptr(i));
end;

//==============================================================================
//
// TMemManager.M_ChangeTag
//
//==============================================================================
procedure TMemManager.M_ChangeTag(ptr: Pointer; tag: integer);
begin
  fitems[ptr2item(ptr)].tag := tag;
end;

//==============================================================================
//
// TMemManager.M_Malloc
//
//==============================================================================
function TMemManager.M_Malloc(size: integer; tag: integer; user: Pointer): pointer;
var
  i: integer;
begin
  if realsize <= fnumitems then
  begin
    realsize := (realsize * 4 div 3 + 64) and not 7;
    realloc(pointer(fitems), fnumitems * SizeOf(Pmemmanageritem_t), realsize * SizeOf(Pmemmanageritem_t));
    for i := fnumitems + 1 to realsize - 1 do
      fitems[i] := nil;
  end;

  fitems[fnumitems] := malloc(size + SizeOf(memmanageritem_t));
  fitems[fnumitems].size := size;
  fitems[fnumitems].tag := tag;
  fitems[fnumitems].index := fnumitems;
  fitems[fnumitems].user := user;
{$IFDEF DEBUG}
  fitems[fnumitems].consistancy := MCONSISTANCY;
{$ENDIF}
  result := item2ptr(fnumitems);
  inc(fnumitems);
  if user <> nil then
    PPointer(user)^ := result;
end;

//==============================================================================
//
// TMemManager.M_Realloc
//
//==============================================================================
function TMemManager.M_Realloc(ptr: Pointer; size: integer; tag: integer; user: Pointer): pointer;
var
  tmp: pointer;
  copysize: integer;
  i: integer;
begin
  if size = 0 then
  begin
    if ptr <> nil then
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
  if fitems[i].size = size then
  begin
    result := ptr;
    exit;
  end;

  if size > fitems[i].size then
    copysize := fitems[i].size
  else
    copysize := size;

  tmp := malloc(copysize);
  memcpy(tmp, ptr, copysize);
  M_Free(ptr);
  result := M_Malloc(size, tag, user);
  memcpy(result, tmp, copysize);
  memfree(tmp, copysize);
end;

//==============================================================================
//
// TMemManager.M_Size
//
//==============================================================================
function TMemManager.M_Size(ptr: Pointer): integer;
var
  i: integer;
begin
  i := ptr2item(ptr);
  result := fitems[i].size;
end;

end.

