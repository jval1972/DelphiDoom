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
//  Smart pointer list
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit m_smartpointerlist;

interface

uses
  d_delphi;

type
  TSmartPointerList = class(TObject)
  private
    fList: PPointerArray;
    fNumItems: integer;
    fRealSize: integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddItem(const value: pointer);
    function DeleteItem(const item: pointer): boolean;
    function ItemExists(const value: pointer): boolean;
    procedure Clear;
    procedure Priority(const index: integer);
    property List: PPointerArray read fList;
    property Count: integer read fNumItems;
  end;

implementation

//==============================================================================
// TSmartPointerList.Create
//
// TSmartPointerList
//
//==============================================================================
constructor TSmartPointerList.Create;
begin
  fList := nil;
  fNumItems := 0;
  fRealSize := 0;
  inherited;
end;

//==============================================================================
//
// TSmartPointerList.Destroy
//
//==============================================================================
destructor TSmartPointerList.Destroy;
begin
  Clear;
  inherited;
end;

//==============================================================================
//
// TSmartPointerList.AddItem
//
//==============================================================================
procedure TSmartPointerList.AddItem(const value: pointer);
var
  newsize: integer;
begin
  if fNumItems >= fRealSize then
  begin
    if fRealSize < 16 then
      newsize := fRealSize + 4
    else if fRealSize < 128 then
      newsize := fRealSize + 16
    else if fRealSize < 1024 then
      newsize := fRealSize + 128
    else
      newsize := fRealSize + 256;
    realloc(pointer(fList), fRealSize * SizeOf(pointer), newsize * SizeOf(pointer));
    fRealSize := newsize;
  end;
  fList[fNumItems] := value;
  Inc(fNumItems);
end;

//==============================================================================
//
// TSmartPointerList.DeleteItem
//
//==============================================================================
function TSmartPointerList.DeleteItem(const item: pointer): boolean;
var
  i: integer;
begin
  for i := 0 to fNumItems - 1 do
    if fList[i] = item then
    begin
      fList[i] := fList[fNumItems - 1];
      dec(fNumItems);
      Result := True;
      Exit;
    end;

  Result := False;
end;

//==============================================================================
//
// TSmartPointerList.ItemExists
//
//==============================================================================
function TSmartPointerList.ItemExists(const value: pointer): boolean;
var
  i: integer;
  p: pointer;
begin
  for i := 0 to fNumItems - 1 do
    if fList[i] = value then
    begin
      if i > 0 then
      begin
        p := fList[i];
        fList[i] := fList[0];
        fList[0] := p;
      end;
      Result := True;
      Exit;
    end;
  Result := False;
end;

//==============================================================================
//
// TSmartPointerList.Clear
//
//==============================================================================
procedure TSmartPointerList.Clear;
begin
  realloc(pointer(fList), fNumItems * SizeOf(pointer), 0);
  fList := nil;
  fNumItems := 0;
  fRealSize := 0;
end;

//==============================================================================
//
// TSmartPointerList.Priority
//
//==============================================================================
procedure TSmartPointerList.Priority(const index: integer);
var
  newindex: integer;
  p: pointer;
begin
  if index > 0 then
    if index < fNumItems then
    begin
      newindex := index div 2;
      p := fList[index];
      fList[index] := fList[newindex];
      fList[newindex] := p;
    end;
end;

end.

