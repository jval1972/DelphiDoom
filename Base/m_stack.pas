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

unit m_stack;

interface

uses
  d_delphi;

type
  TIntegerStack = class(TDNumberList)
  public
    procedure Push(const x: integer);
    function Pop(var x: integer): boolean;
  end;

  TIntegerQueue = class(TDNumberList)
  public
    function Remove(var x: integer): boolean;
  end;

//==============================================================================
//
// M_PushValue
//
//==============================================================================
procedure M_PushValue(const x: integer);

//==============================================================================
//
// M_PopValue
//
//==============================================================================
function M_PopValue: integer;

implementation

uses
  i_system;

var
  globalstack: TIntegerStack;

//==============================================================================
//
// TIntegerStack.Push
//
//==============================================================================
procedure TIntegerStack.Push(const x: integer);
begin
  Add(x);
end;

//==============================================================================
//
// TIntegerStack.Pop
//
//==============================================================================
function TIntegerStack.Pop(var x: integer): boolean;
begin
  result := Count > 0;
  if result then
  begin
    x := Numbers[Count - 1];
    Delete(Count - 1);
  end;
end;

//==============================================================================
//
// TIntegerQueue.Remove
//
//==============================================================================
function TIntegerQueue.Remove(var x: integer): boolean;
begin
  result := Count > 0;
  if result then
  begin
    x := Numbers[0];
    Delete(0);
  end;
end;

//==============================================================================
//
// M_PushValue
//
//==============================================================================
procedure M_PushValue(const x: integer);
begin
  globalstack.Push(x);
end;

//==============================================================================
//
// M_PopValue
//
//==============================================================================
function M_PopValue: integer;
begin
  if not globalstack.Pop(result) then
    I_DevError('M_PopValue(): Global Stack is empty!'#13#10);
end;

initialization
  globalstack := TIntegerStack.Create;

finalization
  globalstack.Free;

end.
