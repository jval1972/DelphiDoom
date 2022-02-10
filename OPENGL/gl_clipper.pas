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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit gl_clipper;

interface

uses
  tables;

//==============================================================================
//
// gld_clipper_SafeCheckRange
//
//==============================================================================
function gld_clipper_SafeCheckRange(const startAngle, endAngle: angle_t): boolean;

//==============================================================================
//
// gld_clipper_SafeAddClipRange
//
//==============================================================================
procedure gld_clipper_SafeAddClipRange(const startAngle, endAngle: angle_t);

//==============================================================================
//
// gld_ClipperAddViewRange
//
//==============================================================================
procedure gld_ClipperAddViewRange;

//==============================================================================
//
// gld_ClipperDone
//
//==============================================================================
procedure gld_ClipperDone;

implementation

//---------------------------------------------------------------------------
// Handles visibility checks.
// Loosely based on the JDoom clipper.
//
//---------------------------------------------------------------------------
// Copyright 2003 Tim Stump
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
// 3. The name of the author may not be used to endorse or promote products
//    derived from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
// OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
// IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
// NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
// THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//---------------------------------------------------------------------------
//

uses
  d_delphi,
  r_aspect,
  r_main;

type
  Pclipnode_t = ^clipnode_t;
  clipnode_t = record
    prev, next: Pclipnode_t;
    start, finish: angle_t;
  end;

var
  freelist: Pclipnode_t = nil;
  clipnodes: Pclipnode_t = nil;
  cliphead: Pclipnode_t = nil;

//==============================================================================
//
// gld_clipnode_GetNew
//
//==============================================================================
function gld_clipnode_GetNew: Pclipnode_t;
begin
  if freelist <> nil then
  begin
    result := freelist;
    freelist := result.next;
    exit;
  end
  else
    result := malloc(SizeOf(clipnode_t));
end;

//==============================================================================
//
// gld_clipnode_NewRange
//
//==============================================================================
function gld_clipnode_NewRange(const start, finish: angle_t): Pclipnode_t;
begin
  result := gld_clipnode_GetNew;
  result.start := start;
  result.finish := finish;
  result.next := nil;
  result.prev := nil;
end;

//==============================================================================
//
// gld_clipper_IsRangeVisible
//
//==============================================================================
function gld_clipper_IsRangeVisible(const startAngle, endAngle: angle_t): boolean;
var
  ci: Pclipnode_t;
begin
  ci := cliphead;

  if (endAngle = 0) then
    if ci <> nil then
      if ci.start = 0 then
      begin
        result := false;
        exit;
      end;

  while (ci <> nil) and (ci.start < endAngle) do
  begin
    if (startAngle >= ci.start) and (endAngle <= ci.finish) then
    begin
      result := false;
      exit;
    end;
    ci := ci.next;
  end;

  result := true;
end;

//==============================================================================
//
// gld_clipper_SafeCheckRange
//
//==============================================================================
function gld_clipper_SafeCheckRange(const startAngle, endAngle: angle_t): boolean;
begin
  if startAngle > endAngle then
    result := gld_clipper_IsRangeVisible(startAngle, ANGLE_MAX) or gld_clipper_IsRangeVisible(0, endAngle)
  else
    result := gld_clipper_IsRangeVisible(startAngle, endAngle);
end;

//==============================================================================
//
// gld_clipnode_Free
//
//==============================================================================
procedure gld_clipnode_Free(node: Pclipnode_t);
begin
  node.next := freelist;
  freelist := node;
end;

//==============================================================================
//
// gld_clipper_RemoveRange
//
//==============================================================================
procedure gld_clipper_RemoveRange(range: Pclipnode_t);
begin
  if range = cliphead then
    cliphead := cliphead.next
  else
  begin
    if range.prev <> nil then
      range.prev.next := range.next;
    if range.next <> nil then
      range.next.prev := range.prev;
  end;

  gld_clipnode_Free(range);
end;

//==============================================================================
//
// gld_clipper_AddClipRange
//
//==============================================================================
procedure gld_clipper_AddClipRange(start, finish: angle_t);
var
  node, temp, prevNode: Pclipnode_t;
begin
  if cliphead <> nil then
  begin
    //check to see if range contains any old ranges
    node := cliphead;
    while (node <> nil) and (node.start < finish) do
    begin
      if (node.start >= start) and (node.finish <= finish) then
      begin
        temp := node;
        node := node.next;
        gld_clipper_RemoveRange(temp);
      end
      else
      begin
        if (node.start <= start) and (node.finish >= finish) then
          exit
        else
          node := node.next;
      end;
    end;
    //check to see if range overlaps a range (or possibly 2)
    node := cliphead;
    while node <> nil do
    begin
      if (node.start >= start) and (node.start <= finish) then
      begin
        node.start := start;
        exit;
      end;
      if (node.finish >= start) and (node.finish <= finish) then
      begin
        // check for possible merger
        if (node.next <> nil) and (node.next.start <= finish) then
        begin
          node.finish := node.next.finish;
          gld_clipper_RemoveRange(node.next);
        end
        else
        begin
          node.finish := finish;
        end;

        exit;
      end;
      node := node.next;
    end;
    //just add range
    node := cliphead;
    prevNode := nil;
    temp := gld_clipnode_NewRange(start, finish);
    while (node <> nil) and (node.start < finish) do
    begin
      prevNode := node;
      node := node.next;
    end;
    temp.next := node;
    if node = nil then
    begin
      temp.prev := prevNode;
      if prevNode <> nil then
      begin
        prevNode.next := temp;
      end;
      if cliphead = nil then
      begin
        cliphead := temp;
      end;
    end
    else
    begin
      if node = cliphead then
      begin
        cliphead.prev := temp;
        cliphead := temp;
      end
      else
      begin
        temp.prev := prevNode;
        prevNode.next := temp;
        node.prev := temp;
      end;
    end;
  end
  else
  begin
    cliphead := gld_clipnode_NewRange(start, finish);
    exit;
  end;
end;

//==============================================================================
//
// gld_clipper_SafeAddClipRange
//
//==============================================================================
procedure gld_clipper_SafeAddClipRange(const startAngle, endAngle: angle_t);
begin
  if startAngle > endAngle then
  begin
    // The range has to added in two parts.
    gld_clipper_AddClipRange(startAngle, ANGLE_MAX);
    gld_clipper_AddClipRange(0, endAngle);
  end
  else
  begin
    // Add the range as usual.
    gld_clipper_AddClipRange(startAngle, endAngle);
  end;
end;

//==============================================================================
//
// gld_clipper_Clear
//
//==============================================================================
procedure gld_clipper_Clear;
var
  node: Pclipnode_t;
  temp: Pclipnode_t;
begin
  node := cliphead;
  while node <> nil do
  begin
    temp := node;
    node := node.next;
    gld_clipnode_Free(temp);
  end;

  cliphead := nil;
end;

//==============================================================================
//
// gld_FrustumAngle
//
//==============================================================================
function gld_FrustumAngle: angle_t;
var
  tilt: single;
begin
  tilt := abs(viewplayer.lookdir16 / 32); // JVAL Smooth Look Up/Down

  // If the pitch is larger than this you can look all around at a FOV of 90
  if tilt > 46.0 then
  begin
    result := ANGLE_MAX;
    exit;
  end;

  // ok, this is a gross hack that barely works...
  // but at least it doesn't overestimate too much...
  result := round(ANG1 * (2.0 + (45.0 * monitor_relative_aspect + (tilt / 1.9))));
  if result >= ANG180 then
    result := ANGLE_MAX;
end;

//==============================================================================
//
// gld_ClipperAddViewRange
//
//==============================================================================
procedure gld_ClipperAddViewRange;
var
  a1: angle_t;
begin
  a1 := gld_FrustumAngle;
  gld_clipper_Clear;
  gld_clipper_SafeAddClipRange(viewangle + a1, viewangle - a1);
end;

//==============================================================================
//
// gld_ClipperDone
//
//==============================================================================
procedure gld_ClipperDone;
var
  node: Pclipnode_t;
  temp: Pclipnode_t;
begin
  node := cliphead;
  while node <> nil do
  begin
    temp := node;
    node := node.next;
    memfree(pointer(temp), SizeOf(clipnode_t));
  end;

  cliphead := nil;
end;

end.
