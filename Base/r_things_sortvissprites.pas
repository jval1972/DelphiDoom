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
//  Vissprite sort (Selection sort, Quick sort & Radix sort variant)
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_things_sortvissprites;

interface

//==============================================================================
//
// R_SortVisSprites
//
//==============================================================================
procedure R_SortVisSprites;

//==============================================================================
//
// R_SortVisSpritesMT
//
//==============================================================================
procedure R_SortVisSpritesMT;

//==============================================================================
//
// R_InitSpriteSort
//
//==============================================================================
procedure R_InitSpriteSort;

//==============================================================================
//
// R_ShutDownSpriteSort
//
//==============================================================================
procedure R_ShutDownSpriteSort;

implementation

uses
  d_delphi,
{$IFDEF DEBUG}
  i_system,
{$ENDIF}
  m_fixed,
  p_mobj_h,
  r_defs,
  r_things;

//==============================================================================
// getvissortscale
//
// R_SortVisSprites
//
//==============================================================================
function getvissortscale(const vis: Pvissprite_t): integer;
begin
  result := vis.scale;
  {$IFDEF HEXEN}
  if vis.mo.flags2 and MF2_DROPPED <> 0 then
  {$ELSE}
  if vis.mobjflags and MF_DROPPED <> 0 then
  {$ENDIF}
    inc(result);
end;

//==============================================================================
//
// R_SortVisSprites_SelectionSort
// Same execution speed or faster than quicksort for small vissprite_p values
//
//==============================================================================
procedure R_SortVisSprites_SelectionSort;
var
  i, j: integer;
  t: Pvissprite_t;
begin
  for i := 0 to vissprite_p - 2 do
    for j := vissprite_p - 1 downto i + 1 do
      if getvissortscale(vissprites[i]) > getvissortscale(vissprites[j]) then
      begin
        t := vissprites[i];
        vissprites[i] := vissprites[j];
        vissprites[j] := t;
      end;
end;

//==============================================================================
//
// R_SortVisSprites_QSort
//
//==============================================================================
procedure R_SortVisSprites_QSort;

  procedure qsortvs(l, r: Integer);
  var
    i, j: Integer;
    t: Pvissprite_t;
    scale: fixed_t;
  begin
    repeat
      i := l;
      j := r;
      scale := getvissortscale(vissprites[(l + r) shr 1]);
      repeat
        while getvissortscale(vissprites[i]) < scale do
          inc(i);
        while getvissortscale(vissprites[j]) > scale do
          dec(j);
        if i <= j then
        begin
          t := vissprites[i];
          vissprites[i] := vissprites[j];
          vissprites[j] := t;
          inc(i);
          dec(j);
        end;
      until i > j;
      if l < j then
        qsortvs(l, j);
      l := i;
    until i >= r;
  end;

begin
  if vissprite_p > 1 then
    qsortvs(0, vissprite_p - 1);
end;

//
// R_SortVisSprites_RadixSort
//

// JVAL 20180914
//   This is a fast Radix Sort variant optimized to work on positive
//   integer values (range [0..MAXINT]), such as a vissprite scale.
//   We represent each value with 3 virtual digits:
//   The 2 least significant "digits" are base 2048 (11 bits) and the
//   most significant "digit" (3rd) is base 1024 (10 bits).
//   Special care has been taken in case that the sort keys are in range 1..2^22
//   This is faster than quicksort if we have more than 500~1000 vissprites.
//   Sorting 5000 - 10000 vissprites takes 1/4 of time compared to quicksort.
//
//
const
  RADIX_BASE = 2048;
  RADIX_MASK = RADIX_BASE - 1;

var
  radix_index_buf: array[0..5 * RADIX_BASE div 2 - 1] of integer;
  vis_buf1: visspritebuffer_p = nil;
  vis_buf2: visspritebuffer_p = nil;
  vis_buf_size1: integer = 0;
  vis_buf_size2: integer = 0;

//==============================================================================
//
// R_SortVisSprites_RadixSort
//
//==============================================================================
procedure R_SortVisSprites_RadixSort;
var
  i, j: integer;
  n1, n2, n3, tot: integer;
  idx1, idx2, idx3: PIntegerArray;
  pi: PInteger;
  scale: LongWord;
begin
  if vissprite_p > vis_buf_size1 then
  begin
    realloc(pointer(vis_buf1), vis_buf_size1 * SizeOf(Pvissprite_t), (128 +
      vissprite_p) * SizeOf(Pvissprite_t));
    vis_buf_size1 := vissprite_p + 128;
  end;

  idx1 := @radix_index_buf;
  idx2 := @idx1[RADIX_BASE];
  idx3 := @idx2[RADIX_BASE];

  FillChar(idx1^, 5 * RADIX_BASE * SizeOf(integer) div 2, 0);
  for i := 0 to vissprite_p - 1 do
  begin
    scale := LongWord(getvissortscale(vissprites[i]));
    inc(idx1[scale and RADIX_MASK]);
    inc(idx2[(scale shr 11) and RADIX_MASK]);
    inc(idx3[scale shr 22]);
  end;

  if idx3[0] = vissprite_p then // 2-pass
  begin
    n1 := 0;
    n2 := 0;

    for j := 0 to RADIX_BASE - 1 do
    begin
      pi := @idx1[j];
      tot := pi^ + n1;
      pi^ := n1 - 1;
      n1 := tot;

      inc(pi, RADIX_BASE);
      tot := pi^ + n2;
      pi^ := n2 - 1;
      n2 := tot;
    end;

    // First Pass
    // From vissprites to vis_buf1
    for i := 0 to vissprite_p - 1 do
    begin
      pi := @idx1[LongWord(getvissortscale(vissprites[i])) and RADIX_MASK];
      inc(pi^);
      vis_buf1[pi^] := vissprites[i];
    end;

    // Second Pass
    // From vis_buf1 back to vissprites
    for i := 0 to vissprite_p - 1 do
    begin
      pi := @idx2[(LongWord(getvissortscale(vis_buf1[i])) shr 11) and RADIX_MASK];
      inc(pi^);
      vissprites[pi^] := vis_buf1[i];
    end;
  end
  else // 3-pass (should rarely happen - when we have sprites very close to view)
  begin
    // We must realloc the second buffer
    if vissprite_p > vis_buf_size2 then
    begin
      realloc(pointer(vis_buf2), vis_buf_size2 * SizeOf(Pvissprite_t), (128 +
        vissprite_p) * SizeOf(Pvissprite_t));
      vis_buf_size2 := vissprite_p + 128;
    end;

    n1 := 0;
    n2 := 0;
    n3 := 0;

    for j := 0 to RADIX_BASE div 2 - 1 do
    begin
      pi := @idx1[j];
      tot := pi^ + n1;
      pi^ := n1 - 1;
      n1 := tot;

      inc(pi, RADIX_BASE);
      tot := pi^ + n2;
      pi^ := n2 - 1;
      n2 := tot;

      inc(pi, RADIX_BASE);
      tot := pi^ + n3;
      pi^ := n3 - 1;
      n3 := tot;
    end;

    for j := RADIX_BASE div 2 to RADIX_BASE - 1 do
    begin
      pi := @idx1[j];
      tot := pi^ + n1;
      pi^ := n1 - 1;
      n1 := tot;

      inc(pi, RADIX_BASE);
      tot := pi^ + n2;
      pi^ := n2 - 1;
      n2 := tot;
    end;

    // First Pass
    // From vissprites to vis_buf1
    for i := 0 to vissprite_p - 1 do
    begin
      pi := @idx1[LongWord(getvissortscale(vissprites[i])) and RADIX_MASK];
      inc(pi^);
      vis_buf1[pi^] := vissprites[i];
    end;

    // Second Pass
    // From vis_buf1 to vis_buf2
    for i := 0 to vissprite_p - 1 do
    begin
      pi := @idx2[(LongWord(getvissortscale(vis_buf1[i])) shr 11) and RADIX_MASK];
      inc(pi^);
      vis_buf2[pi^] := vis_buf1[i];
    end;

    // Third Pass
    // From vis_buf2 back to vissprites
    for i := 0 to vissprite_p - 1 do
    begin
      pi := @idx3[LongWord(getvissortscale(vis_buf2[i])) shr 22];
      inc(pi^);
      vissprites[pi^] := vis_buf2[i];
    end;
  end;

  {$IFDEF DEBUG}
  for i := 0 to vissprite_p - 2 do
    if getvissortscale(vissprites[i]) > getvissortscale(vissprites[i + 1]) then
    begin
      I_Warning('R_SortVisSprites_RadixSort(): Validation failed at %d'#13#10, [i]);
      exit;
    end;
  {$ENDIF}

end;

//==============================================================================
//
// R_SortVisSprites_MergeSort
//
// Algorithm from http://alexandrecmachado.blogspot.com.br/2015/02/merge-sort-for-delphi.html
//
//==============================================================================
procedure R_SortVisSprites_MergeSort;
var
  xTempListSize: Integer;

  procedure DoInsertionSort(ptrList: visspritebuffer_p; FirstIndex: Integer; LastIndex: Integer);
  var
    i, j: Integer;
    t: Pvissprite_t;
  begin
    for i := FirstIndex + 1 to LastIndex do
    begin
      t := ptrList[i];
      j := i;
      while (j > FirstIndex) and (t.scale < ptrList[j - 1].scale) do
      begin
        ptrList[j] := ptrList[j - 1];
        dec(j);
      end;
      ptrList[j] := t;
    end;
  end;

  procedure DoMergeSort(ptrList: visspritebuffer_p; FirstIndex: Integer; LastIndex: Integer);
  const
    // When the list is smaller than this we use InsertionSort instead of calling MergeSort recursively.
    // 8 and 64 seem to be the lower and upper limits where the performance degrades, so
    // something between 16 and 32 probably gives the best performance
    MIN_LIST_SIZE = 16;
  var
    Mid: Integer;
    i, j: Integer;
    ToInx: Integer;
    FirstCount: Integer;
  begin
    // calculate the midpoint
    Mid := (FirstIndex + LastIndex) div 2;
    // sort the 1st half of the list, either with merge sort, or, if there are few enough items, with insertion sort
    if FirstIndex < Mid then
    begin
      if Mid - FirstIndex <= MIN_LIST_SIZE then
        DoInsertionSort(ptrList, FirstIndex, Mid)
      else
        DoMergeSort(ptrList, FirstIndex, Mid);
    end;
    // sort the 2nd half of the list likewise
    if (Mid + 1) < LastIndex then
    begin
      if (LastIndex - Mid - 1) <= MIN_LIST_SIZE then
        DoInsertionSort(ptrList, succ(Mid), LastIndex)
      else
        DoMergeSort(ptrList, succ(Mid), LastIndex);
    end;
    // copy the first half of the list to our temporary list
    FirstCount := Mid - FirstIndex + 1;
    memcpy(@vis_buf1[0], @ptrList[FirstIndex], FirstCount * SizeOf(Pvissprite_t));
    // set up the indexes: i is the index for the temporary list (i.e., the
    //  first half of the list), j is the index for the second half of the
    //  list, ToInx is the index in the merged where items will be copied
    i := 0;
    j := Mid + 1;
    ToInx := FirstIndex;
    // now merge the two lists
    // repeat until one of the lists empties...
    while (i < FirstCount) and (j <= LastIndex) do
    begin
       // calculate the smaller item from the next items in both lists and copy it over; increment the relevant index
      if vis_buf1[i].scale <= ptrList[j].scale then
      begin
        ptrList[ToInx] := vis_buf1[i];
        inc(i);
      end
      else
      begin
        ptrList[ToInx] := ptrList[j];
        inc(j);
      end;
      // there's one more item in the merged list
      inc(ToInx);
    end;
    // if there are any more items in the first list, copy them back over
    if i < FirstCount then
      memcpy(@ptrList[ToInx], @vis_buf1[i], (FirstCount - i) * SizeOf(Pvissprite_t));
    // if there are any more items in the second list then they're already in place and we're done; if there aren't, we're still done
  end;

begin
  if vissprite_p < 2 then
    Exit;

  xTempListSize := (vissprite_p div 2) + 1;
  if xTempListSize > vis_buf_size1 then
  begin
    realloc(pointer(vis_buf1), vis_buf_size1 * SizeOf(Pvissprite_t), (128 +
      xTempListSize) * SizeOf(Pvissprite_t));
    vis_buf_size1 := xTempListSize + 128;
  end;

  DoMergeSort(vissprites, 0, vissprite_p - 1);
end;

//==============================================================================
//
// R_SortVisSprites
//
//==============================================================================
procedure R_SortVisSprites;
begin
  if vissprite_p > 1024 then
    R_SortVisSprites_RadixSort
  else if vissprite_p > 512 then
    R_SortVisSprites_MergeSort
  else if vissprite_p > 32 then
    R_SortVisSprites_QSort
  else
    R_SortVisSprites_SelectionSort;
end;

//
// Sorting while the engine does other things, using a separate thread (idea by zokum)
// https://www.doomworld.com/forum/topic/102482-potential-for-improvement-in-vissprites-sorting/?do=findComment&comment=1920806
//

//==============================================================================
//
// R_SortVisSpritesMT
//
//==============================================================================
procedure R_SortVisSpritesMT;
begin
  // Allocating temp space before activating the thread.
  // This is to speed up FastMM memory manager, since we don't have to
  // set the 'ismultithread' flag to true.
  if vissprite_p > vis_buf_size1 then
  begin
    realloc(pointer(vis_buf1), vis_buf_size1 * SizeOf(Pvissprite_t), (128 +
      vissprite_p) * SizeOf(Pvissprite_t));
    vis_buf_size1 := vissprite_p + 128;
  end;
  if vissprite_p > vis_buf_size2 then
  begin
    realloc(pointer(vis_buf2), vis_buf_size2 * SizeOf(Pvissprite_t), (128 +
      vissprite_p) * SizeOf(Pvissprite_t));
    vis_buf_size2 := vissprite_p + 128;
  end;
end;

//==============================================================================
//
// R_InitSpriteSort
//
//==============================================================================
procedure R_InitSpriteSort;
begin
  vis_buf1 := nil;
  vis_buf2 := nil;
  vis_buf_size1 := 0;
  vis_buf_size2 := 0;
end;

//==============================================================================
//
// R_ShutDownSpriteSort
//
//==============================================================================
procedure R_ShutDownSpriteSort;
begin
  if vis_buf_size1 > 0 then
  begin
    realloc(pointer(vis_buf1), vis_buf_size1 * SizeOf(Pvissprite_t), 0);
    vis_buf_size1 := 0;
  end;
  if vis_buf_size2 > 0 then
  begin
    realloc(pointer(vis_buf2), vis_buf_size2 * SizeOf(Pvissprite_t), 0);
    vis_buf_size2 := 0;
  end;
end;

end.
