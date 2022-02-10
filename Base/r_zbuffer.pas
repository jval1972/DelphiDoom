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
//  Z-buffer struct
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_zbuffer;

interface

uses
  doomdef,
  p_mobj_h,
  r_defs,
  r_column,
  r_span;

type
  zbufferitem_t = record
    start, stop: integer;
    depth: LongWord;
    seg: Pseg_t;
    mo: Pmobj_t;
    pass: Integer;
    planeheightz: Integer;
  end;
  Pzbufferitem_t = ^zbufferitem_t;
  zbufferitem_tArray = array[0..$FF] of zbufferitem_t;
  Pzbufferitem_tArray = ^zbufferitem_tArray;

  zbuffer_t = record
    items: Pzbufferitem_tArray;
    numitems: integer;
    numrealitems: integer;
  end;
  Pzbuffer_t = ^zbuffer_t;

var
  Zspans: array[0..MAXHEIGHT] of zbuffer_t;
  Zcolumns: array[0..MAXWIDTH] of zbuffer_t;

//==============================================================================
//
// R_DrawSpanToZBuffer
//
//==============================================================================
procedure R_DrawSpanToZBuffer;

//==============================================================================
//
// R_DrawSlopeToZBuffer
//
//==============================================================================
procedure R_DrawSlopeToZBuffer;

//==============================================================================
//
// R_DrawColumnToZBuffer
//
//==============================================================================
procedure R_DrawColumnToZBuffer;

//==============================================================================
//
// R_DrawMaskedColumnToZBuffer
//
//==============================================================================
procedure R_DrawMaskedColumnToZBuffer(const mo: Pmobj_t; const depth: LongWord);

//==============================================================================
//
// R_DrawBatchMaskedColumnToZBuffer
//
//==============================================================================
procedure R_DrawBatchMaskedColumnToZBuffer(const mo: Pmobj_t; const depth: LongWord);

//==============================================================================
//
// R_DrawVoxelPixelToZBuffer
//
//==============================================================================
procedure R_DrawVoxelPixelToZBuffer(const depth: LongWord; const x, y: integer; const mo: Pmobj_t);

//==============================================================================
//
// R_DrawVoxelColumnToZBuffer
//
//==============================================================================
procedure R_DrawVoxelColumnToZBuffer(const depth: LongWord; const mo: Pmobj_t);

//==============================================================================
//
// R_DrawBatchVoxelColumnToZBuffer
//
//==============================================================================
procedure R_DrawBatchVoxelColumnToZBuffer(const depth: LongWord; const mo: Pmobj_t);

//==============================================================================
// R_ZBufferAt
//
// Returns the z buffer value at (x, y) or screen
// Lower value means far away
// no z-buffer is sky (or render glitch) - we do not write o zbuffer in skycolfunc
//
//==============================================================================
function R_ZBufferAt(const x, y: integer): Pzbufferitem_t;

//==============================================================================
//
// R_InitZBuffer
//
//==============================================================================
procedure R_InitZBuffer;

//==============================================================================
//
// R_ShutDownZBuffer
//
//==============================================================================
procedure R_ShutDownZBuffer;

//==============================================================================
//
// R_StartZBuffer
//
//==============================================================================
procedure R_StartZBuffer;

//==============================================================================
//
// R_StopZBuffer
//
//==============================================================================
procedure R_StopZBuffer;

//==============================================================================
//
// R_ClearZBuffer
//
//==============================================================================
procedure R_ClearZBuffer;

var
  zbufferactive: boolean = true;

implementation

uses
  d_delphi,
  {$IFDEF DEBUG}
  i_system,
  {$ENDIF}
  c_cmds,
  m_fixed,
  r_batchcolumn,
  r_bsp,
  r_draw,
  r_plane,
  r_main,
  t_png;

//==============================================================================
//
// R_NewZBufferItem
//
//==============================================================================
function R_NewZBufferItem(const Z: Pzbuffer_t): Pzbufferitem_t;
const
  GROWSTEP = 4;
begin
  if Z.numitems >= Z.numrealitems then
  begin
    realloc(pointer(Z.items), Z.numrealitems * SizeOf(zbufferitem_t), (Z.numrealitems + GROWSTEP) * SizeOf(zbufferitem_t));
    Z.numrealitems := Z.numrealitems + GROWSTEP;
  end;
  result := @Z.items[Z.numitems];
  inc(Z.numitems);
end;

//==============================================================================
//
// R_DrawSpanToZBuffer
//
//==============================================================================
procedure R_DrawSpanToZBuffer;
var
  item: Pzbufferitem_t;
begin
{$IFDEF DEBUG}
  if not IsIntegerInRange(ds_y, 0, viewheight - 1) then
    I_Warning('R_DrawSpanToZBuffer(): ds_y=%d not in range [0..viewheight(=%d) - 1]'#13#10, [ds_y, viewheight]);
  if not IsIntegerInRange(ds_x1, 0, viewwidth - 1) then
    I_Warning('R_DrawSpanToZBuffer(): ds_x1=%d not in range [0..viewwidth(=%d) - 1]'#13#10, [ds_x1, viewwidth]);
  if not IsIntegerInRange(ds_x2, 0, viewwidth - 1) then
    I_Warning('R_DrawSpanToZBuffer(): ds_x2=%d not in range [0..viewwidth(=%d) - 1]'#13#10, [ds_x2, viewwidth]);
  if ds_x2 < ds_x1 then
    I_Warning('R_DrawSpanToZBuffer(): ds_x2=%d < ds_x1=%d'#13#10, [ds_x2, ds_x1]);
{$ENDIF}

  item := R_NewZBufferItem(@Zspans[ds_y]);

  if ds_y = centery then
    item.depth := 0
  else
    item.depth := Round(FRACUNIT / (planeheight / abs(centery - ds_y)) * FRACUNIT);

  item.seg := nil;
  item.mo := nil;
  item.pass := 0;

  item.start := ds_x1;
  item.stop := ds_x2;
  item.planeheightz := planeheightz;
end;

//==============================================================================
//
// R_DrawSlopeToZBuffer
//
//==============================================================================
procedure R_DrawSlopeToZBuffer;
var
  item: Pzbufferitem_t;
  ddy: integer;
begin
{$IFDEF DEBUG}
  if not IsIntegerInRange(ds_y, 0, viewheight - 1) then
    I_Warning('R_DrawSlopeToZBuffer(): ds_y=%d not in range [0..viewheight(=%d) - 1]'#13#10, [ds_y, viewheight]);
  if not IsIntegerInRange(ds_x1, 0, viewwidth - 1) then
    I_Warning('R_DrawSlopeToZBuffer(): ds_x1=%d not in range [0..viewwidth(=%d) - 1]'#13#10, [ds_x1, viewwidth]);
  if not IsIntegerInRange(ds_x2, 0, viewwidth - 1) then
    I_Warning('R_DrawSlopeToZBuffer(): ds_x2=%d not in range [0..viewwidth(=%d) - 1]'#13#10, [ds_x2, viewwidth]);
  if ds_x2 < ds_x1 then
    I_Warning('R_DrawSlopeToZBuffer(): ds_x2=%d < ds_x1=%d'#13#10, [ds_x2, ds_x1]);
{$ENDIF}

  item := R_NewZBufferItem(@Zspans[ds_y]);

  ddy := abs(centery - ds_y);
  if ddy < 4 then
    item.depth := Round(FRACUNIT / (planeheight / 4) * FRACUNIT)
  else
    item.depth := Round(FRACUNIT / (planeheight / ddy) * FRACUNIT);

  item.seg := nil;
  item.mo := nil;
  item.pass := 0;

  item.start := ds_x1;
  item.stop := ds_x2;
  item.planeheightz := MAXINT;
end;

//==============================================================================
//
// R_DrawColumnToZBuffer
//
//==============================================================================
procedure R_DrawColumnToZBuffer;
var
  item: Pzbufferitem_t;
begin
{$IFDEF DEBUG}
  if not IsIntegerInRange(dc_x, 0, viewwidth - 1) then
    I_Warning('R_DrawColumnToZBuffer(): ds_x=%d not in range [0..viewwidth(=%d) - 1]'#13#10, [dc_x, viewwidth]);
  if not IsIntegerInRange(dc_yl, 0, viewheight - 1) then
    I_Warning('R_DrawColumnToZBuffer(): ds_yl=%d not in range [0..viewheight(=%d) - 1]'#13#10, [dc_yl, viewheight]);
  if not IsIntegerInRange(dc_yh, 0, viewwidth - 1) then
    I_Warning('R_DrawColumnToZBuffer(): ds_yh=%d not in range [0..viewheight(=%d) - 1]'#13#10, [dc_yh, viewheight]);
  if dc_yh < dc_yl then
    I_Warning('R_DrawColumnToZBuffer(): dc_yh=%d < dc_yl=%d'#13#10, [dc_yh, dc_yl]);
{$ENDIF}

  item := R_NewZBufferItem(@Zcolumns[dc_x]);

  item.depth := trunc((FRACUNIT / dc_iscale) * FRACUNIT);
  item.seg := curline;
  item.mo := nil;
  item.pass := 0;

  item.start := dc_yl;
  item.stop := dc_yh;
end;

//==============================================================================
//
// R_DrawMaskedColumnToZBuffer
//
//==============================================================================
procedure R_DrawMaskedColumnToZBuffer(const mo: Pmobj_t; const depth: LongWord);
var
  item: Pzbufferitem_t;
begin
{$IFDEF DEBUG}
  if not IsIntegerInRange(dc_x, 0, viewwidth - 1) then
    I_Warning('R_DrawMaskedColumnToZBuffer(): ds_x=%d not in range [0..viewwidth(=%d) - 1]'#13#10, [dc_x, viewwidth]);
  if not IsIntegerInRange(dc_yl, 0, viewheight - 1) then
    I_Warning('R_DrawMaskedColumnToZBuffer(): ds_yl=%d not in range [0..viewheight(=%d) - 1]'#13#10, [dc_yl, viewheight]);
  if not IsIntegerInRange(dc_yh, 0, viewwidth - 1) then
    I_Warning('R_DrawMaskedColumnToZBuffer(): ds_yh=%d not in range [0..viewheight(=%d) - 1]'#13#10, [dc_yh, viewheight]);
  if dc_yh < dc_yl then
    I_Warning('R_DrawMaskedColumnToZBuffer(): dc_yh=%d < dc_yl=%d'#13#10, [dc_yh, dc_yl]);
{$ENDIF}

  item := R_NewZBufferItem(@Zcolumns[dc_x]);

  item.depth := depth;
  item.seg := nil;
  item.mo := mo;
  item.pass := 1;

  item.start := dc_yl;
  item.stop := dc_yh;
end;

//==============================================================================
//
// R_DrawBatchMaskedColumnToZBuffer
//
//==============================================================================
procedure R_DrawBatchMaskedColumnToZBuffer(const mo: Pmobj_t; const depth: LongWord);
var
  item: Pzbufferitem_t;
  i: integer;
begin
{$IFDEF DEBUG}
  if not IsIntegerInRange(dc_x, 0, viewwidth - 1) then
    I_Warning('R_DrawBatchMaskedColumnToZBuffer(): ds_x=%d not in range [0..viewwidth(=%d) - 1]'#13#10, [dc_x, viewwidth]);
  if not IsIntegerInRange(dc_yl, 0, viewheight - 1) then
    I_Warning('R_DrawBatchMaskedColumnToZBuffer(): ds_yl=%d not in range [0..viewheight(=%d) - 1]'#13#10, [dc_yl, viewheight]);
  if not IsIntegerInRange(dc_yh, 0, viewwidth - 1) then
    I_Warning('R_DrawBatchMaskedColumnToZBuffer(): ds_yh=%d not in range [0..viewheight(=%d) - 1]'#13#10, [dc_yh, viewheight]);
  if dc_yh < dc_yl then
    I_Warning('R_DrawBatchMaskedColumnToZBuffer(): dc_yh=%d < dc_yl=%d'#13#10, [dc_yh, dc_yl]);
{$ENDIF}

  for i := dc_x to dc_x + num_batch_columns - 1 do
  begin
    item := R_NewZBufferItem(@Zcolumns[i]);

    item.depth := depth;
    item.seg := nil;
    item.mo := mo;
    item.pass := 1;

    item.start := dc_yl;
    item.stop := dc_yh;
  end;
end;

//==============================================================================
//
// R_DrawVoxelPixelToZBuffer
//
//==============================================================================
procedure R_DrawVoxelPixelToZBuffer(const depth: LongWord; const x, y: integer; const mo: Pmobj_t);
var
  item: Pzbufferitem_t;
begin
{$IFDEF DEBUG}
  if not IsIntegerInRange(x, 0, viewwidth - 1) then
    I_Warning('R_DrawVoxelPixelToZBuffer(): ds_x=%d not in range [0..viewwidth(=%d) - 1]'#13#10, [x, viewwidth]);
  if not IsIntegerInRange(y, 0, viewheight - 1) then
    I_Warning('R_DrawVoxelPixelToZBuffer(): ds_yl=%d not in range [0..viewheight(=%d) - 1]'#13#10, [y, viewheight]);
{$ENDIF}

  item := R_NewZBufferItem(@Zcolumns[x]);

  item.depth := depth;
  item.seg := nil;
  item.mo := mo;
  item.pass := 1;

  item.start := y;
  item.stop := y;
end;

//==============================================================================
//
// R_DrawVoxelColumnToZBuffer
//
//==============================================================================
procedure R_DrawVoxelColumnToZBuffer(const depth: LongWord; const mo: Pmobj_t);
var
  Z: Pzbuffer_t;
  item: Pzbufferitem_t;
begin
{$IFDEF DEBUG}
  if not IsIntegerInRange(dc_x, 0, viewwidth - 1) then
    I_Warning('R_DrawVoxelColumnToZBuffer(): ds_x=%d not in range [0..viewwidth(=%d) - 1]'#13#10, [dc_x, viewwidth]);
  if not IsIntegerInRange(dc_yl, 0, viewheight - 1) then
    I_Warning('R_DrawVoxelColumnToZBuffer(): ds_yl=%d not in range [0..viewheight(=%d) - 1]'#13#10, [dc_yl, viewheight]);
  if not IsIntegerInRange(dc_yh, 0, viewwidth - 1) then
    I_Warning('R_DrawVoxelColumnToZBuffer(): ds_yh=%d not in range [0..viewheight(=%d) - 1]'#13#10, [dc_yh, viewheight]);
  if dc_yh < dc_yl then
    I_Warning('R_DrawVoxelColumnToZBuffer(): dc_yh=%d < dc_yl=%d'#13#10, [dc_yh, dc_yl]);
{$ENDIF}

  Z := @Zcolumns[dc_x];
  if Z.numitems > 0 then
  begin
    item := @Z.items[Z.numitems - 1];
    if item.seg = nil then
      if item.mo = mo then
        if depth = item.depth then
          if dc_yl <= item.stop + 1 then
            if dc_yl >= item.start then
            begin
              if dc_yh <= item.stop then
                Exit;
              item.stop := dc_yh;
              Exit;
            end;
  end;

  item := R_NewZBufferItem(Z);

  item.depth := depth;
  item.seg := nil;
  item.mo := mo;
  item.pass := 1;

  item.start := dc_yl;
  item.stop := dc_yh;
end;

//==============================================================================
//
// R_DrawBatchVoxelColumnToZBuffer
//
//==============================================================================
procedure R_DrawBatchVoxelColumnToZBuffer(const depth: LongWord; const mo: Pmobj_t);
var
  Z: Pzbuffer_t;
  item: Pzbufferitem_t;
  i: integer;
begin
{$IFDEF DEBUG}
  if not IsIntegerInRange(dc_x, 0, viewwidth - 1) then
    I_Warning('R_DrawBatchVoxelColumnToZBuffer(): ds_x=%d not in range [0..viewwidth(=%d) - 1]'#13#10, [dc_x, viewwidth]);
  if not IsIntegerInRange(dc_yl, 0, viewheight - 1) then
    I_Warning('R_DrawBatchVoxelColumnToZBuffer(): ds_yl=%d not in range [0..viewheight(=%d) - 1]'#13#10, [dc_yl, viewheight]);
  if not IsIntegerInRange(dc_yh, 0, viewwidth - 1) then
    I_Warning('R_DrawBatchVoxelColumnToZBuffer(): ds_yh=%d not in range [0..viewheight(=%d) - 1]'#13#10, [dc_yh, viewheight]);
  if dc_yh < dc_yl then
    I_Warning('R_DrawBatchVoxelColumnToZBuffer(): dc_yh=%d < dc_yl=%d'#13#10, [dc_yh, dc_yl]);
{$ENDIF}

  for i := dc_x to dc_x + num_batch_columns - 1 do
  begin
    Z := @Zcolumns[i];
    if Z.numitems > 0 then
    begin
      item := @Z.items[Z.numitems - 1];
      if item.seg = nil then
        if item.mo = mo then
          if depth = item.depth then
            if dc_yl <= item.stop + 1 then
              if dc_yl >= item.start then
              begin
                if dc_yh <= item.stop then
                  Continue;
                item.stop := dc_yh;
                Continue;
              end;
    end;

    item := R_NewZBufferItem(Z);

    item.depth := depth;
    item.seg := nil;
    item.mo := mo;
    item.pass := 1;

    item.start := dc_yl;
    item.stop := dc_yh;
  end;
end;

var
  stubzitem: zbufferitem_t = (
    start: 0;
    stop: 0;
    depth: $00000000;
    seg: nil;
    mo: nil;
    pass: 0;
    planeheightz: 0;
  );

//==============================================================================
//
// R_ZBufferAt
//
//==============================================================================
function R_ZBufferAt(const x, y: integer): Pzbufferitem_t;
var
  Z: Pzbuffer_t;
  pi, pistop: Pzbufferitem_t;
  maxdepth, depth: LongWord;
begin
  result := @stubzitem;
  maxdepth := 0;

  Z := @Zcolumns[x];
  pi := @Z.items[0];
  pistop := @Z.items[Z.numitems];
  while pi <> pistop do
  begin
    if (y >= pi.start) and (y <= pi.stop) then
    begin
      depth := pi.depth;
      if depth > maxdepth then
      begin
        result := pi;
        maxdepth := depth;
      end;
    end;
    inc(pi);
  end;

  Z := @Zspans[y];
  pi := @Z.items[0];
  pistop := @Z.items[Z.numitems];
  while pi <> pistop do
  begin
    if (x >= pi.start) and (x <= pi.stop) then
    begin
      depth := pi.depth;
      if depth > maxdepth then
      begin
        result := pi;
        maxdepth := depth;
      end;
    end;
    inc(pi);
  end;
end;

var
  export_zbuffer: boolean = false;
  export_zbuffer_path: string;

//==============================================================================
//
// CmdExportZBuffer
//
//==============================================================================
procedure CmdExportZBuffer(const fname: string);
begin
  if fname = '' then
  begin
    printf('Please specify the PNG filename to save zbuffer'#13#10);
    exit;
  end;

  if strupper(fext(fname)) <> '.PNG' then
    export_zbuffer_path := fname + '.png'
  else
    export_zbuffer_path := fname;
  export_zbuffer := true;
end;

//==============================================================================
//
// DoExportZBuffer
//
//==============================================================================
procedure DoExportZBuffer;
var
  png: TPngObject;
  row, c: integer;
  lpng: PByteArray;
  bufsize: integer;
  buf, lbuf: PLongWordArray;
  x, y: integer;
  z, maxz: LongWord;
  grey: LongWord;
begin
  bufsize := SCREENWIDTH * SCREENHEIGHT * 4;
  buf := malloc(bufsize);

  maxz := 0;
  for x := 0 to SCREENWIDTH - 1 do
    for y := 0 to SCREENHEIGHT - 1 do
    begin
      z := R_ZBufferAt(x, y).depth;
      if z > maxz then
        maxz := z;
      buf[y * SCREENWIDTH + x] := z;
    end;

  png := TPngObject.CreateBlank(COLOR_RGB, 8, SCREENWIDTH, SCREENHEIGHT);
  try
    for row := 0 to SCREENHEIGHT - 1 do
    begin
      lpng := png.Scanline[row];
      lbuf := @buf[row * SCREENWIDTH];
      for c := 0 to SCREENWIDTH - 1 do
      begin
        grey := Trunc(Sqrt(lbuf[c] / (maxz + 1)) * 255);
        lpng[c * 3] := grey;
        lpng[c * 3 + 1] := grey;
        lpng[c * 3 + 2] := grey;
      end;
    end;
    png.SaveToFile(export_zbuffer_path);
  finally
    png.Free;
  end;
  memfree(pointer(buf), bufsize);
end;

//==============================================================================
//
// R_InitZBuffer
//
//==============================================================================
procedure R_InitZBuffer;
begin
  ZeroMemory(@Zspans, SizeOf(Zspans));
  ZeroMemory(@Zcolumns, SizeOf(Zcolumns));
  C_AddCmd('ExportZBuffer, ZBufferExport', @CmdExportZBuffer);
end;

//==============================================================================
//
// R_ShutDownZBuffer
//
//==============================================================================
procedure R_ShutDownZBuffer;
var
  i: integer;
begin
  for i := 0 to MAXWIDTH do
    if Zcolumns[i].numrealitems > 0 then
    begin
      memfree(pointer(Zcolumns[i].items), Zcolumns[i].numrealitems * SizeOf(zbufferitem_t));
      Zcolumns[i].numrealitems := 0;
      Zcolumns[i].numitems := 0;
    end;

  for i := 0 to MAXHEIGHT do
    if Zspans[i].numrealitems > 0 then
    begin
      memfree(pointer(Zspans[i].items), Zspans[i].numrealitems * SizeOf(zbufferitem_t));
      Zspans[i].numrealitems := 0;
      Zspans[i].numitems := 0;
    end;
end;

//==============================================================================
//
// R_StartZBuffer
//
//==============================================================================
procedure R_StartZBuffer;
begin
end;

//==============================================================================
//
// R_StopZBuffer
//
//==============================================================================
procedure R_StopZBuffer;
begin
  if export_zbuffer then
  begin
    DoExportZBuffer;
    export_zbuffer := false;
  end;

  R_ClearZBuffer;
end;

//==============================================================================
//
// R_ClearZBuffer
//
//==============================================================================
procedure R_ClearZBuffer;
var
  i: integer;
begin
  for i := 0 to viewwidth do
    Zcolumns[i].numitems := 0;
  for i := 0 to viewheight do
    Zspans[i].numitems := 0;
end;

end.

