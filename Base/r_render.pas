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
//  Multithreading software rendering
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_render;

interface

uses
  mt_utils;

//==============================================================================
//
// R_InitMultiThreadRender
//
//==============================================================================
procedure R_InitMultiThreadRender;

//==============================================================================
//
// R_ShutDownMultiThreadRender
//
//==============================================================================
procedure R_ShutDownMultiThreadRender;

const
  MAXRENDERINGTHREADS = 256;

type
  renderparams_t = record
    wallparams8: array[0..MAXRENDERINGTHREADS - 1] of mt_linkedrange_t;
    wallparams32: array[0..MAXRENDERINGTHREADS - 1] of mt_linkedrange_t;
    flatparams8: array[0..MAXRENDERINGTHREADS - 1] of mt_linkedrange_t;
    flatparams32: array[0..MAXRENDERINGTHREADS - 1] of mt_linkedrange_t;
    R: array[0..MAXRENDERINGTHREADS - 1] of Integer;
  end;

var
  renderparams: renderparams_t;
  numrenderingthreads: Integer = 0;
  force_numrenderingthreads: Integer = 0;

//==============================================================================
//
// R_RenderMultiThread
//
//==============================================================================
procedure R_RenderMultiThread;

//==============================================================================
//
// R_RenderWait
//
//==============================================================================
procedure R_RenderWait;

implementation

uses
  d_delphi,
  i_system,
  i_threads,
  r_wall8,
  r_wall32,
  r_flat8,
  r_flat32;

var
  renderingthreads: array[0..MAXRENDERINGTHREADS - 1] of TDThread;
  realrenderingthreads: Integer = 0;

var
  default_numrenderingthreads: integer = 0;

//==============================================================================
//
// R_RenderCalcNumThreads
//
//==============================================================================
function R_RenderCalcNumThreads: integer;
begin
  if force_numrenderingthreads > 0 then
    Result := force_numrenderingthreads
  else
    Result := I_GetNumCPUs - 1;

  if Result < 1 then
    Result := 1
  else if Result > MAXRENDERINGTHREADS then
    Result := MAXRENDERINGTHREADS;
end;

//==============================================================================
//
// R_InitMultiThreadRender
//
//==============================================================================
procedure R_InitMultiThreadRender;
var
  i: integer;
begin
  numrenderingthreads := R_RenderCalcNumThreads;

  default_numrenderingthreads := numrenderingthreads;
  for i := 0 to numrenderingthreads - 1 do
    renderingthreads[i] := TDThread.Create;

  realrenderingthreads := numrenderingthreads;

  for i := 0 to MAXRENDERINGTHREADS - 1 do
    renderparams.R[i] := i;
end;

//==============================================================================
//
// R_ShutDownMultiThreadRender
//
//==============================================================================
procedure R_ShutDownMultiThreadRender;
var
  i: integer;
begin
  for i := 0 to maxrenderingthreads - 1 do
    renderingthreads[i].Free;
end;

//==============================================================================
//
// R_RenderMultiThread
//
//==============================================================================
procedure R_RenderMultiThread;
var
  i: integer;
  newnumthreads: integer;
  step: float;
begin
  newnumthreads := R_RenderCalcNumThreads;

  if newnumthreads <> numrenderingthreads then
  begin
    if newnumthreads > realrenderingthreads then
    begin
      for i := realrenderingthreads to newnumthreads - 1 do
        renderingthreads[i] := TDThread.Create;
      realrenderingthreads := newnumthreads;
    end;
    numrenderingthreads := newnumthreads;
    for i := 0 to numrenderingthreads - 2 do
    begin
      renderparams.wallparams8[i].next := @renderparams.wallparams8[i + 1];
      renderparams.wallparams32[i].next := @renderparams.wallparams32[i + 1];
      renderparams.flatparams8[i].next := @renderparams.flatparams8[i + 1];
      renderparams.flatparams32[i].next := @renderparams.flatparams32[i + 1];
    end;
    renderparams.wallparams8[numrenderingthreads - 1].next := @renderparams.wallparams8[0];
    renderparams.wallparams32[numrenderingthreads - 1].next := @renderparams.wallparams32[0];
    renderparams.flatparams8[numrenderingthreads - 1].next := @renderparams.flatparams8[0];
    renderparams.flatparams32[numrenderingthreads - 1].next := @renderparams.flatparams32[0];
  end;

  R_RenderMultiThreadWallParams8(@renderparams.wallparams8);

  for i := 0 to numrenderingthreads - 1 do
    renderingthreads[i].Activate(@renderparams.R[i]);
end;

//==============================================================================
//
// R_RenderWait
//
//==============================================================================
procedure R_RenderWait;
var
  doneid: integer;

  function _alldone: boolean;
  var
    i: integer;
    ret: boolean;
  begin
    result := true;
    for i := doneid to numrenderingthreads - 1 do
    begin
      ret := renderingthreads[i].CheckJobDone;
      result := ret and result;
      if not result then
      begin
        doneid := i;
        exit;
      end;
    end;
  end;

begin
  doneid := 0;
  while not _alldone do
    I_Sleep(0);
end;


end.
