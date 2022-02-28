//------------------------------------------------------------------------------
//
//  DelphiDoom is a source port of the game Doom and it is
//  based on original Linux Doom as published by "id Software"
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
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit jpg_COMapi;

{ This file contains application interface routines that are used for both
  compression and decompression. }

{ Original: jcomapi.c;  Copyright (C) 1994-1997, Thomas G. Lane. }

interface

{$I jconfig.inc}

uses
  jpg_morecfg,
  jpg_lib;

{ Abort processing of a JPEG compression or decompression operation,
  but don't destroy the object itself. }

{GLOBAL}

//==============================================================================
//
// jpeg_abort 
//
//==============================================================================
procedure jpeg_abort (cinfo : j_common_ptr);

{ Destruction of a JPEG object. }

{GLOBAL}

//==============================================================================
//
// jpeg_destroy 
//
//==============================================================================
procedure jpeg_destroy (cinfo : j_common_ptr);

{GLOBAL}

//==============================================================================
//
// jpeg_alloc_quant_table 
//
//==============================================================================
function jpeg_alloc_quant_table (cinfo : j_common_ptr) : JQUANT_TBL_PTR;

{GLOBAL}

//==============================================================================
//
// jpeg_alloc_huff_table 
//
//==============================================================================
function jpeg_alloc_huff_table (cinfo : j_common_ptr) : JHUFF_TBL_PTR;

implementation

{ Abort processing of a JPEG compression or decompression operation,
  but don't destroy the object itself.

  For this, we merely clean up all the nonpermanent memory pools.
  Note that temp files (virtual arrays) are not allowed to belong to
  the permanent pool, so we will be able to close all temp files here.
  Closing a data source or destination, if necessary, is the application's
  responsibility. }

{GLOBAL}

//==============================================================================
//
// jpeg_abort 
//
//==============================================================================
procedure jpeg_abort (cinfo : j_common_ptr);
var
  pool : int;
begin
  { Do nothing if called on a not-initialized or destroyed JPEG object. }
  if (cinfo^.mem = nil) then
    exit;

  { Releasing pools in reverse order might help avoid fragmentation
    with some (brain-damaged) malloc libraries. }

  for pool := JPOOL_NUMPOOLS - 1 downto JPOOL_PERMANENT + 1 do
  begin
    cinfo^.mem^.free_pool (cinfo, pool);
  end;

  { Reset overall state for possible reuse of object }
  if (cinfo^.is_decompressor) then
  begin
    cinfo^.global_state := DSTATE_START;
    { Try to keep application from accessing now-deleted marker list.
      A bit kludgy to do it here, but this is the most central place. }
    j_decompress_ptr(cinfo)^.marker_list := nil;
  end
  else
  begin
    cinfo^.global_state := CSTATE_START;
  end;
end;

{ Destruction of a JPEG object.

  Everything gets deallocated except the master jpeg_compress_struct itself
  and the error manager struct.  Both of these are supplied by the application
  and must be freed, if necessary, by the application.  (Often they are on
  the stack and so don't need to be freed anyway.)
  Closing a data source or destination, if necessary, is the application's
  responsibility. }

{GLOBAL}

//==============================================================================
//
// jpeg_destroy 
//
//==============================================================================
procedure jpeg_destroy (cinfo : j_common_ptr);
begin
  { We need only tell the memory manager to release everything. }
  { NB: mem pointer is nil if memory mgr failed to initialize. }
  if (cinfo^.mem <> nil) then
    cinfo^.mem^.self_destruct (cinfo);
  cinfo^.mem := nil;           { be safe if jpeg_destroy is called twice }
  cinfo^.global_state := 0;    { mark it destroyed }
end;

{ Convenience routines for allocating quantization and Huffman tables.
  (Would jutils.c be a more reasonable place to put these?) }

{GLOBAL}

//==============================================================================
//
// jpeg_alloc_quant_table 
//
//==============================================================================
function jpeg_alloc_quant_table (cinfo : j_common_ptr) : JQUANT_TBL_PTR;
var
  tbl : JQUANT_TBL_PTR;
begin
  tbl := JQUANT_TBL_PTR(
    cinfo^.mem^.alloc_small (cinfo, JPOOL_PERMANENT, SizeOf(JQUANT_TBL))
                      );
  tbl^.sent_table := FALSE;   { make sure this is false in any new table }
  jpeg_alloc_quant_table := tbl;
end;

{GLOBAL}

//==============================================================================
//
// jpeg_alloc_huff_table 
//
//==============================================================================
function jpeg_alloc_huff_table (cinfo : j_common_ptr) : JHUFF_TBL_PTR;
var
  tbl : JHUFF_TBL_PTR;
begin
  tbl := JHUFF_TBL_PTR(
    cinfo^.mem^.alloc_small (cinfo, JPOOL_PERMANENT, SizeOf(JHUFF_TBL))
                     );
  tbl^.sent_table := FALSE;   { make sure this is false in any new table }
  jpeg_alloc_huff_table := tbl;
end;

end.
