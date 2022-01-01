//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
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

unit libogg;

{$Z+}
{$H+}

interface

procedure _ogg_page_bos; external;
procedure _ogg_page_continued; external;
procedure _ogg_page_eos; external;
procedure _ogg_page_granulepos; external;
procedure _ogg_page_serialno; external;
procedure _ogg_stream_clear; external;
procedure _ogg_stream_init; external;
procedure _ogg_stream_packetout; external;
procedure _ogg_stream_packetpeek; external;
procedure _ogg_stream_pagein; external;
procedure _ogg_stream_reset; external;
procedure _ogg_stream_reset_serialno; external;
procedure _ogg_sync_buffer; external;
procedure _ogg_sync_clear; external;
procedure _ogg_sync_init; external;
procedure _ogg_sync_pageseek; external;
procedure _ogg_sync_reset; external;
procedure _ogg_sync_wrote; external;
procedure _oggpack_adv; external;
procedure _oggpack_bytes; external;
procedure _oggpack_get_buffer; external;
procedure _oggpack_look; external;
procedure _oggpack_read; external;
procedure _oggpack_readinit; external;
procedure _oggpack_reset; external;
procedure _oggpack_write; external;
procedure _oggpack_writeclear; external;
procedure _oggpack_writeinit; external;
procedure _oggpack_writetrunc; external;
procedure _ogg_sync_pageout; external;
procedure _ogg_stream_packetin; external;
procedure _ogg_stream_flush; external;
procedure _ogg_stream_pageout; external;
procedure _ogg_page_checksum_set; external;

implementation

uses
  c_lib;

// libogg
{$L libogg\obj\bitwise.obj}
{$L libogg\obj\framing.obj}

end.
