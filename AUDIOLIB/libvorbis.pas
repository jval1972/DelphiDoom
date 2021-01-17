//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2021 by Jim Valavanis
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
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

unit libvorbis;

{$Z+}
{$H+}

interface

procedure _floor0_exportbundle; external;
procedure _floor1_encode; external;
procedure _floor1_exportbundle; external;
procedure _floor1_fit; external;
procedure _floor1_interpolate_fit; external;
procedure _mapping0_exportbundle; external;

procedure _vorbis_bitrate_clear; external;
procedure _vorbis_bitrate_init; external;
procedure _vorbis_block_clear; external;
procedure _vorbis_block_init; external;
procedure _vorbis_book_decode; external;
procedure _vorbis_book_decodev_add; external;
procedure _vorbis_book_decodev_set; external;
procedure _vorbis_book_decodevs_add; external;
procedure _vorbis_book_decodevv_add; external;
procedure _vorbis_book_encode; external;
procedure _vorbis_comment_clear; external;
procedure _vorbis_comment_init; external;
procedure _vorbis_dsp_clear; external;
procedure _vorbis_info_blocksize; external;
procedure _vorbis_info_clear; external;
procedure _vorbis_info_init; external;
procedure _vorbis_packet_blocksize; external;
procedure _vorbis_staticbook_pack; external;
procedure _vorbis_staticbook_unpack; external;
procedure _vorbis_synthesis; external;
procedure _vorbis_synthesis_blockin; external;
procedure _vorbis_synthesis_halfrate; external;
procedure _vorbis_synthesis_halfrate_p; external;
procedure _vorbis_synthesis_headerin; external;
procedure _vorbis_synthesis_idheader; external;
procedure _vorbis_synthesis_init; external;
procedure _vorbis_synthesis_lapout; external;
procedure _vorbis_synthesis_pcmout; external;
procedure _vorbis_synthesis_read; external;
procedure _vorbis_synthesis_restart; external;
procedure _vorbis_synthesis_trackonly; external;
procedure _vorbis_window; external;
procedure _vorbis_analysis; external;
procedure _vorbis_analysis_blockout; external;
procedure _vorbis_analysis_buffer; external;
procedure _vorbis_analysis_headerout; external;
procedure _vorbis_analysis_init; external;
procedure _vorbis_analysis_wrote; external;
procedure _vorbis_bitrate_addblock; external;
procedure _vorbis_bitrate_flushpacket; external;
procedure _vorbis_comment_add_tag; external;
procedure _vorbis_comment_query; external;
procedure _vorbis_encode_init_vbr; external;
procedure _vorbis_version_string; external;
procedure __vorbis_block_alloc; external;
procedure __vorbis_block_ripcord; external;

procedure _safe_realloc_mul_2op_; external;
procedure _safe_realloc_; external;
procedure _safe_malloc_; external;
procedure _safe_malloc_add_2op_; external;
procedure _safe_realloc_add_2op_; external;
procedure _safe_calloc_; external;
procedure _safe_malloc_add_4op_; external;
procedure _safe_malloc_muladd2_; external;

implementation

uses
  c_lib, libogg, libflac;

// libvorbis
{$L libvorbis\obj\alloc.obj}
{$L libvorbis\obj\analysis.obj}
{$L libvorbis\obj\bitrate.obj}
{$L libvorbis\obj\block.obj}
{$L libvorbis\obj\codebook.obj}
{$L libvorbis\obj\envelope.obj}
{$L libvorbis\obj\floor0.obj}
{$L libvorbis\obj\floor1.obj}
{$L libvorbis\obj\info.obj}
{$L libvorbis\obj\lookup.obj}
{$L libvorbis\obj\lpc.obj}
{$L libvorbis\obj\lsp.obj}
{$L libvorbis\obj\mapping0.obj}
{$L libvorbis\obj\mdct.obj}
{$L libvorbis\obj\psy.obj}
{$L libvorbis\obj\registry.obj}
{$L libvorbis\obj\res0.obj}
{$L libvorbis\obj\sharedbook.obj}
{$L libvorbis\obj\smallft.obj}
{$L libvorbis\obj\synthesis.obj}
{$L libvorbis\obj\tone.obj}
{$L libvorbis\obj\vorbisenc.obj}
{$L libvorbis\obj\vorbisfile.obj}
{$L libvorbis\obj\vorbiswindow.obj}

end.
