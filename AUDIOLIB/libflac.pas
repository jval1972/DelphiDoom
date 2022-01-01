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

unit libflac;

{$Z+}
{$H+}

interface

procedure _FLAC__STREAM_METADATA_APPLICATION_ID_LEN; external;
procedure _FLAC__STREAM_METADATA_CUESHEET_INDEX_OFFSET_LEN; external;
procedure _FLAC__STREAM_METADATA_VORBIS_COMMENT_ENTRY_LENGTH_LEN; external;
procedure _FLAC__STREAM_METADATA_VORBIS_COMMENT_NUM_COMMENTS_LEN; external;
procedure _FLAC__STREAM_SYNC_STRING; external;
procedure _FLAC__SUBFRAME_LPC_QLP_SHIFT_LEN; external;
procedure _FLAC__STREAM_METADATA_CUESHEET_INDEX_NUMBER_LEN; external;
procedure _FLAC__STREAM_METADATA_CUESHEET_INDEX_RESERVED_LEN; external;
procedure _FLAC__STREAM_METADATA_CUESHEET_TRACK_OFFSET_LEN; external;
procedure _FLAC__STREAM_METADATA_CUESHEET_TRACK_NUMBER_LEN; external;
procedure _FLAC__STREAM_METADATA_CUESHEET_TRACK_ISRC_LEN; external;
procedure _FLAC__STREAM_METADATA_CUESHEET_TRACK_TYPE_LEN; external;
procedure _FLAC__STREAM_METADATA_CUESHEET_TRACK_PRE_EMPHASIS_LEN; external;
procedure _FLAC__STREAM_METADATA_CUESHEET_TRACK_RESERVED_LEN; external;
procedure _FLAC__STREAM_METADATA_CUESHEET_TRACK_NUM_INDICES_LEN; external;
procedure _FLAC__STREAM_METADATA_CUESHEET_MEDIA_CATALOG_NUMBER_LEN; external;
procedure _FLAC__STREAM_METADATA_CUESHEET_LEAD_IN_LEN; external;
procedure _FLAC__STREAM_METADATA_CUESHEET_IS_CD_LEN; external;
procedure _FLAC__STREAM_METADATA_CUESHEET_RESERVED_LEN; external;
procedure _FLAC__STREAM_METADATA_CUESHEET_NUM_TRACKS_LEN; external;
procedure _FLAC__STREAM_METADATA_PICTURE_TYPE_LEN; external;
procedure _FLAC__STREAM_METADATA_PICTURE_MIME_TYPE_LENGTH_LEN; external;
procedure _FLAC__STREAM_METADATA_PICTURE_DESCRIPTION_LENGTH_LEN; external;
procedure _FLAC__STREAM_METADATA_PICTURE_WIDTH_LEN; external;
procedure _FLAC__STREAM_METADATA_PICTURE_HEIGHT_LEN; external;
procedure _FLAC__STREAM_METADATA_PICTURE_DEPTH_LEN; external;
procedure _FLAC__STREAM_METADATA_PICTURE_COLORS_LEN; external;
procedure _FLAC__STREAM_METADATA_PICTURE_DATA_LENGTH_LEN; external;
procedure _FLAC__STREAM_METADATA_LENGTH_LEN; external;
procedure _FLAC__VENDOR_STRING; external;
procedure _FLAC__STREAM_METADATA_SEEKPOINT_PLACEHOLDER; external;
procedure _FLAC__format_seektable_is_legal; external;
procedure _FLAC__format_seektable_sort; external;
procedure _FLAC__format_vorbiscomment_entry_value_is_legal; external;
procedure _FLAC__format_vorbiscomment_entry_is_legal; external;
procedure _FLAC__format_vorbiscomment_entry_name_is_legal; external;
procedure _FLAC__format_cuesheet_is_legal; external;
procedure _FLAC__format_picture_is_legal; external;
procedure _FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_ORDER_LEN; external;
procedure _FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_PARAMETER_LEN; external;
procedure _FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE2_PARAMETER_LEN; external;
procedure _FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_RAW_LEN; external;
procedure _FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE_ESCAPE_PARAMETER; external;
procedure _FLAC__ENTROPY_CODING_METHOD_PARTITIONED_RICE2_ESCAPE_PARAMETER; external;
procedure _FLAC__ENTROPY_CODING_METHOD_TYPE_LEN; external;
procedure _FLAC__SUBFRAME_LPC_QLP_COEFF_PRECISION_LEN; external;
procedure _FLAC__FRAME_FOOTER_CRC_LEN; external;
procedure _FLAC__STREAM_METADATA_STREAMINFO_MIN_BLOCK_SIZE_LEN; external;
procedure _FLAC__STREAM_METADATA_STREAMINFO_MAX_BLOCK_SIZE_LEN; external;
procedure _FLAC__STREAM_METADATA_STREAMINFO_MIN_FRAME_SIZE_LEN; external;
procedure _FLAC__STREAM_METADATA_STREAMINFO_MAX_FRAME_SIZE_LEN; external;
procedure _FLAC__STREAM_METADATA_STREAMINFO_SAMPLE_RATE_LEN; external;
procedure _FLAC__STREAM_METADATA_STREAMINFO_CHANNELS_LEN; external;
procedure _FLAC__STREAM_METADATA_STREAMINFO_BITS_PER_SAMPLE_LEN; external;
procedure _FLAC__STREAM_METADATA_STREAMINFO_TOTAL_SAMPLES_LEN; external;
procedure _FLAC__STREAM_METADATA_SEEKPOINT_SAMPLE_NUMBER_LEN; external;
procedure _FLAC__STREAM_METADATA_SEEKPOINT_STREAM_OFFSET_LEN; external;
procedure _FLAC__STREAM_METADATA_SEEKPOINT_FRAME_SAMPLES_LEN; external;
procedure _FLAC__STREAM_METADATA_IS_LAST_LEN; external;
procedure _FLAC__STREAM_METADATA_TYPE_LEN; external;
procedure _FLAC__bitreader_new; external;
procedure _FLAC__bitreader_delete; external;
procedure _FLAC__format_entropy_coding_method_partitioned_rice_contents_init; external;
procedure _FLAC__format_entropy_coding_method_partitioned_rice_contents_clear; external;
procedure _FLAC__ogg_decoder_aspect_init; external;
procedure _FLAC__cpu_info; external;
procedure _FLAC__bitreader_bits_left_for_byte_alignment; external;
procedure _FLAC__bitreader_clear; external;
procedure _FLAC__bitreader_free; external;
procedure _FLAC__bitreader_get_input_bits_unconsumed; external;
procedure _FLAC__bitreader_get_read_crc16; external;
procedure _FLAC__bitreader_init; external;
procedure _FLAC__bitreader_is_consumed_byte_aligned; external;
procedure _FLAC__bitreader_read_byte_block_aligned_no_crc; external;
procedure _FLAC__bitreader_read_raw_int32; external;
procedure _FLAC__bitreader_read_raw_uint32; external;
procedure _FLAC__bitreader_read_raw_uint64; external;
procedure _FLAC__bitreader_read_rice_signed_block; external;
procedure _FLAC__bitreader_read_uint32_little_endian; external;
procedure _FLAC__bitreader_read_unary_unsigned; external;
procedure _FLAC__bitreader_read_utf8_uint32; external;
procedure _FLAC__bitreader_read_utf8_uint64; external;
procedure _FLAC__bitreader_reset_read_crc16; external;
procedure _FLAC__bitreader_skip_bits_no_crc; external;
procedure _FLAC__bitreader_skip_byte_block_aligned_no_crc; external;
procedure _FLAC__bitwriter_delete; external;
procedure _FLAC__bitwriter_free; external;
procedure _FLAC__bitwriter_init; external;
procedure _FLAC__bitwriter_new; external;
procedure _FLAC__bitwriter_write_raw_uint32; external;
procedure _FLAC__fixed_compute_best_predictor; external;
procedure _FLAC__fixed_compute_best_predictor_wide; external;
procedure _FLAC__fixed_restore_signal; external;
procedure _FLAC__format_blocksize_is_subset; external;
procedure _FLAC__format_entropy_coding_method_partitioned_rice_contents_ensure_size; external;
procedure _FLAC__format_sample_rate_is_subset; external;
procedure _FLAC__format_sample_rate_is_valid; external;
procedure _FLAC__lpc_compute_autocorrelation; external;
procedure _FLAC__lpc_compute_residual_from_qlp_coefficients; external;
procedure _FLAC__lpc_compute_residual_from_qlp_coefficients_wide; external;
procedure _FLAC__lpc_restore_signal; external;
procedure _FLAC__lpc_restore_signal_wide; external;
procedure _FLAC__MD5Accumulate; external;
procedure _FLAC__MD5Final; external;
procedure _FLAC__MD5Init; external;
procedure _FLAC__memory_alloc_aligned_int32_array; external;
procedure _FLAC__memory_alloc_aligned_real_array; external;
procedure _FLAC__memory_alloc_aligned_uint64_array; external;
procedure _FLAC__ogg_decoder_aspect_finish; external;
procedure _FLAC__ogg_decoder_aspect_flush; external;
procedure _FLAC__ogg_decoder_aspect_read_callback_wrapper; external;
procedure _FLAC__ogg_decoder_aspect_reset; external;
procedure _FLAC__ogg_decoder_aspect_set_defaults; external;
procedure _FLAC__ogg_decoder_aspect_set_serial_number; external;
procedure _FLAC__ogg_encoder_aspect_finish; external;
procedure _FLAC__ogg_encoder_aspect_init; external;
procedure _FLAC__ogg_encoder_aspect_set_defaults; external;
procedure _FLAC__ogg_encoder_aspect_set_num_metadata; external;
procedure _FLAC__ogg_encoder_aspect_set_serial_number; external;
procedure _FLAC__stream_decoder_get_resolved_state_string; external;
procedure _FLAC__stream_decoder_get_state; external;
procedure _FLAC__stream_decoder_init_stream; external;
procedure _FLAC__STREAM_SYNC; external;
procedure _FLAC__STREAM_SYNC_LEN; external;
procedure _FLAC__SUBFRAME_TYPE_LEN; external;
procedure _FLAC__SUBFRAME_WASTED_BITS_FLAG_LEN; external;
procedure _FLAC__SUBFRAME_ZERO_PAD_LEN; external;
procedure _FLAC__bitwriter_clear; external;
procedure _FLAC__bitwriter_get_buffer; external;
procedure _FLAC__bitwriter_get_write_crc16; external;
procedure _FLAC__bitwriter_get_write_crc8; external;
procedure _FLAC__bitwriter_is_byte_aligned; external;
procedure _FLAC__bitwriter_release_buffer; external;
procedure _FLAC__bitwriter_write_byte_block; external;
procedure _FLAC__bitwriter_write_raw_int32; external;
procedure _FLAC__bitwriter_write_raw_uint32_little_endian; external;
procedure _FLAC__bitwriter_write_raw_uint64; external;
procedure _FLAC__bitwriter_write_rice_signed_block; external;
procedure _FLAC__bitwriter_write_unary_unsigned; external;
procedure _FLAC__bitwriter_write_utf8_uint32; external;
procedure _FLAC__bitwriter_write_utf8_uint64; external;
procedure _FLAC__bitwriter_write_zeroes; external;
procedure _FLAC__bitwriter_zero_pad_to_byte_boundary; external;
procedure _FLAC__fixed_compute_residual; external;
procedure _FLAC__format_get_max_rice_partition_order_from_blocksize; external;
procedure _FLAC__format_get_max_rice_partition_order_from_blocksize_limited_max_and_predictor_order; external;
procedure _FLAC__FRAME_HEADER_BITS_PER_SAMPLE_LEN; external;
procedure _FLAC__FRAME_HEADER_BLOCK_SIZE_LEN; external;
procedure _FLAC__FRAME_HEADER_BLOCKING_STRATEGY_LEN; external;
procedure _FLAC__FRAME_HEADER_CHANNEL_ASSIGNMENT_LEN; external;
procedure _FLAC__FRAME_HEADER_CRC_LEN; external;
procedure _FLAC__FRAME_HEADER_RESERVED_LEN; external;
procedure _FLAC__FRAME_HEADER_SAMPLE_RATE_LEN; external;
procedure _FLAC__FRAME_HEADER_SYNC; external;
procedure _FLAC__FRAME_HEADER_SYNC_LEN; external;
procedure _FLAC__FRAME_HEADER_ZERO_PAD_LEN; external;
procedure _FLAC__lpc_compute_best_order; external;
procedure _FLAC__lpc_compute_expected_bits_per_residual_sample; external;
procedure _FLAC__lpc_compute_lp_coefficients; external;
procedure _FLAC__lpc_quantize_coefficients; external;
procedure _FLAC__lpc_window_data; external;
procedure _FLAC__memory_alloc_aligned_unsigned_array; external;
procedure _FLAC__metadata_object_vorbiscomment_append_comment; external;
procedure _FLAC__metadata_object_vorbiscomment_entry_from_name_value_pair; external;
procedure _FLAC__metadata_object_vorbiscomment_find_entry_from; external;
procedure _FLAC__ogg_encoder_aspect_write_callback_wrapper; external;
procedure _FLAC__stream_decoder_process_single; external;
procedure _FLAC__stream_encoder_delete; external;
procedure _FLAC__stream_encoder_finish; external;
procedure _FLAC__stream_encoder_init_stream; external;
procedure _FLAC__stream_encoder_new; external;
procedure _FLAC__stream_encoder_set_channels; external;
procedure _FLAC__stream_encoder_set_metadata; external;
procedure _FLAC__stream_encoder_set_sample_rate; external;
procedure _FLAC__StreamDecoderErrorStatusString; external;
procedure _FLAC__StreamDecoderStateString; external;
procedure _FLAC__StreamEncoderInitStatusString; external;
procedure _FLAC__SUBFRAME_TYPE_CONSTANT_BYTE_ALIGNED_MASK; external;
procedure _FLAC__SUBFRAME_TYPE_FIXED_BYTE_ALIGNED_MASK; external;
procedure _FLAC__SUBFRAME_TYPE_LPC_BYTE_ALIGNED_MASK; external;
procedure _FLAC__SUBFRAME_TYPE_VERBATIM_BYTE_ALIGNED_MASK; external;
procedure _FLAC__stream_decoder_get_decode_position; external;
procedure _FLAC__stream_decoder_seek_absolute; external;
procedure _FLAC__stream_encoder_process_interleaved; external;
procedure _FLAC__stream_encoder_set_bits_per_sample; external;
procedure _FLAC__stream_encoder_set_compression_level; external;

procedure _FLAC__metadata_object_new; external;
procedure _FLAC__metadata_object_delete; external;
procedure _FLAC__stream_decoder_finish; external;
procedure _FLAC__stream_decoder_delete; external;
procedure _FLAC__stream_decoder_new; external;
procedure _FLAC__stream_decoder_set_metadata_respond_all; external;
procedure _FLAC__stream_decoder_process_until_end_of_metadata; external;

procedure _safe_malloc_mul_2op_p; external;

implementation

uses
  c_lib, libogg, libvorbis;

// libflac
{$L libflac\obj\bitmath.obj}
{$L libflac\obj\bitreader.obj}
{$L libflac\obj\bitwriter.obj}
{$L libflac\obj\cpu.obj}
{$L libflac\obj\crc.obj}
{$L libflac\obj\fixed.obj}
{$L libflac\obj\fixed_intrin_sse2.obj}
{$L libflac\obj\fixed_intrin_ssse3.obj}
{$L libflac\obj\float.obj}
{$L libflac\obj\format.obj}
{$L libflac\obj\lpc.obj}
{$L libflac\obj\lpc_intrin_avx2.obj}
{$L libflac\obj\lpc_intrin_sse.obj}
{$L libflac\obj\lpc_intrin_sse2.obj}
{$L libflac\obj\lpc_intrin_sse41.obj}
{$L libflac\obj\lpc_intrin_vsx.obj}
{$L libflac\obj\md5.obj}
{$L libflac\obj\memory.obj}
{$L libflac\obj\metadata_iterators.obj}
{$L libflac\obj\metadata_object.obj}
{$L libflac\obj\ogg_decoder_aspect.obj}
{$L libflac\obj\ogg_encoder_aspect.obj}
{$L libflac\obj\ogg_helper.obj}
{$L libflac\obj\ogg_mapping.obj}
{$L libflac\obj\stream_decoder.obj}
{$L libflac\obj\stream_encoder.obj}
{$L libflac\obj\stream_encoder_framing.obj}
{$L libflac\obj\stream_encoder_intrin_avx2.obj}
{$L libflac\obj\stream_encoder_intrin_sse2.obj}
{$L libflac\obj\stream_encoder_intrin_ssse3.obj}
{$L libflac\obj\window.obj}
{$L libflac\obj\windows_unicode_filenames.obj}

procedure _simple_ogg_page__clear; external;
procedure _simple_ogg_page__get_at; external;
procedure _simple_ogg_page__init; external;
procedure _simple_ogg_page__set_at; external;

end.

