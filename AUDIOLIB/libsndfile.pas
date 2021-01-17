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

unit libsndfile;

{$Z+}
{$H+}

interface

uses
  Windows, Classes;

type
  PMemoryStream = ^TMemoryStream;

type
  off_t = int64;
  sf_count_t = int64;

const
  //* Major formats. *//
  SF_FORMAT_WAV = $010000;    // Microsoft WAV format (little endian default).
  SF_FORMAT_AIFF = $020000;   // Apple/SGI AIFF format (big endian).
  SF_FORMAT_AU = $030000;     // Sun/NeXT AU format (big endian).
  SF_FORMAT_RAW = $040000;    // RAW PCM data.
  SF_FORMAT_PAF = $050000;    // Ensoniq PARIS file format.
  SF_FORMAT_SVX = $060000;    // Amiga IFF / SVX8 / SV16 format.
  SF_FORMAT_NIST = $070000;   // Sphere NIST format.
  SF_FORMAT_VOC = $080000;    // VOC files.
  SF_FORMAT_IRCAM = $0A0000;  // Berkeley/IRCAM/CARL
  SF_FORMAT_W64 = $0B0000;    // Sonic Foundry's 64 bit RIFF/WAV
  SF_FORMAT_MAT4 = $0C0000;   // Matlab (tm) V4.2 / GNU Octave 2.0
  SF_FORMAT_MAT5 = $0D0000;   // Matlab (tm) V5.0 / GNU Octave 2.1
  SF_FORMAT_PVF = $0E0000;    // Portable Voice Format
  SF_FORMAT_XI = $0F0000;     // Fasttracker 2 Extended Instrument
  SF_FORMAT_HTK = $100000;    // HMM Tool Kit format
  SF_FORMAT_SDS = $110000;    // Midi Sample Dump Standard
  SF_FORMAT_AVR = $120000;    // Audio Visual Research
  SF_FORMAT_WAVEX = $130000;  // MS WAVE with WAVEFORMATEX
  SF_FORMAT_SD2 = $160000;    // Sound Designer 2
  SF_FORMAT_FLAC = $170000;   // FLAC lossless file format
  SF_FORMAT_CAF = $180000;    // Core Audio File format
  SF_FORMAT_OGG = $200000;    // Xiph OGG container

const
  //Subtypes from here on.
  SF_FORMAT_PCM_S8 = $0001;   // Signed 8 bit data
  SF_FORMAT_PCM_16 = $0002;   // Signed 16 bit data
  SF_FORMAT_PCM_24 = $0003;   // Signed 24 bit data
  SF_FORMAT_PCM_32 = $0004;   // Signed 32 bit data

  SF_FORMAT_PCM_U8 = $0005;   // Unsigned 8 bit data (WAV and RAW only)

  SF_FORMAT_FLOAT = $0006;    // 32 bit float data
  SF_FORMAT_DOUBLE = $0007;   // 64 bit float data

  SF_FORMAT_ULAW = $0010;     // U-Law encoded.
  SF_FORMAT_ALAW = $0011;     // A-Law encoded.
  SF_FORMAT_IMA_ADPCM = $0012;  // IMA ADPCM.
  SF_FORMAT_MS_ADPCM = $0013;   // Microsoft ADPCM.

  SF_FORMAT_GSM610 = $0020;     // GSM 6.10 encoding.
  SF_FORMAT_VOX_ADPCM = $0021;  // OKI / Dialogix ADPCM

  SF_FORMAT_G721_32 = $0030;    // 32kbs G721 ADPCM encoding.
  SF_FORMAT_G723_24 = $0031;    // 24kbs G723 ADPCM encoding.
  SF_FORMAT_G723_40 = $0032;    // 40kbs G723 ADPCM encoding.

  SF_FORMAT_DWVW_12 = $0040;    // 12 bit Delta Width Variable Word encoding.
  SF_FORMAT_DWVW_16 = $0041;    // 16 bit Delta Width Variable Word encoding.
  SF_FORMAT_DWVW_24 = $0042;    // 24 bit Delta Width Variable Word encoding.
  SF_FORMAT_DWVW_N = $0043;     // N bit Delta Width Variable Word encoding.

  SF_FORMAT_DPCM_8 = $0050;     // 8 bit differential PCM (XI only)
  SF_FORMAT_DPCM_16 = $0051;    // 16 bit differential PCM (XI only)

  SF_FORMAT_VORBIS  = $0060;    // Xiph Vorbis encoding.


const
  //* Endian-ness options. *//
  SF_ENDIAN_FILE = $00000000;   // Default file endian-ness.
  SF_ENDIAN_LITTLE = $10000000; // Force little endian-ness.
  SF_ENDIAN_BIG = $20000000;    // Force big endian-ness.
  SF_ENDIAN_CPU = $30000000;    // Force CPU endian-ness.

  SF_FORMAT_SUBMASK = $0000FFFF;
  SF_FORMAT_TYPEMASK = $0FFF0000;
  SF_FORMAT_ENDMASK = $30000000;

{
** The following are the valid command numbers for the sf_command()
** interface.  The use of these commands is documented in the file
** command.html in the doc directory of the source code distribution.
}
const
  SFC_GET_LIB_VERSION = $1000;
  SFC_GET_LOG_INFO = $1001;

  SFC_GET_NORM_DOUBLE = $1010;
  SFC_GET_NORM_FLOAT = $1011;
  SFC_SET_NORM_DOUBLE = $1012;
  SFC_SET_NORM_FLOAT = $1013;
  SFC_SET_SCALE_FLOAT_INT_READ = $1014;

  SFC_GET_SIMPLE_FORMAT_COUNT = $1020;
  SFC_GET_SIMPLE_FORMAT = $1021;

  SFC_GET_FORMAT_INFO = $1028;

  SFC_GET_FORMAT_MAJOR_COUNT = $1030;
  SFC_GET_FORMAT_MAJOR = $1031;
  SFC_GET_FORMAT_SUBTYPE_COUNT = $1032;
  SFC_GET_FORMAT_SUBTYPE = $1033;

  SFC_CALC_SIGNAL_MAX = $1040;
  SFC_CALC_NORM_SIGNAL_MAX = $1041;
  SFC_CALC_MAX_ALL_CHANNELS = $1042;
  SFC_CALC_NORM_MAX_ALL_CHANNELS = $1043;
  SFC_GET_SIGNAL_MAX = $1044;
  SFC_GET_MAX_ALL_CHANNELS = $1045;

  SFC_SET_ADD_PEAK_CHUNK = $1050;

  SFC_UPDATE_HEADER_NOW = $1060;
  SFC_SET_UPDATE_HEADER_AUTO = $1061;

  SFC_FILE_TRUNCATE = $1080;

  SFC_SET_RAW_START_OFFSET = $1090;

  SFC_SET_DITHER_ON_WRITE = $10A0;
  SFC_SET_DITHER_ON_READ = $10A1;

  SFC_GET_DITHER_INFO_COUNT = $10A2;
  SFC_GET_DITHER_INFO = $10A3;

  SFC_GET_EMBED_FILE_INFO = $10B0;

  SFC_SET_CLIPPING = $10C0;
  SFC_GET_CLIPPING = $10C1;

  SFC_GET_INSTRUMENT = $10D0;
  SFC_SET_INSTRUMENT = $10D1;

  SFC_GET_LOOP_INFO = $10E0;

  SFC_GET_BROADCAST_INFO = $10F0;
  SFC_SET_BROADCAST_INFO = $10F1;

  // Following commands for testing only.
  SFC_TEST_IEEE_FLOAT_REPLACE = $6001;

  {
  ** SFC_SET_ADD_* values are deprecated and will disappear at some
  ** time in the future. They are guaranteed to be here up to and
  ** including version 1.0.8 to avoid breakage of existing software.
  ** They currently do nothing and will continue to do nothing.
  }
  SFC_SET_ADD_DITHER_ON_WRITE = $1070;
  SFC_SET_ADD_DITHER_ON_READ = $1071;

{
** String types that can be set and read from files. Not all file types
** support this and even the file types which support one, may not support
** all string types.
}
const
  SF_STR_TITLE = $01;
  SF_STR_COPYRIGHT = $02;
  SF_STR_SOFTWARE = $03;
  SF_STR_ARTIST = $04;
  SF_STR_COMMENT = $05;
  SF_STR_DATE = $06;

{
** Use the following as the start and end index when doing metadata
** transcoding.
}
  SF_STR_FIRST = SF_STR_TITLE;
  SF_STR_LAST = SF_STR_DATE;

const
  // True and false
  SF_FALSE = 0;
  SF_TRUE = 1;

const
  // Modes for opening files.
  SFM_READ = $10;
  SFM_WRITE = $20;
  SFM_RDWR = $30;

{
** Public error values. These are guaranteed to remain unchanged
** for the duration of the library major version number.
** There are also a large number of private error numbers which are
** internal to the library which can change at any time.
}
const
  SF_ERR_NO_ERROR = 0;
  SF_ERR_UNRECOGNISED_FORMAT = 1;
  SF_ERR_SYSTEM = 2;
  SF_ERR_MALFORMED_FILE = 3;
  SF_ERR_UNSUPPORTED_ENCODING = 4;

//A SNDFILE* pointer can be passed around much like stdio.h's FILE* pointer.

type
  TSNDFILE_HANDLE = pointer;  // this is not a usual pointer, more like a THandle ..
// so NOT called  "PSndFile_handle"
// => we never access members of the internal
//    structure where the pointer points to !
//    Everything is managed by the DLL internally !!!
//PSNDFILE_tag = PSNDFILE;

  {
** The following typedef is system specific and is defined when libsndfile is.
** compiled. uos_count_t can be one of loff_t (Linux), off_t (*BSD),
** off64_t (Solaris), __int64_t (Win32) etc.
}
type
  Puos_count_t = ^Tuos_count_t;
  Tuos_count_t = off_t;

const
  SF_COUNT_MAX = $7FFFFFFFFFFFFFFF;

{
** A pointer to a SF_INFO structure is passed to sf_open_read () and filled in.
** On write, the SF_INFO structure is filled in by the user and passed into
** sf_open_write ().
}

type
  PSF_INFO = ^TSF_INFO;

  TSF_INFO = record
    frames: Tuos_count_t;
    // Used to be called samples.  Changed to avoid confusion.
    samplerate: integer;
    channels: integer;
    format: integer;
    sections: integer;
    seekable: integer;
  end;

{
** The SF_FORMAT_INFO struct is used to retrieve information about the sound
** file formats libsndfile supports using the sf_command () interface.
**
** Using this interface will allow applications to support new file formats
** and encoding types when libsndfile is upgraded, without requiring
** re-compilation of the application.
**
** Please consult the libsndfile documentation (particularly the information
** on the sf_command () interface) for examples of its use.
}

type
  PSF_FORMAT_INFO = ^TSF_FORMAT_INFO;

  TSF_FORMAT_INFO = record
    format: integer;
    Name: PChar;
    extention: PChar;
  end;

{
** Enums and typedefs for adding dither on read and write.
** See the html documentation for sf_command(), SFC_SET_DITHER_ON_WRITE
** and SFC_SET_DITHER_ON_READ.
}
const
  SFD_DEFAULT_LEVEL = 0;
  SFD_CUSTOM_LEVEL = $40000000;

  SFD_NO_DITHER = 500;
  SFD_WHITE = 501;
  SFD_TRIANGULAR_PDF = 502;

type
  PSF_DITHER_INFO = ^TSF_DITHER_INFO;

  TSF_DITHER_INFO = record
    type_: integer;
    level: double;
    Name: PChar;
  end;

{
** Struct used to retrieve information about a file embedded within a
** larger file. See SFC_GET_EMBED_FILE_INFO.
}
type
  PSF_EMBED_FILE_INFO = ^TSF_EMBED_FILE_INFO;

  TSF_EMBED_FILE_INFO = record
    offset: Tuos_count_t;
    length: Tuos_count_t;
  end;

// Structs used to retrieve music sample information from a file.

const
  // The loop mode field in SF_INSTRUMENT will be one of the following.
  SF_LOOP_NONE = 800;
  SF_LOOP_FORWARD = 801;
  SF_LOOP_BACKWARD = 802;
  SF_LOOP_ALTERNATING = 803;

type
  PSF_INSTRUMENT = ^TSF_INSTRUMENT;

  TSF_INSTRUMENT = record
    gain: integer;
    basenote,
    detune: Char;
    velocity_lo,
    velocity_hi: Char;
    loop_count: integer;
    loops: array[0..15] of record
      mode: integer;
      start: integer;
      end_: integer;
      Count: integer;
    end;
  end;

// Struct used to retrieve loop information from a file.
type
  PSF_LOOP_INFO = ^TSF_LOOP_INFO;

  TSF_LOOP_INFO = record
    time_sig_num: word;
    // any positive integer    > 0
    time_sig_den: word;
    // any positive power of 2 > 0
    loop_mode: integer;                // see SF_LOOP enum

    num_beats: integer;
    // this is NOT the amount of quarter notes !!!
    // a full bar of 4/4 is 4 beats
    // a full bar of 7/8 is 7 beats

    bpm: single;
    // suggestion, as it can be calculated using other fields:
    // file's lenght, file's sampleRate and our time_sig_den
    // -> bpms are always the amount of _quarter notes_ per minute

    root_key: integer;
    // MIDI note, or -1 for None
    future: array[0..5] of integer;
  end;


{
**  Struct used to retrieve broadcast (EBU) information from a file.
**  Strongly (!) based on EBU "bext" chunk format used in Broadcast WAVE.
}
type
  PSF_BROADCAST_INFO = ^TSF_BROADCAST_INFO;

  TSF_BROADCAST_INFO = record
    description: array[0..255] of char;
    originator: array[0..31] of char;
    originator_reference: array[0..31] of char;
    origination_date: array[0..9] of char;
    origination_time: array[0..7] of char;
    time_reference_low: LongWord;
    time_reference_high: LongWord;
    version: smallint;
    umid: array[0..63] of char;
    reserved: array[0..189] of char;
    coding_history_size: LongWord;
    coding_history: array[0..255] of char;
  end;

 // Thanks to Phoenix
 type
 //pm_get_filelen = ^tm_get_filelen;
 tm_get_filelen =
  function (pms: PMemoryStream): Tuos_count_t; cdecl;
 //pm_seek = ^tm_seek;
 tm_seek =
  function (offset: Tuos_count_t; whence: integer; pms: PMemoryStream): Tuos_count_t; cdecl;
 //pm_read = ^tm_read;
 tm_read =
  function (const buf: Pointer; count: Tuos_count_t; pms: PMemoryStream): Tuos_count_t; cdecl;
 //pm_write = ^tm_write;
 tm_write =
  function (const buf: Pointer; count: Tuos_count_t; pms: PMemoryStream): Tuos_count_t; cdecl;
 //pm_tell = ^tm_tell;
 tm_tell =
  function (pms: PMemoryStream): Tuos_count_t; cdecl;

 TSF_VIRTUAL = packed record
  sf_vio_get_filelen  : tm_get_filelen;
  seek         : tm_seek;
  read         : tm_read;
  write        : tm_write;
  tell         : tm_tell;
 end;

 PSF_VIRTUAL = ^TSF_VIRTUAL;

{
** Open the specified file for read, write or both. On error, this will
** return a NULL pointer. To find the error number, pass a NULL SNDFILE
** to sf_perror () or sf_error_str ().
** All calls to sf_open() should be matched with a call to sf_close().
}
////////////////////////////////////////////////////////////////////////////////////////

function _sf_open_1(path: string; mode: integer; var sfinfo: TSF_INFO): TSNDFILE_HANDLE;

function _sf_open(path: PChar; mode: integer; sfinfo: PSF_INFO): TSNDFILE_HANDLE; cdecl;

function _sf_version_string: PChar; cdecl;

function _sf_open_fd(fd: integer; mode: integer; sfinfo: PSF_INFO; close_desc: integer): TSNDFILE_HANDLE; cdecl;

function _sf_open_virtual(sfvirtual: PSF_VIRTUAL; mode: integer; sfinfo: PSF_INFO; user_data: Pointer): TSNDFILE_HANDLE; cdecl;

function _sf_error(sndfile: TSNDFILE_HANDLE): integer; cdecl;

function _sf_strerror(sndfile: TSNDFILE_HANDLE): PChar; cdecl;

function _sf_error_number(errnum: integer): PChar; cdecl;

function _sf_perror(sndfile: TSNDFILE_HANDLE): integer; cdecl;

function _sf_error_str(sndfile: TSNDFILE_HANDLE; str: PChar; len: LongWord): integer; cdecl;

{
 In libsndfile there are 4 functions with the same name (sf_command), 3 of them use the parameter "overload".
 In dynamic loading (because of var) we use 4 different names for the 4 functions sf_command :
 sf_command_pointer, sf_command_double, sf_command_array, sf_command_tsf. All that 4 functions gonna point
 to sf_command in libsndfile library.
}

function _sf_command(sndfile: TSNDFILE_HANDLE; command: integer;
  Data: Pointer; datasize: integer): integer; cdecl;

var
  _sf_command_pointer: function(sndfile: TSNDFILE_HANDLE; command: integer;
    Data: Pointer; datasize: integer): integer; cdecl;

var
  _sf_command_double: function(sndfile: TSNDFILE_HANDLE; command: integer;
    var Data: double; datasize: integer): integer; cdecl;

var
  _sf_command_array: function(sndfile: TSNDFILE_HANDLE; command: integer;
    var Data: array of char; datasize: integer): integer; cdecl;

function _sf_format_check(var info: TSF_INFO): integer; cdecl;

{
** Seek within the waveform data chunk of the SNDFILE. sf_seek () uses
** the same values for whence (SEEK_SET, SEEK_CUR and SEEK_END) as
** stdio.h function fseek ().
** An offset of zero with whence set to SEEK_SET will position the
** read / write pointer to the first data sample.
** On success sf_seek returns the current position in (multi-channel)
** samples from the start of the file.
** Please see the libsndfile documentation for moving the read pointer
** separately from the write pointer on files open in mode SFM_RDWR.
** On error all of these functions return -1.
}

//the following CONST values originally are NOT in libsndfile.pas:
const
  SEEK_SET = 0;       //* seek relative to beginning of file */

const
  SEEK_CUR = 1;       //* seek relative to current file position */

const
  SEEK_END = 2;       //* seek relative to end of file */

const
  SEEK_DATA = 3;       //* seek to the next data */

const
  SEEK_HOLE = 4;       //* seek to the next hole */

const
  SEEK_MAX = SEEK_HOLE;

function _sf_seek(sndfile: TSNDFILE_HANDLE; frame: Tuos_count_t; whence: integer): Tuos_count_t; cdecl;

{
** Functions for retrieving and setting string data within sound files.
** Not all file types support this features; AIFF and WAV do. For both
** functions, the str_type parameter must be one of the SF_STR_* values
** defined above.
** On error, sf_set_string() returns non-zero while sf_get_string()
** returns NULL.
}
function _sf_set_string(sndfile: TSNDFILE_HANDLE; str_type: integer; str: PChar): integer; cdecl;

function _sf_get_string(sndfile: TSNDFILE_HANDLE; str_type: integer): PChar; cdecl;

function _sf_read_raw(sndfile: TSNDFILE_HANDLE; ptr: Pointer; bytes: Tuos_count_t): Tuos_count_t; cdecl;

function _sf_write_raw(sndfile: TSNDFILE_HANDLE; ptr: Pointer; bytes: Tuos_count_t): Tuos_count_t; cdecl;

type
  PSmallIntArray = ^TSmallIntArray;
  TSmallIntArray = packed array[0..$FFF] of smallint;

  PIntegerArray = ^TIntegerArray;
  TIntegerArray = packed array[0..$FFF] of integer;

  PSingleArray = ^TSingleArray;
  TSingleArray = packed array[0..$FFF] of integer;

  PDoubleArray = ^TDoubleArray;
  TDoubleArray = packed array[0..$FFF] of integer;
{
** Functions for reading and writing the data chunk in terms of frames.
** The number of items actually read/written = frames * number of channels.
**     sf_xxxx_raw  read/writes the raw data bytes from/to the file
**     sf_xxxx_short  passes data in the native short format
**     sf_xxxx_int  passes data in the native int format
**     sf_xxxx_float  passes data in the native float format
**     sf_xxxx_double  passes data in the native double format
** All of these read/write function return number of frames read/written.
}
function _sf_readf_short(sndfile: TSNDFILE_HANDLE; ptr: PSmallIntArray; frames: Tuos_count_t): Tuos_count_t; cdecl;

function _sf_writef_short(sndfile: TSNDFILE_HANDLE; ptr: PSmallIntArray; frames: Tuos_count_t): Tuos_count_t; cdecl;

function _sf_readf_int(sndfile: TSNDFILE_HANDLE; ptr: PIntegerArray; frames: Tuos_count_t): Tuos_count_t; cdecl;

function _sf_writef_int(sndfile: TSNDFILE_HANDLE; ptr: PIntegerArray; frames: Tuos_count_t): Tuos_count_t; cdecl;

function _sf_readf_float(sndfile: TSNDFILE_HANDLE; ptr: PSingleArray; frames: Tuos_count_t): Tuos_count_t; cdecl;

function _sf_writef_float(sndfile: TSNDFILE_HANDLE; ptr: PSingleArray; frames: Tuos_count_t): Tuos_count_t; cdecl;

function _sf_readf_double(sndfile: TSNDFILE_HANDLE; ptr: PDoubleArray; frames: Tuos_count_t): Tuos_count_t; cdecl;

function _sf_writef_double(sndfile: TSNDFILE_HANDLE; ptr: PDoubleArray; frames: Tuos_count_t): Tuos_count_t; cdecl;

{
** Functions for reading and writing the data chunk in terms of items.
** Otherwise similar to above.
** All of these read/write function return number of items read/written.
}
function _sf_read_short(sndfile: TSNDFILE_HANDLE; ptr: PSmallIntArray; frames: Tuos_count_t): Tuos_count_t; cdecl;

function _sf_write_short(sndfile: TSNDFILE_HANDLE; ptr: PSmallIntArray; frames: Tuos_count_t): Tuos_count_t; cdecl;

function _sf_read_int(sndfile: TSNDFILE_HANDLE; ptr: PIntegerArray; frames: Tuos_count_t): Tuos_count_t; cdecl;

function _sf_write_int(sndfile: TSNDFILE_HANDLE; ptr: PIntegerArray; frames: Tuos_count_t): Tuos_count_t; cdecl;

function _sf_read_float(sndfile: TSNDFILE_HANDLE; ptr: PSingleArray; frames: Tuos_count_t): Tuos_count_t; cdecl;

function _sf_write_float(sndfile: TSNDFILE_HANDLE; ptr: PSingleArray; frames: Tuos_count_t): Tuos_count_t; cdecl;

function _sf_read_double(sndfile: TSNDFILE_HANDLE; ptr: PDoubleArray; frames: Tuos_count_t): Tuos_count_t; cdecl;

function _sf_write_double(sndfile: TSNDFILE_HANDLE; ptr: PDoubleArray; frames: Tuos_count_t): Tuos_count_t; cdecl;

{
** Close the SNDFILE and clean up all memory allocations associated
** with this file.
** Returns 0 on success, or an error number.
}
function _sf_close(sndfile: TSNDFILE_HANDLE): integer; cdecl;

{
** If the file is opened SFM_WRITE or SFM_RDWR, call fsync() on the file
** to force the writing of data to disk. If the file is opened SFM_READ
** no action is taken.
}
function _sf_write_sync(sndfile: TSNDFILE_HANDLE): integer; cdecl;

{Special function for dynamic loading of lib ...}

procedure sf_Load; // load the lib

procedure sf_Unload;
// unload and frees the lib from memory : do not forget to call it before close application.

function _sf_IsLoaded: boolean;

implementation

uses
  c_lib, libogg, libvorbis, libflac;

// libsndfile
{$L libsndfile\obj\add.obj}
{$L libsndfile\obj\ag_dec.obj}
{$L libsndfile\obj\ag_enc.obj}
{$L libsndfile\obj\aiff.obj}
{$L libsndfile\obj\alac.obj}
{$L libsndfile\obj\ALACBitUtilities.obj}
{$L libsndfile\obj\alac_decoder.obj}
{$L libsndfile\obj\alac_encoder.obj}
{$L libsndfile\obj\alaw.obj}
{$L libsndfile\obj\au.obj}
{$L libsndfile\obj\audio_detect.obj}
{$L libsndfile\obj\avr.obj}
{$L libsndfile\obj\broadcast.obj}
{$L libsndfile\obj\caf.obj}
{$L libsndfile\obj\cart.obj}
{$L libsndfile\obj\chanmap.obj}
{$L libsndfile\obj\chunk.obj}
{$L libsndfile\obj\code.obj}
{$L libsndfile\obj\command.obj}
{$L libsndfile\obj\common.obj}
{$L libsndfile\obj\decode.obj}
{$L libsndfile\obj\dither.obj}
{$L libsndfile\obj\double64.obj}
{$L libsndfile\obj\dp_dec.obj}
{$L libsndfile\obj\dp_enc.obj}
{$L libsndfile\obj\dwd.obj}
{$L libsndfile\obj\dwvw.obj}
{$L libsndfile\obj\file_io.obj}
{$L libsndfile\obj\flac.obj}
{$L libsndfile\obj\float32.obj}
{$L libsndfile\obj\g721.obj}
{$L libsndfile\obj\g723_16.obj}
{$L libsndfile\obj\g723_24.obj}
{$L libsndfile\obj\g723_40.obj}
{$L libsndfile\obj\g72x_main.obj}
{$L libsndfile\obj\g72x.obj}
{$L libsndfile\obj\gsm610.obj}
{$L libsndfile\obj\gsm_create.obj}
{$L libsndfile\obj\gsm_decode.obj}
{$L libsndfile\obj\gsm_destroy.obj}
{$L libsndfile\obj\gsm_encode.obj}
{$L libsndfile\obj\gsm_option.obj}
{$L libsndfile\obj\htk.obj}
{$L libsndfile\obj\id3.obj}
{$L libsndfile\obj\ima_adpcm.obj}
{$L libsndfile\obj\ima_oki_adpcm.obj}
{$L libsndfile\obj\interleave.obj}
{$L libsndfile\obj\ircam.obj}
{$L libsndfile\obj\long_term.obj}
{$L libsndfile\obj\lpc.obj}
{$L libsndfile\obj\macos.obj}
{$L libsndfile\obj\mat4.obj}
{$L libsndfile\obj\mat5.obj}
{$L libsndfile\obj\matrix_dec.obj}
{$L libsndfile\obj\matrix_enc.obj}
{$L libsndfile\obj\mpc2k.obj}
{$L libsndfile\obj\ms_adpcm.obj}
{$L libsndfile\obj\nist.obj}
{$L libsndfile\obj\ogg.obj}
{$L libsndfile\obj\ogg_opus.obj}
{$L libsndfile\obj\ogg_pcm.obj}
{$L libsndfile\obj\ogg_speex.obj}
{$L libsndfile\obj\ogg_vorbis.obj}
{$L libsndfile\obj\paf.obj}
{$L libsndfile\obj\pcm.obj}
{$L libsndfile\obj\preprocess.obj}
{$L libsndfile\obj\pvf.obj}
{$L libsndfile\obj\raw.obj}
{$L libsndfile\obj\rf64.obj}
{$L libsndfile\obj\rpe.obj}
{$L libsndfile\obj\rx2.obj}
{$L libsndfile\obj\sd2.obj}
{$L libsndfile\obj\sds.obj}
{$L libsndfile\obj\short_term.obj}
{$L libsndfile\obj\sfendian.obj}
{$L libsndfile\obj\sndfile.obj}
{$L libsndfile\obj\strings.obj}
{$L libsndfile\obj\svx.obj}
{$L libsndfile\obj\table.obj}
{$L libsndfile\obj\txw.obj}
{$L libsndfile\obj\ulaw.obj}
{$L libsndfile\obj\voc.obj}
{$L libsndfile\obj\vox_adpcm.obj}
{$L libsndfile\obj\w64.obj}
{$L libsndfile\obj\wav.obj}
{$L libsndfile\obj\wavlike.obj}
{$L libsndfile\obj\windows.obj}
{$L libsndfile\obj\wve.obj}
{$L libsndfile\obj\xi.obj}

procedure sf_Load;
begin
  @_sf_command_pointer := @_sf_command;
  @_sf_command_double := @_sf_command;
  @_sf_command_array := @_sf_command;
end;

procedure sf_Unload;
begin
end;

function _sf_open_1(path: string; mode: integer;
  var sfinfo: TSF_INFO): TSNDFILE_HANDLE;
begin
  Result := _sf_open(PChar(path), mode, @sfinfo);
end;

function _sf_IsLoaded: boolean;
begin
  Result := True;
end;

function _sf_open(path: PChar; mode: integer; sfinfo: PSF_INFO): TSNDFILE_HANDLE; cdecl; external;

function _sf_version_string: PChar; cdecl; external;

function _sf_open_fd(fd: integer; mode: integer; sfinfo: PSF_INFO; close_desc: integer): TSNDFILE_HANDLE; cdecl; external;

function _sf_open_virtual(sfvirtual: PSF_VIRTUAL; mode: integer; sfinfo: PSF_INFO; user_data: Pointer): TSNDFILE_HANDLE; cdecl; external;

function _sf_error(sndfile: TSNDFILE_HANDLE): integer; cdecl; external;

function _sf_strerror(sndfile: TSNDFILE_HANDLE): PChar; cdecl; external;

function _sf_error_number(errnum: integer): PChar; cdecl; external;

function _sf_perror(sndfile: TSNDFILE_HANDLE): integer; cdecl; external;

function _sf_error_str(sndfile: TSNDFILE_HANDLE; str: PChar; len: LongWord): integer; cdecl; external;

function _sf_command(sndfile: TSNDFILE_HANDLE; command: integer;
  Data: Pointer; datasize: integer): integer; cdecl; external;

function _sf_format_check(var info: TSF_INFO): integer; cdecl; external;

function _sf_seek(sndfile: TSNDFILE_HANDLE; frame: Tuos_count_t; whence: integer): Tuos_count_t; cdecl; external;

function _sf_set_string(sndfile: TSNDFILE_HANDLE; str_type: integer; str: PChar): integer; cdecl; external;

function _sf_get_string(sndfile: TSNDFILE_HANDLE; str_type: integer): PChar; cdecl; external;

function _sf_read_raw(sndfile: TSNDFILE_HANDLE; ptr: Pointer; bytes: Tuos_count_t): Tuos_count_t; cdecl; external;

function _sf_write_raw(sndfile: TSNDFILE_HANDLE; ptr: Pointer; bytes: Tuos_count_t): Tuos_count_t; cdecl; external;

function _sf_readf_short(sndfile: TSNDFILE_HANDLE; ptr: PSmallIntArray; frames: Tuos_count_t): Tuos_count_t; cdecl; external;

function _sf_writef_short(sndfile: TSNDFILE_HANDLE; ptr: PSmallIntArray; frames: Tuos_count_t): Tuos_count_t; cdecl; external;

function _sf_readf_int(sndfile: TSNDFILE_HANDLE; ptr: PIntegerArray; frames: Tuos_count_t): Tuos_count_t; cdecl; external;

function _sf_writef_int(sndfile: TSNDFILE_HANDLE; ptr: PIntegerArray; frames: Tuos_count_t): Tuos_count_t; cdecl; external;

function _sf_readf_float(sndfile: TSNDFILE_HANDLE; ptr: PSingleArray; frames: Tuos_count_t): Tuos_count_t; cdecl; external;

function _sf_writef_float(sndfile: TSNDFILE_HANDLE; ptr: PSingleArray; frames: Tuos_count_t): Tuos_count_t; cdecl; external;

function _sf_readf_double(sndfile: TSNDFILE_HANDLE; ptr: PDoubleArray; frames: Tuos_count_t): Tuos_count_t; cdecl; external;

function _sf_writef_double(sndfile: TSNDFILE_HANDLE; ptr: PDoubleArray; frames: Tuos_count_t): Tuos_count_t; cdecl; external;

function _sf_read_short(sndfile: TSNDFILE_HANDLE; ptr: PSmallIntArray; frames: Tuos_count_t): Tuos_count_t; cdecl; external;

function _sf_write_short(sndfile: TSNDFILE_HANDLE; ptr: PSmallIntArray; frames: Tuos_count_t): Tuos_count_t; cdecl; external;

function _sf_read_int(sndfile: TSNDFILE_HANDLE; ptr: PIntegerArray; frames: Tuos_count_t): Tuos_count_t; cdecl; external;

function _sf_write_int(sndfile: TSNDFILE_HANDLE; ptr: PIntegerArray; frames: Tuos_count_t): Tuos_count_t; cdecl; external;

function _sf_read_float(sndfile: TSNDFILE_HANDLE; ptr: PSingleArray; frames: Tuos_count_t): Tuos_count_t; cdecl; external;

function _sf_write_float(sndfile: TSNDFILE_HANDLE; ptr: PSingleArray; frames: Tuos_count_t): Tuos_count_t; cdecl; external;

function _sf_read_double(sndfile: TSNDFILE_HANDLE; ptr: PDoubleArray; frames: Tuos_count_t): Tuos_count_t; cdecl; external;

function _sf_write_double(sndfile: TSNDFILE_HANDLE; ptr: PDoubleArray; frames: Tuos_count_t): Tuos_count_t; cdecl; external;

function _sf_close(sndfile: TSNDFILE_HANDLE): integer; cdecl; external;

function _sf_write_sync(sndfile: TSNDFILE_HANDLE): integer; cdecl; external;

// libsndfile
procedure _BitBufferReadSmall; external;
procedure _BitBufferRead; external;
procedure _set_ag_params; external;
procedure _dyn_decomp; external;
procedure _BitBufferByteAlign; external;
procedure _BitBufferReadOne; external;
procedure _dyn_comp; external;
procedure _BitBufferWrite; external;
procedure _BitBufferGetPosition; external;
procedure _set_standard_ag_params; external;
procedure _alac_init; external;
procedure _alac_get_desc_chunk_items; external;
procedure _psf_decode_frame_count; external;
procedure _g723_16_decoder; external;
procedure _g723_24_decoder; external;
procedure _g721_decoder; external;
procedure _g723_40_decoder; external;
procedure _g723_16_encoder; external;
procedure _g723_24_encoder; external;
procedure _g721_encoder; external;
procedure _g723_40_encoder; external;
procedure _g72x_reader_init; external;
procedure _g72x_writer_init; external;
procedure _g72x_decode_block; external;
procedure _g72x_encode_block; external;
procedure _Gsm_Decoder; external;
procedure _Gsm_Coder; external;
procedure _psf_get_date_str; external;
procedure _psf_asciiheader_printf; external;
procedure _u_bitwidth_to_subformat; external;
procedure _gsm_add; external;
procedure _gsm_sub; external;
procedure _gsm_asl; external;
procedure _gsm_asr; external;
procedure _psf_use_rsrc; external;
procedure _psf_file_valid; external;
procedure _psf_open_rsrc; external;
procedure _psf_close_rsrc; external;
procedure _psf_allocate; external;
procedure _psf_init_files; external;
procedure _psf_set_stdio; external;
procedure _psf_fopen; external;
procedure _psf_set_file; external;
procedure _psf_fsync; external;
procedure _psf_get_format_simple_count; external;
procedure _psf_get_format_major_count; external;
procedure _psf_get_format_subtype_count; external;
procedure _psf_calc_signal_max; external;
procedure _dither_init; external;
procedure _broadcast_var_set; external;
procedure _cart_var_set; external;
procedure _psf_get_cues; external;
procedure _psf_cues_dup; external;
procedure _cart_var_get; external;
procedure _broadcast_var_get; external;
procedure _psf_ftruncate; external;
procedure _psf_get_max_all_channels; external;
procedure _psf_get_signal_max; external;
procedure _psf_calc_max_all_channels; external;
procedure _psf_get_format_info; external;
procedure _psf_get_format_subtype; external;
procedure _psf_get_format_major; external;
procedure _psf_get_format_simple; external;
procedure _psf_memset; external;
procedure _id3_skip; external;
procedure _psf_fclose; external;
procedure _psf_default_seek; external;
procedure _psf_rand_int32; external;
procedure _aiff_open; external;
procedure _au_open; external;
procedure _raw_open; external;
procedure _rf64_open; external;
procedure _paf_open; external;
procedure _nist_open; external;
procedure _ircam_open; external;
procedure _sds_open; external;
procedure _ogg_open; external;
procedure _dwd_open; external;
procedure _mat4_open; external;
procedure _mat5_open; external;
procedure _pvf_open; external;
procedure _htk_open; external;
procedure _sd2_open; external;
procedure _rx2_open; external;
procedure _avr_open; external;
procedure _flac_open; external;
procedure _caf_open; external;
procedure _mpc2k_open; external;
procedure _psf_log_SF_INFO; external;
procedure _ima_oki_adpcm_init; external;
procedure _ima_oki_adpcm_decode_block; external;
procedure _ima_oki_adpcm_encode_block; external;
procedure _append_snprintf; external;
procedure _gsm_norm; external;
procedure _gsm_mult; external;
procedure _gsm_div; external;
procedure _wavlike_ima_init; external;
procedure _wavlike_msadpcm_init; external;
procedure _wavlike_msadpcm_write_adapt_coeffs; external;
procedure _audio_detect; external;
procedure _broadcast_var_alloc; external;
procedure _psf_location_string_count; external;
procedure _cart_var_alloc; external;

end.


