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
// DESCRIPTION:
//  S3M library
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$Z+}
{$H+}

unit libs3m;

interface

uses
  d_delphi;

const
  S3M_MAX_INSTRUMENTS = 128;
  S3M_MAX_PATTERNS = 128;
  S3M_MAX_ROWS_PER_PATTERN = 64;
  S3M_MAX_SONG_NAME = 28;
  S3M_MAX_CHANNELS = 32;
  S3M_VIBRATO_TABLE_SIZE = 64;

type  
  s3m_header_t = record
    songname    : array[0..S3M_MAX_SONG_NAME - 1] of char;
    hex1Ah      : byte;
    filetype    : byte; { = 10h }
    reserved1   : array[0..1] of byte;
    songlength  : word;
    instruments : word;
    patterns    : word;
    flags       : word;
    version     : word;
    fileformat  : word;
    id          : array[0..3] of char;
    globalvol   : byte;
    starttempo  : byte;
    startbpm    : byte;
    mastervol   : byte;
    ultaclick   : byte;
    defpan      : byte;
    reserved2   : array[0..7] of byte;
    special     : word;
    channelinfo : array[0..S3M_MAX_CHANNELS - 1] of byte;
  end;
  Ps3m_header_t = ^s3m_header_t;

  ts3sample_t = record
    typ       : byte;
    filename  : array[0..11] of char;
    unused    : byte;
    segment   : word;
    size      : longint;
    loopstart : longint;
    loopend   : longint;
    volume    : byte;
    disk      : byte;
    packing   : byte;
    flags     : byte;
    c2spd     : longint;
    reserved  : array[0..3] of byte;
    GrvsRamPos: word;{Internal use for GUS}
    loopexpans: word;{Internal use for SB}
    lastpos   : longint;{Internal use for SB}
    samplename: array[0..27] of char;
    kennung   : array[0..3] of char;{SCRS}
  end;
  Pts3sample_t = ^ts3sample_t;

  channel_t = record
    note: byte;       // last started note
    last_note: byte;  // the note before the current note
    instr: byte;      // index of instrument/sample

    pi: Pts3sample_t;         // pointer to instrument structure
    ps: PShortIntArray;       // pointer to sample data

    vol: smallint;  // current channel volume

    sam_pos: double;    // index to sample position
    sam_period: double; // sample period
    sam_target_period: double; // sample period
    sam_last_period: double; // sample period
    sam_incr: double;   // sample increment

    retrig_fr: byte;  // frames between retrigger

    do_vol_slide: byte;
    do_tone_slide: byte;
    do_tone_porta: byte;
    do_vibrato: byte;
    do_tremolo: byte;
    do_tremor: byte;
    do_arpeggio: byte;

    vol_slide: shortint; // last applied volume slide parameter
    tone_slide: double; // last applied tone slide parameter
    vibrato_speed: byte;
    vibrato_intensity: byte;
    vibrato_pos: byte;
    cmd: byte;        // fx command
    param: byte;      // fx parameter
  end;
  Pchannel_t = ^channel_t;

  runtime_t = record
    tempo: byte; // tempo in audio frames per second
    speed: byte; // speed in frames per row of a pattern
    global_vol: byte;
    master_vol: byte;

    playing: byte;            // currently playing or not?

    sample_ctr: LongWord;
    sample_per_frame: LongWord;

    frame_ctr: byte;  // counts every frame, is reset when

    pattern_idx: byte;
    pattern: PByteArray;
    row_ctr: byte;
    skip_rows: byte;  // for FX Cxx: break pattern and in new one goto row xx

    order_idx: byte;
    chns: array[0..S3M_MAX_CHANNELS - 1] of channel_t;
  end;
  Pruntime_t = ^runtime_t;

  s3m_func_t = procedure (s3m: pointer; arg: pointer);

  s3m_t = record
    buffer: pointer;
    filesize: integer;
    samplerate: LongWord;
    vibrato_table: array[0..S3M_VIBRATO_TABLE_SIZE - 1] of smallint;

    // extracted data from buffer (NOT a copy, just pointer!!!)
    header: Ps3m_header_t;
    instruments: array[0..S3M_MAX_INSTRUMENTS - 1] of Pts3sample_t;
    instr_c4_incr: array[0..S3M_MAX_INSTRUMENTS - 1] of double;
    sample: array[0..S3M_MAX_INSTRUMENTS - 1] of PByteArray;
    pattern: array[0..S3M_MAX_PATTERNS - 1] of PByteArray;
    order: PByteArray;  // orders

    rt: runtime_t;  // runtime data
    row_chg_callback: s3m_func_t; // row changed callback
    row_chg_callback_arg: s3m_func_t; // row changed callback argument
    filler: array[0..1023] of byte;
  end;
  Ps3m_t = ^s3m_t;

procedure _s3m_play(s3m: Ps3m_t); cdecl;

function _s3m_initialize(s3m: Ps3m_t; samplerate: LongWord): integer; cdecl;

procedure _s3m_sound_callback(arg: pointer; streambuf: Pointer; bufferlength: integer); cdecl;

function _s3m_from_ram(s3m: Ps3m_t; buffer: pointer; len: integer): integer; cdecl;

procedure _s3m_stop(s3m: Ps3m_t); cdecl;

function _s3m_get_current_pattern_idx(s3m: Ps3m_t): byte; cdecl;

function _s3m_get_current_row_idx(s3m: Ps3m_t): byte; cdecl;

procedure _s3m_get_current_row(s3m: Ps3m_t; row: pointer); cdecl;

procedure _s3m_register_row_changed_callback(s3m: Ps3m_t; func: s3m_func_t; arg: pointer); cdecl;

implementation

uses
  c_lib, scanf, scanf_c;

{$L libs3m\obj\channel.obj}
{$L libs3m\obj\pattern.obj}
{$L libs3m\obj\s3m.obj}
{$L libs3m\obj\s3m_info.obj}
{$L libs3m\obj\s3m_sound.obj}

procedure _chn_reset; external;
procedure _pat_read_row; external;
procedure _chn_play_note; external;
procedure _chn_set_volume; external;
procedure _chn_do_fx; external;
procedure _pat_skip_rows; external;
procedure _chn_do_fx_frame; external;
procedure _chn_get_sample; external;
procedure _s3m__current_playing; external;

procedure _s3m_play(s3m: Ps3m_t); cdecl; external;

function _s3m_initialize(s3m: Ps3m_t; samplerate: LongWord): integer; cdecl; external;

procedure _s3m_sound_callback(arg: pointer; streambuf: Pointer; bufferlength: integer); cdecl; external;

function _s3m_from_ram(s3m: Ps3m_t; buffer: pointer; len: integer): integer; cdecl; external;

procedure _s3m_stop(s3m: Ps3m_t); cdecl; external;

function _s3m_get_current_pattern_idx(s3m: Ps3m_t): byte; cdecl; external;

function _s3m_get_current_row_idx(s3m: Ps3m_t): byte; cdecl; external;

procedure _s3m_get_current_row(s3m: Ps3m_t; row: pointer); cdecl; external;

procedure _s3m_register_row_changed_callback(s3m: Ps3m_t; func: s3m_func_t; arg: pointer); cdecl; external;

end.

