/*
 * libs3m - S3M player routines for music support in you programs.
 * Copyright (C) 2017  christian <irqmask@web.de>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * \file s3m.h
 * S3M player application programming interface.
 *
 * \author  CV (irqmask@web.de)
 * \date    2017-05-01
 */
#ifndef _S3M_H_
#define _S3M_H_
/* Include section -----------------------------------------------------------*/
#include <stddef.h>
#include <stdint.h>
#include "stdbool.h"

#ifdef __cplusplus
extern "C" {
#endif
        
/* Definitions ---------------------------------------------------------------*/

#define S3M_MAX_INSTRUMENTS             128
#define S3M_MAX_PATTERNS                128
#define S3M_MAX_ROWS_PER_PATTERN        64

#define S3M_MAX_SONG_NAME               0x1C
#define S3M_MAX_CHANNELS                32
#define S3M_VIBRATO_TABLE_SIZE          64

/* Type definitions ----------------------------------------------------------*/

typedef struct _s3m_header {
    char            song_name[S3M_MAX_SONG_NAME];
    char            id_char;
    uint8_t         file_type;
    uint8_t         unused1[2];
    uint16_t        arrangement_length;
    uint16_t        num_instruments;
    uint16_t        num_patterns;
    uint16_t        flags;
    uint16_t        version_tracker;
    uint16_t        version_file_format;
    char            ident[4];
    uint8_t         global_vol;
    uint8_t         start_speed;
    uint8_t         start_tempo;
    uint8_t         master_vol;
    uint8_t         unused2[12];
    uint8_t         channel_settings[S3M_MAX_CHANNELS];
} s3m_header_t;

typedef struct _s3m_sample {
    uint8_t         type;
    char            filename[12];
    uint8_t         file_offs_sample[3];
    uint32_t        length;
    uint32_t        loop_begin;
    uint32_t        loop_end;
    uint8_t         volume;
    uint8_t         unused1;
    uint8_t         pack;
    uint8_t         flags;
    uint32_t        c4_speed;
    uint8_t         unused2[12];
    char            name[28];
    char            ident[4];
} s3m_sample_t;

typedef struct _s3m_adlib {
    uint8_t         type;
    char            filename[12];
    uint8_t         unused1[3];
    uint8_t         d[12];
    uint8_t         volume;
    uint8_t         dsk;
    uint8_t         unused2[2];
    uint32_t        c4_speed;
    uint8_t         unused3[12];
    char            name[28];
    char            ident[4];
} s3m_adlib_t;

typedef union {
    uint8_t         type;
    s3m_sample_t    sample;
    s3m_adlib_t     adlib;
} s3m_instrument_t;

typedef struct _pat_entry {
    uint8_t     chn;
    uint8_t     note;
    uint8_t     instr;
    uint8_t     vol;
    uint8_t     cmd;
    uint8_t     info;  
} pat_entry_t;

typedef struct _pat_row {
    pat_entry_t entry_chn[S3M_MAX_CHANNELS];
} pat_row_t;

// hold channel runtime data
typedef struct _channel {
    uint8_t             note;       // last started note
    uint8_t             last_note;  // the note before the current note
    uint8_t             instr;      // index of instrument/sample

    s3m_instrument_t*   pi;         // pointer to instrument structure
    int8_t*             ps;         // pointer to sample data
    
    int16_t              vol;       // current channel volume
    
    double              sam_pos;    // index to sample position
    double              sam_period; // sample period
    double              sam_target_period; // sample period
    double              sam_last_period; // sample period
    double              sam_incr;   // sample increment
    
    uint8_t             retrig_fr;  // frames between retrigger

    bool                do_vol_slide;
    bool                do_tone_slide;
    bool                do_tone_porta;
    bool                do_vibrato;
    bool                do_tremolo;
    bool                do_tremor;
    bool                do_arpeggio;
    
    int8_t              vol_slide; // last applied volume slide parameter
    double              tone_slide; // last applied tone slide parameter
    uint8_t             vibrato_speed;
    uint8_t             vibrato_intensity;
    uint8_t             vibrato_pos;
    uint8_t             cmd;        // fx command
    uint8_t             param;      // fx parameter
} channel_t;

// hold runtime data, while playing
typedef struct _runtime {
    uint8_t         tempo; // tempo in audio frames per second
    uint8_t         speed; // speed in frames per row of a pattern

    uint8_t         global_vol;
    uint8_t         master_vol;
    
    bool            playing;            // currently playing or not?
    
    uint32_t        sample_ctr;
    uint32_t        sample_per_frame;
    
    uint8_t         frame_ctr;  // counts every frame, is reset when
    
    uint8_t         pattern_idx;
    uint8_t*        pattern;
    uint8_t         row_ctr;
    uint8_t         skip_rows;  // for FX Cxx: break pattern and in new one goto row xx
    
    uint8_t         order_idx;
    channel_t       chns[S3M_MAX_CHANNELS];
} runtime_t;

struct _s3m;

typedef void (*s3m_func_t)(struct _s3m* s3m, void* arg);

typedef struct _s3m {
    uint8_t*            buffer;
    size_t              filesize;
    uint32_t            samplerate;
    int16_t             vibrato_table[S3M_VIBRATO_TABLE_SIZE];
    
    // extracted data from buffer (NOT a copy, just pointer!!!)
    s3m_header_t*       header;
    s3m_instrument_t*   instrument[S3M_MAX_INSTRUMENTS];   
    double              instr_c4_incr[S3M_MAX_INSTRUMENTS];
    uint8_t*            sample[S3M_MAX_INSTRUMENTS];
    uint8_t*            pattern[S3M_MAX_PATTERNS];
    uint8_t*            order;                          // orders
    
    runtime_t           rt;                 // runtime data
    s3m_func_t          row_chg_callback;   // row changed callback
    s3m_func_t          row_chg_callback_arg;// row changed callback argument
} s3m_t;

/* Global variables ----------------------------------------------------------*/

/* Public interface ----------------------------------------------------------*/

int s3m_initialize(s3m_t* s3m, uint32_t samplerate);
void s3m_sound_callback(void* arg, uint8_t* streambuf, int bufferlength);

int s3m_from_ram(s3m_t* s3m, uint8_t* buffer, size_t length);

//int s3m_load(s3m_t* s3m, const char* filename);
void s3m_unload(s3m_t* s3m);

void s3m_play(s3m_t* s3m);
void s3m_stop(s3m_t* s3m);

uint8_t s3m_get_current_pattern_idx(s3m_t* s3m);
uint8_t s3m_get_current_row_idx(s3m_t* s3m);
void s3m_get_current_row(s3m_t* s3m, pat_row_t* row);
void s3m_register_row_changed_callback(s3m_t* s3m, s3m_func_t func, void* arg);


void s3m_print_header(s3m_t* s3m);
void s3m_print_channels(s3m_t* s3m);
void s3m_print_arrangement(s3m_t* s3m);
void s3m_print_instruments(s3m_t* s3m);
void s3m_print_patterns(s3m_t* s3m);

#ifdef __cplusplus
}
#endif

#endif /* _S3M_H_ */
