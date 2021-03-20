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
 * \file s3m.c
 * Most parts of main library programming interface.
 *
 * \author  CV (irqmask@web.de)
 * \date    2017-05-01
 */

/* Include section -----------------------------------------------------------*/
#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

#include "channel.h"
#include "s3m.h"
#include "s3m_intern.h"

/* Definitions ---------------------------------------------------------------*/

#define PI 3.14159265359

/* Type definitions ----------------------------------------------------------*/

/* Local variables -----------------------------------------------------------*/

/* Module local variables ----------------------------------------------------*/

s3m_t* s3m__current_playing = NULL;

/* Global variables ----------------------------------------------------------*/

/* Local functions -----------------------------------------------------------*/

static void load_instruments(s3m_t* s3m, uint8_t* buffer, size_t length)
{
    int i;
    uint32_t offs;
    uint16_t instr_offs;
    s3m_header_t* h = s3m->header;
    double c4, sr;
    
    if (h->num_instruments > S3M_MAX_INSTRUMENTS) {
        h->num_instruments = S3M_MAX_INSTRUMENTS;
    }  
    offs = 0x60 + h->arrangement_length;
    for (i=0; i<h->num_instruments; i++) {

        instr_offs = (buffer[offs+1] << 8) | buffer[offs];
        instr_offs <<= 4;
        s3m->instrument[i] = (s3m_instrument_t*)&buffer[instr_offs];
        offs += 2;
        
        if (s3m->instrument[i]->type == 1) {
            c4 = s3m->instrument[i]->sample.c4_speed;
            sr = s3m->samplerate;
            s3m->instr_c4_incr[i] = c4 / sr;
        }
    }
}

static void load_samples(s3m_t* s3m, uint8_t* buffer, size_t length)
{
    int i;
    uint32_t smpl_offs;
    s3m_header_t* h = s3m->header;

    for (i=0; i<h->num_instruments; i++) {
        if (s3m->instrument[i]->type == 1) {
            smpl_offs = s3m->instrument[i]->sample.file_offs_sample[0] << 16 | 
                        s3m->instrument[i]->sample.file_offs_sample[2] << 8 |
                        s3m->instrument[i]->sample.file_offs_sample[1];
            smpl_offs <<= 4;
            if (smpl_offs >= length || (smpl_offs+s3m->instrument[i]->sample.length) >= length) {
                printf("ERROR: sample pointer out of file for instrument %d of type %d \n", i, s3m->instrument[i]->type);
                continue;
            }
            s3m->sample[i] = (uint8_t*)&buffer[smpl_offs];
        } else {
            printf("ERROR: no sample data for instrument %d of type %d\n", i, s3m->instrument[i]->type);
        }
    }
}

static void load_pattern(s3m_t* s3m, uint8_t* buffer, size_t length)
{
    int i;
    uint32_t offs;
    uint32_t pat_offs;
    s3m_header_t* h = s3m->header;

    if (h->num_patterns > S3M_MAX_PATTERNS) {
        h->num_patterns = S3M_MAX_PATTERNS;
    }  
    offs = 0x60 + h->arrangement_length + (h->num_instruments * 2);  
    for (i=0; i<h->num_patterns; i++) {
        pat_offs = (buffer[offs+1] << 8) | buffer[offs];
        pat_offs <<= 4;
        s3m->pattern[i] = &buffer[pat_offs];
        offs += 2;
    }
}

/* Public functions ----------------------------------------------------------*/

/**
 * Initialize the s3m structure. Do this first, before loading a S3M file 
 * from RAM or disk.
 */
int s3m_initialize(s3m_t* s3m, uint32_t samplerate)
{
    int i;
    double vib_val;

    if(!s3m) {
        return 0;
    }

    memset(s3m, 0, sizeof(s3m_t));
    s3m->samplerate = samplerate;

    for (i=0; i<S3M_VIBRATO_TABLE_SIZE; i++) {
        vib_val = (sin(2*i*PI/S3M_VIBRATO_TABLE_SIZE) * 256);
        s3m->vibrato_table[i] = vib_val + 0.5;
    }

    return 1;
}

/** 
 * Register a callback, which is called, whenever a new row wat executed
 * This callbak is called from sound thread!! Quit the callback fast!
 */
void s3m_register_row_changed_callback(s3m_t* s3m, s3m_func_t func, void* arg)
{
//    assert(s3m != NULL);
    
    s3m->row_chg_callback = func;
    s3m->row_chg_callback_arg = arg;
}

/**
 * Load a S3M file from RAM. The S3M file just needs to be loaded completed
 * info memory before calling this function, without any interpretation of the
 * S3M file content.
 */
int s3m_from_ram(s3m_t* s3m, uint8_t* buffer, size_t length)
{
    int i,o, retval = -1;
    double incr;

//    assert(s3m != NULL);

    do {
        s3m->buffer = buffer;
        s3m->filesize = length;
        s3m->header = (s3m_header_t*)s3m->buffer;
        s3m->header->song_name[S3M_MAX_SONG_NAME-1] = '\0';
        s3m->order = &s3m->buffer[0x60];
        load_instruments(s3m, s3m->buffer, s3m->filesize);
        load_samples(s3m, s3m->buffer, s3m->filesize);
        load_pattern(s3m, s3m->buffer, s3m->filesize);

        retval = 0;
    } while (0);

    return retval;
}

/**
 * Start playing the S3M file.
 */
void s3m_play(s3m_t* s3m)
{
    int c;

    if (!s3m) {
        return;
    }

    if (s3m->rt.playing == false) {
        s3m__current_playing = s3m;

        s3m__set_tempo(s3m, s3m->header->start_tempo);
        s3m__set_speed(s3m, s3m->header->start_speed);
        s3m__set_global_vol(s3m, s3m->header->global_vol);
        s3m__set_master_vol(s3m, s3m->header->master_vol);

        s3m->rt.frame_ctr = 0;
        s3m->rt.sample_ctr = 0;

        // load first pattern
        s3m->rt.order_idx = 0;
        s3m->rt.row_ctr = 0;
        s3m->rt.pattern_idx = s3m->order[s3m->rt.order_idx];
        s3m->rt.pattern = &s3m->pattern[s3m->rt.pattern_idx][2];
        s3m->rt.playing = true;
        for (c=0; c<S3M_MAX_CHANNELS; c++) {
            chn_reset(&s3m->rt.chns[c]);
        }
    }
}

/**
 * Stop playing the S3M file.
 */
void s3m_stop(s3m_t* s3m)
{
    if (!s3m) {
        return;
    }

    if (s3m->rt.playing == true) {
        s3m->rt.playing = false;
    }
}

/**
 * Get the current pattern index.
 * This function is expected mainly to be used for visualization in S3M player apps.
 *
 * @note Be aware, the pattern index is changed within the context of the audio
 * callback function s3m_sound_callback(). This might be another thread and no
 * synchronization tchnique is used to get it!
 */
uint8_t s3m_get_current_pattern_idx(s3m_t* s3m)
{
    if (!s3m) {
        return 0;
    }

    return s3m->rt.pattern_idx;
}

/**
 * Get the current row index (0..63).
 * This function is expected mainly to be used for visualization in S3M player apps.
 * 
 * @note Be aware, the pattern index is changed within the context of the audio 
 * callback function s3m_sound_callback(). This might be another thread and no 
 * synchronization tchnique is used to get it!
 */
uint8_t s3m_get_current_row_idx(s3m_t* s3m)
{
    if (!s3m) {
        return 0;
    }

    return s3m->rt.row_ctr;
}

/** 
 * Copy the current row data. 
 * 
 * This function is expected mainly to be used for visualization in S3M player apps.
 * To avoid synchronization with the audio thread, 
 * internally a A/B buffer is used e.g. the thread writes into B while this 
 * function reads from A. When the thred is finished writing, is switches the 
 * pages and will now write to A and this function will read B.
 *
 * @note Be aware, the pattern index is changed within the context of the audio 
 * callback function s3m_sound_callback(). This might be another thread and no 
 * synchronization tchnique is used to get it!
 */
void s3m_get_current_row(s3m_t* s3m, pat_row_t* row)
{
//    assert(s3m != NULL);
    //! @todo implement
}

/* EOF: s3m.c */
