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
 * \file s3m_sound.c
 * Audio callback and player logic.
 *
 * \author  CV (irqmask@web.de)
 * \date    2017-05-01
 */

/* Include section -----------------------------------------------------------*/
#include <assert.h>

#include <stdio.h>
#include <string.h>
#include <stdint.h>

#include "channel.h"
#include "pattern.h"
#include "s3m.h"
#include "s3m_intern.h"

/* Definitions ---------------------------------------------------------------*/

/* Type definitions ----------------------------------------------------------*/

/* Local variables -----------------------------------------------------------*/

/* Global variables ----------------------------------------------------------*/

/* Local functions -----------------------------------------------------------*/

static uint32_t round_to_u32(double value)
{
    value += 0.5;
    return value;
}

static void process_row(s3m_t* s3m)
{
    int c;
    pat_row_t       row;
    pat_entry_t*    e;
    channel_t*      chn;
    
    pat_read_row(&s3m->rt.pattern, &row);
    // process row content
    for (c=0; c<S3M_MAX_CHANNELS; c++) {
        if (s3m->header->channel_settings[c] > eS3M_CHN_TYPE_S8R) continue;
        e = &row.entry_chn[c];
        chn = &s3m->rt.chns[c];
        if (e->note != 255) {
            chn_play_note(s3m, chn, e->instr, e->note);
        }
        if (e->vol != 255) {
            chn_set_volume(s3m, chn, e->vol);
        }
        // do fx for every not, so previous effects will be stopped
        chn_do_fx(s3m, chn, e->cmd, e->info);
    }
      
    // get next row
    s3m->rt.row_ctr++;
    if (s3m->rt.row_ctr >= S3M_MAX_ROWS_PER_PATTERN) {
        s3m->rt.row_ctr = 0;
        s3m->rt.order_idx++;
        // select next pattern
        if (s3m->rt.order_idx >= s3m->header->arrangement_length) {
            // song is finished, stop.
            s3m->rt.playing = false;            
            return;
        }
        s3m->rt.pattern_idx = s3m->order[s3m->rt.order_idx];
        if (s3m->rt.pattern_idx >= s3m->header->num_patterns) {
            // song is finished, stop.
            s3m->rt.playing = false;            
            return;
        }
        s3m->rt.pattern = &s3m->pattern[s3m->rt.pattern_idx][2];
        // for Cxx effect: Break pattern and in new one goto row xx
        if (s3m->rt.skip_rows > 0) {
            pat_skip_rows(&s3m->rt.pattern, s3m->rt.skip_rows);                
            s3m->rt.row_ctr += s3m->rt.skip_rows;
            s3m->rt.skip_rows = 0;
        }
    }

    if (s3m->row_chg_callback != NULL) {
        s3m->row_chg_callback(s3m, s3m->row_chg_callback_arg);
    }    
}

static void process_frame(s3m_t* s3m)
{
    int c;
    
    if (s3m->rt.frame_ctr-- == 0) {
        s3m->rt.frame_ctr = s3m->rt.speed - 1;
        process_row(s3m);
    }
        
    for (c=0; c<S3M_MAX_CHANNELS; c++) {
        if (s3m->header->channel_settings[c] > eS3M_CHN_TYPE_S8R) continue;
        
        // process effects which apply every tick
        chn_do_fx_frame(s3m, &s3m->rt.chns[c]);
    }
}

static void mix_samples_of_channels(s3m_t* s3m, int16_t* l_sample, int16_t* r_sample)
{
    double ls = 0, rs = 0, volfact;
    int32_t l, r;
    int c, lcc = 0, rcc = 0;
    s3m_chn_type ct;
    
    for (c=0; c<S3M_MAX_CHANNELS; c++) {
        ct = s3m->header->channel_settings[c];
        if (ct >= eS3M_CHN_TYPE_S1L && ct <= eS3M_CHN_TYPE_S8L) {
            // mix to left channel
            ls += chn_get_sample(s3m, &s3m->rt.chns[c]);
            lcc++; // left channel count
        } else if (ct >= eS3M_CHN_TYPE_S1R && ct <= eS3M_CHN_TYPE_S8R) {
            // mix to right channel
            rs += chn_get_sample(s3m, &s3m->rt.chns[c]);
            rcc++; // right channel count
        }
    }
    
    volfact = s3m->rt.global_vol * s3m->rt.master_vol / (64.0*64.0); 
    l = ls * volfact;
    r = rs * volfact;
    
    //l=ls /lcc;
    //r = rs / rcc;
    
    if (l < INT16_MIN) l = INT16_MIN;
    if (l > INT16_MAX) l = INT16_MAX;
    
    if (r < INT16_MIN) r = INT16_MIN;
    if (r > INT16_MAX) r = INT16_MAX;
    //if (lcc) ls /= lcc; // normalize amplitude
    //if (rcc) rs /= rcc;
    *l_sample = l;
    *r_sample = r;  
}

/* Module local functions ----------------------------------------------------*/

void s3m__set_tempo(s3m_t* s3m, uint8_t tempo)
{
    double sample_per_frame;

    if (tempo == 0) return;
    s3m->rt.tempo = tempo;
    sample_per_frame = 2.5 * s3m->samplerate / s3m->rt.tempo;
    s3m->rt.sample_per_frame = round_to_u32(sample_per_frame);
    s3m->rt.sample_ctr = s3m->rt.sample_per_frame - 1;
}

void s3m__set_speed(s3m_t* s3m, uint8_t speed)
{
    if (speed == 0) return;
    s3m->rt.speed = speed;
    s3m->rt.frame_ctr = s3m->rt.speed - 1;    
}

void s3m__set_global_vol(s3m_t* s3m, uint8_t vol)
{
    if (vol > 64) vol = 64;
    s3m->rt.global_vol = vol;
}

void s3m__set_master_vol(s3m_t* s3m, uint8_t vol)
{
    vol &= 0x7F;
    s3m->rt.master_vol = vol;
}

/* Public functions ----------------------------------------------------------*/

void s3m_sound_callback(void* arg, uint8_t* streambuf, int bufferlength)
{
    int i, bi, num_samples;
    int16_t l_sample = 0, r_sample = 0;
    s3m_t* s3m = s3m__current_playing;
    
    // The format of the byte stream is signed 16-bit samples in little-endian 
    // byte order. Stereo samples are stored in a LRLRLR ordering.
    // sample     |             1             |             2             |...
    // channel    |    left     |    right    |    left     |    right    |...
    // byte order | low  | high | low  | high | low  | high | low  | high |...
    // stream     | [0]  | [1]  | [2]  | [3]  | [4]  | [5]  | [6]  | [7]  |...
    
    // Hence num_samples must be divided by four.
   
    num_samples = bufferlength >> 2;
    bi = 0; // start with byte index at the beginning
    
    for (i=0; i<num_samples; i++) {
        if (s3m != NULL && s3m->rt.playing) {
            if (s3m->rt.sample_ctr-- == 0) {
                process_frame(s3m);
                s3m->rt.sample_ctr = s3m->rt.sample_per_frame - 1;
            }
            mix_samples_of_channels(s3m, &l_sample, &r_sample);
        }
        streambuf[bi++] = l_sample & 0x00FF;
        streambuf[bi++] = l_sample >> 8;
        streambuf[bi++] = r_sample & 0x00FF;
        streambuf[bi++] = r_sample >> 8;
    }
}

/* EOF: s3m_sound.c */
