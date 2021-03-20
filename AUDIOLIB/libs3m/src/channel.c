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
 * \file channel.c
 * Do runtime channel tasks, like playing notes, doing effects, getting samples
 * for mixing from the channel.
 *
 * \author  CV (irqmask@web.de)
 * \date    2017-05-01
 */

/* Include section -----------------------------------------------------------*/
#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "channel.h"
#include "s3m.h"
#include "s3m_intern.h"

/* Definitions ---------------------------------------------------------------*/

/* Type definitions ----------------------------------------------------------*/

/* Local variables -----------------------------------------------------------*/

static const uint16_t g_note_period[12] = 
    { 1712, 1616, 1524, 1440, 1356, 1280, 1208, 1140, 1076, 1016, 960, 907 };
    
/* Global variables ----------------------------------------------------------*/

/* Local functions -----------------------------------------------------------*/

/* Module local functions ----------------------------------------------------*/

/* Module global functions ---------------------------------------------------*/

void chn_reset(channel_t* chn)
{
    chn->pi = NULL;
    chn->ps = NULL;

    chn->cmd = 255;
    chn->note = 255;
    chn->last_note = 255; 
    chn->instr = 255;
    
    chn->vol = 64;
    
    chn->do_vol_slide = false;
    chn->do_tone_slide = false;
    chn->do_tone_porta = false;
    chn->do_vibrato = false;
    chn->do_tremor = false;
    chn->do_arpeggio = false;
    chn->do_tremolo = false;
    
    chn->vol_slide = 0;
    chn->tone_slide = 0;    
    chn->retrig_fr = 0;
    chn->sam_pos = 0.0; 
    chn->sam_period = g_note_period[0]; // C-4 is default
    chn->sam_target_period = g_note_period[0]; // C-4 is default
    chn->sam_last_period = g_note_period[0]; // C-4 is default
    chn->sam_incr = 0.0;
}

void chn_update_sample_increment(s3m_t* s3m, channel_t* chn)
{
    double note_herz;
    //                 14317056        8363 * 1712
    // note_herz = --------------- = ---------------
    //              sample_period     sample_period
    note_herz = 14317056 / chn->sam_period;
    
    //                      note_herz
    // sample_increment = --------------
    //                      samplerate   
    chn->sam_incr = note_herz / s3m->samplerate;
}

void chn_calc_note_incr(s3m_t* s3m, channel_t* chn, uint8_t note)
{
    uint8_t n, o, of;
    double instr_c4_speed, herz, incr;
    
    o = note >> 4;
    n = note & 0x0F;
    if (o > 7) o = 7;
    if (n > 11) n = 11;   

    //                  8363 * 16 * (period(note) >> octave(note))
    // sample_period = --------------------------------------------
    //                             instrument_c4_speed
    instr_c4_speed = chn->pi->sample.c4_speed;
    chn->sam_last_period = chn->sam_target_period;
    chn->sam_target_period = (8363.0 * 16 * (g_note_period[n] >> o)) / instr_c4_speed;
    chn->sam_period = chn->sam_target_period; // use this if no tone portamento or vibrato is done
    chn_update_sample_increment(s3m, chn);
}

void chn_play_note(s3m_t* s3m, channel_t* chn, uint8_t instr, uint8_t note)
{   
    // don't play note, if it is >= 12 (more than one octave)
    if ((note & 0x0F) >= 12) return;
    // scream tracker counts instruments beginning from 1
    if (instr > 0) instr--;   
    if (instr >= s3m->header->num_instruments) return;
    chn->last_note = chn->note;
    chn->note = note;
    chn->instr = instr;
    
    // set instrument/sample pointer
    chn->pi = s3m->instrument[instr];
    chn->ps = s3m->sample[instr];
    
    if (chn->pi->type == 1) {
        chn->vol = chn->pi->sample.volume;
    } else {
        chn->vol = chn->pi->adlib.volume;
    }
    chn->sam_pos = 0.0;
    
    // select sample increment depending on note
    chn_calc_note_incr(s3m, chn, note);
}

void chn_set_volume(s3m_t* s3m, channel_t* chn, uint8_t vol)
{
    if (vol > 64) vol = 64;
    chn->vol = vol;
}

void chn_do_fx(s3m_t* s3m, channel_t* chn, uint8_t cmd, uint8_t param)
{
    switch (cmd) {
    case 'A'-64:
        // Axx: Set speed to xx (default 06)
        s3m__set_speed(s3m, param);
        break;
            
    case 'C'-64:
        // Cxx: Break pattern to row xx
        s3m->rt.row_ctr = S3M_MAX_ROWS_PER_PATTERN;
        if (param >= S3M_MAX_ROWS_PER_PATTERN) param = S3M_MAX_ROWS_PER_PATTERN - 1;
        s3m->rt.skip_rows = param;
        break;
            
    case 'D'-64:
        // DFx: fine volume slide down by x
        if ((param & 0xF0) == 0xF0) {
            chn->vol_slide = -(param &= 0x0F);
        }
        // DxF: fine volume slide up by x
        else if (param & 0x0F == 0x0F) {
            chn->vol_slide = (param & 0xF0) >> 4;         
        }
        // D0x or Dx0: volume slide down/up
        else if (param != 0x00) {
            chn->do_vol_slide = true;
            chn->do_tone_slide = false;
            chn->do_tone_porta = false;
            chn->do_vibrato = false;
            chn->do_tremor = false;
            chn->do_arpeggio = false;
            chn->do_tremolo = false;            
        }
        // only fine volume slide
        if (!chn->do_vol_slide) {
            chn->vol += chn->vol_slide;
            if (chn->vol < 0) chn->vol = 0;
            if (chn->vol > 64) chn->vol = 64;
        }
        break;
        
    case 'E'-64:
        // EFx: Fine slide down by x
        if ((param & 0xF0) == 0xF0) {
            // sliding down tonal means increasing the period
            chn->tone_slide = (param & 0x0F) * 5;
        } 
        // EEx: Extra fine slide down by x
        else if ((param & 0xF0) == 0xE0) {
            // sliding down tonal means increasing the period
            chn->tone_slide = (param & 0x0F);
        }
        // Exx: slide down by xx every frame
        else if (param != 0x00) {            
            // save for use in every frame
            chn->tone_slide = param * 3.33;
            chn->do_vol_slide = false;
            chn->do_tone_slide = true;
            chn->do_tone_porta = false;
            chn->do_vibrato = false;
            chn->do_tremor = false;
            chn->do_arpeggio = false;
            chn->do_tremolo = false;                       
        }
        // E00: repeat last slide
        if (!chn->do_tone_slide) {
            chn->sam_period += chn->tone_slide;
        }
        break;
        
    case 'F'-64:
        // FFx: Fine slide up by x
        if ((param & 0xF0) == 0xF0) {
            // sliding up tonal means decreasing the period
            chn->tone_slide = -(param & 0x0F) * 5;
        } 
        // FEx: Extra fine slide up by x
        else if ((param & 0xF0) == 0xE0) {
            // sliding up tonal means decreasing the period
            chn->tone_slide = -(param & 0x0F);
        }
        // Fxx: slide up by xx every frame
        else if (param != 0x00) {            
            // save for use in every frame
            chn->tone_slide = -param * 3.33;
            chn->do_vol_slide = false;
            chn->do_tone_slide = true;
            chn->do_tone_porta = false;
            chn->do_vibrato = false;
            chn->do_tremor = false;
            chn->do_arpeggio = false;
            chn->do_tremolo = false;             
        }
        // F00: repeat last slide
        if (!chn->do_tone_slide) {
            chn->sam_period += chn->tone_slide;
        }
        break;
        
    case 'G'-64:
        // Gxx: Tone portamento, speed xx
        if (param != 0) { // G00 repeats old parameter
            // only reset period, if no previous tone portamento was done
            if (chn->tone_slide == 0) {
                chn->sam_period = chn->sam_last_period; // starting point for portamento
            }
            if (chn->sam_target_period < chn->sam_period) {
                chn->tone_slide = -param * 2.4;
            } 
            else if (chn->sam_target_period > chn->sam_period) {
                chn->tone_slide = param * 2.4;
            }
            else chn->tone_slide = 0;
        }
        chn->do_vol_slide = false;
        chn->do_tone_slide = false;
        chn->do_tone_porta = true;
        chn->do_vibrato = false;
        chn->do_tremor = false;
        chn->do_arpeggio = false;
        chn->do_tremolo = false; 
        break;
        
    case 'H'-64:
        // Hxy: Vibrato, speed x and depth y
        if (param != 0x00) {
            chn->vibrato_speed = (param >> 4);
            chn->vibrato_intensity = param & 0x0F;
            chn->vibrato_pos = 0;
            chn->do_vol_slide = false;
            chn->do_tone_slide = false;
            chn->do_tone_porta = true;
            chn->do_vibrato = true;
            chn->do_tremor = false;
            chn->do_arpeggio = false;
            chn->do_tremolo = false;           
        }
        break;
        
    case 'O'-64:
        // Oxx: Set SampleOffset = xx00h
        if (((uint16_t)param << 8) < chn->pi->sample.length) {
            chn->sam_pos = param;
            chn->sam_pos *= 0x100;
        }
        break;
        
    case 'Q'-64:
        // Qxy: retrigger note
        chn->cmd = cmd;
        chn->param = param;
        chn->retrig_fr = param & 0x0F;
        break;

    case 'V'-64:
        // Vxx: Set global volume (0..64)
        s3m__set_global_vol(s3m, param);
        break;
    // ...
        
    default:
        chn->cmd = 255;
        chn->param = 0;
        // in case of vibrato, reset tone period
        if (chn->do_vibrato) {
            chn->sam_period = chn->sam_target_period;
        }
        chn->do_vol_slide = false;
        chn->do_tone_slide = false;
        chn->do_tone_porta = false;
        chn->do_vibrato = false;
        chn->do_tremor = false;
        chn->do_arpeggio = false;
        chn->do_tremolo = false;
        chn->vol_slide = 0;
        chn->tone_slide = 0;
        chn->vibrato_speed = 0;
        chn->vibrato_intensity = 0;
        chn->vibrato_pos = 0;
        chn->retrig_fr = 0;
        break;
    }
    chn_update_sample_increment(s3m, chn);
}

void chn_do_fx_frame(s3m_t* s3m, channel_t* chn)
{
    int16_t val;
    
    uint8_t slide;
    
    if (chn->do_vol_slide) {
        chn->vol += chn->vol_slide;
        if (chn->vol > 64) chn->vol = 64;
        if (chn->vol < 0) chn->vol = 0;       
    }
    if (chn->do_tone_slide) {
        chn->sam_period += chn->tone_slide;
    }
    if (chn->do_tone_porta) {
        chn->sam_period += chn->tone_slide;
        if (chn->sam_target_period < chn->sam_period) {
            if (chn->sam_period <= chn->sam_target_period) { // target reached?
                chn->sam_period = chn->sam_target_period;
                chn->tone_slide = 0;
            }
        } 
        if (chn->sam_target_period > chn->sam_period) {
            if (chn->sam_period >= chn->sam_target_period) { // target reached?
                chn->sam_period = chn->sam_target_period;
                chn->tone_slide = 0;
            }
        }        
    }
    if (chn->do_vibrato) {
        val = s3m->vibrato_table[chn->vibrato_pos];
        val = val * chn->vibrato_intensity;
        val = val / 16;
       
        chn->vibrato_pos += chn->vibrato_speed;
        chn->vibrato_pos %= S3M_VIBRATO_TABLE_SIZE;
        chn->sam_period = chn->sam_target_period + val; 
    }
    
    switch (chn->cmd) {
 /*   case 'D'-64:
        if ((chn->param & 0x0F) != 0) {
            // D0x: volume slide down by x
            slide = (chn->param & 0x0F);
            if (chn->vol > slide) chn->vol -= slide;
            else chn->vol = 0;
        }
        if ((chn->param & 0xF0) != 0) {
            // Dx0: volume slide up by x
            slide = (chn->param & 0xF0) >> 4;
            if (64 - slide < chn->vol) chn->vol += slide;
            else chn->vol = 64;
        }
        break;
        
    case 'E'-64:
    case 'F'-64:
        // Exx: slide down by xx every frame
        // Fxx: slide up by xx every frame
        chn->sam_period += chn->tone_slide;
        break;
       
    case 'G'-64:
        chn->sam_period += chn->last_tone_slide;
        if (chn->sam_target_period < chn->sam_period) {
            if (chn->sam_period <= chn->sam_target_period) { // target reached?
                chn->sam_period = chn->sam_target_period;
                chn->last_tone_slide = 0;
            }
        } 
        else if (chn->sam_target_period > chn->sam_period) {
            if (chn->sam_period >= chn->sam_target_period) { // target reached?
                chn->sam_period = chn->sam_target_period;
                chn->last_tone_slide = 0;
            }
        }
        break;
        
    case 'H'-64:
        val = s3m->vibrato_table[chn->vibrato_pos];
        val = val * chn->vibrato_intensity;
        val = val / 16;
       
        chn->vibrato_pos += chn->vibrato_speed;
        chn->vibrato_pos %= S3M_VIBRATO_TABLE_SIZE;
        chn->sam_period = chn->sam_target_period + val; 
        break;
*/        
    case 'Q'-64:
        // Qxy: retrigger note
        chn->retrig_fr--;
        if (chn->retrig_fr == 0) {
            chn->retrig_fr = chn->param & 0x0F;
            switch (chn->param >> 4) {
            case 0: break;
            case 1: chn->vol -= 1; break;
            case 2: chn->vol -= 2; break;
            case 3: chn->vol -= 4; break;
            case 4: chn->vol -= 8; break;
            case 5: chn->vol -= 16; break;
            case 6: chn->vol *= 2; chn->vol /= 3; break;
            case 7: chn->vol /= 2; break;
            case 8: break;                    
            case 9: chn->vol += 1; break;
            case 10: chn->vol += 2; break;
            case 11: chn->vol += 4; break;
            case 12: chn->vol += 8; break;
            case 13: chn->vol += 16; break;
            case 14: chn->vol *= 3; chn->vol /= 2; break;
            case 15: chn->vol *= 2; break;
            default: break;
            }
            if (chn->vol < 0) chn->vol = 0;
            if (chn->vol > 64) chn->vol = 64;
            chn->sam_pos = 0.0; // finally retrigger by setting sample pos to beginning
        }
        break;

    // ...
        
    default:
        break;
    }
    chn_update_sample_increment(s3m, chn);
}

int16_t chn_get_sample(s3m_t* s3m, channel_t* chn)
{
    uint8_t raw_sample = 0;
    int16_t sample = 0;
    int16_t vol;
    uint32_t spos;
    
    spos = (chn->sam_pos + 0.5);
    if (chn->pi != NULL && chn->ps != NULL && spos < chn->pi->sample.length) {
        raw_sample = chn->ps[spos];
        
        chn->sam_pos = chn->sam_pos + chn->sam_incr;
        spos = chn->sam_pos + 0.5;
        
        // is sample looped?
        if (chn->pi->sample.flags & S3M_FLAG_INSTR_LOOP) {
            if (spos >= chn->pi->sample.loop_end) {
                chn->sam_pos = chn->pi->sample.loop_begin;
            }
        }
    }
    sample = raw_sample - 128;
    sample = sample * chn->vol;
    
    return sample;
}
    

/* EOF: channel.c */
