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
 * \file s3m_intern.h
 * Miscellanneous internal definitions and functions.
 *
 * \author  CV (irqmask@web.de)
 * \date    2017-05-01
 */
#ifndef _S3M_INTERN_H_
#define _S3M_INTERN_H_
/* Include section -----------------------------------------------------------*/
#include <stddef.h>
#include <stdint.h>

#include "s3m.h"

/* Definitions ---------------------------------------------------------------*/

#define S3M_FLAG_ST2_VIBRATO            (1<<0)
#define S3M_FLAG_ST2_TEMPO              (1<<1)
#define S3M_FLAG_AMIGA_SLIDING          (1<<2)
#define S3M_FLAG_OPT0VOLol              (1<<3)
#define S3M_FLAG_OBSERVE_AMIGA_BOUND    (1<<4)
#define S3M_FLAG_EN_FILTR_AND_SND_FX    (1<<5)

#define S3M_FLAG_INSTR_LOOP             (1<<0)
#define S3M_FLAG_INSTR_STEREO           (1<<1)
#define S3M_FLAG_INSTR_16BITNG          (1<<2)


/* Type definitions ----------------------------------------------------------*/

typedef enum _s3m_chn_type {
    eS3M_CHN_TYPE_S1L = 0,
    eS3M_CHN_TYPE_S2L,
    eS3M_CHN_TYPE_S3L,
    eS3M_CHN_TYPE_S4L,
    eS3M_CHN_TYPE_S5L,
    eS3M_CHN_TYPE_S6L,
    eS3M_CHN_TYPE_S7L,
    eS3M_CHN_TYPE_S8L,
    
    eS3M_CHN_TYPE_S1R,
    eS3M_CHN_TYPE_S2R,
    eS3M_CHN_TYPE_S3R,
    eS3M_CHN_TYPE_S4R,
    eS3M_CHN_TYPE_S5R,
    eS3M_CHN_TYPE_S6R,
    eS3M_CHN_TYPE_S7R,
    eS3M_CHN_TYPE_S8R,
    
    eS3M_CHN_TYPE_A1L,
    eS3M_CHN_TYPE_A2L,
    eS3M_CHN_TYPE_A3L,
    eS3M_CHN_TYPE_A4L,
    eS3M_CHN_TYPE_A5L,
    eS3M_CHN_TYPE_A6L,
    eS3M_CHN_TYPE_A7L,
    eS3M_CHN_TYPE_A8L,
    eS3M_CHN_TYPE_A9L,
    
    eS3M_CHN_TYPE_A1R,
    eS3M_CHN_TYPE_A2R,
    eS3M_CHN_TYPE_A3R,
    eS3M_CHN_TYPE_A4R,
    eS3M_CHN_TYPE_A5R,
    eS3M_CHN_TYPE_A6R,
    eS3M_CHN_TYPE_A7R,
    eS3M_CHN_TYPE_A8R,
    eS3M_CHN_TYPE_A9R,
    
    eS3M_CHN_TYPE_ABL,
    eS3M_CHN_TYPE_ASL,
    eS3M_CHN_TYPE_ATL,
    eS3M_CHN_TYPE_ACL,
    eS3M_CHN_TYPE_AHL,
    
    eS3M_CHN_TYPE_ABR,
    eS3M_CHN_TYPE_ASR,
    eS3M_CHN_TYPE_ATR,
    eS3M_CHN_TYPE_ACR,
    eS3M_CHN_TYPE_AHR,
    eS3M_CHN_TYPE_LAST
} s3m_chn_type;

typedef enum _s3m_fx {
    eS3M_FX00_ARPEGGIO = 0,           //  0 - Jxy - Arpeggio
    eS3M_FX01_PORTA_UP,               //  1 - Fxx - Portamento up: slide up by xx (range 00..DF)
    eS3M_FX02_PORTA_DOWN,             //  2 - Exx - Portamento down: slide down by xx (range 00..DF)
    eS3M_FX03_TONE_PORTA,             //  3 - Gxx - Tone portamento, speed xx
    eS3M_FX04_VIBRATO,                //  4 - Hxy - Vibrato, speed x and depth y
    eS3M_FX05_NOTE_AND_VOL_SLIDE,     //  5 - Lxy - Note and Volume sliding (G00 and Dxy)
    eS3M_FX06_VIBRATO_AND_VOL_SLIDE,  //  6 - Kxy - Vibrato and Volume sliding (H00 and Dxy)
    eS3M_FX07_TREMOLO,                //  7 - Rxy - Tremolo, speed x and depth y
    eS3M_FX08_UNUSED,                 //  8 - Xxx - (unused command 8)
    eS3M_FX09_SAMPLE_OFFSET,          //  9 - Oxx - Set sample offset. Add xx<<8 to current sample pos
    eS3M_FX0A_VOLUME_SLIDE,           // 10 - Dxy - Volume slide up by x, down by y
    eS3M_FX0B_BREAK_PATTERN,          // 11 - Bxx - Break pattern and start pattern of order xx
    eS3M_FX0C_SET_NOTE_VOL,           // 12 - bxx - Set note volume. Unused for S3M
    eS3M_FX0D_BREAK_PATTERN,          // 13 - C00 - Pattern break. Start next.
    eS3M_FX0E_ADDITIONAL,             // 14 - additional effects
    eS3M_FX0F_SET_SPEED,              // 15 - set speed xx (default 06)
} s3m_fx_t;

/* Global variables ----------------------------------------------------------*/

extern s3m_t* s3m__current_playing;

/* Module internal interface -------------------------------------------------*/

void s3m__set_tempo(s3m_t* s3m, uint8_t tempo);
void s3m__set_speed(s3m_t* s3m, uint8_t speed);

void s3m__set_global_vol(s3m_t* s3m, uint8_t vol);
void s3m__set_master_vol(s3m_t* s3m, uint8_t vol);

/* Public interface ----------------------------------------------------------*/

#endif /* _S3M_INTERN_H_ */
