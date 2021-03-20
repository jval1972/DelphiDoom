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
 * \file channel.h
 * Do runtime channel tasks, like playing notes, doing effects, getting samples
 * for mixing from the channel.
 *
 * \author  CV (irqmask@web.de)
 * \date    2017-05-01
 */
#ifndef _CHANNEL_H_
#define _CHANNEL_H_
/* Include section -----------------------------------------------------------*/
#include <stddef.h>
#include <stdint.h>

#include "s3m.h"

/* Definitions ---------------------------------------------------------------*/

/* Type definitions ----------------------------------------------------------*/

/* Local variables -----------------------------------------------------------*/

/* Global variables ----------------------------------------------------------*/

/* Local functions -----------------------------------------------------------*/

/* Module local functions ----------------------------------------------------*/

/* Module global functions ---------------------------------------------------*/

void chn_reset(channel_t* chn);

void chn_play_note(s3m_t* s3m, channel_t* chn, uint8_t instr, uint8_t note);

void chn_set_volume(s3m_t* s3m, channel_t* chn, uint8_t vol);

void chn_do_fx(s3m_t* s3m, channel_t* chn, uint8_t cmd, uint8_t info);

void chn_do_fx_frame(s3m_t* s3m, channel_t* chn);

int16_t chn_get_sample(s3m_t* s3m, channel_t* chn);

#endif /* _CHANNEL_H_ */
