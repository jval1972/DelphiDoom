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
 * \file pattern.h
 * Unpack pattern data from memory row by row.
 *
 * \author  CV (irqmask@web.de)
 * \date    2017-05-01
 */
#ifndef _PATTERN_H_
#define _PATTERN_H_
/* Include section -----------------------------------------------------------*/
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "s3m.h"

/* Definitions ---------------------------------------------------------------*/

/* Type definitions ----------------------------------------------------------*/

/* Global variables ----------------------------------------------------------*/

/* Public interface ----------------------------------------------------------*/

bool pat_read_packed_entry(uint8_t** pd, pat_entry_t* entry);

bool pat_read_row(uint8_t** pd, pat_row_t* row);

void pat_skip_rows(uint8_t** pd, uint8_t skip);

#endif /* _PATTERN_H_ */
