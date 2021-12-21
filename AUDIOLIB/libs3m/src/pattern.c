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
 * \file pattern.c
 * Unpack pattern data from memory row by row.
 *
 * \author  CV (irqmask@web.de)
 * \date    2017-05-01
 */

/* Include section -----------------------------------------------------------*/
#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "pattern.h"

/* Definitions ---------------------------------------------------------------*/

/* Type definitions ----------------------------------------------------------*/

/* Global variables ----------------------------------------------------------*/

/* Local variables -----------------------------------------------------------*/

bool pat_read_packed_entry(uint8_t** pd, pat_entry_t* entry)
{
    uint8_t byte;
    uint8_t *p;
//    assert(pd != NULL); assert(*pd != NULL);
//    assert(entry != NULL);

    entry->chn = 255;
    entry->note = 255;
    entry->instr = 255;
    entry->vol = 255;
    entry->cmd = 255;
    entry->info = 255;

    p = *pd; // increase because eventually this is end of row.
    byte = *p++;
    if (byte == 0) {
        *pd = p;
        return false;
    }
    entry->chn = byte & 0x1F;
    if (byte & 0x20) {
        entry->note = *p++;
        entry->instr = *p++;
    }
    if (byte & 0x40) {
        entry->vol = *p++;
    }
    if (byte & 0x80) {
        entry->cmd = *p++;
        entry->info = *p++;
    }
    *pd = p;
    return true;
}

bool pat_read_row(uint8_t** pd, pat_row_t* row)
{
    pat_entry_t entry;

//    assert(pd != NULL);
//    assert(row != NULL);

    memset(row, 255, sizeof(pat_row_t));
    if (!pat_read_packed_entry(pd, &entry)) return false;

    do {
        memcpy(&row->entry_chn[entry.chn], &entry, sizeof(entry));
    } while (pat_read_packed_entry(pd, &entry));
    return true;
}

void pat_skip_rows(uint8_t** pd, uint8_t skip)
{
    pat_row_t row;

    while (skip-- > 0) {
        pat_read_row(pd, &row);
    }
}

/* EOF: pattern.c */
