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
 * \file s3m_info.c
 * For player and debugging purpuses. Print several data of the S3M file.
 *
 * \author  CV (irqmask@web.de)
 * \date    2017-05-01
 */

/* Include section -----------------------------------------------------------*/
#include <stdio.h>
#include <string.h>

#include "pattern.h"
#include "s3m.h"
#include "s3m_intern.h"

/* Definitions ---------------------------------------------------------------*/

/* Type definitions ----------------------------------------------------------*/

/* Local variables -----------------------------------------------------------*/

const char* g_chnTypeName[eS3M_CHN_TYPE_LAST] = {
    "S1-L", //  0
    "S2-L",
    "S3-L",
    "S4-L",
    "S5-L",
    "S6-L",
    "S7-L",
    "S8-L", //  7
    "S1-R", //  8
    "S2-R",
    "S3-R",
    "S4-R",
    "S5-R",
    "S6-R",
    "S7-R",
    "S8-R", //  15
    "A1-L", //  16
    "A2-L",
    "A3-L",
    "A4-L",
    "A5-L",
    "A6-L",
    "A7-L",
    "A8-L",
    "A9-L", // 24
    "A1-R", // 25
    "A2-R",
    "A3-R",
    "A4-R",
    "A5-R",
    "A6-R",
    "A7-R",
    "A8-R",
    "A9-R", // 33
    "AB-L", // 34
    "AS-L",
    "AT-L",
    "AC-L",
    "AH-L", // 38
    "AB-R", // 39
    "AS-R",
    "AT-R",
    "AC-R",
    "AH-R"  // 43
};

const char* g_note_str[16] = {
    "C-",
    "C#",
    "D-",
    "D#",
    "E-",
    "F-",
    "F#",
    "G-",
    "G#",
    "A-",
    "A#",
    "H-",
    "?",
    "?",
    "?",
    "?"
};

const char* g_fx_str[16] = {
    "J",    //  0 - Jxy - Arpeggio
    "F",    //  1 - Fxx - Portamento up: slide up by xx (range 00..DF)
    "E",    //  2 - Exx - Portamento down: slide down by xx (range 00..DF)
    "G",    //  3 - Gxx - Tone portamento, speed xx
    "H",    //  4 - Hxy - Vibrato, speed x and depth y
    "L",    //  5 - Lxy - Note and Volume sliding (G00 and Dxy)
    "K",    //  6 - Kxy - Vibrato and Volume sliding (H00 and Dxy)
    "R",    //  7 - Rxy - Tremolo, speed x and depth y
    "X",    //  8 - Xxx - (unused command 8)
    "O",    //  9 - Oxx - Set sample offset. Add xx<<8 to current sample pos
    "D",    // 10 - Dxy - Volume slide up by x, down by y
    "B",    // 11 - Bxx - Break pattern and start pattern of order xx
    "b",    // 12 - bxx - Set note volume. Unused for S3M.
    "C",    // 13 - C00 - Pattern break. Start next.
    "e",    // 14 - additional effects
    "A"     // 15 - set speed xx (default 06)
};
  
/* Global variables ----------------------------------------------------------*/


void s3m_print_header(s3m_t* s3mHandle)
{
    s3m_header_t* h;
    h = s3mHandle->header;
    printf("S3M Header:\n");
    printf("-----------------------------------------------------------\n");
    printf("  Title:       %-27s\n", h->song_name);
    printf("  Ident 1A:              0x%02X\n", h->id_char);
    printf("  File type:             0x%02X\n", h->file_type);
    //printf("unused1:             0x%04X\n", h->unused1);
    printf("  Arrangement length:   %5d\n", h->arrangement_length);
    printf("  Num. of instruments:  %5d\n", h->num_instruments);
    printf("  Num. of patterns:     %5d\n", h->num_patterns);
    printf("  Flags:                 0x%02X\n", h->flags);
    printf("  Tracker version:     0x%04X\n", h->version_tracker);
    printf("  File format version: 0x%04X\n", h->version_file_format);
    printf("  Ident:                 %c%c%c%c\n", h->ident[0], h->ident[1], h->ident[2], h->ident[3]);
    printf("  Global volume:          %3d\n", h->global_vol);
    printf("  Start speed:            %3d\n", h->start_speed);
    printf("  Start tempo:            %3d\n", h->start_tempo);
    printf("  Master volume:     0x%02X\n", h->master_vol);
    printf("-----------------------------------------------------------\n");
}

void s3m_print_channels(s3m_t* s3mHandle)
{
    int i;
    uint8_t cs;
    
    s3m_header_t* h;
    h = s3mHandle->header;
    printf("Channel settings:\n");
    for (i=0; i<S3M_MAX_CHANNELS; i++) {
        cs = h->channel_settings[i]; 
        printf("  %02d - %03d - %s\n", 
               i, cs, cs < eS3M_CHN_TYPE_LAST ? g_chnTypeName[cs] : "--");
    }
}

void s3m_print_arrangement(s3m_t* s3m)
{
    int i;
    uint8_t cs;
    
    s3m_header_t* h;
    h = s3m->header;
    printf("Arrangement:\n");
    for (i=0; i<h->arrangement_length; i++) {
        printf("  %03d - %03d\n", i, s3m->order[i]);
    }   
}

void s3m_print_instruments(s3m_t* s3m)
{
    int i;  
    s3m_header_t* h;
    s3m_instrument_t* in;
    char fnstr[13];
    
    h = s3m->header;

    printf("Instruments:\n");
    printf("-----------------------------------------------------------\n");
    for (i=0; i<h->num_instruments; i++) {
        in = s3m->instrument[i];
        printf("Instrument: %d\n", i);
        printf("  Type:            %3d\n" , in->type);
        if (in->type == 1) {
            memcpy(fnstr, in->sample.filename, 12);
            fnstr[12] = '\0';
            printf("  Filename:   %12s\n", fnstr);
            printf("  Memseg:         0x%02X%02X%02X\n", in->sample.file_offs_sample[0], in->sample.file_offs_sample[1], in->sample.file_offs_sample[2]);
            printf("  Length:            %5d\n", in->sample.length);
            printf("  Loop Begin:        %5d\n", in->sample.loop_begin);
            printf("  Loop End:          %5d\n", in->sample.loop_end);
            printf("  Volume:              %3d\n", in->sample.volume);
            printf("  Pack:               0x%02X\n", in->sample.pack);
            printf("  Flags:              0x%02X\n", in->sample.flags);
            printf("  C2 Speed:          %5d\n", in->sample.c4_speed);
            printf("  Name: %s\n", in->sample.name);
            printf("  Ident:             %c%c%c%c\n", in->sample.ident[0], in->sample.ident[1], in->sample.ident[2], in->sample.ident[3]);
            if (i<h->num_instruments-1) printf("\n");
        } else {
            memcpy(fnstr, in->adlib.filename, 12);
            fnstr[12] = '\0';
            printf("  Filename:   %s\n", fnstr);
            printf("  Volume:       %3d\n", in->adlib.volume);
            printf("  C2 Speed:     %5d\n", in->adlib.c4_speed);
            printf("  Name:        %28s\n", in->adlib.name);
            printf("  Ident:       %c%c%c%c\n", in->adlib.ident[0], in->adlib.ident[1], in->adlib.ident[2], in->adlib.ident[3]);
        }
        
    }
}

void s3m_print_entry(pat_entry_t* entry) 
{
    uint8_t octave, note;
    
    if (entry->note == 255) {
        printf("--- ");
    } else {
        octave = entry->note >> 4;
        note = entry->note & 0x0F;
        printf("%s%d ", g_note_str[note], octave);
    }
    if (entry->instr == 255) {
        printf("-- ");
    } else {
        printf("%02d ", entry->instr);
    }
    
    if (entry->vol == 255) {
        printf("-- ");
    } else {
        printf("%02d ", entry->vol);
    }
    
    if (entry->cmd == 255) {
        printf("---");
    } else {
        if (entry->cmd < 26) {
            printf("%c%02X", entry->cmd + 64, entry->info);
        } else {
            printf("?%02X", entry->info);
        }
    }       
}

void s3m_print_patterns(s3m_t* s3m)
{
    int             p, c, r;  
    s3m_header_t*   h;
    uint8_t*        pd;
    pat_row_t       row;
    uint16_t        len;
    pat_entry_t     empty_entry;
    h = s3m->header;
    
    empty_entry.chn = 255;
    empty_entry.note = 255;
    empty_entry.instr = 255;
    empty_entry.vol = 255;
    empty_entry.cmd = 255;
    empty_entry.info = 0;
    
    printf("Patterns:\n");
    printf("-----------------------------------------------------------\n");
    for (p=0; p<h->num_patterns; p++) {
        printf("Pattern %d\n", p);
        pd = s3m->pattern[p];
        len = *pd++ | *pd++<<8;
        printf("  Length in byte %d\n", len);
        r = 0;
        while (pd - s3m->pattern[p] < (len+2) && r<64) {
            pat_read_row(&pd, &row);
            printf("  %03d  ", r);
            for (c=0; c<S3M_MAX_CHANNELS; c++) {
                if (h->channel_settings[c] >= 128) continue;             
                s3m_print_entry(&row.entry_chn[c]);
                printf("  ");
            }
            printf("\n");
            r++;
        }
        while (r<64) {
            printf("  %03d  ", r);
            for (c=0; c<S3M_MAX_CHANNELS; c++) {
                if (h->channel_settings[c] >= 128) continue;             
                s3m_print_entry(&empty_entry);
                printf("  ");
            }
            printf("\n");
            r++;
        }
        
        if(p<h->num_patterns-1) printf("\n");
    }
}

/* EOF: s3m_info.c */
