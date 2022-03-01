/*
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

    Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
    Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
    The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef PARSEMIDI_H
#define PARSEMIDI_H

#include <stddef.h>

#include "ints.h"

/* reading examples for each defined meta type in parsemidi.c:print_event_debug*/
#define META_SEQUENCENUMBER 0x00
#define META_TEXT 0x01
#define META_COPYRIGHT 0x02
#define META_SEQUENCENAME 0x03
#define META_INSTRUMENTNAME 0x04
#define META_LYRIC 0x05
#define META_MARKER 0x06
#define META_CUE 0x07
#define META_ENDTRACK 0x2f // 47 dec
#define META_TEMPO 0x51 // 81 dec
#define META_SMPTE 0x54  // 84 dec
#define META_TIMESIG 0x58 // 88 dec
#define META_KEYSIG 0x59 // 89 dec
#define META_SEQEV 0x7f  // 127 dec


/* controller events*/
/* CONTROLLER_xx (xx in hex) means undefined event*/

/* 0-31 are MSB, with LSB in 32-63*/
/* values for both MSB and LSB generally cover 0-127*/
#define CONTROLLER_BANKSELECT 0 // other banks (not 0) and what they contain vary by synth.  general midi synthing might ignore this and just stick with 0
#define CONTROLLER_MODWHEEL 1 // joystick on some synths
#define CONTROLLER_BREATHCONTROLLER 2
#define CONTROLLER_03 3 // sometimes "continuous controller 3"?
#define CONTROLLER_FOOTPEDAL 4
#define CONTROLLER_PORTAMENTOTIME 5
#define CONTROLLER_DATAENTRY 6 // data entry for RPN, NRPN
#define CONTROLLER_CHANNELVOLUME 7 // 'main volume'
#define CONTROLLER_BALANCE 8 // what is this?
#define CONTROLLER_09 9
#define CONTROLLER_PAN 10
#define CONTROLLER_EXPRESSION 11 // loudness on some synths
#define CONTROLLER_EFF1 12
#define CONTROLLER_EFF2 13
#define CONTROLLER_0E 14
#define CONTROLLER_0F 15
#define CONTROLLER_GP1 16 // general purpose slider controls
#define CONTROLLER_GP2 17 // or sometimes knobs
#define CONTROLLER_GP3 18 //
#define CONTROLLER_GP4 19 //
#define CONTROLLER_14 20
#define CONTROLLER_15 21
#define CONTROLLER_16 22
#define CONTROLLER_17 23
#define CONTROLLER_18 24
#define CONTROLLER_19 25
#define CONTROLLER_1A 26
#define CONTROLLER_1B 27
#define CONTROLLER_1C 28
#define CONTROLLER_1D 29
#define CONTROLLER_1E 30
#define CONTROLLER_1F 31
/* next 32 are LSB of the above*/
#define CONTROLLER_LSB_OFFSET 32 /* many hardware controls don't set LSB*/
                                 /* (it's very fine for a human control)*/

/* binary pedal controls.  0-63 = off, 64-127 = on*/
#define CONTROLLER_SUSTAIN 64 // "hold1", aka damper pedal
#define CONTROLLER_PORTAMENTO 65
#define CONTROLLER_SOSTENUTO 66
#define CONTROLLER_SOFTPEDAL 67
#define CONTROLLER_LEGATO 68
#define CONTROLLER_HOLD2 69

/* 0-127 values*/
#define CONTROLLER_SC1 70 // default "variation"
#define CONTROLLER_SC2 71 // default "timbre" or "intensity", possibly "low pass resonance"
#define CONTROLLER_SC3 72 // default "release time"
#define CONTROLLER_SC4 73 // default "attack time"
#define CONTROLLER_SC5 74 // default "brightness", possibly "low pass cutoff"
#define CONTROLLER_SC6 75 // default "decay time"
#define CONTROLLER_SC7 76 // default "vibrato rate"
#define CONTROLLER_SC8 77 // default "vibrato depth"
#define CONTROLLER_SC9 78 // default "vibrato decay"
#define CONTROLLER_SC10 79 // default undefined

#define CONTROLLER_GP5 80 // "general purpose buttons" - sometimes decay
#define CONTROLLER_GP6 81 // - sometimes hipass frequency
#define CONTROLLER_GP7 82 // -
#define CONTROLLER_GP8 83 // -
#define CONTROLLER_PORTAMENTOCTRL 84 // portamento intensity
#define CONTROLLER_55 85
#define CONTROLLER_56 86
#define CONTROLLER_57 87
#define CONTROLLER_HRVELOCITY 88
#define CONTROLLER_59 89
#define CONTROLLER_5A 80
#define CONTROLLER_E1D 91 // reverb level, often implemented
#define CONTROLLER_E2D 92 // tremolo level, often implemented
#define CONTROLLER_E3D 93 // chorus level , often implemented
#define CONTROLLER_E4D 94 // celeste level (detune)
#define CONTROLLER_E5D 95 // phaser level
#define CONTROLLER_DATA_INCREMENT 96 // +1 to current NRPN/RPN
#define CONTROLLER_DATA_DECREMENT 97 // -1 to current NRPN/RPN
#define CONTROLLER_NRPN_LSB 98 // set NRPN parameter to change
#define CONTROLLER_NRPN_MSB 99 // set NRPN parameter to change
#define CONTROLLER_RPN_LSB 100 // set RPN parameter to change
#define CONTROLLER_RPN_MSB 101 // set RPN parameter to change
#define CONTROLLER_66 102
#define CONTROLLER_67 103
#define CONTROLLER_68 104
#define CONTROLLER_69 105
#define CONTROLLER_6A 106
#define CONTROLLER_6B 107
#define CONTROLLER_6C 108
#define CONTROLLER_6D 109
#define CONTROLLER_6E 110
#define CONTROLLER_6F 111
#define CONTROLLER_70 112
#define CONTROLLER_71 113
#define CONTROLLER_72 114
#define CONTROLLER_73 115
#define CONTROLLER_74 116
#define CONTROLLER_75 117
#define CONTROLLER_76 118
#define CONTROLLER_77 119

/* following parameters adjust operating mode, not sound */
#define CONTROLLER_ALL_SOUND_OFF 120 /* cut output completely; param = 0. */
#define CONTROLLER_RESET_ALL 121 /* all controller values set to defaults; param = 0. */
#define CONTROLLER_LOCALCONTROL 122 /* 0 = off, 127 = on */
#define CONTROLLER_ALL_NOTES_OFF 123 /* notes should still decay normally. param = 0. */
#define CONTROLLER_OMNI_OFF 124 /* param = 0; triggers all notes off. for a receiving synth, omni off receives on only one channel */
#define CONTROLLER_OMNI_ON 125 /* param = 0; triggers all notes off.  for a receiving synth, omni on receives all channels */
#define CONTROLLER_MONO_ON 126 /* triggers all notes off and poly off. param n = number of channels, or 0 = max possible */
#define CONTROLLER_POLY_ON 127 /* triggers all notes off and mono off */

typedef enum
{
  event_noteoff,
  event_noteon,
  event_noteaftertouch,
  event_controller,
  event_programchange,
  event_channelaftertouch,
  event_pitchbend,
  event_meta,
  event_sysex,
  event_sysex_div
} event_type_t;

typedef struct
{
  unsigned delta;
  event_type_t type;
  int channel;
  int param1;
  int param2;
  unsigned len;
  u8 *data;
} event_t;

typedef struct
{
  unsigned numevents;
  event_t *events;
} track_t;

typedef struct
{
  unsigned numtracks;
  track_t *tracks;
  int mode; /* 0 = single, 1 = alongside, 2 = concatenate*/
  int smpte_fps; /* 24, 25, 29, 30, or 0*/
  int timing; /* deltas per frame, or deltas per 'quarter note' if smpte_fps = 0*/
} midi_t;

midi_t *parsemidi (const void *data, size_t len);
void freemidi (midi_t *m);

void print_midi_debug (const midi_t *m);

#endif

