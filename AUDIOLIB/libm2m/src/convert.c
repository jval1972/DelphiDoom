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




#include "ints.h"
#include "parsemidi.h"
#include "smalloc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>



static double compute_spmc_normal (unsigned mpq, unsigned tempo, unsigned sndrate)
{ /* returns samples per midi clock

     inputs: mpq (midi clocks per quarternote, from header)
     tempo (from tempo event, in microseconds per quarternote)
     sndrate (sound sample rate in hz)

     samples   quarternote     microsec    samples    second
     ------- = ----------- * ----------- * ------- * --------
     midiclk     midiclk     quarternote   second    microsec

     return  =  (1 / mpq)  *    tempo    * sndrate * (1 / 1000000) */

  return (double) tempo / 1000000 * sndrate / mpq;
}

static double compute_spmc_smpte (unsigned smpte_fps, unsigned mpf, unsigned sndrate)
{ /* returns samples per midi clock

     inputs: smpte_fps (24, 25, 29, 30)
     mpf (midi clocks per frame, 0-255)
     sndrate (sound sample rate in hz)

     tempo is ignored here

     samples     frame      seconds    samples
     ------- = --------- * --------- * -------
     midiclk    midiclk      frame     second

     return  = (1 / mpf) * (1 / fps) * sndrate */


  double fps; /* actual frames per second */
  switch (smpte_fps)
  {
    case 24:
    case 25:
    case 30:
      fps = smpte_fps;
      break;
    case 29:
      /* i hate NTSC, i really do */
      fps = smpte_fps * 1000.0 / 1001.0;
      break;
    default:
      fprintf (stderr, "Unexpected SMPTE timestamp %i\n", smpte_fps);
      /* assume*/
      fps = 30.0;
      break;
  }

  return (double) sndrate / fps / mpf;
}



typedef struct
{
  u8 *data;
  size_t pos;
  size_t lastevent; /* position of last event code in stream */
  size_t lastdelta; /* position of last delta in stream */
  size_t runningdelta;
} mustrack_t;

static int writebytes (mustrack_t *trk, u8 *data, size_t len)
{
  if (len + trk->pos >= 65536)
  {
    fprintf (stderr, "writebytes: mus track too big!\n");
    return 0;
  }
  memcpy (trk->data + trk->pos, data, len);
  trk->pos += len;
  return 1;
}
  

static int writeevent (mustrack_t *trk, u8 *data, size_t len)
{
  trk->lastevent = trk->pos;
  trk->runningdelta = 0;
  trk->lastdelta = (size_t) -1;

  return writebytes (trk, data, len);
}

static int writedelta (mustrack_t *trk, u32 delta)
{
  /* the only trick here is that some midi events won't be written
     to the mus file (most meta events, most controller events).
     if we come across one of those, which has non-zero time deltas
     both before and after it, then we have to merge those two deltas
     together (since the event between them was zapped) */

  int i;
  u8 scratch[6];

  if (!delta)
  {
    fprintf (stderr, "writedelta: tried to write 0 delta\n");
    return 0;
  }

  if (trk->lastdelta < trk->pos)
  { /* up our delta*/
    delta += trk->runningdelta;
    trk->runningdelta = delta;
    trk->pos = trk->lastdelta; /* rewind*/
  }
  else
  {
    trk->lastdelta = trk->pos;
    if (trk->lastevent < trk->pos)
      trk->data[trk->lastevent] |= 0x80;
    else
    {
      fprintf (stderr, "huh? (track must start with 0time event?\n");
      return 0;
    }
    trk->lastevent = (unsigned) -1;
  }


  for (i = 5; i >= 4; i--)
  {
    if (!delta)
      break;
    scratch[i] = delta & 0x7f;
    scratch[i] |= 0x80;
    delta >>= 7;
  }
  /* zap carry bit from last value*/
  scratch[5] &= 0x7f;

  return writebytes (trk, scratch + i + 1, 5 - i);
}

static int patchlist_cmp (const void *s1, const void *s2)
{
  const u16 *i1 = (u16 *)s1;
  const u16 *i2 = (u16 *)s2;
  return (int) *i1 - (int) *i2;
}

static void patchlist_sort (u16 *patchlist)
{
  myqsort (patchlist + 1, patchlist[0], sizeof (u16), patchlist_cmp);
}

static u16 *patchlist_add (u16 *patchlist, u16 val)
{
  int i;
  for (i = 1; i <= patchlist[0]; i++)
  {
    if (val == patchlist[i])
      return patchlist;
  }
  patchlist = (u16 *)realloc (patchlist, sizeof (u16) * (patchlist[0] + 2));
  patchlist[0]++;
  patchlist[patchlist[0]] = val;
  return patchlist;
}

static u16 *patchlist_create (void)
{
  u16 *ret = smalloc (sizeof (u16));
  *ret = 0;
  return ret;
}

int global_pos;
char *global_buff;

static void writeshort (u16 val)
{
  global_buff[global_pos++] = val & 0xff;
  global_buff[global_pos++] = val >> 8;
}

static void writechar (char val)
{
  global_buff[global_pos++] = val;
}

/* in midi should already be mode 0*/
void convert_midi (midi_t *m, char *outbuff, int *outsize)
{
  mustrack_t trk;
  double spmc;

  int velocitycache[16] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

  int channelmaps[16] = {-1, -1, -1, -1, -1, -1, -1, -1, -1, 15, -1, -1, -1, -1, -1, -1};
  int chnext = 0;

  int foundend = 0;

  int unusechannel = -1;

  u16 scratch;
  int i;


  u16 *patchlist = NULL;

  double error; /* difference between musclock and midiclock clocks, in musclocks */
  unsigned eventnumber; /* number of NEXT event to convert */
  track_t *currtrack;

  global_pos = 0;
  global_buff = outbuff;

  switch (m->mode)
  {
    case 0:
      break;
    case 1:
    case 2:
      fprintf (stderr, "convert_midi: can't render mode %i\n", m->mode);
      return;
    default:
      fprintf (stderr, "convert_midi: bad midi mode %i\n", m->mode);
      return;
  }

  patchlist = patchlist_create ();

  trk.data = smalloc (65536);
  trk.pos = 0;
  trk.lastevent = (size_t) -1;
  trk.lastdelta = (size_t) -1;
  trk.runningdelta = 0;

  /* compute initial tempo */
  if (m->smpte_fps)
    spmc = compute_spmc_smpte (m->smpte_fps, m->timing, 140);
  else
    spmc = compute_spmc_normal (m->timing, 500000, 140); /* assumed bpm of 120 */

  error = 0.0;
  eventnumber = 0;

  currtrack = m->tracks + 0;


  while (!foundend && eventnumber < currtrack->numevents)
  {

    event_t *currevent = currtrack->events + eventnumber;

    /* how many musclocks away next event is */
    double eventdelta = currevent->delta * spmc;

    /* how many musclocks we're actually going to advance (round down) */

    u32 musclocks = (u32) (eventdelta + error);

    if (musclocks)
    {
      if (!writedelta (&trk, musclocks))
        goto fail;
      error -= musclocks;
    }
    /* process event */
    if (1)
    {
      int me_len;
      int outchannel;

      u8 me_data[4];

      me_len = 0;

      /* determine channel remap */
      if (channelmaps[currevent->channel] == -1)
      { /* find lowest usable channel */
        channelmaps[currevent->channel] = chnext;
        unusechannel = chnext;
        chnext++;
      }
      else
        unusechannel = -1;
      outchannel = channelmaps[currevent->channel];

      switch (currevent->type)
      {
        case event_noteon:
          if (currevent->param2 != 0) /* velocity of 0 is processed as noteoff */
          {
            if (currevent->channel == 9) // percussion event: add samples
              patchlist = patchlist_add (patchlist, currevent->param1 + 100); // is + 100 right?

            me_data[0] = outchannel | 0x10;
            if (currevent->param2 == velocitycache[outchannel])
            { // use cached velocity
              me_data[1] = currevent->param1;
              me_len = 2;
            }
            else
            {
              me_data[1] = currevent->param1 | 0x80; // key
              me_data[2] = velocitycache[outchannel] = currevent->param2; // velocity
              me_len = 3;
            }
            break;
          }
          else
            ; // do nothing and fall through
        case event_noteoff:
          if (currevent->param2 != 0) // non-zero velocities are lost in conversion
            fprintf (stderr, "non-zero noteoff velocity lost\n");
          me_data[0] = outchannel | 0x00;
          me_data[1] = currevent->param1;
          me_len = 2;
          break;
        case event_noteaftertouch:
          // can we possibly do something by doing noteoff/noteon with new velocity?
          fprintf (stderr, "note aftertouch event lost\n");
          break;
        case event_controller:
          switch (currevent->param1)
          {
            case CONTROLLER_BANKSELECT + CONTROLLER_LSB_OFFSET:
              fprintf (stderr, "bank select messages are not well understood\n");
              me_data[0] = outchannel | 0x40;
              me_data[1] = 1;
              me_data[2] = currevent->param2;
              me_len = 3;
              break;
            case CONTROLLER_MODWHEEL:
              me_data[0] = outchannel | 0x40;
              me_data[1] = 2;
              me_data[2] = currevent->param2;
              me_len = 3;
              break;
            case CONTROLLER_CHANNELVOLUME:
              me_data[0] = outchannel | 0x40;
              me_data[1] = 3;
              me_data[2] = currevent->param2;
              me_len = 3;
              break;
            case CONTROLLER_PAN:
              me_data[0] = outchannel | 0x40;
              me_data[1] = 4;
              me_data[2] = currevent->param2;
              me_len = 3;
              break;
            case CONTROLLER_EXPRESSION:
              me_data[0] = outchannel | 0x40;
              me_data[1] = 5;
              me_data[2] = currevent->param2;
              me_len = 3;
              break;
            case CONTROLLER_E1D:
              me_data[0] = outchannel | 0x40;
              me_data[1] = 6;
              me_data[2] = currevent->param2;
              me_len = 3;
              break;
            case CONTROLLER_E3D:
              me_data[0] = outchannel | 0x40;
              me_data[1] = 7;
              me_data[2] = currevent->param2;
              me_len = 3;
              break;
            case CONTROLLER_SUSTAIN:
              me_data[0] = outchannel | 0x40;
              me_data[1] = 8;
              me_data[2] = currevent->param2;
              me_len = 3;
              break;
            case CONTROLLER_SOFTPEDAL:
              me_data[0] = outchannel | 0x40;
              me_data[1] = 9;
              me_data[2] = currevent->param2;
              me_len = 3;
              break;
            case CONTROLLER_ALL_SOUND_OFF:
              me_data[0] = outchannel | 0x30;
              me_data[1] = 10;
              me_len = 2;
              break;
            case CONTROLLER_ALL_NOTES_OFF:
              me_data[0] = outchannel | 0x30;
              me_data[1] = 11;
              me_len = 2;
              break;
            case CONTROLLER_MONO_ON:
              me_data[0] = outchannel | 0x30;
              me_data[1] = 12;
              me_len = 2;
              break;
            case CONTROLLER_POLY_ON:
              me_data[0] = outchannel | 0x30;
              me_data[1] = 13;
              me_len = 2;
              break;
            case CONTROLLER_RESET_ALL:
              me_data[0] = outchannel | 0x30;
              me_data[1] = 14;
              me_len = 2;
              break;
            default:
              fprintf (stderr, "don't know how to translate controller event %i\n", currevent->param1);
              break;
          }
          break;
        case event_programchange:
          me_data[0] = outchannel | 0x40;
          me_data[1] = 0;
          me_data[2] = currevent->param1;
          me_len = 3;
          if (outchannel == 15)
            fprintf (stderr, "Unexpected program change on percussion channel\n");
          else
            patchlist = patchlist_add (patchlist, currevent->param1);
          break;
        case event_channelaftertouch:
          fprintf (stderr, "channel aftertouch event lost\n");
          break;
        case event_pitchbend:
          me_data[0] = outchannel | 0x20;
          me_data[1] = (currevent->param1 | currevent-> param2 << 7) >> 6;
          me_len = 2;
          break;
        case event_sysex:
        case event_sysex_div:
          fprintf (stderr, "sysex event lost\n");
          break;
        case event_meta:
          switch (currevent->param1)
          {
            case META_TEMPO:
              if (m->smpte_fps)
              {
                fprintf (stderr, "ignoring meta event tempo on SMPTE midi file\n");
                break;
              }
              if (currevent->len != 3)
              {
                fprintf (stderr, "abnormal tempo event of length %u (not 3)\n", currevent->len);
                break;
              }
              // recompute spmc
              spmc = compute_spmc_normal
              (
                m->timing,
                (u32) currevent->data[0] << 16 |
                (u32) currevent->data[1] << 8 |
                (u32) currevent->data[2],
                140
              );
              break;
            case META_ENDTRACK:
              me_data[0] = 0x60;
              me_len = 1;
              foundend = 1;
              break;
            case META_SEQEV:
              fprintf (stderr, "meta sequencer event not understood\n");
              break;            
            default:
              fprintf (stderr, "unneeded meta event stripped\n");
              break;
          }
          break;
        default:
          fprintf (stderr, "unknown event in midi file? (should not happen)\n");
          break;
      }
      if (me_len)
      {
        if (!writeevent (&trk, me_data, me_len))
          goto fail;
      }
      else if (unusechannel != -1 && currevent->channel != 9)
      { // take back channel allocation, but not for fixed perc channel
        unusechannel = -1;
        channelmaps[currevent->channel] = -1;
        chnext--;
      }
      error += eventdelta;
      eventnumber++;
    }
  }

  if (!foundend)
  { // ran out of events
    fprintf (stderr, "ran out of events without end of track\n");
    goto fail;
  }

  patchlist_sort (patchlist);


  // we now have perfectly formed track  trk.data, trk.pos = len

  writechar ('M');
  writechar ('U');
  writechar ('S');
  writechar (0x1a);

  writeshort (trk.pos); // length of track in bytes
  writeshort (patchlist[0] * 2 + 16); // offset to data

  for (scratch = 0, i = 0; i < 15; i++)
  { // my interpretation of primary channels is any channel that
    // sounded a note at any point, not including drum channels
    // i don't know if this is right
    if (velocitycache[i])
      scratch++;
  }
  writeshort (scratch);

  writeshort (0); // no secondary channels?

  writeshort (patchlist[0]); // number of patches
  writeshort (0); // unused?

  for (i = 1; i <= patchlist[0]; i++)
  {
    writeshort (patchlist[i]);
  }

  free (patchlist);

  for (i = 0; i < trk.pos; i++)
    writechar (trk.data[i]);

  *outsize = global_pos;

  free (trk.data);
  return;

  fail:

  free (patchlist);
  free (trk.data);
  return;
}

