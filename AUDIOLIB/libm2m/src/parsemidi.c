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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "ints.h"

#include "parsemidi.h"
#include "smalloc.h"

typedef struct
{
  const u8 *data;
  size_t len;
  size_t pos;
} memfile_t;




static void freeevent (event_t *e)
{
  if (e->len)
    free (e->data);
  return;
}

static void freetrack (track_t *t)
{
  unsigned i;
  for (i = 0; i < t->numevents; i++)
    freeevent (t->events + i);
  free (t->events);
  return;
}

void freemidi (midi_t *m)
{
  unsigned i;
  for (i = 0; i < m->numtracks; i++)
    freetrack (m->tracks + i);
  free (m->tracks);
  free (m);
}



static int readchar (u8 *dst, memfile_t *mf)
{
  if (mf->pos >= mf->len)
  {
    fprintf (stderr, "parsemidi: unexpected end of stream\n");
    return 0;
  }
  *dst = mf->data[mf->pos++];
  return 1;
}

static int readchars (u8 *dst, unsigned count, memfile_t *mf)
{
  unsigned i;
  for (i = 0; i < count; i++)
  {
    if (!readchar (dst + i, mf))
      return 0;
  }
  return 1;
}

static int readvarlen (u32 *dst, memfile_t *mf, unsigned *bytes)
{
  int i;
  u8 c;
  *dst = 0;

  for (i = 0; i < 4; i++)
  {
    if (!readchar (&c, mf))
      return 0;
    ++*bytes;
    *dst <<= 7;
    *dst += c & 0x7f;
    if (!(c & 0x80))
      return 1;
  }
  fprintf (stderr, "parsemidi: broken variable length");
  return 0;
}

static int readu16 (u16 *dst, memfile_t *mf)
{
  int i;
  u8 c;
  *dst = 0;

  for (i = 0; i < 2; i++)
  {
    if (!readchar (&c, mf))
      return 0;
    *dst <<= 8;
    *dst += c;
  }
  return 1;
}

static int readu32 (u32 *dst, memfile_t *mf)
{
  int i;
  u8 c;
  *dst = 0;

  for (i = 0; i < 4; i++)
  {
    if (!readchar (&c, mf))
      return 0;
    *dst <<= 8;
    *dst += c;
  }
  return 1;
}

static int ungetchar (memfile_t *mf)
{
  if (mf->pos)
  {
    mf->pos--;
    return 1;
  }
  fprintf (stderr, "parsemidi: unexpected rewind\n");
  return 0;
}





static int parseevent (event_t *event, memfile_t *mf, unsigned *bytes, event_type_t prevev, int prevch)
{
  u32 sz;
  u8 c;

  memset (event, 0, sizeof (event_t));

  if (!readvarlen (&sz, mf, bytes))
    return 0;
  event->delta = sz;

  if (!readchar (&c, mf))
    return 0;
  ++*bytes;
  if (!(c & 0x80))
  {
    if (!ungetchar (mf))
      return 0;
    event->type = prevev;
    event->channel = prevch;
    --*bytes;
  }
  else
  {
    event->channel = c & 0xf;
    switch (c & 0xf0)
    {
      case 0x80:
        event->type = event_noteoff;
        break;
      case 0x90:
        event->type = event_noteon;
        break;
      case 0xa0:
        event->type = event_noteaftertouch;
        break;
      case 0xb0:
        event->type = event_controller;
        break;
      case 0xc0:
        event->type = event_programchange;
        break;
      case 0xd0:
        event->type = event_channelaftertouch;
        break;
      case 0xe0:
        event->type = event_pitchbend;
        break;
      case 0xf0:
        if (c == 0xff)
        {
          event->type = event_meta;
          break;
        }
        if (c == 0xf0)
        {
          event->type = event_sysex;
          break;
        }
        if (c == 0xf7)
        {
          event->type = event_sysex_div;
          break;
        }
        // FALL THROUGH
      default:
        fprintf (stderr, "parsemidi: unknown event %02x\n", c);
        return 0;
    }
  }
  switch (event->type)
  {
    case event_noteoff:
    case event_noteon:
    case event_noteaftertouch:
    case event_controller:
    case event_pitchbend:
      // 2 params
      if (!readchar (&c, mf))
        return 0;
      ++*bytes;
      event->param1 = c & 0x7f;
      if (!readchar (&c, mf))
        return 0;
      ++*bytes;
      event->param2 = c & 0x7f;
      return 1;

    case event_programchange:
    case event_channelaftertouch:
      // 1 param
      if (!readchar (&c, mf))
        return 0;
      ++*bytes;
      event->param1 = c & 0x7f;
      return 1;

    case event_meta:
      // meta type
      if (!readchar (&c, mf))
        return 0;
      ++*bytes;
      event->param1 = c /*& 0x7f*/; // can meta events be >127?

      // FALL THROUGH
    case event_sysex:
    case event_sysex_div:
      // variable length and variable data
      if (!readvarlen (&sz, mf, bytes))
        return 0;
      event->len = sz;
      if (!event->len)
        // length 0 is legal, at least for meta
        return 1;
      event->data = smalloc (event->len);
      if (!readchars (event->data, event->len, mf))
      {
        free (event->data);
        return 0;
      }
      *bytes += event->len;
      return 1;
    default:
      fprintf (stderr, "parsemidi: big problem\n");
      return 0;
  }
}

static int parsetrack (track_t *track, memfile_t *mf)
{
  u8 scratch[4];

  u32 sz;

  event_type_t lastev = 0xdeadbeef; // initialized to invalid
  int lastch = 0xdeadbeef;

  unsigned readbytes = 0;
  unsigned totalbytes = 0;
  unsigned numeventspace = 0;

  int i;

  track->numevents = 0;

  if (!readchars (scratch, 4, mf))
    return 0;
  if (strncmp (scratch, "MTrk", 4))
  {
    fprintf (stderr, "parsemidi: missing track header\n");
    return 0;
  }
  if (!readu32 (&sz, mf))
    return 0;
  totalbytes = sz;

  while (readbytes < totalbytes)
  {
    // new event.  increase size if nessecary
    if (track->numevents == numeventspace)
    { // constant 100 is flexible and has no specific meaning
      #define EVENTS_REALLOC_SCALING 100
      track->events = srealloc (track->events, sizeof (event_t) * (track->numevents +
        EVENTS_REALLOC_SCALING));
      numeventspace += EVENTS_REALLOC_SCALING;
    }
    if (!parseevent (track->events + track->numevents, mf, &readbytes, lastev, lastch))
      break;

    lastev = track->events[track->numevents].type;
    lastch = track->events[track->numevents].channel;

    track->numevents++;

    if (track->events[track->numevents - 1].type == event_meta &&
        track->events[track->numevents - 1].param1 == META_ENDTRACK)
    {
      if (readbytes < totalbytes)
        fprintf (stderr, "parsemidi: excess data at end of track (non-fatal?) %u %u\n", readbytes, totalbytes);
      if (readbytes > totalbytes)
        fprintf (stderr, "parsemidi: track overrun (non-fatal?)%u %u\n", readbytes, totalbytes);
      // trim excess event_t space
      track->events = srealloc (track->events, sizeof (event_t) * track->numevents);
      return 1;
    }
  }
  if (readbytes >= totalbytes)
    fprintf (stderr, "parsemidi: track overrun (fatal)%u %u\n", readbytes, totalbytes);
  // otherwise, it was a parseevent failure

  for (i = 0; i < track->numevents; i++)
    freeevent (track->events + i);
  free (track->events);

  return 0;
}

midi_t *parsemidi (const void *data, size_t len)
{
  memfile_t mf;
  u8 scratch[4];
  int i, j;
  u16 s;
  u32 sz;

  midi_t *midi;


  mf.data = (const u8 *) data;
  mf.len = len;
  mf.pos = 0;

  // read header
  if (!readchars (scratch, 4, &mf))
    return NULL;
  if (strncmp (scratch, "MThd", 4))
  {
    fprintf (stderr, "parsemidi: missing header\n");
    return NULL;
  }
  if (!readu32 (&sz, &mf))
    return NULL;
  if (sz != 6)
  {
    fprintf (stderr, "parsemidi: abnormal header\n");
    return NULL;
  }

  // read mode
  if (!readu16 (&s, &mf))
    return NULL;
  if (s > 2)
  {
    fprintf (stderr, "parsemidi: unknown file mode\n");
    return NULL;
  }
  midi = scalloc (sizeof (midi_t));
  midi->mode = s;

  // read number of tracks
  if (!readu16 (&s, &mf))
  {
    free (midi);
    return NULL;
  }
  if (!s)
  {
    fprintf (stderr, "parsemidi: no tracks!\n");
    free (midi);
    return NULL;
  }
  midi->numtracks = s;
  midi->tracks = scalloc (midi->numtracks * sizeof (track_t));

  // read timecode base
  if (!readu16 (&s, &mf))
  {
    free (midi->tracks);
    free (midi);
    return NULL;
  }
  if (s & 0x8000) // SMPTE
  {
    midi->smpte_fps = -(s8) (s >> 8); // should be 24, 25, 29, or 30
    midi->timing = s & 0xff;
  }
  else
  {
    midi->smpte_fps = 0;
    midi->timing = s;
  }

  // read tracks
  for (i = 0; i < midi->numtracks; i++)
  {
    if (!parsetrack (midi->tracks + i, &mf))
    {
      for (j = 0; j < i; j++)
        freetrack (midi->tracks + i);
      free (midi->tracks);
      free (midi);
      return NULL;
    }
  }
  return midi;
}





static void print_text_debug (const event_t *e)
{
  char *str;
  if (e->len == 0)
  {
    printf ("<<LENGTH 0>>\n");
    return;
  }
  str = scalloc (e->len + 1);
  memcpy (str, e->data, e->len);
  printf ("%s\n", str);
  return;
}

static void print_event_debug (const event_t *e)
{
  printf ("    delta %u\n", e->delta);
  switch (e->type)
  {
    case event_noteoff:
      printf ("    [%02i]note off: key %i velo %i\n", e->channel, e->param1, e->param2);
      return;
    case event_noteon:
      printf ("    [%02i]note on: key %i velo %i\n", e->channel, e->param1, e->param2);
      return;
    case event_noteaftertouch:
      printf ("    [%02i]note aftertouch: key %i amnt %i\n", e->channel, e->param1, e->param2);
      return;
    case event_controller:
      printf ("    [%02i]controller: type %i val %i\n", e->channel, e->param1, e->param2);
      return;
    case event_programchange:
      printf ("    [%02i]program change: %i\n", e->channel, e->param1);
      return;
    case event_channelaftertouch:
      printf ("    [%02i]channel aftertouch: amnt %i\n", e->channel, e->param1);
      return;
    case event_pitchbend:
      printf ("    [%02i]channel pitchbend: %04xh\n", e->channel, e->param2 << 7 | e->param1);
      return;
    case event_sysex:
    case event_sysex_div:
      printf ("    [EX]sysex %u bytes\n", e->len);
      return;
    case event_meta:
      printf ("    [MT]meta: ");
      switch (e->param1)
      {
        case META_SEQUENCENUMBER:
          if (e->len != 2)
            printf ("sequence number, but wrong length\n");
          else
            printf ("sequence number %u\n", (u16) e->data[0] << 8 | e->data[1]);
          return;
        case META_TEXT:
          printf ("text event: ");
          print_text_debug (e);
          return;
        case META_COPYRIGHT:
          printf ("copyright notice: ");
          print_text_debug (e);
          return;
        case META_SEQUENCENAME:
          printf ("sequence name: ");
          print_text_debug (e);
          return;
        case META_INSTRUMENTNAME:
          printf ("instrument name: ");
          print_text_debug (e);
          return;
        case META_LYRIC:
          printf ("lyric: ");
          print_text_debug (e);
          return;
        case META_MARKER:
          printf ("text marker: ");
          print_text_debug (e);
          return;
        case META_CUE:
          printf ("cue: ");
          print_text_debug (e);
          return;
        case META_ENDTRACK:
          printf ("END OF TRACK\n");
          return;
        case META_TEMPO:
          if (e->len != 3)
            printf ("tempo change, but wrong length\n");
          else
            printf ("tempo change: %lu MPQN\n", (u32) e->data[0] << 16 | e->data[1] << 8 | e->data[2]);
          return;
        case META_SMPTE:
          if (e->len != 5)
            printf ("SMPTE stamp, but wrong length\n");
          else
            printf ("SMPTE stamp: %2u %2u %2u %2u %2u (hh mm ss fr sb)\n",
              e->data[0], e->data[1], e->data[2], e->data[3], e->data[4]);
          return;
        case META_TIMESIG:
          if (e->len != 4)
            printf ("time signature event, but wrong length\n");
          else
            printf ("time signature: %2u %2u %2u %2u (nn dd mt 32)\n",
              e->data[0], e->data[1], e->data[2], e->data[3]);
          return;
        case META_KEYSIG:
          if (e->len != 2)
            printf ("key signature, but wrong length\n");
          else
            printf ("key signature: %i %s (negative flats, positive sharps)\n",
              (s8) e->data[0], e->data[1] ? "minor" : "major");
          return;
        case META_SEQEV:
          if (e->len == 0)
            printf ("sequencer event, but empty\n");
          else
            printf ("sequencer event: %u bytes, %02xh id\n", e->len, e->data[0]);
          return;
        default:
          printf ("unknown meta event %02xh\n", e->param1);
          return;
      }
    default:
      printf ("    ERROR: STRAY EVENT (should not happen even with mangled input)\n");
      return;
  }
}

static void print_track_debug (const track_t *t)
{
  unsigned i;
  printf ("  numevents: %u\n", t->numevents);
  for (i = 0; i < t->numevents; i++)
  {
    printf ("  event %u\n", i);
    print_event_debug (t->events + i);
  }
}

void print_midi_debug (const midi_t *m)
{
  unsigned i;

  printf ("midi header:\n");

  if (m->smpte_fps)
    printf ("  SMPTE timescale: %i fps, %i delta per frame\n", m->smpte_fps, m->timing);
  else
    printf ("  standard timescale: %i delta per quarter note\n", m->timing);

  printf ("  Type %i (%s)\n", m->mode,
    m->mode == 0 ? "single" : m->mode == 1 ? "simultaneous" : m->mode == 2 ? "sequential" : "UNKNOWN");

  printf ("  %u tracks\n", m->numtracks);

  for (i = 0; i < m->numtracks; i++)
  {
    printf ("track %u\n", i);
    print_track_debug (m->tracks + i);
  }
}






#ifdef TEST

int main (int argc, char **argv)
{
  midi_t *midi;
  FILE *f;

  void *data;
  size_t len;

  if (!argv[1])
  {
    printf ("usage: parsemidi infile\n");
    return EXIT_FAILURE;
  }
  f = fopen (argv[1], "rb");
  if (!f)
  {
    printf ("failed to load %s\n", argv[1]);
    return EXIT_FAILURE;
  }

  fseek (f, 0, SEEK_END);
  len = ftell (f);
  rewind (f);
  data = smalloc (len);
  if (fread (data, 1, len, f) != len)
  {
    printf ("failed to read %u bytes\n", len);
    fclose (f);
    return EXIT_FAILURE;
  }
  fclose (f);

  midi = parsemidi (data, len);

  if (!midi)
  {
    printf ("some failure in parsemidi\n");
    return EXIT_FAILURE;
  }
  print_midi_debug (midi);

  freemidi (midi);

  return 0;
}

#endif


