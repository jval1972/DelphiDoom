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





#include "midimerge.h"
#include "parsemidi.h"
#include "smalloc.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>



static void copyevent (event_t *dest, event_t *source)
{ // makes new copy of event data if applicable
  memcpy (dest, source, sizeof (event_t));

  if (dest->len)
  {
    dest->data = smalloc (dest->len);
    memcpy (dest->data, source->data, dest->len);
  }
}


/*
static void dbg111 (event_t *e)
{
  printf ("%06u [%02i] %i %i %i data[%u]\n", e->delta, e->channel, e->type, e->param1, e->param2, e->len);
}
*/
// combines multiple tracks of a mode 1 midi into a single track

midi_t *mergemode1 (midi_t *in)
{
  midi_t *ret;
  int i;
  int *eventpos;
  int *delta;
  int *trackendflag;

  int numeventspace;

  if (!in)
    return NULL;
  if (in->mode != 1)
    return NULL;





  ret = smalloc (sizeof (midi_t));


  ret->numtracks = 1;
  ret->mode = 0;
  ret->smpte_fps = in->smpte_fps;
  ret->timing = in->timing;
  ret->tracks = smalloc (sizeof (track_t));

  ret->tracks->numevents = 0;
  numeventspace = 0;
  ret->tracks->events = NULL;


  eventpos = scalloc (sizeof (int) * in->numtracks);
  delta = scalloc (sizeof (int) * in->numtracks);
  trackendflag = scalloc (sizeof (int) * in->numtracks);

  while (1)
  {
    int dnext = 0x7fffffff;
    int dnextpos = -1;

    for (i = 0; i < in->numtracks; i++)
    {
      if (eventpos[i] < in->tracks[i].numevents &&
          in->tracks[i].events[eventpos[i]].delta - delta[i] < dnext)
      {
        dnext = in->tracks[i].events[eventpos[i]].delta - delta[i];
        dnextpos = i;
      }
    }
    if (dnextpos == -1)
      // all tracks ran out of events
      break;
    if (in->tracks[dnextpos].events[eventpos[dnextpos]].type == event_meta &&
        in->tracks[dnextpos].events[eventpos[dnextpos]].param1 == META_ENDTRACK)
    { // tracks need not be the same length, and only one end event is allowed in the final product,
      // and it must be at the end end. eat this event, unless all other tracks are finished
      trackendflag[dnextpos] = 1;

      for (i = 0; i < in->numtracks; i++)
      {
        if (!trackendflag[i])
          break;
      }
      if (i < in->numtracks)
      {
        for (i = 0; i < in->numtracks; i++)
        { // we don't 0 out delta[dnextpos] because we didn't actually store an event on it
          // although it doesn't really matter because there ought not to be any events left
          delta[i] += dnext;
        }

        eventpos[dnextpos]++;
        continue;
      }
      // otherwise do nothing special.  the event will be copied into the stream
      // if there are stray events left, they'll be copied in too, but this isn't a concern

    }

    // store deltas and copy event
    for (i = 0; i < in->numtracks; i++)
    {
      if (i != dnextpos)
        delta[i] += dnext;
      else
        delta[i] = 0;
    }

    //printf ("#%02i#", dnextpos);
    //dbg111 (in->tracks[dnextpos].events + eventpos[dnextpos]);

    if (ret->tracks->numevents == numeventspace)
    {
      #define EVENTS_REALLOC_SCALING 100
      ret->tracks->events = srealloc (ret->tracks->events, sizeof (event_t) * (ret->tracks->numevents +
        EVENTS_REALLOC_SCALING));
      numeventspace += EVENTS_REALLOC_SCALING;
    }
    copyevent (ret->tracks->events + ret->tracks->numevents, in->tracks[dnextpos].events + eventpos[dnextpos]);

    ret->tracks->events[ret->tracks->numevents].delta = dnext;

    //printf ("$$$$");
    //dbg111 (ret->tracks->events + ret->tracks->numevents);


    ret->tracks->numevents++;
    eventpos[dnextpos]++;
  }



  // trim
  ret->tracks->events = srealloc (ret->tracks->events, sizeof (event_t) * ret->tracks->numevents);



  free (eventpos);
  free (delta);
  free (trackendflag);

  return ret;
}




