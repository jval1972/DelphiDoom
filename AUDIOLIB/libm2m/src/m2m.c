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

#include "parsemidi.h"
#include <stdio.h>
#include <stdlib.h>
#include "midimerge.h"
#include "convert.h"
#include "smalloc.h"
#include <string.h>


int convertm2m(char *buff, int buffsize, char *outbuff, int *outsize)
{
  midi_t *midi;
  midi_t *newmidi;

  midi = parsemidi (buff, buffsize);

  if (!midi)
  {
    fprintf (stderr, "some failure in parsemidi\n");
    return EXIT_FAILURE;
  }

  if (midi->mode == 1)
  {
    //print_midi_debug (midi);

    fprintf (stderr, "merging tracks on type 1 file\n");
    newmidi = mergemode1 (midi);
    if (!newmidi)
    {
      fprintf (stderr, "fail in merging mode 1 to mode 0\n");
      return EXIT_FAILURE;
    }
    freemidi (midi);
    midi = newmidi;
  }

  convert_midi (midi, outbuff, outsize);

  freemidi (midi);

  return 0;
}

