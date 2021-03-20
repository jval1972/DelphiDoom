/*	MikMod sound library
	(c) 1998, 1999 Miodrag Vallat and others - see file AUTHORS for
	complete list.

	This library is free software; you can redistribute it and/or modify
	it under the terms of the GNU Library General Public License as
	published by the Free Software Foundation; either version 2 of
	the License, or (at your option) any later version.
 
	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU Library General Public License for more details.
 
	You should have received a copy of the GNU Library General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
	02111-1307, USA.
*/

/*==============================================================================

  $Id: drv_wav.c,v 1.18 1999/02/08 07:25:10 miod Exp $

  Driver for output to a file called MUSIC.WAV

==============================================================================*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <mikmod_internals.h>

#define BUFFERSIZE 32768
#define FILENAME "music.wav"

static	MWRITER *wavout=NULL;
static	FILE *wavfile=NULL;
static	SBYTE *audiobuffer=NULL;
static	ULONG dumpsize;

static void putheader(void)
{
	_mm_write_string("RIFF",wavout);
	_mm_write_I_ULONG(dumpsize+44,wavout);
	_mm_write_string("WAVEfmt ",wavout);
	_mm_write_I_ULONG(16,wavout);	/* length of this RIFF block crap */

	_mm_write_I_UWORD(1, wavout);	/* microsoft format type */
	_mm_write_I_UWORD((md_mode&DMODE_STEREO)?2:1,wavout);
	_mm_write_I_ULONG(md_mixfreq,wavout);
	_mm_write_I_ULONG(md_mixfreq*((md_mode&DMODE_STEREO)?2:1)*
	                  ((md_mode&DMODE_16BITS)?2:1),wavout);
	/* block alignment (8/16 bit) */
	_mm_write_I_UWORD(((md_mode&DMODE_16BITS)?2:1)* 
	                  ((md_mode&DMODE_STEREO)?2:1),wavout);
	_mm_write_I_UWORD((md_mode&DMODE_16BITS)?16:8,wavout);
	_mm_write_string("data",wavout);
	_mm_write_I_ULONG(dumpsize,wavout);
}

static BOOL WAV_IsThere(void)
{
	return 1;
}

static BOOL WAV_Init(void)
{
	if(!(wavfile=fopen(FILENAME, "wb"))) {
		_mm_errno=MMERR_OPENING_FILE;
		return 1;
	}
	if(!(wavout=_mm_new_file_writer (wavfile))) {
		fclose(wavfile);unlink(FILENAME);
		wavfile=NULL;
		return 1;
	}
	if(!(audiobuffer=(SBYTE*)_mm_malloc(BUFFERSIZE))) {
		_mm_delete_file_writer(wavout);
		fclose(wavfile);unlink(FILENAME);
		wavfile=NULL;wavout=NULL;
		return 1;
	}

	md_mode|=DMODE_SOFT_MUSIC|DMODE_SOFT_SNDFX;

	if (VC_Init()) {
		_mm_delete_file_writer(wavout);
		fclose(wavfile);unlink(FILENAME);
		wavfile=NULL;wavout=NULL;
		return 1;
	}
	dumpsize=0;
	putheader();

	return 0;
}

static void WAV_Exit(void)
{
	VC_Exit();

	/* write in the actual sizes now */
	if(wavout) {
		_mm_fseek(wavout,4,SEEK_SET);
		_mm_write_I_ULONG(dumpsize+32,wavout);
		_mm_fseek(wavout,40,SEEK_SET);
		_mm_write_I_ULONG(dumpsize,wavout);
		_mm_delete_file_writer(wavout);
		fclose(wavfile);
		wavfile=NULL;wavout=NULL;
	}
	if(audiobuffer) {
		free(audiobuffer);
		audiobuffer=NULL;
	}
}

static void WAV_Update(void)
{
	ULONG done;

	done=VC_WriteBytes(audiobuffer,BUFFERSIZE);
	_mm_write_UBYTES(audiobuffer,done,wavout);
	dumpsize+=done;
}

MDRIVER drv_wav={
	NULL,
	"Disk writer (wav)",
	"Wav disk writer (music.wav) v1.1",
	0,255,
	WAV_IsThere,
	VC_SampleLoad,
	VC_SampleUnload,
	VC_SampleSpace,
	VC_SampleLength,
	WAV_Init,
	WAV_Exit,
	NULL,
	VC_SetNumVoices,
	VC_PlayStart,
	VC_PlayStop,
	WAV_Update,
	NULL,
	VC_VoiceSetVolume,
	VC_VoiceSetFrequency,
	VC_VoiceSetPanning,
	VC_VoicePlay,
	VC_VoiceStop,
	VC_VoiceStopped,
	VC_VoiceGetPosition,
	NULL
};
