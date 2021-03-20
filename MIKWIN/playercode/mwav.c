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

  $Id: mwav.c,v 1.17 1999/02/08 07:24:35 miod Exp $

  WAV sample loader

==============================================================================*/

/*
   FIXME: Stereo .WAV files are not yet supported as samples.
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <string.h>

#include <mikmod_internals.h>

typedef struct WAV {
	CHAR  rID[4];
	ULONG rLen;
	CHAR  wID[4];
	CHAR  fID[4];
	ULONG fLen;
	UWORD wFormatTag;
	UWORD nChannels;
	ULONG nSamplesPerSec;
	ULONG nAvgBytesPerSec;
	UWORD nBlockAlign;
	UWORD nFormatSpecific;
} WAV;

SAMPLE* Sample_LoadGeneric(MREADER* reader)
{
	SAMPLE *si;
	WAV wh;
	CHAR dID[4];

	/* read wav header */
	_mm_read_string(wh.rID,4,reader);
	wh.rLen = _mm_read_I_ULONG(reader);
	_mm_read_string(wh.wID,4,reader);

	while(1) {
		_mm_read_string(wh.fID,4,reader);
		wh.fLen = _mm_read_I_ULONG(reader);
		if(!(memcmp(wh.fID,"fmt ",4))) break;
		_mm_fseek(reader,wh.fLen,SEEK_CUR);
	}

	if(_mm_eof(reader)|| memcmp(wh.rID,"RIFF",4) || memcmp(wh.wID,"WAVE",4)) {
		_mm_errno = MMERR_UNKNOWN_WAVE_TYPE;
		return NULL;
	}

	wh.wFormatTag      = _mm_read_I_UWORD(reader);
	wh.nChannels       = _mm_read_I_UWORD(reader);
	wh.nSamplesPerSec  = _mm_read_I_ULONG(reader);
	wh.nAvgBytesPerSec = _mm_read_I_ULONG(reader);
	wh.nBlockAlign     = _mm_read_I_UWORD(reader);
	wh.nFormatSpecific = _mm_read_I_UWORD(reader);

	if(_mm_eof(reader)) {
		_mm_errno = MMERR_UNKNOWN_WAVE_TYPE;
		return NULL;
	}

	/* skip other crap */
	_mm_fseek(reader,wh.fLen-16,SEEK_CUR);
	_mm_read_string(dID,4,reader);

	if(memcmp(dID,"data",4)) {
		_mm_errno = MMERR_UNKNOWN_WAVE_TYPE;
		return NULL;
	}

	if(wh.nChannels>1) {
		_mm_errno = MMERR_UNKNOWN_WAVE_TYPE;
		return NULL;
	}

#ifdef MIKMOD_DEBUG
	fprintf(stderr,"\rwavloader : wFormatTag=%04x blockalign=%04x nFormatSpc=%04x\n",
	        wh.wFormatTag,wh.nBlockAlign,wh.nFormatSpecific);
#endif

	if(!(si=(SAMPLE*)_mm_malloc(sizeof(SAMPLE)))) return NULL;

	si->speed  = wh.nSamplesPerSec/wh.nChannels;
	si->volume = 64;
	si->length = _mm_read_I_ULONG(reader);

	if(wh.nBlockAlign == 2) {
		si->flags    = SF_16BITS | SF_SIGNED;
		si->length >>= 1;
	}

	si->inflags = si->flags;

	SL_RegisterSample(si,MD_SNDFX,reader);
	SL_LoadSamples();

	return si;
}

SAMPLE* Sample_LoadFP(FILE *fp)
{
	SAMPLE* result=NULL;
	MREADER* reader;

	if((reader=_mm_new_file_reader(fp))) {
		result=Sample_LoadGeneric(reader);
		_mm_delete_file_reader(reader);
	}
	return result;
}

SAMPLE* Sample_Load(CHAR* filename)
{
	FILE *fp;
	SAMPLE *si=NULL;

	if(!(md_mode & DMODE_SOFT_SNDFX)) return NULL;
	if((fp=_mm_fopen(filename,"rb"))) {
		si = Sample_LoadFP(fp);
		fclose(fp);
	}
	return si;
}

void Sample_Free(SAMPLE* si)
{
	if(si) {
		MD_SampleUnload(si->handle);
		free(si);
	}
}

