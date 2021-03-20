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

  $Id: virtch_common.c,v 1.2 1999/02/08 07:24:42 miod Exp $

  Common source parts between the two software mixers.
  This file is probably the ugliest part of libmikmod...

==============================================================================*/

#ifndef _IN_VIRTCH_

#include <mikmod_internals.h>

#define VC_PROC0(suffix)  \
void VC_##suffix##(void)  \
{ if(md_mode&DMODE_HQMIXER) VC2_##suffix##(); else VC1_##suffix##(); }

#define VC_FUNC0(suffix,ret)  \
ret VC_##suffix##(void)  \
{ if(md_mode&DMODE_HQMIXER) return VC2_##suffix##(); else return VC1_##suffix##(); }

#define VC_PROC1(suffix,typ1)  \
void VC_##suffix##(typ1 a)  \
{ if(md_mode&DMODE_HQMIXER) VC2_##suffix##(a); else VC1_##suffix##(a); }

#define VC_FUNC1(suffix,ret,typ1)  \
ret VC_##suffix##(typ1 a)  \
{ if(md_mode&DMODE_HQMIXER) return VC2_##suffix##(a); else return VC1_##suffix##(a); }

#define VC_PROC2(suffix,typ1,typ2)  \
void VC_##suffix##(typ1 a,typ2 b)  \
{ if(md_mode&DMODE_HQMIXER) VC2_##suffix##(a,b); else VC1_##suffix##(a,b); }

#define VC_FUNC2(suffix,ret,typ1,typ2)  \
ret VC_##suffix##(typ1 a,typ2 b)  \
{ if(md_mode&DMODE_HQMIXER) return VC2_##suffix##(a,b); else return VC1_##suffix##(a,b); }

VC_FUNC0(Init,BOOL)
VC_PROC0(Exit)
VC_FUNC0(SetNumVoices,BOOL)
VC_FUNC1(SampleSpace,ULONG,int)
VC_FUNC2(SampleLength,ULONG,int,SAMPLE*)
VC_FUNC0(PlayStart,BOOL)
VC_PROC0(PlayStop)
VC_FUNC2(SampleLoad,SWORD,SAMPLOAD*,int)
VC_PROC1(SampleUnload,SWORD)
VC_FUNC2(WriteBytes,ULONG,SBYTE*,ULONG)
VC_FUNC2(SilenceBytes,ULONG,SBYTE*,ULONG)
VC_PROC2(VoiceSetVolume,UBYTE,UWORD)
VC_PROC2(VoiceSetFrequency,UBYTE,ULONG)
VC_PROC2(VoiceSetPanning,UBYTE,ULONG)

void  VC_VoicePlay(UBYTE a,SWORD b,ULONG c,ULONG d,ULONG e,ULONG f,UWORD g)
{
	if(md_mode&DMODE_HQMIXER)
		VC2_VoicePlay(a,b,c,d,e,f,g);
	else
		VC1_VoicePlay(a,b,c,d,e,f,g);
}

VC_PROC1(VoiceStop,UBYTE)
VC_FUNC1(VoiceStopped,BOOL,UBYTE)
VC_FUNC1(VoiceGetPosition,SLONG,UBYTE)
VC_FUNC1(VoiceRealVolume,ULONG,UBYTE)

#else

#ifndef _VIRTCH_COMMON_
#define _VIRTCH_COMMON_

static ULONG samples2bytes(ULONG samples)
{
	if(vc_mode & DMODE_16BITS) samples <<= 1;
	if(vc_mode & DMODE_STEREO) samples <<= 1;
	return samples;
}

static ULONG bytes2samples(ULONG bytes)
{
	if(vc_mode & DMODE_16BITS) bytes >>= 1;
	if(vc_mode & DMODE_STEREO) bytes >>= 1;
	return bytes;
}

/* Fill the buffer with 'todo' bytes of silence (it depends on the mixing mode
   how the buffer is filled) */
ULONG VC1_SilenceBytes(SBYTE* buf,ULONG todo)
{
	todo=samples2bytes(bytes2samples(todo));

	/* clear the buffer to zero (16 bits signed) or 0x80 (8 bits unsigned) */
	if(vc_mode & DMODE_16BITS)
		memset(buf,0,todo);
	else
		memset(buf,0x80,todo);

	return todo;
}

static void VC1_WriteSamples(SBYTE*,ULONG);

/* Writes 'todo' mixed SBYTES (!!) to 'buf'. It returns the number of SBYTES
   actually written to 'buf' (which is rounded to number of samples that fit
   into 'todo' bytes). */
ULONG VC1_WriteBytes(SBYTE* buf,ULONG todo)
{
	if(!vc_softchn)
		return VC1_SilenceBytes(buf,todo);

	todo = bytes2samples(todo);
	VC1_WriteSamples(buf,todo);

	return samples2bytes(todo);
}

void VC1_Exit(void)
{
	if(vc_tickbuf) free(vc_tickbuf);
	if(vinf) free(vinf);
	if(Samples) free(Samples);

	vc_tickbuf = NULL;
	vinf = NULL;
	Samples = NULL;
}

void VC1_VoiceSetFrequency(UBYTE voice,ULONG frq)
{
	vinf[voice].frq=frq;
}

void VC1_VoicePlay(UBYTE voice,SWORD handle,ULONG start,ULONG size,ULONG reppos,ULONG repend,UWORD flags)
{
	vinf[voice].flags    = flags;
	vinf[voice].handle   = handle;
	vinf[voice].start    = start;
	vinf[voice].size     = size;
	vinf[voice].reppos   = reppos;
	vinf[voice].repend   = repend;
	vinf[voice].kick     = 1;
}

void VC1_VoiceStop(UBYTE voice)
{
	vinf[voice].active = 0;
}  

BOOL VC1_VoiceStopped(UBYTE voice)
{
	return(vinf[voice].active==0);
}

SLONG VC1_VoiceGetPosition(UBYTE voice)
{
	return(vinf[voice].current>>FRACBITS);
}

/*========== External mixer interface */

void VC1_SampleUnload(SWORD handle)
{
	if (handle<MAXSAMPLEHANDLES) {
		if (Samples[handle])
			free(Samples[handle]);
		Samples[handle]=NULL;
	}
}

SWORD VC1_SampleLoad(SAMPLOAD* sload,int type)
{
	SAMPLE *s = sload->sample;
	int handle;
	ULONG t, length,loopstart,loopend;

	if(type==MD_HARDWARE) return -1;

	/* Find empty slot to put sample address in */
	for(handle=0;handle<MAXSAMPLEHANDLES;handle++)
		if(!Samples[handle]) break;

	if(handle==MAXSAMPLEHANDLES) {
		_mm_errno = MMERR_OUT_OF_HANDLES;
		return -1;
	}

	length    = s->length;
	loopstart = s->loopstart;
	loopend   = s->loopend;

	SL_SampleSigned(sload);
	SL_Sample8to16(sload);

	if(!(Samples[handle]=(SWORD*)_mm_malloc((length+20)<<1))) {
		_mm_errno = MMERR_SAMPLE_TOO_BIG;
		return -1;
	}

	/* read sample into buffer */
	if (SL_Load(Samples[handle],sload,length))
		return -1;

	/* Unclick sample */
	if(s->flags & SF_LOOP) {
		if(s->flags & SF_BIDI)
			for(t=0;t<16;t++)
				Samples[handle][loopend+t]=Samples[handle][(loopend-t)-1];
		else
			for(t=0;t<16;t++)
				Samples[handle][loopend+t]=Samples[handle][t+loopstart];
	} else
		for(t=0;t<16;t++)
			Samples[handle][t+length]=0;

	return handle;
}

ULONG VC1_SampleSpace(int type)
{
	return vc_memory;
}

ULONG VC1_SampleLength(int type,SAMPLE* s)
{
	if (!s) return 0;

	return (s->length*((s->flags&SF_16BITS)?2:1))+16;
}

ULONG VC1_VoiceRealVolume(UBYTE voice)
{
	ULONG i,s,size;
	int k,j;
	SWORD *smp;
	SLONG t;

	t = vinf[voice].current>>FRACBITS;
	if(!vinf[voice].active) return 0;

	s = vinf[voice].handle;
	size = vinf[voice].size;

	i=64; t-=64; k=0; j=0;
	if(i>size) i = size;
	if(t<0) t = 0;
	if(t+i > size) t = size-i;

	i &= ~1;  /* make sure it's EVEN. */

	smp = &Samples[s][t];
	for(;i;i--,smp++) {
		if(k<*smp) k = *smp;
		if(j>*smp) j = *smp;
	}
	return abs(k-j);
}

#endif

#endif
