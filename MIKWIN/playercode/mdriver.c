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

  $Id: mdriver.c,v 1.17 1999/02/08 07:24:30 miod Exp $

  These routines are used to access the available soundcard drivers.

==============================================================================*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <mikmod_internals.h>

#include <string.h>

static	MDRIVER *firstdriver=NULL;
		MDRIVER *md_driver=NULL;
extern	MODULE *pf; /* modfile being played */

		UWORD md_device         = 0;
		UWORD md_mixfreq        = 44100;
		UWORD md_mode           = DMODE_STEREO | DMODE_16BITS | DMODE_SURROUND
		                         |DMODE_SOFT_MUSIC | DMODE_SOFT_SNDFX;
		UBYTE md_pansep         = 128; /* 128 == 100% (full left/right) */
		UBYTE md_reverb         = 6; /* Reverb */
		UBYTE md_volume         = 96;  /* Global sound volume (0-128) */
		UBYTE md_musicvolume    = 128; /* volume of song */
		UBYTE md_sndfxvolume    = 128; /* volume of sound effects */
		UWORD md_bpm            = 125;

/* Do not modify the numchn variables yourself!  use MD_SetVoices() */
		UBYTE md_numchn=0,md_sngchn=0,md_sfxchn=0;
		UBYTE md_hardchn=0,md_softchn=0;

		void (*md_player)(void) = Player_HandleTick;
static	BOOL  isplaying=0, initialized = 0;
static	UBYTE *sfxinfo;
static	int sfxpool;

static	SAMPLE **md_sample = NULL;

/* Previous driver in use */
static	UWORD idevice;

/* Limits the number of hardware voices to the specified amount.
   This function should only be used by the low-level drivers. */
static	void LimitHardVoices(int limit)
{
	int t = 0;

	if(!(md_mode & DMODE_SOFT_SNDFX) && (md_sfxchn>limit)) md_sfxchn=limit;
	if(!(md_mode & DMODE_SOFT_MUSIC) && (md_sngchn>limit)) md_sngchn=limit;

	if(!(md_mode & DMODE_SOFT_SNDFX))
		md_hardchn = md_sfxchn;
	else
		md_hardchn = 0;

	if(!(md_mode & DMODE_SOFT_MUSIC)) md_hardchn += md_sngchn;

	while(md_hardchn>limit) {
		if(++t & 1) {
			if(!(md_mode & DMODE_SOFT_SNDFX) && (md_sfxchn > 4)) md_sfxchn--;
		} else {
			if(!(md_mode & DMODE_SOFT_MUSIC) && (md_sngchn > 8)) md_sngchn--;
		}

		if(!(md_mode & DMODE_SOFT_SNDFX))
			md_hardchn = md_sfxchn;
		else
			md_hardchn = 0;

		if(!(md_mode & DMODE_SOFT_MUSIC))
			md_hardchn += md_sngchn;
	}
	md_numchn = md_hardchn + md_softchn;
}

/* Limits the number of hardware voices to the specified amount.
   This function should only be used by the low-level drivers. */
static	void LimitSoftVoices(int limit)
{
	int t = 0;

	if((md_mode & DMODE_SOFT_SNDFX) && (md_sfxchn > limit)) md_sfxchn = limit;
	if((md_mode & DMODE_SOFT_MUSIC) && (md_sngchn > limit)) md_sngchn = limit;

	if(md_mode & DMODE_SOFT_SNDFX)
		md_softchn = md_sfxchn;
	else
		md_softchn = 0;

	if(md_mode & DMODE_SOFT_MUSIC) md_softchn += md_sngchn;

	while(md_softchn > limit) {
		if(++t & 1) {
			if((md_mode & DMODE_SOFT_SNDFX) && (md_sfxchn > 4)) md_sfxchn--;
		} else {
			if((md_mode & DMODE_SOFT_MUSIC) && (md_sngchn > 8)) md_sngchn--;
		}

		if(!(md_mode & DMODE_SOFT_SNDFX))
			md_softchn = md_sfxchn;
		else
			md_softchn = 0;

		if(!(md_mode & DMODE_SOFT_MUSIC))
			md_softchn += md_sngchn;
	}
	md_numchn = md_hardchn + md_softchn;
}

/* Note: 'type' indicates whether the returned value should be for music or for
   sound effects. */
ULONG MD_SampleSpace(int type)
{
	if(type==MD_MUSIC)
		type = (md_mode & DMODE_SOFT_MUSIC) ? MD_SOFTWARE : MD_HARDWARE;
	else if(type==MD_SNDFX)
		type = (md_mode & DMODE_SOFT_SNDFX) ? MD_SOFTWARE : MD_HARDWARE;

	return md_driver->FreeSampleSpace(type);
}

ULONG MD_SampleLength(int type,SAMPLE* s)
{
	if(type==MD_MUSIC)
		type = (md_mode & DMODE_SOFT_MUSIC) ? MD_SOFTWARE : MD_HARDWARE;
	else
	  if(type==MD_SNDFX)
		type = (md_mode & DMODE_SOFT_SNDFX) ? MD_SOFTWARE : MD_HARDWARE;

	return md_driver->RealSampleLength(type,s);
}

CHAR* MikMod_InfoDriver(void)
{
	int t,len=0;
	MDRIVER *l;
	CHAR *list=NULL;

	/* compute size of buffer */
	for(l=firstdriver;l;l=l->next) len+=4+(l->next?1:0)+strlen(l->Version);

	if(len)
		if((list=_mm_malloc(len*sizeof(CHAR)))) {
			list[0]=0;
			/* list all registered device drivers : */
			for(t=1,l=firstdriver;l;l=l->next,t++)
				sprintf(list,(l->next)?"%s%2d %s\n":"%s%2d %s",list,t,l->Version);
		}
	return list;
}

void MikMod_RegisterDriver(MDRIVER* drv)
{
	MDRIVER *cruise = firstdriver;

	/* if we try to register an invalid driver, or an already registered driver,
	   ignore this attempt */
	if ((!drv)||(drv->next))
		return;

	if(cruise) {
		while(cruise->next) cruise=cruise->next;
		cruise->next=drv;
	} else
		firstdriver = drv; 
}

SWORD MD_SampleLoad(SAMPLOAD* s, int type)
{
	SWORD result;

	if(type==MD_MUSIC)
		type = (md_mode & DMODE_SOFT_MUSIC) ? MD_SOFTWARE : MD_HARDWARE;
	else if(type==MD_SNDFX)
		type = (md_mode & DMODE_SOFT_SNDFX) ? MD_SOFTWARE : MD_HARDWARE;

	SL_Init(s);
	result = md_driver->SampleLoad(s, type);
	SL_Exit(s);

	return result;
}

void MD_SampleUnload(SWORD handle)
{
	md_driver->SampleUnload(handle);
}

MikMod_player_t MikMod_RegisterPlayer(MikMod_player_t player)
{
	MikMod_player_t oldproc=md_player;

	md_player=player;
	return oldproc;
}

void MikMod_Update(void)
{
	if(isplaying) {
		if((!pf)||(!pf->forbid)) {
			md_driver->Update();
		} else {
			if (md_driver->Pause)
				md_driver->Pause();
		}
	}
}

void Voice_SetVolume(SBYTE voice,UWORD vol)
{
	ULONG  tmp;

	if((voice<0)||(voice>=md_numchn)) return;

	/* range checks */
	if(md_musicvolume>128) md_musicvolume=128;
	if(md_sndfxvolume>128) md_sndfxvolume=128;
	if(md_volume>128) md_volume=128;

	tmp=(ULONG)vol*(ULONG)md_volume*
	     ((voice<md_sngchn)?(ULONG)md_musicvolume:(ULONG)md_sndfxvolume);
	md_driver->VoiceSetVolume(voice,tmp/16384UL);
}

void Voice_SetFrequency(SBYTE voice,ULONG frq)
{
	if((voice<0)||(voice>=md_numchn)) return;
	if((md_sample[voice])&&(md_sample[voice]->divfactor))
		frq/=md_sample[voice]->divfactor;
	md_driver->VoiceSetFrequency(voice,frq);
}

void Voice_SetPanning(SBYTE voice,ULONG pan)
{
	if((voice<0)||(voice>=md_numchn)) return;
	if(pan!=PAN_SURROUND) {
#ifdef MIKMOD_DEBUG
		if((pan<0)||(pan>255))
			fprintf(stderr,"\rVoice_SetPanning called with pan=%ld\n",pan);
#endif
		if(md_pansep>128) md_pansep=128;
		if(md_mode & DMODE_REVERSE) pan=255-pan;
		pan = (((SWORD)(pan-128)*md_pansep)/128)+128;
	}
	md_driver->VoiceSetPanning(voice, pan);
}

void Voice_Play(SBYTE voice,SAMPLE* s,ULONG start)
{
	ULONG  repend;

	if((voice<0)||(voice>=md_numchn)||(start>=s->length)) return;

	md_sample[voice]=s;
	repend=s->loopend;

	if(s->flags&SF_LOOP)
		/* repend can't be bigger than size */
		if(repend>s->length) repend=s->length;

	md_driver->VoicePlay(voice,s->handle,start,s->length,s->loopstart,repend,s->flags);
}

void Voice_Stop(SBYTE voice)
{
	if((voice<0)||(voice>=md_numchn)) return;
	if(voice>=md_sngchn)
		/* It is a sound effects channel, so flag the voice as non-critical! */
		sfxinfo[voice-md_sngchn]=0;
	md_driver->VoiceStop(voice);
}

BOOL Voice_Stopped(SBYTE voice)
{
	if((voice<0)||(voice>=md_numchn)) return 0;
	return(md_driver->VoiceStopped(voice));
}

SLONG Voice_GetPosition(SBYTE voice)
{
	if((voice<0)||(voice>=md_numchn)) return 0;
	return(md_driver->VoiceGetPosition(voice));
}

ULONG Voice_RealVolume(SBYTE voice)
{
	if((voice<0)||(voice>=md_numchn)) return 0;
	return(md_driver->VoiceRealVolume(voice));
}

BOOL MikMod_Init(void)
{
	UWORD t;

	_mm_critical = 1;

	/* if md_device==0, try to find a device number */
	if(!md_device) {
		for(t=1,md_driver=firstdriver;md_driver;md_driver=md_driver->next,t++)
			if(md_driver->IsPresent()) break;

		if(!md_driver) {
			_mm_errno = MMERR_DETECTING_DEVICE;
			if(_mm_errorhandler) _mm_errorhandler();
			md_driver = &drv_nos;
			return 1;
		}

		md_device = t;
	} else {
		/* if n>0 use that driver */
		for(t=1,md_driver=firstdriver;(md_driver)&&(t!=md_device);md_driver=md_driver->next,t++);

		if(!md_driver) {
			_mm_errno = MMERR_INVALID_DEVICE;
			if(_mm_errorhandler) _mm_errorhandler();
			md_driver = &drv_nos;
			return 1;
		}

		if(!md_driver->IsPresent()) {
			_mm_errno = MMERR_DETECTING_DEVICE;
			if(_mm_errorhandler) _mm_errorhandler();
			md_driver = &drv_nos;
			return 1;
		}
	}

	if(md_driver->Init()) {
		MikMod_Exit();
		if(_mm_errorhandler) _mm_errorhandler();
		return 1;
	}

	initialized=1;
	_mm_critical=0;

	return 0;
}

void MikMod_Exit(void)
{
	MikMod_DisableOutput();
	md_driver->Exit();
	md_numchn = md_sfxchn = md_sngchn = 0;
	md_driver = &drv_nos;

	if(sfxinfo) free(sfxinfo);
	if(md_sample) free(md_sample);
	md_sample  = NULL;
	sfxinfo    = NULL;

	initialized = 0;
}

/* Reset the driver using the new global variable settings. 
   If the driver has not been initialized, it will be now. */
BOOL MikMod_Reset(void)
{
	if(!initialized) return MikMod_Init();

	if((!md_driver->Reset)||(md_device != idevice)) {
		/* md_driver->Reset was NULL, or md_device was changed, so do a full
		   reset of the driver. */
		if(isplaying) md_driver->PlayStop();
		md_driver->Exit();
		if(MikMod_Init()) {
			MikMod_Exit();
			if(_mm_errno)
				if(_mm_errorhandler) _mm_errorhandler();
			return 1;
		}
		if(isplaying) md_driver->PlayStart();
	} else {
		if(md_driver->Reset()) {
			MikMod_Exit();
			if(_mm_errno)
				if(_mm_errorhandler) _mm_errorhandler();
			return 1;
		}
	}
	return 0;
}

/* If either parameter is -1, the current set value will be retained. */
BOOL MikMod_SetNumVoices(int music, int sfx)
{
	BOOL resume = 0;
	int t, oldchn = 0;

	if((!music)&&(!sfx)) return 1;
	_mm_critical = 1;
	if(isplaying) {
		MikMod_DisableOutput();
		oldchn = md_numchn;
		resume = 1;
	}

	if(sfxinfo) free(sfxinfo);
	if(md_sample) free(md_sample);
	md_sample  = NULL;
	sfxinfo    = NULL;

	if(music!=-1) md_sngchn = music;
	if(sfx!=-1)   md_sfxchn = sfx;
	md_numchn = md_sngchn + md_sfxchn;

	LimitHardVoices(md_driver->HardVoiceLimit);
	LimitSoftVoices(md_driver->SoftVoiceLimit);

	if(md_driver->SetNumVoices()) {
		MikMod_Exit();
		if(_mm_errno)
			if(_mm_errorhandler!=NULL) _mm_errorhandler();
		md_numchn = md_softchn = md_hardchn = md_sfxchn = md_sngchn = 0;
		return 1;
	}

	if(md_sngchn+md_sfxchn)
		md_sample=(SAMPLE**)_mm_calloc(md_sngchn+md_sfxchn,sizeof(SAMPLE*));
	if(md_sfxchn)
		sfxinfo = (UBYTE *)_mm_calloc(md_sfxchn,sizeof(UBYTE));

	/* make sure the player doesn't start with garbage */
	for(t=oldchn;t<md_numchn;t++)  Voice_Stop(t);

	sfxpool = 0;
	if(resume) MikMod_EnableOutput();
	_mm_critical = 0;

	return 0;
}

BOOL MikMod_EnableOutput(void)
{
	_mm_critical = 1;
	if(!isplaying) {
		if(md_driver->PlayStart()) return 1;
		isplaying = 1;
	}
	_mm_critical = 0;
	return 0;
}

void MikMod_DisableOutput(void)
{
	if(isplaying && md_driver) {
		isplaying = 0;
		md_driver->PlayStop();
	}
}

BOOL MikMod_Active(void)
{
	return isplaying;
}

/* Plays a sound effects sample.  Picks a voice from the number of voices
   allocated for use as sound effects (loops through voices, skipping all active
   criticals).

   Returns the voice that the sound is being played on.                       */
SBYTE Sample_Play(SAMPLE *s,ULONG start,UBYTE flags)
{
	int orig=sfxpool;/* for cases where all channels are critical */
	int c;

	if(!md_sfxchn) return -1;
	if(s->volume>64) s->volume = 64;

	/* check the first location after sfxpool */
	do {
		if(sfxinfo[sfxpool]&SFX_CRITICAL) {
			if(md_driver->VoiceStopped(c=sfxpool+md_sngchn)) {
				sfxinfo[sfxpool]=flags;
				Voice_Play(c, s, start);
				md_driver->VoiceSetVolume(c,s->volume<<2);
				Voice_SetPanning(c,s->panning);
				md_driver->VoiceSetFrequency(c,s->speed);
				sfxpool++;
				if(sfxpool>=md_sfxchn) sfxpool=0;
				return c;
			}
		} else {
			sfxinfo[sfxpool] = flags;
			Voice_Play(c=sfxpool+md_sngchn, s, start);
			md_driver->VoiceSetVolume(c,s->volume<<2);
			Voice_SetPanning(c,s->panning);
			md_driver->VoiceSetFrequency(c,s->speed);
			sfxpool++;
			if(sfxpool>=md_sfxchn) sfxpool=0;
			return c;
		}

		sfxpool++;
		if(sfxpool>=md_sfxchn) sfxpool = 0;
	} while(sfxpool!=orig);

	return -1;
}

long MikMod_GetVersion(void)
{
	return LIBMIKMOD_VERSION;
}
