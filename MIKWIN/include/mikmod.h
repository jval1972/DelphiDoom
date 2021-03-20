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

  $Id: mikmod.h.in,v 1.24 1999/02/08 07:24:23 miod Exp $

  MikMod sound library include file

  Edited by Jörg Mensmann 15.02.99 (added entry for directsound-raw-driver)

==============================================================================*/

#ifndef _MIKMOD_H_
#define _MIKMOD_H_

#include <stdio.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 *	========== Library version
 */

#define LIBMIKMOD_VERSION_MAJOR 3L
#define LIBMIKMOD_VERSION_MINOR 1L
#define LIBMIKMOD_REVISION      5L

#define LIBMIKMOD_VERSION \
	((LIBMIKMOD_VERSION_MAJOR<<16)| \
	 (LIBMIKMOD_VERSION_MINOR<< 8)| \
	 (LIBMIKMOD_REVISION))

extern long MikMod_GetVersion(void);

/*
 *	========== Platform independent-type definitions
 */

#if defined(__OS2__)||defined(__EMX__)
#include <os2.h>
#else
typedef char CHAR;
#endif

#if defined(__alpha)
/* 64 bit architectures */

typedef signed char     SBYTE;      /* 1 byte, signed */
typedef unsigned char   UBYTE;      /* 1 byte, unsigned */
typedef signed short    SWORD;      /* 2 bytes, signed */
typedef unsigned short  UWORD;      /* 2 bytes, unsigned */
typedef signed int      SLONG;      /* 4 bytes, signed */
typedef unsigned int    ULONG;      /* 4 bytes, unsigned */
typedef int             BOOL;       /* 0=false, <>0 true */

#else
/* 32 bit architectures */

typedef signed char     SBYTE;      /* 1 byte, signed */
typedef unsigned char   UBYTE;      /* 1 byte, unsigned */
typedef signed short    SWORD;      /* 2 bytes, signed */
typedef unsigned short  UWORD;      /* 2 bytes, unsigned */
typedef signed long     SLONG;      /* 4 bytes, signed */
#if !defined(__OS2__)&&!defined(__EMX__)
typedef unsigned long   ULONG;      /* 4 bytes, unsigned */
typedef int             BOOL;       /* 0=false, <>0 true */
#endif
#endif

/*
 *	========== Error codes
 */

enum {
	MMERR_OPENING_FILE = 1,
	MMERR_OUT_OF_MEMORY,
	MMERR_DYNAMIC_LINKING,

	MMERR_SAMPLE_TOO_BIG,
	MMERR_OUT_OF_HANDLES,
	MMERR_UNKNOWN_WAVE_TYPE,

	MMERR_LOADING_PATTERN,
	MMERR_LOADING_TRACK,
	MMERR_LOADING_HEADER,
	MMERR_LOADING_SAMPLEINFO,
	MMERR_NOT_A_MODULE,
	MMERR_NOT_A_STREAM,
	MMERR_MED_SYNTHSAMPLES,
	MMERR_ITPACK_INVALID_DATA,

	MMERR_DETECTING_DEVICE,
	MMERR_INVALID_DEVICE,
	MMERR_INITIALIZING_MIXER,
	MMERR_OPENING_AUDIO,
	MMERR_16BIT_ONLY,
	MMERR_STEREO_ONLY,
	MMERR_NON_BLOCK,

	MMERR_AF_AUDIO_PORT,

	MMERR_AIX_CONFIG_INIT,
	MMERR_AIX_CONFIG_CONTROL,
	MMERR_AIX_CONFIG_START,

	MMERR_GUS_SETTINGS,
	MMERR_GUS_RESET,
	MMERR_GUS_TIMER,

	MMERR_HP_SETSAMPLESIZE,
	MMERR_HP_SETSPEED,
	MMERR_HP_CHANNELS,
	MMERR_HP_AUDIO_OUTPUT,
	MMERR_HP_AUDIO_DESC,
	MMERR_HP_GETGAINS,
	MMERR_HP_SETGAINS,
	MMERR_HP_BUFFERSIZE,

	MMERR_OSS_SETFRAGMENT,
	MMERR_OSS_SETSAMPLESIZE,
	MMERR_OSS_SETSTEREO,
	MMERR_OSS_SETSPEED,

	MMERR_SGI_SPEED,
	MMERR_SGI_16BIT,
	MMERR_SGI_8BIT,
	MMERR_SGI_STEREO,
	MMERR_SGI_MONO,

	MMERR_SUN_INIT,
	MMERR_SUN_16BIT_ULAW,

	MMERR_OS2_MIXSETUP,
	MMERR_OS2_SEMAPHORE,
	MMERR_OS2_TIMER,
	MMERR_OS2_THREAD,

	MMERR_MAX
};

/*
 *	========== Error handling
 */

typedef void (MikMod_handler)(void);
typedef MikMod_handler *MikMod_handler_t;

extern int  MikMod_errno;
extern BOOL MikMod_critical;
extern char *MikMod_strerror(int);

extern MikMod_handler_t MikMod_RegisterErrorHandler(MikMod_handler_t);

/*
 *	========== Library initialization and core functions
 */

struct MDRIVER;

extern void   MikMod_RegisterAllDrivers(void);

extern CHAR*  MikMod_InfoDriver(void);
extern void   MikMod_RegisterDriver(struct MDRIVER*);

extern BOOL   MikMod_Init(void);
extern void   MikMod_Exit(void);
extern BOOL   MikMod_Reset(void);
extern BOOL   MikMod_SetNumVoices(int,int);
extern BOOL   MikMod_Active(void);
extern BOOL   MikMod_EnableOutput(void);
extern void   MikMod_DisableOutput(void);
extern void   MikMod_Update(void);

/*
 *	========== Reader, Writer
 */

typedef struct MREADER {
	BOOL (*Seek)(struct MREADER*,long,int);
	long (*Tell)(struct MREADER*);
	BOOL (*Read)(struct MREADER*,void*,size_t);
	int  (*Get)(struct MREADER*);
	BOOL (*Eof)(struct MREADER*);
} MREADER;

typedef struct MWRITER {
	BOOL (*Seek)(struct MWRITER*,long,int);
	long (*Tell)(struct MWRITER*);
	BOOL (*Write)(struct MWRITER*,void*,size_t);
	BOOL (*Put)(struct MWRITER*,int);
} MWRITER;

/*
 *	========== Samples
 */

/* Sample playback should not be interrupted */
#define SFX_CRITICAL 1

/* Sample format [loading and in-memory] flags: */
#define SF_16BITS       0x0001
#define SF_STEREO       0x0002
#define SF_SIGNED       0x0004
#define SF_BIG_ENDIAN   0x0008
#define SF_DELTA        0x0010
#define SF_ITPACKED		0x0020

#define	SF_FORMATMASK	0x003F

/* General Playback flags */

#define SF_LOOP         0x0040
#define SF_BIDI         0x0080
#define SF_REVERSE      0x0100
#define SF_SUSTAIN      0x0200

#define SF_PLAYBACKMASK	0x03C0

/* Module-only Playback Flags */

#define SF_OWNPAN		0x0400
#define SF_UST_LOOP     0x0800

#define SF_EXTRAPLAYBACKMASK	0x0C00

/* Panning constants */
#define PAN_LEFT       0
#define PAN_CENTER   128
#define PAN_RIGHT    255
#define PAN_SURROUND 512 /* panning value for Dolby Surround */

typedef struct SAMPLE {
	SWORD  panning;     /* panning (0-255 or PAN_SURROUND) */
	ULONG  speed;       /* Base playing speed/frequency of note */
	UBYTE  volume;      /* volume 0-64 */
	UWORD  inflags;		/* sample format on disk */
	UWORD  flags;       /* sample format in memory */
	ULONG  length;      /* length of sample (in samples!) */
	ULONG  loopstart;   /* repeat position (relative to start, in samples) */
	ULONG  loopend;     /* repeat end */
	ULONG  susbegin;    /* sustain loop begin (in samples) \  Not Supported */
	ULONG  susend;      /* sustain loop end                /      Yet! */

	/* Variables used by the module player only! (ignored for sound effects) */
	UBYTE  globvol;     /* global volume */
	UBYTE  vibflags;    /* autovibrato flag stuffs */
	UBYTE  vibtype;     /* Vibratos moved from INSTRUMENT to SAMPLE */
	UBYTE  vibsweep;
	UBYTE  vibdepth;
	UBYTE  vibrate;
	CHAR*  samplename;  /* name of the sample */

	/* Values used internally only */
	UWORD  avibpos;     /* autovibrato pos [player use] */
	UBYTE  divfactor;   /* for sample scaling, maintains proper period slides */
	ULONG  seekpos;     /* seek position in file */
	SWORD  handle;      /* sample handle used by individual drivers */
} SAMPLE;

/* Sample functions */

extern SAMPLE *Sample_Load(CHAR*);
extern SAMPLE *Sample_LoadFP(FILE*);
extern SAMPLE *Sample_LoadGeneric(MREADER*);
extern void   Sample_Free(SAMPLE*);
extern SBYTE  Sample_Play(SAMPLE*,ULONG,UBYTE);

extern void   Voice_SetVolume(SBYTE,UWORD);
extern void   Voice_SetFrequency(SBYTE,ULONG);
extern void   Voice_SetPanning(SBYTE,ULONG);
extern void   Voice_Play(SBYTE,SAMPLE*,ULONG);
extern void   Voice_Stop(SBYTE);
extern BOOL   Voice_Stopped(SBYTE);
extern SLONG  Voice_GetPosition(SBYTE);
extern ULONG  Voice_RealVolume(SBYTE);

/*
 *	========== Internal module representation (UniMod)
 */

/*
	Instrument definition - for information only, the only field which may be
	of use in user programs is the name field
*/

/* Instrument note count */
#define INSTNOTES     120

/* Envelope point */
typedef struct ENVPT {
	SWORD pos;
	SWORD val;
} ENVPT;

/* Instrument structure */
typedef struct INSTRUMENT {
	CHAR* insname;

	UBYTE flags;
	UWORD samplenumber[INSTNOTES];
	UBYTE samplenote[INSTNOTES];

	UBYTE nnatype;
	UBYTE dca;              /* duplicate check action */
	UBYTE dct;              /* duplicate check type */
	UBYTE globvol;
	UWORD volfade;
	SWORD panning;          /* instrument-based panning var */

	UBYTE pitpansep;        /* pitch pan separation (0 to 255) */
	UBYTE pitpancenter;     /* pitch pan center (0 to 119) */
	UBYTE rvolvar;          /* random volume varations (0 - 100%) */
	UBYTE rpanvar;          /* random panning varations (0 - 100%) */

	/* volume envelope */
	UBYTE volflg;           /* bit 0: on 1: sustain 2: loop */
	UBYTE volpts;
	UBYTE volsusbeg;
	UBYTE volsusend;
	UBYTE volbeg;
	UBYTE volend;
	ENVPT volenv[32];
	/* panning envelope */
	UBYTE panflg;           /* bit 0: on 1: sustain 2: loop */
	UBYTE panpts;
	UBYTE pansusbeg;
	UBYTE pansusend;
	UBYTE panbeg;
	UBYTE panend;
	ENVPT panenv[32];
	/* pitch envelope */
	UBYTE pitflg;           /* bit 0: on 1: sustain 2: loop */
	UBYTE pitpts;
	UBYTE pitsusbeg;
	UBYTE pitsusend;
	UBYTE pitbeg;
	UBYTE pitend;
	ENVPT pitenv[32];
} INSTRUMENT;

struct MP_CONTROL;
struct MP_VOICE;

/* Module flags */
#define UF_XMPERIODS 0x01 /* XM periods / finetuning */
#define UF_LINEAR    0x02 /* LINEAR periods (UF_XMPERIODS must be set) */
#define UF_INST      0x04 /* Instruments are used */
#define UF_NNA       0x08 /* New Note Actions used (set numvoices rather than
                             numchn) */
#define UF_S3MSLIDES 0x10 /* uses old S3M volume slides */
#define UF_BGSLIDES  0x20 /* continue volume slides in the background */
#define UF_HIGHBPM   0x40 /* can use >255 bpm */

typedef struct MODULE {
	/* general module information */
		CHAR*       songname;    /* name of the song */
		CHAR*       modtype;     /* string type of module loaded */
		CHAR*       comment;     /* module comments */

		UWORD       flags;       /* See UniMod Flags above */
		UBYTE       numchn;      /* number of module channels */
		UBYTE       numvoices;   /* max # voices used for full NNA playback */
		UWORD       numpos;      /* number of positions in this song */
		UWORD       numpat;      /* number of patterns in this song */
		UWORD       numins;      /* number of instruments */
		UWORD       numsmp;      /* number of samples */
struct  INSTRUMENT* instruments; /* all instruments */
struct  SAMPLE*     samples;     /* all samples */

	/* playback settings */
		UWORD       reppos;      /* restart position */
		UBYTE       initspeed;   /* initial song speed */
		UWORD       inittempo;   /* initial song tempo */
		UBYTE       initvolume;  /* initial global volume (0 - 128) */
		UWORD       panning[64]; /* 64 panning positions */
		UBYTE       chanvol[64]; /* 64 channel positions */
		UWORD       bpm;         /* current beats-per-minute speed */
		UWORD       sngspd;      /* current song speed */
		SWORD       volume;      /* song volume (0-128) (or user volume) */

		BOOL        extspd;      /* extended speed flag (default enabled) */
		BOOL        panflag;     /* panning flag (default enabled) */
		BOOL        wrap;        /* wrap module ? (default disabled) */
		BOOL        loop;		 /* allow module to loop ? (default enabled) */
		BOOL        fadeout;	 /* volume fade out during last pattern */

		UWORD       patpos;      /* current row number */
		SWORD       sngpos;      /* current song position */
		ULONG       sngtime;     /* current song time in 2^-10 seconds */

	/* internal module representation */
		UWORD       numtrk;      /* number of tracks */
		UBYTE**     tracks;      /* array of numtrk pointers to tracks */
		UWORD*      patterns;    /* array of Patterns */
		UWORD*      pattrows;    /* array of number of rows for each pattern */
		UWORD*      positions;   /* all positions */

		BOOL        forbid;      /* if true, no player update! */
		UWORD       numrow;      /* number of rows on current pattern */
		UWORD       vbtick;      /* tick counter (counts from 0 to sngspd) */
		UWORD       sngremainder;/* used for song time computation */

struct MP_CONTROL*  control;     /* Effects Channel info (size pf->numchn) */
struct MP_VOICE*    voice;       /* Audio Voice information (size md_numchn) */

		UBYTE       globalslide; /* global volume slide rate */
		UBYTE       pat_repcrazy;/* module has just looped to position -1 */
		UWORD       patbrk;      /* position where to start a new pattern */
		UBYTE       patdly;      /* patterndelay counter (command memory) */
		UBYTE       patdly2;     /* patterndelay counter (real one) */
		SWORD       posjmp;      /* flag to indicate a jump is needed... */
} MODULE;

/*
 *	========== Module loaders
 */

struct MLOADER;

extern CHAR*   MikMod_InfoLoader(void);
extern void    MikMod_RegisterAllLoaders(void);
extern void    MikMod_RegisterLoader(struct MLOADER*);

extern struct MLOADER load_669; /* 669 and Extended-669 (by Tran/Renaissance) */
extern struct MLOADER load_amf; /* DMP Advanced Module Format (by Otto Chrons) */
extern struct MLOADER load_dsm; /* DSIK internal module format */
extern struct MLOADER load_far; /* Farandole Composer (by Daniel Potter) */
extern struct MLOADER load_it;  /* Impulse Tracker (by Jeffrey Lim) */
extern struct MLOADER load_imf; /* Imago Orpheus (by Lutz Roeder) */
extern struct MLOADER load_med; /* Amiga MED modules (by Teijo Kinnunen) */
extern struct MLOADER load_m15; /* Soundtracker 15-instrument */
extern struct MLOADER load_mod; /* Standard 31-instrument Module loader */
extern struct MLOADER load_mtm; /* Multi-Tracker Module (by Renaissance) */
extern struct MLOADER load_stm; /* ScreamTracker 2 (by Future Crew) */
extern struct MLOADER load_stx; /* STMIK 0.2 (by Future Crew) */
extern struct MLOADER load_s3m; /* ScreamTracker 3 (by Future Crew) */
extern struct MLOADER load_ult; /* UltraTracker  (by MAS) */
extern struct MLOADER load_uni; /* MikMod and APlayer internal module format */
extern struct MLOADER load_xm;  /* FastTracker 2 (by Triton) */

/*
 *	========== Module player
 */

extern MODULE* Player_Load(CHAR*,int,BOOL);
extern MODULE* Player_LoadFP(FILE*,int,BOOL);
extern MODULE* Player_LoadGeneric(MREADER*,int,BOOL);
extern CHAR*   Player_LoadTitle(CHAR*);
extern void    Player_Free(MODULE*);
extern void    Player_Start(MODULE*);
extern BOOL    Player_Active(void);
extern void    Player_Stop(void);
extern void    Player_TogglePause(void);
extern BOOL    Player_Paused(void);
extern void    Player_NextPosition(void);
extern void    Player_PrevPosition(void);
extern void    Player_SetPosition(UWORD);
extern BOOL    Player_Muted(UBYTE);
extern void    Player_SetVolume(SWORD);
extern MODULE* Player_GetModule(void);
extern void    Player_SetSpeed(UWORD);
extern void    Player_SetTempo(UWORD);
extern void    Player_Unmute(SLONG,...);
extern void    Player_Mute(SLONG,...);
extern void    Player_ToggleMute(SLONG,...);
extern int     Player_GetChannelVoice(int);

typedef void (MikMod_player)(void);
typedef MikMod_player *MikMod_player_t;

extern MikMod_player_t MikMod_RegisterPlayer(MikMod_player_t);

#define MUTE_EXCLUSIVE  32000
#define MUTE_INCLUSIVE  32001

/*
 *	========== Drivers
 */

enum {
	MD_MUSIC = 0,
	MD_SNDFX
};

enum {
	MD_HARDWARE = 0,
	MD_SOFTWARE
};

/* Mixing flags */

/* These ones take effect only after MikMod_Init or MikMod_Reset */
#define DMODE_16BITS     0x0001 /* enable 16 bit output */
#define DMODE_STEREO     0x0002 /* enable stereo output */
#define DMODE_SOFT_SNDFX 0x0004 /* Process sound effects via software mixer */
#define DMODE_SOFT_MUSIC 0x0008 /* Process music via software mixer */
#define DMODE_HQMIXER    0x0010 /* Use high-quality (slower) software mixer */
/* These take effect immediately. */
#define DMODE_SURROUND   0x0100 /* enable surround sound */
#define DMODE_INTERP     0x0200 /* enable interpolation */
#define DMODE_REVERSE    0x0400 /* reverse stereo */

struct SAMPLOAD;
typedef struct MDRIVER {
struct MDRIVER* next;
	CHAR*       Name;
	CHAR*       Version;
	UBYTE       HardVoiceLimit; /* Limit of hardware mixer voices */
	UBYTE       SoftVoiceLimit; /* Limit of software mixer voices */

	BOOL        (*IsPresent)        (void);
	SWORD       (*SampleLoad)       (struct SAMPLOAD*,int);
	void        (*SampleUnload)     (SWORD);
	ULONG       (*FreeSampleSpace)  (int);
	ULONG       (*RealSampleLength) (int,struct SAMPLE*);
	BOOL        (*Init)             (void);
	void        (*Exit)             (void);
	BOOL        (*Reset)            (void);
	BOOL        (*SetNumVoices)     (void);
	BOOL        (*PlayStart)        (void);
	void        (*PlayStop)         (void);
	void        (*Update)           (void);
	void		(*Pause)			(void);
	void        (*VoiceSetVolume)   (UBYTE,UWORD);
	void        (*VoiceSetFrequency)(UBYTE,ULONG);
	void        (*VoiceSetPanning)  (UBYTE,ULONG);
	void        (*VoicePlay)        (UBYTE,SWORD,ULONG,ULONG,ULONG,ULONG,UWORD);
	void        (*VoiceStop)        (UBYTE);
	BOOL        (*VoiceStopped)     (UBYTE);
	SLONG       (*VoiceGetPosition) (UBYTE);
	ULONG       (*VoiceRealVolume)  (UBYTE);
} MDRIVER;

/* These variables can be changed at ANY time and results will be immediate */
extern UBYTE md_volume;      /* Global sound volume (0-128) */
extern UBYTE md_musicvolume; /* volume of song */
extern UBYTE md_sndfxvolume; /* volume of sound effects */
extern UBYTE md_reverb;      /* 0 = none;  15 = chaos */
extern UBYTE md_pansep;      /* 0 = mono;  128 == 100% (full left/right) */

/* The variables below can be changed at any time, but changes will not be
   implemented until MikMod_Reset is called. A call to MikMod_Reset may result
   in a skip or pop in audio (depending on the soundcard driver and the settings
   changed). */
extern UWORD md_device;      /* Device */
extern UWORD md_mixfreq;     /* mixing frequency.  Valid 5000 -> 44100 */
extern UWORD md_mode;        /* Mode.  See DMODE_? flags above */

/* The following variable should not be changed! */
extern MDRIVER* md_driver;   /* Current driver in use. */

/* Known drivers list */

extern struct MDRIVER drv_nos;    /* no sound */
extern struct MDRIVER drv_raw;    /* raw file disk writer [music.raw] */
extern struct MDRIVER drv_stdout;
extern struct MDRIVER drv_wav;    /* RIFF WAVE file disk writer [music.wav] */

extern struct MDRIVER drv_ultra;  /* Linux Ultrasound driver */

extern struct MDRIVER drv_AF;     /* Dec Alpha AudioFile */
extern struct MDRIVER drv_aix;    /* AIX audio device */
extern struct MDRIVER drv_alsa;   /* Advanced Linux Sound Architecture (ALSA) */
extern struct MDRIVER drv_esd;    /* Enlightened sound daemon (EsounD) */
extern struct MDRIVER drv_hp;     /* HP-UX audio device */
extern struct MDRIVER drv_oss;    /* OpenSound System (Linux,FreeBSD...) */
extern struct MDRIVER drv_sgi;    /* SGI audio library */
extern struct MDRIVER drv_sun;    /* Sun/NetBSD/OpenBSD audio device */

extern struct MDRIVER drv_dart;   /* OS/2 Direct Audio RealTime */
extern struct MDRIVER drv_os2s;   /* OS/2 MMPM/2 small buffers */
extern struct MDRIVER drv_os2l;   /* OS/2 MMPM/2 large buffers */
extern struct MDRIVER drv_ds_raw; /* Windows DirectX software mixing - jm */

#ifdef __cplusplus
}
#endif

#endif
