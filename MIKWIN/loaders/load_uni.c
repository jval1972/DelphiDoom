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

  $Id: load_uni.c,v 1.2 1999/02/08 07:24:17 miod Exp $

  UNIMOD (libmikmod's and APlayer's internal module format) loader

==============================================================================*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <string.h>

#include <mikmod_internals.h>

/*========== Module structure */

typedef struct UNIHEADER {
	CHAR  id[4];
	UBYTE numchn;
	UWORD numpos;
	UWORD reppos;
	UWORD numpat;
	UWORD numtrk;
	UWORD numins;
	UWORD numsmp;
	UBYTE initspeed;
	UBYTE inittempo;
	UBYTE initvolume;
	UBYTE flags;
	UBYTE numvoices;

	UBYTE positions[256];
	UBYTE panning[32];
} UNIHEADER;

typedef struct UNISMP05 {
	UWORD c2spd;
	UWORD transpose;
	UBYTE volume;
	UBYTE panning;
	ULONG length;
	ULONG loopstart;
	ULONG loopend;
	UWORD flags;
	CHAR* samplename;
	UBYTE vibtype;
	UBYTE vibsweep;
	UBYTE vibdepth;
	UBYTE vibrate;
} UNISMP05;

/*========== Loader variables */

static UBYTE universion;
static UNIHEADER mh;

#define UNI_SMPINCR 64
static UNISMP05 *wh=NULL,*s=NULL;

/*========== Loader code */

static char* readstring(void)
{
	char *s=NULL;
	UWORD len;
	
	len=_mm_read_I_UWORD(modreader);
	if(len) {
		s=_mm_malloc(len+1);
		_mm_read_UBYTES(s,len,modreader);
		s[len]=0;
	}
	return s;
}

BOOL UNI_Test(void)
{
	char id[6];

	if(!_mm_read_UBYTES(id,6,modreader)) return 0;

	/* UNIMod created by MikCvt */
	if(!(memcmp(id,"UN0",3))) {
		if((id[3]>='4')&&(id[3]<='6')) return 1;
	}
	/* UNIMod created by APlayer */
	if(!(memcmp(id,"APUN",4))) {
		if((id[4]==1)&&(id[5]==1)) return 1;
	}
	return 0;
}

BOOL UNI_Init(void)
{
	return 1;
}

void UNI_Cleanup(void)
{
	if(wh) free(wh);
	wh=s=NULL;
}

static UBYTE* readtrack(void)
{
	UBYTE *t;
	UWORD len;
	int cur=0,chunk;

	if(universion>=6)
		len=_mm_read_M_UWORD(modreader);
	else
		len=_mm_read_I_UWORD(modreader);
	if(!(t=_mm_malloc(len))) return NULL;
	_mm_read_UBYTES(t,len,modreader);

	/* Check if the track is correct */
	while(1) {
		chunk=t[cur++];
		if(!chunk) break;
		chunk=(chunk&0x1f)-1;
		while(chunk>0) {
			int opcode,oplen;

			if(cur>=len) {
				free(t);
				return NULL;
			}
			opcode=t[cur];
			if((!opcode)||(opcode>=UNI_LAST)) {
				free(t);
				return NULL;
			}
			oplen=unioperands[opcode]+1;
			cur+=oplen;
			chunk-=oplen;
		}
		if((chunk<0)||(cur>=len)) {
			free(t);
			return NULL;
		}
	}
	return t;
}

static BOOL loadsmp6(void)
{
	int t;
	SAMPLE *s;

	s=of.samples;
	for(t=0;t<of.numsmp;t++,s++) {
		int flags;

		flags         = _mm_read_M_UWORD(modreader);
		/* convert flags */
		s->flags=0;
		if(flags&1024) s->flags|=SF_UST_LOOP;
		if(flags& 512) s->flags|=SF_OWNPAN;
		if(flags& 256) s->flags|=SF_REVERSE;
		if(flags& 128) s->flags|=SF_SUSTAIN;
		if(flags&  64) s->flags|=SF_BIDI;
		if(flags&  32) s->flags|=SF_LOOP;
		if(flags&  16) s->flags|=SF_BIG_ENDIAN;
		if(flags&   8) s->flags|=SF_DELTA;
		if(flags&   4) s->flags|=SF_STEREO;
		if(flags&   2) s->flags|=SF_SIGNED;
		if(flags&   1) s->flags|=SF_16BITS;

		s->speed      = _mm_read_M_ULONG(modreader);
		s->volume     = _mm_read_UBYTE(modreader);
		s->panning    = _mm_read_M_UWORD(modreader);
		s->length     = _mm_read_M_ULONG(modreader);
		s->loopstart  = _mm_read_M_ULONG(modreader);
		s->loopend    = _mm_read_M_ULONG(modreader);
		s->susbegin   = _mm_read_M_ULONG(modreader);
		s->susend     = _mm_read_M_ULONG(modreader);
		s->globvol    = _mm_read_UBYTE(modreader);
		s->vibflags   = _mm_read_UBYTE(modreader);
		s->vibtype    = _mm_read_UBYTE(modreader);
		s->vibsweep   = _mm_read_UBYTE(modreader);
		s->vibdepth   = _mm_read_UBYTE(modreader);
		s->vibrate    = _mm_read_UBYTE(modreader);

		s->samplename=readstring();

		if(_mm_eof(modreader)) {
			_mm_errno = MMERR_LOADING_SAMPLEINFO;
			return 0;
		}
	}
	return 1;
}

static BOOL loadinstr6(void)
{
	int t,w;
	INSTRUMENT *i;

	i=of.instruments;
	for(t=0;t<of.numins;t++,i++) {
		i->flags        = _mm_read_UBYTE(modreader);
		i->nnatype      = _mm_read_UBYTE(modreader);
		i->dca          = _mm_read_UBYTE(modreader);
		i->dct          = _mm_read_UBYTE(modreader);
		i->globvol      = _mm_read_UBYTE(modreader);
		i->panning      = _mm_read_M_UWORD(modreader);
		i->pitpansep    = _mm_read_UBYTE(modreader);
		i->pitpancenter = _mm_read_UBYTE(modreader);
		i->rvolvar      = _mm_read_UBYTE(modreader);
		i->rpanvar      = _mm_read_UBYTE(modreader);
		i->volfade      = _mm_read_M_UWORD(modreader);

		i->volflg       = _mm_read_UBYTE(modreader);
		i->volpts       = _mm_read_UBYTE(modreader);
		i->volsusbeg    = _mm_read_UBYTE(modreader);
		i->volsusend    = _mm_read_UBYTE(modreader);
		i->volbeg       = _mm_read_UBYTE(modreader);
		i->volend       = _mm_read_UBYTE(modreader);
		for(w=0;w<i->volpts;w++) {
			i->volenv[w].pos = _mm_read_M_SWORD(modreader);
			i->volenv[w].val = _mm_read_M_SWORD(modreader);
		}

		i->panflg    = _mm_read_UBYTE(modreader);
		i->panpts    = _mm_read_UBYTE(modreader);
		i->pansusbeg = _mm_read_UBYTE(modreader);
		i->pansusend = _mm_read_UBYTE(modreader);
		i->panbeg    = _mm_read_UBYTE(modreader);
		i->panend    = _mm_read_UBYTE(modreader);
		for(w=0;w<i->panpts;w++) {
			i->panenv[w].pos = _mm_read_M_SWORD(modreader);
			i->panenv[w].val = _mm_read_M_SWORD(modreader);
		}

		i->pitflg    = _mm_read_UBYTE(modreader);
		i->pitpts    = _mm_read_UBYTE(modreader);
		i->pitsusbeg = _mm_read_UBYTE(modreader);
		i->pitsusend = _mm_read_UBYTE(modreader);
		i->pitbeg    = _mm_read_UBYTE(modreader);
		i->pitend    = _mm_read_UBYTE(modreader);
		for(w=0;w<i->pitpts;w++) {
			i->pitenv[w].pos = _mm_read_M_SWORD(modreader);
			i->pitenv[w].val = _mm_read_M_SWORD(modreader);
		}

		for(w=0;w<120;w++)
			i->samplenumber[w]=_mm_read_UBYTE(modreader);
		_mm_read_UBYTES(i->samplenote,120,modreader);

		i->insname=readstring();

		if(_mm_eof(modreader)) {
			_mm_errno = MMERR_LOADING_SAMPLEINFO;
			return 0;
		}
	}
	return 1;
}

static BOOL loadinstr5(void)
{
	INSTRUMENT *i;
	int t;
	UWORD wavcnt=0;
	UBYTE vibtype,vibsweep,vibdepth,vibrate;

	i=of.instruments;
	for(of.numsmp=t=0;t<of.numins;t++,i++) {
		int u,numsmp;

		numsmp=_mm_read_UBYTE(modreader);

		memset(i->samplenumber,0xff,INSTNOTES*sizeof(UWORD));
		for(u=0;u<96;u++)
			i->samplenumber[u]=of.numsmp+_mm_read_UBYTE(modreader);

		i->volflg   =_mm_read_UBYTE(modreader);
		i->volpts   =_mm_read_UBYTE(modreader);
		i->volsusbeg=_mm_read_UBYTE(modreader);
		i->volsusend=i->volsusbeg;
		i->volbeg   =_mm_read_UBYTE(modreader);
		i->volend   =_mm_read_UBYTE(modreader);
		for(u=0;u<12;u++) {
			i->volenv[u].pos=_mm_read_I_SWORD(modreader);
			i->volenv[u].val=_mm_read_I_SWORD(modreader);
		}
		if(i->volpts) i->volflg|=EF_ON;

		i->panflg   =_mm_read_UBYTE(modreader);
		i->panpts   =_mm_read_UBYTE(modreader);
		i->pansusbeg=_mm_read_UBYTE(modreader);
		i->pansusend=i->pansusbeg;
		i->panbeg   =_mm_read_UBYTE(modreader);
		i->panend   =_mm_read_UBYTE(modreader);
		for(u=0;u<12;u++) {
			i->panenv[u].pos=_mm_read_I_SWORD(modreader);
			i->panenv[u].val=_mm_read_I_SWORD(modreader);
		}
		if(i->panpts) i->panflg|=EF_ON;

		vibtype      =_mm_read_UBYTE(modreader);
		vibsweep     =_mm_read_UBYTE(modreader);
		vibdepth     =_mm_read_UBYTE(modreader);
		vibrate      =_mm_read_UBYTE(modreader);

		i->volfade=_mm_read_I_UWORD(modreader);
		i->insname=readstring();

		for(u=0;u<numsmp;u++,s++,of.numsmp++) {
			/* Allocate more room for sample information if necessary */
			if(of.numsmp+u==wavcnt) {
				wavcnt+=UNI_SMPINCR;
				if(!(wh=realloc(wh,wavcnt*sizeof(UNISMP05)))) {
					_mm_errno=MMERR_OUT_OF_MEMORY;
					return 0;
				}
				s=wh+(wavcnt-UNI_SMPINCR);
			}

			s->c2spd    =_mm_read_I_UWORD(modreader);
			s->transpose=_mm_read_SBYTE(modreader);
			s->volume   =_mm_read_UBYTE(modreader);
			s->panning  =_mm_read_UBYTE(modreader);
			s->length   =_mm_read_I_ULONG(modreader);
			s->loopstart=_mm_read_I_ULONG(modreader);
			s->loopend  =_mm_read_I_ULONG(modreader);
			s->flags    =_mm_read_I_UWORD(modreader);
			s->samplename=readstring();

			s->vibtype =vibtype;
			s->vibsweep=vibsweep;
			s->vibdepth=vibdepth;
			s->vibrate =vibrate;

			if(_mm_eof(modreader)) {
				free(wh);wh=NULL;
				_mm_errno=MMERR_LOADING_SAMPLEINFO;
				return 0;
			}
		}
	}

	/* sanity check */
	if(!of.numsmp) {
		if(wh) { free(wh);wh=NULL; }
		_mm_errno=MMERR_LOADING_SAMPLEINFO;
		return 0;
	}
	return 1;
}

static BOOL loadsmp5(void)
{
	int t,u;
	SAMPLE *q;
	INSTRUMENT *d;

	q=of.samples;s=wh;
	for(u=0;u<of.numsmp;u++,q++,s++) {
		q->samplename=s->samplename;

		q->length   =s->length;
		q->loopstart=s->loopstart;
		q->loopend  =s->loopend;
		q->volume   =s->volume;
		q->speed    =s->c2spd;
		q->panning  =s->panning;
		q->vibtype  =s->vibtype;
		q->vibsweep =s->vibsweep;
		q->vibdepth =s->vibdepth;
		q->vibrate  =s->vibrate;

		/* convert flags */
		q->flags=0;
		if(s->flags&128) q->flags|=SF_REVERSE;
		if(s->flags& 64) q->flags|=SF_SUSTAIN;
		if(s->flags& 32) q->flags|=SF_BIDI;
		if(s->flags& 16) q->flags|=SF_LOOP;
		if(s->flags&  8) q->flags|=SF_BIG_ENDIAN;
		if(s->flags&  4) q->flags|=SF_DELTA;
		if(s->flags&  2) q->flags|=SF_SIGNED;
		if(s->flags&  1) q->flags|=SF_16BITS;
	}

	d=of.instruments;s=wh;
	for(u=0;u<of.numins;u++,d++)
		for(t=0;t<INSTNOTES;t++)
			d->samplenote[t]=(d->samplenumber[t]>=of.numsmp)?
			  255:(t+s[d->samplenumber[t]].transpose);

	free(wh);wh=NULL;

	return 1;
}

BOOL UNI_Load(BOOL curious)
{
	int t;
	char *modtype,*oldtype;
	INSTRUMENT *d;
	SAMPLE *q;
	
	/* read module header */
	_mm_read_UBYTES(mh.id,4,modreader);
	if(mh.id[3]!='N')
		universion=mh.id[3]-'0';
	else
		universion=7;

	if(universion>=6) {
		_mm_read_UBYTE(modreader);
		mh.flags     =_mm_read_M_UWORD(modreader);
		mh.numchn    =_mm_read_UBYTE(modreader);
		mh.numvoices =_mm_read_UBYTE(modreader);
		mh.numpos    =_mm_read_M_UWORD(modreader);
		mh.numpat    =_mm_read_M_UWORD(modreader);
		mh.numtrk    =_mm_read_M_UWORD(modreader);
		mh.numins    =_mm_read_M_UWORD(modreader);
		mh.numsmp    =_mm_read_M_UWORD(modreader);
		mh.reppos    =_mm_read_M_UWORD(modreader);
		mh.initspeed =_mm_read_UBYTE(modreader);
		mh.inittempo =_mm_read_UBYTE(modreader);
		mh.initvolume=_mm_read_UBYTE(modreader);

		mh.flags&=(UF_XMPERIODS|UF_LINEAR|UF_INST|UF_NNA);
	} else {
		mh.numchn    =_mm_read_UBYTE(modreader);
		mh.numpos    =_mm_read_I_UWORD(modreader);
		mh.reppos    =(universion==5)?_mm_read_I_UWORD(modreader):0;
		mh.numpat    =_mm_read_I_UWORD(modreader);
		mh.numtrk    =_mm_read_I_UWORD(modreader);
		mh.numins    =_mm_read_I_UWORD(modreader);
		mh.initspeed =_mm_read_UBYTE(modreader);
		mh.inittempo =_mm_read_UBYTE(modreader);
		_mm_read_UBYTES(mh.positions,256,modreader);
		_mm_read_UBYTES(mh.panning,32,modreader);
		mh.flags     =_mm_read_UBYTE(modreader);

		mh.flags&=(UF_XMPERIODS|UF_LINEAR);
		mh.flags|=UF_INST;
	}
	
	/* set module parameters */
	of.flags     =mh.flags;
	of.numchn    =mh.numchn;
	of.numpos    =mh.numpos;
	of.numpat    =mh.numpat;
	of.numtrk    =mh.numtrk;
	of.numins    =mh.numins;
	of.reppos    =mh.reppos;
	of.initspeed =mh.initspeed;
	of.inittempo =mh.inittempo;

	of.songname=readstring();
	oldtype=readstring();
	if(!(modtype=_mm_malloc(strlen(oldtype)+40))) return 0;
	sprintf(modtype,"%s (was %s)",(universion==7)?"APlayer":"MikCvt",oldtype);
	of.modtype=strdup(modtype);
	free(modtype);free(oldtype);
	of.comment=readstring();

	if(universion>=6) {
		of.numvoices=mh.numvoices;
		of.initvolume=mh.initvolume;
	}

	if(_mm_eof(modreader)) {
		_mm_errno=MMERR_LOADING_HEADER;
		return 0;
	}

	/* positions */
	if(!AllocPositions(of.numpos)) return 0;
	if(universion>=6) {
		_mm_read_UBYTES(of.positions,of.numpos,modreader);
		_mm_read_M_UWORDS(of.panning,of.numchn,modreader);
		_mm_read_UBYTES(of.chanvol,of.numchn,modreader);
	} else {
		if((mh.numpos>256)||(mh.numchn>32)) {
			_mm_errno=MMERR_LOADING_HEADER;
			return 0;
		}
		for(t=0;t<of.numpos;t++) of.positions[t]=mh.positions[t];
		for(t=0;t<of.numchn;t++) of.panning[t]=mh.panning[t];
	}

	/* instruments and samples */
	if(universion>=6) {
		of.numsmp=mh.numsmp;
		if(!AllocSamples()) return 0;
		if(!loadsmp6()) return 0;

		if(of.flags&UF_INST) {
			if(!AllocInstruments()) return 0;
			if(!loadinstr6()) return 0;
		}
	} else {
		if(!AllocInstruments()) return 0;
		if(!loadinstr5()) return 0;
		if(!AllocSamples()) {
			if(wh) { free(wh);wh=NULL; }
			return 0;
		}
		if(!loadsmp5()) return 0;

		/* check if the original file had no instruments */
		if(of.numsmp==of.numins) {
			for(t=0,d=of.instruments;t<of.numins;t++,d++) {
				int u;

				if((d->volpts)||(d->panpts)||(d->globvol!=64)) break;
				for(u=0;u<96;u++)
					if((d->samplenumber[u]!=t)||(d->samplenote[u]!=u)) break;
				if(u!=96) break;
			}
			if(t==of.numins) {
				of.flags&=~UF_INST;
				for(t=0,d=of.instruments,q=of.samples;t<of.numins;t++,d++,q++) {
					q->samplename=d->insname;
					d->insname=NULL;
				}
			}
		}
	}

	/* patterns */
	if(!AllocPatterns()) return 0;
	if(universion>=6) {
		_mm_read_M_UWORDS(of.pattrows,of.numpat,modreader);
		_mm_read_M_UWORDS(of.patterns,of.numpat*of.numchn,modreader);
	} else {
		_mm_read_I_UWORDS(of.pattrows,of.numpat,modreader);
		_mm_read_I_UWORDS(of.patterns,of.numpat*of.numchn,modreader);
	}

	/* tracks */
	if(!AllocTracks()) return 0;
	for(t=0;t<of.numtrk;t++)
		if(!(of.tracks[t]=readtrack())) {
			_mm_errno=MMERR_LOADING_TRACK;
			return 0;
		}

	return 1;
}

CHAR *UNI_LoadTitle(void)
{
	UBYTE ver;
	int posit[3]={304,306,26};

	_mm_fseek(modreader,3,SEEK_SET);
	ver=_mm_read_UBYTE(modreader);
	if(ver=='N') ver='6';

	_mm_fseek(modreader,posit[ver-'4'],SEEK_SET);
	return readstring();
}

/*========== Loader information */

MLOADER load_uni={
	NULL,
	"UNI",
	"UNI loader v1.0",
	UNI_Init,
	UNI_Test,
	UNI_Load,
	UNI_Cleanup,
	UNI_LoadTitle
};
