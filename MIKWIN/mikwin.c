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

  Windows DirectSound-Raw Driver
  Copyright (C) 1999 by Jörg Mensmann <joerg.mensmann@gmx.net>

  Wrapper for use as a DLL
  Last edited 15.02.99

==============================================================================*/

#ifdef WIN32

#include <stdio.h>
#include <windows.h>
#include <dsound.h>
#include <mikmod.h>
#include <mikmod_internals.h>
#include <mikwin.h>

/* Variable export/import */

void set_MikMod_errno(int errno) { MikMod_errno = errno; }
int get_MikMod_errno(void) { return MikMod_errno; }

void set_md_volume(UBYTE vol) { md_volume = vol; }
UBYTE get_md_volume(void) { return md_volume; }

void set_md_musicvolume(UBYTE vol) { md_musicvolume = vol; }
UBYTE get_md_musicvolume(void) { return md_musicvolume; }

void set_md_sndfxvolume(UBYTE vol) { md_sndfxvolume = vol; }
UBYTE get_md_sndfxvolume(void) { return md_sndfxvolume; }

void set_md_reverb(UBYTE rev) { md_reverb = rev; }
UBYTE get_md_reverb(void) { return md_reverb; }

void set_md_pansep(UBYTE pan) { md_pansep = pan; }
UBYTE get_md_pansep(void) { return md_pansep; }

void set_md_device(UWORD dev) { md_device = dev; }
UWORD get_md_device(void) { return md_device; }

void set_md_mixfreq(UWORD freq) { md_mixfreq = freq; }
UWORD get_md_mixfreq(void) { return md_mixfreq; }

void set_md_mode(UWORD mode) { md_mode = mode; }
UWORD get_md_mode(void) { return md_mode; }

MDRIVER get_drv_nos(void) { return drv_nos; }
MDRIVER get_drv_ds_raw(void) { return drv_ds_raw; }

/* Threaded player */

extern int dsbuffersize;
extern BOOL DS_Init_Notify(void);
extern void DS_Close_Notify(void);
extern void DS_Update_Notify(void);
extern LPDIRECTSOUNDBUFFER lpdsb;
HANDLE hThread;

DWORD WINAPI PlayerThread_Start(LPVOID lpParameter) {
	if (DS_Init_Notify()) {
		DS_Update_Notify();
	} else {
		lpdsb->lpVtbl->SetCurrentPosition(lpdsb, 0);
		MikMod_Update();
		while(Player_Active()) {
			MikMod_Update();
			Sleep(dsbuffersize / 8);
		}
	}
	return 0;
}

void Player_Start_threaded(MODULE *module)
{
	DWORD dwThreadID;
	Player_Start(module);
	hThread = CreateThread(0, 0, PlayerThread_Start, NULL, 0, &dwThreadID);
}

void Player_Stop_threaded()
{
	lpdsb->lpVtbl->Stop(lpdsb);
	DS_Close_Notify();
	TerminateThread(hThread, 0);
    Player_Stop();
}

/****************************************/
/*      Simple player interface        */
/****************************************/

MODULE * mikwin_module = 0;

BOOL IsDS() { return ((int)md_driver == (int)&drv_ds_raw); }

/* Init the soundsystem */
BOOL MikWin_Init(UWORD mixfreq, BOOL stereo, BOOL bits16, BOOL interpolation,
				 HWND wnd, int buffersize)
{
	UWORD options;
	/* register all the drivers */
	MikMod_RegisterAllDrivers();

    /* register all the module loaders */
	MikMod_RegisterAllLoaders();

	/* initialize the library */
	if (mixfreq == 0) mixfreq = DEFAULT_MIXFREQ;
	set_md_mixfreq(mixfreq);		
	options = DMODE_SOFT_MUSIC;
	if (stereo) options |= DMODE_STEREO;
	if (bits16) options |= DMODE_16BITS;
	if (interpolation) options |= DMODE_INTERP;
	set_md_mode(options);
    
	mikwin_module = 0;
	set_ds_hwnd(wnd);
	set_ds_buffersize(buffersize);

	return !MikMod_Init();
}

/* Free all stuff */
void MikWin_Free()
{
	if (mikwin_module) {
		Player_Stop();
		Player_Free(mikwin_module);
	}
	MikMod_Exit();
	mikwin_module = 0;
}

/* Load a module */
BOOL MikWin_Load(char * filename)
{
	return MikWin_LoadPos(filename, 0);
}

/* Load the module from a certain position (for WADs) */
BOOL MikWin_LoadPos(char * filename, int position)
{
	FILE *fp;
	if (mikwin_module) {
		Player_Stop();
		Player_Free(mikwin_module);
	}
	if((fp=_mm_fopen(filename,"rb"))) {
		fseek(fp, position, SEEK_SET);
		mikwin_module=Player_LoadFP(fp, 32, 0);
		fclose(fp);
	}
	return (BOOL)mikwin_module;
}

/* Play - if loop is set the module will restart when finished */
void MikWin_Play(BOOL loop)
{
	if (mikwin_module) {
		mikwin_module->wrap = loop;
		if (IsDS()) lpdsb->lpVtbl->SetCurrentPosition(lpdsb, 0);
		Player_Start(mikwin_module);
		MikMod_Update();
	}
}

/* Stop and return to the MODs beginning */
void MikWin_Stop()
{
	if (mikwin_module) {
		if (IsDS())	lpdsb->lpVtbl->Stop(lpdsb);
		Player_SetPosition(0);
		Player_Stop();
	}
}

/* Toggle pause */
void MikWin_Pause()
{
	if (mikwin_module) {
		Player_TogglePause();	
		if (IsDS()) {
			if (Player_Paused()) lpdsb->lpVtbl->Stop(lpdsb);
				else lpdsb->lpVtbl->Play(lpdsb, 0, 0, DSBPLAY_LOOPING);
		}
	}
}

/* Is output currently paused? */
BOOL MikWin_Paused() {	return Player_Paused(); }

/* Calculate music */
void MikWin_Update()
{
	if (mikwin_module && MikWin_Playing()) MikMod_Update();
}

extern get_ds_active();

/* Is the player playing? */
BOOL MikWin_Playing()
{
	if (IsDS()) {
		if (Player_Active()) return TRUE;
		if (!get_ds_active()) MikWin_Stop();
		return get_ds_active();
	} else return Player_Active();
}

/* Returns the module */
MODULE * MikWin_GetModule()
{
	return mikwin_module;
}

/* Returns error information */
char * MikWin_GetErrorText() 
{
	return MikMod_strerror(get_MikMod_errno());
}

#endif /* WIN32 */