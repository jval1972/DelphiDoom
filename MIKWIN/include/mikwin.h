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

  MikMod for Windows include file
  Copyright (C) 1999 by Jörg Mensmann <joerg.mensmann@gmx.net>

  Last edited 14.02.99

==============================================================================*/

#ifndef _MIKWIN_H_
#define _MIKWIN_H_

#ifdef WIN32

#define DEFAULT_MIXFREQ 22500

/* Threaded playing routines */
extern void Player_Start_threaded(MODULE *module);
extern void Player_Stop_threaded(void);

/* functions for exporting variables through a DLL */
extern void set_MikMod_errno(int errno);
extern int get_MikMod_errno(void);
extern void set_md_volume(UBYTE vol);
extern UBYTE get_md_volume(void);
extern void set_md_musicvolume(UBYTE vol);
extern UBYTE get_md_musicvolume(void);
extern void set_md_sndfxvolume(UBYTE vol);
extern UBYTE get_md_sndfxvolume(void);
extern void set_md_reverb(UBYTE rev);
extern UBYTE get_md_reverb(void);
extern void set_md_pansep(UBYTE pan);
extern UBYTE get_md_pansep(void);
extern void set_md_device(UWORD dev);
extern UWORD get_md_device(void);
extern void set_md_mixfreq(UWORD freq);
extern UWORD get_md_mixfreq(void);
extern void set_md_mode(UWORD mode);
extern UWORD get_md_mode(void);
extern MDRIVER get_drv_nos(void);
extern MDRIVER get_drv_ds_raw(void);

#include <windows.h>
#include <mmsystem.h>
#include <dsound.h>
extern void set_ds(LPDIRECTSOUND ds);
extern void set_dsbprimary(LPDIRECTSOUNDBUFFER b);
extern void set_ds_hwnd(HWND wnd);
extern void set_ds_buffersize(int size);
extern LPDIRECTSOUNDBUFFER get_ds_dsbstream(void);

BOOL MikWin_Init(UWORD mixfreq, BOOL stereo, BOOL bits16, BOOL interpolation,
				 HWND wnd, int buffersize);
void MikWin_Free();
BOOL MikWin_Load(char * filename);
BOOL MikWin_LoadPos(char * filename, int position);
void MikWin_Play(BOOL loop);
void MikWin_Stop();
void MikWin_Pause();
BOOL MikWin_Paused();
void MikWin_Update();
BOOL MikWin_Playing();
MODULE * MikWin_GetModule();
char * MikWin_GetErrorText();

#endif /* WIN32 */

#endif /* _MIKWIN_H */

