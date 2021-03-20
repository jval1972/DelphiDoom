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

  Mikmod driver for DirectSound with software mixing  
  Last edited 15.02.99

==============================================================================*/

#define INITGUID
#include <windows.h>
#include <dsound.h>
#include <mikmod.h>
#include <mikmod_internals.h>

#define DEFAULT_DSBUFFERSIZE 2000

/*===== \begin{Configurable stuff} ======*/

LPDIRECTSOUND        lpds = 0;			    /* main DSound-object ========*/
LPDIRECTSOUNDBUFFER  lpdsbprimary = 0;		/* primary DSound-buffer =====*/
/* If your program already has its own DirectSound-object then you =======*/
/* should set it here to prevent creation of another one. ================*/

HWND	hwnd = 0;							/* window handle =============*/
/* If not set, the foreground window will be used which could make some ==*/
/* trouble - so ALWAYS set this to your applications window-handle!! =====*/

int		dsbuffersize = DEFAULT_DSBUFFERSIZE;/* size of sound buffer (ms) =*/
/* Actually only half of this is used as as buffer (double-buffering) ====*/
/* A background player should use a high value here to prevent buffer ====*/
/* underflows while a mod player should use lower settings for a faster ==*/
/* response to track-changing by the user. ===============================*/

/*===== \end{Configurable stuff} ======*/

LPDIRECTSOUNDBUFFER  lpdsb;					/* streaming DSound-buffer ===*/
DWORD				 dwMidBuffer;			/* middle of sound buffer ====*/
BOOL				 FirstRun = TRUE;		/* fill buffers immediately ==*/
BOOL				 HasNotify = FALSE;     /* Notification available? ===*/
LPDIRECTSOUNDNOTIFY	 lpdsNotify;			/* Notify interface ==========*/
int					 playingFinished = 0;	/* >= 2: playing is finished =*/

void set_ds(LPDIRECTSOUND ds) { lpds = ds; }
void set_dsbprimary(LPDIRECTSOUNDBUFFER b) { lpdsbprimary = b; }
void set_ds_hwnd(HWND wnd) { hwnd = wnd; }
void set_ds_buffersize(int size) { if (size <= 0) dsbuffersize = DEFAULT_DSBUFFERSIZE; 
                                     else dsbuffersize = size; }
LPDIRECTSOUNDBUFFER get_ds_dsbstream(void) { return lpdsb; }
BOOL get_ds_active() { return playingFinished < 2; }

BOOL FillBufferWithSilence(LPDIRECTSOUNDBUFFER lpDsb)
{
    WAVEFORMATEX    wfx;
    DWORD           dwSizeWritten;
    PBYTE   pb1;
    DWORD   cb1;

    if FAILED(lpDsb->lpVtbl->GetFormat(lpDsb, &wfx, sizeof(WAVEFORMATEX), &dwSizeWritten))
        return FALSE;

    if SUCCEEDED(lpDsb->lpVtbl->Lock(lpDsb, 0, 0, 
                         (LPVOID *)&pb1, &cb1, 
                         NULL, NULL, DSBLOCK_ENTIREBUFFER))
    {
        FillMemory(pb1, cb1, (wfx.wBitsPerSample == 8) ? 128 : 0);
        lpDsb->lpVtbl->Unlock(lpDsb, pb1, cb1, NULL, 0);
        return TRUE;
    }
    return FALSE;
}  

void FillMusicBuffer(DWORD dwStartOfs) 
{
	VOID            *lpvData;
    DWORD           dwBytesLocked;

	if (!Player_Active() && playingFinished < 2) playingFinished++; 
	if FAILED(lpdsb->lpVtbl->Lock( lpdsb,
	          dwStartOfs,	     /* Start = 0 oder middle of buffer =*/
              dwMidBuffer,       /* size = half of the buffer =======*/
              &lpvData,          /* pointer to the locked buffer ====*/
              &dwBytesLocked,    /* bytes to be locked ==============*/
              NULL, NULL, 0)) return;
	if (VC_WriteBytes((SBYTE*)lpvData, dwBytesLocked) == 0) 
		ZeroMemory((SBYTE*)lpvData, dwBytesLocked);
    lpdsb->lpVtbl->Unlock(lpdsb, lpvData, dwBytesLocked, NULL, 0);
}

BOOL DS_Init(void)
{
    HRESULT				hr;
    DSBUFFERDESC        dsbd;
    WAVEFORMATEX        wfm;

    if (hwnd == 0) hwnd = GetForegroundWindow();  /* not such a good way! */
    
	if (lpds == 0) {
		/* Create DirectSound object */
		if FAILED(hr = DirectSoundCreate(NULL, &lpds, NULL)) return FALSE;

	    if FAILED(hr = lpds->lpVtbl->SetCooperativeLevel(lpds, hwnd, DSSCL_PRIORITY))
		    return FALSE;
	}

    /* Set buffer format. */
	memset(&wfm, 0, sizeof(WAVEFORMATEX)); 
    wfm.wFormatTag = WAVE_FORMAT_PCM; 
    wfm.nChannels = (md_mode & DMODE_STEREO) ? 2 : 1; 
    wfm.nSamplesPerSec = md_mixfreq; 
	wfm.wBitsPerSample = (md_mode & DMODE_16BITS) ? 16 : 8; 
    wfm.nBlockAlign = wfm.wBitsPerSample / 8 * wfm.nChannels;
    wfm.nAvgBytesPerSec = wfm.nSamplesPerSec * wfm.nBlockAlign;
	
    if (lpdsbprimary == 0) {
		/* Create primary buffer */
		memset(&dsbd, 0, sizeof(DSBUFFERDESC)); 
		dsbd.dwSize = sizeof(DSBUFFERDESC); 
	    dsbd.dwFlags = DSBCAPS_PRIMARYBUFFER;
	    dsbd.dwBufferBytes = 0;  
		dsbd.lpwfxFormat = NULL; 
		if FAILED(hr = lpds->lpVtbl->CreateSoundBuffer(lpds, &dsbd, &lpdsbprimary, NULL)) 
				return FALSE;

		/* Set sound format */
		if FAILED(hr = lpdsbprimary->lpVtbl->SetFormat(lpdsbprimary, &wfm))
			return FALSE;
		/* Start looping (this doesn't play anything as long as no secondary buffers are active */
		/* but will prevent time-consuming DMA reprogramming on some ISA-soundcards.        	*/
		if FAILED(hr = lpdsbprimary->lpVtbl->Play(lpdsbprimary, 0, 0, DSBPLAY_LOOPING)) 
			return FALSE;
	}

	/* Create secondary buffer */
	memset(&dsbd, 0, sizeof(DSBUFFERDESC)); 
    dsbd.dwSize = sizeof(DSBUFFERDESC); 
    dsbd.dwFlags = DSBCAPS_GETCURRENTPOSITION2 | DSBCAPS_GLOBALFOCUS | DSBCAPS_LOCSOFTWARE | 
				   DSBCAPS_CTRLPOSITIONNOTIFY; 
    dsbd.dwBufferBytes = (wfm.nAvgBytesPerSec / 1000) * dsbuffersize;  
    dsbd.lpwfxFormat = &wfm; 
 
    if FAILED(hr = lpds->lpVtbl->CreateSoundBuffer(lpds, &dsbd, &lpdsb, NULL))
	{
		dsbd.dwFlags = dsbd.dwFlags & (!DSBCAPS_CTRLPOSITIONNOTIFY);
		if FAILED(hr = lpds->lpVtbl->CreateSoundBuffer(lpds, &dsbd, &lpdsb, NULL))
			return FALSE; 
	}
	
    dwMidBuffer = dsbd.dwBufferBytes / 2;	

    return VC_Init();
}

void DS_Exit(void)
{    
    if (lpds) lpds->lpVtbl->Release(lpds);		   /* This releases buffers as well. */
	VC_Exit();
}

BOOL DS_IsThere(void)
{
    if SUCCEEDED(DirectSoundCreate(NULL, &lpds, NULL)) {
		lpds->lpVtbl->Release(lpds);
		lpds = 0;
		return TRUE;
    } else return FALSE;
}

BOOL DS_PlayStart(void)
{
	BOOL r = VC_PlayStart();
    lpdsb->lpVtbl->Play(lpdsb, 0, 0, DSBPLAY_LOOPING);
	playingFinished = 0;
	return r;
}

void DS_PlayStop(void)
{
    lpdsb->lpVtbl->Stop(lpdsb);
	lpdsb->lpVtbl->SetCurrentPosition(lpdsb, 0);
	FirstRun = TRUE;
	FillBufferWithSilence(lpdsb);
	playingFinished = 1000;
	VC_PlayStop();
}

void DS_Update(void)
{
	DWORD           dwPlay;
    static DWORD    dwLastPlayPos = 0;
    	
	if (lpdsb == NULL) return;

	if (FirstRun) {
		dwLastPlayPos = 0;
		FillMusicBuffer(0);			    	/* Fill buffer so playing starts immediately */
  		FillMusicBuffer(dwMidBuffer);
		FirstRun = FALSE;
	  	return;
	}
    
	if FAILED(lpdsb->lpVtbl->GetCurrentPosition(lpdsb, &dwPlay, NULL))
		return;

	if (((dwPlay >= dwMidBuffer) && (dwLastPlayPos < dwMidBuffer))
        || (dwPlay < dwLastPlayPos))
    {
        FillMusicBuffer((dwPlay >= dwMidBuffer) ?  0 : dwMidBuffer);
    } 
    dwLastPlayPos = dwPlay;
}

#define NUM_NOTIFICATIONS 3
DSBPOSITIONNOTIFY PositionNotify[NUM_NOTIFICATIONS];
HANDLE hMyEvent[NUM_NOTIFICATIONS + 1];

BOOL DS_Init_Notify(void)
{
	int i;
	HRESULT hr;
	
	HasNotify = FALSE;

    if (FAILED(IDirectSoundNotify_QueryInterface(lpdsb, &IID_IDirectSoundNotify, (VOID**)&lpdsNotify)))
		return FALSE;
 
	for (i=0; i<=NUM_NOTIFICATIONS; i++) {
		hMyEvent[i] = CreateEvent(NULL, FALSE, FALSE, NULL);
		if (hMyEvent[i] == NULL) return FALSE;
	}
	PositionNotify[0].dwOffset = 0;
	PositionNotify[0].hEventNotify = hMyEvent[0];	
	PositionNotify[1].dwOffset = dwMidBuffer;
	PositionNotify[1].hEventNotify = hMyEvent[1];
	PositionNotify[2].dwOffset = DSBPN_OFFSETSTOP;
	PositionNotify[2].hEventNotify = hMyEvent[2];
 
	lpdsb->lpVtbl->Stop(lpdsb);
	if FAILED(hr = lpdsNotify->lpVtbl->SetNotificationPositions(lpdsNotify, NUM_NOTIFICATIONS, 
		PositionNotify)) 
	{          
		lpdsNotify->lpVtbl->Release(lpdsNotify);
	    lpdsb->lpVtbl->Play(lpdsb, 0, 0, DSBPLAY_LOOPING);
		return FALSE;
	} 

    lpdsb->lpVtbl->Play(lpdsb, 0, 0, DSBPLAY_LOOPING);

	HasNotify = TRUE;
	return HasNotify;
}

void DS_Close_Notify(void)
{
	int i;
	if (!HasNotify) return;

	if (lpdsNotify != 0) {
		lpdsNotify->lpVtbl->Release(lpdsNotify);
		lpdsNotify = 0;
	}
	for (i=0; i<=NUM_NOTIFICATIONS; i++) {
		if (hMyEvent[i] != NULL) CloseHandle(hMyEvent[i]);
		hMyEvent[i] = NULL;
	}
}

void DS_Update_Notify(void) 
{
	if (!HasNotify) return;

	lpdsb->lpVtbl->Stop(lpdsb);
	lpdsb->lpVtbl->SetCurrentPosition(lpdsb, 0);
	DS_Update();
	lpdsb->lpVtbl->Play(lpdsb, 0, 0, DSBPLAY_LOOPING);
	while (1) {
		DWORD dwEvt = MsgWaitForMultipleObjects(NUM_NOTIFICATIONS + 1, 
												hMyEvent, FALSE, INFINITE, QS_ALLINPUT);
		dwEvt -= WAIT_OBJECT_0;
		if (dwEvt < NUM_NOTIFICATIONS)
		{
			DS_Update();
		}
		else if (dwEvt == NUM_NOTIFICATIONS)
		{
			DS_Close_Notify();
			return;
		}
	}
}

BOOL DS_Reset(void)
{
	/* can't change mixfreq at runtime (yet!) */
	return 1;
}

MDRIVER drv_ds_raw = {
        NULL,
        "DirectSound Raw",
        "DirectSound Raw Driver v1.1 - by Jörg Mensmann <joerg.mensmann@gmx.net>",
		0, 255,
        DS_IsThere,
        VC_SampleLoad,
        VC_SampleUnload,
		VC_SampleSpace,
		VC_SampleLength,
        DS_Init,
        DS_Exit,
     	DS_Reset,
		VC_SetNumVoices,
        DS_PlayStart,
        DS_PlayStop,
        DS_Update,
		NULL,
        VC_VoiceSetVolume,
		VC_VoiceSetFrequency,
        VC_VoiceSetPanning,
        VC_VoicePlay,
		VC_VoiceStop,
		VC_VoiceStopped,
		VC_VoiceGetPosition,
		VC_VoiceRealVolume
};

