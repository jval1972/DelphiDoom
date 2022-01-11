(*	MikMod sound library
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
*)

(*==============================================================================

  MikMod sound library import unit

  Translated by Jörg Mensmann 09.01.99 last edited 15.02.99

==============================================================================*)

unit mikmod;

interface

uses Windows, DirectX;

(*
 *	========== Library version
 *)
const
     LIBMIKMOD_VERSION_MAJOR = 3;
     LIBMIKMOD_VERSION_MINOR = 1;
     LIBMIKMOD_REVISION      = 5;

     LIBMIKMOD_VERSION = (LIBMIKMOD_VERSION_MAJOR shl 16) or
                         (LIBMIKMOD_VERSION_MINOR shl 8) or
                         (LIBMIKMOD_REVISION);


function MikMod_GetVersion : LongInt; cdecl;

(*
 *	========== Platform independent-type definitions
 *)
type
   SBYTE = ShortInt;        // 1 byte, signed
   UBYTE = Byte;            // 1 byte, unsigned
   SWORD = SmallInt;        // 2 bytes, signed
   UWORD = Word;            // 2 bytes, unsigned
   SLONG = LongInt;         // 4 bytes, signed
   ULONG = LongInt;         // 4 bytes, unsigned
   BOOL = LongBool;         // 0=false, <>0 true

(*
 *	========== Error codes
 *)

type
    mmErrors  = (
        MMERR_EMPTY,
	MMERR_OPENING_FILE ,
	MMERR_OUT_OF_MEMORY,

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
	MMERR_NON_BLOCK,

	MMERR_AF_AUDIO_PORT,

	MMERR_AIX_CONFIG_INIT,
	MMERR_AIX_CONFIG_CONTROL,
	MMERR_AIX_CONFIG_START,

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
);

(*
 *	========== Error handling
 *)

function MikMod_strerror(err : Integer) : PChar; cdecl;

(*
 *	========== Library initialization and core functions
 *)

procedure MikMod_RegisterAllDrivers; cdecl;

function MikMod_InfoDriver : PChar; cdecl;

function MikMod_Init : BOOL; cdecl;
procedure MikMod_Exit; cdecl;
function MikMod_Reset : BOOL; cdecl;
function MikMod_SetNumVoices(a, b : Integer) : BOOL; cdecl;
function MikMod_Active : BOOL; cdecl;
function MikMod_EnableOutput : BOOL; cdecl;
procedure MikMod_DisableOutput; cdecl;
procedure MikMod_Update; cdecl;

(*
 *	========== Samples
 *)

const
// Sample playback should not be interrupted
SFX_CRITICAL = 1;

// Sample format [loading and in-memory] flags:
SF_16BITS =     $0001;
SF_STEREO =     $0002;
SF_SIGNED =     $0004;
SF_BIG_ENDIAN = $0008;
SF_DELTA =      $0010;
SF_ITPACKED =	$0020;

SF_FORMATMASK = $003F;

// General Playback flags

SF_LOOP     =   $0040;
SF_BIDI     =   $0080;
SF_REVERSE  =   $0100;
SF_SUSTAIN  =   $0200;

SF_PLAYBACKMASK	= $03C0;

// Module-only Playback Flags

SF_OWNPAN	 =      $0400;
SF_UST_LOOP      =      $0800;

SF_EXTRAPLAYBACKMASK =  $0C00;

// Panning constants
PAN_LEFT     =   0;
PAN_CENTER   = 128;
PAN_RIGHT    = 255;
PAN_SURROUND = 512;  // panning value for Dolby Surround

(*
 *	========== Module loaders
 *)

function MikMod_InfoLoader : PChar; cdecl;
procedure MikMod_RegisterAllLoaders; cdecl;

(*
 *	========== Module player
 *)
type
  PMODULE = Pointer;

function Player_LoadFP(f : integer; MaxVoices : Integer; b : BOOL) : PMODULE; cdecl;
function Player_Load(Filename : PChar; MaxVoices : Integer; b : BOOL) : PMODULE; cdecl;
function Player_LoadTitle(Title : PChar) : PChar; cdecl;
procedure Player_Free(module : PMODULE); cdecl;
procedure Player_Start(module : PMODULE); cdecl;
function Player_Active : BOOL; cdecl;
procedure Player_Stop; cdecl;
procedure Player_TogglePause; cdecl;
function Player_Paused : BOOL; cdecl;
function Player_Muted(u : UBYTE) : BOOL; cdecl;
procedure Player_SetVolume(vol : SWORD); cdecl;
function Player_GetModule : PMODULE; cdecl;
procedure Player_Unmute(s : SLONG); cdecl;
procedure Player_Mute(s : SLONG); cdecl;
procedure Player_ToggleMute(s : SLONG); cdecl;

const
     MUTE_EXCLUSIVE = 32000;
     MUTE_INCLUSIVE = 32001;

(*
 *	========== Drivers
 *)

const
	MD_MUSIC = 0;
	MD_SNDFX = 1;

const
	MD_HARDWARE = 0;
	MD_SOFTWARE = 1;

// Mixing flags

(* These ones take effect only after MikMod_Init or MikMod_Reset *)
DMODE_16BITS     = 1;(* enable 16 bit output *)
DMODE_STEREO     = 2;(* enable stereo output *)
DMODE_SOFT_SNDFX = 4;(* Process sound effects via software mixer *)
DMODE_SOFT_MUSIC = 8;(* Process music via software mixer *)
DMODE_HQMIXER    = 16;(* Use high-quality (slower) software mixer *)
(* These take effect immediately. *)
DMODE_SURROUND  = 16;(* enable surround sound *)
DMODE_INTERP    = 32;(* enable interpolation *)
DMODE_REVERSE   = 64;(* reverse stereo *)

(* Threaded playing routines *)
procedure Player_Start_threaded(module : PMODULE); cdecl;

(* functions for exporting variables through a DLL *)
procedure set_MikMod_errno(errno : Integer); cdecl;
function get_MikMod_errno : Integer; cdecl;
procedure set_md_volume(vol : UBYTE); cdecl;
function get_md_volume : UBYTE; cdecl;
procedure set_md_musicvolume(vol : UBYTE); cdecl;
function get_md_musicvolume : UBYTE; cdecl;
procedure set_md_sndfxvolume(vol : UBYTE); cdecl;
function get_md_sndfxvolume : UBYTE; cdecl;
procedure set_md_reverb(rev : UBYTE); cdecl;
function get_md_reverb : UBYTE; cdecl;
procedure set_md_pansep(pan : UBYTE); cdecl;
function get_md_pansep : UBYTE; cdecl;
procedure set_md_device(dev : UWORD); cdecl;
function get_md_device : UWORD; cdecl;
procedure set_md_mixfreq(freq : UWORD); cdecl;
function get_md_mixfreq : UWORD; cdecl;
procedure set_md_mode(mode : UWORD); cdecl;
function get_md_mode : UWORD; cdecl;

type
  LPDIRECTSOUND = ^IDIRECTSOUND;
  LPDIRECTSOUNDBUFFER = ^IDIRECTSOUNDBUFFER;

procedure set_ds(ds : LPDIRECTSOUND); cdecl;
procedure set_dsbprimary(b : LPDIRECTSOUNDBUFFER); cdecl;
procedure set_ds_hwnd(wnd : HWND); cdecl;
procedure set_ds_buffersize(size : Integer); cdecl;

function MikWin_Init(mixfreq : UWORD; stereo, bits16, interpolation : BOOL;
                     wnd : THandle; buffersize : Integer) : BOOL; cdecl;
procedure MikWin_Free; cdecl;
function MikWin_Load(filename : PChar) : BOOL; cdecl;
procedure MikWin_Play(loop : BOOL); cdecl;
procedure MikWin_Stop; cdecl;
procedure MikWin_Pause; cdecl;
function MikWin_Paused : BOOL; cdecl;
procedure MikWin_Update; cdecl;
function MikWin_Playing : BOOL; cdecl;
function MikWin_GetModule : PMODULE; cdecl;
function MikWin_GetErrorText : PChar; cdecl;

implementation

const
  MikDll = 'mikwin.dll';

function MikMod_GetVersion : LongInt; external MikDll;
function MikMod_strerror(err : Integer) : PChar; external MikDll;
procedure MikMod_RegisterAllDrivers; external MikDll;
function MikMod_InfoDriver : PChar; external MikDll;

function MikMod_Init : BOOL; external MikDll;
procedure MikMod_Exit; external MikDll;
function MikMod_Reset : BOOL; external MikDll;
function MikMod_SetNumVoices(a, b : Integer) : BOOL; external MikDll;
function MikMod_Active : BOOL; external MikDll;
function MikMod_EnableOutput : BOOL; external MikDll;
procedure MikMod_DisableOutput; external MikDll;
procedure MikMod_Update; external MikDll;

function MikMod_InfoLoader : PChar; external MikDll;
procedure MikMod_RegisterAllLoaders; external MikDll;

function Player_LoadFP(f : integer; MaxVoices : Integer; b : BOOL) : PMODULE;
 external MikDll;
function Player_Load(Filename : PChar; MaxVoices : Integer; b : BOOL) : PMODULE;
 external MikDll;
function Player_LoadTitle(Title : PChar) : PChar; external MikDll;
procedure Player_Free(module : PMODULE); external MikDll;
procedure Player_Start(module : PMODULE); external MikDll;
function Player_Active : BOOL; external MikDll;
procedure Player_Stop; external MikDll;
procedure Player_TogglePause; external MikDll;
function Player_Paused : BOOL; external MikDll;
function Player_Muted(u : UBYTE) : BOOL; external MikDll;
procedure Player_SetVolume(vol : SWORD); external MikDll;
function Player_GetModule : PMODULE; external MikDll;
procedure Player_Unmute(s : SLONG); external MikDll;
procedure Player_Mute(s : SLONG); external MikDll;
procedure Player_ToggleMute(s : SLONG); external MikDll;

procedure Player_Start_threaded(module : PMODULE); external MikDll;

procedure set_MikMod_errno(errno : Integer); external MikDll;
function get_MikMod_errno : Integer; external MikDll;
procedure set_md_volume(vol : UBYTE); external MikDll;
function get_md_volume : UBYTE; external MikDll;
procedure set_md_musicvolume(vol : UBYTE); external MikDll;
function get_md_musicvolume : UBYTE; external MikDll;
procedure set_md_sndfxvolume(vol : UBYTE); external MikDll;
function get_md_sndfxvolume : UBYTE; external MikDll;
procedure set_md_reverb(rev : UBYTE); external MikDll;
function get_md_reverb : UBYTE; external MikDll;
procedure set_md_pansep(pan : UBYTE); external MikDll;
function get_md_pansep : UBYTE; external MikDll;
procedure set_md_device(dev : UWORD); external MikDll;
function get_md_device : UWORD; external MikDll;
procedure set_md_mixfreq(freq : UWORD); external MikDll;
function get_md_mixfreq : UWORD; external MikDll;
procedure set_md_mode(mode : UWORD); external MikDll;
function get_md_mode : UWORD; external MikDll;

procedure set_ds(ds : LPDIRECTSOUND); external MikDll;
procedure set_dsbprimary(b : LPDIRECTSOUNDBUFFER); external MikDll;
procedure set_ds_hwnd(wnd : HWND); external MikDll;
procedure set_ds_buffersize(size : Integer); external MikDll;

function MikWin_Init(mixfreq : UWORD; stereo, bits16, interpolation : BOOL;
                     wnd : THandle; buffersize : Integer) : BOOL; external MikDll;
procedure MikWin_Free; external MikDll;
function MikWin_Load(filename : PChar) : BOOL; external MikDll;
procedure MikWin_Play(loop : BOOL); external MikDll;
procedure MikWin_Stop; external MikDll;
procedure MikWin_Pause; external MikDll;
function MikWin_Paused : BOOL; external MikDll;
procedure MikWin_Update; external MikDll;
function MikWin_Playing : BOOL; external MikDll;
function MikWin_GetModule : PMODULE; external MikDll;
function MikWin_GetErrorText : PChar; external MikDll;

end.
