//------------------------------------------------------------------------------
//
//  DelphiDoom is a source port of the game Doom and it is
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2022 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//  DESCRIPTION:
//   System interface, sound.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit i_sound;

interface

uses
  sounddata;

//==============================================================================
// I_InitSound
//
// Init at program start...
//
//==============================================================================
procedure I_InitSound;

//==============================================================================
// I_ShutDownSound
//
// ... shut down and relase at program termination.
//
//==============================================================================
procedure I_ShutDownSound;

//==============================================================================
// I_SetChannels
//
//  SFX I/O
//
// Initialize channels?
//
//==============================================================================
procedure I_SetChannels;

//==============================================================================
// I_GetSfxLumpNum
//
// Get raw data lump index for sound descriptor.
//
//==============================================================================
function I_GetSfxLumpNum(sfxinfo: Psfxinfo_t): integer;

//==============================================================================
// I_StartSound
//
// Starts a sound in a particular sound channel.
//
//==============================================================================
function I_StartSound(id: integer; vol: integer; sep: integer;
  pitch: integer; priority: integer): integer;

//==============================================================================
// I_StopSound
//
// Stops a sound channel.
//
//==============================================================================
procedure I_StopSound(handle: integer);

//==============================================================================
// I_SoundIsPlaying
//
// Called by S_*() functions
//  to see if a channel is still playing.
// Returns 0 if no longer playing, 1 if playing.
//
//==============================================================================
function I_SoundIsPlaying(handle: integer): boolean;

//==============================================================================
// I_UpdateSoundParams
//
// Updates the volume, separation,
//  and pitch of a sound channel.
//
//==============================================================================
procedure I_UpdateSoundParams(handle: integer; vol: integer; sep: integer;
  pitch: integer);

var
  useexternalwav: boolean;
  preferewavnamesingamedirectory: boolean;

//==============================================================================
//
// I_SetUseExternalWav
//
//==============================================================================
procedure I_SetUseExternalWav(const newu: boolean);

implementation

uses
  d_delphi,
{$IFNDEF DLL}
  audiolib, // JVAL: 20210117 - Search for various sound formats
{$ENDIF}
  MMSystem,
  z_zone,
  m_argv,
  i_system,
  i_mainwindow,
  DirectX,
  s_sound,
  w_wad,
  w_folders,
  w_pak,
  doomdef;

// The number of internal mixing channels,
//  the samples calculated for each mixing step,
//  the size of the 16bit, 2 hardware channel (stereo)
//  mixing buffer, and the samplerate of the raw data.

// Needed for calling the actual sound output.
const
  NUM_CHANNELS = 32;

var
  pDS: IDirectSound;
  pDSBPrimary: IDirectSoundBuffer;
  HandleCount: integer;

  SampleFormat: TWAVEFORMATEX;

type
  wavestatus_t = (ws_nowave, ws_internalwave, ws_externalwave, ws_wavefailed);

  soundparam_t = record
    length: integer;
    offset: integer;
    wavformat: word;
    freq: LongWord;
    avgfreq: LongWord;
    samples: byte;
    channels: byte;
    wavestatus: wavestatus_t;
  end;
  Psoundparam_t = ^soundparam_t;

  soundparam_tArray = array[0..$FFFF] of soundparam_t;
  Psoundparam_tArray = ^soundparam_tArray;

var
  soundparams: Psoundparam_tArray = nil;
  numsoundparams: integer = 0;

//==============================================================================
//
// GetSoundParam
//
//==============================================================================
function GetSoundParam(id: integer): Psoundparam_t;
var
  oldsize: integer;
begin
  if id >= numsoundparams then
  begin
    oldsize := numsoundparams * SizeOf(soundparam_t);
    numsoundparams := id + 1;
    realloc(pointer(soundparams), oldsize, numsoundparams * SizeOf(soundparam_t));
    soundparams[id].wavestatus := ws_nowave;
  end;
  result := @soundparams[id];
end;

type
  soundheader_t = record
    filler1: word;
    freq: word;
  end;
  Psoundheader_t = ^soundheader_t;

var
// The sound in channel handles,
//  determined on registration,
//  might be used to unregister/stop/modify,
//  currently unused.
  channelhandles: array[0..NUM_CHANNELS - 1] of integer;

// SFX id of the playing sound effect.
// Used to catch duplicates (like chainsaw).
  channelids: array[0..NUM_CHANNELS - 1] of integer;

//actual data buffers
  ChannelBuffers: array[0..NUM_CHANNELS - 1] of IDirectSoundBuffer;
  ChannelActive: packed array[0..NUM_CHANNELS - 1] of boolean;

//==============================================================================
// I_GetSfxLumpNum
//
// Retrieve the raw data lump index
//  for a given SFX name.
//
//==============================================================================
function I_GetSfxLumpNum(sfxinfo: Psfxinfo_t): integer;
var
  namebuf: string;
begin
{$IFNDEF HEXEN}
  // JVAL: 20171216 - Fix links
  if sfxinfo.link <> nil then
    sfxinfo := sfxinfo.link;
{$ENDIF}

  result := -1;

  sprintf(namebuf, 'ds%s', [sfxinfo.name]);
  if Length(namebuf) <= 8 then
    result := W_CheckNumForName(namebuf);

  if result = -1 then // JVAL, search without the ds prefix
    result := W_CheckNumForName(sfxinfo.name);
end;

// This function loads the sound data from the WAD lump,
//  for single sound.
//
const
  CS_RIFF = $46464952;  // RIFF in HEX
  CS_WAVE = $45564157;  // WAVE in HEX
  CS_fmt  = $20746D66;  // fmt' ' in HEX
  CS_data = $61746164;  // data in HEX

{$IFNDEF DLL}
const
  CS_fLac = 1130450022; // fLac
  CS_OggS = 1399285583; // OggS
{$ENDIF}

//==============================================================================
//
// I_CacheSFX
//
//==============================================================================
procedure I_CacheSFX(const sfxid: integer);
var
  name: string;
  sfx: Psfxinfo_t;
  lump: integer;
  strm: TDStream;
  wavfilename: string;
  externalwavfilenames: TDStringList;
  foundwav: boolean;
  wavformat: TWAVEFORMATEX;
  i: integer;
  l: LongWord;
  len: integer;
  datalen: integer;
  dwtype: LongWord;
  dwlen: integer;
  donefmt, donedata: boolean;
  PLData, PLData2: PLongWordArray;
  plwhat: integer;
  sparm: Psoundparam_t;
begin
  sfx := @S_sfx[sfxid];
  if sfx.data <> nil then
    exit;

  sparm := GetSoundParam(sfxid);

  strm := nil;
  foundwav := false;

  if useexternalwav and (sparm.wavestatus <> ws_wavefailed) then
  begin
    // JVAL: 20210117 - Search for various sound formats
{$IFNDEF DLL}
    foundwav := Audiolib_SearchSoundPAK(sfx.name, strm, 0);

    if not foundwav then
{$ENDIF}
    begin
      // JVAL: Create a list with external wav filenames to check
      externalwavfilenames := TDStringList.Create;
      externalwavfilenames.Add('ds%s.wav', [sfx.name]);
      externalwavfilenames.Add('%s.wav', [sfx.name]);

      for i := 0 to externalwavfilenames.Count - 1 do
      begin
        wavfilename := externalwavfilenames[i];
        if preferewavnamesingamedirectory then
          strm := TPakStream.Create(wavfilename, pm_prefered, gamedirectories)
        else
          strm := TPakStream.Create(wavfilename, pm_short, '', FOLDER_SOUNDS);
        strm.OnBeginBusy := I_BeginDiskBusy;
        foundwav := strm.IOResult = 0;
        if foundwav then
          break
        else
          strm.Free;
      end;

      externalwavfilenames.Free;

    end;
  end;

  datalen := 0;
  donefmt := false;
  donedata := false;
  if foundwav then
  begin
    repeat
      if strm.Size < 8 + SizeOf(TWAVEFORMATEX) then
      begin
        I_Warning('CacheSFX(): Sound %s.wav has invalid size'#13#10, [sfx.name]);
        foundwav := false;
        break;
      end;

      strm.Seek(0, sFromBeginning);
      strm.Read(l, SizeOf(LongWord));
      if l <> CS_RIFF then
      begin
        I_Warning('CacheSFX(): Sound %s.wav has invalid header'#13#10, [sfx.name]);
        foundwav := false;
        break;
      end;

      strm.Read(datalen, SizeOf(integer));
      strm.Read(l, SizeOf(LongWord));
      if l <> CS_WAVE then
      begin
        I_Warning('CacheSFX(): Sound %s.wav has not the WAVE file indicator'#13#10, [sfx.name]);
        foundwav := false;
        break;
      end;

      while strm.Position < strm.Size - 8 do
      begin
        strm.Read(dwtype, SizeOf(LongWord));
        strm.Read(dwlen, SizeOf(LongWord));
        if dwtype = CS_fmt then
        begin
          if (dwlen < SizeOf(TWAVEFORMAT)) and not donefmt then
          begin
            I_Warning('CacheSFX(): Sound %s.wav has invalid fmt CHUNK'#13#10, [sfx.name]);
            foundwav := false;
            break;
          end;
          donefmt := true;
          strm.Read(wavformat, SizeOf(TWAVEFORMATEX));
          if wavformat.wFormatTag <> WAVE_FORMAT_PCM then
          begin
            I_Warning('CacheSFX(): Sound %s.wav is not a WAV file'#13#10, [sfx.name]);
            foundwav := false;
            break;
          end;

          strm.Seek(dwlen - SizeOf(TWAVEFORMATEX), sFromCurrent);
          sparm.wavformat := wavformat.wFormatTag;
          sparm.freq := wavformat.nSamplesPerSec;
          sparm.avgfreq := wavformat.nAvgBytesPerSec;
          sparm.samples := wavformat.wBitsPerSample;
          sparm.channels := wavformat.nChannels;
        end
        else if (dwtype = CS_data) and not donedata then
        begin
          if dwlen > strm.Size - strm.Position then
          begin
            I_Warning('CacheSFX(): Sound %s.wav has invalid data CHUNK'#13#10, [sfx.name]);
            foundwav := false;
            break;
          end;
          donedata := true;
          sparm.length := dwlen;
          sfx.data := Z_Malloc(dwlen, PU_SOUND, @sfx.data);
          strm.Read(sfx.data^, dwlen);
          sparm.offset := 0;
        end
        else
          strm.Seek(-6, sFromCurrent);
        if donefmt and donedata then
        begin
          sparm.wavestatus := ws_externalwave;
          break;
        end;
      end;

    until true;
    strm.Free;
  end;

  if not foundwav then
  begin

    if sfx.lumpnum = -1 then
    begin
      // Get the sound data from the WAD, allocate lump
      //  in zone memory.
      sprintf(name, 'ds%s', [sfx.name]);

      // Now, there is a severe problem with the
      //  sound handling, in it is not (yet/anymore)
      //  gamemode aware. That means, sounds from
      //  DOOM II will be requested even with DOOM
      //  shareware.
      // The sound list is wired into sounds.c,
      //  which sets the external variable.
      // I do not do runtime patches to that
      //  variable. Instead, we will use a
      //  default sound for replacement.
      lump := -1;
      if Length(name) <= 8 then
        lump := W_CheckNumForName(name);

      if lump = -1 then // JVAL Search without the DS prefix
      begin
        name := sfx.name;
        lump := W_CheckNumForName(name);
      end;

      if lump = -1 then
        sfx.lumpnum := W_GetNumForName('dspistol')
      else
        sfx.lumpnum := lump;
    end;

    sfx.data := W_CacheLumpNum(sfx.lumpnum, PU_SOUND);
    len := W_LumpLength(sfx.lumpnum);
    PLData := sfx.data;
{$IFNDEF DLL}
    if (PLData[0] = CS_fLac) or (PLData[0] = CS_OggS) then
      if Audiolib_DecodeSoundWAD(sfx.data, len, @sfx.data, len, 1) then
        PLData := sfx.data;
{$ENDIF}
    PLData2 := PLongWordArray(Integer(PLData) + 2);
    if PLData[0] = CS_RIFF then // WAVE Sound inside WAD as lump
    begin

      repeat
        if len < 8 + SizeOf(TWAVEFORMATEX) then
        begin
          I_Warning('CacheSFX(): Sound %s has invalid size'#13#10, [sfx.name]);
          break;
        end;

        i := 1;
        datalen := PLData[i];
        inc(i);
        if PLData[i] <> CS_WAVE then
        begin
          I_Warning('CacheSFX(): Sound %s has not the WAVE file indicator'#13#10, [sfx.name]);
          break;
        end;

        while i < (len div 4) - 2 do
        begin
          dwtype := PLData[i];
          if (dwtype <> CS_fmt) and (dwtype <> CS_data) then
          begin
            dwtype := PLData2[i];
            dwlen := PLData2[i + 1];
            plwhat := 2;
          end
          else
          begin
            dwlen := PLData[i + 1];
            plwhat := 1;
          end;
          i := i + 2;
          if (dwtype = CS_fmt) and not donefmt then
          begin
            if dwlen < SizeOf(TWAVEFORMAT) then
            begin
              I_Warning('CacheSFX(): Sound %s has invalid fmt CHUNK'#13#10, [sfx.name]);
              break;
            end;
            donefmt := true;
            memcpy(@wavformat, @PLData[i], SizeOf(TWAVEFORMATEX));
            if wavformat.wFormatTag <> WAVE_FORMAT_PCM then
            begin
              I_Warning('CacheSFX(): Sound %s is not a WAV file'#13#10, [sfx.name]);
              break;
            end;

            i := i + dwlen div 4; // JVAL: skip WAVEFORMAT record
            sparm.wavformat := wavformat.wFormatTag;
            sparm.freq := wavformat.nSamplesPerSec;
            sparm.avgfreq := wavformat.nAvgBytesPerSec;
            sparm.samples := wavformat.wBitsPerSample;
            sparm.channels := wavformat.nChannels;
          end
          else if (dwtype = CS_data) and not donedata then
          begin
            if dwlen > len - i * 4 then
            begin
              I_Warning('CacheSFX(): Sound %s has invalid data CHUNK'#13#10, [sfx.name]);
              break;
            end;
            donedata := true;
            sparm.length := dwlen;
            sparm.offSet := i * 4;
            if plwhat = 2 then
              sparm.offset := sparm.offset + 2;
          end
          else
            dec(i);
          if donefmt and donedata then
          begin
            sparm.wavestatus := ws_internalwave;
            break;
          end;
        end;

      until true;

/////////////////////

    end
    else
    begin
      sparm.length := len;
      sparm.wavformat := WAVE_FORMAT_PCM;
      sparm.freq := Psoundheader_t(sfx.data).freq;
      sparm.avgfreq := Psoundheader_t(sfx.data).freq;
      sparm.samples := 8;
      sparm.channels := 1;
      sparm.offset := 8;
    end;
  end;
end;

//==============================================================================
//
// I_SetSfxFormat
//
//==============================================================================
procedure I_SetSfxFormat(const sfxid: integer);
var
  sparm: Psoundparam_t;
begin
  sparm := GetSoundParam(sfxid);
  SampleFormat.wFormatTag := sparm.wavformat;
  SampleFormat.nSamplesPerSec := sparm.freq;
  SampleFormat.nAvgBytesPerSec := sparm.avgfreq;
  SampleFormat.wBitsPerSample := sparm.samples;
  SampleFormat.nChannels := sparm.channels;
end;

//==============================================================================
// I_SetChannels
//
// SFX API
// Note: this was called by S_Init.
// However, whatever they did in the
// old DPMS based DOS version, this
// were simply dummies in the Linux
// version.
// See soundserver initdata().
//
//==============================================================================
procedure I_SetChannels;
begin
end;

//==============================================================================
//
// I_SetSfxVolume
//
//==============================================================================
procedure I_SetSfxVolume(volume: integer);
begin
  // Identical to DOS.
  // Basically, this should propagate
  //  the menu/config file setting
  //  to the state variable used in
  //  the mixing.
  snd_SfxVolume := volume;
end;

//==============================================================================
//
// I_ChannelPlaying
//
//==============================================================================
function I_ChannelPlaying(channel: integer): boolean;
var
  status: LongWord;
begin
  if pDS = nil then
  begin
    result := false;
    exit;
  end;

  if ChannelBuffers[channel] = nil then
  begin
    result := false;
    exit;
  end;

  if not ChannelActive[channel] then
  begin
    result := false;
    exit;
  end;

  ChannelBuffers[channel].GetStatus(status);
  if status and DSBSTATUS_PLAYING <> 0 then
    result := true
  else
  begin
    ChannelActive[channel] := false;
    result := false;
  end;
end;

//==============================================================================
//
// I_KillChannel
//
//==============================================================================
procedure I_KillChannel(channel: integer);
begin
  if pDS <> nil then
  begin
    if ChannelBuffers[channel] <> nil then
    begin
      ChannelBuffers[channel].Stop;
      I_ClearInterface(IInterface(ChannelBuffers[channel]));
    end;
  end;
end;

const
  vulumetrans: array[0..15] of integer = (
      0,  96, 128, 168, 186, 200, 212, 222,
    230, 237, 243, 248, 250, 252, 254, 255
  );

  vulumetransshift = 8;

//==============================================================================
//
// I_SepToDSPan
//
//==============================================================================
function I_SepToDSPan(const sep: integer): integer;
begin
  result := DSBPAN_CENTER +
    (DSBPAN_RIGHT - DSBPAN_LEFT) * (sep * sep - 128 * 128) div
      (16 * 128 * 128);
end;

//==============================================================================
//
// I_VolToDSVol
//
//==============================================================================
function I_VolToDSVol(const vol: integer): integer;
begin
  result := DSBVOLUME_MIN +
    _SHR((DSBVOLUME_MAX - DSBVOLUME_MIN) * (vulumetrans[vol] + 1), vulumetransshift);
end;

//==============================================================================
//
// I_UpdateSoundParams
//
//==============================================================================
procedure I_UpdateSoundParams(handle: integer; vol: integer; sep: integer;
  pitch: integer);
var
  channel: integer;
  dsb: IDirectSoundBuffer;
begin
  if pDS = nil then
    exit;

  for channel := 0 to NUM_CHANNELS - 1 do
  begin
    if I_ChannelPlaying(channel) and (channelhandles[channel] = handle) then
    begin
      dsb := ChannelBuffers[channel];
      dsb.SetPan(I_SepToDSPan(sep));
      dsb.SetVolume(I_VolToDSVol(vol));
      exit;
    end;
  end;
end;

//==============================================================================
//
// I_RestartChannel
//
//==============================================================================
function I_RestartChannel(channel: integer; vol: integer; sep: integer): integer;
var
  dsb: IDirectSoundBuffer;
begin
  if pDS = nil then
  begin
    result := HandleCount;
    inc(HandleCount);
    exit;
  end;

  ChannelActive[channel] := true;
  dsb := ChannelBuffers[channel];
  if dsb = nil then
    I_Error('I_RestartChannel(): Restarting dead sound at channel %d', [channel]);

  dsb.Stop;
  dsb.SetCurrentPosition(0);
  dsb.SetPan(I_SepToDSPan(sep));
  dsb.SetVolume(I_VolToDSVol(vol));
  dsb.Play(0, 0, 0);
  channelhandles[channel] := HandleCount;
  result := HandleCount;
  inc(HandleCount);
end;

//==============================================================================
// I_StartSound
//
// Starting a sound means adding it
//  to the current list of active sounds
//  in the internal channels.
// As the SFX info struct contains
//  e.g. a pointer to the raw data,
//  it is ignored.
// As our sound handling does not handle
//  priority, it is ignored.
// Pitching (that is, increased speed of playback)
//  is set, but currently not used by mixing.
//
//==============================================================================
function I_StartSound(id: integer; vol: integer; sep: integer;
  pitch: integer; priority: integer): integer;
var
  channel: integer;
  dsb: IDirectSoundBuffer;
  hres: HRESULT;
  dsbd: DSBUFFERDESC;
  oldchannel: integer;
  oldhandle: integer;
  freechannel: integer;
  p: pointer;
  p2: pointer;
  s: LongWord;
  s2: LongWord;
  sparm: Psoundparam_t;

  procedure I_ErrorStartSound(const procname: string);
  begin
    I_DevError('I_StartSound(): %s failed, result = %d. Sound id = %d (%s).'#13#10, [procname, hres, id, S_sfx[id].name]);
  end;

begin
  if pDS = nil then
  begin
    result := HandleCount;
    inc(HandleCount);
    exit;
  end;

  oldhandle := 0;
  oldchannel := 0;
  freechannel := NUM_CHANNELS;
  for channel := 0 to NUM_CHANNELS - 1 do
  begin
    if ChannelBuffers[channel] <> nil then
    begin
      if (channelids[channel] = id) and not I_ChannelPlaying(channel) then
      begin
        result := I_RestartChannel(channel, vol, sep);
        exit;
      end;
      if HandleCount - channelhandles[channel] > oldhandle then
      begin
        oldhandle := HandleCount - channelhandles[channel];
        oldchannel := channel;
      end
    end
    else
      freechannel := channel;
  end;

  if freechannel <> 0 then
    channel := freechannel
  else
    channel := oldchannel;
  I_CacheSFX(id);
  I_SetSfxFormat(id);
  ZeroMemory(@dsbd, SizeOf(DSBUFFERDESC));
  dsbd.dwSize := Sizeof(DSBUFFERDESC);
  dsbd.dwFlags := DSBCAPS_CTRLVOLUME or DSBCAPS_CTRLFREQUENCY or
                  DSBCAPS_CTRLPAN or DSBCAPS_GETCURRENTPOSITION2 or DSBCAPS_STATIC;
  sparm := GetSoundParam(id);
  if sparm.length = 0 then
  begin
    I_Warning('I_StartSound(): Sound %d(%s) has zero length.'#13#10, [id, S_sfx[id].name]);
    result := HandleCount;
    exit;
  end;

  dsbd.dwBufferBytes := sparm.length;
  SampleFormat.nBlockAlign := SampleFormat.nChannels * SampleFormat.wBitsPerSample div 8;
  SampleFormat.cbSize := 0;
  dsbd.lpwfxFormat := @SampleFormat;

  hres := pDS.CreateSoundBuffer(dsbd, dsb, nil);
  if hres <> DS_OK then
  begin
    if sparm.wavestatus = ws_externalwave then
    begin
      I_Warning('I_StartSound(): External sound %d caused a problem and will be discarded'#13#10, [id]);
      sparm.wavestatus := ws_wavefailed;
      Z_Free(S_sfx[id].data);
      S_sfx[id].data := nil;
      I_CacheSFX(id);
      I_SetSfxFormat(id);
      ZeroMemory(@dsbd, SizeOf(DSBUFFERDESC));
      dsbd.dwSize := Sizeof(DSBUFFERDESC);
      dsbd.dwFlags := DSBCAPS_CTRLVOLUME or DSBCAPS_CTRLFREQUENCY or
                      DSBCAPS_CTRLPAN or DSBCAPS_GETCURRENTPOSITION2 or DSBCAPS_STATIC;
      dsbd.dwBufferBytes := sparm.length;
      SampleFormat.nBlockAlign := SampleFormat.nChannels * SampleFormat.wBitsPerSample div 8;
      SampleFormat.cbSize := 0;
      dsbd.lpwfxFormat := @SampleFormat;

      hres := pDS.CreateSoundBuffer(dsbd, dsb, nil);
      if hres <> DS_OK then
      begin
        SampleFormat.nAvgBytesPerSec := SampleFormat.nSamplesPerSec * SampleFormat.nChannels * SampleFormat.wBitsPerSample div 8;
        dsbd.lpwfxFormat := @SampleFormat;

        hres := pDS.CreateSoundBuffer(dsbd, dsb, nil);
        if hres <> DS_OK then
        begin
          I_ErrorStartSound('CreateSoundBuffer()');
          result := 0;
          exit;
        end
        else
          sparm.avgfreq := SampleFormat.nAvgBytesPerSec;
      end;

    end
    else
    begin
      SampleFormat.nAvgBytesPerSec := SampleFormat.nSamplesPerSec * SampleFormat.nChannels * SampleFormat.wBitsPerSample div 8;
      dsbd.lpwfxFormat := @SampleFormat;

      hres := pDS.CreateSoundBuffer(dsbd, dsb, nil);
      if hres <> DS_OK then
      begin
        I_ErrorStartSound('CreateSoundBuffer()');
        result := 0;
        exit;
      end
      else
        sparm.avgfreq := SampleFormat.nAvgBytesPerSec;
    end;
  end;

  hres := dsb.Lock(0, sparm.length - sparm.offset, p, s, p2, s2, 0);
  if hres <> DS_OK then
  begin
    I_ErrorStartSound('SoundBuffer.Lock()');
    result := 0;
    exit;
  end;

  memcpy(p, pointer(integer(S_sfx[id].data) + sparm.offset), s);
  hres := dsb.Unlock(p, s, p2, s2);
  if hres <> DS_OK then
  begin
    I_ErrorStartSound('SoundBuffer.Unlock()');
    result := 0;
    exit;
  end;

  ChannelBuffers[channel] := dsb;
  channelids[channel] := id;
  result := I_RestartChannel(channel, vol, sep);
end;

//==============================================================================
//
// I_StopSound
//
//==============================================================================
procedure I_StopSound(handle: integer);
var
  channel: integer;
begin
  if pDS = nil then
    exit;

  for channel := 0 to NUM_CHANNELS - 1 do
  begin
    if I_ChannelPlaying(channel) and (channelhandles[channel] = handle) then
    begin
      ChannelBuffers[channel].Stop;
      ChannelActive[channel] := false;
    end;
  end;
end;

//==============================================================================
//
// I_SoundIsPlaying
//
//==============================================================================
function I_SoundIsPlaying(handle: integer): boolean;
var
  channel: integer;
begin
  if pDS = nil then
  begin
    result := false;
    exit;
  end;

  for channel := 0 to NUM_CHANNELS - 1 do
  begin
    if (channelhandles[channel] = handle) and I_ChannelPlaying(channel) then
    begin
      result := true;
      exit;
    end;
  end;
  result := false;
end;

//==============================================================================
//
// I_ShutDownSound
//
//==============================================================================
procedure I_ShutDownSound;
var
  i: integer;
begin
  if pDS <> nil then
  begin
    for i := 0 to NUM_CHANNELS - 1 do
      I_KillChannel(i);
  end;

  if pDSBPrimary <> nil then
    I_ClearInterface(IInterface(pDSBPrimary));

  if pDS <> nil then
    I_ClearInterface(IInterface(pDS));

  memfree(pointer(soundparams), numsoundparams * SizeOf(soundparam_t));

  S_ShutDownSound;
end;

//==============================================================================
//
// I_InitSound
//
//==============================================================================
procedure I_InitSound;
var
  hres: HRESULT;
  dsbd: DSBUFFERDESC;
  i: integer;
begin
  if M_CheckParm('-nosound') <> 0 then
    exit;

  hres := DirectSoundCreate(nil, pDS, nil);
  if hres <> DS_OK then
  begin
    pDS := nil;
    printf('I_InitSound(): DirectSoundCreate Failed, result = %d', [hres]);
    exit;
  end;

  hres := pDS.SetCooperativeLevel(hMainWnd, DSSCL_PRIORITY);
  if hres <> DS_OK then
    I_Error('I_InitSound(): DirectSound.SetCooperativeLevel Failed, result = %d', [hres]);

  SampleFormat.wFormatTag := WAVE_FORMAT_PCM;
  SampleFormat.nChannels := 1;
  SampleFormat.cbSize := 0;
  SampleFormat.nBlockAlign := 1;
  SampleFormat.nSamplesPerSec := 11025;
  SampleFormat.nAvgBytesPerSec := 11025;
  SampleFormat.wBitsPerSample := 8;

  ZeroMemory(@dsbd, SizeOf(DSBUFFERDESC));
  dsbd.dwSize := SizeOf(DSBUFFERDESC);
  dsbd.dwFlags := DSBCAPS_PRIMARYBUFFER;
  dsbd.dwBufferBytes := 0;
  dsbd.lpwfxFormat := nil;

  hres := pDS.CreateSoundBuffer(dsbd, pDSBPrimary, nil);
  if hres <> DS_OK then
  begin
    I_Warning('I_InitSound(): Unable to access primary sound buffer, result = %d', [hres]);
    pDSBPrimary := nil;
  end
  else
  begin
    hres := pDSBPrimary.SetFormat(SampleFormat);
    if hres <> DS_OK then
      I_Warning('I_InitSound(): Unable to set primary sound buffer format, result = %d', [hres]);
    pDSBPrimary.Play(0, 0, DSBPLAY_LOOPING);
  end;

  for i := 0 to NUM_CHANNELS - 1 do
    ChannelBuffers[i] := nil;

  for i := 0 to numsfx - 1 do
    S_sfx[i].data := nil;

end;

//==============================================================================
//
// I_SetUseExternalWav
//
//==============================================================================
procedure I_SetUseExternalWav(const newu: boolean);
var
  i: integer;
begin
  if newu <> useexternalwav then
  begin
    useexternalwav := newu;
    for i := 0 to numsfx - 1 do
      if S_sfx[i].data <> nil then
      begin
        Z_Free(S_sfx[i].data);
        S_sfx[i].data := nil;
      end;
    for i := 0 to NUM_CHANNELS - 1 do
      channelids[i] := -1;
  end;
end;

initialization
  pDS := nil;
  pDSBPrimary := nil;
  HandleCount := 1;

end.
