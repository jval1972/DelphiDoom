//------------------------------------------------------------------------------
//
//  DelphiHeretic: A modified and improved Heretic port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
//  Copyright (C) 2004-2021 by Jim Valavanis
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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//  DESCRIPTION:
//    The not so system specific sound interface.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit s_sound;

interface

uses
  p_mobj_h;

//
// Initializes sound stuff, including volume
// Sets channels, SFX and music volume,
//  allocates channel buffer, sets S_sfx lookup.
//
procedure S_Init(sfxVolume: integer; musicVolume: integer);

procedure S_ShutDownSound;

//
// Per level startup code.
// Kills playing sounds at start of level,
//  determines music if any, changes music.
//
procedure S_Start;


//
// Start sound for thing at <origin>
//  using <sound_id> from sounds.h
//
procedure S_StartSound(origin: pointer; sfx_id: integer); overload;

procedure S_StartSound(origin: pointer; const sndname: string); overload;


// Will start a sound at a given volume.
procedure S_StartSoundAtVolume(origin_p: pointer; sfx_id: integer; volume: integer);


// Stop sound for thing at <origin>
procedure S_StopSound(origin: pointer);


// Start music using <music_id> from sounds.h
procedure S_StartMusic(music_id: integer);

// Start music using <music_id> from sounds.h,
//  and set whether looping
procedure S_ChangeMusic(musicnum: integer; looping: boolean);

// Stops the music fer sure.
procedure S_StopMusic;

// Stop and resume music, during game PAUSE.
procedure S_PauseSound;
procedure S_ResumeSound;

//
// Updates music & sounds
//
procedure S_UpdateSounds(listener_p: pointer);

procedure S_SetMusicVolume(volume: integer);
procedure S_SetSfxVolume(volume: integer);

procedure S_PrecacheSounds;

var
// These are not used, but should be (menu).
// Maximum volume of a sound effect.
// Internal default is max out of 0-15.
  snd_SfxVolume: integer = 15;

// Maximum volume of music. Useless so far.
  snd_MusicVolume: integer = 15;

// following is set
//  by the defaults code in M_misc:
// number of channels available
  numChannels: integer;

const
  MIN_NUMCHANNELS = 8;
  MAX_NUMCHANNELS = 32;

type
  chaninfo_t = record
    id: integer;
    priority: integer;
    name: string;
    mo: Pmobj_t;
    distance: integer;
  end;
  Pchaninfo_t = ^chaninfo_t;

  soundinfo_t = record
    numchannels: integer;
    musicvolume: integer;
    soundvolume: integer;
    chan: array[0..MAX_NUMCHANNELS - 1] of chaninfo_t;
  end;
  Psoundinfo_t = ^soundinfo_t;

procedure S_GetChannelInfo(s: Psoundinfo_t);

function S_DefaultMusicForMap(const episode, map: integer): integer;

implementation

uses
  d_delphi,
  c_cmds,
  d_player,
  d_think,
  g_game,
  i_system,
  i_sound,
  i_music,
  i_mp3,
  info_h,
  m_fixed,
  m_rnd,
  m_misc,
  p_mobj,
  p_tick,
  p_local,
  p_maputl,
  sounds,
  s_externalmusic,
  z_zone,
  w_folders,
  w_wad,
  w_pak,
  doomdef,
  r_main,
  tables,
  doomstat;

// Purpose?
const
  snd_prefixen: array[0..11] of char =
    ('P', 'P', 'A', 'S', 'S', 'S', 'M', 'M', 'M', 'S', 'S', 'S' );

  S_MAX_VOLUME = 127;

// when to clip out sounds
// Does not fit the large outdoor areas.
  S_CLIPPING_DIST = 1200 * $10000;

// Distance tp origin when sounds should be maxed out.
// This should relate to movement clipping resolution
// (see BLOCKMAP handling).
// Originally: (200*0x10000).
  S_CLOSE_DIST = 160 * $10000;
//  S_CLOSE_DIST = 200 * $10000;

  S_ATTENUATOR = (S_CLIPPING_DIST - S_CLOSE_DIST) div FRACUNIT;

// Adjustable by menu.
  NORM_PITCH = 128;
  NORM_PRIORITY = 64;
  NORM_SEP = 128;

  S_PITCH_PERTURB = 1;
  S_STEREO_SWING = 96 * $10000;

type
  channel_t = record
    // sound information (if null, channel avail.)
    sfxinfo: Psfxinfo_t;

    // origin of sound
    origin: pointer;

    // handle of the sound being played
    handle: integer;
  end;
  Pchannel_t = ^channel_t;
  channel_tArray = packed array[0..$FFFF] of channel_t;
  Pchannel_tArray = ^channel_tArray;

// the set of channels available
var
  channels: Pchannel_tArray;

var
// whether songs are mus_paused
  mus_paused: boolean;

// music currently being played
  mus_playing: Pmusicinfo_t = nil;
  looping_playing: boolean;

//
// Internals.
//
function S_GetChannel(origin: pointer; sfxinfo: Psfxinfo_t): integer; forward;

function S_AdjustSoundParams(listener: Pmobj_t; source:Pmobj_t;
  vol: Pinteger; sep: Pinteger; pitch:Pinteger): boolean; forward;

procedure S_StopChannel(cnum: integer); forward;

procedure S_DoChangeMusic(music: Pmusicinfo_t; looping: boolean); forward;

procedure S_CmdUseMP3(const parm1: string = '');
var
  newu: boolean;
begin
  if parm1 = '' then
  begin
    printf('Current setting: usemp3 = %s.'#13#10, [truefalseStrings[usemp3]]);
    exit;
  end;

  newu := C_BoolEval(parm1, usemp3);
  if newu <> usemp3 then
  begin
    usemp3 := newu;
    if mus_playing <> nil then
      S_DoChangeMusic(mus_playing, looping_playing);
  end;
  S_CmdUseMP3;
end;


procedure S_CmdUseExternalWav(const parm: string = '');
begin
  if parm = '' then
  begin
    printf('Current setting: useexternalwav = %s.'#13#10, [truefalseStrings[useexternalwav]]);
    exit;
  end;

  I_SetUseExternalWav(C_BoolEval(parm, useexternalwav));
  S_CmdUseExternalWav;
end;

procedure S_CmdMidiTempo(const parm1: string = '');
var
  newt: integer;
begin
  if parm1 = '' then
  begin
    printf('Current setting: miditempo = %d'#13#10, [miditempo]);
    exit;
  end;

  newt := atoi(parm1);
  if (newt > 0) and (newt < 256) then
  begin
    if newt <> miditempo then
    begin
      miditempo := newt;
      if mus_playing <> nil then
        S_DoChangeMusic(mus_playing, looping_playing);
    end;
    S_CmdMidiTempo;
  end
  else
    printf('Specify an integer number in [1..255]'#13#10);

end;

//
// Initializes sound stuff, including volume
// Sets channels, SFX and music volume,
//  allocates channel buffer, sets S_sfx lookup.
//
procedure S_Init(sfxVolume: integer; musicVolume: integer);
var
  i: integer;
begin
  printf('S_Init: default sfx volume %d'#13#10, [sfxVolume]);

  S_ExternalMusicInit;

  // Whatever these did with DMX, these are rather dummies now.
  I_SetChannels;

  S_SetSfxVolume(sfxVolume);
  // No music with Linux - another dummy.
  S_SetMusicVolume(musicVolume);

  // Allocating the internal channels for mixing
  // (the maximum numer of sounds rendered
  // simultaneously) within zone memory.
  if numChannels < MIN_NUMCHANNELS then
    numChannels := MIN_NUMCHANNELS; // JVAL: Set the minimum number of channels

  if numChannels > MAX_NUMCHANNELS then
    numChannels := MAX_NUMCHANNELS; // JVAL: Set the maximum number of channels

  channels := Z_Malloc(numChannels * SizeOf(channel_t), PU_STATIC, nil);

  // Free all channels for use
  for i := 0 to numChannels - 1 do
    channels[i].sfxinfo := nil;

  // no sounds are playing, and they are not mus_paused
  mus_paused := false;

  // Note that sounds have not been cached (yet).
  for i := 1 to numsfx - 1 do
  begin
    S_sfx[i].lumpnum := -1;
    S_sfx[i].usefulness := -1;
  end;

  C_AddCmd('usemp3', @S_CmdUseMP3);
  C_AddCmd('useexternalwav', @S_CmdUseExternalWav);
  C_AddCmd('miditempo', @S_CmdMidiTempo);
end;

procedure S_ShutDownSound;
begin
  S_FreeRandomSoundLists;
  S_ShutDownExternalMusic;
end;

function S_DefaultMusicForMap(const episode, map: integer): integer;
begin
  // JVAL: Use DEH files to specify new sounds for E4
  if episode < 6 then
    result := Ord(mus_e1m1) + (episode - 1) * 9 + map - 1
  else  // JVAL Game episode > 5 ????
  begin
    case map of
      1: result := Ord(mus_e1m1);
      2: result := Ord(mus_e1m2);
      3: result := Ord(mus_e1m3);
      4: result := Ord(mus_e1m4);
      5: result := Ord(mus_e1m5);
      6: result := Ord(mus_e1m6);
      7: result := Ord(mus_e1m7);
      8: result := Ord(mus_e1m8);
      9: result := Ord(mus_e1m9);
    else
      result := Ord(mus_e1m1); // JVAL ?????
    end;
  end;
end;

//
// Per level startup code.
// Kills playing sounds at start of level,
//  determines music if any, changes music.
//
procedure S_Start;
var
  cnum: integer;
  mnum: integer;
begin
  // kill all playing sounds at start of level
  //  (trust me - a good idea)
  for cnum := 0 to numChannels - 1 do
    if channels[cnum].sfxinfo <> nil then
      S_StopChannel(cnum);

  // start new music for the level
  mus_paused := false;

  mnum := S_DefaultMusicForMap(gameepisode, gamemap);

  S_ChangeMusic(mnum, true);
end;

procedure S_StartSoundAtVolume(origin_p: pointer; sfx_id: integer; volume: integer);
var
  rc: boolean;
  sep: integer;
  pitch: integer;
  priority: integer;
  sfx: Psfxinfo_t;
  cnum: integer;
  origin: Pmobj_t;
begin
  origin := Pmobj_t(origin_p);

  // check for bogus sound #
  if (sfx_id < 1) or (sfx_id > numsfx) then
  begin
    I_DevError('S_StartSoundAtVolume(): Bad sfx #: %d'#13#10, [sfx_id]);
    exit;
  end;

  sfx := @S_sfx[sfx_id];

  // Initialize sound parameters
  if sfx.link <> nil then
  begin
    pitch := sfx.pitch;
    priority := sfx.priority;
    volume := volume + sfx.volume;

    if volume < 1 then
      exit;

    if volume > snd_SfxVolume then
      volume := snd_SfxVolume;
  end
  else
  begin
    pitch := NORM_PITCH;
    priority := NORM_PRIORITY;
  end;

  // Check to see if it is audible,
  //  and if not, modify the params
  if (origin <> nil) and (origin <> players[consoleplayer].mo) then
  begin
    rc := S_AdjustSoundParams(players[consoleplayer].mo, origin,
           @volume,
           @sep,
           @pitch);

    if (origin.x = players[consoleplayer].mo.x) and
       (origin.y = players[consoleplayer].mo.y) then
      sep := NORM_SEP;

    if not rc then
      exit;
  end
  else
    sep := NORM_SEP;

  // kill old sound
  S_StopSound(origin);

  // try to find a channel
  cnum := S_GetChannel(origin, sfx);

  if cnum < 0 then
  begin
    I_DevWarning('S_StartSoundAtVolume(): Can not find channel for sfx=%d'#13#10, [sfx_id]);
    exit;
  end;

  //
  // This is supposed to handle the loading/caching.
  // For some odd reason, the caching is done nearly
  //  each time the sound is needed?
  //

  // get lumpnum if necessary
  if sfx.lumpnum < 0 then
  begin
    sfx.lumpnum := I_GetSfxLumpNum(sfx);

    // JVAL
    // Prevent crash, simply don't play the sound
    if sfx.lumpnum < 0 then
    begin
      I_Warning('S_StartSoundAtVolume(): Sfx #: %d not found.'#13#10, [sfx_id]);
      exit;
    end;
  end;

  // increase the usefulness
  if sfx.usefulness < 0 then
    sfx.usefulness := 0; // JVAL ??? original was := 1
  sfx.usefulness := sfx.usefulness + 1;

  // Assigns the handle to one of the channels in the
  //  mix/output buffer.
  pitch := 127 - (M_Random and 3) + (M_Random and 3);
  channels[cnum].handle := I_StartSound(sfx_id, volume, sep, pitch, priority);
end;

procedure S_StartSound(origin: pointer; sfx_id: integer);
begin
  S_StartSoundAtVolume(origin, sfx_id, snd_SfxVolume);
end;

procedure S_StartSound(origin: pointer; const sndname: string);
begin
  S_StartSoundAtVolume(origin, S_GetSoundNumForName(sndname), snd_SfxVolume);
end;

procedure S_StopSound(origin: pointer);
var
  cnum: integer;
begin
  for cnum := 0 to numChannels - 1 do
  begin
    if (channels[cnum].sfxinfo <> nil) and (channels[cnum].origin = origin) then
    begin
      S_StopChannel(cnum);
      break;
    end;
  end;
end;

//
// Stop and resume music, during game PAUSE.
//
procedure S_PauseSound;
begin
  if (mus_playing <> nil) and (not mus_paused) then
  begin
    I_PauseSong(mus_playing.handle);
    mus_paused := true;
  end;
end;

procedure S_ResumeSound;
begin
  if (mus_playing <> nil) and mus_paused then
  begin
    I_ResumeSong(mus_playing.handle);
    mus_paused := false;
  end;
end;

//
// Updates music & sounds
//
procedure S_UpdateSounds(listener_p: pointer);
var
  audible: boolean;
  cnum: integer;
  volume: integer;
  sep: integer;
  pitch: integer;
  sfx: Psfxinfo_t;
  c: Pchannel_t;
  listener: Pmobj_t;
begin
  listener := Pmobj_t(listener_p);

  for cnum := 0 to numChannels - 1 do
  begin
    c := @channels[cnum];
    sfx := c.sfxinfo;

    if sfx <> nil then
    begin
      if I_SoundIsPlaying(c.handle) then
      begin
        // initialize parameters
        volume := snd_SfxVolume;
        pitch := NORM_PITCH;
        sep := NORM_SEP;

        if sfx.link <> nil then
        begin
          pitch := sfx.pitch;
          volume := volume + sfx.volume;
          if volume < 1 then
          begin
            S_StopChannel(cnum);
            continue;
          end
          else if volume > snd_SfxVolume then
          begin
            volume := snd_SfxVolume;
          end;
        end;

        // check non-local sounds for distance clipping
        //  or modify their params
        if (c.origin <> nil) and (integer(listener_p) <> integer(c.origin)) then
        begin
          audible := S_AdjustSoundParams(listener, c.origin, @volume, @sep, @pitch);

          if not audible then
          begin
            S_StopChannel(cnum);
          end
          else
            I_UpdateSoundParams(c.handle, volume, sep, pitch);
        end
      end
      else
      begin
        // if channel is allocated but sound has stopped,
        //  free it
        S_StopChannel(cnum);
      end;
    end;
  end;
end;

procedure S_SetMusicVolume(volume: integer);
begin
  if (volume < 0) or (volume > 15) then
  begin
    I_DevError('S_SetMusicVolume(): Attempt to set music volume at %d', [volume]);
    volume := 8;
  end;

  I_SetMusicVolume(volume);
  snd_MusicVolume := volume;
end;

procedure S_SetSfxVolume(volume: integer);
begin
  if (volume < 0) or (volume > 127) then
  begin
    I_Warning('S_SetSfxVolume(): Attempt to set sfx volume at %d', [volume]);
    snd_SfxVolume := 64;
  end
  else
    snd_SfxVolume := volume;
end;

//
// Starts some music with the music id found in sounds.h.
//
procedure S_StartMusic(music_id: integer);
begin
  S_ChangeMusic(music_id, false);
end;

procedure S_DoChangeMusic(music: Pmusicinfo_t; looping: boolean);
var
  i: integer;
  namebuf: char8_t;
  mp3filename: string;
  mp3header: Pmp3header_t;
  externalmusicfilenames: TDStringList;
  mp3error: boolean;
begin
  // shutdown old music
  S_StopMusic;

  // get lumpnum if neccessary
  if music.lumpnum = 0 then
  begin
    namebuf := stringtochar8('MUS_' + music.name);
    music.lumpnum := W_CheckNumForName(namebuf);
    if music.lumpnum <= 0 then
    begin
      namebuf := stringtochar8(music.name);
      music.lumpnum := W_GetNumForName(namebuf);
    end;
  end;

  if usemp3 then
  begin
    if music.mp3stream = nil then
    begin

    // JVAL: Create a list with external mp3 filenames to check
      externalmusicfilenames := TDStringList.Create;
      if music.mapname <> '' then
      begin
        externalmusicfilenames.Add(music.mapname + '.mp3');
        externalmusicfilenames.Add('MUS_' + music.mapname + '.mp3');
      end;
      externalmusicfilenames.Add(music.name + '.mp3');
      externalmusicfilenames.Add('MUS_' + music.name + '.mp3');

      for i := 0 to externalmusicfilenames.Count - 1 do
      begin
        mp3filename := externalmusicfilenames[i];
        if preferemp3namesingamedirectory then
          music.mp3stream := TPakStream.Create(mp3filename, pm_prefered, gamedirectories)
        else
          music.mp3stream := TPakStream.Create(mp3filename, pm_short, '', FOLDER_MUSIC);
        music.mp3stream.OnBeginBusy := I_BeginDiskBusy;
        mp3error := music.mp3stream.IOResult <> 0;
        if mp3error then
          FreeAndNil(music.mp3stream)
        else
          break;
      end;

      externalmusicfilenames.Free;

    end;
  end;

  if usemp3 and (music.mp3stream <> nil) then
  begin
    mp3header := Z_Malloc(SizeOf(mp3header_t), PU_MUSIC, nil);
    mp3header.ID := MP3MAGIC;
    mp3header.Stream := music.mp3stream;
    music.data := mp3header;
    music.handle := I_RegisterSong(music.data, W_LumpLength(music.lumpnum));
  end
  else
  begin
    if not S_TryLoadExternalMusic(music) then
    begin
      // load & register it
      music.data := W_CacheLumpNum(music.lumpnum, PU_MUSIC);
      music.handle := I_RegisterSong(music.data, W_LumpLength(music.lumpnum));
    end;
  end;


  // play it
  I_PlaySong(music.handle, looping);

  mus_playing := music;
  looping_playing := looping;
end;

procedure S_ChangeMusic(musicnum: integer; looping: boolean);
var
  music: Pmusicinfo_t;
begin
  if (musicnum <= Ord(mus_None)) or
     (musicnum >= nummusic) then
    I_Error('S_ChangeMusic(): Bad music number %d', [musicnum]);

  music := @S_music[musicnum];

  if mus_playing = music then
    exit;

  if mus_playing <> nil then
    if music.name = mus_playing.name then
      exit;

  S_DoChangeMusic(music, looping);
end;

procedure S_StopMusic;
begin
  if mus_playing <> nil then
  begin
    if mus_paused then
      I_ResumeSong(mus_playing.handle);

    I_UnRegisterSong(mus_playing.handle);
    if mus_playing.mp3stream <> nil then
    begin
      FreeAndNil(mus_playing.mp3stream);
      Z_Free(mus_playing.data);
    end
    else
      Z_ChangeTag(mus_playing.data, PU_CACHE);

    mus_playing.data := nil;
    mus_playing := nil;
  end;
end;

procedure S_StopChannel(cnum: integer);
var
  i: integer;
  c: Pchannel_t;
begin
  c := @channels[cnum];

  if c.sfxinfo <> nil then
  begin
    // stop the sound playing
    if I_SoundIsPlaying(c.handle) then
    begin
      I_StopSound(c.handle);
    end;

    // check to see
    //  if other channels are playing the sound
    for i := 0 to numChannels - 1 do
    begin
      if (cnum <> i) and
         (c.sfxinfo = channels[i].sfxinfo) then
        break;
    end;

    // degrade usefulness of sound data
    c.sfxinfo.usefulness := c.sfxinfo.usefulness - 1;

    c.sfxinfo := nil;
  end;
end;

//
// Changes volume, stereo-separation, and pitch variables
//  from the norm of a sound effect to be played.
// If the sound is not audible, returns a 0.
// Otherwise, modifies parameters and returns 1.
//
function S_AdjustSoundParams(listener: Pmobj_t; source:Pmobj_t;
  vol: Pinteger; sep: Pinteger; pitch:Pinteger): boolean;
var
  approx_dist: fixed_t;
  adx: fixed_t;
  ady: fixed_t;
  ad: fixed_t;
  angle: angle_t;
  langle: angle_t;
begin
  // calculate the distance to sound origin
  //  and clip it if necessary
  adx := abs(listener.x - source.x);
  ady := abs(listener.y - source.y);

  if adx < ady then
    ad := adx
  else
    ad := ady;
  // From _GG1_ p.428. Appox. eucledian distance fast.
  approx_dist := adx + ady - ad div 2;

  if (gamemap <> 8) and
     (approx_dist > S_CLIPPING_DIST) then
  begin
    result := false;
    exit;
  end;

  // angle of source to listener
  angle := R_PointToAngle2(listener.x, listener.y, source.x, source.y);

  langle := listener.angle + listener.viewangle;
  if angle > langle then
    angle := angle - langle
  else
    angle := angle + ($ffffffff - langle);

  {$IFDEF FPC}
  angle := angle div ANGLETOFINEUNIT;
  {$ELSE}
  angle := angle shr ANGLETOFINESHIFT;
  {$ENDIF}

  // stereo separation
  sep^ := NORM_SEP - (FixedMul(S_STEREO_SWING, finesine[angle]) div FRACUNIT);

  // volume calculation
  if approx_dist < S_CLOSE_DIST then
  begin
    vol^ := snd_SfxVolume;
  end
  else if gamemap = 8 then
  begin
    if approx_dist > S_CLIPPING_DIST then
      approx_dist := S_CLIPPING_DIST;

    vol^ := 15 + ((snd_SfxVolume - 15) *
      ((S_CLIPPING_DIST - approx_dist) div FRACUNIT)) div S_ATTENUATOR;
  end
  else
  begin
    // distance effect
    vol^ := (snd_SfxVolume * ((S_CLIPPING_DIST - approx_dist) div FRACUNIT)) div
              S_ATTENUATOR;
  end;

  result := vol^ > 0;
end;

//
// S_GetChannel :
//   If none available, return -1.  Otherwise channel #.
//
function S_GetChannel(origin: pointer; sfxinfo: Psfxinfo_t): integer;
var
  // channel number to use
  cnum: integer;
  c: Pchannel_t;
begin
  // Find an open channel
  cnum := 0;
  while cnum < numChannels do
  begin
    if channels[cnum].sfxinfo = nil then
      break
    else if (origin <> nil) and (channels[cnum].origin = origin) then
    begin
      S_StopChannel(cnum);
      break;
    end;
    inc(cnum);
  end;

  // None available
  if cnum = numChannels then
  begin
    // Look for lower priority
    cnum := 0;
    while cnum < numChannels do
    begin
      if channels[cnum].sfxinfo.priority >= sfxinfo.priority then
        break;
      inc(cnum);
    end;

    if cnum = numChannels then
    begin
      // FUCK!  No lower priority.  Sorry, Charlie.
      result := -1;
      exit;
    end
    else
    begin
      // Otherwise, kick out lower priority.
      S_StopChannel(cnum);
    end;
  end;

  c := @channels[cnum];

  // channel is decided to be cnum.
  c.sfxinfo := sfxinfo;
  c.origin := origin;

  result := cnum;
end;

procedure S_GetChannelInfo(s: Psoundinfo_t);
var
  i: integer;
  c: Pchaninfo_t;
begin
  s.numchannels := numChannels;
  s.musicVolume := snd_MusicVolume;
  s.soundVolume := snd_SfxVolume;
  for i := 0 to numChannels - 1 do
  begin
    c := @s.chan[i];
    if channels[i].sfxinfo <> nil then
    begin
      c.id := channels[i].sfxinfo.lumpnum;  // JVAL: check!
      c.priority := channels[i].sfxinfo.priority;
      c.name := channels[i].sfxinfo.name;
      c.mo := channels[i].origin;
      if c.mo <> nil then
        c.distance := P_AproxDistance(c.mo.x - viewx, c.mo.y - viewy) div FRACUNIT
      else
        c.distance := 0;
      end
    else
    begin
      c.id := 0;
      c.priority := 0;
      c.name := '(NULL)';
      c.mo := nil;
      c.distance := 0;
    end;
  end;
end;

procedure S_PrecacheSounds;
var
  hitlist: PBooleanArray;
  i: integer;
  th: Pthinker_t;
  inf: Pmobjinfo_t;
  sndmem: integer;

  procedure Add1(const id: integer);
  begin
    if id > 0 then
      hitlist[id] := true;
  end;

begin
  printf('S_PrecacheSounds()'#13#10);

  hitlist := mallocz(numsfx * SizeOf(boolean));

  th := thinkercap.next;
  while (th <> nil) and (th <> @thinkercap) do
  begin
    if @th._function.acp1 = @P_MobjThinker then
    begin
      inf := Pmobj_t(th).info;
      Add1(inf.seesound);
      Add1(inf.attacksound);
      Add1(inf.painsound);
      Add1(inf.deathsound);
      Add1(inf.activesound);
      Add1(inf.meleesound);
      Add1(inf.customsound1);
      Add1(inf.customsound2);
      Add1(inf.customsound3);
    end;
    th := th.next;
  end;

  sndmem := 0;
  for i := 1 to numsfx - 1 do
    if hitlist[i] then
    begin
      S_sfx[i].lumpnum := I_GetSfxLumpNum(@S_sfx[i]);
      if S_sfx[i].lumpnum >= 0 then
      begin
        W_CacheLumpNum(S_sfx[i].lumpnum, PU_SOUND);
        sndmem := sndmem + W_LumpLength(S_sfx[i].lumpnum);
      end;
    end;
  printf('%6d KB memory usage for sound cache'#13#10, [sndmem div 1024]);

  memfree(pointer(hitlist), numsfx * SizeOf(boolean));
end;

end.

