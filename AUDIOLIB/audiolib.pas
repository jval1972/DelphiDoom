//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
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
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//  DESCRIPTION:
//   Ogg, flac, etc sounds.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit audiolib;

interface

uses
  d_delphi;

function Audiolib_SearchSoundPAK(const sndname: string; var streamout: TDStream): boolean;

implementation

uses
  doomdef,
  i_system,
  i_sound,
  libsndfile,
  w_folders,
  w_pak;

const
  BUFFER_LEN = 4096;

function tm_get_filelen_func(pms: PDStream): Tuos_count_t; cdecl;
begin
  result := pms.Size;
end;

function tm_seek_func(offset: Tuos_count_t; whence: integer; pms: PDStream): Tuos_count_t; cdecl;
begin
  result := pms.Seek(offset, whence);
end;

function tm_read_func(const buf: Pointer; count: Tuos_count_t; pms: PDStream): Tuos_count_t; cdecl;
begin
  result := pms.Read(buf^, count);
end;

function tm_write_func(const buf: Pointer; count: Tuos_count_t; pms: PDStream): Tuos_count_t; cdecl;
begin
  result := pms.Write(buf^, count);
end;

function tm_tell_func(pms: PDStream): Tuos_count_t; cdecl;
begin
  result := pms.Position;
end;

procedure Audiolib_InitIO(const psfio: PSF_VIRTUAL);
begin
  psfio.sf_vio_get_filelen := @tm_get_filelen_func;
  psfio.seek := @tm_seek_func;
  psfio.read := @tm_read_func;
  psfio.write := @tm_write_func;
  psfio.tell := @tm_tell_func;
end;

function Audiolib_SearchSoundPAK(const sndname: string; var streamout: TDStream): boolean;
var
  sfinfo: TSF_INFO;
  sfvirtualio: TSF_VIRTUAL;
  sfinhandle: TSNDFILE_HANDLE;
  sfouthandle: TSNDFILE_HANDLE;
  data: array[0..BUFFER_LEN - 1] of integer;
  frames, readcount: integer;
  streamin: TPakStream;
  externalsoundfilenames: TDStringList;
  i: integer;
  foundsound: boolean;
  soundfilename: string;
  totalwrite: integer;
begin
  Audiolib_InitIO(@sfvirtualio);

  externalsoundfilenames := TDStringList.Create;
  externalsoundfilenames.Add('%s.ogg', [sndname]);
  externalsoundfilenames.Add('%s.oga', [sndname]);
  externalsoundfilenames.Add('%s.flac', [sndname]);
  externalsoundfilenames.Add('%s.voc', [sndname]);
  externalsoundfilenames.Add('%s.au', [sndname]);
  externalsoundfilenames.Add('%s.snd', [sndname]);
  externalsoundfilenames.Add('ds%s.ogg', [sndname]);
  externalsoundfilenames.Add('ds%s.oga', [sndname]);
  externalsoundfilenames.Add('ds%s.flac', [sndname]);
  externalsoundfilenames.Add('ds%s.voc', [sndname]);
  externalsoundfilenames.Add('ds%s.au', [sndname]);
  externalsoundfilenames.Add('ds%s.snd', [sndname]);

  foundsound := false;

  for i := 0 to externalsoundfilenames.Count - 1 do
  begin
    soundfilename := externalsoundfilenames[i];
    if preferewavnamesingamedirectory then
      streamin := TPakStream.Create(soundfilename, pm_prefered, gamedirectories)
    else
      streamin := TPakStream.Create(soundfilename, pm_short, '', FOLDER_SOUNDS);
    streamin.OnBeginBusy := I_BeginDiskBusy;
    foundsound := streamin.IOResult = 0;
    if foundsound then
      break
    else
      streamin.Free;
  end;

  externalsoundfilenames.Free;

  if not foundsound then
  begin
    streamout := nil;
    Result := false;
    Exit;
  end;

  sfinfo.format := 0;
  sfinhandle := _sf_open_virtual(@sfvirtualio, SFM_READ, @sfinfo, @streamin);
  if sfinhandle = nil then
  begin
    _sf_close(sfinhandle);
    streamin.Free;
    streamout := nil;
    Result := false;
    Exit;
  end;

  sfinfo.format := SF_FORMAT_PCM_16 or SF_FORMAT_WAV or SF_ENDIAN_LITTLE;
  sfinfo.samplerate := 44100;
  sfinfo.channels := 2;
  frames := BUFFER_LEN div sfinfo.channels;
  readcount := frames;

  streamout := TDMemoryStream.Create;
  totalwrite := 0;
  sfouthandle := _sf_open_virtual(@sfvirtualio, SFM_WRITE, @sfinfo, @streamout);
  if sfouthandle = nil then
  begin
    _sf_close(sfinhandle);
    _sf_close(sfouthandle);
    streamin.Free;
    streamout.Free;
    streamout := nil;
    Result := false;
    Exit;
  end;

  while readcount > 0 do
  begin
    readcount := _sf_readf_int(sfinhandle, @data, frames);
    if readcount > 0 then
      totalwrite := totalwrite + _sf_writef_int(sfouthandle, @data, readcount);
  end;

  Result := (streamout.Size > 0) and (totalwrite > 0);

  if Result then
  begin
    streamout.Seek(0, sFromBeginning);
    printf('Audiolib_SearchSoundPAK(): Found external sound %s'#13#10, [soundfilename]);
  end
  else
  begin
    streamout.Free;
    streamout := nil;
  end;

  _sf_close(sfinhandle);
  _sf_close(sfouthandle);
  streamin.Free;
end;

end.
