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
//   Ogg, flac, etc sounds.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit audiolib;

interface

uses
  d_delphi;

//==============================================================================
//
// Audiolib_SearchSoundPAK
//
//==============================================================================
function Audiolib_SearchSoundPAK(const sndname: string; var streamout: TDStream;
  const normalize: integer): boolean;

//==============================================================================
//
// Audiolib_DecodeSoundWAD
//
//==============================================================================
function Audiolib_DecodeSoundWAD(memin: Pointer; meminsize: integer;
  memout: PPointer; var memoutsize: integer; const normalize: integer): boolean;

implementation

uses
  doomdef,
  i_system,
  i_sound,
  libsndfile,
  w_folders,
  w_pak,
  z_zone;

const
  BUFFER_LEN = 4096;

//==============================================================================
//
// tm_get_filelen_func
//
//==============================================================================
function tm_get_filelen_func(pms: PDStream): Tuos_count_t; cdecl;
begin
  result := pms.Size;
end;

//==============================================================================
//
// tm_seek_func
//
//==============================================================================
function tm_seek_func(offset: Tuos_count_t; whence: integer; pms: PDStream): Tuos_count_t; cdecl;
begin
  result := pms.Seek(offset, whence);
end;

//==============================================================================
//
// tm_read_func
//
//==============================================================================
function tm_read_func(const buf: Pointer; count: Tuos_count_t; pms: PDStream): Tuos_count_t; cdecl;
begin
  result := pms.Read(buf^, count);
end;

//==============================================================================
//
// tm_write_func
//
//==============================================================================
function tm_write_func(const buf: Pointer; count: Tuos_count_t; pms: PDStream): Tuos_count_t; cdecl;
begin
  result := pms.Write(buf^, count);
end;

//==============================================================================
//
// tm_tell_func
//
//==============================================================================
function tm_tell_func(pms: PDStream): Tuos_count_t; cdecl;
begin
  result := pms.Position;
end;

//==============================================================================
//
// Audiolib_InitIO
//
//==============================================================================
procedure Audiolib_InitIO(const psfio: PSF_VIRTUAL);
begin
  psfio.sf_vio_get_filelen := @tm_get_filelen_func;
  psfio.seek := @tm_seek_func;
  psfio.read := @tm_read_func;
  psfio.write := @tm_write_func;
  psfio.tell := @tm_tell_func;
end;

//==============================================================================
//
// Audiolib_DoConvert
//
//==============================================================================
function Audiolib_DoConvert(const sfinhandle, sfouthandle: TSNDFILE_HANDLE;
  const sfinfo: PSF_INFO; const infileminor: integer; const normalize: integer): int64;
var
  dataI: array[0..BUFFER_LEN - 1] of integer;
  dataS: array[0..BUFFER_LEN - 1] of smallint;
  dataD: array[0..BUFFER_LEN - 1] of double;
  i: integer;
  frames, readcount: integer;
  sbuf: TDMemoryStream;
  mx, mn: double;
  numread: integer;
begin
  Result := 0;

  frames := BUFFER_LEN div sfinfo.channels;

  if (normalize <> 0) or
     (infileminor = SF_FORMAT_DOUBLE) or (infileminor = SF_FORMAT_FLOAT) or
     {(infileminor = SF_FORMAT_OPUS) or }(infileminor = SF_FORMAT_VORBIS) then
  begin
    sbuf := TDMemoryStream.Create;
    mx := 0.0;
    mn := 0.0;
    while true do
    begin
      readcount := _sf_readf_double(sfinhandle, @dataD, frames);
      if readcount > 0 then
      begin
        for i := 0 to readcount * sfinfo.channels - 1 do
        begin
          if dataD[i] > 0 then
          begin
            if dataD[i] > mx then
              mx := dataD[i];
          end
          else
          begin
            if dataD[i] < mn then
              mn := dataD[i];
          end;
        end;
        sbuf.Write(dataD, readcount * sfinfo.channels * SizeOf(double));
      end
      else
        break;
    end;
    Result := sbuf.Size div SizeOf(double);
    if (Result > 0) and (mx <> mn) then
    begin
      if mx < -mn then
        mx := -mn;
      sbuf.Seek(0, sFromBeginning);
      Result := 0;
      repeat
        numread := sbuf.Read(dataD, BUFFER_LEN * SizeOf(double)) div SizeOf(Double);
        if numread > 0 then
        begin
          for i := 0 to numread - 1 do
            dataS[i] := GetIntegerInRange(Round(dataD[i] / mx * 32768), -32768, 32767);
          Result := Result + _sf_writef_short(sfouthandle, @dataS, numread div sfinfo.channels);
        end;
      until numread = 0;
    end;
    sbuf.Free;
  end
  else
  begin
    while true do
    begin
      readcount := _sf_readf_int(sfinhandle, @dataI, frames);
      if readcount > 0 then
        Result := Result + _sf_writef_int(sfouthandle, @dataI, readcount)
      else
        break;
    end;
  end;
end;

//==============================================================================
//
// Audiolib_SearchSoundPAK
//
//==============================================================================
function Audiolib_SearchSoundPAK(const sndname: string; var streamout: TDStream;
  const normalize: integer): boolean;
var
  sfinfo: TSF_INFO;
  sfvirtualio: TSF_VIRTUAL;
  sfinhandle: TSNDFILE_HANDLE;
  sfouthandle: TSNDFILE_HANDLE;
  streamin: TPakStream;
  externalsoundfilenames: TDStringList;
  i: integer;
  foundsound: boolean;
  soundfilename: string;
  totalwrite: int64;
  infileminor: integer;
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

  infileminor := sfinfo.format and SF_FORMAT_SUBMASK;

  sfinfo.format := SF_FORMAT_PCM_16 or SF_FORMAT_WAV or SF_ENDIAN_LITTLE;

  streamout := TDMemoryStream.Create;
  streamout.OnBeginBusy := I_BeginDiskBusy;
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

  totalwrite := Audiolib_DoConvert(sfinhandle, sfouthandle, @sfinfo, infileminor, normalize);

  Result := (streamout.Size > 0) and (totalwrite > 0);

  _sf_close(sfinhandle);
  _sf_close(sfouthandle);

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

  streamin.Free;
end;

//==============================================================================
//
// Audiolib_DecodeSoundWAD
//
//==============================================================================
function Audiolib_DecodeSoundWAD(memin: Pointer; meminsize: integer;
  memout: PPointer; var memoutsize: integer; const normalize: integer): boolean;
var
  sfinfo: TSF_INFO;
  sfvirtualio: TSF_VIRTUAL;
  sfinhandle: TSNDFILE_HANDLE;
  sfouthandle: TSNDFILE_HANDLE;
  streamin: TAttachableMemoryStream;
  streamout: TDMemoryStream;
  totalwrite: int64;
  infileminor: integer;
begin
  Audiolib_InitIO(@sfvirtualio);

  streamin := TAttachableMemoryStream.Create;
  streamin.Attach(memin, meminsize);
  streamin.OnBeginBusy := I_BeginDiskBusy;

  sfinfo.format := 0;
  sfinhandle := _sf_open_virtual(@sfvirtualio, SFM_READ, @sfinfo, @streamin);
  if sfinhandle = nil then
  begin
    _sf_close(sfinhandle);
    streamin.Free;
    memout^ := nil;
    memoutsize := 0;
    Result := false;
    Exit;
  end;

  infileminor := sfinfo.format and SF_FORMAT_SUBMASK;

  sfinfo.format := SF_FORMAT_PCM_16 or SF_FORMAT_WAV or SF_ENDIAN_LITTLE;

  streamout := TDMemoryStream.Create;
  streamout.OnBeginBusy := I_BeginDiskBusy;
  sfouthandle := _sf_open_virtual(@sfvirtualio, SFM_WRITE, @sfinfo, @streamout);
  if sfouthandle = nil then
  begin
    _sf_close(sfinhandle);
    _sf_close(sfouthandle);
    streamin.Free;
    streamout.Free;
    memout^ := nil;
    memoutsize := 0;
    Result := false;
    Exit;
  end;

  totalwrite := Audiolib_DoConvert(sfinhandle, sfouthandle, @sfinfo, infileminor, normalize);

  Result := (streamout.Size > 0) and (totalwrite > 0);

  _sf_close(sfinhandle);
  _sf_close(sfouthandle);

  if Result then
  begin
    memoutsize := streamout.Size;
    memout^ := Z_Malloc(memoutsize, PU_SOUND, memout);
    streamout.Seek(0, sFromBeginning);
    streamout.Read(memout^^, memoutsize);
  end
  else
  begin
    memout^ := nil;
    memoutsize := 0;
  end;

  streamout.Free;
  streamin.Free;
end;

end.
