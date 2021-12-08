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
// DESCRIPTION:
//  Main loop menu stuff.
//  Default Config File.
//  Screenshots.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit m_misc;

interface

//
// MISC
//

var
  screenshotformat: string = 'PNG';

function M_WriteFile(const name: string; source: pointer; len: integer): boolean;

function M_AppendFile(const name: string; source: pointer; len: integer): integer;

function M_ReadFile(const name: string; var buffer: Pointer): integer;

procedure M_FixScreenshotFormat;

procedure M_ScreenShot(const filename: string = ''; const silent: boolean = false);

procedure M_SetDefaults;

procedure M_SetDefault(const parm: string);

procedure M_LoadDefaults;

procedure M_SaveDefaults;

procedure Cmd_Set(const name: string; const value: string);

procedure Cmd_Get(const name: string);

procedure Cmd_TypeOf(const name: string);

var
  yesnoStrings: array[boolean] of string = ('NO', 'YES');
  truefalseStrings: array[boolean] of string = ('FALSE', 'TRUE');
  confignotfound: Boolean = true;

implementation

uses
  d_delphi,
  c_cmds,
  doomdef,
  {$IFDEF  DOOM}
  doomstat,
  {$ENDIF}
  d_main,
  d_player,
  g_game,
  m_argv,
  m_base,
  m_defs,
  i_system,
  m_sshot_jpg,
  t_png,
{$IFDEF OPENGL}
  gl_main,
  gl_defs,
{$ELSE}
  i_video,
{$ENDIF}
  z_zone;

function M_WriteFile(const name: string; source: pointer; len: integer): boolean;
var
  handle: file;
  count: integer;
begin
  if not fopen(handle, name, fCreate) then
  begin
    Result := false;
    exit;
  end;

  BlockWrite(handle, source^, len, count);
  close(handle);

  Result := count > 0;
end;

function M_AppendFile(const name: string; source: pointer; len: integer): integer;
var
  handle: TFile;
begin
  if not fexists(name) then
  begin
    if M_WriteFile(name, source, len) then
      Result := len
    else
      Result := 0;
    exit;
  end;

  if len > 0 then
  begin
    handle := TFile.Create(name, fOpenReadWrite);
    handle.Seek(handle.Size, sFromBeginning);
    Result := handle.Write(source^, len);
    handle.Free;
  end
  else
    Result := 0;
end;

function M_ReadFile(const name: string; var buffer: Pointer): integer;
var
  handle: file;
  count: integer;
begin
  if not fopen(handle, name, fOpenReadOnly) then
    I_Error('M_ReadFile(): Could not read file %s', [name]);

  Result := FileSize(handle);
  // JVAL
  // If Z_Malloc changed to malloc() a lot of changes must be made....
  buffer := Z_Malloc(Result, PU_STATIC, nil);
  BlockRead(handle, buffer^, Result, count);
  close(handle);

  if count < Result then
    I_Error('M_ReadFile(): Could not read file %s', [name]);
end;

type
  TargaHeader = record
    id_length, colormap_type, image_type: byte;
    colormap_index, colormap_length: word;
    colormap_size: byte;
    x_origin, y_origin, width, height: word;
    pixel_size, attributes: byte;
  end;

const
  MSG_ERR_SCREENSHOT = 'Couldn''t create a screenshot';

//
// M_ScreenShot
//
procedure M_FixScreenshotFormat;
begin
  screenshotformat := strupper(strtrim(screenshotformat));
  if Pos('.', screenshotformat) = 1 then
    Delete(screenshotformat, 1, 1);
  if screenshotformat = '' then
  begin
    screenshotformat := 'PNG';
    exit;
  end;
  if screenshotformat <> 'PNG' then
    if screenshotformat <> 'JPG' then
      if screenshotformat <> 'TGA' then
        screenshotformat := 'PNG';
end;

function M_DoScreenShotTGA(const filename: string): boolean;
var
  buffer: PByteArray;
  bufsize: integer;
  src: PByteArray;
  i: integer;
begin
  bufsize := SCREENWIDTH * SCREENHEIGHT * 4 + 18;
  buffer := malloc(bufsize);
  ZeroMemory(buffer, 18);
  buffer[2] := 2;    // uncompressed type
  buffer[12] := SCREENWIDTH and 255;
  buffer[13] := SCREENWIDTH div 256;
  buffer[14] := SCREENHEIGHT and 255;
  buffer[15] := SCREENHEIGHT div 256;
  buffer[16] := 32;  // pixel size
  buffer[17] := {$IFDEF OPENGL}0{$ELSE}32{$ENDIF};  // Origin in upper left-hand corner.

  src := @buffer[18];

  I_ReadScreen32(src);

  // JVAL 21/4/2017: Thanks Vladimir :)
  for i := 0 to SCREENWIDTH * SCREENHEIGHT - 1 do
    src[i * 4 + 3] := 255;

  Result := M_WriteFile(filename, buffer, SCREENWIDTH * SCREENHEIGHT * 4 + 18);

  memfree(pointer(buffer), bufsize);
end;

function M_DoScreenShotPNG(const filename: string): boolean;
var
  png: TPngObject;
  r, c: integer;
  lpng, lsrc: PByteArray;
  bufsize: integer;
  buf: PByteArray;
begin
  bufsize := SCREENWIDTH * SCREENHEIGHT * 4;
  buf := malloc(bufsize);
  I_ReadScreen32(buf);
  png := TPngObject.CreateBlank(COLOR_RGB, 8, SCREENWIDTH, SCREENHEIGHT);
  try
    for r := 0 to SCREENHEIGHT - 1 do
    begin
      lpng := png.Scanline[{$IFDEF OPENGL}SCREENHEIGHT - 1 - {$ENDIF}r];
      lsrc := @buf[r * SCREENWIDTH * 4];
      for c := 0 to SCREENWIDTH - 1 do
      begin
        lpng[c * 3] := lsrc[c * 4];
        lpng[c * 3 + 1] := lsrc[c * 4 + 1];
        lpng[c * 3 + 2] := lsrc[c * 4 + 2];
      end;
    end;
    png.SaveToFile(filename);
    result := png.IOresult = '';
  finally
    png.Free;
  end;
  memfree(pointer(buf), bufsize);
end;

procedure M_ScreenShot(const filename: string = ''; const silent: boolean = false);
var
  tganame: string;
  basetganame: string;
  jpgname: string;
  pngname: string;
  gstr: string;
  l: integer;
  ret: boolean;
  dir: string;
  date: TDateTime;
  dfmt: string;
  sctype: string;
begin
  M_FixScreenshotFormat;
  dir := M_SaveFileName('DATA');
  MkDir(dir);
  MkDir(dir + '\SCREENSHOTS');
  MkDir(dir + '\SCREENSHOTS\TGA');
  MkDir(dir + '\SCREENSHOTS\JPG');
  MkDir(dir + '\SCREENSHOTS\PNG');
  if filename = '' then
  begin
    {$IFDEF DOOM}
    if customgame in [cg_chex, cg_chex2] then
      gstr := 'CHEX'
    else if customgame = cg_hacx then
      gstr := 'HACX'
    else
    {$ENDIF}
    gstr := _GAME;

    basetganame := 'SSHOT_' + gstr + '_';

    tganame := M_SaveFileName('DATA\SCREENSHOTS\TGA\' + basetganame);
    jpgname := M_SaveFileName('DATA\SCREENSHOTS\JPG\' + basetganame);
    pngname := M_SaveFileName('DATA\SCREENSHOTS\PNG\' + basetganame);
    date := NowTime;
    dfmt := formatDateTimeAsString('yyyymmdd', date) + '_' + formatDateTimeAsString('hhnnsszzz', date);
    tganame := tganame + dfmt + '.tga';
    jpgname := jpgname + dfmt + '.jpg';
    pngname := pngname + dfmt + '.png';

    sctype := screenshotformat;
  end
  else
  begin
    if strupper(fext(filename)) = '.TGA' then
    begin
      sctype := 'TGA';
      tganame := filename;
      jpgname := filename;
      pngname := filename;
      l := length(filename);
      jpgname[l - 2] := 'j';
      jpgname[l - 1] := 'p';
      jpgname[l] := 'g';
      pngname[l - 2] := 'p';
      pngname[l - 1] := 'n';
      pngname[l] := 'g';
    end
    else if strupper(fext(filename)) = '.JPG' then
    begin
      sctype := 'JPG';
      tganame := filename;
      jpgname := filename;
      pngname := filename;
      l := length(filename);
      tganame[l - 2] := 't';
      tganame[l - 1] := 'g';
      tganame[l] := 'a';
      pngname[l - 2] := 'p';
      pngname[l - 1] := 'n';
      pngname[l] := 'g';
    end
    else if strupper(fext(filename)) = '.PNG' then
    begin
      sctype := 'PNG';
      tganame := filename;
      jpgname := filename;
      pngname := filename;
      l := length(filename);
      tganame[l - 2] := 't';
      tganame[l - 1] := 'g';
      tganame[l] := 'a';
      jpgname[l - 2] := 'j';
      jpgname[l - 1] := 'p';
      jpgname[l] := 'g';
    end
    else
    begin
      sctype := 'PNG';
      tganame := filename + '.tga';
      jpgname := filename + '.jpg';
      pngname := filename + '.png';
    end;

    if Pos('/', tganame) = 0 then
      if Pos('\', tganame) = 0 then
        tganame := M_SaveFileName('DATA\SCREENSHOTS\TGA\' + tganame);
    if Pos('/', jpgname) = 0 then
      if Pos('\', jpgname) = 0 then
        jpgname := M_SaveFileName('DATA\SCREENSHOTS\JPG\' + jpgname);
    if Pos('/', pngname) = 0 then
      if Pos('\', pngname) = 0 then
        jpgname := M_SaveFileName('DATA\SCREENSHOTS\PNG\' + pngname);
  end;

  if sctype = 'TGA' then
  begin
    ret := M_DoScreenShotTGA(tganame);
    if ret and mirrorjpgsshot then
      TGAtoJPG(tganame, jpgname);
  end
  else if sctype = 'JPG' then
  begin
    ret := M_DoScreenShotTGA(tganame);
    if ret then
    begin
      TGAtoJPG(tganame, jpgname);
      fdelete(tganame);
    end;
  end
  else
  begin
    ret := M_DoScreenShotPNG(pngname);
    if ret and mirrorjpgsshot then
    begin
      ret := M_DoScreenShotTGA(tganame);
      if ret then
      begin
        TGAtoJPG(tganame, jpgname);
        fdelete(tganame);
      end;
    end;
  end;

  if not silent then
  begin
    if ret then
      players[consoleplayer]._message := 'screen shot'
    else
      players[consoleplayer]._message := MSG_ERR_SCREENSHOT;
  end;
end;

procedure Cmd_Set(const name: string; const value: string);
var
  i: integer;
  pd: Pdefault_t;
  cname: string;
  cmd: cmd_t;
  clist: TDStringList;
  rlist: TDStringList;
  setflags: byte;
begin
  if netgame then
    setflags := DFS_NETWORK
  else
    setflags := DFS_SINGLEPLAYER;

  if name = '' then
  begin
    printf('Usage is:'#13#10'set [name] [value]'#13#10);
    printf(' Configures the following settings:'#13#10);
    pd := @defaults[0];
    for i := 0 to NUMDEFAULTS - 1 do
    begin
      if pd._type <> tGroup then
        if pd.setable and setflags <> 0 then
          printf('  %s'#13#10, [pd.name]);
      inc(pd);
    end;
    exit;
  end;

  if pos('*', name) > 0 then // Is a mask
  begin
    clist := TDStringList.Create;
    try
      pd := @defaults[0];
      for i := 0 to NUMDEFAULTS - 1 do
      begin
        if pd._type <> tGroup then
          if pd.setable and setflags <> 0 then
            clist.Add(pd.name);
        inc(pd);
      end;

      rlist := C_GetMachingList(clist, name);
      try
        for i := 0 to rlist.Count - 1 do
          printf('%s'#13#10, [rlist[i]]);
      finally
        rlist.Free;
      end;
    finally
      clist.Free;
    end;
    exit;
  end;

  if value = '' then
  begin
    printf('Please give the value to set %s'#13#10, [name]);
    exit;
  end;

  cname := strlower(name);

  pd := @defaults[0];
  for i := 0 to NUMDEFAULTS - 1 do
  begin
    if pd._type <> tGroup then
    begin
      if pd.name = cname then
      begin
        if pd.setable and setflags <> 0 then
        begin
          if pd._type = tInteger then
            PInteger(pd.location)^ := atoi(value)
          else if pd._type = tBoolean then
            PBoolean(pd.location)^ := C_BoolEval(value, PBoolean(pd.location)^)
          else if pd._type = tString then
            PString(pd.location)^ := value;
        end
        else
        begin
          if pd.setable = DFS_NEVER then
            I_Warning('Can not set readonly variable: %s'#13#10, [name])
          else if pd.setable = DFS_SINGLEPLAYER then
            I_Warning('Can not set variable: %s during network game'#13#10, [name]);
        end;
        exit;
      end;
    end;
    inc(pd);
  end;

  if C_GetCmd(name, cmd) then
    if C_ExecuteCmd(@cmd, value) then
      exit;

  C_UnknowCommandMsg;
end;

procedure Cmd_Get(const name: string);
var
  i: integer;
  pd: Pdefault_t;
  cname: string;
  cmd: cmd_t;
  clist: TDStringList;
  rlist: TDStringList;
begin
  if name = '' then
  begin
    printf('Usage is:'#13#10'get [name]'#13#10);
    printf(' Display the current settings of:'#13#10);
    pd := @defaults[0];
    for i := 0 to NUMDEFAULTS - 1 do
    begin
      if pd._type <> tGroup then
        printf('  %s'#13#10, [pd.name]);
      inc(pd);
    end;
    exit;
  end;

  if pos('*', name) > 0 then // Is a mask
  begin
    clist := TDStringList.Create;
    try
      pd := @defaults[0];
      for i := 0 to NUMDEFAULTS - 1 do
      begin
        if pd._type <> tGroup then
          clist.Add(pd.name);
        inc(pd);
      end;

      rlist := C_GetMachingList(clist, name);
      try
        for i := 0 to rlist.Count - 1 do
          Cmd_Get(rlist[i]);
      finally
        rlist.Free;
      end;
    finally
      clist.Free;
    end;
    exit;
  end;

  cname := strlower(name);

  pd := @defaults[0];
  for i := 0 to NUMDEFAULTS - 1 do
  begin
    if pd._type <> tGroup then
    begin
      if pd.name = cname then
      begin
        if pd._type = tInteger then
          printf('%s=%d'#13#10, [name, PInteger(pd.location)^])
        else if pd._type = tBoolean then
        begin
          if PBoolean(pd.location)^ then
            printf('%s=ON'#13#10, [name])
          else
            printf('%s=OFF'#13#10, [name])
        end
        else if pd._type = tString then
          printf('%s=%s'#13#10, [name, PString(pd.location)^]);
        exit;
      end;
    end;
    inc(pd);
  end;

  if C_GetCmd(name, cmd) then
    if C_ExecuteCmd(@cmd) then
      exit;

  C_UnknowCommandMsg;
end;

procedure Cmd_TypeOf(const name: string);
var
  i: integer;
  pd: Pdefault_t;
  cname: string;
  clist: TDStringList;
  rlist: TDStringList;
begin
  if name = '' then
  begin
    printf('Usage is:'#13#10'typeof [name]'#13#10);
    printf(' Display the type of variable.'#13#10);
  end;

  if pos('*', name) > 0 then // Is a mask
  begin
    clist := TDStringList.Create;
    try
      pd := @defaults[0];
      for i := 0 to NUMDEFAULTS - 1 do
      begin
        if pd._type <> tGroup then
          clist.Add(pd.name);
        inc(pd);
      end;

      rlist := C_GetMachingList(clist, name);
      try
        for i := 0 to rlist.Count - 1 do
          Cmd_TypeOf(rlist[i]);
      finally
        rlist.Free;
      end;
    finally
      clist.Free;
    end;
    exit;
  end;

  cname := strlower(name);

  pd := @defaults[0];
  for i := 0 to NUMDEFAULTS - 1 do
  begin
    if pd._type <> tGroup then
    begin
      if pd.name = cname then
      begin
        if pd._type = tInteger then
          printf('%s is integer'#13#10, [name])
        else if pd._type = tBoolean then
          printf('%s is boolean'#13#10, [name])
        else if pd._type = tString then
          printf('%s is string'#13#10, [name]);
        exit;
      end;
    end;
    inc(pd);
  end;

  printf('Unknown variable: %s'#13#10, [name]);
end;

const
  VERFMT = 'ver %d.%.*d';

var
  defaultfile: string;


procedure M_SaveDefaults;
var
  i: integer;
  pd: Pdefault_t;
  s: TDStringList;
  verstr: string;
begin
  s := TDStringList.Create;
  try
    sprintf(verstr, '[' + AppTitle + ' ' + VERFMT + ']', [VERSION div 100, 2, VERSION mod 100]);
    s.Add(verstr);
    pd := @defaults[0];
    for i := 0 to NUMDEFAULTS - 1 do
    begin
      if pd._type = tInteger then
        s.Add(pd.name + '=' + itoa(PInteger(pd.location)^))
      else if pd._type = tString then
        s.Add(pd.name + '=' + PString(pd.location)^)
      else if pd._type = tBoolean then
        s.Add(pd.name + '=' + itoa(intval(PBoolean(pd.location)^)))
      else if pd._type = tGroup then
      begin
        s.Add('');
        s.Add('[' + pd.name + ']');
      end;
      inc(pd);
    end;

    s.SaveToFile(defaultfile);

  finally
    s.Free;
  end;
end;

procedure M_SetDefaults;
begin
  M_SetDefault('*');
end;

procedure M_SetDefault(const parm: string);
var
  i: integer;
  def: string;
  parm1: string;
  pd: Pdefault_t;
  clist: TDStringList;
  rlist: TDStringList;
  setflags: byte;
begin
  // set parm1 to base value
  if parm = '' then
  begin
    printf('Please specify the variable to reset to default value'#13#10);
    exit;
  end;

  if netgame then
    setflags := DFS_NETWORK
  else
    setflags := DFS_SINGLEPLAYER;

  if pos('*', parm) > 0 then // Is a mask
  begin
    clist := TDStringList.Create;
    try
      pd := @defaults[0];
      for i := 0 to NUMDEFAULTS - 1 do
      begin
        if pd._type <> tGroup then
          clist.Add(pd.name);
        inc(pd);
      end;

      rlist := C_GetMachingList(clist, parm);
      try
        for i := 0 to rlist.Count - 1 do
          M_SetDefault(rlist[i]);
      finally
        rlist.Free;
      end;
    finally
      clist.Free;
    end;
    exit;
  end;

  def := strlower(parm);
  for i := 0 to NUMDEFAULTS - 1 do
    if defaults[i].name = def then
    begin
      if defaults[i].setable and setflags <> 0 then
      begin
        if defaults[i]._type = tInteger then
          PInteger(defaults[i].location)^ := defaults[i].defaultivalue
        else if defaults[i]._type = tBoolean then
          PBoolean(defaults[i].location)^ := defaults[i].defaultbvalue
        else if defaults[i]._type = tString then
          PString(defaults[i].location)^ := defaults[i].defaultsvalue
        else
          exit; // Ouch!
        printf('Setting default value for %s'#13#10, [parm]);
        Cmd_Get(def); // Display the default value
      end
      else if C_CmdExists(def) then
      begin
        if defaults[i]._type = tInteger then
          parm1 := itoa(defaults[i].defaultivalue)
        else if defaults[i]._type = tBoolean then
          parm1 := yesnostrings[defaults[i].defaultbvalue]
        else if defaults[i]._type = tString then
          parm1 := defaults[i].defaultsvalue
        else
          exit; // Ouch!
        printf('Setting default value for %s'#13#10, [parm]);
        C_ExecuteCmd(def, parm1);
      end;
    end;
end;

procedure M_LoadDefaults;
var
  i: integer;
  j: integer;
  idx: integer;
  pd: Pdefault_t;
  s: TDStringList;
  n: string;
  verstr: string;
  ver: integer;
  acceptablever: Boolean;
  stmp: string;
begin
  // set everything to base values
  for i := 0 to NUMDEFAULTS - 1 do
    if defaults[i]._type = tInteger then
      PInteger(defaults[i].location)^ := defaults[i].defaultivalue
    else if defaults[i]._type = tBoolean then
      PBoolean(defaults[i].location)^ := defaults[i].defaultbvalue
    else if defaults[i]._type = tString then
      PString(defaults[i].location)^ := defaults[i].defaultsvalue;

  if M_CheckParm('-defaultvalues') > 0 then
    exit;

  // check for a custom default file
  i := M_CheckParm('-config');
  if (i > 0) and (i < myargc - 1) then
  begin
    defaultfile := myargv[i + 1];
    printf(' default file: %s'#13#10, [defaultfile]);
  end
  else
    defaultfile := basedefault;

  s := TDStringList.Create;
  try
    // read the file in, overriding any set defaults
    if fexists(defaultfile) then
      s.LoadFromFile(defaultfile);

    if s.Count > 1 then
    begin
      acceptablever := False;
      for ver := VERSION + 1 downto 110 do  // JVAL: 20210122 - Accept next version config
      begin
        sprintf(verstr, VERFMT, [ver div 100, 2, ver mod 100]);
        if Pos(verstr, s[0]) > 0 then
        begin
          acceptablever := True;
          break;
        end;
      end;
      if acceptablever then
      begin
        s.Delete(0);
        confignotfound := False;

        for i := 0 to s.Count - 1 do
        begin
          idx := -1;
          n := strlower(s.Names[i]);
          for j := 0 to NUMDEFAULTS - 1 do
            if defaults[j].name = n then
            begin
              idx := j;
              break;
            end;

          if idx > -1 then
          begin
            pd := @defaults[idx];
            if pd._type = tInteger then
            begin
              stmp := s.Values[n];
              PInteger(pd.location)^ := atoi(stmp);
              // JVAL: 20151116 hack to easy set key values inside config file
              if length(stmp) = 3 then
                if (stmp[1] in ['''', '"']) and (stmp[3] in ['''', '"']) then
                  if stmp[1] = stmp[3] then // JVAL 20180101 Same container quotes
                  begin
                    if stmp[2] in ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] then
                      PInteger(pd.location)^ := atoi(stmp[2])
                    else
                      PInteger(pd.location)^ := Ord(stmp[2]);
                  end;
              if length(stmp) = 1 then
                if not (stmp[1] in ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0']) then
                  PInteger(pd.location)^ := Ord(stmp[1]);
            end
            else if pd._type = tBoolean then
              PBoolean(pd.location)^ := atoi(s.Values[n]) <> 0
            else if pd._type = tString then
              PString(pd.location)^ := s.Values[n];
          end;
        end;
      end;
    end;

  finally
    s.Free;
  end;
  {$IFNDEF STRIFE}
  if confignotfound then
    G_SetKeyboardMode(1);
  {$ENDIF}
  {$IFNDEF DEBUG}
  {$IFDEF OPENGL}
  gl_drawsky := true;
  {$ENDIF}
  {$ENDIF}
end;

end.

