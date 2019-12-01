//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2016 by Jim Valavanis
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
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit m_misc;

interface

//
// MISC 
//

function M_WriteFile(const name: string; source: pointer; length: integer): boolean;

function M_ReadFile(const name: string; var buffer: Pointer): integer;

procedure M_ScreenShot(const filename: string = ''; const silent: boolean = false);

function M_DoScreenShot(const filename: string): boolean;

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
  d_main,
  d_player,
  g_game,
  m_argv,
  m_defs,
  i_system,
{$IFDEF OPENGL}
  gl_main,
{$ELSE}
  i_video,
{$ENDIF}
  z_zone;

function M_WriteFile(const name: string; source: pointer; length: integer): boolean;
var
  handle: file;
  count: integer;
begin
  if not fopen(handle, name, fCreate) then
  begin
    result := false;
    exit;
  end;

  BlockWrite(handle, source^, length, count);
  close(handle);

  result := count > 0;
end;

function M_ReadFile(const name: string; var buffer: Pointer): integer;
var
  handle: file;
  count: integer;
begin
  if not fopen(handle, name, fOpenReadOnly) then
    I_Error('M_ReadFile(): Could not read file %s', [name]);

  result := FileSize(handle);
  // JVAL
  // If Z_Malloc changed to malloc() a lot of changes must be made....
  buffer := Z_Malloc(result, PU_STATIC, nil);
  BlockRead(handle, buffer^, result, count);
  close(handle);

  if count < result then
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
procedure M_ScreenShot(const filename: string = ''; const silent: boolean = false);
var
  tganame: string;
  i: integer;
  h: integer;
  len: integer;
  ret: boolean;
begin
  if filename = '' then
  begin
    tganame := M_SaveFileName(
                        {$IFDEF DOOM}'DOOM000.tga'{$ENDIF}
                        {$IFDEF HERETIC}'HTIC000.tga'{$ENDIF}
                        {$IFDEF HEXEN}'HEXN000.tga'{$ENDIF}
                        {$IFDEF STRIFE}'STRIF000.tga'{$ENDIF}
                      );
//
// find a file name to save it to
//

    len := length(tganame);
    i := 0;
    while i <= 999 do
    begin
      h := i div 100;
      tganame[len - 6] := Chr(h + Ord('0'));
      h := i mod 100;
      tganame[len - 5] := Chr((h div 10) + Ord('0'));
      tganame[len - 4] := Chr((h mod 10) + Ord('0'));
      if not fexists(tganame) then
        break;  // file doesn't exist
      inc(i);
    end;
    if i = 1000 then
    begin
      players[consoleplayer]._message := MSG_ERR_SCREENSHOT;
      exit;
    end;
  end
  else
  begin
    if Pos('.', filename) = 0 then
      tganame := filename + '.tga'
    else
      tganame := filename;
  end;

  ret := M_DoScreenShot(tganame);
  if not silent then
  begin
    if ret then
      players[consoleplayer]._message := 'screen shot'
    else
      players[consoleplayer]._message := MSG_ERR_SCREENSHOT;
  end;
end;

function M_DoScreenShot(const filename: string): boolean;
var
  buffer: PByteArray;
  bufsize: integer;
  src: PByteArray;
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

  result := M_WriteFile(filename, buffer, SCREENWIDTH * SCREENHEIGHT * 4 + 18);

  memfree(pointer(buffer), bufsize);
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
  VERFMT = 'ver %d.%d';

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
    sprintf(verstr, '[' + AppTitle + ' ' + VERFMT + ']', [VERSION div 100, VERSION mod 100]);
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
      sprintf(verstr, VERFMT, [VERSION div 100, VERSION mod 100]);
      if Pos(verstr, s[0]) > 0 then
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
              // jval: 20151116 hack to easy set key values inside config file
              if length(stmp) = 3 then
                if (stmp[1] in ['''', '"']) and (stmp[3] in ['''', '"']) then
                  if stmp[2] in ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'] then
                    PInteger(pd.location)^ := atoi(stmp[2])
                  else
                    PInteger(pd.location)^ := Ord(stmp[2]);
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
    G_SetKeyboardMode(0);
  {$ENDIF}
end;

end.

