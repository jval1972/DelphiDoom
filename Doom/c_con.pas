//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2008 by Jim Valavanis
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
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://delphidoom.sitesled.com/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit c_con;

//
// Console
//

interface

uses
  d_event;

procedure C_Init;

procedure C_ShutDown;

procedure C_AddLine(const line: string; len: integer = -1);

procedure C_AddText(const txt: string);

procedure C_Drawer;

procedure C_Ticker;

function C_Responder(ev: Pevent_t): boolean;

function C_IsConsoleActive: boolean;

function C_ConsoleHeight: integer;

function C_ConsolePos: integer;

procedure C_AddDivideLine;

var
  ConsoleColormap: integer;

var
  mirror_stdout: boolean;
  autoexecfile: string;

const
  DEFAUTOEXEC = 'Doom32.con';

implementation

uses
  d_delphi,
  doomdef,
  c_cmds, c_utils,
  d_main,
  g_game,
  hu_stuff,
  m_argv, m_fixed,
  i_io, i_system,
  r_defs,
  r_main,
  
  v_data, v_video,
  w_utils, w_wad,
  z_zone;

const
  CONSOLE_PROMPTCHAR: string = ']';
  MAX_CONSOLE_INPUT_LEN = 1024;
  MAX_CONSOLE_LINES = 256; //must be power of 2
  CONSOLETEXT_MASK = MAX_CONSOLE_LINES - 1;
  CMD_HISTORY_SIZE = 64;

type
  conline_t = record
    line: string;
  end;
  Pconline_t = ^conline_t;

  consolestate_t = (
    CST_UP,
    CST_RAISE,
    CST_LOWER,
    CST_DOWN
  );

var
  consoleraiseticks: integer = 0;
  consolelowerticks: integer = 0;

var
  ConsoleText: array[0..MAX_CONSOLE_LINES - 1] of conline_t;
  ConsoleHead: integer;
  ConsoleWidth: integer;      //chars
  ConsoleHeight: integer = 0; //lines
  ConsolePos: integer = 0;    //bottom of console, in pixels
  MaxConsolePos: integer;
  ConsoleYFrac: integer = 0;
  ConsoleLineBuffer: string = '';
  ConsoleState: consolestate_t;
  ConsoleInputBuff: string;
  CommandsHistory: array[0..CMD_HISTORY_SIZE - 1] of string;
  PrevCommandHead: integer;
  NextCommand: integer;
  con_needsupdate: boolean;
  divideline: string;

var
  ConsoleInitialized: boolean = false;

const
  C_FONTWIDTH = 8;
  C_FONTHEIGHT = 8;

function isDivideLine(const s: string): boolean;
var
  i: integer;
begin
  result := Length(s) > 3;
  if result then
    for i := 1 to Length(s) do
      if not (s[i] in ['-', '=']) then
      begin
        result := false;
        exit;
      end;
end;

//
// C_ResetInputBuff
//
procedure C_ResetInputBuff;
begin
  ConsoleInputBuff := CONSOLE_PROMPTCHAR;
  con_needsupdate := true;
end;

function C_IsInputBuffEmpty: boolean;
begin
  result := ConsoleInputBuff = CONSOLE_PROMPTCHAR;
end;

//
// Commands
//
procedure C_CmdCloseConsole(const parm: string);
var
  ticks: integer;
begin
  ticks := atoi(parm);
  if ticks <= 0 then
    ConsoleState := CST_RAISE
  else
    consoleraiseticks := ticks;
end;

procedure C_CmdOpenConsole(const parm: string);
var
  ticks: integer;
begin
  ticks := atoi(parm);
  if ticks <= 0 then
    ConsoleState := CST_LOWER
  else
    consolelowerticks := ticks
end;

procedure C_CmdConsoleColormap(const parm1: string);
var
  c: integer;
begin
  c := atoi(parm1);
  if (parm1 = '') or (c < 0) or (c >= NUMCOLORMAPS) then
  begin
    printf('Please specify a parameter in range [0..%d]'#13#10, [NUMCOLORMAPS - 1]);
    printf('Current console colormap: %d'#13#10, [ConsoleColormap]);
  end
  else
    ConsoleColormap := c;
end;

procedure C_CmdCls;
var
  i: integer;
begin
  for i := 0 to MAX_CONSOLE_LINES - 1 do
    ConsoleText[i].line := '';
  C_ResetInputBuff;
end;

var
  execs: TDStringList = nil;

procedure C_ExecCommandFile(const filename: string);
var
  fname: string;
  cmd: string;
  t: text;
begin
  if filename = '' then
  begin
    printf('Usage:'#13#10' exec [command file(*.con)]'#13#10);
    exit;
  end;

  fname := fexpand(filename);
  if not fexists(fname) then
  begin
    fname := fname + '.CON';
    if not fexists(fname) then
    begin
      printf(' Command file not found: %s'#13#10, [filename]);
      exit;
    end;
  end;

  fname := strupper(fname);

  if execs = nil then
    execs := TDStringList.Create
  else if execs.IndexOf(fname) >= 0 then
  begin
    I_Warning(' Recursive calls of con files is not allowed(%s)'#13#10, [filename]);
    exit;
  end;
  execs.Add(fname);

  printf(' Running command file: %s'#13#10, [fname]);

  {$I-}
  assign(t, fname);
  FileMode := 0;
  reset(t);
  while not EOF(t) and (IOResult = 0) do
  begin
    readln(t, cmd);
    cmd := strtrim(cmd);
    if cmd <> '' then
      if Pos('//', cmd) <> 1 then
        C_ExecuteCmd(cmd);
  end;
  close(t);
  {$I+}
  execs.Delete(execs.IndexOf(fname));
end;

procedure C_CmdPrintf(const parm1, parm2: string);
begin
  if parm2 = '' then
    printf(parm1 + #13#10)
  else
    printf(parm1 + ' ' + parm2 + #13#10);
end;

var
  console_paused: boolean = false;

procedure C_CmdPauseConsole;
begin
  console_paused := true;
end;


procedure Cmd_Use(const parm1, parm2: string);
begin
  if parm1 = '' then
  begin
    C_ExecuteCmd('cmdlist', 'use*');
    exit;
  end;

  if not C_ExecuteCmd('use' + parm1, parm2) then
    printf('%s mnemonic not found!');
end;


//
// C_Init
//
procedure C_Init;
var
  i: integer;
begin
  ConsoleHead := 0;
  ConsoleState := CST_UP;
  for i := 0 to MAX_CONSOLE_LINES - 1 do
    ConsoleText[i].line := '';
  C_ResetInputBuff;
  ConsoleYFrac := V_GetScreenHeight(SCN_CON) div 20;
  MaxConsolePos := ConsoleYFrac * 11;
  ConsoleWidth := V_GetScreenWidth(SCN_CON) div C_FONTWIDTH - 2;
  divideline := '';
  for i := 1 to ConsoleWidth do
    divideline := divideline + '-';
  divideline := divideline + #13#10;
  for i := 0 to CMD_HISTORY_SIZE - 1 do
    CommandsHistory[i] := '';
  PrevCommandHead := 0;
  NextCommand := 0;
  ConsoleInitialized := true;

  C_AddText(stdoutbuffer.Text);
  outproc := C_AddText;

  C_AddCmd('closeconsole', @C_CmdCloseConsole);
  C_AddCmd('openconsole', @C_CmdOpenConsole); // For autoexec
  C_AddCmd('consolecolormap', @C_CmdConsoleColormap);
  C_AddCmd('cls, clearscreen', @C_CmdCls);
  C_AddCmd('cmdlist, list, listcmds', @C_CmdList);
  C_AddCmd('screenshot', @G_ScreenShot);
  C_AddCmd('playdemo', @G_CmdPlayDemo);
  C_AddCmd('start, playgame, engage', @G_CmdNewGame);
  C_AddCmd('load, loadgame', @G_LoadGame);
  C_AddCmd('save, savegame', @G_CmdSaveGame);
  C_AddCmd('exec, execcommandfile', @C_ExecCommandFile);
  C_AddCmd('printf, write, writeln', @C_CmdPrintf);  // Mostly for autoexec
  C_AddCmd('pause_console, pauseconsole', @C_CmdPauseConsole);
  C_AddCmd('commandlineparams', @M_CmdShowCommandLineParams);
  C_AddCmd('cmdline', @M_CmdShowCmdline);
  C_AddCmd('use', @Cmd_Use);
  C_RegisterUtilityCommands;
  W_RegisterUtilityCommands;
end;

procedure C_ShutDown;
begin
  if execs <> nil then
    execs.Free;
end;

var
  consolebuffer: string;

procedure C_AddLine(const line: string; len: integer = -1);
var
  cline: string;
  i, j: integer;
begin
  if not ConsoleInitialized then
    exit; //not initialised yet

  if line = '' then
    cline := ' '
  else
  begin
    if len = -1 then
      len := Length(line);
    SetLength(cline, len);
    for i := 1 to len do
      cline[i] := ' ';
    j := 0;
    for i := 1 to len do
    begin
      if line[i] = #8 then
        inc(j, 2)
      else
        cline[i - j] := line[i];
    end;
    SetLength(cline, len - j div 2);
  end;
  if console_paused then
  begin
    consolebuffer := consolebuffer + cline + #13#10;
  end
  else
  begin
    ConsoleHead := (ConsoleHead + CONSOLETEXT_MASK) and CONSOLETEXT_MASK;
    ConsoleText[ConsoleHead].line := cline;
    i := (ConsoleHead + CONSOLETEXT_MASK) and CONSOLETEXT_MASK;
    ConsoleText[i].line := '';
    con_needsupdate := true;
  end;
end;

procedure C_AddText(const txt: string);
var
  i: integer;
  c: char;
begin
  if not ConsoleInitialized then
    exit; //not initialised yet

  for i := 1 to Length(txt) do
  begin
    c := txt[i];
    if c = #13 then
      C_AddLine(ConsoleLineBuffer)
    else if c = #10 then
      ConsoleLineBuffer := ''
    else
      ConsoleLineBuffer := ConsoleLineBuffer + c;
  end;
  if mirror_stdout then
    fprintf(stdout, txt);
end;

procedure C_RunAutoExec;
var
  str: TDstringList;
  deffile: string;
begin
  printf('C_RunAutoExec()'#13#10);
  if not fexists(DEFAUTOEXEC) then
  begin
    str := TDStringList.Create;
    try
      str.Add('// Add autoexec console commands in this file.');
      str.Add('// These console commands will be executed everytime you start DelphiDoom.');
      deffile := M_SaveFileName(DEFAUTOEXEC);
      str.SaveToFile(deffile);
    finally
      str.Free;
    end;
  end;
  C_ExecCommandFile(autoexecfile);
end;

var
  shiftdown: boolean = false;
  firsttime: boolean = true;

function C_Responder(ev: Pevent_t): boolean;
var
  c: integer;
begin
  if not ConsoleInitialized then
  begin
    result := false;
    exit; //not initialised yet
  end;

  if firsttime then
  begin
    C_RunAutoExec;
    firsttime := false;
  end;

  if (ev._type <> ev_keyup) and (ev._type <> ev_keydown) then
  begin
    result := false;
    exit;
  end;

  c := ev.data1;
  if c = KEY_RSHIFT then
    shiftdown := ev._type = ev_keydown;

  case ConsoleState of
    CST_DOWN,
    CST_LOWER:
      begin
        if ev._type = ev_keydown then
        begin
          if console_paused then
          begin
            console_paused := false;
            C_AddText(consolebuffer);
            consolebuffer := '';
          end
          else
          case c of
            KEY_CON:
              begin
                ConsoleState := CST_RAISE;
              end;
            KEY_ESCAPE:
              begin
                if not C_IsInputBuffEmpty then
                  C_ResetInputBuff
                else
                  ConsoleState := CST_RAISE
              end;
            KEY_ENTER:
              begin
                if not C_IsInputBuffEmpty then
                begin
                  C_AddText(ConsoleInputBuff + #13#10);
                  CommandsHistory[PrevCommandHead] := ConsoleInputBuff;
                  inc(PrevCommandHead);
                  NextCommand := PrevCommandHead;
                  if PrevCommandHead >= CMD_HISTORY_SIZE then
                    PrevCommandHead := 0;
                  CommandsHistory[PrevCommandHead] := '';
                  if not C_ExecuteCmd(Copy(ConsoleInputBuff, Length(CONSOLE_PROMPTCHAR) + 1, Length(ConsoleInputBuff) - Length(CONSOLE_PROMPTCHAR))) then
                    C_UnknowCommandMsg;
                  C_ResetInputBuff;
                end;
              end;
            KEY_UPARROW,
            KEY_DOWNARROW:
              begin
                if c = KEY_UPARROW then
                begin
                  c := NextCommand - 1;
                  if c < 0 then
                    c := CMD_HISTORY_SIZE - 1
                end
                else
                begin
                  c := NextCommand + 1;
                  if c >= CMD_HISTORY_SIZE then
                    c := 0;
                end;
                if CommandsHistory[c] <> '' then
                begin
                  ConsoleInputBuff := CommandsHistory[c];
                  NextCommand := c;
                  con_needsupdate := true;
                end;
              end;
            KEY_BACKSPACE:
              begin
                if not C_IsInputBuffEmpty then
                begin
                  SetLength(ConsoleInputBuff, Length(ConsoleInputBuff) - 1);
                  con_needsupdate := true;
                end;
              end;
          else
            begin
              c := Ord(toupper(Chr(c)));
              if ((c >= Ord(HU_FONTSTART)) and (c <= Ord(HU_FONTEND))) or
                 (Chr(c) in [' ', '.', '!', '-', '+', '=', '*', '/', '\']) then
              begin
                if shiftdown then
                  c := Ord(shiftxform[c]);
                ConsoleInputBuff := ConsoleInputBuff + Chr(c);
                con_needsupdate := true;
              end;
            end;
          end;
          result := true;
          exit;
        end;
      end;
    CST_UP,
    CST_RAISE:
      begin
        if c = Ord('~') then
        begin
          if ev._type = ev_keydown then
          begin
            ConsoleState := CST_LOWER;
            result := true;
            exit;
          end;
        end;
      end;
  end;

  if (ev._type = ev_keydown) and ((devparm and (c = KEY_F1)) or (c = KEY_PRNT)) then
  begin
    G_ScreenShot;
    result := true;
    exit;
  end;

  result := false;
end;

const
  C_BLINKRATE = TICRATE * 3 div 4;

var
  cursonon: boolean = false;
  cursorticker: integer = C_BLINKRATE;
  cursor_x: integer = 0;
  cursor_y: integer = 0;
  cursor_needs_update: boolean = true;

procedure C_Ticker;
begin
  if not ConsoleInitialized then
    exit; //not initialised yet

  if consoleraiseticks > 0 then
  begin
    dec(consoleraiseticks);
    if consoleraiseticks = 0 then
      ConsoleState := CST_RAISE;
  end;

  if consolelowerticks > 0 then
  begin
    dec(consolelowerticks);
    if consolelowerticks = 0 then
      ConsoleState := CST_LOWER;
  end;

  case ConsoleState of
    CST_LOWER:
      begin
        inc(ConsolePos, ConsoleYFrac);
        if ConsolePos >= MaxConsolePos then
        begin
          ConsoleState := CST_DOWN;
          MaxConsolePos := (1 + ConsoleHeight) * C_FONTHEIGHT;
          ConsolePos := MaxConsolePos; // Crop MaxConsolePos
        end
        else
          ConsoleHeight := ConsolePos div C_FONTHEIGHT;
        con_needsupdate := true;
      end;
    CST_RAISE:
      begin
        if ConsolePos < MaxConsolePos div 5 then
          dec(ConsolePos, ConsoleYFrac)
        else
          dec(ConsolePos, 2 * ConsoleYFrac); // Raise console faster...
        if ConsolePos <= 0 then
        begin
          ConsoleState := CST_UP;
          ConsolePos := 0;
          ConsoleHeight := 0;
        end
        else
          ConsoleHeight := ConsolePos div C_FONTHEIGHT;
        con_needsupdate := true;
      end;
  end;

// Decide of cursor visibility
  dec(cursorticker);
  if cursorticker = 0 then
  begin
    cursonon := not cursonon;
    cursorticker := C_BLINKRATE;
    if cursonon then
      cursor_needs_update := true
    else
      con_needsupdate := true;
  end;

end;

procedure C_DrawConsoleBackground;
begin
  V_DrawPatch(0, 0, SCN_TMP, pg_TITLE, false);

// A little bit darker background...
  R_ApplyColormap(0, 320 * 200, SCN_TMP, ConsoleColormap);

  V_CopyRect(0, 0, SCN_TMP, 320, 200, 0, 0, SCN_CON, true);
end;

const
  FIXED_PITCH_CHARS = ['1'..'9', '0', '.', '=', '-'];

procedure C_Drawer;
var
  line: integer;
  y: integer;
  i: integer;
  c: char;
  x: integer;
  len: integer;
  lnum: integer;
  xmax: integer;
  patch: Ppatch_t;
begin
  if not ConsoleInitialized then
    exit; //not initialised yet

  if ConsoleHeight <= 0 then
    exit;

  xmax := (ConsoleWidth + 1) * C_FONTWIDTH;
  
  if con_needsupdate then
  begin

    C_DrawConsoleBackground;

    line := ConsoleHead;
    lnum := ConsoleHeight;
    while (ConsoleText[line].line <> '') and (lnum > 0) do
    begin
      x := C_FONTWIDTH;
      len := Length(ConsoleText[line].line);
      lnum := lnum - (len - 1) div ConsoleWidth - 1;
      i := 1;
      if lnum < 0 then
      begin
        i := i - ConsoleWidth * lnum;
        lnum := 0;
      end;
      y := V_GetScreenHeight(SCN_CON) - (ConsoleHeight - lnum + 1) * C_FONTHEIGHT;
      if isDivideLine(ConsoleText[line].line) then
      begin
        patch := W_CacheLumpName('brdr_t', PU_STATIC);
        if len > ConsoleWidth then
          len := ConsoleWidth;
        for i := 1 to len do
        begin
          V_DrawPatch(x, y - 2, SCN_CON, patch, false);
          x := x + 8;
        end;
        Z_ChangeTag(patch, PU_CACHE);
      end
      else
        while i <= len do
        begin
          c := toupper(ConsoleText[line].line[i]);
          if (c >= HU_FONTSTART) and (c <= HU_FONTEND) then
          begin
            patch := hu_font[Ord(c) - Ord(HU_FONTSTART)];
            if c in FIXED_PITCH_CHARS then
            begin
              V_DrawPatch(x + C_FONTWIDTH - patch.width, y, SCN_CON, patch, false);
              x := x + C_FONTWIDTH;
            end
            else
            begin
              V_DrawPatch(x, y, SCN_CON, patch, false);
              x := x + patch.width + 1;
            end;
          end
          else
            x := x + C_FONTWIDTH;
          if x > xmax then
          begin
            x := C_FONTWIDTH;
            y := y + C_FONTHEIGHT;
          end;
          inc(i);
        end;
        line := (line + 1) and CONSOLETEXT_MASK;
      end;

    x := C_FONTWIDTH;
    y := V_GetScreenHeight(SCN_CON) - C_FONTHEIGHT;
    for i := 1 to Length(ConsoleInputBuff) do
    begin
      c := toupper(ConsoleInputBuff[i]);
      if (c >= HU_FONTSTART) and (c <= HU_FONTEND) and (x < xmax) then
      begin
        patch := hu_font[Ord(c) - Ord(HU_FONTSTART)];
        if c in FIXED_PITCH_CHARS then
        begin
          V_DrawPatch(x + C_FONTWIDTH - patch.width, y, SCN_CON, patch, false);
          x := x + C_FONTWIDTH;
        end
        else
        begin
          V_DrawPatch(x, y, SCN_CON, patch, false);
          x := x + patch.width + 1;
        end;
      end
      else
        x := x + C_FONTWIDTH;
    end;

    if cursonon and (x < xmax) then
    begin
      patch := hu_font[Ord('_') - Ord(HU_FONTSTART)];
      V_DrawPatch(x, y, SCN_CON, patch, false);
    end;
    cursor_x := x;
    cursor_y := y;

    con_needsupdate := false;
    cursor_needs_update := false;

  end
  else if cursor_needs_update then
  begin
    if cursonon and (cursor_x < xmax) then
    begin
      patch := hu_font[Ord('_') - Ord(HU_FONTSTART)];
      V_DrawPatch(cursor_x, cursor_y, SCN_CON, patch, false);
    end;
    cursor_needs_update := false;
  end;


  V_CopyAddRect(0, V_GetScreenHeight(SCN_CON) - ConsolePos, SCN_CON,
    V_GetScreenWidth(SCN_CON), ConsolePos, 0, 0, SCN_FG,
      false,
        (MaxConsolePos - ConsolePos) * FRACUNIT div MaxConsolePos);
end;

function C_IsConsoleActive: boolean;
begin
  result := ConsoleState <> CST_UP;
end;

function C_ConsoleHeight: integer;
begin
  result := ConsoleHeight;
end;

function C_ConsolePos: integer;
begin
  result := ConsolePos;
end;

procedure C_AddDivideLine;
begin
  C_AddText(divideline);
end;

end.

