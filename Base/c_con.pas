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
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit c_con;

//
// Console
//

interface

uses
  d_event;

//==============================================================================
//
// C_Init
//
//==============================================================================
procedure C_Init;

//==============================================================================
//
// C_ShutDown
//
//==============================================================================
procedure C_ShutDown;

//==============================================================================
//
// C_AdjustScreenSize
//
//==============================================================================
procedure C_AdjustScreenSize;

//==============================================================================
//
// C_AddCommand
//
//==============================================================================
procedure C_AddCommand(const cmd: string);

//==============================================================================
//
// C_AddText
//
//==============================================================================
procedure C_AddText(const txt: string);

//==============================================================================
//
// C_Drawer
//
//==============================================================================
procedure C_Drawer;

//==============================================================================
//
// C_Ticker
//
//==============================================================================
procedure C_Ticker;

//==============================================================================
//
// C_Responder
//
//==============================================================================
function C_Responder(ev: Pevent_t): boolean;

//==============================================================================
//
// C_IsConsoleActive
//
//==============================================================================
function C_IsConsoleActive: boolean;

//==============================================================================
//
// C_ConsoleHeight
//
//==============================================================================
function C_ConsoleHeight: integer;

//==============================================================================
//
// C_ConsolePos
//
//==============================================================================
function C_ConsolePos: integer;

//==============================================================================
//
// C_AddDivideLine
//
//==============================================================================
procedure C_AddDivideLine;

var
  ConsoleColormap: integer;

var
  mirror_stdout: boolean;
  autoexecfile: string;

const
{$IFDEF DOOM}
  DEFAUTOEXEC = 'Doom32.con';
{$ENDIF}
{$IFDEF HERETIC}
  DEFAUTOEXEC = 'Heretic32.con';
{$ENDIF}
{$IFDEF HEXEN}
  DEFAUTOEXEC = 'Hexen32.con';
{$ENDIF}
{$IFDEF STRIFE}
  DEFAUTOEXEC = 'Strife32.con';
{$ENDIF}

implementation

uses
  d_delphi,
{$IFDEF HEXEN}
  g_demo,
{$ENDIF}
{$IFDEF STRIFE}
  d_check,
{$ENDIF}
{$IFDEF DOOM}
  doomstat,
{$ENDIF}
  doomdef,
  c_cmds,
  c_utils,
  d_main,
  g_game,
  hu_stuff,
  m_argv,
  m_fixed,
  i_io,
  i_system,
  p_tick,
  r_defs,
  r_main,
  v_data,
  v_video,
  w_utils,
  w_wad,
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
  ConsolePgUpDown: integer = -1; // Page Up / Page Down
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
{$IFDEF STRIFE}
  C_FONTWIDTH = 10;
  C_FONTHEIGHT = 10;
{$ELSE}
  C_FONTWIDTH = 8;
  C_FONTHEIGHT = 8;
{$ENDIF}

//==============================================================================
//
// isDivideLine
//
//==============================================================================
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

//==============================================================================
//
// C_ResetInputBuff
//
//==============================================================================
procedure C_ResetInputBuff;
begin
  ConsoleInputBuff := CONSOLE_PROMPTCHAR;
  con_needsupdate := true;
end;

//==============================================================================
//
// C_IsInputBuffEmpty
//
//==============================================================================
function C_IsInputBuffEmpty: boolean;
begin
  result := ConsoleInputBuff = CONSOLE_PROMPTCHAR;
end;

//==============================================================================
// C_CmdCloseConsole
//
// Commands
//
//==============================================================================
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

//==============================================================================
//
// C_CmdOpenConsole
//
//==============================================================================
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

//==============================================================================
//
// C_CmdConsoleColormap
//
//==============================================================================
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

//==============================================================================
//
// C_CmdCls
//
//==============================================================================
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

//==============================================================================
//
// C_ExecCommandFile
//
//==============================================================================
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

  strupperproc(fname);

  if execs = nil then
    execs := TDStringList.Create
  else if execs.IndexOf(fname) >= 0 then
  begin
    I_Warning(' Recursive calls of con files are not allowed(%s)'#13#10, [filename]);
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
    trimproc(cmd);
    if cmd <> '' then
      if Pos('//', cmd) <> 1 then
        C_ExecuteCmd(cmd);
  end;
  close(t);
  {$I+}
  execs.Delete(execs.IndexOf(fname));
end;

//==============================================================================
//
// C_CmdPrintf
//
//==============================================================================
procedure C_CmdPrintf(const parm1, parm2: string);
begin
  if parm2 = '' then
    printf(parm1 + #13#10)
  else
    printf(parm1 + ' ' + parm2 + #13#10);
end;

var
  console_paused: boolean = false;

//==============================================================================
//
// C_CmdPauseConsole
//
//==============================================================================
procedure C_CmdPauseConsole;
begin
  console_paused := true;
end;

//==============================================================================
//
// Cmd_Use
//
//==============================================================================
procedure Cmd_Use(const parm1, parm2: string);
begin
  if parm1 = '' then
  begin
    C_ExecuteCmd('cmdlist', 'use*');
    exit;
  end;

  if not C_ExecuteCmd('use' + parm1, parm2) then
    printf('%s mnemonic not found!'#13#10);
end;

//==============================================================================
//
// Cmd_Freeze
//
//==============================================================================
procedure Cmd_Freeze(const parm1, parm2: string);
begin
  if demoplayback or demorecording then
  begin
    printf('Can not freeze game while demo playback/recording'#13#10);
    exit;
  end;

  isgamefreezed := not isgamefreezed;

  if isgamefreezed then
    printf('Game is freezed'#13#10)
  else
    printf('Game is not freezed'#13#10);
end;

//
// C_Init
//
var
  pendingcommands: TDStringList;
  consolebuffer: TDStringList;

const
  MAX_CONSOLE_BUFFER = 16384;

//==============================================================================
//
// C_Init
//
//==============================================================================
procedure C_Init;
var
  i: integer;
begin
  pendingcommands := TDStringList.Create;
  consolebuffer := TDStringList.Create;
  ConsoleHead := 0;
  ConsoleState := CST_UP;
  for i := 0 to MAX_CONSOLE_LINES - 1 do
    ConsoleText[i].line := '';
  C_ResetInputBuff;
  C_AdjustScreenSize;
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
  C_AddCmd('start, playgame, engage, visit'{$IFDEF STRIFE} + ', rift'{$ENDIF}, @G_CmdNewGame);
  {$IFDEF DOOM}
  C_AddCmd('testmap,test', @G_CmdTestMap);
  {$ENDIF}
  C_AddCmd('load, loadgame', @G_LoadGame);
  C_AddCmd('save, savegame', @G_CmdSaveGame);
  C_AddCmd('exec, execcommandfile', @C_ExecCommandFile);
  C_AddCmd('printf, write, writeln', @C_CmdPrintf);  // Mostly for autoexec
  C_AddCmd('pause_console, pauseconsole', @C_CmdPauseConsole);
  C_AddCmd('commandlineparams', @M_CmdShowCommandLineParams);
  C_AddCmd('cmdline', @M_CmdShowCmdline);
  C_AddCmd('use', @Cmd_Use);
  C_AddCmd('freeze', @Cmd_Freeze);
  C_RegisterUtilityCommands;
  W_RegisterUtilityCommands;
end;

//==============================================================================
//
// C_ShutDown
//
//==============================================================================
procedure C_ShutDown;
begin
  ConsoleInitialized := False;
  if execs <> nil then
    execs.Free;
  pendingcommands.Free;
  consolebuffer.Free;
end;

//==============================================================================
//
// C_AdjustScreenSize
//
//==============================================================================
procedure C_AdjustScreenSize;
begin
  ConsoleYFrac := V_GetScreenHeight(SCN_CON) div 20;
  MaxConsolePos := ConsoleYFrac * 11;
  ConsoleWidth := V_GetScreenWidth(SCN_CON) div C_FONTWIDTH - 2;
  ConsolePos := 0;
end;

//==============================================================================
//
// C_AddCommand
//
//==============================================================================
procedure C_AddCommand(const cmd: string);
begin
  if not ConsoleInitialized then
    exit;
  pendingcommands.Add(cmd);
end;

var
  commandbuffer: string;

//==============================================================================
//
// C_RestoreFromBuffer
//
//==============================================================================
procedure C_RestoreFromBuffer;
var
  i: integer;
begin
  if not ConsoleInitialized then
    exit;
  for i := consolebuffer.Count - MAX_CONSOLE_LINES to consolebuffer.Count - 1 do
  begin
    ConsoleHead := (ConsoleHead + CONSOLETEXT_MASK) and CONSOLETEXT_MASK;
    if i >= 0 then
      ConsoleText[ConsoleHead].line := consolebuffer.Strings[i]
    else
      ConsoleText[ConsoleHead].line := '';
  end;
  i := (ConsoleHead + CONSOLETEXT_MASK) and CONSOLETEXT_MASK;
  ConsoleText[i].line := '';
end;

//==============================================================================
//
// C_AddLine
//
//==============================================================================
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
    cline := '';
    if len = -1 then
      len := Length(line);
    for i := 1 to len do
    begin
      if line[i] = #8 then
      begin
        if cline <> '' then
          SetLength(cline, Length(cline) - 1);
      end
      else
        cline := cline + line[i];
    end;
  end;
  if console_paused then
  begin
    commandbuffer := commandbuffer + cline + #13#10;
  end
  else
  begin
    if ConsolePgUpDown <> -1 then
    begin
      C_RestoreFromBuffer;
      ConsolePgUpDown := -1;
    end;

    ConsoleHead := (ConsoleHead + CONSOLETEXT_MASK) and CONSOLETEXT_MASK;
    ConsoleText[ConsoleHead].line := cline;
    consolebuffer.Add(cline);
    if consolebuffer.Count > MAX_CONSOLE_BUFFER then
      for j := 0 to MAX_CONSOLE_BUFFER div 100 do
        consolebuffer.Delete(0);
    i := (ConsoleHead + CONSOLETEXT_MASK) and CONSOLETEXT_MASK;
    ConsoleText[i].line := '';
    con_needsupdate := true;
  end;
end;

//==============================================================================
//
// C_DoPageUp
//
//==============================================================================
procedure C_DoPageUp;
var
  bufstart, bufend: integer;
  i: integer;
begin
  if consolebuffer.Count < ConsoleHeight then
    Exit; // Too short output to page up

  if ConsolePgUpDown = 0 then
    Exit; // At the top

  if ConsolePgUpDown = -1 then
    ConsolePgUpDown := consolebuffer.Count - 2 * ConsoleHeight
  else
    ConsolePgUpDown := ConsolePgUpDown - ConsoleHeight;
  if ConsolePgUpDown < 0 then
    ConsolePgUpDown := 0;

  bufstart := ConsolePgUpDown;
  bufend := ConsolePgUpDown + ConsoleHeight - 1;

  for i := bufstart to bufend do
  begin
    ConsoleHead := (ConsoleHead + CONSOLETEXT_MASK) and CONSOLETEXT_MASK;
    if i < consolebuffer.Count then
      ConsoleText[ConsoleHead].line := consolebuffer.Strings[i]
    else
      ConsoleText[ConsoleHead].line := '';
  end;
  i := (ConsoleHead + CONSOLETEXT_MASK) and CONSOLETEXT_MASK;
  ConsoleText[i].line := '';
  con_needsupdate := true;
end;

//==============================================================================
//
// C_DoPageDown
//
//==============================================================================
procedure C_DoPageDown;
var
  bufstart, bufend: integer;
  i: integer;
begin
  if consolebuffer.Count < ConsoleHeight then
    Exit; // Too short output to page up

  if ConsolePgUpDown = -1 then
    Exit; // At the end

  if ConsolePgUpDown >= consolebuffer.Count - ConsoleHeight then
    ConsolePgUpDown := ConsolePgUpDown - 2 * ConsoleHeight
  else
    ConsolePgUpDown := ConsolePgUpDown + ConsoleHeight;

  if ConsolePgUpDown >= consolebuffer.Count - ConsoleHeight then
  begin
    C_RestoreFromBuffer;
    ConsolePgUpDown := -1;
    con_needsupdate := true;
    Exit;
  end;

  bufstart := ConsolePgUpDown;
  bufend := ConsolePgUpDown + ConsoleHeight - 1;

  for i := bufstart to bufend do
  begin
    ConsoleHead := (ConsoleHead + CONSOLETEXT_MASK) and CONSOLETEXT_MASK;
    if i >= 0 then
      ConsoleText[ConsoleHead].line := consolebuffer.Strings[i]
    else
      ConsoleText[ConsoleHead].line := '';
  end;
  i := (ConsoleHead + CONSOLETEXT_MASK) and CONSOLETEXT_MASK;
  ConsoleText[i].line := '';
  con_needsupdate := true;
end;

//==============================================================================
//
// C_AddText
//
//==============================================================================
procedure C_AddText(const txt: string);
var
  i: integer;
  c: char;
begin
  if mirror_stdout then
    fprintf(stdout, txt);

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
end;

const
  AUTOEXECLUMPNAME = 'AUTOEXEC';

//==============================================================================
//
// C_ExecCommands
//
//==============================================================================
procedure C_ExecCommands(const commands: string);
var
  l: TDStringList;
  cmd: string;
  i: integer;
begin
  l := TDStringList.Create;
  l.Text := commands;
  for i := 0 to l.Count - 1 do
  begin
    cmd := strtrim(l.Strings[i]);
    if cmd <> '' then
      if Pos('//', cmd) <> 1 then
        C_ExecuteCmd(cmd);
  end;
  l.Free;
end;

//==============================================================================
//
// C_RunAutoExec
//
//==============================================================================
procedure C_RunAutoExec;
var
  str: TDstringList;
  deffile: string;
  i: integer;
  p: integer;
begin
  printf('C_RunAutoExec()'#13#10);
  if not fexists(DEFAUTOEXEC) then
  begin
    str := TDStringList.Create;
    try
      str.Add('// Add autoexec console commands in this file.');
      str.Add('// These console commands will be executed everytime you start ' + AppTitle);
      deffile := M_SaveFileName(DEFAUTOEXEC);
      str.SaveToFile(deffile);
    finally
      str.Free;
    end;
  end;
  C_ExecCommandFile(autoexecfile);
  for i := 0 to W_NumLumps - 1 do
    if char8tostring(W_GetNameForNum(i)) = AUTOEXECLUMPNAME then
      C_ExecCommands(W_TextLumpNum(i));
  p := M_CheckParm('-con');
  if p > 0 then
    if p < myargc - 1 then
      C_ExecCommandFile(myargv[p + 1]);

end;

var
  shiftdown: boolean = false;
  firsttime: boolean = true;

//==============================================================================
//
// C_Responder
//
//==============================================================================
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

  for c := 0 to pendingcommands.Count - 1 do
    C_ExecuteCmd(pendingcommands.Strings[c]);
  pendingcommands.Clear;

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
            C_AddText(commandbuffer);
            commandbuffer := '';
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
            KEY_PAGEUP:
              C_DoPageUp;
            KEY_PAGEDOWN:
              C_DoPageDown;
          else
            begin
              c := Ord(toupper(Chr(c)));
              if ((c >= Ord(HU_FONTSTART)) and (c <= Ord({$IFDEF DOOM_OR_STRIFE}HU_FONTEND{$ELSE}HU_CFONTEND{$ENDIF}))) or
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

//==============================================================================
//
// C_Ticker
//
//==============================================================================
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

//==============================================================================
//
// C_DrawConsoleBackground
//
//==============================================================================
procedure C_DrawConsoleBackground;
begin
{$IFDEF DOOM}
  V_DrawPatchFullScreenTMP320x200(decide(customgame = cg_bfg2, pg_DMENUPIC, pg_TITLE));
{$ENDIF}
{$IFDEF HERETIC_OR_HEXEN}
  V_CopyRawDataToScreen(SCN_TMP, 'TITLE');
{$ENDIF}
{$IFDEF STRIFE}
  V_DrawPatchFullScreenTMP320x200(char8tostring(W_GetNameForNum(D_Help0Lump)));
{$ENDIF}

// A little bit darker background...
  R_ApplyColormap(0, 320 * 200, SCN_TMP, ConsoleColormap);

  V_CopyRect(0, 0, SCN_TMP, 320, 200, 0, 0, SCN_CON, true);
end;

const
  FIXED_PITCH_CHARS = ['1'..'9', '0', '.', '=', '-'];

//==============================================================================
//
// C_Drawer
//
//==============================================================================
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

  xmax := (ConsoleWidth{$IFDEF DOOM} + 1{$ENDIF}) * C_FONTWIDTH;

  if con_needsupdate then
  begin

    C_DrawConsoleBackground;

    line := ConsoleHead;
    lnum := ConsoleHeight;
    while (ConsoleText[line].line <> '') and (lnum > 0) do
    begin
      x := C_FONTWIDTH;
      len := Length(ConsoleText[line].line);
      if ConsolePgUpDown = -1 then
        lnum := lnum - (len - 1) div ConsoleWidth - 1
      else
        lnum := lnum - 1;
      i := 1;
      if lnum < 0 then
      begin
        i := i - ConsoleWidth * lnum;
        lnum := 0;
      end;
      y := V_GetScreenHeight(SCN_CON) - (ConsoleHeight - lnum + 1) * C_FONTHEIGHT;
      if isDivideLine(ConsoleText[line].line) then
      begin
        patch := W_CacheLumpName({$IFDEF DOOM_OR_STRIFE}'brdr_t'{$ELSE}'BORDB'{$ENDIF}, PU_STATIC);
        if len > ConsoleWidth then
          len := ConsoleWidth;
        for i := 1 to len do
        begin
          V_DrawPatch(x, y + 2{$IFDEF DOOM_OR_STRIFE} - 4{$ENDIF}, SCN_CON, patch, false);
          x := x + 8;
        end;
        Z_ChangeTag(patch, PU_CACHE);
      end
      else
      begin
        while i <= len do
        begin
          c := toupper(ConsoleText[line].line[i]);
          if (c >= HU_FONTSTART) and (c <= {$IFDEF DOOM_OR_STRIFE}HU_FONTEND{$ELSE}HU_CFONTEND{$ENDIF}) then
          begin
            patch := {$IFDEF DOOM_OR_STRIFE}hu_font{$ELSE}hu_font3{$ENDIF}[Ord(c) - Ord(HU_FONTSTART)];
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
            if ConsolePgUpDown = -1 then
            begin
              x := C_FONTWIDTH;
              y := y + C_FONTHEIGHT;
            end
            else
              break;
          end;
          inc(i);
        end;
      end;
      line := (line + 1) and CONSOLETEXT_MASK;
    end;

    x := C_FONTWIDTH;
    y := V_GetScreenHeight(SCN_CON) - C_FONTHEIGHT;
    for i := 1 to Length(ConsoleInputBuff) do
    begin
      c := toupper(ConsoleInputBuff[i]);
      if (c >= HU_FONTSTART) and (c <= {$IFDEF DOOM_OR_STRIFE}HU_FONTEND{$ELSE}HU_CFONTEND{$ENDIF}) and (x < xmax) then
      begin
        patch := {$IFDEF DOOM_OR_STRIFE}hu_font{$ELSE}hu_font3{$ENDIF}[Ord(c) - Ord(HU_FONTSTART)];
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
      patch := {$IFDEF DOOM_OR_STRIFE}hu_font{$ELSE}hu_font3{$ENDIF}[Ord('_') - Ord(HU_FONTSTART)];
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
      patch := {$IFDEF DOOM_OR_STRIFE}hu_font{$ELSE}hu_font3{$ENDIF}[Ord('_') - Ord(HU_FONTSTART)];
      V_DrawPatch(cursor_x, cursor_y, SCN_CON, patch, false);
    end;
    cursor_needs_update := false;
  end;

  V_CopyAddRect(0, V_GetScreenHeight(SCN_CON) - ConsolePos, SCN_CON,
    V_GetScreenWidth(SCN_CON), ConsolePos, 0, 0, SCN_FG,
      false,
        (MaxConsolePos - ConsolePos) * FRACUNIT div MaxConsolePos);
end;

//==============================================================================
//
// C_IsConsoleActive
//
//==============================================================================
function C_IsConsoleActive: boolean;
begin
  result := ConsoleState <> CST_UP;
end;

//==============================================================================
//
// C_ConsoleHeight
//
//==============================================================================
function C_ConsoleHeight: integer;
begin
  result := ConsoleHeight;
end;

//==============================================================================
//
// C_ConsolePos
//
//==============================================================================
function C_ConsolePos: integer;
begin
  result := ConsolePos;
end;

//==============================================================================
//
// C_AddDivideLine
//
//==============================================================================
procedure C_AddDivideLine;
begin
  C_AddText(divideline);
end;

end.

