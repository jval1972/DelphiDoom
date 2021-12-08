//------------------------------------------------------------------------------
//
//  DelphiStrife: A modified and improved Strife source port for Windows.
//
//  Based on:
//    - Linux Doom by "id Software"
//    - Chocolate Strife by "Simon Howard"
//    - DelphiDoom by "Jim Valavanis"
//
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2005 Simon Howard
//  Copyright (C) 2010 James Haley, Samuel Villarreal
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
//    Head up display
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit hu_stuff;

interface

uses
  doomdef,
  d_event,
  r_defs;

const
//
// Globally visible constants.
//
  HU_FONTSTART = '!'; // the first font characters
  HU_FONTEND = '_'; // the last font characters

// Calculate # of glyphs in font.
  HU_FONTSIZE = (Ord(HU_FONTEND) - Ord(HU_FONTSTART)) + 1;
  HU_BROADCAST = 9;   // haleyjd [STRIFE] Changed 5 -> 9
  HU_CHANGENAME = 10; // haleyjd [STRIFE] Special command

  HU_MSGREFRESH = Ord('/');
  HU_MSGX = 0;
  HU_MSGY = 0;
  HU_MSGHEIGHT = 2; // in lines

  HU_MSGTIMEOUT = 8 * TICRATE;  // haleyjd [STRIFE] Doubled message timeout

//
// HEADS UP TEXT
//
procedure HU_Init;

procedure HU_Start;

function HU_Responder(ev: Pevent_t): boolean;

procedure HU_Ticker;

procedure HU_Drawer;

{$IFDEF OPENGL}
function HU_Height: integer;
{$ENDIF}

function HU_dequeueChatChar: char;

procedure HU_Erase;

var
  hu_font: array[0..HU_FONTSIZE - 1] of Ppatch_t;
  yfont: array[0..HU_FONTSIZE - 1] of Ppatch_t;  // haleyjd 09/18/10: [STRIFE]

var
  m_font3: array['A'..Chr(Ord('Z') + 1)] of Ppatch_t;


  chat_on: boolean;

  message_on: boolean;
  message_dontfuckwithme: boolean;
  message_nottobefuckedwith: boolean;

var
// STRIFE MAP NAMES
  mapnames: array[0..33] of string;

  player_names: array[0..7] of string;

var
  chat_macros: array[0..9] of string;

var
  key_multi_msgplayer: array[0..MAXPLAYERS - 1] of string;

var
  shiftxform: array[0..127] of char;

procedure HU_DoFPSStuff;

function HU_FPS: integer;

var
  drawfps: boolean;

implementation

uses
  d_delphi,
  c_cmds,
  z_zone,
  w_wad,
  i_system,
  doomstat,
  am_map,
  dstrings,
  d_englsh,
  d_player,
  d_main,
  g_game,
  hu_lib,
  m_menu,
  m_fixed,
  p_tick,
  r_draw,
  s_sound,
  sounds,
  v_data,
  v_video;

// FPS Stuff

const
  FPSSIZE = 128;
  FPSSIZE2 = 512;

var
  FPSHISTORY: array[0..FPSSIZE - 1] of integer;
  FPSHISTORY2: array[0..FPSSIZE2 - 1] of integer;
  fpshead: integer = -1;
  fpshead2: integer = -1;

procedure HU_DoFPSStuff;
var
  ftime: integer;
begin
  ftime := I_GetFracTime;
  fpshead := (fpshead + 1) mod FPSSIZE;
  FPSHISTORY[fpshead] := ftime;
  fpshead2 := (fpshead2 + 1) mod FPSSIZE2;
  FPSHISTORY2[fpshead2] := ftime;
end;

function HU_FPS: integer;
var
  fpsdiff: integer;
begin
  fpsdiff := FPSHISTORY[fpshead] - FPSHISTORY[(fpshead + 1) mod FPSSIZE] + 1;
  if fpsdiff > 0 then
  begin
    result :=  TICRATE * FPSSIZE * FRACUNIT div fpsdiff;
    if result > FPSSIZE then
    begin
      fpsdiff := FPSHISTORY2[fpshead2] - FPSHISTORY2[(fpshead2 + 1) mod FPSSIZE2] + 1;
      if fpsdiff > 0 then
        result :=  TICRATE * FPSSIZE2 * FRACUNIT div fpsdiff;
    end;
  end
  else
    result := TICRATE;
end;

procedure HU_CmdFPS;
begin
  printf('%d fps'#13#10, [HU_FPS]);
end;

procedure HU_CmdPlayerMessage(const parm1, parm2: string);
begin
  players[consoleplayer]._message := parm1 + ' ' + parm2;
end;

function HU_TITLE: string;
begin
  result := mapnames[gamemap - 1];
end;

var
  plr: Pplayer_t;
  w_title: hu_textline_t;
  w_leveltime: hu_textline_t;
  w_chat: hu_itext_t;
  always_off: boolean = false;
  chat_dest: array[0..MAXPLAYERS - 1] of char;
  w_inputbuffer: array[0..MAXPLAYERS - 1] of hu_itext_t;

  w_message: hu_stext_t;
  message_counter: integer;

  headsupactive: boolean = false;

  hu_setting_name: boolean = false;

const
  HU_TITLEHEIGHT = 1;
  HU_TITLEX = 0;
  HU_LEVELTIMEX = 0;

// haleyjd 09/01/10: [STRIFE] 167 -> 160 to move up level name
function HU_TITLEY: integer;
begin
  result := {$IFDEF OPENGL}V_GetScreenHeight(SCN_FG) * 160 div 200{$ELSE}160{$ENDIF} - hu_font[0].height;
end;

function HU_LEVELTIMEY: integer;
begin
  result := {$IFDEF OPENGL}V_GetScreenHeight(SCN_FG) * 160 div 200{$ELSE}160{$ENDIF} - 2 * hu_font[0].height;
end;

const
  HU_INPUTTOGGLE: char = 't';

function HU_INPUTX: integer;
begin
  result := HU_MSGX;
end;

function HU_INPUTY: integer;
begin
  result := HU_MSGY + HU_MSGHEIGHT * (hu_font[0].height + 1)
end;

const
  HU_INPUTWIDTH = 64;
  HU_INPUTHEIGHT = 1;

const
  french_shiftxform: array[0..127] of char = (
    #0,
    #1, #2, #3, #4, #5, #6, #7, #8, #9, #10,
    #11, #12, #13, #14, #15, #16, #17, #18, #19, #20,
    #21, #22, #23, #24, #25, #26, #27, #28, #29, #30,
    #31,
    ' ', '!', '"', '#', '$', '%', '&',
    '"', // shift-'
    '(', ')', '*', '+',
    '?', // shift-,
    '_', // shift--
    '>', // shift-.
    '?', // shift-/
    '0', // shift-0
    '1', // shift-1
    '2', // shift-2
    '3', // shift-3
    '4', // shift-4
    '5', // shift-5
    '6', // shift-6
    '7', // shift-7
    '8', // shift-8
    '9', // shift-9
    '/',
    '.', // shift-;
    '<',
    '+', // shift-=
    '>', '?', '@',
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
    'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    '[', // shift-[
    '!', // shift-backslash - OH MY GOD DOES WATCOM SUCK
    ']', // shift-]
    '"', '_',
    '''', // shift-`
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
    'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    '{', '|', '}', '~', #127
  );

const
  english_shiftxform: array[0..127] of char = (
    #0,
    #1, #2, #3, #4, #5, #6, #7, #8, #9, #10,
    #11, #12, #13, #14, #15, #16, #17, #18, #19, #20,
    #21, #22, #23, #24, #25, #26, #27, #28, #29, #30,
    #31,
    ' ', '!', '"', '#', '$', '%', '&',
    '"', // shift-'
    '(', ')', '*', '+',
    '<', // shift-,
    '_', // shift--
    '>', // shift-.
    '?', // shift-/
    ')', // shift-0
    '!', // shift-1
    '@', // shift-2
    '#', // shift-3
    '$', // shift-4
    '%', // shift-5
    '^', // shift-6
    '&', // shift-7
    '*', // shift-8
    '(', // shift-9
    ':',
    ':', // shift-;
    '<',
    '+', // shift-=
    '>', '?', '@',
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
    'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    '[', // shift-[
    '!', // shift-backslash - OH MY GOD DOES WATCOM SUCK
    ']', // shift-]
    '"', '_',
    '''', // shift-`
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
    'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    '{', '|', '}', '~', #127
  );

const
  frenchKeyMap: array[0..127] of char = (
    #0,
    #1,#2,#3,#4,#5,#6,#7,#8,#9,#10,
    #11,#12,#13,#14,#15,#16,#17,#18,#19,#20,
    #21,#22,#23,#24,#25,#26,#27,#28,#29,#30,
    #31,
    ' ','!','"','#','$','%','&','%','(',')','*','+',';','-',':','!',
    '0','1','2','3','4','5','6','7','8','9',':','M','<','=','>','?',
    '@','Q','B','C','D','E','F','G','H','I','J','K','L',',','N','O',
    'P','A','R','S','T','U','V','Z','X','Y','W','^','\','$','^','_',
    '@','Q','B','C','D','E','F','G','H','I','J','K','L',',','N','O',
    'P','A','R','S','T','U','V','Z','X','Y','W','^','\','$','^',#127
  );

function ForeignTranslation(ch: char): char;
begin
  if ch < #128 then
    result := frenchKeyMap[Ord(ch)]
  else
    result := ch;
end;

procedure HU_Init;
var
  i: integer;
  j: integer;
  buffer: string;
  c: char;
begin
  if language = french then
  begin
    for i := 0 to 127 do
      shiftxform[i] := french_shiftxform[i];
  end
  else
  begin
    for i := 0 to 127 do
      shiftxform[i] := english_shiftxform[i];
  end;

  // load the heads-up font
  j := Ord(HU_FONTSTART);
  for i := 0 to HU_FONTSIZE - 1 do
  begin
    buffer := 'STCFN' + IntToStrZfill(3, j);
    inc(j);
    hu_font[i] := W_CacheLumpName(buffer, PU_STATIC);
    // haleyjd 09/18/10: load yfont as well; and yes, this is exactly
    // how Rogue did it :P
    buffer[3] := 'B';
    yfont[i] := W_CacheLumpName(buffer, PU_STATIC);
  end;

  for c := 'A' to 'Z' do
    m_font3[c] := W_CacheLumpName('FONT3_' + c, PU_STATIC);
  m_font3[Chr(Ord('Z') + 1)] := W_CacheLumpName('FONT3_!', PU_STATIC);

  for i := 0 to FPSSIZE - 1 do
    FPSHISTORY[i] := 0;
  for i := 0 to FPSSIZE2 - 1 do
    FPSHISTORY2[i] := 0;

  C_AddCmd('fps', @HU_CmdFPS);
  C_AddCmd('playermessage', @HU_CmdPlayerMessage);
end;

procedure HU_Stop;
begin
  headsupactive := false;
end;

procedure HU_Start;
var
  i: integer;
  s: string;
begin
  if headsupactive then
    HU_Stop;

  plr := @players[consoleplayer];
  message_on := false;
  message_dontfuckwithme := false;
  message_nottobefuckedwith := false;
  chat_on := false;

  // create the message widget
  HUlib_initSText(@w_message,
    HU_MSGX, HU_MSGY, HU_MSGHEIGHT,
    @hu_font,
    Ord(HU_FONTSTART), @message_on);

  // create the map title widget
  HUlib_initTextLine(@w_title,
    HU_TITLEX, HU_TITLEY,
    @hu_font,
    Ord(HU_FONTSTART));

  HUlib_initTextLine(@w_leveltime,
    HU_LEVELTIMEX, HU_LEVELTIMEY,
    @hu_font,
    Ord(HU_FONTSTART));

  s := HU_TITLE;

  for i := 1 to Length(s) do
    HUlib_addCharToTextLine(@w_title, s[i]);

  // create the chat widget
  HUlib_initIText(@w_chat,
    HU_INPUTX, HU_INPUTY,
    @hu_font,
    Ord(HU_FONTSTART), @chat_on);

  // create the inputbuffer widgets
  for i := 0 to MAXPLAYERS - 1 do
    HUlib_initIText(@w_inputbuffer[i], 0, 0, nil, 0, @always_off);

  // haleyjd 09/18/10: [STRIFE] nickname weirdness.
  if nickname <> player_names[consoleplayer] then
    if nickname <> '' then
    begin
      printf('have one'#13#10);
      nickname := player_names[consoleplayer];
    end;

  headsupactive := true;
end;

{$IFDEF OPENGL}
var
  hu_h: integer = 0;

function HU_Height: integer;
begin
  result := hu_h;
end;
{$ENDIF}

var
  m_fps: string = '';
  fps_ticker: integer = 0;

procedure HU_DrawFPS;
var
  i: integer;
  x, y: integer;
  c: integer;
begin
{$IFDEF OPENGL}
  x := V_GetScreenWidth(SCN_FG) - 9;
  y := 1;
  for i := length(m_fps) downto 1 do
  begin
    if m_fps[i] <> ' ' then
    begin
      c := Ord(toupper(m_fps[i])) - Ord(HU_FONTSTART);
      V_DrawPatch(x, y, SCN_FG, hu_font[c], false);
      x := x - 8;
    end
    else
      x := x - 4;
  end;
  hu_h := hu_h + 9;
{$ELSE}
  if amstate = am_only then
  begin
    x := 311;
    y := 1;
  end
  else
  begin
    x := (viewwindowx + viewwidth) * 320 div SCREENWIDTH - 9;
    y := viewwindowy * 200 div SCREENHEIGHT + 1;
  end;
  for i := length(m_fps) downto 1 do
  begin
    if m_fps[i] <> ' ' then
    begin
      c := Ord(toupper(m_fps[i])) - Ord(HU_FONTSTART);
      V_DrawPatch(x, y, SCN_FG, hu_font[c], true);
      x := x - 8;
    end
    else
      x := x - 4;
  end;
{$ENDIF}
end;

// 19/9/2009 JVAL: For drawing demo progress
procedure HU_DrawDemoProgress;
var
  dp: Ppatch_t;
  i: integer;
  x, y: integer;
begin
  dp := W_CacheLumpName('DEMOTIME', PU_STATIC);
  x := viewwindowx{$IFDEF OPENGL} * 320 div SCREENWIDTH{$ENDIF};
  y := (viewwindowy + viewheight){$IFDEF OPENGL} * 200 div SCREENHEIGHT{$ENDIF};
{$IFDEF OPENGL}
  i := round(G_DemoProgress * viewwidth / SCREENWIDTH * 320 / FRACUNIT);
{$ELSE}
  i := G_DemoProgress * viewwidth div FRACUNIT;
{$ENDIF}
  while i > 0 do
  begin
    V_DrawPatchTransparent(x, y, SCN_FG, dp, {$IFDEF OPENGL}true{$ELSE}false{$ENDIF});
    i := i - dp.width;
    x := x + dp.width;
  end;
  Z_ChangeTag(dp, PU_CACHE);
end;

procedure HU_Drawer;
var
  i, t: integer;
{$IFDEF OPENGL}
  i2, idx, lines: integer;
{$ENDIF}
  lt: string;
begin
{$IFDEF OPENGL}
  hu_h := 0;
{$ENDIF}
  if drawfps then
    HU_DrawFPS;
  if demoplayback and showdemoplaybackprogress then
    HU_DrawDemoProgress;

  HUlib_drawSText(@w_message);
  {$IFDEF OPENGL}
  if w_message._on^ then
    for i := 0 to w_message.height - 1 do
    begin
      idx := w_message.curline - i;
      if idx < 0 then
        idx := idx + w_message.height; // handle queue of lines
      lines := 0;
      for i2 := 0 to w_message.lines[idx].len - 1 do
        if w_message.lines[idx].line[i2] = #10 then
          inc(lines);
      if lines * 10 > hu_h then
        hu_h := lines * 10;
    end;
  {$ENDIF}
  HUlib_drawIText(@w_chat);
  if amstate = am_only then
  begin
    repeat
    until not HUlib_delCharFromTextLine(@w_leveltime);
    t := leveltime div TICRATE;
    lt := IntToStrZFill(2, t mod 60);
    t := t div 60;
    lt := IntToStrZFill(2, t mod 60) + ':' + lt;
    t := t div 60;
    lt := 'Time: ' + IntToStrZFill(2, t) + ':' + lt;

    for i := 1 to Length(lt) do
      HUlib_addCharToTextLine(@w_leveltime, lt[i]);
    HUlib_drawTextLine(@w_leveltime, false);
    HUlib_drawTextLine(@w_title, false);
  end;
end;

procedure HU_Erase;
begin
  HUlib_eraseSText(@w_message);
  HUlib_eraseIText(@w_chat);
  HUlib_eraseTextLine(@w_title);
end;

procedure HU_Ticker;
var
  i: integer;
  rc: boolean;
  c: char;
begin
  dec(fps_ticker);
  if fps_ticker <= 0 then
  begin
    m_fps := itoa(HU_FPS) + ' fps';
    fps_ticker := TICRATE div 2;
  end;

  // tick down message counter if message is up
  if message_counter <> 0 then
  begin
    dec(message_counter);
    if message_counter = 0 then
    begin
      HUlib_removeLineFromSText(@w_message);
      message_on := w_message.lines[w_message.curline].len > 0;
      if message_on then
        message_counter := HU_MSGTIMEOUT;
      message_nottobefuckedwith := false;
    end;
  end;

  if (showMessages <> 0) or message_dontfuckwithme then
  begin
    // display message if necessary
    if ((plr._message <> '') and (not message_nottobefuckedwith)) or
       ((plr._message <> '') and message_dontfuckwithme) then
    begin
      HUlib_addMessageToSText2(@w_message, '', plr._message);
      plr._message := '';
      message_on := true;
      message_counter := HU_MSGTIMEOUT;
      message_nottobefuckedwith := message_dontfuckwithme;
      message_dontfuckwithme := false;
    end;
  end; // else message_on = false;

  // check for incoming chat characters
  if netgame then
  begin
    for i := 0 to MAXPLAYERS - 1 do
    begin
      if not playeringame[i] then
        continue;
      c := Chr(players[i].cmd.chatchar);
      if (i <> consoleplayer) and (c <> #0) then
      begin
        if c <= Chr(HU_CHANGENAME) then // [STRIFE]: allow HU_CHANGENAME here
          chat_dest[i] := c
        else
        begin
          if (c >= 'a') and (c <= 'z') then
            c := shiftxform[Ord(c)];
          rc := HUlib_keyInIText(@w_inputbuffer[i], Ord(c));
          if rc and (Ord(c) = KEY_ENTER) then
          begin
            if (w_inputbuffer[i].line.len <> 0) and
               ((Ord(chat_dest[i]) = consoleplayer + 1) or (Ord(chat_dest[i]) = HU_BROADCAST)) then
            begin
              HUlib_addMessageToSText(@w_message,
                player_names[i],
                w_inputbuffer[i].line.line);

              message_nottobefuckedwith := true;
              message_on := true;
              message_counter := HU_MSGTIMEOUT;
              S_StartSound(nil, Ord(sfx_radio))
            end;
            HUlib_resetIText(@w_inputbuffer[i]);
          end;
        end;
        players[i].cmd.chatchar := 0;
      end;
    end;
  end;
end;

const
  QUEUESIZE = 128;

var
  chatchars: array[0..QUEUESIZE - 1] of char;
  head: integer = 0;
  tail: integer = 0;

procedure HU_queueChatChar(c: char);
begin
  if ((head + 1) and (QUEUESIZE - 1)) = tail then
    plr._message := HUSTR_MSGU
  else
  begin
    chatchars[head] := c;
    head := (head + 1) and (QUEUESIZE - 1);
  end;
end;

function HU_dequeueChatChar: char;
begin
  if head <> tail then
  begin
    result := chatchars[tail];
    tail := (tail + 1) and (QUEUESIZE - 1);
  end
  else
    result := #0;
end;

var
  lastmessage: string;
  shiftdown: boolean = false;
  altdown: boolean = false;
  num_nobrainers: integer = 0;

function HU_Responder(ev: Pevent_t): boolean;
var
  macromessage: string;
  c: char;
  i: integer;
  numplayers: integer;
begin
  result := false;

  if ev.data1 = KEY_RSHIFT then
  begin
    shiftdown := ev._type = ev_keydown;
    exit;
  end
  else if (ev.data1 = KEY_RALT) or (ev.data1 = KEY_LALT) then
  begin
    altdown := ev._type = ev_keydown;
    exit;
  end;

  if ev._type <> ev_keydown then
    exit;

  numplayers := 0;
  for i := 0 to MAXPLAYERS - 1 do
    if playeringame[i] then
      inc(numplayers);

  if not chat_on then
  begin
    if ev.data1 = HU_MSGREFRESH then
    begin
      message_on := true;
      message_counter := HU_MSGTIMEOUT;
      result := true;
    end
    else if netgame and (ev.data1 = Ord(HU_INPUTTOGGLE)) then
    begin
      result := true;
      chat_on := true;
      HUlib_resetIText(@w_chat);
      HU_queueChatChar(Chr(HU_BROADCAST));
    end
    else if netgame and (numplayers > 2) then
    begin
      for i := 0 to MAXPLAYERS - 1 do
      begin
        if key_multi_msgplayer[i] <> '' then
        begin
          if ev.data1 = Ord(key_multi_msgplayer[i][1]) then
          begin
            if playeringame[i] and (i <> consoleplayer) then
            begin
              result := true;
              chat_on := true;
              HUlib_resetIText(@w_chat);
              HU_queueChatChar(Chr(i + 1));
              break;
            end
            else if i = consoleplayer then
            begin
              inc(num_nobrainers);
              if num_nobrainers < 3 then
                plr._message := HUSTR_TALKTOSELF1
              else if num_nobrainers < 6 then
                plr._message := HUSTR_TALKTOSELF2
              else if num_nobrainers < 9 then
                plr._message := HUSTR_TALKTOSELF3
              else if num_nobrainers < 32 then
                plr._message := HUSTR_TALKTOSELF4
              else
                plr._message := HUSTR_TALKTOSELF5;
            end
          end;
        end;
      end;
    end;
  end
  else
  begin
    c := Chr(ev.data1);
    // send a macro
    if altdown then
    begin
      c := Chr(Ord(c) - Ord('0'));
      if c > Chr(9) then
        exit;
      macromessage := chat_macros[Ord(c)];

      // kill last message with a '\n'
      HU_queueChatChar(Chr(KEY_ENTER)); // DEBUG!!!

      // send the macro message
      for i := 1 to Length(macromessage) do
        HU_queueChatChar(macromessage[i]);
      HU_queueChatChar(Chr(KEY_ENTER));

      // leave chat mode and notify that it was sent
      chat_on := false;
      lastmessage := chat_macros[Ord(c)];
      plr._message := lastmessage;
      result := true;
    end
    else
    begin
      if language = french then
        c := ForeignTranslation(c);
      if shiftdown or ((c >= 'a') and (c <= 'z')) then
        c := shiftxform[Ord(c)];
      result := HUlib_keyInIText(@w_chat, Ord(c));
      if result then
        HU_queueChatChar(c);
      if Ord(c) = KEY_ENTER then
      begin
        chat_on := false;
        if w_chat.line.len <> 0 then
        begin
          lastmessage := w_chat.line.line;
          plr._message := lastmessage;
        end
      end
      else if Ord(c) = KEY_ESCAPE then
        chat_on := false;
    end;
  end;
end;

initialization
  chat_macros[0] := HUSTR_CHATMACRO0;
  chat_macros[1] := HUSTR_CHATMACRO1;
  chat_macros[2] := HUSTR_CHATMACRO2;
  chat_macros[3] := HUSTR_CHATMACRO3;
  chat_macros[4] := HUSTR_CHATMACRO4;
  chat_macros[5] := HUSTR_CHATMACRO5;
  chat_macros[6] := HUSTR_CHATMACRO6;
  chat_macros[7] := HUSTR_CHATMACRO7;
  chat_macros[8] := HUSTR_CHATMACRO8;
  chat_macros[9] := HUSTR_CHATMACRO9;

  player_names[0] := HUSTR_PLAYER1;
  player_names[1] := HUSTR_PLAYER2;
  player_names[2] := HUSTR_PLAYER3;
  player_names[3] := HUSTR_PLAYER4;
  player_names[4] := HUSTR_PLAYER5;
  player_names[5] := HUSTR_PLAYER6;
  player_names[6] := HUSTR_PLAYER7;
  player_names[7] := HUSTR_PLAYER8;

////////////////////////////////////////////////////////////////////////////////
//
// Builtin map names.
// The actual names can be found in DStrings.h.
//
////////////////////////////////////////////////////////////////////////////////

// STRIFE map names.

  mapnames[0] := HUSTR_1;
  mapnames[1] := HUSTR_2;
  mapnames[2] := HUSTR_3;
  mapnames[3] := HUSTR_4;
  mapnames[4] := HUSTR_5;
  mapnames[5] := HUSTR_6;
  mapnames[6] := HUSTR_7;
  mapnames[7] := HUSTR_8;
  mapnames[8] := HUSTR_9;
  mapnames[9] := HUSTR_10;
  mapnames[10] := HUSTR_11;

  mapnames[11] := HUSTR_12;
  mapnames[12] := HUSTR_13;
  mapnames[13] := HUSTR_14;
  mapnames[14] := HUSTR_15;
  mapnames[15] := HUSTR_16;
  mapnames[16] := HUSTR_17;
  mapnames[17] := HUSTR_18;
  mapnames[18] := HUSTR_19;
  mapnames[19] := HUSTR_20;

  mapnames[20] := HUSTR_21;
  mapnames[21] := HUSTR_22;
  mapnames[22] := HUSTR_23;
  mapnames[23] := HUSTR_24;
  mapnames[24] := HUSTR_25;
  mapnames[25] := HUSTR_26;
  mapnames[26] := HUSTR_27;
  mapnames[27] := HUSTR_28;
  mapnames[28] := HUSTR_29;
  mapnames[29] := HUSTR_30;
  mapnames[30] := HUSTR_31;
  mapnames[31] := HUSTR_32;
  mapnames[32] := HUSTR_33;
  mapnames[33] := HUSTR_34;

////////////////////////////////////////////////////////////////////////////////

  key_multi_msgplayer[0] := '1';
  key_multi_msgplayer[1] := '2';
  key_multi_msgplayer[2] := '3';
  key_multi_msgplayer[3] := '4';
  key_multi_msgplayer[4] := '5';
  key_multi_msgplayer[5] := '6';
  key_multi_msgplayer[6] := '7';
  key_multi_msgplayer[7] := '8';

end.

