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

unit hu_lib;

interface

uses
  d_delphi,
  doomdef,
  r_defs;

const
// font stuff
  HU_CHARERASE = KEY_BACKSPACE;

  HU_MAXLINES = 4;
  HU_MAXLINELINESS = 4;
  HU_MAXLINELENGTH = HU_MAXLINELINESS * 80;

//
// Typedefs of widgets
//

// Text Line widget
//  (parent of Scrolling Text and Input Text widgets)
type
  hu_textline_t = record
    // left-justified position of scrolling text window
    x: integer;
    y: integer;

    font: Ppatch_tPArray; // font
    sc: integer; // start character
    line: array[0..(HU_MAXLINELENGTH + 1) - 1] of char; // line of text
    len: integer; // current line length

    // whether this line needs to be udpated
    needsupdate: integer;
  end;
  Phu_textline_t = ^hu_textline_t;

// Scrolling Text window widget
//  (child of Text Line widget)

  hu_stext_t = record
    lines: array[0..HU_MAXLINES - 1] of hu_textline_t; // text lines to draw
    height: integer;      // height in lines
    curline: integer;     // current line number
    _on: Pboolean;        // pointer to boolean stating whether to update window
    laston: boolean;      // last value of *._on.
  end;
  Phu_stext_t = ^hu_stext_t;

// Input Text Line widget
//  (child of Text Line widget)
  hu_itext_t = record
    line: hu_textline_t;  // text line to input on
    lm: integer;          // left margin past which I am not to delete characters
    _on: Pboolean;        // pointer to boolean stating whether to update window
    laston: boolean;      // last value of *->on;
  end;
  Phu_itext_t = ^hu_itext_t;

//==============================================================================
// HUlib_init
//
// Widget creation, access, and update routines
//
// initializes heads-up widget library
//
//==============================================================================
procedure HUlib_init;

//==============================================================================
// HUlib_clearTextLine
//
// textline code
//
// clear a line of text
//
//==============================================================================
procedure HUlib_clearTextLine(t: Phu_textline_t);

//==============================================================================
//
// HUlib_initTextLine
//
//==============================================================================
procedure HUlib_initTextLine(t: Phu_textline_t; x: integer; y: integer; f: Ppatch_tPArray; sc: integer);

//==============================================================================
// HUlib_addCharToTextLine
//
// returns success
//
//==============================================================================
function HUlib_addCharToTextLine(t: Phu_textline_t; ch: char): boolean;

//==============================================================================
// HUlib_delCharFromTextLine
//
// returns success
//
//==============================================================================
function HUlib_delCharFromTextLine(t: Phu_textline_t): boolean;

//==============================================================================
// HUlib_drawTextLine
//
// draws tline
//
//==============================================================================
procedure HUlib_drawTextLine(l: Phu_textline_t; drawcursor: boolean);

{$IFDEF STRIFE}

//==============================================================================
//
// HUlib_drawYellowText
//
// haleyjd 20100918: [STRIFE] New function.
//
//==============================================================================
procedure HUlib_drawYellowText(x, y: integer; txt: string; const scn: integer);
{$ENDIF}

//==============================================================================
// HUlib_eraseTextLine
//
// erases text line
//
//==============================================================================
procedure HUlib_eraseTextLine(l: Phu_textline_t);

//==============================================================================
// HUlib_initSText
//
// Scrolling Text window widget routines
//
// ?
//
//==============================================================================
procedure HUlib_initSText(s: Phu_stext_t; x: integer; y: integer; h: integer;
  font: Ppatch_tPArray; startchar: integer; _on: Pboolean);

//==============================================================================
// HUlib_addLineToSText
//
// add a new line
//
//==============================================================================
procedure HUlib_addLineToSText(s: Phu_stext_t);

//==============================================================================
//
// HUlib_removeLineFromSText
//
//==============================================================================
procedure HUlib_removeLineFromSText(s: Phu_stext_t);

//==============================================================================
// HUlib_addMessageToSText
//
// ?
//
//==============================================================================
procedure HUlib_addMessageToSText(s: Phu_stext_t; prefix: string; msg: string);

//==============================================================================
//
// HUlib_addMessageToSText2
//
//==============================================================================
procedure HUlib_addMessageToSText2(s: Phu_stext_t; prefix: string; msg: string);

//==============================================================================
// HUlib_drawSText
//
// draws stext
//
//==============================================================================
procedure HUlib_drawSText(s: Phu_stext_t);

//==============================================================================
// HUlib_eraseSText
//
// erases all stext lines
//
//==============================================================================
procedure HUlib_eraseSText(s: Phu_stext_t);

//==============================================================================
// HUlib_initIText
//
// Input Text Line widget routines
//
//==============================================================================
procedure HUlib_initIText(it: Phu_itext_t; x: integer; y: integer; font: Ppatch_tPArray;
  startchar: integer; _on: Pboolean);

//==============================================================================
// HUlib_delCharFromIText
//
// enforces left margin
//
//==============================================================================
procedure HUlib_delCharFromIText(it: Phu_itext_t);

//==============================================================================
// HUlib_eraseLineFromIText
//
// enforces left margin
//
//==============================================================================
procedure HUlib_eraseLineFromIText(it: Phu_itext_t);

//==============================================================================
// HUlib_resetIText
//
// resets line and left margin
//
//==============================================================================
procedure HUlib_resetIText(it: Phu_itext_t);

//==============================================================================
// HUlib_addPrefixToIText
//
// left of left-margin
//
//==============================================================================
procedure HUlib_addPrefixToIText(it: Phu_itext_t; str: string);

//==============================================================================
// HUlib_keyInIText
//
// whether eaten
//
//==============================================================================
function HUlib_keyInIText(it: Phu_itext_t; ch: byte): boolean;

//==============================================================================
//
// HUlib_drawIText
//
//==============================================================================
procedure HUlib_drawIText(it: Phu_itext_t);

//==============================================================================
// HUlib_eraseIText
//
// erases all itext lines
//
//==============================================================================
procedure HUlib_eraseIText(it: Phu_itext_t);

implementation

uses
  am_map,
  {$IFDEF STRIFE}
  hu_stuff,
  {$ENDIF}
  r_draw,
  v_data,
  v_video;

//==============================================================================
//
// HUlib_init
//
//==============================================================================
procedure HUlib_init;
begin
end;

//==============================================================================
//
// HUlib_clearTextLine
//
//==============================================================================
procedure HUlib_clearTextLine(t: Phu_textline_t);
begin
  t.len := 0;
  t.line[0] := Chr(0);
  t.needsupdate := 1; //true;
end;

//==============================================================================
//
// HUlib_initTextLine
//
//==============================================================================
procedure HUlib_initTextLine(t: Phu_textline_t; x: integer; y: integer; f: Ppatch_tPArray; sc: integer);
begin
  t.x := x;
  t.y := y;
  t.font := f;
  t.sc := sc;
  HUlib_clearTextLine(t);
end;

//==============================================================================
//
// HUlib_addCharToTextLine
//
//==============================================================================
function HUlib_addCharToTextLine(t: Phu_textline_t; ch: char): boolean;
begin
  if t.len = HU_MAXLINELENGTH then
    result := false
  else
  begin
    t.line[t.len] := ch;
    inc(t.len);
    t.line[t.len] := Chr(0);
    t.needsupdate := 4;
    result := true;
  end;
end;

//==============================================================================
//
// HUlib_delCharFromTextLine
//
//==============================================================================
function HUlib_delCharFromTextLine(t: Phu_textline_t): boolean;
begin
  if t.len = 0 then
    result := false
  else
  begin
    dec(t.len);
    t.line[t.len] := Chr(0);
    t.needsupdate := 4;
    result := true;
  end;
end;

//==============================================================================
//
// HUlib_drawTextLine
//
//==============================================================================
procedure HUlib_drawTextLine(l: Phu_textline_t; drawcursor: boolean);
var
  i: integer;
  w: integer;
  x: integer;
  y: integer;
  c: char;
begin
  // draw the new stuff
  x := l.x;
  y := l.y;
  for i := 0 to l.len - 1 do
  begin
    c := toupper(l.line[i]);
    if c = #10 then
      x := l.x
    else if c = #13 then
      y := y + 8
    else if c = ' ' then
    begin
      x := x + 4;
      if x >= {$IFDEF OPENGL}V_GetScreenWidth(SCN_FG){$ELSE}320{$ENDIF} then
        break;
    end
    else if (Ord(c) >= l.sc) and (c {$IFDEF STRIFE}<{$ELSE}<={$ENDIF} '_') then
    begin
      w := l.font[Ord(c) - l.sc].width;
      if x + w > {$IFDEF OPENGL}V_GetScreenWidth(SCN_FG){$ELSE}320{$ENDIF} then
        break;
      V_DrawPatch(x, y, SCN_FG, l.font[Ord(c) - l.sc], {$IFDEF OPENGL}false{$ELSE}true{$ENDIF});
      x := x + w;
    end;
  end;

  // draw the cursor if requested
  if drawcursor and (x + l.font[Ord('_') - l.sc].width <= {$IFDEF OPENGL}V_GetScreenWidth(SCN_FG){$ELSE}320{$ENDIF}) then
    V_DrawPatch(x, l.y, SCN_FG, l.font[Ord('_') - l.sc], true);
end;

{$IFDEF STRIFE}

//==============================================================================
//
// HUlib_drawYellowText
//
// haleyjd 20100918: [STRIFE] New function.
//
//==============================================================================
procedure HUlib_drawYellowText(x, y: integer; txt: string; const scn: integer);
var
  i: integer;
  w: integer;
  c: char;
  len: integer;
  start_x: integer;
  patch: Ppatch_t;
  id: integer;
begin
  len := length(txt);
  start_x := x;
  for i := 1 to len do
  begin
    c := toupper(txt[i]);
    if c = #10 then
    begin
      x := start_x;
      continue;
    end
    else if c = #13 then
    begin
      y := y + 12;
      continue;
    end
    else if c = '_' then
      c := ' '
    else if (c = ' ') and (x = start_x) then // skip spaces at the start of a line
      continue;

    id := Ord(c) - Ord(HU_FONTSTART);
    if (id < 0) or (id >= Ord(HU_FONTSIZE)) then
    begin
      x := x + 4;
      continue;
    end;

    patch := yfont[id];
    w := patch.width;
    if x + w > 320 - 20 then
    begin // word warp
      x := start_x;
      y := y + 12;
    end;
    V_DrawPatch(x, y, scn, patch, scn = SCN_FG);
    x := x + w;
  end;
end;
{$ENDIF}

//==============================================================================
// HUlib_countLineLines
//
// sorta called by HU_Erase and just better darn get things straight
//
//==============================================================================
function HUlib_countLineLines(l: Phu_textline_t): integer;
var
  i: integer;
begin
  if l.len = 0 then
    result := 0
  else
  begin
    result := 1;
    for i := 1 to l.len - 1 do
      if l.line[i] = #10 then
        inc(result);
  end;
end;

//==============================================================================
//
// HUlib_eraseTextLine
//
//==============================================================================
procedure HUlib_eraseTextLine(l: Phu_textline_t);
var
  lh: integer;
  y: integer;
  yoffset: integer;
begin
  // Only erases when NOT in automap and the screen is reduced,
  // and the text must either need updating or refreshing
  // (because of a recent change back from the automap)
  if (amstate <> am_only) and (viewwindowx <> 0) and (l.needsupdate <> 0) then
  begin
    lh := l.font[0].height + 1;
    lh := lh * HU_MAXLINELINESS;
    y := V_PreserveY(l.y);
    yoffset := y * SCREENWIDTH;
    while y < V_PreserveY(l.y) + V_PreserveY(lh) + 1 do
    begin
      if (y < viewwindowy) or (y >= viewwindowy + viewheight) then
        {$IFDEF OPENGL}
        R_VideoErase(V_PreserveGLY(yoffset), GLDRAWWIDTH) // erase entire line
        {$ELSE}
        R_VideoErase(yoffset, SCREENWIDTH) // erase entire line
        {$ENDIF}
      else
      begin
        // erase left border
        {$IFDEF OPENGL}
        R_VideoErase(V_PreserveGLY(yoffset), V_PreserveGLX(viewwindowx));
        // erase right border     asfa
        R_VideoErase(V_PreserveGLY(yoffset + viewwindowx + viewwidth), V_PreserveGLX(viewwindowx));
        {$ELSE}
        R_VideoErase(yoffset, viewwindowx);
        // erase right border
        R_VideoErase(yoffset + viewwindowx + viewwidth, viewwindowx);
        {$ENDIF}
      end;
      inc(y);
      yoffset := yoffset + SCREENWIDTH;
    end;
  end;

  if l.needsupdate > 0 then
    l.needsupdate := l.needsupdate - 1;
end;

//==============================================================================
//
// HUlib_initSText
//
//==============================================================================
procedure HUlib_initSText(s: Phu_stext_t; x: integer; y: integer; h: integer;
  font: Ppatch_tPArray; startchar: integer; _on: Pboolean);
var
  i: integer;
begin
  s.height := h;
  s._on := _on;
  s.laston := true;
  s.curline := 0;
  for i := 0 to h - 1 do
    HUlib_initTextLine(@s.lines[i], x, y - i * (font[0].height + 1),
      font, startchar);
end;

//==============================================================================
//
// HUlib_addLineToSText
//
//==============================================================================
procedure HUlib_addLineToSText(s: Phu_stext_t);
var
  i: integer;
begin
  // add a clear line
  inc(s.curline);
  if s.curline = s.height then
    s.curline := 0;
  HUlib_clearTextLine(@s.lines[s.curline]);

  // everything needs updating
  for i := 0 to s.height - 1 do
    s.lines[i].needsupdate := 4;
end;

//==============================================================================
//
// HUlib_removeLineFromSText
//
//==============================================================================
procedure HUlib_removeLineFromSText(s: Phu_stext_t);
var
  i: integer;
  len: integer;
  sl: TDStringList;
  ss: string;
begin
  len := s.lines[s.curline].len;
  ss := '';
  for i := 0 to len - 1 do
    ss := ss + s.lines[s.curline].line[i];
  sl := TDStringList.Create;
  try
    sl.Text := ss;
    if sl.Count > 0 then
      sl.Delete(0);
    ss := sl.Text;
    if Length(ss) > 1 then
      if Copy(ss, Length(ss) - 1, 2) = #13#10 then
        SetLength(ss, Length(ss) - 2);
    len := Length(ss);
    for i := 1 to len do
      s.lines[s.curline].line[i - 1] := ss[i];
    s.lines[s.curline].len := len;
  finally
    sl.Free;
  end;

  s.lines[s.curline].needsupdate := 4;
end;

//==============================================================================
//
// HUlib_addMessageToSText
//
//==============================================================================
procedure HUlib_addMessageToSText(s: Phu_stext_t; prefix: string; msg: string);
var
  i: integer;
begin
  HUlib_addLineToSText(s);

  printf('%s%s'#13#10, [prefix, msg]);

  for i := 1 to Length(prefix) do
    HUlib_addCharToTextLine(@s.lines[s.curline], prefix[i]);

  for i := 1 to Length(msg) do
    HUlib_addCharToTextLine(@s.lines[s.curline], msg[i]);
end;

//==============================================================================
//
// HUlib_addMessageToSText2
//
//==============================================================================
procedure HUlib_addMessageToSText2(s: Phu_stext_t; prefix: string; msg: string);
var
  i: integer;
begin
  while HUlib_countLineLines(@s.lines[s.curline]) >= HU_MAXLINELINESS do
  begin
    HUlib_removeLineFromSText(s);
  end;

  printf('%s%s'#13#10, [prefix, msg]);

  if s.lines[s.curline].len > 0 then
  begin
    HUlib_addCharToTextLine(@s.lines[s.curline], #13);
    HUlib_addCharToTextLine(@s.lines[s.curline], #10);
  end;

  for i := 1 to Length(prefix) do
    HUlib_addCharToTextLine(@s.lines[s.curline], prefix[i]);

  for i := 1 to Length(msg) do
    HUlib_addCharToTextLine(@s.lines[s.curline], msg[i]);

  s.lines[s.curline].needsupdate := 4;

end;

//==============================================================================
//
// HUlib_drawSText
//
//==============================================================================
procedure HUlib_drawSText(s: Phu_stext_t);
var
  i, idx: integer;
  l: Phu_textline_t;
begin
  if not s._on^ then
    exit; // if not on, don't draw

  // draw everything
  for i := 0 to s.height - 1 do
  begin
    idx := s.curline - i;
    if idx < 0 then
      idx := idx + s.height; // handle queue of lines

    l := @s.lines[idx];

    // need a decision made here on whether to skip the draw
    HUlib_drawTextLine(l, false); // no cursor, please
  end;
end;

//==============================================================================
//
// HUlib_eraseSText
//
//==============================================================================
procedure HUlib_eraseSText(s: Phu_stext_t);
var
  i: integer;
begin
  for i := 0 to s.height - 1 do
  begin
    if s.laston and not s._on^ then
      s.lines[i].needsupdate := 4;
    HUlib_eraseTextLine(@s.lines[i]);
  end;
  s.laston := s._on^;
end;

//==============================================================================
//
// HUlib_initIText
//
//==============================================================================
procedure HUlib_initIText(it: Phu_itext_t; x: integer; y: integer; font: Ppatch_tPArray;
  startchar: integer; _on: Pboolean);
begin
  it.lm := 0; // default left margin is start of text
  it._on := _on;
  it.laston := true;
  HUlib_initTextLine(@it.line, x, y, font, startchar);
end;

//==============================================================================
// HUlib_delCharFromIText
//
// The following deletion routines adhere to the left margin restriction
//
//==============================================================================
procedure HUlib_delCharFromIText(it: Phu_itext_t);
begin
  if it.line.len <> it.lm then
    HUlib_delCharFromTextLine(@it.line);
end;

//==============================================================================
//
// HUlib_eraseLineFromIText
//
//==============================================================================
procedure HUlib_eraseLineFromIText(it: Phu_itext_t);
begin
  while it.lm <> it.line.len do
    HUlib_delCharFromTextLine(@it.line);
end;

//==============================================================================
// HUlib_resetIText
//
// Resets left margin as well
//
//==============================================================================
procedure HUlib_resetIText(it: Phu_itext_t);
begin
  it.lm := 0;
  HUlib_clearTextLine(@it.line);
end;

//==============================================================================
//
// HUlib_addPrefixToIText
//
//==============================================================================
procedure HUlib_addPrefixToIText(it: Phu_itext_t; str: string);
var
  i: integer;
begin
  for i := 1 to Length(str) do
    HUlib_addCharToTextLine(@it.line, str[i]);
  it.lm := it.line.len;
end;

//==============================================================================
// HUlib_keyInIText
//
// wrapper function for handling general keyed input.
// returns true if it ate the key
//
//==============================================================================
function HUlib_keyInIText(it: Phu_itext_t; ch: byte): boolean;
begin
  if (ch >= Ord(' ')) and (ch <= Ord('_')) then
    HUlib_addCharToTextLine(@it.line, Chr(ch))
  else
  if ch = KEY_BACKSPACE then
    HUlib_delCharFromIText(it)
  else if ch <> KEY_ENTER then
  begin
    result := false; // did not eat key
    exit;
  end;

  result := true; // ate the key
end;

//==============================================================================
//
// HUlib_drawIText
//
//==============================================================================
procedure HUlib_drawIText(it: Phu_itext_t);
begin
  if it._on^ then
    HUlib_drawTextLine(@it.line, true); // draw the line w/ cursor
end;

//==============================================================================
//
// HUlib_eraseIText
//
//==============================================================================
procedure HUlib_eraseIText(it: Phu_itext_t);
begin
  if it.laston and not it._on^ then
    it.line.needsupdate := 4;
  HUlib_eraseTextLine(@it.line);
  it.laston := it._on^;
end;

end.

