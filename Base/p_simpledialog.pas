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
// DESCRIPTION:
//   Simple dialogs
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_simpledialog;

interface

uses
  p_mobj_h;

//==============================================================================
//
// A_SimpleDialog
//
//==============================================================================
procedure A_SimpleDialog(actor: Pmobj_t);

implementation

uses
  d_delphi,
  g_game,
{$IFDEF  HEXEN}
  g_demo,
{$ENDIF}
  hu_stuff,
  m_menu,
  p_common,
  r_defs,
  s_sound,
  sounddata,
  v_data,
  v_video,
  w_pak,
  w_wad,
  z_zone;

const
  FOLDER_DIALOGS = 'DIALOGS';

//==============================================================================
//
// P_SimpleDialogLoadText
//
//==============================================================================
function P_SimpleDialogLoadText(const lumpname: string): string;
var
  lump: integer;
  strm: TPakStream;
  sl: TDStringList;
begin
  if Length(lumpname) <= 8 then
  begin
    lump := W_CheckNumForName(lumpname);
    if lump >= 0 then
    begin
      Result := W_TextLumpNum(lump);
      Exit;
    end;
  end;

  strm := TPakStream.Create(lumpname, pm_short, '', FOLDER_DIALOGS);
  if strm.IOResult <> 0 then
  begin
    strm.Free;
    strm := TPakStream.Create(lumpname, pm_short, '');
    if strm.IOResult <> 0 then
    begin
      strm.Free;
      Result := '';
      Exit;
    end;
  end;

  sl := TDStringList.Create;
  sl.LoadFromStream(strm);
  Result := sl.Text;
  sl.Free;
  strm.Free;
end;

//==============================================================================
//
// M_SimpleDialogDimMsg
//
//==============================================================================
function M_SimpleDialogDimMsg(x, y: integer; str: string): string;
var
  maxwidth: integer;
  i, j: integer;
  lst, lst2: TDStringList;
  line, s, s1, s2: string;

  function _StringWidth(const s: string): integer;
  var
    l: integer;
    c: integer;
  begin
    result := 0;
    for l := 1 to Length(s) do
    begin
      c := Ord(toupper(s[l])) - Ord(HU_FONTSTART);
      if (c < 0) or (c >= HU_FONTSIZE) then
        result := result + 4
      else
        result := result + hu_font[c].width;
    end;
  end;

begin
  maxwidth := 320 - 2 * x;

  result := '';

  if str = '' then
    exit;

  lst := TDStringList.Create;

  lst.Text := strupper(str);

  for i := 0 to lst.Count - 1 do
  begin
    lst2 := TDStringList.Create;
    s := lst.Strings[i];
    repeat
      splitstring_ch(s, s1, s2);
      lst2.Add(s1);
      s := s2;
    until s = '';

    line := '';
    for j := 0 to lst2.Count - 1 do
    begin
      if (_StringWidth(line + lst2.Strings[j]) <= maxwidth) or (line = '') then
      begin
        if line = '' then
          line := lst2.Strings[j]
        else
          line := line + ' ' + lst2.Strings[j];
      end
      else
      begin
        result := result + line + #13#10;
        line := lst2.Strings[j];
      end;
    end;
    lst2.Free;
    if line <> '' then
    begin
      result := result + line;
    end;
      if i <> lst.Count - 1 then
        result := result + #13#10;
  end;
  lst.Free;
end;

const
  MAXSIMPLEDIALOGMENUITEMS = 32;

var
  simpledialogmenuitems: array[0..MAXSIMPLEDIALOGMENUITEMS - 1] of menuitem_t;
  simpledialogmenus: array[0..MAXSIMPLEDIALOGMENUITEMS - 1] of menu_t;
  simpledialoglumps: array[0..MAXSIMPLEDIALOGMENUITEMS - 1] of string;
  numsimpledialogs: Integer;
  currentsimpledialog: Integer;

//==============================================================================
//
// M_SimpleDialogChoice
//
//==============================================================================
procedure M_SimpleDialogChoice(choice: Integer);
begin
  Inc(currentsimpledialog);
  if currentsimpledialog = numsimpledialogs then
    menuactive := False
  else
    M_SetupNextMenu(@simpledialogmenus[currentsimpledialog]);
end;

//==============================================================================
//
// M_SimpleDialogDrawer
//
//==============================================================================
procedure M_SimpleDialogDrawer;
var
  str: string;
begin
  V_DrawPatchTransparent(20, 10, SCN_FG, W_CacheLumpName('M_DIALOG', PU_STATIC), true);
  str := M_SimpleDialogDimMsg(39, 17, P_SimpleDialogLoadText(simpledialoglumps[currentsimpledialog]));
  M_WriteText(39, 17, str);
end;

//==============================================================================
//
// A_SimpleDialog
//
//==============================================================================
procedure A_SimpleDialog(actor: Pmobj_t);
var
  i, cnt: integer;
  pmi: Pmenuitem_t;
  sndid: integer;
begin
  if menuactive or netgame then
    exit;

  if demoplayback or demorecording then
    exit;

  if not P_CheckStateParams(actor, 1, CSP_AT_LEAST) then
    exit;

  cnt := actor.state.params.Count;
  if cnt > MAXSIMPLEDIALOGMENUITEMS then
    cnt := MAXSIMPLEDIALOGMENUITEMS;

  numsimpledialogs := cnt;
  currentsimpledialog := 0;

  // Generate the menu sequence
  pmi := @simpledialogmenuitems[0];
  for i := 0 to cnt - 1 do
  begin
    pmi.status := 1;
    pmi.name := '';
    pmi.cmd := '';
    pmi.routine := @M_SimpleDialogChoice;
    pmi.pBoolVal := nil;
    pmi.alphaKey := ' ';
    Inc(pmi);

    simpledialoglumps[i] := actor.state.params.StrVal[i];

    simpledialogmenus[i].numitems := 1;
    simpledialogmenus[i].prevMenu := nil;
    simpledialogmenus[i].leftMenu := nil;
    simpledialogmenus[i].rightMenu := nil;
    simpledialogmenus[i].menuitems := Pmenuitem_tArray(@simpledialogmenuitems[i]);
    simpledialogmenus[i].drawproc := @M_SimpleDialogDrawer;  // draw routine
    simpledialogmenus[i].x := 0;
    simpledialogmenus[i].y := 0;
    simpledialogmenus[i].lastOn := 0;
    simpledialogmenus[i].itemheight := 10;
    simpledialogmenus[i].runonselect := true;
  end;

  M_StartControlPanel;
  {$IFDEF DOOM_OR_STRIFE}
  sndid := Ord(sfx_swtchn);
  {$ENDIF}
  {$IFDEF HERETIC}
  sndid := Ord(sfx_dorcls);
  {$ENDIF}
  {$IFDEF HEXEN}
  sndid := Ord(SFX_DOOR_LIGHT_CLOSE);
  {$ENDIF}
  S_StartSound(nil, sndid);
  currentMenu := @simpledialogmenus[0];
end;

end.

