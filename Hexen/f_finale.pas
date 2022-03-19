//------------------------------------------------------------------------------
//
//  DelphiHexen is a source port of the game Hexen and it is
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit f_finale;

interface

uses
  d_event;

//==============================================================================
// F_Responder
//
//-----------------------------------------------------------------------------
//
// DESCRIPTION:
// Game completion, final screen animation.
//
//-----------------------------------------------------------------------------
//
//==============================================================================
function F_Responder(ev: Pevent_t): boolean;

{ Called by main loop. }

//==============================================================================
//
// F_Ticker
//
//==============================================================================
procedure F_Ticker;

{ Called by main loop. }

//==============================================================================
//
// F_Drawer
//
//==============================================================================
procedure F_Drawer;

//==============================================================================
//
// F_StartFinale
//
//==============================================================================
procedure F_StartFinale;

implementation

uses
  d_delphi,
  i_system,
  {$IFDEF OPENGL}
  gl_main,
  {$ELSE}
  i_video,
  {$ENDIF}
  m_fixed,
  am_map,
  g_game,
  d_player,
  hu_stuff,
  p_inter,
  s_sound,
  sounddata,
  v_data,
  v_video,
  doomdef,
  w_wad,
  z_zone;

var
// Stage of animation:
//  0 = text, 1 = art screen, 2 = character cast
  FinaleStage: integer;
  FinaleCount: integer;

const
  TEXTSPEED = 3;
  TEXTWAIT = 250;

var
  finaletext: string;
  FinaleEndCount: integer;
  FinaleLumpNum: integer;
  Palette: Pfixed_tArray;
  PaletteDelta: Pfixed_tArray;
  RealPalette: PByteArray;

const
  winMsgLumpNames: array[0..2] of string = (
    'win1msg',
    'win2msg',
    'win3msg'
  );

//==============================================================================
//
// F_GetFinaleText
//
//==============================================================================
function F_GetFinaleText(sequence: integer): string;
begin
  result := W_TextLumpName(winMsgLumpNames[sequence]);
  if length(result) >= MAX_INTRMSN_MESSAGE_SIZE then
    I_Warning('F_GetFinaleText(): Finale message too long (%s)', [winMsgLumpNames[sequence]]);
end;

//==============================================================================
//
// F_InitializeFade
//
//==============================================================================
procedure F_InitializeFade(fadeIn: boolean);
var
  i: integer;
  pal: PByteArray;
begin
  Palette := Z_Malloc(768 * SizeOf(fixed_t), PU_STATIC, nil);
  PaletteDelta := Z_Malloc(768 * SizeOf(fixed_t), PU_STATIC, nil);
  RealPalette := Z_Malloc(768 * SizeOf(byte), PU_STATIC, nil);

  pal := W_CacheLumpName('playpal', PU_STATIC);

  if fadeIn then
  begin
    memset(RealPalette, 0, 768 * SizeOf(byte));
    for i := 0 to 767 do
    begin
      Palette[i] := 0;
      PaletteDelta[i] := FixedDiv(pal[i] * FRACUNIT, 70 * FRACUNIT);
    end;
  end
  else
  begin
    for i := 0 to 767 do
    begin
      RealPalette[i] := pal[i];
      Palette[i] := RealPalette[i] * FRACUNIT;
      PaletteDelta[i] := FixedDiv(Palette[i], -70 * FRACUNIT);
    end;
  end;
  I_SetPalette(RealPalette);
  Z_ChangeTag(pal, PU_CACHE);
end;

//==============================================================================
//
// F_DeInitializeFade
//
//==============================================================================
procedure F_DeInitializeFade;
begin
  Z_Free(Palette);
  Z_Free(PaletteDelta);
  Z_Free(RealPalette);
end;

//==============================================================================
//
// F_StartFinale
//
//==============================================================================
procedure F_StartFinale;
begin
  gameaction := ga_nothing;
  gamestate := GS_FINALE;
  viewactive := false;
  amstate := am_inactive;
  P_ClearMessage(@players[consoleplayer]);

  FinaleStage := 0;
  FinaleCount := 0;
  FinaleText := F_GetFinaleText(0);
  FinaleEndCount := 70;
  FinaleLumpNum := W_GetNumForName('FINALE1');
  F_InitializeFade(true);

  S_ChangeMusic(Ord(mus_hall), false); // don't loop the song
end;

//==============================================================================
//
// F_Responder
//
//==============================================================================
function F_Responder(ev: Pevent_t): boolean;
begin
  result := false;
end;

//==============================================================================
// F_FadePic
//
// FadePic
//
//==============================================================================
procedure F_FadePic;
var
  i: integer;
begin
  for i := 0 to 767 do
  begin
    Palette[i] := Palette[i] + PaletteDelta[i];
    RealPalette[i] := FixedInt(Palette[i]);
  end;
  I_SetPalette(RealPalette);
end;

//==============================================================================
//
// F_Ticker
//
//==============================================================================
procedure F_Ticker;
begin
  inc(FinaleCount);
  if (FinaleStage < 5) and (FinaleCount >= FinaleEndCount) then
  begin
    FinaleCount := 0;
    inc(FinaleStage);
    case FinaleStage of
      1: // Text 1
        begin
          FinaleEndCount := Length(FinaleText) * TEXTSPEED + TEXTWAIT;
        end;
      2: // Pic 2, Text 2
        begin
          FinaleText := F_GetFinaleText(1);
          FinaleEndCount := Length(FinaleText) * TEXTSPEED + TEXTWAIT;
          FinaleLumpNum := W_GetNumForName('FINALE2');
          S_ChangeMusic(Ord(mus_orb), false);
        end;
      3: // Pic 2 -- Fade out
        begin
          FinaleEndCount := 70;
          F_DeInitializeFade;
          F_InitializeFade(false);
        end;
      4: // Pic 3 -- Fade in
        begin
          FinaleLumpNum := W_GetNumForName('FINALE3');
          FinaleEndCount := 71;
          F_DeInitializeFade;
          F_InitializeFade(true);
          S_ChangeMusic(Ord(mus_chess), true);
        end;
      5: // Pic 3 , Text 3
        begin
          FinaleText := F_GetFinaleText(2);
          F_DeInitializeFade;
        end;
    end;
    exit;
  end;

  if (FinaleStage = 0) or (FinaleStage = 3) or (FinaleStage = 4) then
    F_FadePic;
end;

//==============================================================================
//
// F_TextWrite
//
//==============================================================================
procedure F_TextWrite;
var
  w: integer;
  count: integer;
  ch: string;
  c: char;
  c1: integer;
  i: integer;
  len: integer;
  cx: integer;
  cy: integer;
begin
  // erase the entire screen to a tiled background

  memcpy(@screens[SCN_TMP][0], W_CacheLumpNum(FinaleLumpNum, PU_CACHE), 320 * 200);

  if FinaleStage = 5 then
  begin // Chess pic, draw the correct character graphic
    if netgame then
    begin
      V_DrawPatch(20, 0, SCN_TMP, 'chessall', false);
    end
    else if Ord(PlayerClass[consoleplayer]) <> 0 then
    begin
      V_DrawPatch(60, 0, SCN_TMP, W_CacheLumpNum(W_GetNumForName('chessc') + Ord(PlayerClass[consoleplayer]) - 1, PU_CACHE), false);
    end;
  end;
  // Draw the actual text
  if FinaleStage = 5 then
  begin
    cy := 135;
  end
  else
  begin
    cy := 5;
  end;

  // draw some of the text onto the screen
  cx := 20;

  ch := finaletext;
  len := Length(ch);

  count := (finalecount - 10) div TEXTSPEED;
  if count < 0 then
    count := 0;

  i := 1;
  while count > 0 do
  begin

    if i > len then
      break;

    c := ch[i];
    inc(i);
    if c = #13 then
    begin
      cy := cy + 9;
      continue;
    end;
    if c = #10 then
    begin
      cx := 10;
      continue;
    end;

    c1 := Ord(toupper(c)) - Ord(HU_FONTSTART);
    if (c1 < 0) or (c1 > HU_FONTSIZE) then
    begin
      cx := cx + 5;
      continue;
    end;

    w := hu_font[c1].width;
    if cx + w > 320 then
      break;
    V_DrawPatch(cx, cy, SCN_TMP, hu_font[c1], false);
    cx := cx + w;
    dec(count);
  end;
  V_CopyRect(0, 0, SCN_TMP, 320, 200, 0, 0, SCN_FG, true);

  V_FullScreenStretch;
end;

//==============================================================================
//
// F_DrawPic
//
//==============================================================================
procedure F_DrawPic;
begin
  memcpy(@screens[SCN_TMP][0], W_CacheLumpNum(FinaleLumpNum, PU_CACHE), 320 * 200);

  if (FinaleStage = 4) or (FinaleStage = 5) then
  begin // Chess pic, draw the correct character graphic
    if netgame then
    begin
      V_DrawPatch(20, 0, SCN_TMP, W_CacheLumpName('chessall', PU_CACHE), false);
    end
    else if Ord(PlayerClass[consoleplayer]) <> 0 then
    begin
      V_DrawPatch(60, 0, SCN_TMP, W_CacheLumpNum(W_GetNumForName('chessc') + Ord(PlayerClass[consoleplayer]) - 1, PU_CACHE), false);
    end;
  end;
end;

//==============================================================================
//
// F_Drawer
//
//==============================================================================
procedure F_Drawer;
begin
  case FinaleStage of
    0: // Fade in initial finale screen
      F_DrawPic;
    1,
    2:
      F_TextWrite;
    3: // Fade screen out
      F_DrawPic;
    4: // Fade in chess screen
      F_DrawPic;
    5:
      F_TextWrite;
  end;
end;

end.

