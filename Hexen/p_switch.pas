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

unit p_switch;

interface

uses
  r_defs,
  p_mobj_h,
  p_spec;

//==============================================================================
//
// P_InitSwitchList
//
//==============================================================================
procedure P_InitSwitchList;

//==============================================================================
//
// P_ChangeSwitchTexture
//
//==============================================================================
procedure P_ChangeSwitchTexture(line: Pline_t; useAgain: boolean);

var
  buttonlist: array[0..MAXBUTTONS - 1] of button_t;

implementation

uses
  p_setup,
  i_system,
  s_sound,
  r_data,
// Data
  sounddata;

type
//
// P_SWITCH
//
  switchlist_t = record
    name1: string[8];
    name2: string[8];
    soundID: integer;
  end;
  Pswitchlist_t = ^switchlist_t;

const
  NUMSWITCHLIST = 11;

  alphSwitchList: array[0..NUMSWITCHLIST - 1] of switchlist_t = (
    // Hexen switches
    (name1: 'SW_1_UP';  name2: 'SW_1_DN';  soundID: Ord(SFX_SWITCH1)),
    (name1: 'SW_2_UP';  name2: 'SW_2_DN';  soundID: Ord(SFX_SWITCH1)),
    (name1: 'VALVE1';   name2: 'VALVE2';   soundID: Ord(SFX_VALVE_TURN)),
    (name1: 'SW51_OFF'; name2: 'SW51_ON';  soundID: Ord(SFX_SWITCH2)),
    (name1: 'SW52_OFF'; name2: 'SW52_ON';  soundID: Ord(SFX_SWITCH2)),
    (name1: 'SW53_UP';  name2: 'SW53_DN';  soundID: Ord(SFX_ROPE_PULL)),
    (name1: 'PUZZLE5';  name2: 'PUZZLE9';  soundID: Ord(SFX_SWITCH1)),
    (name1: 'PUZZLE6';  name2: 'PUZZLE10'; soundID: Ord(SFX_SWITCH1)),
    (name1: 'PUZZLE7';  name2: 'PUZZLE11'; soundID: Ord(SFX_SWITCH1)),
    (name1: 'PUZZLE8';  name2: 'PUZZLE12'; soundID: Ord(SFX_SWITCH1)),

    (name1: '';         name2: '';         soundID: -1)
  );

var
  switchlist: array[0..2 * MAXSWITCHES - 1] of integer;
  numswitches: integer;

//==============================================================================
//
// P_InitSwitchList
// Only called at game initialization.
//
//==============================================================================
procedure P_InitSwitchList;
var
  i: integer;
  index: integer;
begin

  index := 0;
  for i := 0 to MAXSWITCHES - 1 do
  begin
    if alphSwitchList[i].soundID = -1 then
    begin
      numswitches := index div 2;
      switchlist[index] := -1;
      break;
    end;

    switchlist[index] := R_CheckTextureNumForName(alphSwitchList[i].name1); // JVAL: was R_TextureNumForName
    inc(index);
    switchlist[index] := R_CheckTextureNumForName(alphSwitchList[i].name2); // JVAL: was R_TextureNumForName
    inc(index);
  end;
end;

//==============================================================================
// P_StartButton
//
// Start a button counting down till it turns off.
//
//==============================================================================
procedure P_StartButton(line: Pline_t; w: bwhere_e; texture: integer; time: integer);
var
  i: integer;
begin
  // See if button is already pressed
  for i := 0 to MAXBUTTONS - 1 do
    if (buttonlist[i].btimer <> 0) and (buttonlist[i].line = line) then
      exit;

  for i := 0 to MAXBUTTONS - 1 do
  begin
    if buttonlist[i].btimer = 0 then
    begin
      buttonlist[i].line := line;
      buttonlist[i].where := w;
      buttonlist[i].btexture := texture;
      buttonlist[i].btimer := time;
      buttonlist[i].soundorg := Pmobj_t(@line.frontsector.soundorg);
      exit;
    end;
  end;

  I_Error('P_StartButton(): no button slots left!');
end;

//==============================================================================
// P_ChangeSwitchTexture
//
// Function that changes wall texture.
// Tell it if switch is ok to use again (1=yes, it's a button).
//
//==============================================================================
procedure P_ChangeSwitchTexture(line: Pline_t; useAgain: boolean);
var
  texTop: integer;
  texMid: integer;
  texBot: integer;
  i: integer;
  sdnum: integer;
begin
  if not useAgain then
    line.special := 0;

  sdnum := line.sidenum[0];
  texTop := sides[sdnum].toptexture;
  texMid := sides[sdnum].midtexture;
  texBot := sides[sdnum].bottomtexture;

  for i := 0 to numswitches * 2 - 1 do
  begin
    if switchlist[i] = texTop then
    begin
      S_StartSound(@line.frontsector.soundorg, alphSwitchList[i div 2].soundID);
      sides[line.sidenum[0]].toptexture := switchlist[i xor 1];

      if useAgain then
        P_StartButton(line, SWTCH_TOP, switchlist[i], BUTTONTIME);

      exit;
    end
    else
    begin
      if switchlist[i] = texMid then
      begin
        S_StartSound(@line.frontsector.soundorg, alphSwitchList[i div 2].soundID);
        sides[line.sidenum[0]].midtexture := switchlist[i xor 1];

        if useAgain then
          P_StartButton(line, SWTCH_MIDDLE, switchlist[i], BUTTONTIME);

        exit;
      end
      else
      begin
        if switchlist[i] = texBot then
        begin
          S_StartSound(@line.frontsector.soundorg, alphSwitchList[i div 2].soundID);
          sides[line.sidenum[0]].bottomtexture := switchlist[i xor 1];

          if useAgain then
            P_StartButton(line, SWTCH_BOTTOM, switchlist[i], BUTTONTIME);

          exit;
        end;
      end;
    end;
  end;
end;

end.
