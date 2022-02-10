//------------------------------------------------------------------------------
//
//  DelphiHeretic is a source port of the game Heretic and it is
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
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
// DESCRIPTION:
//  Switches, buttons. Two-state animation. Exits.
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

//==============================================================================
//
// P_UseSpecialLine
//
//==============================================================================
function P_UseSpecialLine(thing: Pmobj_t; line: Pline_t; side: integer; const bossaction: boolean = false): boolean;

var
  buttonlist: array[0..MAXBUTTONS - 1] of button_t;

implementation

uses
  doomdata,
  m_fixed,
  p_setup,
  p_plats,
  p_doors,
  p_ceilng,
  p_floor,
  i_system,
  g_game,
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
    episode: smallint;
  end;
  Pswitchlist_t = ^switchlist_t;

const
  NUMSWITCHLIST = 3;

  alphSwitchList: array[0..NUMSWITCHLIST - 1] of switchlist_t = (
    // Heretic switches
    (name1: 'SW1OFF';   name2: 'SW1ON';    episode: 0),
    (name1: 'SW2OFF';   name2: 'SW2ON';    episode: 0),

    (name1: '';         name2: '';         episode: -1)
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
    if alphSwitchList[i].episode = -1 then
    begin
      numswitches := index div 2;
      switchlist[index] := -1;
      break;
    end;

    if alphSwitchList[i].episode = 0 then
    begin
      switchlist[index] := R_TextureNumForName(alphSwitchList[i].name1);
      inc(index);
      switchlist[index] := R_TextureNumForName(alphSwitchList[i].name2);
      inc(index);
    end;
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
  sound: integer;
  sdnum: integer;
begin
  if not useAgain then
    line.special := 0;

  sdnum := line.sidenum[0];
  texTop := sides[sdnum].toptexture;
  texMid := sides[sdnum].midtexture;
  texBot := sides[sdnum].bottomtexture;

  sound := Ord(sfx_switch);

  for i := 0 to numswitches * 2 - 1 do
  begin
    if switchlist[i] = texTop then
    begin
      S_StartSound(buttonlist[0].soundorg, sound);
      sides[line.sidenum[0]].toptexture := switchlist[i xor 1];

      if useAgain then
        P_StartButton(line, top, switchlist[i], BUTTONTIME);

      exit;
    end
    else
    begin
      if switchlist[i] = texMid then
      begin
        S_StartSound(buttonlist[0].soundorg, sound);
        sides[line.sidenum[0]].midtexture := switchlist[i xor 1];

        if useAgain then
          P_StartButton(line, middle, switchlist[i], BUTTONTIME);

        exit;
      end
      else
      begin
        if switchlist[i] = texBot then
        begin
          S_StartSound(buttonlist[0].soundorg, sound);
          sides[line.sidenum[0]].bottomtexture := switchlist[i xor 1];

          if useAgain then
            P_StartButton(line, bottom, switchlist[i], BUTTONTIME);

          exit;
        end;
      end;
    end;
  end;
end;

//==============================================================================
//
// P_UseSpecialLine
// Called when a thing uses a special line.
// Only the front sides of lines are usable.
//
//==============================================================================
function P_UseSpecialLine(thing: Pmobj_t; line: Pline_t; side: integer; const bossaction: boolean = false): boolean;
begin
  // Switches that other things can activate.
  if thing.player = nil then
  begin
    // never open secret doors
    if line.flags and ML_SECRET <> 0 then
    begin
      result := false;
      exit;
    end;

    case line.special of
       1:; // MANUAL DOOR RAISE
      32:; // MANUAL BLUE
      33:; // MANUAL RED
      34:; // MANUAL YELLOW
    else
      begin
        result := false;
        exit;
      end;
    end;
  end;

  if bossaction then
    case line.special of
    // MANUALS
       1, // Vertical Door
      26, // Blue Door/Locked
      27, // Yellow Door /Locked
      28, // Red Door /Locked

      31, // Manual door open
      32, // Blue locked door open
      33, // Red locked door open
      34: // Yellow locked door open
        begin
          result := false;
          exit;
        end;
    end;

  // do something
  case line.special of
  // MANUALS
     1, // Vertical Door
    26, // Blue Door/Locked
    27, // Yellow Door /Locked
    28, // Red Door /Locked

    31, // Manual door open
    32, // Blue locked door open
    33, // Red locked door open
    34: // Yellow locked door open

      EV_VerticalDoor(line, thing);

  //UNUSED - Door Slide Open&Close
  // case 124:
  // EV_SlidingDoor (line, thing);
  // break;

  // SWITCHES
     7:
      begin
        // Build Stairs
        if EV_BuildStairs(line, 8 * FRACUNIT) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;
   107:
      begin
        // Build Stairs
        if EV_BuildStairs(line, 16 * FRACUNIT) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;
     9:
      begin
        // Change Donut
        if EV_DoDonut(line) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;
    11:
      begin
        // Exit level
        P_ChangeSwitchTexture(line, false);
        G_ExitLevel;
      end;

    14:
      begin
        // Raise Floor 32 and change texture
        if EV_DoPlat(line, raiseAndChange, 32) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    15:
      begin
        // Raise Floor 24 and change texture
        if EV_DoPlat(line, raiseAndChange, 24) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    18:
      begin
        // Raise Floor to next highest floor
        if EV_DoFloor(line, raiseFloorToNearest) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    20:
      begin
        // Raise Plat next highest floor and change texture
        if EV_DoPlat(line, raiseToNearestAndChange, 0) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    21:
      begin
        // PlatDownWaitUpStay
        if EV_DoPlat(line, downWaitUpStay, 0) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    23:
      begin
        // Lower Floor to Lowest
        if EV_DoFloor(line, lowerFloorToLowest) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    29:
      begin
        // Raise Door
        if EV_DoDoor(line, normal) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    41:
      begin
        // Lower Ceiling to Floor
        if EV_DoCeiling(line, lowerToFloor) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    71:
      begin
        // Turbo Lower Floor
        if EV_DoFloor(line, turboLower) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    49:
      begin
        // Ceiling Crush And Raise
        if EV_DoCeiling(line, lowerAndCrush) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    50:
      begin
        // Close Door
        if EV_DoDoor(line, close) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    51:
      begin
        // Secret EXIT
        P_ChangeSwitchTexture(line, false);
        G_SecretExitLevel;
      end;

    55:
      begin
        // Raise Floor Crush
        if EV_DoFloor(line, raiseFloorCrush) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

   101:
      begin
        // Raise Floor
        if EV_DoFloor(line, raiseFloor) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

   102:
      begin
        // Lower Floor to Surrounding floor height
        if EV_DoFloor(line, lowerFloor) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

   103:
      begin
        // Open Door
        if EV_DoDoor(line, open) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

  // BUTTONS
    42:
      begin
        // Close Door
        if EV_DoDoor(line, close) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    43:
      begin
        // Lower Ceiling to Floor
        if EV_DoCeiling(line, lowerToFloor) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    45:
      begin
        // Lower Floor to Surrounding floor height
        if EV_DoFloor(line, lowerFloor) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    60:
      begin
        // Lower Floor to Lowest
        if EV_DoFloor(line, lowerFloorToLowest) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    61:
      begin
        // Open Door
        if EV_DoDoor(line, open) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    62:
      begin
        // PlatDownWaitUpStay
        if EV_DoPlat(line, downWaitUpStay, 1) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    63:
      begin
        // Raise Door
        if EV_DoDoor(line, normal) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    64:
      begin
        // Raise Floor to ceiling
        if EV_DoFloor(line, raiseFloor) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    66:
      begin
        // Raise Floor 24 and change texture
        if EV_DoPlat(line, raiseAndChange, 24) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    67:
      begin
        // Raise Floor 32 and change texture
        if EV_DoPlat(line, raiseAndChange, 32) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    65:
      begin
        // Raise Floor Crush
        if EV_DoFloor(line, raiseFloorCrush) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    68:
      begin
        // Raise Plat to next highest floor and change texture
        if EV_DoPlat(line, raiseToNearestAndChange, 0) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    69:
      begin
        // Raise Floor to next highest floor
        if EV_DoFloor(line, raiseFloorToNearest) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    70:
      begin
        // Turbo Lower Floor
        if EV_DoFloor(line, turboLower) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

  end;
  result := true;
end;

end.
