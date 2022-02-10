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
//  DESCRIPTION:
//   Switches, buttons. Two-state animation. Exits.
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
  d_delphi,
  doomdata,
  p_setup,
  p_lights,
  p_plats,
  p_doors,
  p_ceilng,
  p_floor,
  p_genlin,
  p_telept,
  i_system,
  doomdef,
  g_game,
  s_sound,
  r_data,
  w_wad,
  z_zone,
// Data
  sounddata,
// State
  doomstat;

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
  switchlist_tArray = array[0..$FFFF] of switchlist_t;
  Pswitchlist_tArray = ^switchlist_tArray;

var
  alphSwitchList: Pswitchlist_tArray;

type
  wad_switchlist_t = packed record
    name1: array[0..8] of char;
    name2: array[0..8] of char;
    episode: smallint;
  end;
  Pwad_switchlist_t = ^wad_switchlist_t;
  wad_switchlist_tArray = array[0..$FFFF] of wad_switchlist_t;
  Pwad_switchlist_tArray = ^wad_switchlist_tArray;

const
  NUMSWITCHLIST = 40;

  fixedalphSwitchList: array[0..NUMSWITCHLIST] of switchlist_t = (
    // Doom shareware episode 1 switches
    (name1: 'SW1BRCOM'; name2: 'SW2BRCOM'; episode: 1),
    (name1: 'SW1BRN1';  name2: 'SW2BRN1';  episode: 1),
    (name1: 'SW1BRN2';  name2: 'SW2BRN2';  episode: 1),
    (name1: 'SW1BRNGN'; name2: 'SW2BRNGN'; episode: 1),
    (name1: 'SW1BROWN'; name2: 'SW2BROWN'; episode: 1),
    (name1: 'SW1COMM';  name2: 'SW2COMM';  episode: 1),
    (name1: 'SW1COMP';  name2: 'SW2COMP';  episode: 1),
    (name1: 'SW1DIRT';  name2: 'SW2DIRT';  episode: 1),
    (name1: 'SW1EXIT';  name2: 'SW2EXIT';  episode: 1),
    (name1: 'SW1GRAY';  name2: 'SW2GRAY';  episode: 1),
    (name1: 'SW1GRAY1'; name2: 'SW2GRAY1'; episode: 1),
    (name1: 'SW1METAL'; name2: 'SW2METAL'; episode: 1),
    (name1: 'SW1PIPE';  name2: 'SW2PIPE';  episode: 1),
    (name1: 'SW1SLAD';  name2: 'SW2SLAD';  episode: 1),
    (name1: 'SW1STARG'; name2: 'SW2STARG'; episode: 1),
    (name1: 'SW1STON1'; name2: 'SW2STON1'; episode: 1),
    (name1: 'SW1STON2'; name2: 'SW2STON2'; episode: 1),
    (name1: 'SW1STONE'; name2: 'SW2STONE'; episode: 1),
    (name1: 'SW1STRTN'; name2: 'SW2STRTN'; episode: 1),

    // Doom registered episodes 2&3 switches
    (name1: 'SW1BLUE';  name2: 'SW2BLUE';  episode: 2),
    (name1: 'SW1CMT';   name2: 'SW2CMT';   episode: 2),
    (name1: 'SW1GARG';  name2: 'SW2GARG';  episode: 2),
    (name1: 'SW1GSTON'; name2: 'SW2GSTON'; episode: 2),
    (name1: 'SW1HOT';   name2: 'SW2HOT';   episode: 2),
    (name1: 'SW1LION';  name2: 'SW2LION';  episode: 2),
    (name1: 'SW1SATYR'; name2: 'SW2SATYR'; episode: 2),
    (name1: 'SW1SKIN';  name2: 'SW2SKIN';  episode: 2),
    (name1: 'SW1VINE';  name2: 'SW2VINE';  episode: 2),
    (name1: 'SW1WOOD';  name2: 'SW2WOOD';  episode: 2),

    // Doom II switches
    (name1: 'SW1PANEL'; name2: 'SW2PANEL'; episode: 3),
    (name1: 'SW1ROCK';  name2: 'SW2ROCK';  episode: 3),
    (name1: 'SW1MET2';  name2: 'SW2MET2';  episode: 3),
    (name1: 'SW1WDMET'; name2: 'SW2WDMET'; episode: 3),
    (name1: 'SW1BRIK';  name2: 'SW2BRIK';  episode: 3),
    (name1: 'SW1MOD1';  name2: 'SW2MOD1';  episode: 3),
    (name1: 'SW1ZIM';   name2: 'SW2ZIM';   episode: 3),
    (name1: 'SW1STON6'; name2: 'SW2STON6'; episode: 3),
    (name1: 'SW1TEK';   name2: 'SW2TEK';   episode: 3),
    (name1: 'SW1MARB';  name2: 'SW2MARB';  episode: 3),
    (name1: 'SW1SKULL'; name2: 'SW2SKULL'; episode: 3),

    (name1: '';         name2: '';         episode: -1)
  );

var
  switchlist: PIntegerArray;
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
  episode: integer;
  lump: integer;
  len: integer;
  wad_switchlist: Pwad_switchlist_tArray;
  tex1, tex2: integer;
begin

  if (gamemode = registered) or (gamemode = retail) then
    episode := 2
  else if gamemode = commercial then
    episode := 3
  else
    episode := 1;

  // JVAL: Check for 'SWITCHES' lump (BOOM compatibility)
  lump := W_CheckNumForName('SWITCHES');
  if lump >= 0 then
  begin
    wad_switchlist := W_CacheLumpNum(lump, PU_STATIC);
    len := W_LumpLength(lump);
    len := len div SizeOf(wad_switchlist_t);
    alphSwitchList := Z_Malloc((len + 1) * SizeOf(switchlist_t), PU_STATIC, nil);
    for i := 0 to len - 1 do
    begin
      alphSwitchList[i].name1 := wad_switchlist[i].name1;
      alphSwitchList[i].name2 := wad_switchlist[i].name2;
      alphSwitchList[i].episode := wad_switchlist[i].episode;
    end;
    Z_Free(wad_switchlist);
    alphSwitchList[len].name1 := '';
    alphSwitchList[len].name2 := '';
    alphSwitchList[len].episode := -1;
  end
  else
  begin
    len := NUMSWITCHLIST;
    alphSwitchList := Z_Malloc(SizeOf(fixedalphSwitchList), PU_STATIC, nil);
    memcpy(alphSwitchList, @fixedalphSwitchList, SizeOf(fixedalphSwitchList));
  end;

  index := 0;
  i := 0;
  // JVAL: Removed size of switchlist limit
  switchlist := Z_Malloc((len + 1) * 2 * SizeOf(integer), PU_STATIC, nil);
  while (alphSwitchList[i].episode <> -1) and (alphSwitchList[i].name1 <> '') and (alphSwitchList[i].name2 <> '') do
  begin
    if alphSwitchList[i].episode <= episode then
    begin
      tex1 := R_CheckTextureNumForName(alphSwitchList[i].name1);
      if tex1 < 0 then
        I_Warning('P_InitSwitchList(): Unknown texture "%s"'#13#10, [alphSwitchList[i].name1]);
      tex2 := R_CheckTextureNumForName(alphSwitchList[i].name2);
      if tex2 < 0 then
        I_Warning('P_InitSwitchList(): Unknown texture "%s"'#13#10, [alphSwitchList[i].name2]);
      if (tex1 >= 0) and (tex2 >= 0) then
      begin
        switchlist[index] := tex1;
        inc(index);
        switchlist[index] := tex2;
        inc(index);
      end;
    end;
    inc(i);
  end;
  numswitches := index div 2;
  switchlist[index] := -1;
  // JVAL: Resize to needed size
  switchlist := Z_ReAlloc(switchlist, (index + 1) * SizeOf(integer), PU_STATIC, nil);
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

  sound := Ord(sfx_swtchn);

  // EXIT SWITCH?
  if line.special = 11 then
    sound := Ord(sfx_swtchx);

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
var
  linefunc: linefunc_t;
  oldcompatibility: boolean;
begin
  // Err...
  // Use the back sides of VERY SPECIAL lines...
  if side <> 0 then
  begin
    case line.special of
      124:
        // Sliding door open&close
        // UNUSED?
        ;
      else
      begin
        result := false;
        exit;
      end;
    end;
  end;

  oldcompatibility := true;
  // generalized types not recognized if demo older than VERSION116
  if G_PlayingEngineVersion > VERSION115 then
  begin
    oldcompatibility := false;

    // pointer to line function is nil by default, set non-null if
    // line special is push or switch generalized linedef type
    linefunc := nil;

    // check each range of generalized linedefs
    if word(line.special) >= CGENFLOORBASE then
    begin
      if (thing.player = nil) and not bossaction then
        if (line.special and gen_FloorChange <> 0) or (line.special and gen_FloorModel = 0) then
        begin
          result := false; // FloorModel is 'Allow Monsters' if FloorChange is 0
          exit;
        end;
      if (line.tag = 0) and (line.special and 6 <> 6) then //jff 2/27/98 all non-manual
      begin                                                 // generalized types require tag
        result := false;
        exit;
      end;
      linefunc := @EV_DoGenFloor;
    end
    else if word(line.special) >= CGENCEILINGBASE then
    begin
      if (thing.player = nil) and not bossaction then
        if (line.special and CeilingChange <> 0) or (line.special and CeilingModel = 0) then
        begin
          result := false;   // CeilingModel is 'Allow Monsters' if CeilingChange is 0
          exit;
        end;
      if (line.tag = 0) and (line.special and 6 <> 6) then  //jff 2/27/98 all non-manual
      begin                                                 // generalized types require tag
        result := false;
        exit;
      end;
      linefunc := @EV_DoGenCeiling;
    end
    else if word(line.special) >= CGENDOORBASE then
    begin
      if (thing.player = nil) and not bossaction then
      begin
        if line.special and DoorMonster = 0 then
        begin
          result := false;   // monsters disallowed from this door
          exit;
        end;
        if line.flags and ML_SECRET <> 0 then // they can't open secret doors either
        begin
          result := false;
          exit;
        end;
      end;
      if (line.tag = 0) and (line.special and 6 <> 6) then  //jff 3/2/98 all non-manual
      begin                                                 // generalized types require tag
        result := false;
        exit;
      end;
      linefunc := @EV_DoGenDoor;
    end
    else if word(line.special) >= CGENLOCKEDBASE then
    begin
      if (thing.player = nil) or bossaction then
      begin
        result := false;   // monsters disallowed from unlocking doors
        exit;
      end;
      if not P_CanUnlockGenDoor(line, thing.player) then
      begin
        result := false;
        exit;
      end;
      if (line.tag = 0) and (line.special and 6 <> 6) then  //jff 2/27/98 all non-manual
      begin                                                 // generalized types require tag
        result := false;
        exit;
      end;
      linefunc := @EV_DoGenLockedDoor;
    end
    else if word(line.special) >= CGENLIFTBASE then
    begin
      if (thing.player = nil) and not bossaction then
        if line.special and LiftMonster = 0 then
        begin
          result := false; // monsters disallowed
          exit;
        end;
      if (line.tag = 0) and (line.special and 6 <> 6) then  //jff 2/27/98 all non-manual
      begin                                                 // generalized types require tag
        result := false;
        exit;
      end;
      linefunc := @EV_DoGenLift;
    end
    else if word(line.special) >= CGENSTAIRSBASE then
    begin
      if (thing.player = nil) and not bossaction then
        if line.special and StairMonster = 0 then
        begin
          result := false; // monsters disallowed
          exit;
        end;
      if (line.tag = 0) and (line.special and 6 <> 6) then  //jff 2/27/98 all non-manual
      begin                                                 // generalized types require tag
        result := false;
        exit;
      end;
      linefunc := @EV_DoGenStairs;
    end
    else if word(line.special) >= CGENCRUSHERBASE then
    begin
      if (thing.player = nil) and not bossaction then
        if line.special and CrusherMonster = 0 then
        begin
          result := false; // monsters disallowed
          exit;
        end;
      if (line.tag = 0) and (line.special and 6 <> 6) then  //jff 2/27/98 all non-manual
      begin                                                 // generalized types require tag
        result := false;
        exit;
      end;
      linefunc := @EV_DoGenCrusher;
    end;

    if Assigned(linefunc) then
    begin

      case (line.special and TriggerType) shr TriggerTypeShift of
        Ord(PushOnce):
          begin
            if side = 0 then
              if linefunc(line) <> 0 then
                line.special := 0;
            result := true;
          end;

        Ord(PushMany):
          begin
            if side = 0 then
              linefunc(line);
            result := true;
          end;

        Ord(SwitchOnce):
          begin
            if linefunc(line) <> 0 then
              P_ChangeSwitchTexture(line, false);
            result := true;
          end;

        Ord(SwitchMany):
          begin
            if linefunc(line) <> 0 then
              P_ChangeSwitchTexture(line, true);
            result := true;
          end;

      else
        result := false;
      end;

      exit;
    end;

  end;

  // Switches that other things can activate.
  if (thing.player = nil) and not bossaction then
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
      //jff 3/5/98 add ability to use teleporters for monsters
     195:; // switch teleporters
     174:;
     210:; // silent switch teleporters
     209:;
    else
      begin
        result := false;
        exit;
      end;
    end;
  end;

  if bossaction then
    case line.special of
		// 0-tag specials, locked switches and teleporters need to be blocked for boss actions.
      1,         // MANUAL DOOR RAISE
      32,        // MANUAL BLUE
      33,        // MANUAL RED
      34,        // MANUAL YELLOW
      117,       // Blazing door raise
      118,       // Blazing door open
      133,       // BlzOpenDoor BLUE
      135,       // BlzOpenDoor RED
      137,       // BlzOpenDoor YEL

      99,        // BlzOpenDoor BLUE
      134,       // BlzOpenDoor RED
      136,       // BlzOpenDoor YELLOW

		//jff 3/5/98 add ability to use teleporters for monsters
      195,       // switch teleporters
      174,
      210,       // silent switch teleporters
      209:
        begin
          result := false;
          exit;
        end;
    end;

  if not P_CheckTag(line) then  //jff 2/27/98 disallow zero tag on some types
  begin
    result := false;
    exit;
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
    34, // Yellow locked door open

   117, // Blazing door raise
   118: // Blazing door open
      EV_VerticalDoor(line, thing);

  //UNUSED - Door Slide Open&Close
  // case 124:
  // EV_SlidingDoor (line, thing);
  // break;

  // SWITCHES
     7:
      begin
        // Build Stairs
        if EV_BuildStairs(line, build8) <> 0 then
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
        if EV_DoCeiling(line, crushAndRaise) <> 0 then
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

   111:
      begin
        // Blazing Door Raise (faster than TURBO!)
        if EV_DoDoor(line, blazeRaise) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

   112:
      begin
        // Blazing Door Open (faster than TURBO!)
        if EV_DoDoor(line, blazeOpen) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

   113:
      begin
        // Blazing Door Close (faster than TURBO!)
        if EV_DoDoor(line, blazeClose) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

   122:
      begin
        // Blazing PlatDownWaitUpStay
        if EV_DoPlat(line, blazeDWUS, 0) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

   127:
      begin
        // Build Stairs Turbo 16
        if EV_BuildStairs(line, turbo16) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

   131:
      begin
        // Raise Floor Turbo
        if EV_DoFloor(line, raiseFloorTurbo) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

   133, // BlzOpenDoor BLUE
   135, // BlzOpenDoor RED
   137: // BlzOpenDoor YELLOW
      begin
        if EV_DoLockedDoor(line, blazeOpen, thing) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

   140:
      begin
        // Raise Floor 512
        if EV_DoFloor(line, raiseFloor512) <> 0 then
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

   114:
      begin
        // Blazing Door Raise (faster than TURBO!)
        if EV_DoDoor(line, blazeRaise) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

   115:
      begin
        // Blazing Door Open (faster than TURBO!)
        if EV_DoDoor(line, blazeOpen) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

   116:
      begin
        // Blazing Door Close (faster than TURBO!)
        if EV_DoDoor(line, blazeClose) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

   123:
      begin
        // Blazing PlatDownWaitUpStay
        if EV_DoPlat(line, blazeDWUS, 0) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

   132:
      begin
        // Raise Floor Turbo
        if EV_DoFloor(line, raiseFloorTurbo) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    99, // BlzOpenDoor BLUE
   134, // BlzOpenDoor RED
   136: // BlzOpenDoor YELLOW
      begin
        if EV_DoLockedDoor(line, blazeOpen, thing) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

   138:
      begin
        // Light Turn On
        EV_LightTurnOn(line, 255);
        P_ChangeSwitchTexture(line, true);
      end;

   139:
      begin
        // Light Turn Off
        EV_LightTurnOn(line, 35);
        P_ChangeSwitchTexture(line, true);
      end;
  else
    begin
      if not oldcompatibility then
        case line.special of
          //jff 1/29/98 added linedef types to fill all functions out so that
          // all possess SR, S1, WR, W1 types

          158:
            begin
              // Raise Floor to shortest lower texture
              // 158 S1  EV_DoFloor(raiseToTexture), CSW(0)
              if EV_DoFloor(line, raiseToTexture) <> 0 then
                P_ChangeSwitchTexture(line, false);
            end;

          159:
            begin
              // Raise Floor to shortest lower texture
              // 159 S1  EV_DoFloor(lowerAndChange)
              if EV_DoFloor(line, lowerAndChange) <> 0 then
                P_ChangeSwitchTexture(line, false);
            end;

          160:
            begin
              // Raise Floor 24 and change
              // 160 S1  EV_DoFloor(raiseFloor24AndChange)
              if EV_DoFloor(line, raiseFloor24AndChange) <> 0 then
                P_ChangeSwitchTexture(line, false);
            end;

          161:
            begin
              // Raise Floor 24
              // 161 S1  EV_DoFloor(raiseFloor24)
              if EV_DoFloor(line, raiseFloor24) <> 0 then
                P_ChangeSwitchTexture(line, false);
            end;

          162:
            begin
              // Moving floor min n to max n
              // 162 S1  EV_DoPlat(perpetualRaise,0)
              if EV_DoPlat(line, perpetualRaise, 0) <> 0 then
                P_ChangeSwitchTexture(line, false);
            end;

          163:
            begin
              // Stop Moving floor
              // 163 S1  EV_DoPlat(perpetualRaise,0)
              EV_StopPlat(line);
              P_ChangeSwitchTexture(line, false);
            end;

          164:
            begin
              // Start fast crusher
              // 164 S1  EV_DoCeiling(fastCrushAndRaise)
              if EV_DoCeiling(line, fastCrushAndRaise) <> 0 then
                P_ChangeSwitchTexture(line, false);
            end;

          165:
            begin
              // Start slow silent crusher
              // 165 S1  EV_DoCeiling(silentCrushAndRaise)
              if EV_DoCeiling(line, silentCrushAndRaise) <> 0 then
                P_ChangeSwitchTexture(line, false);
            end;

          166:
            begin
              // Raise ceiling, Lower floor
              // 166 S1 EV_DoCeiling(raiseToHighest), EV_DoFloor(lowerFloortoLowest)
              if (EV_DoCeiling(line, raiseToHighest) <> 0) or
                 (EV_DoFloor(line, lowerFloorToLowest) <> 0) then
                P_ChangeSwitchTexture(line, false);
            end;

          167:
            begin
              // Lower floor and Crush
              // 167 S1 EV_DoCeiling(lowerAndCrush)
              if EV_DoCeiling(line, lowerAndCrush) <> 0 then
                P_ChangeSwitchTexture(line, false);
            end;

          168:
            begin
              // Stop crusher
              // 168 S1 EV_CeilingCrushStop
              if EV_CeilingCrushStop(line) <> 0 then
                P_ChangeSwitchTexture(line, false);
            end;

          169:
            begin
              // Lights to brightest neighbor sector
              // 169 S1  EV_LightTurnOn(0)
              EV_LightTurnOn(line, 0);
              P_ChangeSwitchTexture(line, false);
            end;

          170:
            begin
              // Lights to near dark
              // 170 S1  EV_LightTurnOn(35)
              EV_LightTurnOn(line, 35);
              P_ChangeSwitchTexture(line, false);
            end;

          171:
            begin
              // Lights on full
              // 171 S1  EV_LightTurnOn(255)
              EV_LightTurnOn(line, 255);
              P_ChangeSwitchTexture(line, false);
            end;

          172:
            begin
              // Start Lights Strobing
              // 172 S1  EV_StartLightStrobing
              EV_StartLightStrobing(line);
              P_ChangeSwitchTexture(line, false);
            end;

          173:
            begin
              // Lights to Dimmest Near
              // 173 S1  EV_TurnTagLightsOff
              EV_TurnTagLightsOff(line);
              P_ChangeSwitchTexture(line, false);
            end;

          174:
            begin
              // Teleport
              // 174 S1  EV_Teleport(side,thing)
              if EV_Teleport(line, side, thing) <> 0 then
                P_ChangeSwitchTexture(line, false);
            end;

          175:
            begin
              // Close Door, Open in 30 secs
              // 175 S1  EV_DoDoor(close30ThenOpen)
              if EV_DoDoor(line, close30ThenOpen) <> 0 then
                P_ChangeSwitchTexture(line, false);
            end;

          189:
            begin
              //jff 3/15/98 create texture change no motion type
              // Texture Change Only (Trigger)
              // 189 S1 Change Texture/Type Only
              if EV_DoChange(line, trigChangeOnly) <> 0 then
                P_ChangeSwitchTexture(line, false);
            end;

          203:
            begin
              // Lower ceiling to lowest surrounding ceiling
              // 203 S1 EV_DoCeiling(lowerToLowest)
              if EV_DoCeiling(line, lowerToLowest) <> 0 then
                P_ChangeSwitchTexture(line, false);
            end;

          204:
            begin
              // Lower ceiling to highest surrounding floor
              // 204 S1 EV_DoCeiling(lowerToMaxFloor)
              if EV_DoCeiling(line, lowerToMaxFloor) <> 0 then
                P_ChangeSwitchTexture(line, false);
            end;

          209:
            begin
              // killough 1/31/98: silent teleporter
              //jff 209 S1 SilentTeleport
              if EV_SilentTeleport(line, side, thing) <> 0 then
                P_ChangeSwitchTexture(line, false);
            end;

          241:
            begin
              //jff 3/15/98 create texture change no motion type
              // Texture Change Only (Numeric)
              // 241 S1 Change Texture/Type Only
              if EV_DoChange(line, numChangeOnly) <> 0 then
                P_ChangeSwitchTexture(line, false);
            end;

          221:
            begin
              // Lower floor to next lowest floor
              // 221 S1 Lower Floor To Nearest Floor
              if EV_DoFloor(line, lowerFloorToNearest) <> 0 then
                P_ChangeSwitchTexture(line, false);
            end;

          229:
            begin
              // Raise elevator next floor
              // 229 S1 Raise Elevator next floor
              if EV_DoElevator(line, elevateUp) <> 0 then
                P_ChangeSwitchTexture(line, false);
            end;

          233:
            begin
              // Lower elevator next floor
              // 233 S1 Lower Elevator next floor
              if EV_DoElevator(line, elevateDown) <> 0 then
                P_ChangeSwitchTexture(line, false);
            end;

          237:
            begin
              // Elevator to current floor
              // 237 S1 Elevator to current floor
              if EV_DoElevator(line, elevateCurrent) <> 0 then
                P_ChangeSwitchTexture(line, false);
            end;

          // jff 1/29/98 end of added S1 linedef types

          //jff 1/29/98 added linedef types to fill all functions out so that
          // all possess SR, S1, WR, W1 types

          78:
            begin
              //jff 3/15/98 create texture change no motion type
              // Texture Change Only (Numeric)
              // 78 SR Change Texture/Type Only
              if EV_DoChange(line, numChangeOnly) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          176:
            begin
              // Raise Floor to shortest lower texture
              // 176 SR  EV_DoFloor(raiseToTexture), CSW(1)
              if EV_DoFloor(line, raiseToTexture) <> 0 then
                P_ChangeSwitchTexture(line, false);
            end;

          177:
            begin
              // Raise Floor to shortest lower texture
              // 177 SR  EV_DoFloor(lowerAndChange)
              if EV_DoFloor(line, lowerAndChange) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          178:
            begin
              // Raise Floor 512
              // 178 SR  EV_DoFloor(raiseFloor512)
              if EV_DoFloor(line, raiseFloor512) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          179:
            begin
              // Raise Floor 24 and change
              // 179 SR  EV_DoFloor(raiseFloor24AndChange)
              if EV_DoFloor(line, raiseFloor24AndChange) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          180:
            begin
              // Raise Floor 24
              // 180 SR  EV_DoFloor(raiseFloor24)
              if EV_DoFloor(line, raiseFloor24) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          181:
            begin
              // Moving floor min n to max n
              // 181 SR  EV_DoPlat(perpetualRaise,0)

              EV_DoPlat(line, perpetualRaise, 0);
              P_ChangeSwitchTexture(line, true);
            end;

          182:
            begin
              // Stop Moving floor
              // 182 SR  EV_DoPlat(perpetualRaise,0)
              EV_StopPlat(line);
              P_ChangeSwitchTexture(line, true);
            end;

          183:
            begin
              // Start fast crusher
              // 183 SR  EV_DoCeiling(fastCrushAndRaise)
              if EV_DoCeiling(line, fastCrushAndRaise) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          184:
            begin
              // Start slow crusher
              // 184 SR  EV_DoCeiling(crushAndRaise)
              if EV_DoCeiling(line, crushAndRaise) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          185:
            begin
              // Start slow silent crusher
              // 185 SR  EV_DoCeiling(silentCrushAndRaise)
              if EV_DoCeiling(line, silentCrushAndRaise) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          186:
            begin
              // Raise ceiling, Lower floor
              // 186 SR EV_DoCeiling(raiseToHighest), EV_DoFloor(lowerFloortoLowest)
              if (EV_DoCeiling(line, raiseToHighest) <> 0) or
                 (EV_DoFloor(line, lowerFloorToLowest) <> 0) then
                P_ChangeSwitchTexture(line, true);
            end;

          187:
            begin
              // Lower floor and Crush
              // 187 SR EV_DoCeiling(lowerAndCrush)
              if EV_DoCeiling(line, lowerAndCrush) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          188:
            begin
              // Stop crusher
              // 188 SR EV_CeilingCrushStop
              if EV_CeilingCrushStop(line) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          190:
            begin
              //jff 3/15/98 create texture change no motion type
              // Texture Change Only (Trigger)
              // 190 SR Change Texture/Type Only
              if EV_DoChange(line, trigChangeOnly) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          191:
            begin
              // Lower Pillar, Raise Donut
              // 191 SR  EV_DoDonut
              if EV_DoDonut(line) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          192:
            begin
              // Lights to brightest neighbor sector
              // 192 SR  EV_LightTurnOn(0)
              EV_LightTurnOn(line, 0);
              P_ChangeSwitchTexture(line, true);
            end;

          193:
            begin
              // Start Lights Strobing
              // 193 SR  EV_StartLightStrobing
              EV_StartLightStrobing(line);
              P_ChangeSwitchTexture(line, true);
            end;

          194:
            begin
              // Lights to Dimmest Near
              // 194 SR  EV_TurnTagLightsOff
              EV_TurnTagLightsOff(line);
              P_ChangeSwitchTexture(line, true);
            end;

          195:
            begin
              // Teleport
              // 195 SR  EV_Teleport(side,thing)
              if EV_Teleport(line, side, thing) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          196:
            begin
              // Close Door, Open in 30 secs
              // 196 SR  EV_DoDoor(close30ThenOpen)
              if EV_DoDoor(line, close30ThenOpen) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          205:
            begin
              // Lower ceiling to lowest surrounding ceiling
              // 205 SR EV_DoCeiling(lowerToLowest)
              if EV_DoCeiling(line, lowerToLowest) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          206:
            begin
              // Lower ceiling to highest surrounding floor
              // 206 SR EV_DoCeiling(lowerToMaxFloor)
              if EV_DoCeiling(line, lowerToMaxFloor) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          210:
            begin
              // killough 1/31/98: silent teleporter
              //jff 210 SR SilentTeleport
              if EV_SilentTeleport(line, side, thing) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          211:
            begin
              //jff 3/14/98 create instant toggle floor type
              // Toggle Floor Between C and F Instantly
              // 211 SR Toggle Floor Instant
              if EV_DoPlat(line, toggleUpDn, 0) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          222:
            begin
              // Lower floor to next lowest floor
              // 222 SR Lower Floor To Nearest Floor
              if EV_DoFloor(line, lowerFloorToNearest) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          230:
            begin
              // Raise elevator next floor
              // 230 SR Raise Elevator next floor
              if EV_DoElevator(line, elevateUp) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          234:
            begin
              // Lower elevator next floor
              // 234 SR Lower Elevator next floor
              if EV_DoElevator(line, elevateDown) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          238:
            begin
              // Elevator to current floor
              // 238 SR Elevator to current floor
              if EV_DoElevator(line, elevateCurrent) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          258:
            begin
              // Build stairs, step 8
              // 258 SR EV_BuildStairs(build8)
              if EV_BuildStairs(line, build8) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          259:
            begin
              // Build stairs, step 16
              // 259 SR EV_BuildStairs(turbo16)
              if EV_BuildStairs(line, turbo16) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          // 1/29/98 jff end of added SR linedef types

        end;

    end;
  end;

  result := true;
end;

end.
