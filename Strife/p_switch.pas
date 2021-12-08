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
//  DESCRIPTION:
//   Switches, buttons. Two-state animation. Exits.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_switch;

interface

uses
  r_defs,
  p_mobj_h,
  p_spec;

procedure P_InitSwitchList;

procedure P_ChangeSwitchTexture(line: Pline_t; useAgain: boolean);

function P_UseSpecialLine(thing: Pmobj_t; line: Pline_t; side: integer): boolean;

var
  buttonlist: array[0..MAXBUTTONS - 1] of button_t;

implementation

uses
  d_delphi,
  deh_main,
  doomdata,
  d_main,
  d_player,
  m_fixed,
  m_rnd,
  m_bbox,
  tables,
  info_h,
  p_user,
  p_inter,
  p_local,
  p_dialog,
  p_setup,
  p_lights,
  p_plats,
  p_doors,
  p_ceilng,
  p_floor,
  p_genlin,
  p_telept,
  p_mobj,
  p_enemy,
  i_system,
  doomdef,
  g_game,
  s_sound,
  r_data,
  w_wad,
  z_zone,
// Data
  sounds,
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
    sound: integer; // villsa [STRIFE]
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
    sound: integer;
  end;
  Pwad_switchlist_t = ^wad_switchlist_t;
  wad_switchlist_tArray = array[0..$FFFF] of wad_switchlist_t;
  Pwad_switchlist_tArray = ^wad_switchlist_tArray;

const
  NUMSWITCHLIST = 70;

  fixedalphSwitchList: array[0..NUMSWITCHLIST - 1] of switchlist_t = (
    (name1: 'GLASS01';  name2: 'GLASS02';  episode: 1; sound: Ord(sfx_bglass)),
    (name1: 'GLASS03';  name2: 'GLASS04';  episode: 1; sound: Ord(sfx_bglass)),
    (name1: 'GLASS05';  name2: 'GLASS06';  episode: 1; sound: Ord(sfx_bglass)),
    (name1: 'GLASS07';  name2: 'GLASS08';  episode: 1; sound: Ord(sfx_bglass)),
    (name1: 'GLASS17';  name2: 'GLASS18';  episode: 1; sound: Ord(sfx_bglass)),
    (name1: 'GLASS19';  name2: 'GLASS20';  episode: 1; sound: Ord(sfx_bglass)),
    (name1: 'SWKNOB01'; name2: 'SWKNOB02'; episode: 1; sound: Ord(sfx_swknob)),
    (name1: 'SWLITE01'; name2: 'SWLITE02'; episode: 1; sound: Ord(sfx_None)),
    (name1: 'SWCHN01';  name2: 'SWCHN02';  episode: 1; sound: Ord(sfx_pulchn)),
    (name1: 'COMP01';   name2: 'COMP04B';  episode: 1; sound: Ord(sfx_bglass)),
    (name1: 'COMP05';   name2: 'COMP12B';  episode: 1; sound: Ord(sfx_bglass)),
    (name1: 'COMP09';   name2: 'COMP12B';  episode: 1; sound: Ord(sfx_bglass)),
    (name1: 'COMP12';   name2: 'COMP04B';  episode: 1; sound: Ord(sfx_bglass)),
    (name1: 'COMP13';   name2: 'COMP12B';  episode: 1; sound: Ord(sfx_bglass)),
    (name1: 'COMP17';   name2: 'COMP20B';  episode: 1; sound: Ord(sfx_bglass)),
    (name1: 'COMP21';   name2: 'COMP28B';  episode: 1; sound: Ord(sfx_bglass)),
    (name1: 'WALTEK09'; name2: 'WALTEKB1'; episode: 1; sound: Ord(sfx_None)),
    (name1: 'WALTEK10'; name2: 'WALTEKB1'; episode: 1; sound: Ord(sfx_None)),
    (name1: 'WALTEK15'; name2: 'WALTEKB1'; episode: 1; sound: Ord(sfx_None)),
    (name1: 'SWFORC01'; name2: 'SWFORC02'; episode: 1; sound: Ord(sfx_None)),
    (name1: 'SWEXIT01'; name2: 'SWEXIT02'; episode: 1; sound: Ord(sfx_None)),
    (name1: 'DORSBK01'; name2: 'DORSBK02'; episode: 1; sound: Ord(sfx_swston)),
    (name1: 'SWSLD01';  name2: 'SWSLD02';  episode: 1; sound: Ord(sfx_None)),
    (name1: 'DORWS04';  name2: 'DORWS05';  episode: 1; sound: Ord(sfx_swbolt)),
    (name1: 'SWIRON01'; name2: 'SWIRON02'; episode: 1; sound: Ord(sfx_None)),
    (name1: 'GLASS09';  name2: 'GLASS10';  episode: 2; sound: Ord(sfx_bglass)),
    (name1: 'GLASS11';  name2: 'GLASS12';  episode: 2; sound: Ord(sfx_bglass)),
    (name1: 'GLASS13';  name2: 'GLASS14';  episode: 2; sound: Ord(sfx_bglass)),
    (name1: 'GLASS15';  name2: 'GLASS16';  episode: 2; sound: Ord(sfx_bglass)),
    (name1: 'SWFORC03'; name2: 'SWFORC04'; episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWCIT01';  name2: 'SWCIT02';  episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWTRMG01'; name2: 'SWTRMG04'; episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWMETL01'; name2: 'SWMETL02'; episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWWOOD01'; name2: 'SWWOOD02'; episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWTKBL01'; name2: 'SWTKBL02'; episode: 2; sound: Ord(sfx_None)),
    (name1: 'AZWAL21';  name2: 'AZWAL22';  episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWINDT01'; name2: 'SWINDT02'; episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWRUST01'; name2: 'SWRUST02'; episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWCHAP01'; name2: 'SWCHAP02'; episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWALIN01'; name2: 'SWALIN02'; episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWWALG01'; name2: 'SWWALG02'; episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWWALG03'; name2: 'SWWALG04'; episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWTRAM01'; name2: 'SWTRAM02'; episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWTRAM03'; name2: 'SWTRAM04'; episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWORC01';  name2: 'SWORC02';  episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWBRIK01'; name2: 'SWBRIK02'; episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWIRON03'; name2: 'SWIRON04'; episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWIRON05'; name2: 'SWIRON06'; episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWIRON07'; name2: 'SWIRON08'; episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWCARD01'; name2: 'SWCARD02'; episode: 2; sound: Ord(sfx_keycrd)),
    (name1: 'SWSIGN01'; name2: 'SWSIGN02'; episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWLEV01';  name2: 'SWLEV02';  episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWLEV03';  name2: 'SWLEV04';  episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWLEV05';  name2: 'SWLEV06';  episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWBRN01';  name2: 'SWBRN02';  episode: 2; sound: Ord(sfx_keycrd)),
    (name1: 'SWPIP01';  name2: 'SWPIP02';  episode: 2; sound: Ord(sfx_valve)),
    (name1: 'SWPALM01'; name2: 'SWPALM02'; episode: 2; sound: Ord(sfx_swscan)),
    (name1: 'SWKNOB03'; name2: 'SWKNOB04'; episode: 2; sound: Ord(sfx_swknob)),
    (name1: 'ALTSW01';  name2: 'ALTSW02';  episode: 2; sound: Ord(sfx_None)),
    (name1: 'COMP25';   name2: 'COMP28B';  episode: 2; sound: Ord(sfx_bglass)),
    (name1: 'COMP29';   name2: 'COMP20B';  episode: 2; sound: Ord(sfx_bglass)),
    (name1: 'COMP33';   name2: 'COMP50';   episode: 2; sound: Ord(sfx_bglass)),
    (name1: 'COMP42';   name2: 'COMP51';   episode: 2; sound: Ord(sfx_bglass)),
    (name1: 'GODSCRN1'; name2: 'GODSCRN2'; episode: 2; sound: Ord(sfx_difool)),
    (name1: 'ALIEN04';  name2: 'ALIEN05';  episode: 2; sound: Ord(sfx_None)),
    (name1: 'CITADL04'; name2: 'CITADL05'; episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWITE03';  name2: 'SWITE04';  episode: 2; sound: Ord(sfx_None)),
    (name1: 'SWTELP01'; name2: 'SWTELP02'; episode: 2; sound: Ord(sfx_None)),
    (name1: 'BRNSCN01'; name2: 'BRNSCN05'; episode: 2; sound: Ord(sfx_firxpl)),
    (name1: '';         name2: '';         episode:-1; sound: Ord(sfx_None))
  );

var
  switchlist: PIntegerArray;
  numswitches: integer;

//
// P_InitSwitchList
// Only called at game initialization.
//
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

  if isregistered then
    episode := 2
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
      alphSwitchList[i].sound := wad_switchlist[i].sound;
    end;
    Z_Free(wad_switchlist);
    alphSwitchList[len].name1 := '';
    alphSwitchList[len].name2 := '';
    alphSwitchList[len].episode := -1;
    alphSwitchList[len].sound := Ord(sfx_None);
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

//
// Start a button counting down till it turns off.
//
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

//
// P_SpawnBrokenGlass
// villsa [STRIFE] new function
//
procedure P_SpawnBrokenGlass(line: Pline_t);
var
  x1, x2: fixed_t;
  y1, y2: fixed_t;
  i: integer;
  glass: Pmobj_t;
  an: angle_t;
begin
  x1 := (line.v2.x + line.v1.x) div 2;
  y1 := (line.v2.y + line.v1.y) div 2;
  x2 := ((line.frontsector.soundorg.x - x1) div 5) + x1;
  y2 := ((line.frontsector.soundorg.y - y1) div 5) + y1;

  for i := 0 to 6 do
  begin
    glass := P_SpawnMobj(x2, y2, ONFLOORZ, Ord(MT_JUNK));
    glass.z := glass.z + (24 * FRACUNIT);
    glass.flags := glass.flags or (MF_SHADOW or MF_MVIS);

    P_SetMobjState(glass, statenum_t(P_Random mod 3 + Ord(S_SHRD_03))); // 284

    an := (P_Random * 8192) div 256;

    glass.angle := an * ANGLETOFINEUNIT;
    glass.momx := FixedMul(finecosine[an], (P_Random and 3) * FRACUNIT);
    glass.momy := FixedMul(finesine[an],   (P_Random and 3) * FRACUNIT);
    glass.momz := (P_Random and 7) * FRACUNIT;
    glass.tics := glass.tics + (P_Random + 7) and 7;
  end;
end;

//
// Function that changes wall texture.
// Tell it if switch is ok to use again (1=yes, it's a button).
//
procedure P_ChangeSwitchTexture(line: Pline_t; useAgain: boolean);
var
  texTop: integer;
  texMid: integer;
  texBot: integer;
  i: integer;
  sound: integer;
  sdnum: integer;
  breakglass: boolean; // villsa [STRIFE]
  sl: Pswitchlist_t;   // villsa [STRIFE]
  sn: integer;
begin
  sdnum := line.sidenum[0];
  texTop := sides[sdnum].toptexture;
  texMid := sides[sdnum].midtexture;
  texBot := sides[sdnum].bottomtexture;

  sound := Ord(sfx_swtchn);

  // villsa [STRIFE] check for linetype 182 (break glass)
  if line.special = 182 then
  begin
    line.flags := line.flags and not ML_BLOCKING;
    breakglass := true;

    if useAgain then
    begin
      // haleyjd 09/21/10: Corrected (>> 16 == next field)
      texTop := 0;
      texBot := 0;
    end;

    if texMid <> 0 then // haleyjd 09/21/10: Corrected (>> 16 == next field)
      useAgain := false;

    sound := Ord(sfx_bglass);
  end
  else
    breakglass := false;

  if not useAgain then
    line.special := 0;

  for i := 0 to numswitches * 2 - 1 do
  begin
    sl := @alphSwitchList[i div 2]; // villsa [STRIFE]
    if switchlist[i] = texTop then
    begin
      // villsa [STRIFE] set sound
      if sl.sound <> 0 then
        sound := sl.sound;
      // haleyjd 20141026: [STRIFE]: Rogue fixed wrong sound origin
      S_StartSound(@line.frontsector.soundorg, sound);
      sides[line.sidenum[0]].toptexture := switchlist[i xor 1];

      if useAgain then
        P_StartButton(line, top, switchlist[i], BUTTONTIME);

      if breakglass then
        P_SpawnBrokenGlass(line);

      exit;
    end
    else
    begin
      if switchlist[i] = texMid then
      begin
        // villsa [STRIFE] set sound
        if sl.sound <> 0 then
          sound := sl.sound;
        // haleyjd 20141026: [STRIFE]: Rogue fixed wrong sound origin
        S_StartSound(@line.frontsector.soundorg, sound);
        sides[line.sidenum[0]].midtexture := switchlist[i xor 1];

        // villsa [STRIFE] affect second side of line
        // BUG: will crash if 1S line is marked with TWOSIDED flag!
        if line.flags and ML_TWOSIDED <> 0 then
        begin
          sn := line.sidenum[1];
          if sn >= 0 then // JVAL: extra check
            sides[sn].midtexture := switchlist[i xor 1];
        end;

        if useAgain then
          P_StartButton(line, middle, switchlist[i], BUTTONTIME);

        // villsa [STRIFE]: Mines Transmitter hack
        if sound = Ord(sfx_firxpl) then
        begin
          breakglass := true;

          // give quest flag 29 to player
          players[0].questflags := players[0].questflags or QF_QUEST29;

          // give stamina/accuracy items
          if not netgame then
          begin
            P_GiveItemToPlayer(@players[0], Ord(SPR_TOKN), MT_TOKEN_STAMINA);
            P_GiveItemToPlayer(@players[0], Ord(SPR_TOKN), MT_TOKEN_NEW_ACCURACY);
          end;

        end;

        // villsa [STRIFE]
        if breakglass or (sound = Ord(sfx_bglass)) then
          P_SpawnBrokenGlass(line);

        exit;
      end
      else
      begin
        if switchlist[i] = texBot then
        begin
          // villsa [STRIFE] set sound
          if sl.sound <> 0 then
            sound := sl.sound;
          // haleyjd 20141026: [STRIFE]: Rogue fixed wrong sound origin
          S_StartSound(@line.frontsector.soundorg, sound);
          sides[line.sidenum[0]].bottomtexture := switchlist[i xor 1];

          if useAgain then
            P_StartButton(line, bottom, switchlist[i], BUTTONTIME);

          if breakglass then
            P_SpawnBrokenGlass(line);

          exit;
        end;
      end;
    end;
  end;
end;

//
// P_MoveWall
//
// villsa [STRIFE] New function.
// Dynamically move a solid line. Unused in Strife
//
procedure P_MoveWall(line: Pline_t; thing: Pmobj_t);
var
  v1, v2: Pvertex_t;
  x, y: fixed_t;
begin
  v1 := line.v1;
  v2 := line.v2;
  S_StartSound(thing, Ord(sfx_stnmov));

  if line.dx <> 0 then
  begin
    if thing.x >= v1.x then
    begin
      v1.y := v1.y - (8 * FRACUNIT);
      v2.y := v2.y - (8 * FRACUNIT);
    end
    else
    begin
      v1.y := v1.y + (8 * FRACUNIT);
      v2.y := v2.y + (8 * FRACUNIT);
    end;
  end
  else
  begin
    if thing.y >= v1.y then
    begin
      v1.x := v1.x - (8 * FRACUNIT);
      v2.x := v2.x - (8 * FRACUNIT);
    end
    else
    begin
      v1.x := v1.x + (8 * FRACUNIT);
      v2.x := v2.x + (8 * FRACUNIT);
    end;
  end;

  if v1.x >= v2.x then
  begin
    line.bbox[BOXLEFT] := v2.x;
    x := v1.x;
  end
  else
  begin
    line.bbox[BOXLEFT] := v1.x;
    x := v2.x;
  end;

  line.bbox[BOXRIGHT] := x;

  if v1.y >= v2.y then
  begin
    line.bbox[BOXBOTTOM] := v2.y;
    y := v1.y;
  end
  else
  begin
    line.bbox[BOXBOTTOM] := v1.y;
    y := v2.y;
  end;

  line.bbox[BOXTOP] := y;
end;

//
// P_UseSpecialLine
// Called when a thing uses a special line.
// Only the front sides of lines are usable.
//
function P_UseSpecialLine(thing: Pmobj_t; line: Pline_t; side: integer): boolean;
var
  linefunc: linefunc_t;
  oldcompatibility: boolean;
begin
  // Err...
  // Use the back sides of VERY SPECIAL lines...
  if side <> 0 then
  begin
    case line.special of
      148:
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
  if not G_NeedsCompatibilityMode then
  begin
    oldcompatibility := false;

    // pointer to line function is nil by default, set non-null if
    // line special is push or switch generalized linedef type
    linefunc := nil;

    // check each range of generalized linedefs
    if word(line.special) >= CGENFLOORBASE then
    begin
      if thing.player = nil then
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
      if thing.player = nil then
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
      if thing.player = nil then
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
      if thing.player = nil then
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
      if thing.player = nil then
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
      if thing.player = nil then
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
      if thing.player = nil then
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
      31:; // haleyjd [STRIFE]
     144:; // haleyjd [STRIFE] Manual sliding door
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

  // do something
  case line.special of
  // MANUALS
    1,             // Vertical Door
    26,            // DR ID Card
    27,            // DR Pass Card
    28,            // DR ID Badge
    31,            // Manual door open
    32,            // D1 ID Card
    33,            // D1 ID Badge
    34,            // D1 Pass Card
    117,           // Blazing door raise
    118,           // Blazing door open
    156,           // haleyjd [STRIFE] D1 Brass Key
    157,           // haleyjd [STRIFE] D1 Silver Key
    158,           // haleyjd [STRIFE] D1 Gold Key
    159,           // haleyjd [STRIFE] DR Gold Key
    160,           // haleyjd [STRIFE] DR Silver Key
    161,           // haleyjd [STRIFE] DR Brass Key
    165,           // villsa  [STRIFE] That doesn't seem to work
    166,           // haleyjd [STRIFE] DR Hand Print
    169,           // haleyjd [STRIFE] DR Base Key
    170,           // haleyjd [STRIFE] DR Gov's Key
    190,           // haleyjd [STRIFE] DR Order Key
    205,           // villsa  [STRIFE] Available in retail only
    213,           // haleyjd [STRIFE] DR Chalice
    217,           // haleyjd [STRIFE] DR Core Key
    221,           // haleyjd [STRIFE] DR Mauler Key
    224,           // haleyjd [STRIFE] DR Chapel Key
    225,           // haleyjd [STRIFE] DR Catacomb Key
    232:           // villsa  [STRIFE] DR Oracle Pass
      begin
        EV_VerticalDoor(line, thing);
      end;

    // haleyjd: For the sake of our sanity, I have reordered all the line
    // specials from this point down so that they are strictly in numeric
    // order, and not divided up in a semi-arbitrary fashion.

    7:
      begin
        // Build Stairs - [STRIFE] Verified unmodified
        if EV_BuildStairs(line, build8) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    9:
      begin
        // Change Donut - [STRIFE] Verified unmodified
        if EV_DoDonut(line) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    11:
      begin
        // Exit level - [STRIFE] Modified to take tag, etc.
        P_ChangeSwitchTexture(line, true);
        if levelTimer and (levelTimeCount <> 0) then
        else
          G_ExitLevel(line.tag);
      end;

    14:
      begin
        // Raise Floor 32 and change texture - [STRIFE] Verified unmodified
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
        // Raise Floor to next highest floor - [STRIFE] Verified unmodified
        if EV_DoFloor(line, raiseFloorToNearest) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    20:
      begin
        // Raise Plat next highest floor and change texture - [STRIFE] Verified unmodified
        if EV_DoPlat(line, raiseToNearestAndChange, 0) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    21:
      begin
        // PlatDownWaitUpStay - [STRIFE] Verified unmodified
        if EV_DoPlat(line, downWaitUpStay, 0) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    23:
      begin
        // Lower Floor to Lowest - [STRIFE] Verified unmodified
        if EV_DoFloor(line, lowerFloorToLowest) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    29:
      begin
        // Raise Door - [STRIFE] Verified unmodified
        if EV_DoDoor(line, vld_normal) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    40:
      begin
        // villsa [STRIFE] Split Open Door
        if EV_DoDoor(line, vld_splitOpen) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end; // haleyjd

    41:
      begin
        // Lower Ceiling to Floor - [STRIFE] Verified unmodified
        if EV_DoCeiling(line, lowerToFloor) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    42:
      begin
        // Close Door - [STRIFE] Verified unmodified
        if EV_DoDoor(line, vld_close) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    43:
      begin
        // Lower Ceiling to Floor - [STRIFE] Verified unmodified
        if EV_DoCeiling(line, lowerToFloor) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    45:
      begin
        // Lower Floor to Surrounding floor height - [STRIFE] Verified unmodified
        if EV_DoFloor(line, lowerFloor) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    49:
      begin
        // Ceiling Crush And Raise - [STRIFE] Verified unmodified
        if EV_DoCeiling(line, crushAndRaise) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    50:
      begin
        // Close Door - [STRIFE] Verified unmodified
        if EV_DoDoor(line, vld_close) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    51:
      begin
        // [STRIFE] Modifed into S1 Start Finale (was Secret Exit)
        P_ChangeSwitchTexture(line, false);
        G_StartFinale();
      end;

    55:
      begin
        // Raise Floor Crush - [STRIFE] Verified unmodified
        if EV_DoFloor(line, raiseFloorCrush) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    60:
      begin
        // Lower Floor to Lowest - [STRIFE] Verified unmodified
        if EV_DoFloor(line, lowerFloorToLowest) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    61:
      begin
        // Open Door - [STRIFE] Verified unmodified
        if EV_DoDoor(line, vld_open) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    62:
      begin
        // PlatDownWaitUpStay - [STRIFE] Verified unmodified
        if EV_DoPlat(line, downWaitUpStay, 1) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    63:
      begin
        // Raise Door - [STRIFE] Verified unmodified
        if EV_DoDoor(line, vld_normal) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    64:
      begin
        // Raise Floor to ceiling - [STRIFE] Verified unmodified
        if EV_DoFloor(line, raiseFloor) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    65:
      begin
        // Raise Floor Crush - [STRIFE] Verified unmodified
        if EV_DoFloor(line, raiseFloorCrush) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    66:
      begin
        // Raise Floor 24 and change texture - [STRIFE] Verified unmodified
        if EV_DoPlat(line, raiseAndChange, 24) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    67:
      begin
        // Raise Floor 32 and change texture - [STRIFE] Verified unmodified
        if EV_DoPlat(line, raiseAndChange, 32) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    68:
      begin
        // Raise Plat to next highest floor and change texture - [STRIFE] Verified unmodified
        if EV_DoPlat(line, raiseToNearestAndChange, 0) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    69:
      begin
        // Raise Floor to next highest floor - [STRIFE] Verified unmodified
        if EV_DoFloor(line, raiseFloorToNearest) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    70:
      begin
        // Turbo Lower Floor - [STRIFE] Verified unmodified
        if EV_DoFloor(line, turboLower) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    71:
      begin
        // Turbo Lower Floor - [STRIFE] Verified unmodified
        if EV_DoFloor(line, turboLower) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    101:
      begin
        // Raise Floor - [STRIFE] Verified unmodified
        if EV_DoFloor(line, raiseFloor) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    102:
      begin
        // Lower Floor to Surrounding floor height - [STRIFE] Verified unmodified
        if EV_DoFloor(line, lowerFloor) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    103:
      begin
        // Open Door - [STRIFE] Verified unmodified
        if EV_DoDoor(line, vld_open) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    111:
      begin
        // Blazing Door Raise (faster than TURBO!) - [STRIFE] Verified unmodified
        if EV_DoDoor(line, vld_blazeRaise) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    112:
      begin
        // Blazing Door Open (faster than TURBO!) - [STRIFE] Verified unmodified
        if EV_DoDoor(line, vld_blazeOpen) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    113:
      begin
        // Blazing Door Close (faster than TURBO!) - [STRIFE] Verified unmodified
        if EV_DoDoor (line, vld_blazeClose) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    114:
      begin
        // Blazing Door Raise (faster than TURBO!) - [STRIFE] Verified unmodified
        if EV_DoDoor(line, vld_blazeRaise) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    115:
      begin
        // Blazing Door Open (faster than TURBO!) - [STRIFE] Verified unmodified
        if EV_DoDoor(line, vld_blazeOpen) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    116:
      begin
        // Blazing Door Close (faster than TURBO!) - [STRIFE] Verified unmodified
        if EV_DoDoor(line, vld_blazeClose) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    122:
      begin
        // Blazing PlatDownWaitUpStay - [STRIFE] Verified unmodified
        if EV_DoPlat(line, blazeDWUS, 0) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    123:
      begin
        // Blazing PlatDownWaitUpStay - [STRIFE] Verified unmodified
        if EV_DoPlat(line, blazeDWUS, 0) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    127:
      begin
        // Build Stairs Turbo 16 - [STRIFE] Verified unmodified
        if EV_BuildStairs(line, turbo16) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    131:
      begin
        // Raise Floor Turbo - [STRIFE] Verified unmodified
        if EV_DoFloor(line, raiseFloorTurbo) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    132:
      begin
        // Raise Floor Turbo - [STRIFE] Verified unmodified
        if EV_DoFloor(line, raiseFloorTurbo) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    133, // [STRIFE] TODO - which key is it?
    135, // [STRIFE] TODO - which key is it?
    137: // [STRIFE] TODO - which key is it?
      begin
        if EV_DoLockedDoor(line, vld_blazeOpen, thing) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    99,  // [STRIFE] TODO: which key is it?
    134, // [STRIFE] TODO: which key is it?
    136: // [STRIFE] TODO: which key is it?
      begin
        if EV_DoLockedDoor(line, vld_blazeOpen, thing) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    138:
      begin
        // Light Turn On - [STRIFE] Verified unmodified
        EV_LightTurnOn(line, 255);
        P_ChangeSwitchTexture(line, true);
      end;

    139:
      begin
        // Light Turn Off - [STRIFE] Verified unmodified
        EV_LightTurnOn(line, 35);
        P_ChangeSwitchTexture(line, true);
      end;

    140:
      begin
        // Raise Floor 512 - [STRIFE] Verified unmodified
        if EV_DoFloor(line, raiseFloor512) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    144:
      begin
        // villsa [STRIFE] manual sliding door
        EV_SlidingDoor(line, thing);
      end;

    146:
      begin
        // haleyjd 09/24/10: [STRIFE] S1 Build Stairs Down 16 (new type)
        if EV_BuildStairs(line, buildDown16) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    147:
      begin
        // haleyjd 09/24/10: [STRIFE] S1 Clear Force Fields
        if EV_ClearForceFields(line) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    148:
      begin
        // haleyjd 09/16/10: [STRIFE] using forcefields hurts
        P_DamageMobj(thing, nil, nil, 16);
        P_Thrust(thing.player, thing.angle + ANG180, 125 * FRACUNIT div 16);
      end;

    151, // villsa [STRIFE] BlzOpenDoor Gold key
    152, // [STRIFE] TODO: which key is it?
    153: // [STRIFE] TODO: which key is it?
      begin
        if EV_DoLockedDoor(line, vld_blazeOpen, thing) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    154:
      begin
        // villsa [STRIFE] plat lower wait rise if have gold key
        if Pplayer_t(thing.player).cards[Ord(key_GoldKey)] then
        begin
          if EV_DoPlat(line, downWaitUpStay, 0) <> 0 then
            P_ChangeSwitchTexture(line, true);
        end
        else
        begin
          Pplayer_t(thing.player)._message := DEH_GetString('You need a gold key');
          S_StartSound(thing, Ord(sfx_oof));
        end;
      end;

    155:
      begin
        // villsa [STRIFE] raise plat wait lower
        if EV_DoPlat(line, upWaitDownStay, 0) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    162, // [STRIFE] TODO: which key is it?
    163, // [STRIFE] TODO: which key is it?
    164, // villsa [STRIFE] BlzOpenDoor Gold key
    167: // [STRIFE] TODO: which key is it?
      begin
        if EV_DoLockedDoor(line, vld_blazeOpen, thing) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    168: // [STRIFE] TODO: which key is it?
      begin
        // haleyjd 09/25/10: [STRIFE] SR Blaze Open Door ???? Key
        if EV_DoLockedDoor(line, vld_blazeOpen, thing) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    171: // [STRIFE] TODO: which key is it?
      begin
        // haleyjd 09/25/10: [STRIFE] S1 Open Door ???? Key
        if EV_DoLockedDoor(line, vld_open, thing) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    172, // [STRIFE] TODO: which key is it?
    173, // [STRIFE] TODO: which key is it?
    176, // [STRIFE] TODO: which key is it?
    191, // [STRIFE] TODO: which key is it?
    192, // [STRIFE] TODO: which key is it?
    223: // [STRIFE] TODO: which key is it?
      begin
        if EV_DoLockedDoor(line, vld_normal, thing) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    177:
      begin
        // villsa [STRIFE] plat lower wait rise if have power3 key
        if Pplayer_t(thing.player).cards[Ord(key_Power3Key)] then
        begin
            if EV_DoPlat(line, downWaitUpStay, 0) <> 0 then
              P_ChangeSwitchTexture(line, false);
        end
        else
        begin
          Pplayer_t(thing.player)._message := DEH_GetString('You don''t have the key');
          S_StartSound(thing, Ord(sfx_oof));
        end;
      end;

    181:
      begin
        // haleyjd 09/25/10: [STRIFE] S1 Floor Raise 512 & Change
        if EV_DoFloor(line, raiseFloor512AndChange) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    189: // [STRIFE] TODO: which key is it???
      begin
        // haleyjd 09/25/10: [STRIFE] S1 Split Open Door ???? Key
        if EV_DoLockedDoor(line, vld_splitOpen, thing) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    194:
      begin
        // villsa [STRIFE] S1 Free Prisoners
        if EV_DoDoor(line, vld_open) <> 0 then
        begin
          P_ChangeSwitchTexture(line, false);
          P_FreePrisoners;
        end;
      end;

    199:
      begin
        // haleyjd 09/25/10: [STRIFE] S1 Destroy Converter
        if EV_DoCeiling(line, lowerAndCrush) <> 0 then
        begin
          P_ChangeSwitchTexture(line, false);
          P_DestroyConverter;
        end;
      end;

    207:
      begin
        // villsa [STRIFE] SR Remote Sliding Door
        if EV_RemoteSlidingDoor(line, thing) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end; // haleyjd

    209:
      begin
        // haleyjd 09/24/10: [STRIFE] S1 Build Stairs Down 16 if Have Chalice
        if not P_PlayerHasItem(thing.player, MT_INV_CHALICE) <> 0 then
        begin
            Pplayer_t(thing.player)._message := DEH_GetString('You need the chalice!');
            S_StartSound(thing, Ord(sfx_oof));
        end
        else if EV_BuildStairs(line, buildDown16) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    211:
      begin
        // villsa [STRIFE] S1 Play VOC## sound
        if @players[consoleplayer] = thing.player then
          if Pplayer_t(thing.player).powers[Ord(pw_communicator)] <> 0 then
          begin
            S_StartVoice('VOC' + itoa(line.tag));
            line.special := 0;
          end;
      end;

    214:
      begin
        // villsa [STRIFE] S1 slow lift lower wait up stay
        if EV_DoPlat(line, slowDWUS, 1) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    219:
      begin
        // haleyjd 09/25/10: S1 Lower Floor Blue Crystal
        if not Pplayer_t(thing.player).cards[Ord(key_BlueCrystalKey)] then
        begin
          Pplayer_t(thing.player)._message := DEH_GetString('You need the Blue Crystal');
          S_StartSound(thing, Ord(sfx_oof));
        end
        else if EV_DoFloor(line, lowerFloor) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    220:
      begin
        // haleyjd 09/25/10: S1 Lower Floor Red Crystal
        if not Pplayer_t(thing.player).cards[Ord(key_RedCrystalKey)] then
        begin
          Pplayer_t(thing.player)._message := DEH_GetString('You need the Red Crystal');
          S_StartSound(thing, Ord(sfx_oof));
        end
        else if EV_DoFloor(line, lowerFloor) <> 0 then
          P_ChangeSwitchTexture(line, false);
      end;

    226:
      begin
        // villsa [STRIFE] S1 Complete Training Area
        if EV_DoFloor(line, lowerFloor) <> 0 then
        begin
          P_GiveItemToPlayer(thing.player, Ord(SPR_TOKN), MT_TOKEN_STAMINA);
          P_GiveItemToPlayer(thing.player, Ord(SPR_TOKN), MT_TOKEN_NEW_ACCURACY);
          P_ChangeSwitchTexture(line, false);
          Pplayer_t(thing.player)._message := DEH_GetString('Congratulations! You have completed the training area.');
        end;
      end;

    229:
      begin
        // villsa [STRIFE] SR Sigil Sliding Door
        if Pplayer_t(thing.player).sigiltype = 4 then
        begin
          if EV_RemoteSlidingDoor(line, thing) <> 0 then
            P_ChangeSwitchTexture(line, true);
        end;
      end; // haleyjd

    233:
      begin
        // villsa [STRIFE] objective given after revealing the computer
        if EV_DoDoor(line, vld_splitOpen) = 0 then
        begin
          result := true;
          exit;
        end;

        P_ChangeSwitchTexture(line, true);
        P_GiveVoiceObjective('VOC70', 'LOG70', 0);

        // haleyjd: Strife used sprintf here, not a direct set.
        Pplayer_t(thing.player)._message := DEH_GetString('Incoming Message from BlackBird...');

      end;

    234:
      begin
        // haleyjd 09/24/10: [STRIFE] SR Raise Door if Quest 3
        if Pplayer_t(thing.player).questflags and QF_QUEST3 = 0 then // QUEST3 == Irale
        begin
          // BUG: doesn't make sfx_oof sound like all other message-
          // giving door types. I highly doubt this was intentional.
          Pplayer_t(thing.player)._message := DEH_GetString('That doesn''t seem to work!');
          S_StartSound(thing, Ord(sfx_oof));  // JVAL: added sound
        end
        else if EV_DoDoor(line, vld_normal) <> 0 then
          P_ChangeSwitchTexture(line, true);
      end;

    235:
      begin
        // haleyjd 09/25/10: [STRIFE] S1 Split Open Door if Have Sigil 4
        if Pplayer_t(thing.player).sigiltype = 4 then
          if EV_DoDoor(line, vld_splitOpen) <> 0 then
            P_ChangeSwitchTexture(line, false);
      end;

    666:
      begin
        // villsa [STRIFE] SR Move Wall
        P_MoveWall(line, thing);
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
              if EV_DoFloor(line, raiseFloor64) <> 0 then
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
              if EV_Teleport(line, side, thing, 0) <> 0 then
                P_ChangeSwitchTexture(line, false);
            end;

          175:
            begin
              // Close Door, Open in 30 secs
              // 175 S1  EV_DoDoor(close30ThenOpen)
              if EV_DoDoor(line, vld_close30ThenOpen) <> 0 then
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
              if EV_Teleport(line, side, thing, 0) <> 0 then
                P_ChangeSwitchTexture(line, true);
            end;

          196:
            begin
              // Close Door, Open in 30 secs
              // 196 SR  EV_DoDoor(close30ThenOpen)
              if EV_DoDoor(line, vld_close30ThenOpen) <> 0 then
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
