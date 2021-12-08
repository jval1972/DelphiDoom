//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
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
//    Intermission screens.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit wi_stuff;

interface

uses
  d_player;

const
// States for the intermission
  NoState = -1;
  StatCount = 0;
  ShowNextLoc = 1;

type
  stateenum_t = integer;

// Called by main loop, animate the intermission.
procedure WI_Ticker;

// Called by main loop,
// draws the intermission directly into the screen buffer.
procedure WI_Drawer;

// Setup for an intermission screen.
procedure WI_Start(wbstartstruct: Pwbstartstruct_t);

implementation

uses
  d_delphi,
  doomdef,
  d_event,
  z_zone,
  m_rnd,
  i_system,
  w_wad,
  g_game,
  s_sound,
  r_defs,
  doomstat,
  sounds,
  v_data,
  v_video;

const
//
// Data needed to add patches to full screen intermission pics.
// Patches are statistics messages, and animations.
// Loads of by-pixel layout and placement, offsets etc.
//


//
// Different between registered DOOM (1994) and
//  Ultimate DOOM - Final edition (retail, 1995?).
// This is supposedly ignored for commercial
//  release (aka DOOM II), which had 34 maps
//  in one episode. So there.
  NUMEPISODES = 4;
  NUMMAPS = 9;


// in tics
//U #define PAUSELEN    (TICRATE*2)
//U #define SCORESTEP    100
//U #define ANIMPERIOD    32
// pixel distance from "(YOU)" to "PLAYER N"
//U #define STARDIST    10
//U #define WK 1


// GLOBAL LOCATIONS
  WI_TITLEY = 2;
  WI_SPACINGY = 33;

// SINGPLE-PLAYER STUFF
  SP_STATSX = 50;
  SP_STATSY = 50;

  SP_TIMEX = 16;
  SP_TIMEY = 200 - 32; // (SCREENHEIGHT - 32);

// NET GAME STUFF
  NG_STATSY = 50;

  NG_SPACINGX = 64;


// DEATHMATCH STUFF
  DM_MATRIXX = 42;
  DM_MATRIXY = 68;

  DM_SPACINGX = 40;

  DM_TOTALSX = 269;

  DM_KILLERSX = 10;
  DM_KILLERSY = 100;
  DM_VICTIMSX = 5;
  DM_VICTIMSY = 50;

type
  animenum_t = (
    ANIM_ALWAYS,
    ANIM_RANDOM,
    ANIM_LEVEL
  );


type
  point_t = record
    x: integer;
    y: integer;
  end;


//
// Animation.
// There is another anim_t used in p_spec.
//
type
  wianim_t = record // JVAL: renamed from anim_t -> wianim_t
    _type: animenum_t;

    // period in tics between animations
    period: integer;

    // number of animation frames
    nanims: integer;

    // location of animation
    loc: point_t;

    // ALWAYS: n/a,
    // RANDOM: period deviation (<256),
    // LEVEL: level
    data1: integer;

    // ALWAYS: n/a,
    // RANDOM: random base period,
    // LEVEL: n/a
    data2: integer;

    // actual graphics for frames of animations
    p: array[0..2] of Ppatch_t;

    // following must be initialized to zero before use!

    // next value of bcnt (used in conjunction with period)
    nexttic: integer;

    // last drawn animation frame
    lastdrawn: integer;

    // next frame number to animate
    ctr: integer;

    // used by RANDOM and LEVEL when animating
    state: integer;
  end;
  Pwianim_t = ^wianim_t;
  wianim_tArray = packed array[0..$FFFF] of wianim_t;
  Pwianim_tArray = ^wianim_tArray;


var
  lnodes: array[0..NUMEPISODES - 1, 0..NUMMAPS - 1] of point_t = (
  // Episode 0 World Map
    (
    (x: 185; y: 164),  // location of level 0 (CJ)
    (x: 148; y: 143),  // location of level 1 (CJ)
    (x:  69; y: 122),  // location of level 2 (CJ)
    (x: 209; y: 102),  // location of level 3 (CJ)
    (x: 116; y:  89),  // location of level 4 (CJ)
    (x: 166; y:  55),  // location of level 5 (CJ)
    (x:  71; y:  56),  // location of level 6 (CJ)
    (x: 135; y:  29),  // location of level 7 (CJ)
    (x:  71; y:  24)   // location of level 8 (CJ)
    ),

  // Episode 1 World Map should go here
    (
    (x: 254; y:  25),  // location of level 0 (CJ)
    (x:  97; y:  50),  // location of level 1 (CJ)
    (x: 188; y:  64),  // location of level 2 (CJ)
    (x: 128; y:  78),  // location of level 3 (CJ)
    (x: 214; y:  92),  // location of level 4 (CJ)
    (x: 133; y: 130),  // location of level 5 (CJ)
    (x: 208; y: 136),  // location of level 6 (CJ)
    (x: 148; y: 140),  // location of level 7 (CJ)
    (x: 235; y: 158)   // location of level 8 (CJ)
    ),

  // Episode 2 World Map should go here
    (
    (x: 156; y: 168),  // location of level 0 (CJ)
    (x:  48; y: 154),  // location of level 1 (CJ)
    (x: 174; y:  95),  // location of level 2 (CJ)
    (x: 265; y:  75),  // location of level 3 (CJ)
    (x: 130; y:  48),  // location of level 4 (CJ)
    (x: 279; y:  23),  // location of level 5 (CJ)
    (x: 198; y:  48),  // location of level 6 (CJ)
    (x: 140; y:  25),  // location of level 7 (CJ)
    (x: 281; y: 136)   // location of level 8 (CJ)
    ),
  ////////////////////////////////////////////////
    (
    (x: 0; y: 0),
    (x: 0; y: 0),
    (x: 0; y: 0),
    (x: 0; y: 0),
    (x: 0; y: 0),
    (x: 0; y: 0),
    (x: 0; y: 0),
    (x: 0; y: 0),
    (x: 0; y: 0)
    )
  );

//
// Animation locations for episode 0 (1).
// Using patches saves a lot of space,
//  as they replace 320x200 full screen frames.
//
  epsd0animinfo: array[0..9] of wianim_t = (
    (_type: ANIM_ALWAYS; period: TICRATE div 3; nanims: 3; loc: (x: 224; y: 104)),
    (_type: ANIM_ALWAYS; period: TICRATE div 3; nanims: 3; loc: (x: 184; y: 160)),
    (_type: ANIM_ALWAYS; period: TICRATE div 3; nanims: 3; loc: (x: 112; y: 136)),
    (_type: ANIM_ALWAYS; period: TICRATE div 3; nanims: 3; loc: (x:  72; y: 112)),
    (_type: ANIM_ALWAYS; period: TICRATE div 3; nanims: 3; loc: (x:  88; y:  96)),
    (_type: ANIM_ALWAYS; period: TICRATE div 3; nanims: 3; loc: (x:  64; y:  48)),
    (_type: ANIM_ALWAYS; period: TICRATE div 3; nanims: 3; loc: (x: 192; y:  40)),
    (_type: ANIM_ALWAYS; period: TICRATE div 3; nanims: 3; loc: (x: 136; y:  16)),
    (_type: ANIM_ALWAYS; period: TICRATE div 3; nanims: 3; loc: (x:  80; y:  16)),
    (_type: ANIM_ALWAYS; period: TICRATE div 3; nanims: 3; loc: (x:  64; y:  24))
  );

  epsd1animinfo: array[0..8] of wianim_t = (
    (_type: ANIM_LEVEL; period: TICRATE div 3; nanims: 1; loc: (x: 128; y: 136); data1: 1),
    (_type: ANIM_LEVEL; period: TICRATE div 3; nanims: 1; loc: (x: 128; y: 136); data1: 2),
    (_type: ANIM_LEVEL; period: TICRATE div 3; nanims: 1; loc: (x: 128; y: 136); data1: 3),
    (_type: ANIM_LEVEL; period: TICRATE div 3; nanims: 1; loc: (x: 128; y: 136); data1: 4),
    (_type: ANIM_LEVEL; period: TICRATE div 3; nanims: 1; loc: (x: 128; y: 136); data1: 5),
    (_type: ANIM_LEVEL; period: TICRATE div 3; nanims: 1; loc: (x: 128; y: 136); data1: 6),
    (_type: ANIM_LEVEL; period: TICRATE div 3; nanims: 1; loc: (x: 128; y: 136); data1: 7),
    (_type: ANIM_LEVEL; period: TICRATE div 3; nanims: 3; loc: (x: 192; y: 144); data1: 8),
    (_type: ANIM_LEVEL; period: TICRATE div 3; nanims: 1; loc: (x: 128; y: 136); data1: 8)
  );

  epsd2animinfo: array[0..5] of wianim_t = (
    (_type: ANIM_ALWAYS; period: TICRATE div 3; nanims: 3; loc: (x: 104; y: 168)),
    (_type: ANIM_ALWAYS; period: TICRATE div 3; nanims: 3; loc: (x:  40; y: 136)),
    (_type: ANIM_ALWAYS; period: TICRATE div 3; nanims: 3; loc: (x: 160; y:  96)),
    (_type: ANIM_ALWAYS; period: TICRATE div 3; nanims: 3; loc: (x: 104; y:  80)),
    (_type: ANIM_ALWAYS; period: TICRATE div 3; nanims: 3; loc: (x: 120; y:  32)),
    (_type: ANIM_ALWAYS; period: TICRATE div 4; nanims: 3; loc: (x:  40; y:   0))
  );

const
  NUMANIMS: array[0..NUMEPISODES - 1] of integer = (10, 9, 6, 0);

var
  anims: array[0..NUMEPISODES - 1] of Pwianim_tArray;


//
// GENERAL DATA
//

//
// Locally used stuff.
//
const
// in seconds
  SHOWNEXTLOCDELAY = 4;

var
// used to accelerate or skip a stage
  acceleratestage: integer = 0;

// wbs->pnum
  me: integer = 0;

// specifies current state
  state: stateenum_t;

// contains information passed into intermission
  wbs: Pwbstartstruct_t = nil;

  plrs: Pwbplayerstruct_tArray = nil;  // wbs->plyr[]

// used for general timing
  cnt: integer = 0;

// used for timing of background animation
  bcnt: integer;

// signals to refresh everything for one frame
  firstrefresh: integer = 0;

  cnt_kills: array[0..MAXPLAYERS - 1] of integer;
  cnt_items: array[0..MAXPLAYERS - 1] of integer;
  cnt_secret: array[0..MAXPLAYERS - 1] of integer;
  cnt_time: integer = 0;
  cnt_par: integer = 0;
  cnt_pause: integer = 0;

// # of commercial levels
  NUMCMAPS: integer = 0;


//
//  GRAPHICS
//

// You Are Here graphic
  yah: array[0..1] of Ppatch_t;

// splat
  splat: Ppatch_t = nil;

// %, : graphics
  percent: Ppatch_t;
  colon: Ppatch_t;

// 0-9 graphic
  num: array[0..9] of Ppatch_t;

// minus sign
  wiminus: Ppatch_t = nil;

// "Finished!" graphics
  finished: Ppatch_t = nil;

// "Entering" graphic
  entering: Ppatch_t = nil;

// "secret"
  sp_secret: Ppatch_t = nil;

 // "Kills", "Scrt", "Items", "Frags"
  kills: Ppatch_t = nil;
  secret: Ppatch_t = nil;
  items: Ppatch_t = nil;
  frags: Ppatch_t = nil;

// Time sucks.
  time: Ppatch_t = nil;
  par: Ppatch_t = nil;
  sucks: Ppatch_t = nil;

// "killers", "victims"
  killers: Ppatch_t = nil;
  victims: Ppatch_t = nil;

// "Total", your face, your dead face
  total: Ppatch_t = nil;
  star: Ppatch_t = nil;
  bstar: Ppatch_t = nil;

// "red P[1..MAXPLAYERS]"
  p: array[0..MAXPLAYERS - 1] of Ppatch_t;

// "gray P[1..MAXPLAYERS]"
  bp: array[0..MAXPLAYERS - 1] of Ppatch_t;

 // Name graphics of each level (centered)
  lnames: Ppatch_tPArray;
  lnamessize: integer = 0;

  wibackground: string;
//
// CODE
//

// slam background
// UNUSED static unsigned char *background=0;


procedure WI_slamBackground;
begin
  V_DrawPatchFullScreenTMP320x200(wibackground);
end;

// The ticker is used to detect keys
//  because of timing issues in netgames.
function WI_Responder(ev: Pevent_t): boolean;
begin
  result := false;
end;

// Draws "<Levelname> Finished!"
procedure WI_DrawLF;
var
  y: integer;
begin
  y := WI_TITLEY;

  // draw <LevelName>
  if wbs.last < lnamessize then // JVAL: 20170826 Avoid crash when missing levelname patches
  begin
    V_DrawPatch((320 - lnames[wbs.last].width) div 2, y, SCN_TMP, lnames[wbs.last], false);
    y := y + (5 * lnames[wbs.last].height) div 4;
    if y + finished.height > 200 then
      y := 200 - finished.height;

    V_DrawPatch((320 - finished.width) div 2, y, SCN_TMP, finished, false);
  end;
end;

// Draws "Entering <LevelName>"
procedure WI_DrawEL;
var
  y: integer;
begin
  y := WI_TITLEY;

  // draw "Entering"
  V_DrawPatch((320 - entering.width) div 2, y, SCN_TMP, entering, false);

  // draw level
  if wbs.next < lnamessize then // JVAL: 20170826 Avoid crash when missing levelname patches
  begin
    y := y + (5 * lnames[wbs.next].height) div 4;
    if y + lnames[wbs.next].height > 200 then
      y := 200 - lnames[wbs.next].height;

    V_DrawPatch((320 - lnames[wbs.next].width) div 2, y, SCN_TMP, lnames[wbs.next], false);
  end;
end;

procedure WI_DrawOnLnode(n: integer; c: Ppatch_tPArray);
var
  i: integer;
  left: integer;
  top: integer;
  right: integer;
  bottom: integer;
  fits: boolean;
begin
  fits := false;

  i := 0;
  repeat
    left := lnodes[wbs.epsd][n].x - c[i].leftoffset;
    top := lnodes[wbs.epsd][n].y - c[i].topoffset;
    right := left + c[i].width;
    bottom := top + c[i].height;

    if (left >= 0) and
       (right < 320) and
       (top >= 0) and
       (bottom < 200) then
      fits := true
    else
      inc(i);
  until not ((not fits) and (i <> 2));

  if fits and (i < 2) then
    V_DrawPatch(lnodes[wbs.epsd][n].x, lnodes[wbs.epsd][n].y, SCN_TMP, c[i], false)
  else
    // DEBUG
    I_Warning('WI_DrawOnLnode(): Could not place patch on level %d'#13#10, [n + 1]);
end;

procedure WI_InitAnimatedBack;
var
  i: integer;
  a: Pwianim_t;
begin
  if gamemode = commercial then
    exit;

  if wbs.epsd > 2 then
    exit;

  for i := 0 to NUMANIMS[wbs.epsd] - 1 do
  begin
    a := @anims[wbs.epsd, i];

    // init variables
    a.ctr := -1;

    // specify the next time to draw it
    if a._type = ANIM_ALWAYS then
      a.nexttic := bcnt + 1 + (M_Random mod a.period)
    else if a._type = ANIM_RANDOM then
      a.nexttic := bcnt + 1 + a.data2 + (M_Random mod a.data1)
    else if a._type = ANIM_LEVEL then
      a.nexttic := bcnt + 1;
  end;
end;

procedure WI_UpdateAnimatedBack;
var
  i: integer;
  a: Pwianim_t;
begin
  if gamemode = commercial then
    exit;

  if wbs.epsd > 2 then
    exit;

  for i := 0 to NUMANIMS[wbs.epsd] - 1 do
  begin
    a := @anims[wbs.epsd, i];

    if bcnt = a.nexttic then
    begin
      case a._type of
        ANIM_ALWAYS:
          begin
            a.ctr := a.ctr + 1;
            if a.ctr >= a.nanims then
              a.ctr := 0;
            a.nexttic := bcnt + a.period;
          end;
        ANIM_RANDOM:
          begin
            a.ctr := a.ctr + 1;
            if a.ctr = a.nanims then
            begin
              a.ctr := -1;
              a.nexttic := bcnt + a.data2 + (M_Random mod a.data1);
            end
            else
              a.nexttic := bcnt + a.period;
          end;
        ANIM_LEVEL:
          begin
            // gawd-awful hack for level anims
            if (not ((state = StatCount) and (i = 7))) and
               (wbs.next = a.data1) then
            begin
              a.ctr := a.ctr + 1;
              if a.ctr = a.nanims then
                a.ctr := a.ctr - 1;
              a.nexttic := bcnt + a.period;
            end;
          end;
      end;
    end;
  end;
end;

procedure WI_DrawAnimatedBack;
var
  i: integer;
  a: Pwianim_t;
begin
  if gamemode = commercial then
    exit;

  if wbs.epsd > 2 then
    exit;

  for i := 0 to NUMANIMS[wbs.epsd] - 1 do
  begin
    a := @anims[wbs.epsd, i];

    if a.ctr >= 0 then
      V_DrawPatch(a.loc.x, a.loc.y, SCN_TMP, a.p[a.ctr], false);
  end;
end;

//
// Draws a number.
// If digits > 0, then use that many digits minimum,
//  otherwise only use as many as necessary.
// Returns new x position.
//

function WI_DrawNum(x, y: integer; n: integer; digits: integer): integer;
var
  fontwidth: integer;
  neg: boolean;
  temp: integer;
begin
  fontwidth := num[0].width;
  if digits < 0 then
  begin
    if n = 0 then
      // make variable-length zeros 1 digit long
      digits := 1
    else
    begin
      // figure out # of digits in #
      digits := 0;
      temp := n;

      while temp <> 0 do
      begin
        temp := temp div 10;
        inc(digits);
      end;

    end;
  end;

  neg := n < 0;
  if neg then
    n := -n;

  // if non-number, do not draw it
  if n = 1994 then
  begin
    result := 0;
    exit;
  end;

  // draw the new number
  while digits > 0 do
  begin
    x := x - fontwidth;
    V_DrawPatch(x, y, SCN_TMP, num[n mod 10], false);
    n := n div 10;
    dec(digits);
  end;

  // draw a minus sign if necessary
  if neg then
  begin
    x := x - 8;
    V_DrawPatch(x, y, SCN_TMP, wiminus, false);
  end;

  result := x;
end;

procedure WI_DrawPercent(x, y: integer; p: integer);
begin
  if p < 0 then
    exit;

  V_DrawPatch(x, y, SCN_TMP, percent, false);
  WI_DrawNum(x, y, p, -1);
end;

//
// Display level completion time and par,
//  or "sucks" message if overflow.
//
procedure WI_DrawTime(x, y: integer; t: integer);
var
  _div: integer;
  n: integer;
begin
  if t < 0 then
    exit;

  if t <= 61 * 59 then
  begin
    _div := 1;

    repeat
      n := (t div _div) mod 60;
      x := WI_DrawNum(x, y, n, 2) - colon.width;
      _div := _div * 60;

      // draw
      if (_div = 60) or (t div _div <> 0) then
        V_DrawPatch(x, y, SCN_TMP, colon, false);
    until t div _div = 0;
  end
  else
    // "sucks"
    V_DrawPatch(x - sucks.width, y, SCN_TMP, sucks, false);
end;

procedure WI_UnloadData; forward;

procedure WI_End;
begin
  WI_UnloadData;
end;

procedure WI_InitNoState;
begin
  state := NoState;
  acceleratestage := 0;
  cnt := 10;
end;

procedure WI_UpdateNoState;
begin
  WI_UpdateAnimatedBack;

  dec(cnt);
  if cnt = 0 then
  begin
    WI_End;
    G_WorldDone;
  end;
end;

var
  snl_pointeron: boolean = false;

procedure WI_InitShowNextLoc;
begin
  state := ShowNextLoc;
  acceleratestage := 0;
  cnt := SHOWNEXTLOCDELAY * TICRATE;

  WI_InitAnimatedBack;
end;

procedure WI_UpdateShowNextLoc;
begin
  WI_UpdateAnimatedBack;

  dec(cnt);
  if (cnt = 0) or (acceleratestage <> 0) then
    WI_InitNoState
  else
    snl_pointeron := (cnt and 31) < 20;
end;


procedure WI_DrawShowNextLoc;
var
  i: integer;
  last: integer;
begin
  if gamemode <> commercial then
  begin
    if wbs.epsd > 2 then
    begin
      WI_DrawEL;
      exit;
    end;

    if wbs.last = 8 then
      last := wbs.next - 1
    else
      last := wbs.last;

    // draw a splat on taken cities.
    for i := 0 to last do
      WI_DrawOnLnode(i, @splat);

    // splat the secret level?
    if wbs.didsecret then
      WI_DrawOnLnode(8, @splat);

    // draw flashing ptr
    if snl_pointeron then
      WI_DrawOnLnode(wbs.next, @yah);
  end;

  // draws which level you are entering..
  if (gamemode <> commercial) or
     (wbs.next <> 30) then
    WI_DrawEL;
end;

procedure WI_DrawNoState;
begin
  snl_pointeron := true;
  WI_DrawShowNextLoc;
end;

function WI_FragSum(playernum: integer): integer;
var
  i: integer;
begin
  result := 0;

  for i := 0 to MAXPLAYERS - 1 do
  begin
    if playeringame[i] and (i <> playernum) then
      result := result + plrs[playernum].frags[i];
  end;

  // JDC hack - negative frags.
  result := result - plrs[playernum].frags[playernum];
    // UNUSED if (result < 0)
    //   result = 0;
end;

var
  dm_state: integer;
  dm_frags: array[0..MAXPLAYERS - 1, 0..MAXPLAYERS - 1] of integer;
  dm_totals: array[0..MAXPLAYERS - 1] of integer;

procedure WI_InitDeathmatchStats;
var
  i: integer;
  j: integer;
begin
  state := StatCount;
  acceleratestage := 0;
  dm_state := 1;

  cnt_pause := TICRATE;

  for i := 0 to MAXPLAYERS - 1 do
  begin
    if playeringame[i] then
    begin
      for j := 0 to MAXPLAYERS - 1 do
        if playeringame[j] then
          dm_frags[i, j] := 0;

      dm_totals[i] := 0;
    end;
  end;

  WI_InitAnimatedBack;
end;

procedure WI_UpdateDeathmatchStats;
var
  i: integer;
  j: integer;
  stillticking: boolean;
begin
  WI_UpdateAnimatedBack;

  if (acceleratestage <> 0) and (dm_state <> 4) then
  begin
    acceleratestage := 0;

    for i := 0 to MAXPLAYERS - 1 do
    begin
      if playeringame[i] then
      begin
        for j := 0 to MAXPLAYERS - 1 do
          if playeringame[j] then
            dm_frags[i, j] := plrs[i].frags[j];

        dm_totals[i] := WI_FragSum(i);
      end;
    end;

    S_StartSound(nil, Ord(sfx_barexp));
    dm_state := 4;
  end;

  if dm_state = 2 then
  begin
    if bcnt and 3 = 0 then
      S_StartSound(nil, Ord(sfx_pistol));

    stillticking := false;

    for i := 0 to MAXPLAYERS - 1 do
    begin
      if playeringame[i] then
      begin
        for j := 0 to MAXPLAYERS - 1 do
        begin
          if playeringame[j] and (dm_frags[i, j] <> plrs[i].frags[j]) then
          begin
            if plrs[i].frags[j] < 0 then
              dm_frags[i, j] := dm_frags[i, j] - 1
            else
              dm_frags[i, j] := dm_frags[i, j] + 1;

            if dm_frags[i, j] > 99 then
              dm_frags[i, j] := 99
            else if dm_frags[i, j] < -99 then
              dm_frags[i, j] := -99;
            stillticking := true;
          end;
        end;
        dm_totals[i] := WI_FragSum(i);

        if dm_totals[i] > 99 then
          dm_totals[i] := 99
        else if dm_totals[i] < -99 then
          dm_totals[i] := -99;
      end;
    end;
    if not stillticking then
    begin
      S_StartSound(nil, Ord(sfx_barexp));
      inc(dm_state);
    end;
  end
  else if dm_state = 4 then
  begin
    if acceleratestage <> 0 then
    begin
      S_StartSound(nil, Ord(sfx_slop));

      if gamemode = commercial then
        WI_InitNoState
      else
        WI_InitShowNextLoc;
    end;
  end
  else if dm_state and 1 <> 0 then
  begin
    dec(cnt_pause);
    if cnt_pause = 0 then
    begin
      inc(dm_state);
      cnt_pause := TICRATE;
    end;
  end;
end;

procedure WI_DrawDeathmatchStats;
var
  i: integer;
  j: integer;
  x: integer;
  y: integer;
  w: integer;
begin

  WI_DrawLF;

  // draw stat titles (top line)
  V_DrawPatch(DM_TOTALSX - total.width div 2, DM_MATRIXY - WI_SPACINGY + 10, SCN_TMP, total, false);

  V_DrawPatch(DM_KILLERSX, DM_KILLERSY, SCN_TMP, killers, false);
  V_DrawPatch(DM_VICTIMSX, DM_VICTIMSY, SCN_TMP, victims, false);

  // draw P?
  x := DM_MATRIXX + DM_SPACINGX;
  y := DM_MATRIXY;

  for i := 0 to MAXPLAYERS - 1 do
  begin
    if playeringame[i] then
    begin
      V_DrawPatch(x - p[i].width div 2, DM_MATRIXY - WI_SPACINGY, SCN_TMP, p[i], false);
      V_DrawPatch(DM_MATRIXX - p[i].width div 2, y, SCN_TMP, p[i], false);

      if i = me then
      begin
        V_DrawPatch(x - p[i].width div 2, DM_MATRIXY - WI_SPACINGY, SCN_TMP, bstar, false);
        V_DrawPatch(DM_MATRIXX - p[i].width div 2, y, SCN_TMP, star, false);
      end;
    end
    else
    begin
      // V_DrawPatch(x-SHORT(bp[i]->width)/2,
      //   DM_MATRIXY - WI_SPACINGY, FB, bp[i]);
      // V_DrawPatch(DM_MATRIXX-SHORT(bp[i]->width)/2,
      //   y, FB, bp[i]);
    end;
    x := x + DM_SPACINGX;
    y := y + WI_SPACINGY;
  end;

  // draw stats
  y := DM_MATRIXY + 10;
  w := num[0].width;

  for i := 0 to MAXPLAYERS - 1 do
  begin
    x := DM_MATRIXX + DM_SPACINGX;

    if playeringame[i] then
    begin
      for j := 0 to MAXPLAYERS - 1 do
      begin
        if playeringame[j] then
          WI_DrawNum(x + w, y, dm_frags[i, j], 2);
        x := x + DM_SPACINGX;
      end;
      WI_DrawNum(DM_TOTALSX + w, y, dm_totals[i], 2);
    end;
    y := y + WI_SPACINGY;
  end;
end;

var
  cnt_frags: array[0..MAXPLAYERS - 1] of integer;
  dofrags: integer = 0;
  ng_state: integer = 0;

function NG_STATSX: integer;
begin
  result := 64 + star.width div 2 + 32 * (not intval(dofrags <> 0)); // JVAL ???
end;

procedure WI_InitNetgameStats;
var
  i: integer;
begin
  state := StatCount;
  acceleratestage := 0;
  ng_state := 1;

  cnt_pause := TICRATE;

  for i := 0 to MAXPLAYERS - 1 do
  begin
    if not playeringame[i] then
      continue;

    cnt_kills[i] := 0;
    cnt_items[i] := 0;
    cnt_secret[i] := 0;
    cnt_frags[i] := 0;

    dofrags := dofrags + WI_FragSum(i);
  end;

  dofrags := intval(dofrags <> 0);

  WI_InitAnimatedBack;
end;

procedure WI_UpdateNetgameStats;
var
  i: integer;
  fsum: integer;
  stillticking: boolean;
begin
  WI_UpdateAnimatedBack;

  if (acceleratestage <> 0) and (ng_state <> 10) then
  begin
    acceleratestage := 0;

    for i := 0 to MAXPLAYERS - 1 do
    begin
      if not playeringame[i] then
        continue;

      cnt_kills[i] := (plrs[i].skills * 100) div wbs.maxkills;
      cnt_items[i] := (plrs[i].sitems * 100) div wbs.maxitems;
      cnt_secret[i] := (plrs[i].ssecret * 100) div wbs.maxsecret;

      if dofrags <> 0 then
        cnt_frags[i] := WI_FragSum(i);
    end;
    S_StartSound(nil, Ord(sfx_barexp));
    ng_state := 10;
  end;

  if ng_state = 2 then
  begin
    if bcnt and 3 = 0 then
      S_StartSound(nil, Ord(sfx_pistol));

    stillticking := false;

    for i := 0 to MAXPLAYERS - 1 do
    begin
      if not playeringame[i] then
        continue;

      cnt_kills[i] := cnt_kills[i] + 2;

      if cnt_kills[i] >= (plrs[i].skills * 100) div wbs.maxkills then
        cnt_kills[i] := (plrs[i].skills * 100) div wbs.maxkills
      else
        stillticking := true;
    end;

    if not stillticking then
    begin
      S_StartSound(nil, Ord(sfx_barexp));
      inc(ng_state);
    end;
  end
  else if ng_state = 4 then
  begin
    if bcnt and 3 = 0 then
      S_StartSound(nil, Ord(sfx_pistol));

    stillticking := false;

    for i := 0 to MAXPLAYERS - 1 do
    begin
      if not playeringame[i] then
        continue;

      cnt_items[i] := cnt_items[i] + 2;
      if cnt_items[i] >= (plrs[i].sitems * 100) div wbs.maxitems then
        cnt_items[i] := (plrs[i].sitems * 100) div wbs.maxitems
      else
        stillticking := true;
    end;
    if not stillticking then
    begin
      S_StartSound(nil, Ord(sfx_barexp));
      inc(ng_state);
    end;
  end
  else if ng_state = 6 then
  begin
    if bcnt and 3 = 0 then
      S_StartSound(nil, Ord(sfx_pistol));

    stillticking := false;

    for i := 0 to MAXPLAYERS - 1 do
    begin
      if not playeringame[i] then
        continue;

      cnt_secret[i] := cnt_secret[i] + 2;

      if cnt_secret[i] >= (plrs[i].ssecret * 100) div wbs.maxsecret then
        cnt_secret[i] := (plrs[i].ssecret * 100) div wbs.maxsecret
      else
        stillticking := true;
    end;
    if not stillticking then
    begin
      S_StartSound(nil, Ord(sfx_barexp));
      ng_state := ng_state + 1 + 2 * intval(dofrags <> 0);
    end;
  end
  else if ng_state = 8 then
  begin
    if bcnt and 3 = 0 then
      S_StartSound(nil, Ord(sfx_pistol));

    stillticking := false;

    for i := 0 to MAXPLAYERS - 1 do
    begin
      if not playeringame[i] then
        continue;

      cnt_frags[i] := cnt_frags[i] + 1;

      fsum := WI_FragSum(i);
      if cnt_frags[i] >= fsum then
        cnt_frags[i] := fsum
      else
        stillticking := true;
    end;
    if not stillticking then
    begin
      S_StartSound(nil, Ord(sfx_pldeth));
      inc(ng_state);
    end;
  end
  else if ng_state = 10 then
  begin
    if acceleratestage <> 0 then
    begin
      S_StartSound(nil, Ord(sfx_sgcock));
      if gamemode = commercial then
        WI_InitNoState
      else
        WI_InitShowNextLoc;
    end;
  end
  else if ng_state and 1 <> 0 then
  begin
    dec(cnt_pause);
    if cnt_pause = 0 then
    begin
      inc(ng_state);
      cnt_pause := TICRATE;
    end;
  end;
end;

procedure WI_DrawNetgameStats;
var
  i: integer;
  x: integer;
  y: integer;
  pwidth: integer;
begin
  pwidth := percent.width;

  WI_DrawLF;

  // draw stat titles (top line)
  V_DrawPatch(NG_STATSX + NG_SPACINGX - kills.width, NG_STATSY, SCN_TMP, kills, false);

  V_DrawPatch(NG_STATSX + 2 * NG_SPACINGX - items.width, NG_STATSY, SCN_TMP, items, false);

  V_DrawPatch(NG_STATSX + 3 * NG_SPACINGX - secret.width, NG_STATSY, SCN_TMP, secret, false);

  if dofrags <> 0 then
    V_DrawPatch(NG_STATSX + 4 * NG_SPACINGX - frags.width, NG_STATSY, SCN_TMP, frags, false);

  // draw stats
  y := NG_STATSY + kills.height;

  for i := 0 to MAXPLAYERS - 1 do
  begin
    if not playeringame[i] then
      continue;

    x := NG_STATSX;
    V_DrawPatch(x - p[i].width, y, SCN_TMP, p[i], false);

    if i = me then
      V_DrawPatch(x - p[i].width, y, SCN_TMP, star, false);

    x := x + NG_SPACINGX;
    WI_DrawPercent(x - pwidth, y + 10, cnt_kills[i]);
    x := x + NG_SPACINGX;
    WI_DrawPercent(x - pwidth, y + 10, cnt_items[i]);
    x := x + NG_SPACINGX;
    WI_DrawPercent(x - pwidth, y + 10, cnt_secret[i]);
    x := x + NG_SPACINGX;

    if dofrags <> 0 then
      WI_DrawNum(x, y + 10, cnt_frags[i], -1);

    y := y + WI_SPACINGY;
  end;
end;

var
  sp_state: integer = 0;

procedure WI_InitStats;
begin
  state := StatCount;
  acceleratestage := 0;
  sp_state := 1;
  cnt_kills[0] := -1;
  cnt_items[0] := -1;
  cnt_secret[0] := -1;
  cnt_time := -1;
  cnt_par := -1;
  cnt_pause := TICRATE;

  WI_InitAnimatedBack;
end;

procedure WI_UpdateStats;
begin
  WI_UpdateAnimatedBack;

  if (acceleratestage <> 0) and (sp_state <> 10) then
  begin
    acceleratestage := 0;
    cnt_kills[0] := (plrs[me].skills * 100) div wbs.maxkills;
    cnt_items[0] := (plrs[me].sitems * 100) div wbs.maxitems;
    cnt_secret[0] := (plrs[me].ssecret * 100) div wbs.maxsecret;
    cnt_time := plrs[me].stime div TICRATE;
    cnt_par := wbs.partime div TICRATE;
    S_StartSound(nil, Ord(sfx_barexp));
    sp_state := 10;
  end;

  if sp_state = 2 then
  begin
    cnt_kills[0] := cnt_kills[0] + 2;

    if bcnt and 3 = 0 then
      S_StartSound(nil, Ord(sfx_pistol));

    if cnt_kills[0] >= (plrs[me].skills * 100) div wbs.maxkills then
    begin
      cnt_kills[0] := (plrs[me].skills * 100) div wbs.maxkills;
      S_StartSound(nil, Ord(sfx_barexp));
      inc(sp_state);
    end;
  end
  else if sp_state = 4 then
  begin
    cnt_items[0] := cnt_items[0] + 2;

    if bcnt and 3 <> 0 then
      S_StartSound(nil, OrD(sfx_pistol));

    if cnt_items[0] >= (plrs[me].sitems * 100) div wbs.maxitems then
    begin
      cnt_items[0] := (plrs[me].sitems * 100) div wbs.maxitems;
      S_StartSound(nil, Ord(sfx_barexp));
      inc(sp_state);
    end;
  end
  else if sp_state = 6 then
  begin
    cnt_secret[0] := cnt_secret[0] + 2;

    if bcnt and 3 = 0 then
      S_StartSound(nil, Ord(sfx_pistol));

    if cnt_secret[0] >= (plrs[me].ssecret * 100) div wbs.maxsecret then
    begin
      cnt_secret[0] := (plrs[me].ssecret * 100) div wbs.maxsecret;
      S_StartSound(nil, Ord(sfx_barexp));
      inc(sp_state);
    end;
  end
  else if sp_state = 8 then
  begin
    if bcnt and 3 = 0 then
      S_StartSound(nil, Ord(sfx_pistol));

    cnt_time := cnt_time + 3;

    if cnt_time >= plrs[me].stime div TICRATE then
      cnt_time := plrs[me].stime div TICRATE;

    cnt_par := cnt_par + 3;

    if cnt_par >= wbs.partime div TICRATE then
    begin
      cnt_par := wbs.partime div TICRATE;

      if cnt_time >= plrs[me].stime div TICRATE then
      begin
        S_StartSound(nil, Ord(sfx_barexp));
        inc(sp_state);
      end;
    end;
  end
  else if sp_state = 10 then
  begin
    if acceleratestage <> 0 then
    begin
      S_StartSound(nil, Ord(sfx_sgcock));

      if gamemode = commercial then
        WI_InitNoState
      else
        WI_InitShowNextLoc;
    end;
  end
  else if sp_state and 1 <> 0 then
  begin
    dec(cnt_pause);
    if cnt_pause = 0 then
    begin
      inc(sp_state);
      cnt_pause := TICRATE;
    end;
  end;
end;

procedure WI_DrawStats;
var
  // line height
  lh: integer;
begin
  lh := (3 * num[0].height) div 2;

  WI_DrawLF;

  V_DrawPatch(SP_STATSX, SP_STATSY, SCN_TMP, kills, false);
  WI_DrawPercent(320 - SP_STATSX, SP_STATSY, cnt_kills[0]);

  V_DrawPatch(SP_STATSX, SP_STATSY + lh, SCN_TMP, items, false);
  WI_DrawPercent(320 - SP_STATSX, SP_STATSY + lh, cnt_items[0]);

  V_DrawPatch(SP_STATSX, SP_STATSY + 2 * lh, SCN_TMP, sp_secret, false);
  WI_DrawPercent(320 - SP_STATSX, SP_STATSY + 2 * lh, cnt_secret[0]);

  V_DrawPatch(SP_TIMEX, SP_TIMEY, SCN_TMP, time, false);
  WI_DrawTime(160 - SP_TIMEX, SP_TIMEY, cnt_time);

  if wbs.epsd < 3 then
  begin
    V_DrawPatch(160 + SP_TIMEX, SP_TIMEY, SCN_TMP, par, false);
    WI_DrawTime(320 - SP_TIMEX, SP_TIMEY, cnt_par);
  end;
end;

procedure WI_checkForAccelerate;
var
  i: integer;
  player: Pplayer_t;
begin
  // check for button presses to skip delays
  for i := 0 to MAXPLAYERS - 1 do
  begin
    player := @players[i];

    if playeringame[i] then
    begin

      if player.cmd.buttons and BT_ATTACK <> 0 then
      begin
        if not player.attackdown then
          acceleratestage := 1;
        player.attackdown := true;
      end
      else
        player.attackdown := false;

      if player.cmd.buttons and BT_USE <> 0 then
      begin
        if not player.usedown then
          acceleratestage := 1;
        player.usedown := true;
      end
      else
        player.usedown := false;
    end;
  end;
end;

// Updates stuff each tick
procedure WI_Ticker;
begin
  // counter for general background animation
  inc(bcnt);

  if bcnt = 1 then
  begin
    // intermission music
    if gamemode = commercial then
      S_ChangeMusic(Ord(mus_dm2int), true)
    else
      S_ChangeMusic(Ord(mus_inter), true);
  end;

  WI_checkForAccelerate;

  case state of
    StatCount:
      begin
        if deathmatch <> 0 then
          WI_UpdateDeathmatchStats
        else if netgame then
          WI_UpdateNetgameStats
        else
          WI_UpdateStats;
      end;

    ShowNextLoc:
      begin
        WI_UpdateShowNextLoc;
      end;

    NoState:
      begin
        WI_UpdateNoState;
      end;
  end;
end;

var
  wi_loaded: boolean = false;

procedure WI_LoadData;
var
  i: integer;
  j: integer;
  a: Pwianim_t;
  name: string;
begin
  if wi_loaded then
    exit;

  if gamemode = commercial then
    wibackground := 'INTERPIC'
  else
    sprintf(wibackground, 'WIMAP%d', [wbs.epsd]);

  if gamemode = retail then
    if wbs.epsd = 3 then
      wibackground := 'INTERPIC';

  if gamemode = commercial then
  begin
    NUMCMAPS := 32;
    lnamessize := NUMCMAPS; // JVAL: VERSION 204
    lnames :=
      Ppatch_tPArray(
        Z_Malloc(SizeOf(Ppatch_t) * NUMCMAPS, PU_STATIC, nil));
    for i := 0 to NUMCMAPS - 1 do
    begin
      name := 'CWILV' + IntToStrZfill(2, i); // JVAL: was sprintf(name, 'CWILV%2.2d', i);
      lnames[i] := W_CacheLumpName(name, PU_STATIC);
    end;
  end
  else
  begin
    lnamessize := NUMMAPS; // JVAL: VERSION 204
    lnames :=
      Ppatch_tPArray(
        Z_Malloc(SizeOf(Ppatch_t) * NUMMAPS, PU_STATIC, nil));
    for i := 0 to NUMMAPS - 1 do
    begin
      sprintf(name, 'WILV%d%d', [wbs.epsd, i]);
      lnames[i] := W_CacheLumpName(name, PU_STATIC);
    end;

    // you are here
    yah[0] := W_CacheLumpName('WIURH0', PU_STATIC);

    // you are here (alt.)
    yah[1] := W_CacheLumpName('WIURH1', PU_STATIC);

    // splat
    splat := W_CacheLumpName('WISPLAT', PU_STATIC);

    if wbs.epsd < 3 then
    begin
      for j := 0 to NUMANIMS[wbs.epsd] - 1 do
      begin
        a := @anims[wbs.epsd, j];
        for i := 0 to a.nanims - 1 do
        begin
          // MONDO HACK!
          if (wbs.epsd <> 1) or (j <> 8) then
          begin
            // animations
            sprintf(name, 'WIA%d%.2d%.2d', [wbs.epsd, j, i]); // JVAL ????
            a.p[i] := W_CacheLumpName(name, PU_STATIC);
          end
          else
          begin
          // HACK ALERT!
            a.p[i] := anims[1, 4].p[i];
          end;
        end;
      end;
    end;
  end;

  // More hacks on minus sign.
  if oldsharewareversion or oldversion then
    wiminus := W_CacheLumpName('STCFN046', PU_STATIC)
  else
    wiminus := W_CacheLumpName('WIMINUS', PU_STATIC);

  for i := 0 to 9 do
  begin
    // numbers 0-9
    sprintf(name, 'WINUM%d', [i]);
    num[i] := W_CacheLumpName(name, PU_STATIC);
  end;

  // percent sign
  percent := W_CacheLumpName('WIPCNT', PU_STATIC);

  // "finished"
  finished := W_CacheLumpName('WIF', PU_STATIC);

  // "entering"
  entering := W_CacheLumpName('WIENTER', PU_STATIC);

  // "kills"
  kills := W_CacheLumpName('WIOSTK', PU_STATIC);

  // "scrt"
  secret := W_CacheLumpName('WIOSTS', PU_STATIC);

  // "secret"
  sp_secret := W_CacheLumpName('WISCRT2', PU_STATIC);

  // Yuck.
  if language = french then
  begin
    // "items"
    if netgame and (deathmatch = 0) then
      items := W_CacheLumpName('WIOBJ', PU_STATIC)
    else
      items := W_CacheLumpName('WIOSTI', PU_STATIC);
  end
  else
    items := W_CacheLumpName('WIOSTI', PU_STATIC);

  // "frgs"
  frags := W_CacheLumpName('WIFRGS', PU_STATIC);

  // ":"
  colon := W_CacheLumpName('WICOLON', PU_STATIC);

  // "time"
  time := W_CacheLumpName('WITIME', PU_STATIC);

  // "sucks"
  sucks := W_CacheLumpName('WISUCKS', PU_STATIC);

  // "par"
  par := W_CacheLumpName('WIPAR', PU_STATIC);

  // "killers" (vertical)
  killers := W_CacheLumpName('WIKILRS', PU_STATIC);

  // "victims" (horiz)
  victims := W_CacheLumpName('WIVCTMS', PU_STATIC);

  // "total"
  total := W_CacheLumpName('WIMSTT', PU_STATIC);

  // your face
  star := W_CacheLumpName('STFST01', PU_STATIC);

  // dead face
  bstar := W_CacheLumpName('STFDEAD0', PU_STATIC);

  for i := 0 to MAXPLAYERS - 1 do
  begin
    // "1,2,3,4"
    sprintf(name, 'STPB%d', [i]);
    p[i] := W_CacheLumpName(name, PU_STATIC);

    // "1,2,3,4"
    sprintf(name, 'WIBP%d', [i + 1]);
    bp[i] := W_CacheLumpName(name, PU_STATIC);
  end;

  wi_loaded := true;
end;

procedure WI_UnloadData;
var
  i: integer;
  j: integer;
begin
  Z_ChangeTag(wiminus, PU_CACHE);

  for i := 0 to 9 do
    Z_ChangeTag(num[i], PU_CACHE);

  if gamemode = commercial then
  begin
    for i := 0 to NUMCMAPS - 1 do
      Z_ChangeTag(lnames[i], PU_CACHE);
  end
  else
  begin
    Z_ChangeTag(yah[0], PU_CACHE);
    Z_ChangeTag(yah[1], PU_CACHE);

    Z_ChangeTag(splat, PU_CACHE);

    for i := 0 to NUMMAPS - 1 do
      Z_ChangeTag(lnames[i], PU_CACHE);

    if wbs.epsd < 3 then
    begin
      for j := 0 to NUMANIMS[wbs.epsd] - 1 do
      begin
        if (wbs.epsd <> 1) or (j <> 8) then
          for i := 0 to anims[wbs.epsd, j].nanims - 1 do
            Z_ChangeTag(anims[wbs.epsd, j].p[i], PU_CACHE);
      end;
    end;
  end;

  Z_Free(lnames);

  Z_ChangeTag(percent, PU_CACHE);
  Z_ChangeTag(colon, PU_CACHE);
  Z_ChangeTag(finished, PU_CACHE);
  Z_ChangeTag(entering, PU_CACHE);
  Z_ChangeTag(kills, PU_CACHE);
  Z_ChangeTag(secret, PU_CACHE);
  Z_ChangeTag(sp_secret, PU_CACHE);
  Z_ChangeTag(items, PU_CACHE);
  Z_ChangeTag(frags, PU_CACHE);
  Z_ChangeTag(time, PU_CACHE);
  Z_ChangeTag(sucks, PU_CACHE);
  Z_ChangeTag(par, PU_CACHE);

  Z_ChangeTag(victims, PU_CACHE);
  Z_ChangeTag(killers, PU_CACHE);
  Z_ChangeTag(total, PU_CACHE);
  Z_ChangeTag(star, PU_CACHE);
  Z_ChangeTag(bstar, PU_CACHE);

  for i := 0 to MAXPLAYERS - 1 do
    Z_ChangeTag(p[i], PU_CACHE);

  for i := 0 to MAXPLAYERS - 1 do
    Z_ChangeTag(bp[i], PU_CACHE);

  wi_loaded := false;
end;

procedure WI_Drawer;
begin
  if not wi_loaded then
    WI_LoadData;

  WI_slamBackground;

  // draw animated background
  WI_DrawAnimatedBack;

  case state of
    StatCount:
      begin
        if deathmatch <> 0 then
          WI_DrawDeathmatchStats
        else if netgame then
          WI_DrawNetgameStats
        else
          WI_DrawStats;
      end;

    ShowNextLoc:
      begin
        WI_DrawShowNextLoc;
      end;

    NoState:
      begin
        WI_DrawNoState;
      end;
  end;

  V_CopyRect(0, 0, SCN_TMP, 320, 200, 0, 0, SCN_FG, true);

  V_FullScreenStretch;
end;

procedure WI_InitVariables(wbstartstruct: Pwbstartstruct_t);
begin
  wbs := wbstartstruct;

  acceleratestage := 0;
  cnt := 0;
  bcnt := 0;
  firstrefresh := 1;
  me := wbs.pnum;
  plrs := @wbs.plyr;

  if wbs.maxkills = 0 then
    wbs.maxkills := 1;

  if wbs.maxitems = 0 then
    wbs.maxitems := 1;

  if wbs.maxsecret = 0 then
    wbs.maxsecret := 1;

  if gamemode <> retail then
    if wbs.epsd > 2 then
      wbs.epsd := wbs.epsd - 3;
end;

procedure WI_Start(wbstartstruct: Pwbstartstruct_t);
begin
  WI_InitVariables(wbstartstruct);
  WI_LoadData;

  if deathmatch <> 0 then
    WI_InitDeathmatchStats
  else if netgame then
    WI_InitNetgameStats
  else
    WI_InitStats;
end;

initialization
  anims[0] := @epsd0animinfo;
  anims[1] := @epsd1animinfo;
  anims[2] := @epsd2animinfo;
  anims[3] := nil;

end.

