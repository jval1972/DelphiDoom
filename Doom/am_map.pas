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
//   AutoMap module.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit am_map;

interface

uses
  d_delphi,
  z_zone,
  doomdef,
  doomdata,
  d_player,
  r_defs,
  d_event,
  st_stuff,
  p_local,
  w_wad,
  m_cheat,
  tables,
  m_fixed;

const
  AM_MSGHEADER = (Ord('a') shl 24) + (Ord('m') shl 16);
  AM_MSGENTERED = AM_MSGHEADER or (Ord('e') shl 8);
  AM_MSGEXITED = AM_MSGHEADER or (Ord('x') shl 8);

const
  REDS = 256 - (5 * 16);
  REDRANGE = 16;
  BLUES = (256 - (4 * 16)) + 8;
  BLUERANGE = 8;
  GREENS = 7 * 16;
  GREENRANGE = 16;
  GRAYS = 6 * 16;
  GRAYSRANGE = 16;
  BROWNS = 4 * 16;
  BROWNRANGE = 16;
  YELLOWS = (256 - 32) + 7;
  YELLOWRANGE = 1;
  WHITE = 256 - 47;

  { Automap colors }
  YOURCOLORS = WHITE;
  WALLCOLORS = REDS;
  WALLRANGE = REDRANGE;
  TSWALLCOLORS = GRAYS;
  TSWALLRANGE = GRAYSRANGE;
  FDWALLCOLORS = BROWNS;
  FDWALLRANGE = BROWNRANGE;
  CDWALLCOLORS = YELLOWS;
  CDWALLRANGE = YELLOWRANGE;
  THINGCOLORS = GREENS;
  THINGRANGE = GREENRANGE;
  SECRETWALLCOLORS = WALLCOLORS;
  SECRETWALLRANGE = WALLRANGE;
  GRIDCOLORS = GRAYS + (GRAYSRANGE div 2);
  GRIDRANGE = 0;
  XHAIRCOLORS = GRAYS;

  AM_PANDOWNKEY = KEY_DOWNARROW;
  AM_PANUPKEY = KEY_UPARROW;
  AM_PANRIGHTKEY = KEY_RIGHTARROW;
  AM_PANLEFTKEY = KEY_LEFTARROW;
  AM_ZOOMINKEY = '=';
  AM_ZOOMINKEY2 = '+';
  AM_ZOOMOUTKEY = '-';
  AM_TONGLEKEY = KEY_TAB;

var
  AM_GOBIGKEY: integer = Ord('o');
  AM_FOLLOWKEY: integer = Ord('f');
  AM_GRIDKEY: integer = Ord('g');
  AM_ROTATEKEY: integer = Ord('r');
  AM_TEXTUREDAUTOMAP: integer = Ord('t');
  AM_MARKKEY: integer = Ord('m');
  AM_CLEARMARKKEY: integer = Ord('c');

const
  AM_NUMMARKPOINTS = 10;

// scale on entry
  INITSCALEMTOF = FRACUNIT div 5;

// how much the automap moves window per tic in frame-buffer coordinates }
// moves 140 pixels in 1 second }
  F_PANINC = 4;

const
  MAPBITS = 12;
  MAPUNIT = 1 shl MAPBITS;
  FRACTOMAPBITS = FRACBITS - MAPBITS;
  FRACTOMAPUNIT = 1 shl FRACTOMAPBITS;

//==============================================================================
//
// M_ZOOMIN
//
// How much zoom-in per tic
//
//==============================================================================
function M_ZOOMIN: integer;

//==============================================================================
//
// M_ZOOMOUT
//
// How much zoom-out per tic
//
//==============================================================================
function M_ZOOMOUT: integer;

// Translates between frame-buffer and map distances

//==============================================================================
//
// FTOM
//
//==============================================================================
function FTOM(x: integer): integer;

//==============================================================================
//
// MTOF
//
//==============================================================================
function MTOF(x: integer): integer;

// Translates between frame-buffer and map coordinates

//==============================================================================
//
// CXMTOF
//
//==============================================================================
function CXMTOF(x: integer): integer;

//==============================================================================
//
// CYMTOF
//
//==============================================================================
function CYMTOF(y: integer): integer;

{ the following is crap }

const
  LINE_NEVERSEE = ML_DONTDRAW;

type
  fpoint_t = record
    x: integer;
    y: integer;
  end;
  Pfpoint_t = ^fpoint_t;

  fline_t = record
    a: fpoint_t;
    b: fpoint_t;
  end;
  Pfline_t = ^fline_t;

  mpoint_t = record
    x: fixed_t;
    y: fixed_t;
  end;
  Pmpoint_t = ^mpoint_t;

  mline_t = record
    a: mpoint_t;
    b: mpoint_t;
  end;
  Pmline_t = ^mline_t;
  mline_tArray = packed array[0..$FFFF] of mline_t;
  Pmline_tArray = ^mline_tArray;

  islope_t = record
    slp: fixed_t;
    islp: fixed_t;
  end;
  Pislope_t = ^islope_t;

//
// The vector graphics for the automap.
//  A line drawing of the player pointing right,
//   starting from the middle.
//
const
  NUMPLYRLINES = 7;

var
  player_arrow: array[0..NUMPLYRLINES - 1] of mline_t;

const
  NUMCHEATPLYRLINES = 16;

var
  cheat_player_arrow: array[0..NUMCHEATPLYRLINES - 1] of mline_t;

const
  NUMTRIANGLEGUYLINES = 3;

var
  triangle_guy: array[0..NUMTRIANGLEGUYLINES - 1] of mline_t;

const
  NUMTHINTRIANGLEGUYLINES = 3;

var
  thintriangle_guy: array[0..NUMTHINTRIANGLEGUYLINES - 1] of mline_t;

const
  NUMTTRACEPLAYERLINES = 3;

var
  traceplayer_guy: array[0..NUMTTRACEPLAYERLINES - 1] of mline_t;

type
  automapstate_t = (am_inactive, am_only, am_overlay, AM_NUMSTATES);

var
  am_cheating: integer = 0;
  automapgrid: boolean = false;
  automaptraceplayer: integer = 0;

  leveljuststarted: integer = 1;   // kluge until AM_LevelInit() is called

  amstate: automapstate_t = am_inactive; //boolean = false;

// location of window on screen
  f_x: integer;
  f_y: integer;

// size of window on screen
  f_w: integer;
  f_h: integer;

  lightlev: integer;      // used for funky strobing effect
  fb: PByteArray;         // pseudo-frame buffer
  fb32: PLongWordArray;   // pseudo-frame buffer
  amclock: integer;

  m_paninc: mpoint_t;     // how far the window pans each tic (map coords)
  mtof_zoommul: fixed_t;  // how far the window zooms in each tic (map coords)
  ftom_zoommul: fixed_t;  // how far the window zooms in each tic (fb coords)

  m_x, m_y: fixed_t;      // LL x,y where the window is on the map (map coords)
  m_x2, m_y2: fixed_t;    // UR x,y where the window is on the map (map coords)

//
// width/height of window on map (map coords)
//
  m_w: fixed_t;
  m_h: fixed_t;

// based on level size
  min_x: fixed_t;
  min_y: fixed_t;
  max_x: fixed_t;
  max_y: fixed_t;

  max_w: fixed_t; // max_x-min_x,
  max_h: fixed_t; // max_y-min_y

// based on player size
  min_w: fixed_t;
  min_h: fixed_t;

  min_scale_mtof: fixed_t; // used to tell when to stop zooming out
  max_scale_mtof: fixed_t; // used to tell when to stop zooming in

// old stuff for recovery later
  old_m_w, old_m_h: fixed_t;
  old_m_x, old_m_y: fixed_t;

// old location used by the Follower routine
  f_oldloc: mpoint_t;

// used by MTOF to scale from map-to-frame-buffer coords
  scale_mtof: fixed_t = INITSCALEMTOF;
// used by FTOM to scale from frame-buffer-to-map coords (=1/scale_mtof)
  scale_ftom: fixed_t;

  plr: Pplayer_t; // the player represented by an arrow

var
  marknums: array[0..9] of Ppatch_t;  // numbers used for marking by the automap
  marknumsid: array[0..9] of integer; // numbers used for marking by the automap

  markpoints: array[0..AM_NUMMARKPOINTS - 1] of mpoint_t; // where the points are

  markpointnum: integer = 0; // next point to be assigned

  followplayer: boolean = true; // specifies whether to follow the player around

const
  cheat_amap_seq: string = Chr($b2) + Chr($26) + Chr($26) + Chr($2e) + Chr($ff); // iddt

var
  cheat_amap: cheatseq_t;

  stopped: boolean = true;

//==============================================================================
//
// AM_Responder
//
//==============================================================================
function AM_Responder(ev: Pevent_t): boolean;

//==============================================================================
// AM_Ticker
//
// Called by main loop.
//
//==============================================================================
procedure AM_Ticker;

//==============================================================================
// AM_Drawer
//
// Called by main loop,
// called instead of view drawer if automap active.
//
//==============================================================================
procedure AM_Drawer;

//==============================================================================
//
// AM_Init
//
//==============================================================================
procedure AM_Init;

//==============================================================================
// AM_Stop
//
// Called to force the automap to quit
// if the level is completed while it is up.
//
//==============================================================================
procedure AM_Stop;

//==============================================================================
//
// AM_Start
//
//==============================================================================
procedure AM_Start;

var
  allowautomapoverlay: boolean;
  allowautomaprotate: boolean;
  texturedautomap: boolean;

//==============================================================================
//
// AM_rotate
//
//==============================================================================
procedure AM_rotate(x: Pfixed_t; y: Pfixed_t; a: angle_t; xpos, ypos: fixed_t);

//==============================================================================
//
// AM_ShutDown
//
//==============================================================================
procedure AM_ShutDown;

implementation

uses
  am_textured,
  c_cmds,
  d_englsh,
  g_game,
  mt_utils,
  p_mobj_h,
  p_setup,
  p_maputl,
  p_playertrace,
  p_tick,
  r_data,
{$IFNDEF OPENGL}
  r_draw,
  r_hires,
{$ELSE}
  gl_automap,
{$ENDIF}
  v_data,
  v_video;

const
  AM_MIN_SCALE = 2048;
// player radius
  PLAYERRADIUS = 16 * (1 shl MAPBITS);

//==============================================================================
//
// CmdAllowautomapoverlay
//
//==============================================================================
procedure CmdAllowautomapoverlay(const parm: string);
begin
  allowautomapoverlay := C_BoolEval(parm, allowautomapoverlay);
  if not allowautomapoverlay and (amstate = am_overlay) then
    amstate := am_inactive;
end;

//==============================================================================
// M_ZOOMIN
//
// how much zoom-in per tic
//
//==============================================================================
function M_ZOOMIN: integer;
begin
  result := trunc(1.02 * FRACUNIT);
end;

//==============================================================================
// M_ZOOMOUT
//
// how much zoom-out per tic
//
//==============================================================================
function M_ZOOMOUT: integer;
begin
  result := trunc(FRACUNIT / 1.02);
end;

//==============================================================================
//
// FTOM
//
//==============================================================================
function FTOM(x: integer): integer;
begin
  result := FixedMul(x * FRACUNIT, scale_ftom);
end;

//==============================================================================
//
// MTOF
//
//==============================================================================
function MTOF(x: integer): integer;
begin
  result := FixedInt64(FixedMul64(x, scale_mtof));
end;

//==============================================================================
//
// CXMTOF
//
//==============================================================================
function CXMTOF(x: integer): integer;
begin
  result := f_x + MTOF(x) - MTOF(m_x);
end;

//==============================================================================
//
// CYMTOF
//
//==============================================================================
function CYMTOF(y: integer): integer;
begin
  result := f_y + (f_h - MTOF(y) + MTOF(m_y));
end;

//==============================================================================
// AM_getIslope
//
//==============================================================================
procedure AM_getIslope(ml: Pmline_t; _is: Pislope_t);
var
  dx, dy: integer;
begin
  dx := ml.b.x - ml.a.x;
  dy := ml.a.y - ml.b.y;

  if dy = 0 then
  begin
    if dx < 0 then
      _is.islp := -MAXINT
    else
      _is.islp := MAXINT
  end
  else
    _is.islp := FixedDiv(dx, dy);

  if dx = 0 then
  begin
    if dy < 0 then
      _is.slp := -MAXINT
    else
      _is.slp := MAXINT
  end
  else
    _is.slp := FixedDiv(dy, dx);
end;

//==============================================================================
// AM_activateNewScale
//
//==============================================================================
procedure AM_activateNewScale;
begin
  m_x := m_x + m_w div 2;
  m_y := m_y + m_h div 2;
  m_w := FTOM(f_w);
  m_h := FTOM(f_h);
  m_x := m_x - m_w div 2;
  m_y := m_y - m_h div 2;
  m_x2 := m_x + m_w;
  m_y2 := m_y + m_h;
end;

//==============================================================================
// AM_saveScaleAndLoc
//
//==============================================================================
procedure AM_saveScaleAndLoc;
begin
  old_m_x := m_x;
  old_m_y := m_y;
  old_m_w := m_w;
  old_m_h := m_h;
end;

//==============================================================================
// AM_restoreScaleAndLoc
//
//==============================================================================
procedure AM_restoreScaleAndLoc;
begin
  m_w := old_m_w;
  m_h := old_m_h;
  if not followplayer then
  begin
    m_x := old_m_x;
    m_y := old_m_y;
  end
  else
  begin
    m_x := (plr.mo.x div FRACTOMAPUNIT) - m_w div 2;
    m_y := (plr.mo.y div FRACTOMAPUNIT) - m_h div 2;
  end;

  m_x2 := m_x + m_w;
  m_y2 := m_y + m_h;

  // Change the scaling multipliers
  scale_mtof := FixedDiv(f_w * FRACUNIT, m_w);
  scale_ftom := FixedDiv(FRACUNIT, scale_mtof);
end;

//==============================================================================
// AM_addMark
//
// adds a marker at the current location
//
//==============================================================================
procedure AM_addMark;
begin
  markpoints[markpointnum].x := m_x + m_w div 2;
  markpoints[markpointnum].y := m_y + m_h div 2;
  markpointnum := (markpointnum + 1) mod AM_NUMMARKPOINTS;
end;

//==============================================================================
// AM_findMinMaxBoundaries
//
// Determines bounding box of all vertices,
// sets global variables controlling zoom range.
//
//==============================================================================
procedure AM_findMinMaxBoundaries;
var
  i: integer;
  a, b: fixed_t;
  pvi: Pvertex_t;
begin
  min_x := MAXINT;
  min_y := MAXINT;
  max_x := -MAXINT;
  max_y := -MAXINT;

  pvi := @vertexes[0];
  for i := 0 to numvertexes - 1 do
  begin
    if pvi.x < min_x then
      min_x := pvi.x
    else if pvi.x > max_x then
      max_x := pvi.x;

    if pvi.y < min_y then
      min_y := pvi.y
    else if pvi.y > max_y then
      max_y := pvi.y;
    inc(pvi);

  end;

  max_x := max_x div FRACTOMAPUNIT;
  min_x := min_x div FRACTOMAPUNIT;
  max_y := max_y div FRACTOMAPUNIT;
  min_y := min_y div FRACTOMAPUNIT;

  max_w := max_x - min_x;
  max_h := max_y - min_y;

  min_w := 10 * PLAYERRADIUS; // const? never changed?
  min_h := 10 * PLAYERRADIUS;

  a := FixedDiv(f_w * FRACUNIT, max_w);
  b := FixedDiv(f_h * FRACUNIT, max_h);

  if a < b then
    min_scale_mtof := a
  else
    min_scale_mtof := b;

  max_scale_mtof := FixedDiv(f_h * FRACUNIT, 2 * PLAYERRADIUS);
end;

//==============================================================================
// AM_changeWindowLoc
//
//==============================================================================
procedure AM_changeWindowLoc;
begin
  if (m_paninc.x <> 0) or (m_paninc.y <> 0) then
  begin
    followplayer := false;
    f_oldloc.x := MAXINT;
  end;

  m_x := m_x + m_paninc.x;
  m_y := m_y + m_paninc.y;

  if m_x + m_w div 2 > max_x then
    m_x := max_x - m_w div 2
  else if m_x + m_w div 2 < min_x then
    m_x := min_x - m_w div 2;

  if m_y + m_h div 2 > max_y then
    m_y := max_y - m_h div 2
  else if m_y + m_h div 2 < min_y then
    m_y := min_y - m_h div 2;

  m_x2 := m_x + m_w;
  m_y2 := m_y + m_h;
end;

//
//
//
var
  st_notify_AM_initVariables: event_t;

//==============================================================================
//
// AM_initVariables
//
//==============================================================================
procedure AM_initVariables;
var
  pnum: integer;
  i: integer;
begin
  fb := screens[SCN_FG];
  fb32 := screen32;

  f_oldloc.x := MAXINT;
  amclock := 0;
  lightlev := 0;

  m_paninc.x := 0;
  m_paninc.y := 0;

  ftom_zoommul := FRACUNIT;
  mtof_zoommul := FRACUNIT;

  m_w := FTOM(f_w);
  m_h := FTOM(f_h);

  if gamestate = GS_LEVEL then
  begin
    // find player to center on initially
    pnum := consoleplayer;
    if not playeringame[pnum] then
    begin
      pnum := -1;
      for i := 0 to MAXPLAYERS - 1 do
        if playeringame[i] then
        begin
          pnum := i;
          break;
        end;
    end;

    if pnum >= 0 then
    begin
      plr := @players[pnum];
      if plr.mo <> nil then
      begin
        m_x := (plr.mo.x div FRACTOMAPUNIT) - m_w div 2;
        m_y := (plr.mo.y div FRACTOMAPUNIT) - m_h div 2;
      end;
    end;
  end;

  AM_changeWindowLoc;

  // for saving & restoring
  //AM_saveScaleAndLoc;
  old_m_x := m_x;
  old_m_y := m_y;
  old_m_w := m_w;
  old_m_h := m_h;

  // inform the status bar of the change
  ST_Responder(@st_notify_AM_initVariables);
end;

//==============================================================================
// AM_loadPics
//
//==============================================================================
procedure AM_loadPics;
var
  i: integer;
  namebuf: string;
begin
  for i := 0 to AM_NUMMARKPOINTS - 1 do
  begin
    sprintf(namebuf, 'AMMNUM%d', [i]);
    marknumsid[i] := W_GetNumForName(namebuf);
    marknums[i] := W_CacheLumpNum(marknumsid[i], PU_STATIC);
  end;
end;

//==============================================================================
//
// AM_unloadPics
//
//==============================================================================
procedure AM_unloadPics;
var
  i: integer;
begin
  for i := 0 to AM_NUMMARKPOINTS - 1 do
    Z_ChangeTag(marknums[i], PU_CACHE);
end;

//==============================================================================
//
// AM_clearMarks
//
//==============================================================================
procedure AM_clearMarks;
var
  i: integer;
begin
  for i := 0 to AM_NUMMARKPOINTS - 1 do
    markpoints[i].x := -1; // means empty
  markpointnum := 0;
end;

//==============================================================================
// AM_LevelInit
//
// should be called at the start of every level
// right now, i figure it out myself
//
//==============================================================================
procedure AM_LevelInit;
begin
  leveljuststarted := 0;

  f_x := 0;
  f_y := 0;
  f_w := SCREENWIDTH;
  f_h := ST_Y * SCREENHEIGHT div 200;

  AM_clearMarks;

  AM_findMinMaxBoundaries;
  scale_mtof := FixedDiv(min_scale_mtof, trunc(0.7 * FRACUNIT));
  if scale_mtof > max_scale_mtof then
    scale_mtof := min_scale_mtof;
  scale_ftom := FixedDiv(FRACUNIT, scale_mtof);
end;

//
//
//
var
  st_notify_AM_Stop: event_t;

//==============================================================================
//
// AM_Stop
//
//==============================================================================
procedure AM_Stop;
begin
  if not stopped then
  begin
    AM_unloadPics;
    ST_Responder(@st_notify_AM_Stop);
    stopped := true;
  end;
end;

//
//
//
var
  lastlevel: integer = -1;
  lastepisode: integer = -1;
  lastscreenwidth: integer = -1;
  lastscreenheight: integer = -1;

//==============================================================================
//
// AM_Start
//
//==============================================================================
procedure AM_Start;
begin
  AM_Stop;

  stopped := false;

  if (lastlevel <> gamemap) or (lastepisode <> gameepisode) or
     (lastscreenwidth <> SCREENWIDTH) or (lastscreenheight <> SCREENHEIGHT) then
  begin
    AM_LevelInit;
    lastlevel := gamemap;
    lastepisode := gameepisode;
    lastscreenwidth := SCREENWIDTH;
    lastscreenheight := SCREENHEIGHT;
  end;
  AM_initVariables;
  AM_loadPics;
end;

//==============================================================================
// AM_minOutWindowScale
//
// set the window scale to the maximum size
//
//==============================================================================
procedure AM_minOutWindowScale;
begin
  scale_mtof := min_scale_mtof;
  scale_ftom := FixedDiv(FRACUNIT, scale_mtof);
  AM_activateNewScale;
end;

//==============================================================================
// AM_maxOutWindowScale
//
// set the window scale to the minimum size
//
//==============================================================================
procedure AM_maxOutWindowScale;
begin
  scale_mtof := max_scale_mtof;
  scale_ftom := FixedDiv(FRACUNIT, scale_mtof);
  AM_activateNewScale;
end;

//
// Handle events (user inputs) in automap mode
//
var
  bigstate: boolean = false;

//==============================================================================
//
// AM_Responder
//
//==============================================================================
function AM_Responder(ev: Pevent_t): boolean;
var
  _message: string;
begin
  result := false;

  if amstate = am_inactive then
  begin
    if (ev._type = ev_keydown) and (ev.data1 = AM_TONGLEKEY) then
    begin
      if allowautomapoverlay then
        amstate := automapstate_t((Ord(amstate) + 1) mod Ord(AM_NUMSTATES))
      else
      begin
        if amstate = am_inactive then
          amstate := am_only
        else
          amstate := am_inactive
      end;
      AM_Start;
      viewactive := false;
      result := true;
    end;
  end
  else if ev._type = ev_keydown then
  begin
    result := true;
    case ev.data1 of
      AM_PANRIGHTKEY: // pan right
        begin
          if not followplayer then
            m_paninc.x := FTOM(F_PANINC)
          else
            result := false;
        end;
      AM_PANLEFTKEY: // pan left
        begin
          if not followplayer then
            m_paninc.x := -FTOM(F_PANINC)
          else
            result := false;
        end;
      AM_PANUPKEY: // pan up
        begin
          if not followplayer then
            m_paninc.y := FTOM(F_PANINC)
          else
            result := false;
        end;
      AM_PANDOWNKEY: // pan down
        begin
          if not followplayer then
            m_paninc.y := -FTOM(F_PANINC)
          else
            result := false;
        end;
      Ord(AM_ZOOMOUTKEY): // zoom out
        begin
          mtof_zoommul := M_ZOOMOUT;
          ftom_zoommul := M_ZOOMIN;
        end;
      Ord(AM_ZOOMINKEY),
      Ord(AM_ZOOMINKEY2): // zoom in
        begin
          mtof_zoommul := M_ZOOMIN;
          ftom_zoommul := M_ZOOMOUT;
        end;
      AM_TONGLEKEY:
        begin
          amstate := automapstate_t((Ord(amstate) + 1) mod Ord(AM_NUMSTATES));
          if amstate <> am_only then
          begin
            bigstate := false;
            viewactive := true;
            if amstate = am_inactive then
              AM_Stop;
          end;
        end;
      else
      begin
        if ev.data1 = AM_GOBIGKEY then
        begin
          bigstate := not bigstate;
          if bigstate then
          begin
            AM_saveScaleAndLoc;
            AM_minOutWindowScale;
          end
          else
            AM_restoreScaleAndLoc;
        end
        else if ev.data1 = AM_FOLLOWKEY then
        begin
          followplayer := not followplayer;
          f_oldloc.x := MAXINT;
          if followplayer then
            plr._message := AMSTR_FOLLOWON
          else
            plr._message := AMSTR_FOLLOWOFF;
        end
        else if ev.data1 = AM_GRIDKEY then
        begin
          automapgrid := not automapgrid;
          if automapgrid then
            plr._message := AMSTR_GRIDON
          else
            plr._message := AMSTR_GRIDOFF;
        end
        else if ev.data1 = AM_ROTATEKEY then
        begin
          allowautomaprotate := not allowautomaprotate;
          if allowautomaprotate then
            plr._message := AMSTR_ROTATEON
          else
            plr._message := AMSTR_ROTATEOFF;
        end
        else if ev.data1 = AM_TEXTUREDAUTOMAP then
        begin
          texturedautomap := not texturedautomap;
          if texturedautomap then
            plr._message := 'TEXTURED AUTOMAP ON'
          else
            plr._message := 'TEXTURED AUTOMAP OFF';
        end
        else if ev.data1 = AM_MARKKEY then
        begin
          sprintf(_message, '%s %d', [AMSTR_MARKEDSPOT, markpointnum]);
          plr._message := _message;
          AM_addMark;
        end
        else if ev.data1 = AM_CLEARMARKKEY then
        begin
          AM_clearMarks;
          plr._message := AMSTR_MARKSCLEARED;
        end
        else
          result := false;
      end;
    end;
  end
  else if ev._type = ev_keyup then
  begin
    result := false;
    case ev.data1 of
      AM_PANRIGHTKEY:
        begin
          if not followplayer then
            m_paninc.x := 0;
        end;
      AM_PANLEFTKEY:
        begin
          if not followplayer then
            m_paninc.x := 0;
        end;
      AM_PANUPKEY:
        begin
          if not followplayer then
            m_paninc.y := 0;
        end;
      AM_PANDOWNKEY:
        begin
          if not followplayer then
            m_paninc.y := 0;
        end;
      Ord(AM_ZOOMOUTKEY),
      Ord(AM_ZOOMINKEY),
      Ord(AM_ZOOMINKEY2):
        begin
          mtof_zoommul := FRACUNIT;
          ftom_zoommul := FRACUNIT;
        end;
    end;
  end;

end;

//==============================================================================
// AM_changeWindowScale
//
// Zooming
//
//==============================================================================
procedure AM_changeWindowScale;
begin
  // Change the scaling multipliers
  scale_mtof := FixedMul(scale_mtof, mtof_zoommul);
  scale_ftom := FixedDiv(FRACUNIT, scale_mtof);

  if scale_mtof < min_scale_mtof then
    AM_minOutWindowScale
  else if scale_mtof > max_scale_mtof then
    AM_maxOutWindowScale
  else
    AM_activateNewScale;
end;

//==============================================================================
// AM_doFollowPlayer
//
//==============================================================================
procedure AM_doFollowPlayer;
begin
  if (f_oldloc.x <> plr.mo.x) or (f_oldloc.y <> plr.mo.y) then
  begin
    m_x := FTOM(MTOF(plr.mo.x div FRACTOMAPUNIT)) - m_w div 2;
    m_y := FTOM(MTOF(plr.mo.y div FRACTOMAPUNIT)) - m_h div 2;
    m_x2 := m_x + m_w;
    m_y2 := m_y + m_h;
    f_oldloc.x := plr.mo.x;
    f_oldloc.y := plr.mo.y;
  end;
end;

//
//
//
const
  NUMLITELEVELS = 8;

var
  nexttic: integer = 0;
  litelevels: array[0..NUMLITELEVELS - 1] of integer = (0, 4, 7, 10, 12, 14, 15, 15);
  litelevelscnt: integer = 0;

//==============================================================================
//
// AM_updateLightLev
//
//==============================================================================
procedure AM_updateLightLev;
begin
  // Change light level
  if amclock > nexttic then
  begin
    lightlev := litelevels[litelevelscnt];
    litelevelscnt := (litelevelscnt + 1) mod NUMLITELEVELS;
    nexttic := amclock + 6 - (amclock mod 6);
  end;
end;

//==============================================================================
//
// AM_Ticker
//
//==============================================================================
procedure AM_Ticker;
begin
  if amstate = am_inactive then
    exit;

  inc(amclock);

  if followplayer then
    AM_doFollowPlayer;

  // Change the zoom if necessary
  if ftom_zoommul <> FRACUNIT then
    AM_changeWindowScale;

  // Change x,y location
  if (m_paninc.x <> 0) or (m_paninc.y <> 0) then
    AM_changeWindowLoc;

    // Update light level
    // AM_updateLightLev();

end;

//
// Clear automap frame buffer.
//
{$IFNDEF OPENGL}

//==============================================================================
//
// AM_clearFB
//
//==============================================================================
procedure AM_clearFB(color: integer);
var
  c: LongWord;
begin
  if videomode = vm32bit then
  begin
    c := videopal[color];
    MT_memseti(fb32, c, f_w * f_h);
  end
  else
    MT_memset(fb, color, f_w * f_h);
end;
{$ENDIF}

//==============================================================================
// AM_clipMline
//
// Automap clipping of lines.
//
// Based on Cohen-Sutherland clipping algorithm but with a slightly
// faster reject and precalculated slopes.  If the speed is needed,
// use a hash algorithm to handle  the common cases.
//
//==============================================================================
function AM_clipMline(ml: Pmline_t; fl: Pfline_t): boolean;
const
  LEFT = 1;
  RIGHT  = 2;
  BOTTOM = 4;
  TOP = 8;
var
  outcode1, outcode2, outside: integer;
  tmp: fpoint_t;
  dx, dy: integer;

  procedure DOOUTCODE(var oc: integer; mx, my: integer);
  begin
    oc := 0;
    if my < 0 then
      oc := oc or TOP
    else if my >= f_h then
      oc := oc or BOTTOM;
    if mx < 0 then
      oc := oc or LEFT
    else if mx >= f_w then
      oc := oc or RIGHT;
  end;

begin
  // do trivial rejects and outcodes
  if ml.a.y > m_y2 then
    outcode1 := TOP
  else if ml.a.y < m_y then
    outcode1 := BOTTOM
  else
    outcode1 := 0;

  if ml.b.y > m_y2 then
    outcode2 := TOP
  else if ml.b.y < m_y then
    outcode2 := BOTTOM
  else
    outcode2 := 0;

  if outcode1 and outcode2 <> 0 then
  begin
    result := false; // trivially outside
    exit;
  end;

  if ml.a.x < m_x then
    outcode1 := outcode1 or LEFT
  else if ml.a.x > m_x2 then
    outcode1 := outcode1 or RIGHT;

  if ml.b.x < m_x then
    outcode2 := outcode2 or LEFT
  else if ml.b.x > m_x2 then
    outcode2 := outcode2 or RIGHT;

  if outcode1 and outcode2 <> 0 then
  begin
    result := false; // trivially outside
    exit;
  end;

  // transform to frame-buffer coordinates.
  fl.a.x := CXMTOF(ml.a.x);
  fl.a.y := CYMTOF(ml.a.y);
  fl.b.x := CXMTOF(ml.b.x);
  fl.b.y := CYMTOF(ml.b.y);

  DOOUTCODE(outcode1, fl.a.x, fl.a.y);
  DOOUTCODE(outcode2, fl.b.x, fl.b.y);

  if outcode1 and outcode2 <> 0 then
  begin
    result := false; // trivially outside
    exit;
  end;

  while outcode1 or outcode2 <> 0 do
  begin
  // may be partially inside box
  // find an outside point
    if outcode1 <> 0 then
      outside := outcode1
    else
      outside := outcode2;

  // clip to each side
    if outside and TOP <> 0 then
    begin
      dy := fl.a.y - fl.b.y;
      dx := fl.b.x - fl.a.x;
      tmp.x := fl.a.x + (dx * (fl.a.y)) div dy;
      tmp.y := 0;
    end
    else if outside and BOTTOM <> 0 then
    begin
      dy := fl.a.y - fl.b.y;
      dx := fl.b.x - fl.a.x;
      tmp.x := fl.a.x + (dx * (fl.a.y - f_h)) div dy;
      tmp.y := f_h - 1;
    end
    else if outside and RIGHT <> 0 then
    begin
      dy := fl.b.y - fl.a.y;
      dx := fl.b.x - fl.a.x;
      tmp.y := fl.a.y + (dy * (f_w - 1 - fl.a.x)) div dx;
      tmp.x := f_w - 1;
    end
    else if outside and LEFT <> 0 then
    begin
      dy := fl.b.y - fl.a.y;
      dx := fl.b.x - fl.a.x;
      tmp.y := fl.a.y + (dy * (-fl.a.x)) div dx;
      tmp.x := 0;
    end;

    if outside = outcode1 then
    begin
      fl.a := tmp;
      DOOUTCODE(outcode1, fl.a.x, fl.a.y);
    end
    else
    begin
      fl.b := tmp;
      DOOUTCODE(outcode2, fl.b.x, fl.b.y);
    end;

    if outcode1 and outcode2 <> 0 then
    begin
      result := false; // trivially outside
      exit;
    end;
  end;

  result := true;
end;

//==============================================================================
// AM_drawFline
//
// Classic Bresenham w/ whatever optimizations needed for speed
//
//==============================================================================
procedure AM_drawFline(fl: Pfline_t; color: integer);
{$IFDEF OPENGL}
begin
  gld_AddAutomapLine(fl.a.x, fl.a.y, fl.b.x, fl.b.y,  videopal[color]);
end;
{$ELSE}
var
  x, y,
  dx, dy,
  sx, sy,
  ax, ay,
  d: integer;

  procedure PUTDOT(xx, yy, cc: integer);
  begin
  // JVAL Clip line if in overlay mode
    if amstate = am_overlay then
    begin
      if yy <= viewwindowy then
        exit;
      if yy >= viewwindowy + viewheight then
        exit;
      if xx <= viewwindowx then
        exit;
      if xx >= viewwindowx + viewwidth then
        exit;
    end;
    if videomode = vm32bit then
      fb32[yy * f_w + xx] := videopal[cc]
    else
      fb[yy * f_w + xx] := cc;
  end;

begin
  // For debugging only
  {$IFDEF DEBUG}
  if (fl.a.x < 0) or (fl.a.x >= f_w) or
     (fl.a.y < 0) or (fl.a.y >= f_h) or
     (fl.b.x < 0) or (fl.b.x >= f_w) or
     (fl.b.y < 0) or (fl.b.y >= f_h) then
  begin
    I_Error('AM_drawFline(): fuck!');
    exit;
  end;
  {$ENDIF}

  dx := fl.b.x - fl.a.x;
  ax := 2 * abs(dx);
  if dx < 0 then
    sx := -1
  else
    sx := 1;

  dy := fl.b.y - fl.a.y;
  ay := 2 * abs(dy);
  if dy < 0 then
    sy := -1
  else
    sy := 1;

  x := fl.a.x;
  y := fl.a.y;

  if ax > ay then
  begin
    d := ay - ax div 2;
    while true do
    begin
      PUTDOT(x, y, color);
      if x = fl.b.x then exit;
      if d >= 0 then
      begin
        y := y + sy;
        d := d - ax;
      end;
      x := x + sx;
      d := d + ay;
    end;
  end
  else
  begin
    d := ax - ay div 2;
    while true do
    begin
      PUTDOT(x, y, color);
      if y = fl.b.y then exit;
      if d >= 0 then
      begin
        x := x + sx;
        d := d - ay;
      end;
      y := y + sy;
      d := d + ax;
    end;
  end;
end;
{$ENDIF}

//
// Clip lines, draw visible parts of lines.
//
var
  fl: fline_t;

//==============================================================================
//
// AM_drawMline
//
//==============================================================================
procedure AM_drawMline(ml: Pmline_t; color: integer);
begin
  if AM_clipMline(ml, @fl) then
    AM_drawFline(@fl, color); // draws it on frame buffer using fb coords
end;

//==============================================================================
// AM_drawGrid
//
// Draws flat (floor/ceiling tile) aligned grid lines.
//
//==============================================================================
procedure AM_drawGrid(color: integer);
var
  x, y: fixed_t;
  start, finish: fixed_t;
  ml: mline_t;
  dw, dh: double;
  minlen, extx, exty: fixed_t;
  minx, miny: fixed_t;
  rx, ry: fixed_t;
begin
  dw := m_w;
  dh := m_h;

  minlen := trunc(sqrt(2.0 * dw * dw + 2.0 * dh * dh));
  extx := (minlen - m_w) div 2;
  exty := (minlen - m_h) div 2;

  minx := m_x;
  miny := m_y;

  rx := plr.mo.x div FRACTOMAPUNIT;
  ry := plr.mo.y div FRACTOMAPUNIT;

  // Figure out start of vertical gridlines
  start := m_x - extx;
  if ((start - bmaporgx div FRACTOMAPUNIT) mod (MAPBLOCKUNITS * FRACUNIT)) <> 0 then
    start := start {+ (MAPBLOCKUNITS * FRACUNIT)}
          - ((start - bmaporgx div FRACTOMAPUNIT) mod (MAPBLOCKUNITS * FRACUNIT));
  finish := minx + minlen - extx;

  // draw vertical gridlines
  x := start;
  while x < finish do
  begin
    ml.a.x := x;
    ml.b.x := x;
    ml.a.y := miny - exty;
    ml.b.y := ml.a.y + minlen;

    if allowautomaprotate then
    begin
      AM_rotate(@ml.a.x, @ml.a.y, ANG90 - plr.mo.angle, rx, ry);
      AM_rotate(@ml.b.x, @ml.b.y, ANG90 - plr.mo.angle, rx, ry);
    end;

    AM_drawMline(@ml, color);
    x := x + (MAPBLOCKUNITS * MAPUNIT);
  end;

  // Figure out start of horizontal gridlines
  start := miny - exty;
  if ((start - bmaporgy) mod (MAPBLOCKUNITS * FRACUNIT)) <> 0 then
    start := start {+ (MAPBLOCKUNITS * FRACUNIT)}
          - ((start - bmaporgy) mod (MAPBLOCKUNITS * FRACUNIT));
  finish := miny + minlen - exty;

  // draw horizontal gridlines
  y := start;
  while y < finish do
  begin
    ml.a.x := minx - extx;
    ml.b.x := ml.a.x + minlen;
    ml.a.y := y;
    ml.b.y := y;

    if allowautomaprotate then
    begin
      AM_rotate(@ml.a.x, @ml.a.y, ANG90 - plr.mo.angle, rx, ry);
      AM_rotate(@ml.b.x, @ml.b.y, ANG90 - plr.mo.angle, rx, ry);
    end;

    AM_drawMline(@ml, color);
    y := y + (MAPBLOCKUNITS * MAPUNIT);
  end;
end;

//==============================================================================
// AM_rotate
//
// Rotation in 2D.
// Used to rotate player arrow line character.
//
//==============================================================================
procedure AM_rotate(x: Pfixed_t; y: Pfixed_t; a: angle_t; xpos, ypos: fixed_t);
var
  tmpx: fixed_t;
  ang: angle_t;
begin
  ang := a shr FRACBITS;

  tmpx := xpos +
    FixedMul(x^ - xpos, fixedcosine[ang]) -
    FixedMul(y^ - ypos, fixedsine[ang]);

  y^ := ypos +
    FixedMul(x^ - xpos, fixedsine[ang]) +
    FixedMul(y^ - ypos, fixedcosine[ang]);

  x^ := tmpx;
end;

//==============================================================================
//
// AM_rotate_dbl
//
//==============================================================================
procedure AM_rotate_dbl(x: Pfixed_t; y: Pfixed_t; a: angle_t; xpos, ypos: fixed_t);
var
  tmpx: fixed_t;
  ang: double;
  asin, acos: double;
begin
  ang := a * ANGLE_T_TO_RAD;
  asin := Sin(ang);
  acos := Cos(ang);
  tmpx := xpos + Round((x^ - xpos) * acos - (y^ - ypos) * asin);
  y^ := ypos + Round((x^ - xpos) * asin + (y^ - ypos) * acos);
  x^ := tmpx;
end;

//==============================================================================
// AM_drawWalls
//
// Determines visible lines, draws them.
// This is LineDef based, not LineSeg based.
//
//==============================================================================
procedure AM_drawWalls;
var
  i: integer;
  l: mline_t;
  pl: Pline_t;
  plrx, plry: fixed_t;
  plra: angle_t;
begin
  plrx := plr.mo.x div FRACTOMAPUNIT;
  plry := plr.mo.y div FRACTOMAPUNIT;
  plra := ANG90 - plr.mo.angle;
  pl := @lines[0];
  for i := 0 to numlines - 1 do
  begin
    l.a.x := pl.v1.x div FRACTOMAPUNIT;
    l.a.y := pl.v1.y div FRACTOMAPUNIT;
    l.b.x := pl.v2.x div FRACTOMAPUNIT;
    l.b.y := pl.v2.y div FRACTOMAPUNIT;

    if allowautomaprotate then
    begin
      AM_rotate(@l.a.x, @l.a.y, plra, plrx, plry);
      AM_rotate(@l.b.x, @l.b.y, plra, plrx, plry);
    end;

    if (am_cheating <> 0) or (pl.flags and ML_MAPPED <> 0) then
    begin
      if (pl.flags and LINE_NEVERSEE <> 0) and (am_cheating = 0) then
      begin
        inc(pl);
        continue;
      end;
      if pl.backsector = nil then
      begin
        AM_drawMline(@l, WALLCOLORS + lightlev);
      end
      else
      begin
        if pl.special = 39 then
        begin // teleporters
          AM_drawMline(@l, WALLCOLORS + WALLRANGE div 2);
        end
        else if pl.flags and ML_SECRET <> 0 then // secret door
        begin
          if am_cheating <> 0 then
            AM_drawMline(@l, SECRETWALLCOLORS + lightlev)
          else
            AM_drawMline(@l, WALLCOLORS + lightlev);
        end
        else if pl.backsector.floorheight <> pl.frontsector.floorheight then
        begin
          AM_drawMline(@l, FDWALLCOLORS + lightlev); // floor level change
        end
        else if pl.backsector.ceilingheight <> pl.frontsector.ceilingheight then
        begin
          AM_drawMline(@l, CDWALLCOLORS + lightlev); // ceiling level change
        end
        else if am_cheating <> 0 then
        begin
          AM_drawMline(@l, TSWALLCOLORS + lightlev);
        end;
      end;
    end
    else if plr.powers[Ord(pw_allmap)] <> 0 then
    begin
      if pl.flags and LINE_NEVERSEE = 0 then
        AM_drawMline(@l, GRAYS + 3);
    end;
    inc(pl);
  end;
end;

//==============================================================================
//
// AM_drawLineCharacter
//
//==============================================================================
procedure AM_drawLineCharacter(lineguy: Pmline_tArray; lineguylines: integer;
  scale: fixed_t; angle: angle_t; color: integer;
  x: fixed_t; y: fixed_t);
var
  i: integer;
  l: mline_t;
  plg: Pmline_t;
begin
  if allowautomaprotate then
    angle := angle + ANG90 - plr.mo.angle;

  plg := @lineguy[0];
  for i := 0 to lineguylines - 1 do
  begin
    l.a.x := plg.a.x;
    l.a.y := plg.a.y;

    if scale <> 0 then
    begin
      l.a.x := FixedMul(scale, l.a.x);
      l.a.y := FixedMul(scale, l.a.y);
    end;

    if angle <> 0 then
      AM_rotate_dbl(@l.a.x, @l.a.y, angle, 0, 0);

    l.a.x := l.a.x + x;
    l.a.y := l.a.y + y;

    l.b.x := plg.b.x;
    l.b.y := plg.b.y;

    if scale <> 0 then
    begin
      l.b.x := FixedMul(scale, l.b.x);
      l.b.y := FixedMul(scale, l.b.y);
    end;

    if angle <> 0 then
      AM_rotate_dbl(@l.b.x, @l.b.y, angle, 0, 0);

    l.b.x := l.b.x + x;
    l.b.y := l.b.y + y;

    AM_drawMline(@l, color);
    inc(plg);
  end;
end;

//==============================================================================
//
// AM_drawPlayerTrace
//
//==============================================================================
procedure AM_drawPlayerTrace(const p: Pplayer_t; const color1: Integer);
const
  PTRACECOLORRANGE = 8;
var
  i: integer;
  x, y, x2, y2: fixed_t;
  trace: Pplayertrace_t;
  plrx, plry: fixed_t;
  plra: angle_t;
  traceline: mline_t;
  color: integer;
  colordiff: integer;
begin
  plrx := plr.mo.x div FRACTOMAPUNIT;
  plry := plr.mo.y div FRACTOMAPUNIT;
  plra := ANG90 - plr.mo.angle;

  x2 := plrx;
  y2 := plry;

  automaptraceplayer := ibetween(automaptraceplayer, 0, NUMPLAYERTRACEHISTORY - 1);
  for i := 1 to automaptraceplayer do
  begin
    trace := P_GetPlayerTraceAtPos(p, i);
    if trace <> nil then
    begin
      x := trace.x div FRACTOMAPUNIT;
      y := trace.y div FRACTOMAPUNIT;

      if allowautomaprotate then
        AM_rotate(@x, @y, plra, plrx, plry);

      if P_AproxDistance(x - x2, y - y2) < (128 * FRACUNIT) div FRACTOMAPUNIT then
      begin

        colordiff := (i + leveltime div 4) and (2 * PTRACECOLORRANGE - 1);
        if colordiff > PTRACECOLORRANGE then
          colordiff := 2 * PTRACECOLORRANGE - colordiff;

        color := color1 + colordiff;

        AM_drawLineCharacter
          (@traceplayer_guy, NUMTTRACEPLAYERLINES, 4 * FRACUNIT, trace.angle,
           color, x, y);

        traceline.a.x := x2 div 2 + x div 2;
        traceline.a.y := y2 div 2 + y div 2;
        traceline.b.x := x2;
        traceline.b.y := y2;
        AM_drawMline(@traceline, color);
      end;
      x2 := x;
      y2 := y;
    end;
  end;
end;

//==============================================================================
//
// AM_drawPlayers
//
//==============================================================================
procedure AM_drawPlayers;
const
  their_colors: array[0..MAXPLAYERS - 1] of integer = (GREENS, GRAYS, BROWNS, REDS);
var
  i: integer;
  p: Pplayer_t;
  their_color, color: integer;
  x, y: fixed_t;
begin
  if not netgame then
  begin
    if am_cheating <> 0 then
      AM_drawLineCharacter
        (@cheat_player_arrow, NUMCHEATPLYRLINES, 0,
        plr.mo.angle, WHITE, plr.mo.x div FRACTOMAPUNIT, plr.mo.y div FRACTOMAPUNIT)
    else
      AM_drawLineCharacter
        (@player_arrow, NUMPLYRLINES, 0, plr.mo.angle,
        WHITE, plr.mo.x div FRACTOMAPUNIT, plr.mo.y div FRACTOMAPUNIT);
    AM_drawPlayerTrace(plr, GREENS);
    exit;
  end;

  their_color := -1;
  for i := 0 to MAXPLAYERS - 1 do
  begin
    inc(their_color);
    p := @players[i];

    if (deathmatch <> 0) and not singledemo and (p <> plr) then
      continue;

    if not playeringame[i] then
      continue;

    if p.powers[Ord(pw_invisibility)] <> 0 then
      color := 246 // *close* to black
    else
      color := their_colors[their_color];

    x := p.mo.x;
    y := p.mo.y;

    if allowautomaprotate then
      AM_rotate(@x, @y, ANG90 - plr.mo.angle, plr.mo.x div FRACTOMAPUNIT, plr.mo.y div FRACTOMAPUNIT);

    AM_drawLineCharacter
      (@player_arrow, NUMPLYRLINES, 0, p.mo.angle,
       color, x div FRACTOMAPUNIT, y div FRACTOMAPUNIT);

    AM_drawPlayerTrace(p, color);
  end;
end;

//==============================================================================
//
// AM_drawThings
//
//==============================================================================
procedure AM_drawThings;
var
  i: integer;
  t: Pmobj_t;
  x, y: fixed_t;
  plrx, plry: fixed_t;
  plra: angle_t;
  color: integer;
begin
  plrx := plr.mo.x div FRACTOMAPUNIT;
  plry := plr.mo.y div FRACTOMAPUNIT;
  plra := ANG90 - plr.mo.angle;
  for i := 0 to numsectors - 1 do
  begin
    t := sectors[i].thinglist;
    while t <> nil do
    begin
      x := t.x div FRACTOMAPUNIT;
      y := t.y div FRACTOMAPUNIT;

      if allowautomaprotate then
        AM_rotate(@x, @y, plra, plrx, plry);

      if t.flags2_ex and MF2_EX_FRIEND <> 0 then
        color := WHITE
      else if t.flags and MF_SHOOTABLE <> 0 then
        color := THINGCOLORS + lightlev
      else if t.flags and MF_SPECIAL <> 0 then
        color := BLUES
      else
        color := GRAYS;

      if t.radius >= 40 * FRACUNIT then
        AM_drawLineCharacter
          (@triangle_guy, NUMTRIANGLEGUYLINES,
          16 * FRACUNIT, t.angle, color, x, y)
      else
        AM_drawLineCharacter
          (@thintriangle_guy, NUMTHINTRIANGLEGUYLINES,
          16 * FRACUNIT, t.angle, color, x, y);
      t := t.snext;
    end;
  end;
end;

//==============================================================================
//
// AM_drawMarks
//
//==============================================================================
procedure AM_drawMarks;
var
  i, fx, fy, w, h: integer;
begin
  for i := 0 to AM_NUMMARKPOINTS - 1 do
  begin
    if markpoints[i].x <> -1 then
    begin
      w := 5; // because something's wrong with the wad, i guess
      h := 6; // because something's wrong with the wad, i guess
      fx := markpoints[i].x;
      fy := markpoints[i].y;

      if allowautomaprotate then
        AM_rotate(@fx, @fy, ANG90 - plr.mo.angle, plr.mo.x div FRACTOMAPUNIT, plr.mo.y div FRACTOMAPUNIT);

      fx := CXMTOF(fx);
      fy := CYMTOF(fy);

      if (fx >= f_x) and (fx <= f_w - w) and (fy >= f_y) and (fy <= f_h - h) then
      {$IFDEF OPENGL}
        gld_AddAutomapPatch(fx, fy, marknumsid[i])
      {$ELSE}
        V_DrawPatchZoomed(fx, fy, SCN_FG, marknums[i], false, FRACUNIT * SCREENHEIGHT div 200);
      {$ENDIF}
    end;
  end;
end;

//==============================================================================
//
// AM_drawCrosshair
//
//==============================================================================
procedure AM_drawCrosshair(color: integer);
begin
  {$IFNDEF OPENGL}
  if videomode = vm32bit then
    fb32[(f_w * (f_h div 2)) + (f_w div 2)] := videopal[color] // single point for now
  else
    fb[(f_w * (f_h div 2)) + (f_w div 2)] := color; // single point for now
  {$ELSE}
  gld_AddAutomapLine(f_w div 2 - 1, f_h div 2, f_w div 2 + 1, f_h div 2,  videopal[color]);
  gld_AddAutomapLine(f_w div 2, f_h div 2 - 1, f_w div 2, f_h div 2 + 1,  videopal[color]);
  {$ENDIF}
end;

//==============================================================================
//
// AM_Drawer
//
//==============================================================================
procedure AM_Drawer;
begin
  if amstate = am_inactive then
    exit;

  if followplayer then
  begin
    m_w := FTOM(f_w);
    m_h := FTOM(f_h);
    m_x := plr.mo.x div FRACTOMAPUNIT - m_w div 2;
    m_y := plr.mo.y div FRACTOMAPUNIT - m_h div 2;
  end;

  if amstate = am_only then
  begin
    {$IFDEF OPENGL}
    gld_ClearAMBuffer(f_w, f_h, $080808);
    {$ELSE}
    AM_clearFB(aprox_black);
    {$ENDIF}
    if texturedautomap then
    begin
      AM_setSubSectorDrawFuncs;
      AM_drawSubSectors;
    end;
  end;
  if automapgrid then
    AM_drawGrid(GRIDCOLORS);
  AM_drawWalls;

  AM_drawMarks;

  AM_drawPlayers;
  if am_cheating = 2 then
    AM_drawThings;
  AM_drawCrosshair(XHAIRCOLORS);

  {$IFDEF OPENGL}
  gld_DrawAutomap;
  {$ENDIF}
end;

//==============================================================================
//
// AM_Init
//
//==============================================================================
procedure AM_Init;
var
  pl: Pmline_t;
begin
  AM_InitTextured;

  pl := @player_arrow[0];
  pl.a.x := -((8 * PLAYERRADIUS) div 7) + ((8 * PLAYERRADIUS) div 7) div 8;
  pl.a.y := 0;
  pl.b.x := ((8 * PLAYERRADIUS) div 7);
  pl.b.y := 0;

  inc(pl);
  pl.a.x := ((8 * PLAYERRADIUS) div 7);
  pl.a.y := 0;
  pl.b.x := ((8 * PLAYERRADIUS) div 7) - ((8 * PLAYERRADIUS) div 7) div 2;
  pl.b.y := ((8 * PLAYERRADIUS) div 7) div 4;

  inc(pl);
  pl.a.x := ((8 * PLAYERRADIUS) div 7);
  pl.a.y := 0;
  pl.b.x := ((8 * PLAYERRADIUS) div 7) - ((8 * PLAYERRADIUS) div 7) div 2;
  pl.b.y := -((8 * PLAYERRADIUS) div 7) div 4;

  inc(pl);
  pl.a.x := -((8 * PLAYERRADIUS) div 7) + ((8 * PLAYERRADIUS) div 7) div 8;
  pl.a.y := 0;
  pl.b.x := -((8 * PLAYERRADIUS) div 7) - ((8 * PLAYERRADIUS) div 7) div 8;
  pl.b.y := ((8 * PLAYERRADIUS) div 7) div 4;

  inc(pl);
  pl.a.x := -((8 * PLAYERRADIUS) div 7) + ((8 * PLAYERRADIUS) div 7) div 8;
  pl.a.y := 0;
  pl.b.x := -((8 * PLAYERRADIUS) div 7) - ((8 * PLAYERRADIUS) div 7) div 8;
  pl.b.y := -((8 * PLAYERRADIUS) div 7) div 4;

  inc(pl);
  pl.a.x := -((8 * PLAYERRADIUS) div 7) + 3 * ((8 * PLAYERRADIUS) div 7) div 8;
  pl.a.y := 0;
  pl.b.x := -((8 * PLAYERRADIUS) div 7) + ((8 * PLAYERRADIUS) div 7) div 8;
  pl.b.y := ((8 * PLAYERRADIUS) div 7) div 4;

  inc(pl);
  pl.a.x := -((8 * PLAYERRADIUS) div 7) + 3 * ((8 * PLAYERRADIUS) div 7) div 8;
  pl.a.y := 0;
  pl.b.x := -((8 * PLAYERRADIUS) div 7) + ((8 * PLAYERRADIUS) div 7) div 8;
  pl.b.y := -((8 * PLAYERRADIUS) div 7) div 4;

////////////////////////////////////////////////////////////////////////////////

  pl := @cheat_player_arrow[0];
  pl.a.x := -((8 * PLAYERRADIUS) div 7) + ((8 * PLAYERRADIUS) div 7) div 8;
  pl.a.y := 0;
  pl.b.x := ((8 * PLAYERRADIUS) div 7);
  pl.b.y := 0;

  inc(pl);
  pl.a.x := ((8 * PLAYERRADIUS) div 7);
  pl.a.y := 0;
  pl.b.x := ((8 * PLAYERRADIUS) div 7) - ((8 * PLAYERRADIUS) div 7) div 2;
  pl.b.y := ((8 * PLAYERRADIUS) div 7) div 6;

  inc(pl);
  pl.a.x := ((8 * PLAYERRADIUS) div 7);
  pl.a.y := 0;
  pl.b.x := ((8 * PLAYERRADIUS) div 7) - ((8 * PLAYERRADIUS) div 7) div 2;
  pl.b.y := -((8 * PLAYERRADIUS) div 7) div 6;

  inc(pl);
  pl.a.x := -((8 * PLAYERRADIUS) div 7) + ((8 * PLAYERRADIUS) div 7) div 8;
  pl.a.y := 0;
  pl.b.x := -((8 * PLAYERRADIUS) div 7) - ((8 * PLAYERRADIUS) div 7) div 8;
  pl.b.y := ((8 * PLAYERRADIUS) div 7) div 6;

  inc(pl);
  pl.a.x := -((8 * PLAYERRADIUS) div 7) + ((8 * PLAYERRADIUS) div 7) div 8;
  pl.a.y := 0;
  pl.b.x := -((8 * PLAYERRADIUS) div 7) - ((8 * PLAYERRADIUS) div 7) div 8;
  pl.b.y := -((8 * PLAYERRADIUS) div 7) div 6;

  inc(pl);
  pl.a.x := -((8 * PLAYERRADIUS) div 7) + 3 * ((8 * PLAYERRADIUS) div 7) div 8;
  pl.a.y := 0;
  pl.b.x := -((8 * PLAYERRADIUS) div 7) + ((8 * PLAYERRADIUS) div 7) div 8;
  pl.b.y := ((8 * PLAYERRADIUS) div 7) div 6;

  inc(pl);
  pl.a.x := -((8 * PLAYERRADIUS) div 7) + 3 * ((8 * PLAYERRADIUS) div 7) div 8;
  pl.a.y := 0;
  pl.b.x := -((8 * PLAYERRADIUS) div 7) + ((8 * PLAYERRADIUS) div 7) div 8;
  pl.b.y := -((8 * PLAYERRADIUS) div 7) div 6;

  inc(pl);
  pl.a.x := -((8 * PLAYERRADIUS) div 7) div 2;
  pl.a.y := 0;
  pl.b.x := -((8 * PLAYERRADIUS) div 7) div 2;
  pl.b.y := -((8 * PLAYERRADIUS) div 7) div 6;

  inc(pl);
  pl.a.x := -((8 * PLAYERRADIUS) div 7) div 2;
  pl.a.y := -((8 * PLAYERRADIUS) div 7) div 6;
  pl.b.x := -((8 * PLAYERRADIUS) div 7) div 2 + ((8 * PLAYERRADIUS) div 7) div 6;
  pl.b.y := -((8 * PLAYERRADIUS) div 7) div 6;

  inc(pl);
  pl.a.x := -((8 * PLAYERRADIUS) div 7) div 2 + ((8 * PLAYERRADIUS) div 7) div 6;
  pl.a.y := -((8 * PLAYERRADIUS) div 7) div 6;
  pl.b.x := -((8 * PLAYERRADIUS) div 7) div 2 + ((8 * PLAYERRADIUS) div 7) div 6;
  pl.b.y := ((8 * PLAYERRADIUS) div 7) div 4;

  inc(pl);
  pl.a.x := -((8 * PLAYERRADIUS) div 7) div 6;
  pl.a.y := 0;
  pl.b.x := -((8 * PLAYERRADIUS) div 7) div 6;
  pl.b.y := -((8 * PLAYERRADIUS) div 7) div 6;

  inc(pl);
  pl.a.x := -((8 * PLAYERRADIUS) div 7) div 6;
  pl.a.y := -((8 * PLAYERRADIUS) div 7) div 6;
  pl.b.x := 0;
  pl.b.y := -((8 * PLAYERRADIUS) div 7) div 6;

  inc(pl);
  pl.a.x := 0;
  pl.a.y := -((8 * PLAYERRADIUS) div 7) div 6;
  pl.b.x := 0;
  pl.b.y := ((8 * PLAYERRADIUS) div 7) div 4;

  inc(pl);
  pl.a.x := ((8 * PLAYERRADIUS) div 7) div 6;
  pl.a.y := ((8 * PLAYERRADIUS) div 7) div 4;
  pl.b.x := ((8 * PLAYERRADIUS) div 7) div 6;
  pl.b.y := -((8 * PLAYERRADIUS) div 7) div 7;

  inc(pl);
  pl.a.x := ((8 * PLAYERRADIUS) div 7) div 6;
  pl.a.y := -((8 * PLAYERRADIUS) div 7) div 7;
  pl.b.x := ((8 * PLAYERRADIUS) div 7) div 6 + ((8 * PLAYERRADIUS) div 7) div 32;
  pl.b.y := -((8 * PLAYERRADIUS) div 7) div 7 - ((8 * PLAYERRADIUS) div 7) div 32;

  inc(pl);
  pl.a.x := ((8 * PLAYERRADIUS) div 7) div 6 + ((8 * PLAYERRADIUS) div 7) div 32;
  pl.a.y := -((8 * PLAYERRADIUS) div 7) div 7 - ((8 * PLAYERRADIUS) div 7) div 32;
  pl.b.x := ((8 * PLAYERRADIUS) div 7) div 6 + ((8 * PLAYERRADIUS) div 7) div 10;
  pl.b.y := -((8 * PLAYERRADIUS) div 7) div 7;

////////////////////////////////////////////////////////////////////////////////

  pl := @triangle_guy[0];
  pl.a.x := round(-0.867 * MAPUNIT);
  pl.a.y := round(-0.5 * MAPUNIT);
  pl.b.x := round(0.867 * MAPUNIT);
  pl.b.y := round(-0.5 * MAPUNIT);

  inc(pl);
  pl.a.x := round(0.867 * MAPUNIT);
  pl.a.y := round(-0.5 * MAPUNIT);
  pl.b.x := 0;
  pl.b.y := MAPUNIT;

  inc(pl);
  pl.a.x := 0;
  pl.a.y := MAPUNIT;
  pl.b.x := round(-0.867 * MAPUNIT);
  pl.b.y := round(-0.5 * MAPUNIT);

////////////////////////////////////////////////////////////////////////////////

  pl := @thintriangle_guy[0];
  pl.a.x := round(-0.5 * MAPUNIT);
  pl.a.y := round(-0.7 * MAPUNIT);
  pl.b.x := MAPUNIT;
  pl.b.y := 0;

  inc(pl);
  pl.a.x := MAPUNIT;
  pl.a.y := 0;
  pl.b.x := round(-0.5 * MAPUNIT);
  pl.b.y := round(0.7 * MAPUNIT);

  inc(pl);
  pl.a.x := round(-0.5 * MAPUNIT);
  pl.a.y := round(0.7 * MAPUNIT);
  pl.b.x := round(-0.5 * MAPUNIT);
  pl.b.y := round(-0.7 * MAPUNIT);

////////////////////////////////////////////////////////////////////////////////

  pl := @traceplayer_guy[0];
  pl.a.x := 0;
  pl.a.y := round(-0.7 * MAPUNIT);
  pl.b.x := round(0.7 * MAPUNIT);
  pl.b.y := 0;

  inc(pl);
  pl.a.x := 0;
  pl.a.y := round(0.7 * MAPUNIT);
  pl.b.x := round(0.7 * MAPUNIT);
  pl.b.y := 0;

  inc(pl);
  pl.a.x := 0;
  pl.a.y := 0;
  pl.b.x := round(0.7 * MAPUNIT);
  pl.b.y := 0;

////////////////////////////////////////////////////////////////////////////////
  cheat_amap.sequence := get_cheatseq_string(cheat_amap_seq);
  cheat_amap.p := get_cheatseq_string(0);

////////////////////////////////////////////////////////////////////////////////
  ZeroMemory(@st_notify_AM_initVariables, SizeOf(st_notify_AM_initVariables));
  st_notify_AM_initVariables._type := ev_keyup;
  st_notify_AM_initVariables.data1 := AM_MSGENTERED;

////////////////////////////////////////////////////////////////////////////////
  ZeroMemory(@st_notify_AM_Stop, SizeOf(st_notify_AM_Stop));
  st_notify_AM_Stop._type := ev_keyup;
  st_notify_AM_Stop.data1 := AM_MSGEXITED;

  C_AddCmd('allowautomapoverlay', @CmdAllowautomapoverlay);
end;

//==============================================================================
//
// AM_ShutDown
//
//==============================================================================
procedure AM_ShutDown;
begin
  AM_ShutDownTextured;
end;

end.

