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
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit m_defs;

interface

uses
  am_map,
  c_con,
  doomdef,
  h_strings,
  d_main,
  g_game,
  hu_stuff,
  p_mobj_h,
  p_setup,
  p_adjust,
  p_obituaries,
  i_system,
  i_mp3,
  i_midi,
  i_music,
  i_sound,
  i_threads,
{$IFDEF OPENGL}
  gl_main,
  gl_defs,
  gl_models,
  gl_voxels,
  gl_lightmaps,
  gl_shadows,
{$ELSE}
  i_video,
  r_batchcolumn,
  r_wall8,
  r_wall32,
  r_scale,
  r_voxels,
  r_softlights,
{$ENDIF}
  e_endoom,
  m_menu,
  m_misc,
  r_aspect,
  r_defs,
  r_main,
  r_hires,
  r_lights,
  r_intrpl,
{$IFNDEF OPENGL}
  r_fake3d,
  r_slopes, // JVAL: Slopes
{$ENDIF}
  r_camera,
  r_draw,
{$IFNDEF OPENGL}
  r_segs,
{$ENDIF}
  r_dynlights,
  s_sound,
  sc_actordef,
  t_main,
  t_png,
  m_sshot_jpg,
  vx_voxelsprite,
  v_video;

const
  DFS_NEVER = 0;
  DFS_SINGLEPLAYER = 1;
  DFS_NETWORK = 2;
  DFS_ALWAYS = 3;

var
{$IFDEF OPENGL}
// Stub variables
  soft_SCREENWIDTH,
  soft_SCREENHEIGHT: integer;
  usefake3d: boolean;
  optimizedthingsrendering: boolean;
  force_numwallrenderingthreads_8bit: integer;
  force_numwallrenderingthreads_32bit: integer;
  precisescalefromglobalangle: boolean;
  preciseslopedrawing: boolean; // JVAL: Slopes
  r_drawvoxels: boolean;
  showfullhdlogo: boolean = false;
  soft_fullscreen: integer = 1;
  r_uselightmaps: boolean = true;
  r_lightmapfadeoutfunc: integer = 0;
  lightmapcolorintensity: integer = 128;
  lightwidthfactor: integer = 5;
  r_bltasync: boolean = true;
  r_blitmultiplier: integer = 1;
  r_lightmaponmasked: boolean = true;
  r_lightmaponemitters: boolean = false;
{$ELSE}
  tran_filter_pct: integer;
  use_fog: boolean;
  fog_density: integer;
  use_white_fog: boolean;
  white_fog_density: integer;
  gl_nearclip: integer;
  gl_tex_filter_string: string;
  gl_texture_filter_anisotropic: boolean;
  gl_drawsky: boolean;
  gl_stencilsky: boolean;
  gl_screensync: boolean;
  gl_linear_hud: boolean;
  gl_add_all_lines: boolean;
  gl_SCREENWIDTH,
  gl_SCREENHEIGHT: integer;
  gl_drawmodels: boolean;
  gl_drawvoxels: boolean;
  gl_smoothmodelmovement: boolean;
  gl_precachemodeltextures: boolean;
  gl_uselightmaps: boolean;
  gl_drawshadows: boolean;
  gl_renderwireframe: boolean;
  gl_no_glfinish_hack: boolean = true;
  gl_old_ripple_effect: Boolean = false;
  gl_fullscreen: boolean = true;
  vx_maxoptimizerpasscount: integer;
{$ENDIF}

type
  ttype_t = (tString, tInteger, tBoolean, tGroup);

  default_t = record
    name: string;
    location: pointer;
    oldlocation: pointer;
    setable: byte;
    defaultsvalue: string;
    defaultivalue: integer;
    defaultbvalue: boolean;
    _type: ttype_t;
  end;
  Pdefault_t = ^default_t;

const
  NUMDEFAULTS = 218;

// JVAL
// Note: All setable defaults must be in lowercase, don't ask why. Just do it. :)
  defaults: array[0..NUMDEFAULTS - 1] of default_t = (
    (name: 'Display';
     location: nil;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tGroup),

    (name: 'soft_screenwidth';
     location: @{$IFDEF OPENGL}soft_SCREENWIDTH{$ELSE}SCREENWIDTH{$ENDIF};
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: -1;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'soft_screenheight';
     location: @{$IFDEF OPENGL}soft_SCREENHEIGHT{$ELSE}SCREENHEIGHT{$ENDIF};
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: -1;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'soft_fullscreen';
     location: {$IFDEF OPENGL}@soft_fullscreen{$ELSE}@fullscreen{$ENDIF};
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tInteger),

    (name: 'gl_screenwidth';
     location: @{$IFDEF OPENGL}SCREENWIDTH{$ELSE}gl_SCREENWIDTH{$ENDIF};
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: -1;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'gl_screenheight';
     location: @{$IFDEF OPENGL}SCREENHEIGHT{$ELSE}gl_SCREENHEIGHT{$ENDIF};
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: -1;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'gl_fullscreen';
     location: {$IFDEF OPENGL}@fullscreen{$ELSE}@gl_fullscreen{$ENDIF};
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'interpolate';
     location: @interpolate;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'interpolateprecise';
     location: @interpolateprecise;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'interpolateoncapped';
     location: @interpolateoncapped;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'interpolatepolyobjs';
     location: @interpolatepolyobjs;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'interpolatereducelag';
     location: @interpolatereducelag;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'fixstallhack';
     location: @fixstallhack;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: '32bittexturepaletteeffects';
     location: @dc_32bittexturepaletteeffects;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'zaxisshift';
     location: @zaxisshift;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'usefake3d';
     location: @usefake3d;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'chasecamera';
     location: @chasecamera;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'chasecamera_viewxy';
     location: @chasecamera_viewxy;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 64;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'chasecamera_viewz';
     location: @chasecamera_viewz;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 16;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'drawfps';
     location: @drawfps;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'drawcrosshair';
     location: @drawcrosshair;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'shademenubackground';
     location: @shademenubackground;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'menubackgroundflat';
     location: @menubackgroundflat;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: DEFMENUBACKGROUNDFLAT;
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tString),

    (name: 'displaydiskbusyicon';
     location: @displaydiskbusyicon;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'displayendscreen';
     location: @displayendscreen;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'screenblocks';
     location: @screenblocks;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 9;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'detaillevel';
     location: @detailLevel;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: DL_NORMAL;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'allowlowdetails';
     location: @allowlowdetails;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'allowhidetails';
     location: @allowhidetails;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'optimizedcolumnrendering';
     location: @optimizedcolumnrendering;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'optimizedthingsrendering';
     location: @optimizedthingsrendering;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'uselightboost';
     location: @uselightboost;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'lightboostfactor';
     location: @lightboostfactor;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 192;
     defaultbvalue: true;
     _type: tInteger),

    (name: 'usegamma';
     location: @usegamma;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'forcecolormaps';
     location: @forcecolormaps;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'use32bitfuzzeffect';
     location: @use32bitfuzzeffect;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'usetransparentsprites';
     location: @usetransparentsprites;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'diher8bittransparency';
     location: @diher8bittransparency;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'autoadjustmissingtextures';
     location: @autoadjustmissingtextures;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'widescreensupport';
     location: @widescreensupport;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'intermissionstretch';
     location: @intermissionstretch;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'excludewidescreenplayersprites';
     location: @excludewidescreenplayersprites;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'forcedaspect';
     location: @forcedaspectstr;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '0.00';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tString),

    (name: 'precisescalefromglobalangle';
     location: @precisescalefromglobalangle;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '0.00';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'r_uselightmaps';
     location: @r_uselightmaps;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'r_lightmaponmasked';
     location: @r_lightmaponmasked;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'r_lightmapfadeoutfunc';
     location: @r_lightmapfadeoutfunc;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'r_lightmaponemitters';
     location: @r_lightmaponemitters;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'lightmapcolorintensity';
     location: @lightmapcolorintensity;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 128;
     defaultbvalue: true;
     _type: tInteger),

    (name: 'lightwidthfactor';
     location: @lightwidthfactor;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 5;
     defaultbvalue: true;
     _type: tInteger),

    (name: 'gldefs_as_lightdef';
     location: @gldefs_as_lightdef;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'r_bltasync';
     location: @r_bltasync;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'r_blitmultiplier';
     location: @r_blitmultiplier;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tInteger),

     // JVAL: Slopes
    (name: 'preciseslopedrawing';
     location: @preciseslopedrawing;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '0.00';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'r_fakecontrast';
     location: {$IFDEF OPENGL}@gl_fakecontrast{$ELSE}@r_fakecontrast{$ENDIF};
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '0.00';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'OpenGL';
     location: nil;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tGroup),

    (name: 'default_transparent_filter_percent';
     location: @tran_filter_pct;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '66';
     defaultivalue: 66;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'use_fog';
     location: @use_fog;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'fog_density';
     location: @fog_density;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '100';
     defaultivalue: 100;
     defaultbvalue: true;
     _type: tInteger),

    (name: 'use_white_fog';
     location: @use_white_fog;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'white_fog_density';
     location: @fog_density;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '200';
     defaultivalue: 200;
     defaultbvalue: true;
     _type: tInteger),

    (name: 'gl_nearclip';
     location: @gl_nearclip;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '5';
     defaultivalue: 5;
     defaultbvalue: true;
     _type: tInteger),

    (name: 'gl_tex_filter';
     location: @gl_tex_filter_string;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: 'GL_LINEAR';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tString),

    (name: 'gl_texture_filter_anisotropic';
     location: @gl_texture_filter_anisotropic;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'gl_renderwireframe';
     location: @gl_renderwireframe;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'gl_no_glfinish_hack';
     location: @gl_no_glfinish_hack;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'gl_old_ripple_effect';
     location: @gl_old_ripple_effect;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'gl_drawsky';
     location: @gl_drawsky;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'gl_stencilsky';
     location: @gl_stencilsky;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'gl_drawmodels';
     location: @gl_drawmodels;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'gl_smoothmodelmovement';
     location: @gl_smoothmodelmovement;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'gl_precachemodeltextures';
     location: @gl_precachemodeltextures;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'gl_uselightmaps';
     location: @gl_uselightmaps;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'gl_drawshadows';
     location: @gl_drawshadows;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'gl_screensync';
     location: @gl_screensync;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'gl_linear_hud';
     location: @gl_linear_hud;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'gl_add_all_lines';
     location: @gl_add_all_lines;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'useglnodesifavailable';
     location: @useglnodesifavailable;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'autoloadgwafiles';
     location: @autoloadgwafiles;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'Voxels';
     location: nil;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tGroup),

    (name: 'gl_drawvoxels';
     location: @gl_drawvoxels;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'r_drawvoxels';
     location: @r_drawvoxels;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'vx_maxoptimizerpasscount';
     location: @vx_maxoptimizerpasscount;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tInteger),

    (name: 'r_generatespritesfromvoxels';
     location: @r_generatespritesfromvoxels;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'Automap';
     location: nil;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tGroup),

    (name: 'allowautomapoverlay';
     location: @allowautomapoverlay;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'allowautomaprotate';
     location: @allowautomaprotate;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'texturedautomap';
     location: @texturedautomap;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'automapgrid';
     location: @automapgrid;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'automaptraceplayer';
     location: @automaptraceplayer;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 64;
     defaultbvalue: false;
     _type: tInteger),

     // Textures
    (name: 'Textures';
     location: nil;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tGroup),

    (name: 'useexternaltextures';
     location: @useexternaltextures;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'preferetexturesnamesingamedirectory';
     location: @preferetexturesnamesingamedirectory;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'extremeflatfiltering';
     location: @extremeflatfiltering;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'pngtransparentcolor';
     location: @pngtransparentcolor;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: $0;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'pngtransparentcolor2';
     location: @pngtransparentcolor2;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: $0;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'assumecommontranspantcolors';
     location: @assumecommontranspantcolors;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

     // Compatibility
    (name: 'Compatibility';
     location: nil;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tGroup),

    (name: 'compatibilitymode';
     location: @compatibilitymode;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'keepcheatsinplayerreborn';
     location: @keepcheatsinplayerreborn;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'allowplayerjumps';
     location: @allowplayerjumps;
     oldlocation: nil;
     setable: DFS_SINGLEPLAYER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

     // JVAL: 20211101 - Crouch
    (name: 'allowplayercrouch';
     location: @allowplayercrouch;
     oldlocation: nil;
     setable: DFS_SINGLEPLAYER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'decorate_as_actordef';
     location: @decorate_as_actordef;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'defaultskill';
     location: @defaultskill;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 2;
     defaultbvalue: true;
     _type: tInteger),

    (name: 'UserInterface';
     location: nil;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tGroup),

    (name: 'showmessageboxonmodified';
     location: @showmessageboxonmodified;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'showfullhdlogo';
     location: @showfullhdlogo;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

     // Navigation
    (name: 'Controls';
     location: nil;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tGroup),

    (name: 'autorunmode';
     location: @autorunmode;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'Keyboard';
     location: nil;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tGroup),

    (name: 'key_right';
     location: @key_right;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: KEY_RIGHTARROW;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_left';
     location: @key_left;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: KEY_LEFTARROW;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_up';
     location: @key_up;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: KEY_UPARROW;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_down';
     location: @key_down;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: KEY_DOWNARROW;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_strafeleft';
     location: @key_strafeleft;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord(',');
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_straferight';
     location: @key_straferight;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord('.');
     defaultbvalue: false;
     _type: tInteger),

     // JVAL Jump
    (name: 'key_jump';
     location: @key_jump;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord('a');
     defaultbvalue: false;
     _type: tInteger),

     // JVAL: 20211101 - Crouch
    (name: 'key_crouch';
     location: @key_crouch;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord('z');
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_fire';
     location: @key_fire;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: KEY_RCTRL;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_use';
     location: @key_use;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord(' ');
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_strafe';
     location: @key_strafe;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: KEY_RALT;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_speed';
     location: @key_speed;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: KEY_RSHIFT;
     defaultbvalue: false;
     _type: tInteger),

     // JVAL Look UP and DOWN using z-axis shift
    (name: 'key_lookup';
     location: @key_lookup;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: KEY_PAGEDOWN;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_lookdown';
     location: @key_lookdown;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: KEY_DELETE;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_lookcenter';
     location: @key_lookcenter;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: KEY_INS;
     defaultbvalue: false;

     _type: tInteger),

     // Fly movement
    (name: 'key_flyup';
     location: @key_flyup;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: KEY_PAGEUP;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_flydown';
     location: @key_flydown;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: KEY_HOME;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_flycenter';
     location: @key_flycenter;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: KEY_END;
     defaultbvalue: false;
     _type: tInteger),

     // JVAL Look LEFT/RIGHT
    (name: 'key_lookright';
     location: @key_lookright;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord('*');
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_lookleft';
     location: @key_lookleft;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord('/');
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_lookforward';
     location: @key_lookforward;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: KEY_ENTER;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_invleft';
     location: @key_invleft;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord('[');
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_invright';
     location: @key_invright;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord(']');
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_useartifact';
     location: @key_useartifact;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: KEY_ENTER;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_weapon0';
     location: @key_weapon0;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord('1');
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_weapon1';
     location: @key_weapon1;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord('2');
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_weapon2';
     location: @key_weapon2;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord('3');
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_weapon3';
     location: @key_weapon3;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord('4');
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_weapon4';
     location: @key_weapon4;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord('5');
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_weapon5';
     location: @key_weapon5;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord('6');
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_weapon6';
     location: @key_weapon6;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord('7');
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_weapon7';
     location: @key_weapon7;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord('8');
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_am_gobigkey';
     location: @AM_GOBIGKEY;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord('0');
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_am_followkey';
     location: @AM_FOLLOWKEY;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord('f');
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_am_gridkey';
     location: @AM_GRIDKEY;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord('g');
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_am_rotatekey';
     location: @AM_ROTATEKEY;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord('r');
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_am_texturedautomap';
     location: @AM_TEXTUREDAUTOMAP;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord('t');
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_am_markkey';
     location: @AM_MARKKEY;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord('m');
     defaultbvalue: false;
     _type: tInteger),

    (name: 'key_am_clearmarkkey';
     location: @AM_CLEARMARKKEY;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: Ord('c');
     defaultbvalue: false;
     _type: tInteger),

     // Mouse
    (name: 'Mouse';
     location: nil;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tGroup),

    (name: 'use_mouse';
     location: @usemouse;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'mouse_sensitivity';
     location: @mouseSensitivity;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 5;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'mouse_sensitivityx';
     location: @mouseSensitivityX;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 5;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'mouse_sensitivityy';
     location: @mouseSensitivityY;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 5;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'invertmouselook';
     location: @invertmouselook;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'invertmouseturn';
     location: @invertmouseturn;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'mouseb_fire';
     location: @mousebfire;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'mouseb_strafe';
     location: @mousebstrafe;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'mouseb_forward';
     location: @mousebforward;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 2;
     defaultbvalue: false;
     _type: tInteger),

     // Joystick
    (name: 'Joystick';
     location: nil;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tGroup),

    (name: 'use_joystick';
     location: @usejoystick;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'joyb_fire';
     location: @joybfire;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'joyb_strafe';
     location: @joybstrafe;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'joyb_use';
     location: @joybuse;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 3;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'joyb_speed';
     location: @joybspeed;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 2;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'joyb_jump';
     location: @joybjump;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 4;
     defaultbvalue: false;
     _type: tInteger),

     // JVAL: 20211101 - Crouch
    (name: 'joyb_crouch';
     location: @joybcrouch;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 5;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'joyb_lookleft';
     location: @joyblleft;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 6;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'joyb_lookright';
     location: @joyblright;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 7;
     defaultbvalue: false;
     _type: tInteger),

     // Sound
    (name: 'Sound';
     location: nil;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tGroup),

    (name: 'snd_channels';
     location: @numChannels;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 32;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'sfx_volume';
     location: @snd_SfxVolume;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 15;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'music_volume';
     location: @snd_MusicVolume;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 8;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'miditempo';
     location: @miditempo;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 160;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'snd_uselegacymidiplayer';
     location: @snd_uselegacymidiplayer;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 2;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'usemp3';
     location: @usemp3;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'preferemp3namesingamedirectory';
     location: @preferemp3namesingamedirectory;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'useexternalwav';
     location: @useexternalwav;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'preferewavnamesingamedirectory';
     location: @preferewavnamesingamedirectory;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'full_sounds';
     location: @full_sounds;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

     // Console
    (name: 'Console';
     location: nil;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tGroup),

    (name: 'console_colormap';
     location: @ConsoleColormap;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: NUMCOLORMAPS div 2;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'mirror_stdout';
     location: @mirror_stdout;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'mirrorjpgsshot';
     location: @mirrorjpgsshot;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'screenshotformat';
     location: @screenshotformat;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: 'PNG';
     defaultivalue: 1;
     defaultbvalue: false;
     _type: tString),

    (name: 'keepsavegamename';
     location: @keepsavegamename;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

     // Messages
    (name: 'show_messages';
     location: @showMessages;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tInteger),

    (name: 'show_obituaries';
     location: @show_obituaries;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'Chat strings';
     location: nil;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tGroup),

    (name: 'chatmacro0';
     location: @chat_macros[0];
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: HUSTR_CHATMACRO0;
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tString),

    (name: 'chatmacro1';
     location: @chat_macros[1];
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: HUSTR_CHATMACRO1;
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tString),

    (name: 'chatmacro2';
     location: @chat_macros[2];
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: HUSTR_CHATMACRO2;
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tString),

    (name: 'chatmacro3';
     location: @chat_macros[3];
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: HUSTR_CHATMACRO3;
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tString),

    (name: 'chatmacro4';
     location: @chat_macros[4];
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: HUSTR_CHATMACRO4;
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tString),

    (name: 'chatmacro5';
     location: @chat_macros[5];
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: HUSTR_CHATMACRO5;
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tString),

    (name: 'chatmacro6';
     location: @chat_macros[6];
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: HUSTR_CHATMACRO6;
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tString),

    (name: 'chatmacro7';
     location: @chat_macros[7];
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: HUSTR_CHATMACRO7;
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tString),

    (name: 'chatmacro8';
     location: @chat_macros[8];
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: HUSTR_CHATMACRO8;
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tString),

    (name: 'chatmacro9';
     location: @chat_macros[9];
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: HUSTR_CHATMACRO9;
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tString),

    (name: 'Randomizer';
     location: nil;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tGroup),

    (name: 'spawnrandommonsters';
     location: @spawnrandommonsters;
     oldlocation: nil;
     setable: DFS_SINGLEPLAYER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'Advanced';
     location: nil;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tGroup),

    (name: 'safemode';
     location: @safemode;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'usemmx';
     location: @usemmx;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'usemultithread';
     location: @usemultithread;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'dotestactivethreads';
     location: @dotestactivethreads;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'force_numwallrenderingthreads_8bit';
     location: @force_numwallrenderingthreads_8bit;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'force_numwallrenderingthreads_32bit';
     location: @force_numwallrenderingthreads_32bit;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'criticalcpupriority';
     location: @criticalcpupriority;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tBoolean),

    (name: 'zonesize';
     location: @zonesize;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 32;
     defaultbvalue: false;
     _type: tInteger),

    (name: 'Paths';
     location: nil;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tGroup),

    (name: 'searchdoomwaddir';
     location: @searchdoomwaddir;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'searchdoomwadpath';
     location: @searchdoomwadpath;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'searchsteampaths';
     location: @searchsteampaths;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 1;
     defaultbvalue: true;
     _type: tBoolean),

    (name: 'additionalwadpaths';
     location: @additionalwadpaths;
     oldlocation: nil;
     setable: DFS_ALWAYS;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tString),

    (name: 'Autoload';
     location: nil;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tGroup),

    (name: 'wads_autoload';
     location: @wads_autoload;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tString),

    (name: 'paks_autoload';
     location: @paks_autoload;
     oldlocation: nil;
     setable: DFS_NEVER;
     defaultsvalue: '';
     defaultivalue: 0;
     defaultbvalue: false;
     _type: tString)
  );

implementation

end.

