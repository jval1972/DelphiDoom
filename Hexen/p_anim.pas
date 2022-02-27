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

unit p_anim;

interface

uses
  d_delphi,
  r_defs;

//==============================================================================
//
// P_ForceLightning
//
//==============================================================================
procedure P_ForceLightning(const tics: integer = 0);

//==============================================================================
//
// P_InitFTAnims
//
//==============================================================================
procedure P_InitFTAnims;

//==============================================================================
//
// P_AnimateSurfaces
//
//==============================================================================
procedure P_AnimateSurfaces;

//==============================================================================
//
// P_InitLightning
//
//==============================================================================
procedure P_InitLightning;

//==============================================================================
//
// P_InitAnimations
//
//==============================================================================
procedure P_InitAnimations;

//==============================================================================
//
// P_ShutDownAnimations
//
//==============================================================================
procedure P_ShutDownAnimations;

const
  MAXLINEANIMS = 1024; // JVAL original was 64

var
  numlinespecials: smallint;
  linespeciallist: array[0..MAXLINEANIMS - 1] of Pline_t;
  animatedflatslist: TDNumberList;
  animatedtextureslist: TDNumberList;

implementation

uses
  g_game,
  i_system,
  m_fixed,
  m_rnd,
  p_mapinfo,
  p_setup,
  p_tick,
  r_data,
  r_sky,
  s_sound,
  sounddata,
  sc_engine,
  doomdef,
  w_wad,
  z_zone;

const
  ANIM_SCRIPT_NAME = 'ANIMDEFS';
  MAX_ANIM_DEFS = 20;
  MAX_FRAME_DEFS = 96;
  ANIM_FLAT = 0;
  ANIM_TEXTURE = 1;
  SCI_FLAT = 'flat';
  SCI_TEXTURE = 'texture';
  SCI_PIC = 'pic';
  SCI_TICS = 'tics';
  SCI_RAND = 'rand';

  LIGHTNING_SPECIAL = 198;
  LIGHTNING_SPECIAL2 = 199;
  SKYCHANGE_SPECIAL = 200;

// TYPES -------------------------------------------------------------------

type
  frameDef_t = record
    index: integer;
    tics: integer;
  end;
  PframeDef_t = ^frameDef_t;

  animDef_t = record
    _type: integer;
    index: integer;
    tics: integer;
    currentFrameDef: integer;
    startFrameDef: integer;
    endFrameDef: integer;
  end;
  PanimDef_t = ^animDef_t;

// PRIVATE DATA DEFINITIONS ------------------------------------------------
var
  AnimDefs: array[0..MAX_ANIM_DEFS - 1] of animDef_t;
  FrameDefs: array[0..MAX_FRAME_DEFS - 1] of frameDef_t;
  AnimDefCount: integer;
  LevelHasLightning: boolean;
  NextLightningFlash: integer;
  LightningFlash: integer;
  LightningLightLevels: PIntegerArray;

// CODE --------------------------------------------------------------------

//==============================================================================
//
// P_LightningFlash
//
//==============================================================================
procedure P_LightningFlash;
var
  i: integer;
  tempSec: Psector_t;
  tempLight: PInteger;
  foundSec: boolean;
  flashLight: integer;
begin
  if LightningFlash <> 0 then
  begin
    dec(LightningFlash);
    if LightningFlash <> 0 then
    begin
      tempLight := @LightningLightLevels[0];
      tempSec := @sectors[0];
      for i := 0 to numsectors - 1 do
      begin
        if (tempSec.ceilingpic = skyflatnum) or
           (tempSec.special = LIGHTNING_SPECIAL) or
           (tempSec.special = LIGHTNING_SPECIAL2) then
        begin
          if tempLight^ < tempSec.lightlevel - 4 then
            tempSec.lightlevel := tempSec.lightlevel - 4;
          inc(tempLight);
        end;
        inc(tempSec);
      end;
    end
    else
    begin // remove the alternate lightning flash special
      tempLight := @LightningLightLevels[0];
      tempSec := @sectors[0];
      for i := 0 to numsectors - 1 do
      begin
        if (tempSec.ceilingpic = skyflatnum) or
           (tempSec.special = LIGHTNING_SPECIAL) or
           (tempSec.special = LIGHTNING_SPECIAL2) then
        begin
          tempSec.lightlevel := tempLight^;
          inc(tempLight);
        end;
        inc(tempSec);
      end;
      SkyTexture := P_GetMapSky1Texture(gamemap);
    end;
    exit;
  end;
  LightningFlash := (P_Random and 7) + 8;
  flashLight := 200 + (P_Random and 31);
  tempSec := @sectors[0];
  tempLight := @LightningLightLevels[0];
  foundSec := false;
  for i := 0 to numsectors - 1 do
  begin
    if (tempSec.ceilingpic = skyflatnum) or
       (tempSec.special = LIGHTNING_SPECIAL) or
       (tempSec.special = LIGHTNING_SPECIAL2) then
    begin
      tempLight^ := tempSec.lightlevel;
      if tempSec.special = LIGHTNING_SPECIAL then
      begin
        tempSec.lightlevel := tempSec.lightlevel + 64;
        if tempSec.lightlevel > flashLight then
          tempSec.lightlevel := flashLight;
      end
      else if tempSec.special = LIGHTNING_SPECIAL2 then
      begin
        tempSec.lightlevel := tempSec.lightlevel + 32;
        if tempSec.lightlevel > flashLight then
          tempSec.lightlevel := flashLight;
      end
      else
        tempSec.lightlevel := flashLight;
      if tempSec.lightlevel < tempLight^ then
      begin
        tempSec.lightlevel := tempLight^;
      end;
      inc(tempLight);
      foundSec := true;
    end;
    inc(tempSec);
  end;
  if foundSec then
  begin
    SkyTexture := P_GetMapSky2Texture(gamemap); // set alternate sky
    S_StartSound(nil, Ord(SFX_THUNDER_CRASH));
  end;
  // Calculate the next lighting flash
  if NextLightningFlash = 0 then
  begin
    if P_Random < 50 then
    begin // Immediate Quick flash
      NextLightningFlash := (P_Random and 15) + 16;
    end
    else
    begin
      if (P_Random < 128) and (leveltime and 32 = 0) then
        NextLightningFlash := ((P_Random and 7) + 2) * TICRATE
      else
        NextLightningFlash := ((P_Random and 15) + 5) * TICRATE;
    end;
  end;
end;

//==============================================================================
//
// P_AnimateSurfaces
//
//==============================================================================
procedure P_AnimateSurfaces;
var
  i: integer;
  ad: PanimDef_t;
  line: Pline_t;
begin
  // Animate flats and textures
  for i := 0 to AnimDefCount - 1 do
  begin
    ad := @AnimDefs[i];
    dec(ad.tics);
    if ad.tics = 0 then
    begin
      if ad.currentFrameDef = ad.endFrameDef then
        ad.currentFrameDef := ad.startFrameDef
      else
        inc(ad.currentFrameDef);
      ad.tics := FrameDefs[ad.currentFrameDef].tics;
      if ad.tics > 255 then
      begin // Random tics
        ad.tics := _SHR(ad.tics, 16) + P_Random mod _SHR(ad.tics and $ff00, 8);
      end;
      if ad._type = ANIM_FLAT then
      begin
        flats[ad.index].translation := FrameDefs[ad.currentFrameDef].index; // JVAL SOS
      end
      else
      begin // Texture
        texturetranslation[ad.index] := FrameDefs[ad.currentFrameDef].index;
      end;
    end;
  end;

  // Update scrolling textures
  for i := 0 to numlinespecials - 1 do
  begin
    line := linespeciallist[i];

    case line.special of

      100: // Scroll_Texture_Left
        sides[line.sidenum[0]].textureoffset := sides[line.sidenum[0]].textureoffset + _SHL(line.arg1, 10);

      101: // Scroll_Texture_Right
        sides[line.sidenum[0]].textureoffset := sides[line.sidenum[0]].textureoffset - _SHL(line.arg1, 10);

      102: // Scroll_Texture_Up
        sides[line.sidenum[0]].rowoffset := sides[line.sidenum[0]].rowoffset + _SHL(line.arg1, 10);

      103: // Scroll_Texture_Down
        sides[line.sidenum[0]].rowoffset := sides[line.sidenum[0]].rowoffset - _SHL(line.arg1, 10);

    end;

  end;

  // Update sky column offsets
  Sky1ColumnOffset := Sky1ColumnOffset + Sky1ScrollDelta;
  Sky2ColumnOffset := Sky2ColumnOffset + Sky2ScrollDelta;

  if LevelHasLightning then
  begin
    if (NextLightningFlash = 0) or (LightningFlash <> 0) then
      P_LightningFlash
    else
      dec(NextLightningFlash);
  end;
end;

//==============================================================================
//
// P_ForceLightning
//
//==============================================================================
procedure P_ForceLightning(const tics: integer = 0);
begin
  NextLightningFlash := tics;
end;

//==============================================================================
//
// P_InitLightning
//
//==============================================================================
procedure P_InitLightning;
var
  i: integer;
  secCount: integer;
begin
  if not P_GetMapLightning(gamemap) then
  begin
    LevelHasLightning := false;
    LightningFlash := 0;
    exit;
  end;

  LightningFlash := 0;
  secCount := 0;
  for i := 0 to numsectors - 1 do
    if (sectors[i].ceilingpic = skyflatnum) or
       (sectors[i].special = LIGHTNING_SPECIAL) or
       (sectors[i].special = LIGHTNING_SPECIAL2) then
      inc(secCount);

  if secCount > 0 then
    LevelHasLightning := true
  else
  begin
    LevelHasLightning := false;
    exit;
  end;

  LightningLightLevels := Z_Malloc(secCount * SizeOf(integer), PU_LEVEL, nil);
  NextLightningFlash := ((P_Random and 15) + 5) * TICRATE; // don't flash at level start
end;

//==============================================================================
//
// P_InitFTAnims
//
// Initialize flat and texture animation lists.
//
//==============================================================================
procedure P_InitFTAnims;
var
  base: integer;
  _mod: integer;
  fd: integer;
  ad: PanimDef_t;
  ignore: boolean;
  done: boolean;
  sc: TScriptEngine;
begin
  animatedflatslist.Clear;
  animatedtextureslist.Clear;
  fd := 0;
  ad := @AnimDefs[0];
  AnimDefCount := 0;
  sc := TScriptEngine.Create(SC_RemoveLineQuotes(W_TextLumpName(ANIM_SCRIPT_NAME)));
  while sc.GetString do
  begin
    if AnimDefCount = MAX_ANIM_DEFS then
      I_Error('P_InitFTAnims(): too many AnimDefs (%d).', [AnimDefCount]);

    if sc.Compare(SCI_FLAT) then
    begin
      ad._type := ANIM_FLAT
    end
    else if sc.Compare(SCI_TEXTURE) then
    begin
      ad._type := ANIM_TEXTURE
    end
    else
    begin
      sc.ScriptError('P_InitFTAnims(): Unknown animation type: %s'#13#10, [sc._String]);
    end;
    sc.MustGetString; // Name
    ignore := false;
    if ad._type = ANIM_FLAT then
    begin
      if W_CheckNumForName(sc._String) = -1 then
      begin
        ignore := true;
      end
      else
      begin
        ad.index := R_FlatNumForName(sc._String);
        animatedflatslist.Add(ad.index);
      end;
    end
    else
    begin // Texture
      if R_CheckTextureNumForName(sc._String) = -1 then
      begin
        ignore := true;
      end
      else
      begin
        ad.index := R_TextureNumForName(sc._String);
        animatedtextureslist.Add(ad.index);
      end;
    end;
    ad.startFrameDef := fd;
    done := false;
    while not done do
    begin
      if sc.GetString then
      begin
        if sc.Compare(SCI_PIC) then
        begin
          if fd = MAX_FRAME_DEFS then
            I_Error('P_InitFTAnims(): too many FrameDefs (%d).', [fd]);

          sc.MustGetInteger;
          if not ignore then
          begin
            FrameDefs[fd].index := ad.index + sc._Integer - 1;
            if ad._type = ANIM_FLAT then
              animatedflatslist.Add(FrameDefs[fd].index)
            else
              animatedtextureslist.Add(FrameDefs[fd].index);
          end;

          sc.MustGetString;
          if sc.Compare(SCI_TICS) then
          begin
            sc.MustGetInteger;
            if not ignore then
            begin
              FrameDefs[fd].tics := sc._Integer;
              inc(fd);
            end;
          end
          else if sc.Compare(SCI_RAND) then
          begin
            sc.MustGetInteger;
            base := sc._Integer;
            sc.MustGetInteger;
            if not ignore then
            begin
              _mod := sc._Integer - base + 1;
              FrameDefs[fd].tics := (base * FRACUNIT) + (_mod * 256);
              inc(fd);
            end;
          end
          else
            sc.ScriptError('P_InitFTAnims(): Unknown animation type: %s'#13#10, [sc._String]);
        end
        else
        begin
          sc.UnGet;
          done := true;
        end;
      end
      else
      begin
        done := true;
      end;
    end;
    if not ignore and (fd - ad.startFrameDef < 2) then
      I_Error('P_InitFTAnims(): AnimDef has framecount < 2.');
    if not ignore then
    begin
      ad.endFrameDef := fd - 1;
      ad.currentFrameDef := ad.endFrameDef;
      ad.tics := 1; // Force 1st game tic to animate
      inc(AnimDefCount);
      inc(ad);
    end;
  end;
  sc.Free;
end;

//==============================================================================
//
// P_InitAnimations
//
//==============================================================================
procedure P_InitAnimations;
begin
  animatedflatslist := TDNumberList.Create;
  animatedtextureslist := TDNumberList.Create;
end;

//==============================================================================
//
// P_ShutDownAnimations
//
//==============================================================================
procedure P_ShutDownAnimations;
begin
  FreeAndNil(animatedflatslist);
  FreeAndNil(animatedtextureslist);
end;

end.
