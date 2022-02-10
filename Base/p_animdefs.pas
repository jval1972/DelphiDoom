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
//  ANIMDEFS lump
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_animdefs;

interface

uses
  d_delphi;

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

var
  animatedflatslist: TDNumberList;
  animatedtextureslist: TDNumberList;

implementation

uses
  m_rnd,
  m_fixed,
  i_system,
  r_data,
  r_defs,
  sc_engine,
  w_wad;

const
  ANIM_SCRIPT_NAME = 'ANIMDEFS';
  MAX_ANIM_DEFS = 128;
  MAX_FRAME_DEFS = 512;
  ANIM_FLAT = 0;
  ANIM_TEXTURE = 1;
  SCI_FLAT = 'flat';
  SCI_TEXTURE = 'texture';
  SCI_PIC = 'pic';
  SCI_TICS = 'tics';
  SCI_RAND = 'rand';

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

//==========================================================================
//
// P_AnimateSurfaces
//
//==========================================================================
//
//==============================================================================
procedure P_AnimateSurfaces;
var
  i: integer;
  ad: PanimDef_t;
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

end;

//==========================================================================
//
// P_InitFTAnims
//
// Initialize flat and texture animation lists.
//
//==========================================================================
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
  animlump: integer;
begin
  animlump := W_CheckNumForName(ANIM_SCRIPT_NAME);
  if animlump < 0 then
    exit;

  animatedflatslist.Clear;
  animatedtextureslist.Clear;
  fd := 0;
  ad := @AnimDefs[0];
  AnimDefCount := 0;
  sc := TScriptEngine.Create(SC_RemoveLineQuotes(W_TextLumpNum(animlump)));
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

