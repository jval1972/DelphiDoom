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
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_lights;

interface

uses
  d_delphi;

const
  LIGHTBOOSTSIZE = 256;

type
  lpost_t = record
    topdelta: integer;
    length: integer;   // length data bytes follows
  end;
  Plpost_t = ^lpost_t;

const
  LFACTORMIN = 16;
  LFACTORMAX = 1024;

var
  lightboostfactor: LongWord = 128;
  lighboostlookup: array[0..LIGHTBOOSTSIZE - 1] of lpost_t;
  lightboost: PLongWordArray = nil;
  uselightboost: boolean;
  uselightboostgodmode: boolean;

procedure R_InitLightBoost;

procedure R_ShutDownLightBoost;

procedure R_CmdLightBoostFactor(const parm1: string = '');

implementation

{$IFDEF OPENGL}
uses
  gl_lights;
{$ENDIF}

//
// R_InitLightBoost
//
procedure R_InitLightBoost;
var
  i, j: integer;
  dist: double;
  c: LongWord;
begin
  if lightboost = nil then
    lightboost := PLongWordArray(malloc(LIGHTBOOSTSIZE * LIGHTBOOSTSIZE * SizeOf(LongWord)));
  for i := 0 to LIGHTBOOSTSIZE - 1 do
  begin
    lighboostlookup[i].topdelta := MAXINT;
    lighboostlookup[i].length := 0;
    for j := 0 to LIGHTBOOSTSIZE - 1 do
    begin
      dist := sqrt(sqr(i - (LIGHTBOOSTSIZE shr 1)) + sqr(j - (LIGHTBOOSTSIZE shr 1)));
      if dist <= (LIGHTBOOSTSIZE shr 1) then
      begin
        inc(lighboostlookup[i].length);
        c := round(dist * 2);
        if c > 255 then
          c := 0
        else
          c := 255 - c;
        lightboost[i * LIGHTBOOSTSIZE + j] := $10000 + c * {$IFDEF OPENGL}GL_LIGHTBOOSTFACTOR{$ELSE}lightboostfactor{$ENDIF};
        if j < lighboostlookup[i].topdelta then
          lighboostlookup[i].topdelta := j;
      end
      else
        lightboost[i * LIGHTBOOSTSIZE + j] := $10000;
    end;
  end;
end;

procedure R_ShutDownLightBoost;
begin
  if lightboost <> nil then
    memfree(pointer(lightboost), LIGHTBOOSTSIZE * LIGHTBOOSTSIZE * SizeOf(LongWord));
end;

procedure R_CmdLightBoostFactor(const parm1: string = '');
var
  newfactor: LongWord;
begin
  if parm1 = '' then
  begin
    printf('Current setting: lightboostfactor = %d'#13#10, [lightboostfactor]);
    exit;
  end;

  newfactor := atoi(parm1, lightboostfactor);
  if newfactor <> lightboostfactor then
  begin
    if (newfactor >= LFACTORMIN) and (newfactor <= LFACTORMAX) then
    begin
      lightboostfactor := newfactor;
      R_InitLightBoost;
    end
    else
    begin
      printf('Please specify a value in range [%d..%d]'#13#10, [LFACTORMIN, LFACTORMAX]);
      exit;
    end;
  end;

  R_CmdLightBoostFactor;
end;

end.
