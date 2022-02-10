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
//
// DESCRIPTION:
//  Scripting consts
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit sc_consts;

interface

//==============================================================================
//
// SC_InitConsts
//
//==============================================================================
procedure SC_InitConsts;

//==============================================================================
//
// SC_ShutDownConsts
//
//==============================================================================
procedure SC_ShutDownConsts;

//==============================================================================
//
// SC_AddConst
//
//==============================================================================
function SC_AddConst(const name: string; const value: integer): boolean;

//==============================================================================
//
// SC_GetConst
//
//==============================================================================
function SC_GetConst(const name: string; var value: integer): boolean;

implementation

uses
  d_delphi,
  doomdef,
  i_system,
  info_common,
  p_aaptr,
  p_common;

const
  CONSTSHASHSIZE = 16;

var
  constshashtable: array[0..CONSTSHASHSIZE - 1] of TDStringList;

//==============================================================================
//
// _hash
//
//==============================================================================
function _hash(const s: string): integer;
var
  len: integer;
begin
  len := Length(s);
  result := (len + Ord(s[1]) + Ord(s[len])) and (CONSTSHASHSIZE - 1);
end;

//==============================================================================
//
// SC_InitConsts
//
//==============================================================================
procedure SC_InitConsts;
var
  i: integer;
begin
  for i := 0 to CONSTSHASHSIZE - 1 do
    constshashtable[i] := TDStringList.Create;

  SC_AddConst('C_ARG1', 0);
  SC_AddConst('C_ARG2', 1);
  SC_AddConst('C_ARG3', 2);
  SC_AddConst('C_ARG4', 3);
  SC_AddConst('C_ARG5', 4);

  SC_AddConst('AAPTR_DEFAULT', AAPTR_DEFAULT);
  SC_AddConst('AAPTR_NULL', AAPTR_NULL);
  SC_AddConst('AAPTR_TARGET', AAPTR_TARGET);
  SC_AddConst('AAPTR_MASTER', AAPTR_MASTER);
  SC_AddConst('AAPTR_TRACER', AAPTR_TRACER);
  SC_AddConst('AAPTR_PLAYER_GETTARGET', AAPTR_PLAYER_GETTARGET);
  SC_AddConst('AAPTR_PLAYER_GETCONVERSATION', AAPTR_PLAYER_GETCONVERSATION);
  SC_AddConst('AAPTR_PLAYER1', AAPTR_PLAYER1);
  SC_AddConst('AAPTR_PLAYER2', AAPTR_PLAYER2);
  SC_AddConst('AAPTR_PLAYER3', AAPTR_PLAYER3);
  SC_AddConst('AAPTR_PLAYER4', AAPTR_PLAYER4);
  SC_AddConst('AAPTR_PLAYER5', AAPTR_PLAYER5);
  SC_AddConst('AAPTR_PLAYER6', AAPTR_PLAYER6);
  SC_AddConst('AAPTR_PLAYER7', AAPTR_PLAYER7);
  SC_AddConst('AAPTR_PLAYER8', AAPTR_PLAYER8);
  SC_AddConst('AAPTR_FRIENDPLAYER', AAPTR_FRIENDPLAYER);
  SC_AddConst('AAPTR_GET_LINETARGET', AAPTR_GET_LINETARGET);
  SC_AddConst('AAPTR_PLAYER_SELECTORS', AAPTR_PLAYER_SELECTORS);
  SC_AddConst('AAPTR_GENERAL_SELECTORS', AAPTR_GENERAL_SELECTORS);
  SC_AddConst('AAPTR_STATIC_SELECTORS', AAPTR_STATIC_SELECTORS);
  SC_AddConst('PTROP_UNSAFETARGET', PTROP_UNSAFETARGET);
  SC_AddConst('PTROP_UNSAFEMASTER', PTROP_UNSAFEMASTER);
  SC_AddConst('PTROP_NOSAFEGUARDS', PTROP_NOSAFEGUARDS);

  SC_AddConst('SIXF_TRANSFERTRANSLATION', SIXF_TRANSFERTRANSLATION);
  SC_AddConst('SIXF_ABSOLUTEPOSITION', SIXF_ABSOLUTEPOSITION);
  SC_AddConst('SIXF_ABSOLUTEANGLE', SIXF_ABSOLUTEANGLE);
  SC_AddConst('SIXF_ABSOLUTEMOMENTUM', SIXF_ABSOLUTEMOMENTUM);
  SC_AddConst('SIXF_SETMASTER', SIXF_SETMASTER);
  SC_AddConst('SIXF_NOCHECKPOSITION', SIXF_NOCHECKPOSITION);
  SC_AddConst('SIXF_TELEFRAG', SIXF_TELEFRAG);
  SC_AddConst('SIXF_TRANSFERAMBUSHFLAG', SIXF_TRANSFERAMBUSHFLAG);
  SC_AddConst('SIXF_TRANSFERPITCH', SIXF_TRANSFERPITCH);
  SC_AddConst('SIXF_TRANSFERPOINTERS', SIXF_TRANSFERPOINTERS);
  SC_AddConst('SIXF_USEBLOODCOLOR', SIXF_USEBLOODCOLOR);
  SC_AddConst('SIXF_CLEARCALLERTID', SIXF_CLEARCALLERTID);
  SC_AddConst('SIXF_MULTIPLYSPEED', SIXF_MULTIPLYSPEED);
  SC_AddConst('SIXF_TRANSFERSCALE', SIXF_TRANSFERSCALE);
  SC_AddConst('SIXF_TRANSFERSPECIAL', SIXF_TRANSFERSPECIAL);
  SC_AddConst('SIXF_CLEARCALLERSPECIAL', SIXF_CLEARCALLERSPECIAL);
  SC_AddConst('SIXF_TRANSFERSTENCILCOL', SIXF_TRANSFERSTENCILCOL);
  SC_AddConst('SIXF_TRANSFERALPHA', SIXF_TRANSFERALPHA);
  SC_AddConst('SIXF_TRANSFERRENDERSTYLE', SIXF_TRANSFERRENDERSTYLE);
  SC_AddConst('SIXF_SETTARGET', SIXF_SETTARGET);
  SC_AddConst('SIXF_SETTRACER', SIXF_SETTRACER);
  SC_AddConst('SIXF_NOPOINTERS', SIXF_NOPOINTERS);
  SC_AddConst('SIXF_ORIGINATOR', SIXF_ORIGINATOR);
  SC_AddConst('SIXF_TRANSFERSPRITEFRAME', SIXF_TRANSFERSPRITEFRAME);
  SC_AddConst('SIXF_TRANSFERROLL', SIXF_TRANSFERROLL);
  SC_AddConst('SIXF_ISTARGET', SIXF_ISTARGET);
  SC_AddConst('SIXF_ISMASTER', SIXF_ISMASTER);
  SC_AddConst('SIXF_ISTRACER', SIXF_ISTRACER);
  SC_AddConst('SIXF_DROPPED', SIXF_DROPPED);

  SC_AddConst('SXF_TRANSFERTRANSLATION', SIXF_TRANSFERTRANSLATION);
  SC_AddConst('SXF_ABSOLUTEPOSITION', SIXF_ABSOLUTEPOSITION);
  SC_AddConst('SXF_ABSOLUTEANGLE', SIXF_ABSOLUTEANGLE);
  SC_AddConst('SXF_ABSOLUTEMOMENTUM', SIXF_ABSOLUTEMOMENTUM);
  SC_AddConst('SXF_SETMASTER', SIXF_SETMASTER);
  SC_AddConst('SXF_NOCHECKPOSITION', SIXF_NOCHECKPOSITION);
  SC_AddConst('SXF_TELEFRAG', SIXF_TELEFRAG);
  SC_AddConst('SXF_TRANSFERAMBUSHFLAG', SIXF_TRANSFERAMBUSHFLAG);
  SC_AddConst('SXF_TRANSFERPITCH', SIXF_TRANSFERPITCH);
  SC_AddConst('SXF_TRANSFERPOINTERS', SIXF_TRANSFERPOINTERS);
  SC_AddConst('SXF_USEBLOODCOLOR', SIXF_USEBLOODCOLOR);
  SC_AddConst('SXF_CLEARCALLERTID', SIXF_CLEARCALLERTID);
  SC_AddConst('SXF_MULTIPLYSPEED', SIXF_MULTIPLYSPEED);
  SC_AddConst('SXF_TRANSFERSCALE', SIXF_TRANSFERSCALE);
  SC_AddConst('SXF_TRANSFERSPECIAL', SIXF_TRANSFERSPECIAL);
  SC_AddConst('SXF_CLEARCALLERSPECIAL', SIXF_CLEARCALLERSPECIAL);
  SC_AddConst('SXF_TRANSFERSTENCILCOL', SIXF_TRANSFERSTENCILCOL);
  SC_AddConst('SXF_TRANSFERALPHA', SIXF_TRANSFERALPHA);
  SC_AddConst('SXF_TRANSFERRENDERSTYLE', SIXF_TRANSFERRENDERSTYLE);
  SC_AddConst('SXF_SETTARGET', SIXF_SETTARGET);
  SC_AddConst('SXF_SETTRACER', SIXF_SETTRACER);
  SC_AddConst('SXF_NOPOINTERS', SIXF_NOPOINTERS);
  SC_AddConst('SXF_ORIGINATOR', SIXF_ORIGINATOR);
  SC_AddConst('SXF_TRANSFERSPRITEFRAME', SIXF_TRANSFERSPRITEFRAME);
  SC_AddConst('SXF_TRANSFERROLL', SIXF_TRANSFERROLL);
  SC_AddConst('SXF_ISTARGET', SIXF_ISTARGET);
  SC_AddConst('SXF_ISMASTER', SIXF_ISMASTER);
  SC_AddConst('SXF_ISTRACER', SIXF_ISTRACER);
  SC_AddConst('SXF_DROPPED', SIXF_DROPPED);

  SC_AddConst('CMF_AIMOFFSET', CMF_AIMOFFSET);
  SC_AddConst('CMF_AIMDIRECTION', CMF_AIMDIRECTION);
  SC_AddConst('CMF_TRACKOWNER', CMF_TRACKOWNER);
  SC_AddConst('CMF_CHECKTARGETDEAD', CMF_CHECKTARGETDEAD);
  SC_AddConst('CMF_ABSOLUTEPITCH', CMF_ABSOLUTEPITCH);
  SC_AddConst('CMF_OFFSETPITCH', CMF_OFFSETPITCH);
  SC_AddConst('CMF_SAVEPITCH', CMF_SAVEPITCH);
  SC_AddConst('CMF_ABSOLUTEANGLE', CMF_ABSOLUTEANGLE);

  SC_AddConst('SPF_FORCECLAMP', SPF_FORCECLAMP);
  SC_AddConst('SPF_INTERPOLATE', SPF_INTERPOLATE);

  SC_AddConst('RMVF_MISSILES', RMVF_MISSILES);
  SC_AddConst('RMVF_NOMONSTERS', RMVF_NOMONSTERS);
  SC_AddConst('RMVF_MISC', RMVF_MISC);
  SC_AddConst('RMVF_EVERYTHING', RMVF_EVERYTHING);
  SC_AddConst('RMVF_EXFILTER', RMVF_EXFILTER);
  SC_AddConst('RMVF_EXSPECIES', RMVF_EXSPECIES);
  SC_AddConst('RMVF_EITHER', RMVF_EITHER);

  SC_AddConst('FTF_REMOVE', FTF_REMOVE);
  SC_AddConst('FTF_CLAMP', FTF_CLAMP);

  SC_AddConst('AMF_TARGETEMITTER', AMF_TARGETEMITTER);
  SC_AddConst('AMF_TARGETNONPLAYER', AMF_TARGETNONPLAYER);
  SC_AddConst('AMF_EMITFROMTARGET', AMF_EMITFROMTARGET);

  SC_AddConst('TICRATE', TICRATE);

  SC_AddConst('SK_BABY', Ord(sk_baby));
  SC_AddConst('SK_EASY', Ord(sk_easy));
  SC_AddConst('SK_MEDIUM', Ord(sk_medium));
  SC_AddConst('SK_HARD', Ord(sk_hard));
  SC_AddConst('SK_NIGHTMARE', Ord(sk_nightmare));

  // infighting groups
  SC_AddConst('IG_INVALID', IG_INVALID);
  SC_AddConst('IG_DEFAULT', IG_DEFAULT);
  SC_AddConst('IG_END', IG_END);
  // projectile groups
  SC_AddConst('PG_INVALID', PG_INVALID);
  SC_AddConst('PG_GROUPLESS', PG_GROUPLESS);
  SC_AddConst('PG_DEFAULT', PG_DEFAULT);
  {$IFDEF  DOOM}
  SC_AddConst('PG_BARON', PG_BARON);
  {$ENDIF}
  SC_AddConst('PG_END', PG_END);
  // Splash groups
  SC_AddConst('SG_INVALID', SG_INVALID);
  SC_AddConst('SG_DEFAULT', SG_DEFAULT);
  SC_AddConst('SG_END', SG_END);
end;

//==============================================================================
//
// SC_ShutDownConsts
//
//==============================================================================
procedure SC_ShutDownConsts;
var
  i, j: integer;
begin
  for i := 0 to CONSTSHASHSIZE - 1 do
  begin
    for j := 0 to constshashtable[i].Count - 1 do
      constshashtable[i].Objects[j].Free;
    constshashtable[i].Free;
  end;
end;

//==============================================================================
//
// SC_AddConst
//
//==============================================================================
function SC_AddConst(const name: string; const value: integer): boolean;
var
  uToken: string;
  hash: integer;
begin
  uToken := strupper(strtrim(name));
  if uToken = '' then
  begin
    I_Warning('SC_AddConst(): Trying to add const in hash without a name, value="%d"'#13#10, [value]);
    result := false;
    exit;
  end;

  hash := _hash(uToken);
  if constshashtable[hash].IndexOf(uToken) >= 0 then
  begin
    I_Warning('SC_AddConst(): Trying to add const "%s" twice in hash, value="%d"'#13#10, [name, value]);
    result := false;
    exit;
  end;

  constshashtable[hash].AddObject(uToken, TInteger.Create(value));
  result := true;
end;

//==============================================================================
//
// SC_GetConst
//
//==============================================================================
function SC_GetConst(const name: string; var value: integer): boolean;
var
  uToken: string;
  hash: integer;
  idx: integer;
begin
  uToken := strupper(strtrim(name));
  if uToken = '' then
  begin
    result := false;
    exit;
  end;

  hash := _hash(uToken);
  idx := constshashtable[hash].IndexOf(uToken);
  if idx >= 0 then
  begin
    value := (constshashtable[hash].Objects[idx] as TInteger).intnum;
    result := true;
    exit;
  end;

  result := false;
end;

end.
